// Tests for GraphQL null propagation per spec
// https://spec.graphql.org/October2021/#sec-Handling-Field-Errors

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: option.Option(String))
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Test", Some("test@example.com")))
}

// ============================================================================
// Helper: Build test schema with nullable and non-null fields
// ============================================================================

fn build_test_schema_with_user_resolver(
  user_resolver: fn(schema.ExecutionContext) -> Result(Dynamic, String),
) -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  // Query with nullable user field
  let user_query =
    query.query("user", schema.named_type("User"), user_resolver, fn(u) {
      types.to_dynamic(u)
    })

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

fn build_test_schema_with_required_user_resolver(
  user_resolver: fn(schema.ExecutionContext) -> Result(Dynamic, String),
) -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  // Query with NON-NULL user field (User!)
  let user_query =
    query.query(
      "requiredUser",
      schema.non_null(schema.named_type("User")),
      user_resolver,
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

// ============================================================================
// Test: Nullable field returning null should be fine
// ============================================================================

pub fn nullable_field_returns_null_test() {
  // Build schema where user resolver returns null
  let test_schema =
    build_test_schema_with_user_resolver(fn(_ctx) {
      // Return null for nullable field - this is OK
      Ok(types.to_dynamic(Nil))
    })

  let result = executor.execute_query(test_schema, "{ user { id name } }")

  // Should have data (with null user), no errors
  case result.data {
    Some(_) -> Nil
    None -> panic as "Should have data even when nullable field is null"
  }

  case result.errors {
    [] -> Nil
    _ -> panic as "Should have no errors for nullable field returning null"
  }
}

// ============================================================================
// Test: Non-null field returning null should error
// ============================================================================

pub fn non_null_field_returns_null_test() {
  // Build schema where requiredUser resolver returns null
  let test_schema =
    build_test_schema_with_required_user_resolver(fn(_ctx) {
      // Return null for non-null field - this is an ERROR
      Ok(types.to_dynamic(Nil))
    })

  let result =
    executor.execute_query(test_schema, "{ requiredUser { id name } }")

  // Should have an error about null on non-null field
  case result.errors {
    [executor.NullValueError(_, _), ..] -> Nil
    [] -> panic as "Should have error for non-null field returning null"
    _ -> Nil  // Other error types also acceptable
  }

  // Data should be None (null bubbled to root)
  case result.data {
    None -> Nil
    Some(_) ->
      panic as "Data should be null when non-null field at root returns null"
  }
}

// ============================================================================
// Test: Null bubbles to nullable parent
// ============================================================================

pub fn null_bubbles_to_nullable_parent_test() {
  // Build a schema with nested types where inner non-null field returns null
  // but outer field is nullable, so it absorbs the null

  let email_type =
    schema.object("Email")
    |> schema.field(
      schema.field_def("address", schema.non_null(schema.string_type()))
      |> schema.resolver(fn(_info) {
        // Non-null field returns null - error!
        Ok(types.to_dynamic(Nil))
      }),
    )

  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.id_type())
      |> schema.resolver(fn(_info) { Ok(types.to_dynamic("1")) }),
    )
    |> schema.field(
      // email field is NULLABLE, so null can stop here
      schema.field_def("email", schema.named_type("Email"))
      |> schema.resolver(fn(_info) {
        Ok(
          types.to_dynamic(
            dict.from_list([#("address", types.to_dynamic(Nil))]),
          ),
        )
      }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      // user field is NULLABLE
      schema.field_def("user", schema.named_type("User"))
      |> schema.resolver(fn(_info) {
        Ok(types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))])))
      }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.ObjectTypeDef(email_type))

  let result =
    executor.execute_query(test_schema, "{ user { id email { address } } }")

  // Should have errors
  case result.errors != [] {
    True -> Nil
    False -> panic as "Should have error for non-null field returning null"
  }

  // Data should exist (user becomes null but query still has data)
  case result.data {
    Some(_) -> Nil
    None -> panic as "Data should exist when null bubbles to nullable parent"
  }
}

// ============================================================================
// Test: Null bubbles all the way to root if all non-null
// ============================================================================

pub fn null_bubbles_to_root_when_all_non_null_test() {
  // Schema where all fields up to root are non-null
  let email_type =
    schema.object("Email")
    |> schema.field(
      schema.field_def("address", schema.non_null(schema.string_type()))
      |> schema.resolver(fn(_info) {
        // Returns null on non-null field
        Ok(types.to_dynamic(Nil))
      }),
    )

  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.id_type())
      |> schema.resolver(fn(_info) { Ok(types.to_dynamic("1")) }),
    )
    |> schema.field(
      // email is NON-NULL
      schema.field_def("email", schema.non_null(schema.named_type("Email")))
      |> schema.resolver(fn(_info) {
        Ok(
          types.to_dynamic(
            dict.from_list([#("address", types.to_dynamic(Nil))]),
          ),
        )
      }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      // user is NON-NULL
      schema.field_def("user", schema.non_null(schema.named_type("User")))
      |> schema.resolver(fn(_info) {
        Ok(types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))])))
      }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.ObjectTypeDef(email_type))

  let result =
    executor.execute_query(test_schema, "{ user { id email { address } } }")

  // Should have errors
  case result.errors != [] {
    True -> Nil
    False -> panic as "Should have error"
  }

  // Data should be None because null bubbled all the way to root
  case result.data {
    None -> Nil
    Some(_) -> panic as "Data should be null when null bubbles to root"
  }
}

// ============================================================================
// Test: Errors are collected during null propagation
// ============================================================================

pub fn errors_collected_during_null_propagation_test() {
  let test_schema =
    build_test_schema_with_required_user_resolver(fn(_ctx) {
      Ok(types.to_dynamic(Nil))
    })

  let result =
    executor.execute_query(test_schema, "{ requiredUser { id name } }")

  // Should have at least one NullValueError
  case result.errors {
    [executor.NullValueError(_message, path), ..] -> {
      // Path should include the field name
      case path != [] {
        True -> Nil
        False -> panic as "Error path should not be empty"
      }
    }
    [] -> panic as "Should have error"
    _ -> Nil
    // Other error types also acceptable
  }
}

// ============================================================================
// Test: List with non-null items where one item is null
// ============================================================================

pub fn list_with_non_null_items_null_test() {
  // Schema with [User!] where one user is null
  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.id_type())
      |> schema.resolver(fn(_info) { Ok(types.to_dynamic("1")) }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      // users: [User!] - list items are non-null
      schema.field_def(
          "users",
          schema.list_type(schema.non_null(schema.named_type("User"))),
        )
      |> schema.resolver(fn(_info) {
        // Return a list with a null item - use Dynamic list so Nil can be included
        let item1 = types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))]))
        let item2 = types.to_dynamic(Nil)  // This null item should cause error
        let item3 = types.to_dynamic(dict.from_list([#("id", types.to_dynamic("3"))]))
        Ok(types.to_dynamic([item1, item2, item3]))
      }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))

  let result = executor.execute_query(test_schema, "{ users { id } }")

  // Should have an error about null item in non-null list
  case result.errors != [] {
    True -> Nil
    False -> panic as "Should have error for null item in [User!] list"
  }
}

// ============================================================================
// Test: Nested non-null fields
// ============================================================================

pub fn nested_non_null_fields_test() {
  // Schema with deeply nested non-null fields
  let address_type =
    schema.object("Address")
    |> schema.field(
      schema.field_def("street", schema.non_null(schema.string_type()))
      |> schema.resolver(fn(_info) {
        // Return null on non-null field at deepest level
        Ok(types.to_dynamic(Nil))
      }),
    )

  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.id_type())
      |> schema.resolver(fn(_info) { Ok(types.to_dynamic("1")) }),
    )
    |> schema.field(
      schema.field_def(
          "address",
          schema.non_null(schema.named_type("Address")),
        )
      |> schema.resolver(fn(_info) {
        Ok(types.to_dynamic(dict.from_list([#("street", types.to_dynamic(Nil))])))
      }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      // user is nullable - null should stop here
      schema.field_def("user", schema.named_type("User"))
      |> schema.resolver(fn(_info) {
        Ok(types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))])))
      }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.ObjectTypeDef(address_type))

  let result =
    executor.execute_query(test_schema, "{ user { id address { street } } }")

  // Should have error
  case result.errors != [] {
    True -> Nil
    False -> panic as "Should have error for nested non-null returning null"
  }

  // Data should exist (null stopped at nullable user field)
  case result.data {
    Some(_) -> Nil
    None -> panic as "Data should exist when null stops at nullable parent"
  }
}

// ============================================================================
// Test: Valid non-null field - no error
// ============================================================================

pub fn valid_non_null_field_test() {
  let test_schema =
    build_test_schema_with_required_user_resolver(fn(_ctx) {
      // Return a valid user
      Ok(
        types.to_dynamic(
          dict.from_list([
            #("id", types.to_dynamic("1")),
            #("name", types.to_dynamic("Test User")),
          ]),
        ),
      )
    })

  let result =
    executor.execute_query(test_schema, "{ requiredUser { id name } }")

  // Should have no errors
  case result.errors {
    [] -> Nil
    _ -> panic as "Should have no errors for valid non-null field"
  }

  // Should have data
  case result.data {
    Some(_) -> Nil
    None -> panic as "Should have data for valid query"
  }
}

// ============================================================================
// Test: Nullable list with null items is OK
// ============================================================================

pub fn nullable_list_items_test() {
  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.id_type())
      |> schema.resolver(fn(_info) { Ok(types.to_dynamic("1")) }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      // users: [User] - list items are nullable (can be null)
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.resolver(fn(_info) {
        // Return a list with a null item - this should be OK
        let item1 = types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))]))
        let item2 = types.to_dynamic(Nil)
        let item3 = types.to_dynamic(dict.from_list([#("id", types.to_dynamic("3"))]))
        Ok(types.to_dynamic([item1, item2, item3]))
      }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))

  let result = executor.execute_query(test_schema, "{ users { id } }")

  // Should have NO errors - nullable list items can be null
  case result.errors {
    [] -> Nil
    _ -> panic as "Should have no errors for nullable list items"
  }

  // Should have data
  case result.data {
    Some(_) -> Nil
    None -> panic as "Should have data"
  }
}
