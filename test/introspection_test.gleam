// Tests for GraphQL Introspection support

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import mochi/executor
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

fn assert_true(condition: Bool, message: String) -> Nil {
  case condition {
    True -> Nil
    False -> panic as message
  }
}

fn assert_ok(res: Result(a, b), message: String) -> a {
  case res {
    Ok(v) -> v
    Error(_) -> panic as message
  }
}

fn parse_ok(query_str: String) -> Nil {
  case parser.parse(query_str) {
    Ok(_) -> Nil
    Error(_) -> panic as "Parse should succeed"
  }
}

// ============================================================================
// Test Types
// ============================================================================

pub type TestUser {
  TestUser(id: String, name: String, email: String)
}

fn decode_test_user(_dyn: Dynamic) -> Result(TestUser, String) {
  Ok(TestUser("1", "Test User", "test@example.com"))
}

fn create_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: TestUser) { u.id })
    |> types.string("name", fn(u: TestUser) { u.name })
    |> types.string("email", fn(u: TestUser) { u.email })
    |> types.build(decode_test_user)

  let role_enum =
    types.enum_type("Role")
    |> types.enum_description("User roles")
    |> types.value("ADMIN")
    |> types.value("USER")
    |> types.value("GUEST")
    |> types.build_enum

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([TestUser("1", "Test", "test@example.com")]) },
      types.to_dynamic,
    )
    |> query.query_description("Get all users")

  let user_query =
    query.query_with_args(
      "user",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.named_type("User"),
      fn(args) {
        dict.get(args, "id")
        |> result.map(fn(_) { "1" })
        |> result.map_error(fn(_) { "Missing id" })
      },
      fn(id, _ctx) { Ok(TestUser(id, "User " <> id, "user@example.com")) },
      types.to_dynamic,
    )
    |> query.query_description("Get a user by ID")

  query.new()
  |> query.add_query(users_query)
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.add_enum(role_enum)
  |> query.build
}

// ============================================================================
// Parser Tests - Introspection Queries
// ============================================================================

pub fn parse_typename_query_test() {
  parse_ok("{ user(id: \"1\") { __typename name } }")
}

pub fn parse_schema_introspection_test() {
  parse_ok("{ __schema { queryType { name } } }")
}

pub fn parse_type_introspection_test() {
  parse_ok("{ __type(name: \"User\") { name kind } }")
}

pub fn parse_full_introspection_query_test() {
  let query_str =
    "
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          kind
          name
          description
          fields {
            name
            description
            args {
              name
              description
              type { kind name ofType { kind name } }
            }
            type { kind name ofType { kind name } }
          }
          enumValues { name description }
        }
        directives { name description }
      }
    }
  "
  parse_ok(query_str)
}

// ============================================================================
// Executor Tests - Introspection
// ============================================================================

pub fn execute_typename_test() {
  let test_schema = create_test_schema()
  let _ = executor.execute_query(test_schema, "{ __typename }")
  Nil
}

pub fn execute_schema_introspection_test() {
  let test_schema = create_test_schema()
  let _ =
    executor.execute_query(test_schema, "{ __schema { queryType { name } } }")
  Nil
}

pub fn execute_type_introspection_test() {
  let test_schema = create_test_schema()
  let _ =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"User\") { name kind } }",
    )
  Nil
}

pub fn execute_type_introspection_with_variable_test() {
  let test_schema = create_test_schema()
  let query_str =
    "query GetType($name: String!) { __type(name: $name) { name kind } }"
  let variables = dict.from_list([#("name", types.to_dynamic("User"))])
  let _ =
    executor.execute_query_with_variables(test_schema, query_str, variables)
  Nil
}

// ============================================================================
// Schema Type Tests
// ============================================================================

pub fn schema_has_user_type_test() {
  let test_schema = create_test_schema()

  let user_type =
    assert_ok(
      dict.get(test_schema.types, "User"),
      "Schema should have User type",
    )

  case user_type {
    schema.ObjectTypeDef(obj) ->
      assert_true(obj.name == "User", "Type name should be User")
    _ -> panic as "Should be ObjectTypeDef"
  }
}

pub fn schema_has_role_enum_test() {
  let test_schema = create_test_schema()

  let role_type =
    assert_ok(
      dict.get(test_schema.types, "Role"),
      "Schema should have Role enum",
    )

  case role_type {
    schema.EnumTypeDef(enum) -> {
      assert_true(enum.name == "Role", "Enum name should be Role")
      assert_true(dict.size(enum.values) == 3, "Role enum should have 3 values")
    }
    _ -> panic as "Should be EnumTypeDef"
  }
}

pub fn schema_query_type_test() {
  let test_schema = create_test_schema()

  case test_schema.query {
    Some(query_type) -> {
      assert_true(query_type.name == "Query", "Query type name should be Query")
      let _ =
        assert_ok(
          dict.get(query_type.fields, "users"),
          "Should have users field",
        )
      let _ =
        assert_ok(dict.get(query_type.fields, "user"), "Should have user field")
      Nil
    }
    _ -> panic as "Schema should have query type"
  }
}

// ============================================================================
// Introspection Accuracy Tests
// ============================================================================

fn get_string_field(d: Dynamic, key: String) -> String {
  decode.run(d, decode.at([key], decode.string))
  |> result.unwrap("")
}

fn get_list_field(d: Dynamic, key: String) -> List(Dynamic) {
  decode.run(d, decode.at([key], decode.list(decode.dynamic)))
  |> result.unwrap([])
}

fn get_data(result: executor.ExecutionResult) -> Dynamic {
  case result.data {
    Some(d) -> d
    None -> panic as "Expected data but got None"
  }
}

/// The executor returns data as a list of field dicts merged together.
/// This helper finds a field by name within that list.
fn find_field(data: Dynamic, name: String) -> Dynamic {
  case decode.run(data, decode.list(decode.dynamic)) {
    Ok(items) ->
      list.find_map(items, fn(item) {
        decode.run(item, decode.at([name], decode.dynamic))
      })
      |> result.unwrap(types.to_dynamic(Nil))
    Error(_) ->
      // Also try direct dict access (for nested selections)
      decode.run(data, decode.at([name], decode.dynamic))
      |> result.unwrap(types.to_dynamic(Nil))
  }
}

// Test that enum types return kind "ENUM" not "OBJECT" in __type introspection
pub fn enum_type_kind_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"Role\") { kind name } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  assert_true(
    get_string_field(type_obj, "kind") == "ENUM",
    "Role enum should have kind ENUM, not OBJECT",
  )
  assert_true(
    get_string_field(type_obj, "name") == "Role",
    "Type name should be Role",
  )
}

pub type Item {
  Item(id: String, status: String)
}

// Test that enum field type on an object returns kind "ENUM" in field type info
pub fn field_type_kind_for_enum_test() {
  // Build a schema where a field returns the Status enum
  let role_enum =
    types.enum_type("Status")
    |> types.value("ACTIVE")
    |> types.value("INACTIVE")
    |> types.build_enum

  let item_type =
    types.object("Item")
    |> types.id("id", fn(i: Item) { i.id })
    |> types.string("status", fn(i: Item) { i.status })
    |> types.build(fn(_) { Ok(Item("1", "ACTIVE")) })

  let test_schema =
    query.new()
    |> query.add_query(query.query(
      "item",
      schema.named_type("Item"),
      fn(_) { Ok(Item("1", "ACTIVE")) },
      types.to_dynamic,
    ))
    |> query.add_type(item_type)
    |> query.add_enum(role_enum)
    |> query.build

  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"Status\") { kind name } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  assert_true(
    get_string_field(type_obj, "kind") == "ENUM",
    "Status should have kind ENUM",
  )
}

// Test that __schema.directives returns built-in directives (skip, include, deprecated)
pub fn schema_directives_not_empty_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(test_schema, "{ __schema { directives { name } } }")
  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")
  let directives = get_list_field(schema_obj, "directives")
  assert_true(
    directives != [],
    "Schema should expose at least the built-in directives",
  )
}

pub fn schema_directives_include_skip_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(test_schema, "{ __schema { directives { name } } }")
  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")
  let directives = get_list_field(schema_obj, "directives")
  let names = list.map(directives, fn(d) { get_string_field(d, "name") })
  assert_true(
    list.contains(names, "skip"),
    "@skip directive should be in schema directives",
  )
  assert_true(
    list.contains(names, "include"),
    "@include directive should be in schema directives",
  )
  assert_true(
    list.contains(names, "deprecated"),
    "@deprecated directive should be in schema directives",
  )
}

// Test that directive locations are included in __schema.directives
pub fn schema_directive_locations_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(
      test_schema,
      "{ __schema { directives { name locations } } }",
    )
  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")
  let directives = get_list_field(schema_obj, "directives")
  let skip_dir =
    list.find(directives, fn(d) { get_string_field(d, "name") == "skip" })
  case skip_dir {
    Ok(d) -> {
      let locations = get_list_field(d, "locations")
      assert_true(locations != [], "@skip should have locations")
    }
    Error(_) -> panic as "Expected @skip directive"
  }
}

// Test that interface type exposes possibleTypes
pub type Dog {
  Dog(name: String)
}

pub fn interface_possible_types_test() {
  // Build schema with interface and implementing types
  let animal_interface =
    schema.interface("Animal")
    |> schema.interface_field(schema.field_def("name", schema.string_type()))

  let dog_type =
    types.object("Dog2")
    |> types.string("name", fn(d: Dog) { d.name })
    |> types.build(fn(_) { Ok(Dog("Rex")) })
    |> fn(obj) { schema.ObjectType(..obj, interfaces: [animal_interface]) }

  let test_schema =
    query.new()
    |> query.add_query(query.query(
      "dog",
      schema.named_type("Dog2"),
      fn(_) { Ok(Dog("Rex")) },
      types.to_dynamic,
    ))
    |> query.add_type(dog_type)
    |> query.add_interface(animal_interface)
    |> query.build

  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"Animal\") { kind possibleTypes { name kind } } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  assert_true(
    get_string_field(type_obj, "kind") == "INTERFACE",
    "Animal should be kind INTERFACE",
  )
  let possible = get_list_field(type_obj, "possibleTypes")
  assert_true(possible != [], "Interface should have possibleTypes")
  let names = list.map(possible, fn(t) { get_string_field(t, "name") })
  assert_true(
    list.contains(names, "Dog2"),
    "Dog2 should be in possibleTypes for Animal interface",
  )
}

// Test that scalar type kind resolves correctly in field type introspection
pub fn scalar_field_type_kind_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"User\") { fields { name type { kind name } } } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  let fields = get_list_field(type_obj, "fields")
  // The "id" field on User should have type kind SCALAR (ID type)
  let id_field =
    list.find(fields, fn(f) { get_string_field(f, "name") == "id" })
  case id_field {
    Ok(f) -> {
      let field_type =
        decode.run(f, decode.at(["type"], decode.dynamic))
        |> result.unwrap(types.to_dynamic(Nil))
      // ID is NonNull(Named("ID")) so we check the ofType
      let of_type =
        decode.run(field_type, decode.at(["ofType"], decode.dynamic))
        |> result.unwrap(types.to_dynamic(Nil))
      assert_true(
        get_string_field(of_type, "kind") == "SCALAR",
        "ID type should be SCALAR kind",
      )
    }
    Error(_) -> panic as "Expected id field on User"
  }
}

// Test defaultValue is returned for input object fields that have defaults
pub fn input_field_default_value_test() {
  let filter_input =
    schema.InputObjectType(
      name: "FilterInput",
      description: None,
      fields: dict.from_list([
        #(
          "limit",
          schema.InputFieldDefinition(
            name: "limit",
            description: None,
            field_type: schema.Named("Int"),
            default_value: Some(types.to_dynamic(10)),
          ),
        ),
      ]),
    )

  let base = create_test_schema()
  let test_schema =
    schema.Schema(
      ..base,
      types: dict.insert(
        base.types,
        "FilterInput",
        schema.InputObjectTypeDef(filter_input),
      ),
    )

  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"FilterInput\") { inputFields { name defaultValue } } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  let input_fields = get_list_field(type_obj, "inputFields")
  let limit_field =
    list.find(input_fields, fn(f) { get_string_field(f, "name") == "limit" })
  case limit_field {
    Ok(f) -> {
      let dv = get_string_field(f, "defaultValue")
      assert_true(dv == "10", "defaultValue for limit should be \"10\"")
    }
    Error(_) -> panic as "Expected limit input field"
  }
}

// ============================================================================
// Issue #34: Introspection sub-field traversal tests
// ============================================================================

fn get_nested_field(d: Dynamic, keys: List(String)) -> Dynamic {
  case keys {
    [] -> d
    [key, ..rest] -> {
      let nested =
        decode.run(d, decode.at([key], decode.dynamic))
        |> result.unwrap(types.to_dynamic(Nil))
      get_nested_field(nested, rest)
    }
  }
}

// Test that deeply nested ofType traversal works (issue #34)
pub fn deep_oftype_traversal_test() {
  let test_schema = create_test_schema()
  // Query a field type - the users field returns List(Named("User"))
  // This creates a nested ofType structure: LIST -> User
  let result =
    executor.execute_query(
      test_schema,
      "{ __schema { queryType { fields { name type { kind name ofType { kind name } } } } } }",
    )
  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")
  let query_type = get_nested_field(schema_obj, ["queryType"])
  let fields = get_list_field(query_type, "fields")

  // Find the "users" field which returns List(Named("User"))
  let users_field =
    list.find(fields, fn(f) { get_string_field(f, "name") == "users" })
  case users_field {
    Ok(f) -> {
      let field_type = get_nested_field(f, ["type"])
      // First level: LIST
      assert_true(
        get_string_field(field_type, "kind") == "LIST",
        "users field type should be LIST at top level",
      )
      // ofType should be the User object type
      let of_type = get_nested_field(field_type, ["ofType"])
      assert_true(
        get_string_field(of_type, "kind") == "OBJECT",
        "ofType should be OBJECT",
      )
      assert_true(
        get_string_field(of_type, "name") == "User",
        "ofType name should be User",
      )
    }
    Error(_) -> panic as "Expected users field"
  }
}

// Test that __schema sub-selections are properly processed (issue #34)
pub fn schema_selection_set_test() {
  let test_schema = create_test_schema()
  // Only request specific fields from __schema
  let result =
    executor.execute_query(test_schema, "{ __schema { queryType { name } } }")
  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")

  // queryType should have name
  let query_type = get_nested_field(schema_obj, ["queryType"])
  assert_true(
    get_string_field(query_type, "name") == "Query",
    "queryType.name should be Query",
  )
}

// Test that __type sub-selections traverse nested type info (issue #34)
pub fn type_nested_fields_traversal_test() {
  let test_schema = create_test_schema()
  let result =
    executor.execute_query(
      test_schema,
      "{ __type(name: \"User\") { fields { name type { kind name ofType { kind name } } } } }",
    )
  let data = get_data(result)
  let type_obj = find_field(data, "__type")
  let fields = get_list_field(type_obj, "fields")

  // Find the "id" field (ID type wrapped in NonNull)
  let id_field =
    list.find(fields, fn(f) { get_string_field(f, "name") == "id" })
  case id_field {
    Ok(f) -> {
      let field_type = get_nested_field(f, ["type"])
      // id field type should be NON_NULL
      assert_true(
        get_string_field(field_type, "kind") == "NON_NULL",
        "id field type should be NON_NULL",
      )
      // Check that we can access the nested ofType
      let of_type = get_nested_field(field_type, ["ofType"])
      // ofType should be SCALAR(ID)
      assert_true(
        get_string_field(of_type, "kind") == "SCALAR",
        "ofType.kind for id field should be SCALAR",
      )
      assert_true(
        get_string_field(of_type, "name") == "ID",
        "ofType.name for id field should be ID",
      )
    }
    Error(_) -> panic as "Expected id field on User type"
  }
}

// Test full introspection query with deep nesting (issue #34)
pub fn full_introspection_deep_nesting_test() {
  let test_schema = create_test_schema()
  let query_str =
    "
    {
      __schema {
        types {
          kind
          name
          fields {
            name
            type {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  "
  let result = executor.execute_query(test_schema, query_str)

  // Should complete without errors
  assert_true(result.errors == [], "Full introspection should have no errors")

  let data = get_data(result)
  let schema_obj = find_field(data, "__schema")
  let types_list = get_list_field(schema_obj, "types")

  // Find Query type and verify its fields have proper type info
  let query_type =
    list.find(types_list, fn(t) { get_string_field(t, "kind") == "OBJECT" })
  case query_type {
    Ok(_) -> Nil
    Error(_) -> panic as "Should have at least one OBJECT type"
  }
}
