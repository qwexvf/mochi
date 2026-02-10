// Tests for GraphQL Introspection support

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{Some}
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
  let query_str = "
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
  let _ = executor.execute_query(test_schema, "{ __schema { queryType { name } } }")
  Nil
}

pub fn execute_type_introspection_test() {
  let test_schema = create_test_schema()
  let _ = executor.execute_query(test_schema, "{ __type(name: \"User\") { name kind } }")
  Nil
}

pub fn execute_type_introspection_with_variable_test() {
  let test_schema = create_test_schema()
  let query_str = "query GetType($name: String!) { __type(name: $name) { name kind } }"
  let variables = dict.from_list([#("name", types.to_dynamic("User"))])
  let _ = executor.execute_query_with_variables(test_schema, query_str, variables)
  Nil
}

// ============================================================================
// Schema Type Tests
// ============================================================================

pub fn schema_has_user_type_test() {
  let test_schema = create_test_schema()

  let user_type = assert_ok(
    dict.get(test_schema.types, "User"),
    "Schema should have User type",
  )

  case user_type {
    schema.ObjectTypeDef(obj) -> assert_true(obj.name == "User", "Type name should be User")
    _ -> panic as "Should be ObjectTypeDef"
  }
}

pub fn schema_has_role_enum_test() {
  let test_schema = create_test_schema()

  let role_type = assert_ok(
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
      let _ = assert_ok(dict.get(query_type.fields, "users"), "Should have users field")
      let _ = assert_ok(dict.get(query_type.fields, "user"), "Should have user field")
      Nil
    }
    _ -> panic as "Schema should have query type"
  }
}
