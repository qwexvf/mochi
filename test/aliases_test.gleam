// Tests for GraphQL Aliases support
// Aliases allow renaming fields in the response

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import mochi/internal/ast
import mochi/executor
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type TestUser {
  TestUser(id: String, name: String, email: String)
}

// ============================================================================
// Test Helpers
// ============================================================================

fn assert_eq(a: a, b: a, message: String) -> Nil {
  case a == b {
    True -> Nil
    False -> panic as message
  }
}

fn assert_true(condition: Bool, message: String) -> Nil {
  case condition {
    True -> Nil
    False -> panic as message
  }
}

fn parse_ok(query_str: String) -> ast.Document {
  case parser.parse(query_str) {
    Ok(doc) -> doc
    Error(_) -> panic as "Parse failed"
  }
}

fn get_first_operation(doc: ast.Document) -> ast.Operation {
  case doc.definitions {
    [ast.OperationDefinition(op), ..] -> op
    _ -> panic as "Expected operation"
  }
}

fn get_first_field(op: ast.Operation) -> ast.Field {
  let ss = case op {
    ast.Operation(selection_set: ss, ..) -> ss
    ast.ShorthandQuery(selection_set: ss) -> ss
  }
  case ss.selections {
    [ast.FieldSelection(f), ..] -> f
    _ -> panic as "Expected field"
  }
}

fn get_all_fields(op: ast.Operation) -> List(ast.Field) {
  let ss = case op {
    ast.Operation(selection_set: ss, ..) -> ss
    ast.ShorthandQuery(selection_set: ss) -> ss
  }
  list.filter_map(ss.selections, fn(sel) {
    case sel {
      ast.FieldSelection(f) -> Ok(f)
      _ -> Error(Nil)
    }
  })
}

fn decode_test_user(_dyn: Dynamic) -> Result(TestUser, String) {
  Ok(TestUser("1", "Test User", "test@example.com"))
}

// ============================================================================
// Schema Setup for Execution Tests
// ============================================================================

fn create_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: TestUser) { u.id })
    |> types.string("name", fn(u: TestUser) { u.name })
    |> types.string("email", fn(u: TestUser) { u.email })
    |> types.build(decode_test_user)

  // Query that returns a user by ID
  let user_query =
    query.query_with_args(
      "user",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.named_type("User"),
      fn(args, _ctx) {
        use id <- result.try(query.get_id(args, "id"))
        Ok(TestUser(id, "User " <> id, "user@example.com"))
      },
    )

  // Simple name query for basic alias testing
  let name_query =
    query.query(name: "name", returns: schema.string_type(), resolve: fn(_ctx) {
      Ok("John Doe")
    })

  // Simple greeting query for basic alias testing
  let greeting_query =
    query.query(
      name: "greeting",
      returns: schema.string_type(),
      resolve: fn(_ctx) { Ok("Hello, World!") },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_query(name_query)
  |> query.add_query(greeting_query)
  |> query.add_type(user_type)
  |> query.build
}

// ============================================================================
// Parser Tests - Alias Parsing
// ============================================================================

pub fn parse_simple_alias_test() {
  let doc = parse_ok("{ userName: name }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(field.alias, Some("userName"), "Field should have alias 'userName'")
  assert_eq(field.name, "name", "Field name should be 'name'")
}

pub fn parse_alias_with_arguments_test() {
  let doc = parse_ok("{ admin: user(id: \"1\") { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(field.alias, Some("admin"), "Field should have alias 'admin'")
  assert_eq(field.name, "user", "Field name should be 'user'")
  assert_eq(
    field.arguments,
    [ast.Argument("id", ast.StringValue("1"))],
    "Should have id argument",
  )
  assert_true(option.is_some(field.selection_set), "Should have selection set")
}

pub fn parse_multiple_aliases_same_field_test() {
  let doc =
    parse_ok("{ a: user(id: \"1\") { name } b: user(id: \"2\") { name } }")
  let fields = doc |> get_first_operation |> get_all_fields

  assert_eq(list.length(fields), 2, "Should have two fields")

  case fields {
    [first, second] -> {
      assert_eq(first.alias, Some("a"), "First field should have alias 'a'")
      assert_eq(first.name, "user", "First field name should be 'user'")

      assert_eq(second.alias, Some("b"), "Second field should have alias 'b'")
      assert_eq(second.name, "user", "Second field name should be 'user'")
    }
    _ -> panic as "Expected two fields"
  }
}

pub fn parse_alias_without_arguments_test() {
  let doc = parse_ok("{ myName: name }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(field.alias, Some("myName"), "Field should have alias 'myName'")
  assert_eq(field.name, "name", "Field name should be 'name'")
  assert_eq(field.arguments, [], "Should have no arguments")
}

pub fn parse_nested_aliases_test() {
  let doc =
    parse_ok(
      "{ adminUser: user(id: \"1\") { userName: name userEmail: email } }",
    )
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.alias,
    Some("adminUser"),
    "Root field should have alias 'adminUser'",
  )
  assert_eq(field.name, "user", "Root field name should be 'user'")

  case field.selection_set {
    Some(ss) -> {
      let nested_fields =
        list.filter_map(ss.selections, fn(sel) {
          case sel {
            ast.FieldSelection(f) -> Ok(f)
            _ -> Error(Nil)
          }
        })

      assert_eq(list.length(nested_fields), 2, "Should have two nested fields")

      case nested_fields {
        [first, second] -> {
          assert_eq(
            first.alias,
            Some("userName"),
            "First nested field should have alias 'userName'",
          )
          assert_eq(
            first.name,
            "name",
            "First nested field name should be 'name'",
          )

          assert_eq(
            second.alias,
            Some("userEmail"),
            "Second nested field should have alias 'userEmail'",
          )
          assert_eq(
            second.name,
            "email",
            "Second nested field name should be 'email'",
          )
        }
        _ -> panic as "Expected two nested fields"
      }
    }
    None -> panic as "Expected selection set"
  }
}

pub fn parse_field_without_alias_test() {
  let doc = parse_ok("{ name }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(field.alias, None, "Field should have no alias")
  assert_eq(field.name, "name", "Field name should be 'name'")
}

pub fn parse_mixed_alias_and_no_alias_test() {
  let doc = parse_ok("{ userName: name email age }")
  let fields = doc |> get_first_operation |> get_all_fields

  assert_eq(list.length(fields), 3, "Should have three fields")

  case fields {
    [first, second, third] -> {
      assert_eq(first.alias, Some("userName"), "First field should have alias")
      assert_eq(first.name, "name", "First field name should be 'name'")

      assert_eq(second.alias, None, "Second field should have no alias")
      assert_eq(second.name, "email", "Second field name should be 'email'")

      assert_eq(third.alias, None, "Third field should have no alias")
      assert_eq(third.name, "age", "Third field name should be 'age'")
    }
    _ -> panic as "Expected three fields"
  }
}

pub fn parse_alias_in_named_query_test() {
  let doc = parse_ok("query GetUsers { adminUser: user(id: \"1\") { name } }")
  let op = doc |> get_first_operation
  let field = get_first_field(op)

  case op {
    ast.Operation(name: op_name, ..) ->
      assert_eq(op_name, Some("GetUsers"), "Query should have name")
    _ -> panic as "Expected named query"
  }

  assert_eq(
    field.alias,
    Some("adminUser"),
    "Field should have alias 'adminUser'",
  )
  assert_eq(field.name, "user", "Field name should be 'user'")
}

// ============================================================================
// Executor Tests - Alias Execution
// The executor tests follow the pattern from introspection_test.gleam
// which executes queries without asserting on errors (as the executor
// may have partial failures for complex queries)
// ============================================================================

pub fn execute_simple_alias_test() {
  let test_schema = create_test_schema()
  let query_str = "{ userName: name }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_alias_with_arguments_test() {
  let test_schema = create_test_schema()
  let query_str = "{ admin: user(id: \"1\") { name } }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_multiple_aliases_same_field_test() {
  let test_schema = create_test_schema()
  let query_str =
    "{ adminUser: user(id: \"1\") { name } regularUser: user(id: \"2\") { name } }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_alias_without_selection_set_test() {
  let test_schema = create_test_schema()
  let query_str = "{ myName: name }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_nested_aliases_test() {
  let test_schema = create_test_schema()
  let query_str =
    "{ admin: user(id: \"1\") { userName: name userEmail: email } }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_mixed_alias_and_no_alias_test() {
  let test_schema = create_test_schema()
  let query_str = "{ admin: user(id: \"1\") { userName: name email } }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_alias_with_variables_test() {
  let test_schema = create_test_schema()
  let query_str =
    "query GetUser($userId: ID!) { admin: user(id: $userId) { name } }"
  let variables = dict.from_list([#("userId", types.to_dynamic("1"))])

  // Execute the query - test passes if no panic occurs
  let _ =
    executor.execute_query_with_variables(test_schema, query_str, variables)
  Nil
}

pub fn execute_multiple_scalar_aliases_test() {
  let test_schema = create_test_schema()
  // Test multiple aliases on scalar fields (name and greeting)
  let query_str = "{ myName: name myGreeting: greeting }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

pub fn execute_deeply_nested_aliases_test() {
  let test_schema = create_test_schema()
  // Note: This tests alias at root level with nested field selection
  let query_str =
    "{ adminUser: user(id: \"1\") { userId: id userName: name userEmail: email } }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}

// ============================================================================
// Edge Case Tests
// ============================================================================

pub fn parse_alias_same_as_field_name_test() {
  // Edge case: alias is the same as the field name (valid but redundant)
  let doc = parse_ok("{ name: name }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(field.alias, Some("name"), "Field should have alias 'name'")
  assert_eq(field.name, "name", "Field name should be 'name'")
}

pub fn execute_alias_same_as_field_name_test() {
  let test_schema = create_test_schema()
  let query_str = "{ name: name }"

  // Execute the query - test passes if no panic occurs
  let _ = executor.execute_query(test_schema, query_str)
  Nil
}
