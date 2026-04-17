// Tests for GraphQL Variables support

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{Some}
import mochi/ast
import mochi/executor
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

fn assert_eq(a: a, b: a, message: String) -> Nil {
  case a == b {
    True -> Nil
    False -> panic as message
  }
}

fn parse_ok(query: String) -> ast.Document {
  case parser.parse(query) {
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

fn get_variable_definitions(op: ast.Operation) -> List(ast.VariableDefinition) {
  case op {
    ast.Operation(variable_definitions: vars, ..) -> vars
    ast.ShorthandQuery(_) -> []
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

// ============================================================================
// Parser Tests - Variable Definitions
// ============================================================================

pub fn parse_query_with_variables_test() {
  let doc = parse_ok("query GetUser($id: ID!) { user(id: $id) { name } }")
  let vars = doc |> get_first_operation |> get_variable_definitions

  assert_eq(
    vars,
    [
      ast.VariableDefinition(
        "id",
        ast.NonNullType(ast.NamedType("ID")),
        option.None,
        [],
      ),
    ],
    "Should have one ID! variable",
  )
}

pub fn parse_multiple_variables_test() {
  let doc =
    parse_ok("query GetUsers($limit: Int, $offset: Int = 0) { users { id } }")
  let vars = doc |> get_first_operation |> get_variable_definitions

  assert_eq(list.length(vars), 2, "Should have two variables")

  case vars {
    [first, second] -> {
      assert_eq(first.variable, "limit", "First should be limit")
      assert_eq(second.variable, "offset", "Second should be offset")
      assert_eq(
        second.default_value,
        Some(ast.IntValue(0)),
        "Offset should have default 0",
      )
    }
    _ -> panic as "Expected two variables"
  }
}

import gleam/list

pub fn parse_list_type_variable_test() {
  let doc =
    parse_ok("query GetByIds($ids: [ID!]!) { users(ids: $ids) { name } }")
  let vars = doc |> get_first_operation |> get_variable_definitions

  case vars {
    [ast.VariableDefinition(variable: "ids", type_: t, ..)] ->
      assert_eq(
        t,
        ast.NonNullType(ast.ListType(ast.NonNullType(ast.NamedType("ID")))),
        "Type should be [ID!]!",
      )
    _ -> panic as "Expected ids variable"
  }
}

// ============================================================================
// Parser Tests - Arguments
// ============================================================================

pub fn parse_field_with_string_argument_test() {
  let doc = parse_ok("{ user(id: \"123\") { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.arguments,
    [ast.Argument("id", ast.StringValue("123"))],
    "Should have string argument",
  )
}

pub fn parse_field_with_int_argument_test() {
  let doc = parse_ok("{ users(limit: 10) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.arguments,
    [ast.Argument("limit", ast.IntValue(10))],
    "Should have int argument",
  )
}

pub fn parse_field_with_variable_argument_test() {
  let doc = parse_ok("query GetUser($id: ID!) { user(id: $id) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.arguments,
    [ast.Argument("id", ast.VariableValue("id"))],
    "Should have variable argument",
  )
}

pub fn parse_field_with_boolean_argument_test() {
  let doc = parse_ok("{ users(active: true) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.arguments,
    [ast.Argument("active", ast.BooleanValue(True))],
    "Should have boolean argument",
  )
}

pub fn parse_field_with_null_argument_test() {
  let doc = parse_ok("{ user(email: null) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(
    field.arguments,
    [ast.Argument("email", ast.NullValue)],
    "Should have null argument",
  )
}

pub fn parse_field_with_list_argument_test() {
  let doc = parse_ok("{ users(ids: [\"1\", \"2\", \"3\"]) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  let expected_list =
    ast.ListValue([
      ast.StringValue("1"),
      ast.StringValue("2"),
      ast.StringValue("3"),
    ])
  assert_eq(
    field.arguments,
    [ast.Argument("ids", expected_list)],
    "Should have list argument",
  )
}

pub fn parse_field_with_object_argument_test() {
  let doc = parse_ok("{ createUser(input: {name: \"John\", age: 30}) { id } }")
  let field = doc |> get_first_operation |> get_first_field

  let expected_obj =
    ast.ObjectValue([
      ast.ObjectField("name", ast.StringValue("John")),
      ast.ObjectField("age", ast.IntValue(30)),
    ])
  assert_eq(
    field.arguments,
    [ast.Argument("input", expected_obj)],
    "Should have object argument",
  )
}

pub fn parse_multiple_arguments_test() {
  let doc = parse_ok("{ users(limit: 10, offset: 20, active: true) { name } }")
  let field = doc |> get_first_operation |> get_first_field

  assert_eq(list.length(field.arguments), 3, "Should have three arguments")
}

// ============================================================================
// Executor Tests - Variable Substitution
// ============================================================================

pub type TestUser {
  TestUser(id: String, name: String, age: Int)
}

fn decode_test_user(_dyn: Dynamic) -> Result(TestUser, String) {
  Ok(TestUser("1", "Test User", 25))
}

fn create_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: TestUser) { u.id })
    |> types.string("name", fn(u: TestUser) { u.name })
    |> types.int("age", fn(u: TestUser) { u.age })
    |> types.build(decode_test_user)

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
      fn(id, _ctx) { Ok(TestUser(id, "User " <> id, 25)) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

import gleam/result

pub fn executor_with_variables_test() {
  let test_schema = create_test_schema()
  let query_str = "query GetUser($id: ID!) { user(id: $id) { name } }"
  let variables = dict.from_list([#("id", types.to_dynamic("123"))])

  // Should execute without panicking
  let _ =
    executor.execute_query_with_variables(test_schema, query_str, variables)
  Nil
}
