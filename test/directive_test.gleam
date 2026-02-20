import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/option
import gleam/result
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

pub type User {
  User(id: String, name: String, active: Bool)
}

fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.bool("active", fn(u: User) { u.active })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_info) { Ok(types.to_dynamic(User("1", "Alice", True))) },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

fn decode_user(_dyn: dynamic.Dynamic) -> Result(User, String) {
  Ok(User("1", "Alice", True))
}

fn execute_query(
  schema_def: schema.Schema,
  query_str: String,
) -> executor.ExecutionResult {
  executor.execute_query(schema_def, query_str)
}

fn execute_query_with_vars(
  schema_def: schema.Schema,
  query_str: String,
  vars: dict.Dict(String, dynamic.Dynamic),
) -> executor.ExecutionResult {
  executor.execute_query_with_variables(schema_def, query_str, vars)
}

// ============================================================================
// @skip Directive Tests
// ============================================================================

pub fn skip_directive_true_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name @skip(if: true)
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn skip_directive_false_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name @skip(if: false)
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn skip_directive_with_variable_true_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser($skipName: Boolean!) {
      user {
        id
        name @skip(if: $skipName)
      }
    }
    "

  let vars = dict.from_list([#("skipName", types.to_dynamic(True))])
  let result = execute_query_with_vars(schema_def, query_str, vars)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn skip_directive_with_variable_false_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser($skipName: Boolean!) {
      user {
        id
        name @skip(if: $skipName)
      }
    }
    "

  let vars = dict.from_list([#("skipName", types.to_dynamic(False))])
  let result = execute_query_with_vars(schema_def, query_str, vars)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// @include Directive Tests
// ============================================================================

pub fn include_directive_true_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name @include(if: true)
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn include_directive_false_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name @include(if: false)
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn include_directive_with_variable_true_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser($includeName: Boolean!) {
      user {
        id
        name @include(if: $includeName)
      }
    }
    "

  let vars = dict.from_list([#("includeName", types.to_dynamic(True))])
  let result = execute_query_with_vars(schema_def, query_str, vars)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn include_directive_with_variable_false_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser($includeName: Boolean!) {
      user {
        id
        name @include(if: $includeName)
      }
    }
    "

  let vars = dict.from_list([#("includeName", types.to_dynamic(False))])
  let result = execute_query_with_vars(schema_def, query_str, vars)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Multiple Directives Tests
// ============================================================================

pub fn multiple_directives_on_field_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name @skip(if: false) @include(if: true)
        active
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn no_directives_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name
        active
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Data Content Assertions
// ============================================================================

/// Extract a string field from a nested path in execution result data.
/// The executor returns data as a list of field dicts; we search through them.
fn get_field_from_result(
  result: executor.ExecutionResult,
  path: List(String),
) -> Result(dynamic.Dynamic, Nil) {
  case result.data {
    option.None -> Error(Nil)
    option.Some(data) -> get_nested(data, path)
  }
}

fn get_nested(
  data: dynamic.Dynamic,
  path: List(String),
) -> Result(dynamic.Dynamic, Nil) {
  case path {
    [] -> Ok(data)
    [key, ..rest] -> {
      // Try direct dict access first
      let direct =
        decode.run(data, decode.at([key], decode.dynamic))
        |> result.map_error(fn(_) { Nil })
      case direct {
        Ok(v) -> get_nested(v, rest)
        Error(_) ->
          // Try list of dicts (executor merges results as a list)
          case decode.run(data, decode.list(decode.dynamic)) {
            Ok(items) ->
              list.find_map(items, fn(item) {
                case decode.run(item, decode.at([key], decode.dynamic)) {
                  Ok(v) ->
                    get_nested(v, rest) |> result.map_error(fn(_) { Nil })
                  Error(_) -> Error(Nil)
                }
              })
            Error(_) -> Error(Nil)
          }
      }
    }
  }
}

// @skip(if: true) — field must be absent from response
pub fn skip_true_field_absent_test() {
  let schema_def = build_test_schema()
  let result =
    execute_query(schema_def, "query { user { id name @skip(if: true) } }")
  should.equal(result.errors, [])
  // name must NOT be present in the user object
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_error(name_field)
}

// @skip(if: false) — field must be present in response
pub fn skip_false_field_present_test() {
  let schema_def = build_test_schema()
  let result =
    execute_query(schema_def, "query { user { id name @skip(if: false) } }")
  should.equal(result.errors, [])
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_ok(name_field)
}

// @include(if: true) — field must be present in response
pub fn include_true_field_present_test() {
  let schema_def = build_test_schema()
  let result =
    execute_query(schema_def, "query { user { id name @include(if: true) } }")
  should.equal(result.errors, [])
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_ok(name_field)
}

// @include(if: false) — field must be absent from response
pub fn include_false_field_absent_test() {
  let schema_def = build_test_schema()
  let result =
    execute_query(schema_def, "query { user { id name @include(if: false) } }")
  should.equal(result.errors, [])
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_error(name_field)
}

// @skip(if: $var) with var=True — field must be absent
pub fn skip_variable_true_field_absent_test() {
  let schema_def = build_test_schema()
  let vars = dict.from_list([#("skip", types.to_dynamic(True))])
  let result =
    execute_query_with_vars(
      schema_def,
      "query Q($skip: Boolean!) { user { id name @skip(if: $skip) } }",
      vars,
    )
  should.equal(result.errors, [])
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_error(name_field)
}

// @include(if: $var) with var=False — field must be absent
pub fn include_variable_false_field_absent_test() {
  let schema_def = build_test_schema()
  let vars = dict.from_list([#("inc", types.to_dynamic(False))])
  let result =
    execute_query_with_vars(
      schema_def,
      "query Q($inc: Boolean!) { user { id name @include(if: $inc) } }",
      vars,
    )
  should.equal(result.errors, [])
  let name_field = get_field_from_result(result, ["user", "name"])
  should.be_error(name_field)
}
