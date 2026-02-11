import gleam/dict
import gleam/dynamic
import gleam/option
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

fn execute_query(schema_def: schema.Schema, query_str: String) -> executor.ExecutionResult {
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
