// Regression tests for GraphQL spec §6.4.1 (CoerceVariableValues):
// when a variable has an explicit null value and is declared with a
// nullable type, coercion must succeed and store null directly.
// Passing explicit null previously raised
// `Variable "$x": Expected String, got incompatible type`.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/string
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Schema wiring — one resolver per scalar type, each takes a matching
// nullable variable except `echoRequired` which takes a NonNull String.
// ============================================================================

fn build_schema() -> schema.Schema {
  let echo_string =
    query.query_with_args(
      name: "echoString",
      args: [query.arg("x", schema.string_type())],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  let echo_id =
    query.query_with_args(
      name: "echoId",
      args: [query.arg("x", schema.id_type())],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  let echo_int =
    query.query_with_args(
      name: "echoInt",
      args: [query.arg("x", schema.int_type())],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  let echo_float =
    query.query_with_args(
      name: "echoFloat",
      args: [query.arg("x", schema.float_type())],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  let echo_bool =
    query.query_with_args(
      name: "echoBool",
      args: [query.arg("x", schema.boolean_type())],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  let echo_required =
    query.query_with_args(
      name: "echoRequired",
      args: [query.arg("x", schema.non_null(schema.string_type()))],
      returns: schema.string_type(),
      resolve: fn(_args, _ctx) { Ok("ok") },
    )

  query.new()
  |> query.add_query(echo_string)
  |> query.add_query(echo_id)
  |> query.add_query(echo_int)
  |> query.add_query(echo_float)
  |> query.add_query(echo_bool)
  |> query.add_query(echo_required)
  |> query.build
}

fn null_dyn() -> Dynamic {
  types.to_dynamic(Nil)
}

fn error_message(err: executor.ExecutionError) -> String {
  case err {
    executor.ValidationError(message: m, ..) -> m
    executor.ResolverError(message: m, ..) -> m
    executor.TypeError(message: m, ..) -> m
    executor.NullValueError(message: m, ..) -> m
    executor.RichResolverError(graphql_error: e, ..) -> e.message
  }
}

fn has_error_containing(
  result: executor.ExecutionResult,
  needle: String,
) -> Bool {
  list.any(result.errors, fn(e) { string.contains(error_message(e), needle) })
}

// Filter out errors about the root type / unrelated execution noise — we
// only care about Variable "$x" coercion errors for these regression tests.
fn variable_errors(result: executor.ExecutionResult) -> List(String) {
  result.errors
  |> list.map(error_message)
  |> list.filter(fn(m) { string.contains(m, "Variable \"$") })
}

// ============================================================================
// 1. Explicit null for nullable variables coerces successfully
// ============================================================================

pub fn explicit_null_for_nullable_string_variable_coerces_test() {
  let sch = build_schema()
  let q = "query Q($x: String) { echoString(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)

  // Coercion must NOT produce a Variable "$x" error anymore.
  assert variable_errors(result) == []
}

pub fn explicit_null_for_nullable_id_variable_coerces_test() {
  let sch = build_schema()
  let q = "query Q($x: ID) { echoId(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)
  assert variable_errors(result) == []
}

pub fn explicit_null_for_nullable_int_variable_coerces_test() {
  let sch = build_schema()
  let q = "query Q($x: Int) { echoInt(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)
  assert variable_errors(result) == []
}

pub fn explicit_null_for_nullable_float_variable_coerces_test() {
  let sch = build_schema()
  let q = "query Q($x: Float) { echoFloat(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)
  assert variable_errors(result) == []
}

pub fn explicit_null_for_nullable_boolean_variable_coerces_test() {
  let sch = build_schema()
  let q = "query Q($x: Boolean) { echoBool(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)
  assert variable_errors(result) == []
}

// ============================================================================
// 2. Explicit null for NonNull variable produces a clear error
// ============================================================================

pub fn explicit_null_for_non_null_variable_errors_test() {
  let sch = build_schema()
  let q = "query Q($x: String!) { echoRequired(x: $x) }"
  let vars = dict.from_list([#("x", null_dyn())])
  let result = executor.execute_query_with_variables(sch, q, vars)

  assert has_error_containing(result, "Expected non-null value but got null")
}

// ============================================================================
// 3. Non-null values still type-check correctly
// ============================================================================

pub fn non_null_string_value_for_nullable_string_variable_ok_test() {
  let sch = build_schema()
  let q = "query Q($x: String) { echoString(x: $x) }"
  let vars = dict.from_list([#("x", types.to_dynamic("hello"))])
  let result = executor.execute_query_with_variables(sch, q, vars)

  assert variable_errors(result) == []
}

pub fn number_value_for_nullable_string_variable_errors_test() {
  let sch = build_schema()
  let q = "query Q($x: String) { echoString(x: $x) }"
  let vars = dict.from_list([#("x", types.to_dynamic(42))])
  let result = executor.execute_query_with_variables(sch, q, vars)

  assert has_error_containing(result, "Expected String, got incompatible type")
}

// ============================================================================
// 4. Missing variable with nullable type and no default coerces to null
// ============================================================================

pub fn missing_nullable_variable_coerces_to_null_test() {
  let sch = build_schema()
  let q = "query Q($x: String) { echoString(x: $x) }"
  // Variable key not present in the dict at all.
  let vars = dict.new()
  let result = executor.execute_query_with_variables(sch, q, vars)

  assert variable_errors(result) == []
}

pub fn missing_non_null_variable_errors_test() {
  let sch = build_schema()
  let q = "query Q($x: String!) { echoRequired(x: $x) }"
  let vars = dict.new()
  let result = executor.execute_query_with_variables(sch, q, vars)

  assert has_error_containing(result, "is not provided")
}
