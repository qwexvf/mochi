// Tests for custom directive execution
// Verifies that custom directive handlers are called during query execution

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

pub type Message {
  Message(id: String, text: String, count: Int)
}

fn decode_message(_dyn: Dynamic) -> Result(Message, String) {
  Ok(Message("1", "hello", 5))
}

// Helper to try to extract a string from Dynamic
fn try_get_string(value: Dynamic) -> Result(String, String) {
  decode.run(value, decode.string)
  |> result.map_error(fn(_) { "Not a string" })
}

// Helper to try to extract an int from Dynamic
fn try_get_int(value: Dynamic) -> Result(Int, String) {
  decode.run(value, decode.int)
  |> result.map_error(fn(_) { "Not an int" })
}

// ============================================================================
// @uppercase Directive - Transforms string to uppercase
// ============================================================================

fn uppercase_handler(
  _args: dict.Dict(String, Dynamic),
  value: Dynamic,
) -> Result(Dynamic, String) {
  case try_get_string(value) {
    Ok(str) -> Ok(types.to_dynamic(string.uppercase(str)))
    Error(_) -> Ok(value)
    // Pass through non-strings
  }
}

fn build_uppercase_schema() -> schema.Schema {
  let uppercase_directive =
    schema.directive("uppercase", [schema.FieldLocation])
    |> schema.directive_description("Transforms string to uppercase")
    |> schema.directive_handler(uppercase_handler)

  let message_type =
    types.object("Message")
    |> types.id("id", fn(m: Message) { m.id })
    |> types.string("text", fn(m: Message) { m.text })
    |> types.int("count", fn(m: Message) { m.count })
    |> types.build(decode_message)

  let message_query =
    query.query(
      "message",
      schema.Named("Message"),
      fn(_info) { Ok(types.to_dynamic(Message("1", "hello world", 5))) },
      fn(m) { types.to_dynamic(m) },
    )

  query.new()
  |> query.add_query(message_query)
  |> query.add_type(message_type)
  |> query.build
  |> schema.add_directive(uppercase_directive)
}

pub fn uppercase_directive_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        text @uppercase
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn uppercase_directive_on_non_string_passes_through_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        count @uppercase
      }
    }
    "

  // The uppercase directive should pass through non-string values
  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// @multiply Directive - Multiplies a number by a factor
// ============================================================================

fn multiply_handler(
  args: dict.Dict(String, Dynamic),
  value: Dynamic,
) -> Result(Dynamic, String) {
  case dict.get(args, "by") {
    Ok(by_value) -> {
      case try_get_int(by_value), try_get_int(value) {
        Ok(multiplier), Ok(num) -> Ok(types.to_dynamic(num * multiplier))
        _, _ -> Ok(value)
        // Pass through if types don't match
      }
    }
    Error(_) -> Error("@multiply directive requires 'by' argument")
  }
}

fn build_multiply_schema() -> schema.Schema {
  let multiply_directive =
    schema.directive("multiply", [schema.FieldLocation])
    |> schema.directive_description("Multiplies a number by the given factor")
    |> schema.directive_argument(
      schema.arg("by", schema.non_null(schema.int_type()))
      |> schema.arg_description("The multiplier"),
    )
    |> schema.directive_handler(multiply_handler)

  let message_type =
    types.object("Message")
    |> types.id("id", fn(m: Message) { m.id })
    |> types.string("text", fn(m: Message) { m.text })
    |> types.int("count", fn(m: Message) { m.count })
    |> types.build(decode_message)

  let message_query =
    query.query(
      "message",
      schema.Named("Message"),
      fn(_info) { Ok(types.to_dynamic(Message("1", "hello", 5))) },
      fn(m) { types.to_dynamic(m) },
    )

  query.new()
  |> query.add_query(message_query)
  |> query.add_type(message_type)
  |> query.build
  |> schema.add_directive(multiply_directive)
}

pub fn multiply_directive_test() {
  let schema_def = build_multiply_schema()
  let query_str =
    "
    query {
      message {
        id
        count @multiply(by: 3)
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn multiply_directive_with_variable_test() {
  let schema_def = build_multiply_schema()
  let query_str =
    "
    query MultiplyQuery($factor: Int!) {
      message {
        id
        count @multiply(by: $factor)
      }
    }
    "

  let vars = dict.from_list([#("factor", types.to_dynamic(10))])
  let result =
    executor.execute_query_with_variables(schema_def, query_str, vars)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Multiple Directives - Chained Execution
// ============================================================================

fn add_prefix_handler(
  args: dict.Dict(String, Dynamic),
  value: Dynamic,
) -> Result(Dynamic, String) {
  case dict.get(args, "prefix") {
    Ok(prefix_value) -> {
      case try_get_string(prefix_value), try_get_string(value) {
        Ok(prefix), Ok(str) -> Ok(types.to_dynamic(prefix <> str))
        _, _ -> Ok(value)
      }
    }
    Error(_) -> Error("@addPrefix directive requires 'prefix' argument")
  }
}

fn build_chained_schema() -> schema.Schema {
  let uppercase_directive =
    schema.directive("uppercase", [schema.FieldLocation])
    |> schema.directive_handler(uppercase_handler)

  let add_prefix_directive =
    schema.directive("addPrefix", [schema.FieldLocation])
    |> schema.directive_argument(schema.arg("prefix", schema.string_type()))
    |> schema.directive_handler(add_prefix_handler)

  let message_type =
    types.object("Message")
    |> types.id("id", fn(m: Message) { m.id })
    |> types.string("text", fn(m: Message) { m.text })
    |> types.int("count", fn(m: Message) { m.count })
    |> types.build(decode_message)

  let message_query =
    query.query(
      "message",
      schema.Named("Message"),
      fn(_info) { Ok(types.to_dynamic(Message("1", "hello", 5))) },
      fn(m) { types.to_dynamic(m) },
    )

  query.new()
  |> query.add_query(message_query)
  |> query.add_type(message_type)
  |> query.build
  |> schema.add_directive(uppercase_directive)
  |> schema.add_directive(add_prefix_directive)
}

pub fn chained_directives_test() {
  let schema_def = build_chained_schema()
  // Directives are applied left-to-right:
  // 1. @uppercase transforms "hello" -> "HELLO"
  // 2. @addPrefix adds ">>>" -> ">>>HELLO"
  let query_str =
    "
    query {
      message {
        id
        text @uppercase @addPrefix(prefix: \">>>\")
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn chained_directives_reverse_order_test() {
  let schema_def = build_chained_schema()
  // Directives are applied left-to-right:
  // 1. @addPrefix adds ">>>" -> ">>>hello"
  // 2. @uppercase transforms -> ">>>HELLO"
  let query_str =
    "
    query {
      message {
        id
        text @addPrefix(prefix: \">>>\") @uppercase
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Directive Handler Returning Error
// ============================================================================

fn failing_handler(
  _args: dict.Dict(String, Dynamic),
  _value: Dynamic,
) -> Result(Dynamic, String) {
  Error("Directive execution failed intentionally")
}

fn build_failing_directive_schema() -> schema.Schema {
  let fail_directive =
    schema.directive("fail", [schema.FieldLocation])
    |> schema.directive_description("A directive that always fails")
    |> schema.directive_handler(failing_handler)

  let message_type =
    types.object("Message")
    |> types.id("id", fn(m: Message) { m.id })
    |> types.string("text", fn(m: Message) { m.text })
    |> types.int("count", fn(m: Message) { m.count })
    |> types.build(decode_message)

  let message_query =
    query.query(
      "message",
      schema.Named("Message"),
      fn(_info) { Ok(types.to_dynamic(Message("1", "hello", 5))) },
      fn(m) { types.to_dynamic(m) },
    )

  query.new()
  |> query.add_query(message_query)
  |> query.add_type(message_type)
  |> query.build
  |> schema.add_directive(fail_directive)
}

pub fn directive_handler_error_test() {
  let schema_def = build_failing_directive_schema()
  let query_str =
    "
    query {
      message {
        id
        text @fail
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  // Should have an error
  should.be_false(list.is_empty(result.errors))
}

pub fn directive_handler_error_stops_chain_test() {
  let schema_def =
    build_failing_directive_schema()
    |> schema.add_directive(
      schema.directive("uppercase", [schema.FieldLocation])
      |> schema.directive_handler(uppercase_handler),
    )

  let query_str =
    "
    query {
      message {
        id
        text @fail @uppercase
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  // Should have an error because @fail comes first
  should.be_false(list.is_empty(result.errors))
}

// ============================================================================
// Directive Without Handler (pass-through)
// ============================================================================

fn build_no_handler_schema() -> schema.Schema {
  // A directive with no handler should just pass through values
  let log_directive =
    schema.directive("log", [schema.FieldLocation])
    |> schema.directive_description("Logs field access (no-op in tests)")

  let message_type =
    types.object("Message")
    |> types.id("id", fn(m: Message) { m.id })
    |> types.string("text", fn(m: Message) { m.text })
    |> types.int("count", fn(m: Message) { m.count })
    |> types.build(decode_message)

  let message_query =
    query.query(
      "message",
      schema.Named("Message"),
      fn(_info) { Ok(types.to_dynamic(Message("1", "hello", 5))) },
      fn(m) { types.to_dynamic(m) },
    )

  query.new()
  |> query.add_query(message_query)
  |> query.add_type(message_type)
  |> query.build
  |> schema.add_directive(log_directive)
}

pub fn directive_without_handler_passes_through_test() {
  let schema_def = build_no_handler_schema()
  let query_str =
    "
    query {
      message {
        id
        text @log
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Unknown Directive (not in schema)
// ============================================================================

pub fn unknown_directive_passes_through_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        text @unknownDirective
      }
    }
    "

  // Unknown directives should be lenient and pass through
  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Built-in Directives Still Work
// ============================================================================

pub fn skip_directive_still_works_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        text @skip(if: true)
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn include_directive_still_works_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        text @include(if: true) @uppercase
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn builtin_and_custom_combined_test() {
  let schema_def = build_uppercase_schema()
  let query_str =
    "
    query {
      message {
        id
        text @skip(if: false) @uppercase
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}
