// Tests for JSON serialization

import gleam/dict
import gleam/dynamic
import gleam/string
import mochi/error
import mochi/json
import mochi/output
import mochi/response
import mochi/types

fn ok_encode(v: dynamic.Dynamic) -> String {
  let assert Ok(s) = json.encode(v)
  s
}

fn ok_encode_pretty(v: dynamic.Dynamic, indent: Int) -> String {
  let assert Ok(s) = json.encode_pretty(v, indent)
  s
}

// ============================================================================
// Primitive Type Tests
// ============================================================================

pub fn encode_null_test() {
  let assert True = ok_encode(types.to_dynamic(Nil)) == "null"
}

pub fn encode_true_test() {
  let assert True = ok_encode(types.to_dynamic(True)) == "true"
}

pub fn encode_false_test() {
  let assert True = ok_encode(types.to_dynamic(False)) == "false"
}

pub fn encode_positive_int_test() {
  let assert True = ok_encode(types.to_dynamic(42)) == "42"
}

pub fn encode_negative_int_test() {
  let assert True = ok_encode(types.to_dynamic(-123)) == "-123"
}

pub fn encode_zero_test() {
  let assert True = ok_encode(types.to_dynamic(0)) == "0"
}

pub fn encode_float_test() {
  let assert True = string.contains(ok_encode(types.to_dynamic(3.14)), "3.14")
}

pub fn encode_negative_float_test() {
  let assert True = string.contains(ok_encode(types.to_dynamic(-2.5)), "-2.5")
}

// ============================================================================
// String Tests
// ============================================================================

pub fn encode_simple_string_test() {
  let assert True = ok_encode(types.to_dynamic("hello")) == "\"hello\""
}

pub fn encode_empty_string_test() {
  let assert True = ok_encode(types.to_dynamic("")) == "\"\""
}

pub fn encode_string_with_quotes_test() {
  let assert True =
    ok_encode(types.to_dynamic("say \"hello\"")) == "\"say \\\"hello\\\"\""
}

pub fn encode_string_with_backslash_test() {
  let assert True =
    ok_encode(types.to_dynamic("path\\to\\file")) == "\"path\\\\to\\\\file\""
}

pub fn encode_string_with_newline_test() {
  let assert True =
    ok_encode(types.to_dynamic("line1\nline2")) == "\"line1\\nline2\""
}

pub fn encode_string_with_tab_test() {
  let assert True =
    ok_encode(types.to_dynamic("col1\tcol2")) == "\"col1\\tcol2\""
}

pub fn encode_string_with_carriage_return_test() {
  let assert True =
    ok_encode(types.to_dynamic("line1\rline2")) == "\"line1\\rline2\""
}

// ============================================================================
// Array/List Tests
// ============================================================================

pub fn encode_empty_list_test() {
  let assert True = ok_encode(types.to_dynamic([])) == "[]"
}

pub fn encode_int_list_test() {
  let assert True = ok_encode(types.to_dynamic([1, 2, 3])) == "[1,2,3]"
}

pub fn encode_string_list_test() {
  let assert True =
    ok_encode(types.to_dynamic(["a", "b", "c"])) == "[\"a\",\"b\",\"c\"]"
}

pub fn encode_mixed_list_test() {
  let items = [
    types.to_dynamic(1),
    types.to_dynamic("two"),
    types.to_dynamic(True),
  ]
  let assert True = ok_encode(types.to_dynamic(items)) == "[1,\"two\",true]"
}

pub fn encode_nested_list_test() {
  let assert True =
    ok_encode(types.to_dynamic([[1, 2], [3, 4]])) == "[[1,2],[3,4]]"
}

// ============================================================================
// Object/Dict Tests
// ============================================================================

pub fn encode_empty_dict_test() {
  let assert True = ok_encode(types.to_dynamic(dict.new())) == "{}"
}

pub fn encode_simple_dict_test() {
  let d = dict.from_list([#("name", types.to_dynamic("John"))])
  let assert True = ok_encode(types.to_dynamic(d)) == "{\"name\":\"John\"}"
}

pub fn encode_dict_with_multiple_keys_test() {
  let d =
    dict.from_list([
      #("name", types.to_dynamic("John")),
      #("age", types.to_dynamic(30)),
    ])
  let result = ok_encode(types.to_dynamic(d))
  let assert True =
    string.contains(result, "\"name\":\"John\"")
    && string.contains(result, "\"age\":30")
}

pub fn encode_nested_dict_test() {
  let inner = dict.from_list([#("city", types.to_dynamic("NYC"))])
  let outer = dict.from_list([#("address", types.to_dynamic(inner))])
  let result = ok_encode(types.to_dynamic(outer))
  let assert True =
    string.contains(result, "\"address\"")
    && string.contains(result, "\"city\":\"NYC\"")
}

// ============================================================================
// Pretty Print Tests
// ============================================================================

pub fn encode_pretty_simple_object_test() {
  let d = dict.from_list([#("name", types.to_dynamic("John"))])
  let result = ok_encode_pretty(types.to_dynamic(d), 2)
  let assert True = string.contains(result, "\n")
}

pub fn encode_pretty_empty_array_test() {
  let result = ok_encode_pretty(types.to_dynamic([]), 2)
  let assert True = result == "[]"
}

pub fn encode_pretty_empty_object_test() {
  let result = ok_encode_pretty(types.to_dynamic(dict.new()), 2)
  let assert True = result == "{}"
}

pub fn encode_pretty_array_with_indent_test() {
  let result = ok_encode_pretty(types.to_dynamic([1, 2, 3]), 4)
  // 4-space indentation per nesting level
  let assert True = string.contains(result, "    1")
}

// ============================================================================
// Error surfacing — unsupported runtime shapes do NOT silently turn into null
// ============================================================================

pub fn encode_unsupported_value_returns_error_test() {
  // A 2-tuple isn't representable as JSON and isn't a list/map either.
  // Old behaviour: silently emitted "null". New behaviour: explicit error.
  let assert Error(output.UnsupportedValue(..)) =
    json.encode(types.to_dynamic(#(1, 2)))
}

pub fn encode_unsupported_value_describes_path_test() {
  // Tuple nested inside a dict — error path should point at it.
  let bad = dict.from_list([#("inner", types.to_dynamic(#("oops", "tuple")))])
  let assert Error(err) = json.encode(types.to_dynamic(bad))
  let assert True = string.contains(json.describe_error(err), "/inner")
}

// ============================================================================
// GraphQL Response Tests
// ============================================================================

pub fn response_to_json_success_test() {
  let data =
    types.to_dynamic(dict.from_list([#("user", types.to_dynamic("john"))]))
  let resp = response.success(data)
  let assert True = string.contains(response.to_json(resp), "\"data\"")
}

pub fn response_to_json_failure_test() {
  let errors = [error.new("Something went wrong")]
  let resp = response.failure(errors)
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"errors\"")
    && string.contains(result, "Something went wrong")
}

pub fn response_to_json_partial_test() {
  let data =
    types.to_dynamic(dict.from_list([#("partial", types.to_dynamic(True))]))
  let errors = [error.new("Warning")]
  let resp = response.partial(data, errors)
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"data\"") && string.contains(result, "\"errors\"")
}

pub fn response_to_json_with_extensions_test() {
  let data = types.to_dynamic("test")
  let resp =
    response.success(data)
    |> response.with_extension("requestId", types.to_dynamic("req-123"))
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"extensions\"")
    && string.contains(result, "requestId")
}

pub fn response_to_json_pretty_test() {
  let data =
    types.to_dynamic(dict.from_list([#("user", types.to_dynamic("john"))]))
  let resp = response.success(data)
  let assert True = string.contains(response.to_json_pretty(resp), "\n")
}

// ============================================================================
// Complex Nested Structure Tests
// ============================================================================

pub fn encode_complex_structure_test() {
  let user =
    dict.from_list([
      #("id", types.to_dynamic("1")),
      #("name", types.to_dynamic("John Doe")),
      #("age", types.to_dynamic(30)),
      #("active", types.to_dynamic(True)),
      #("email", types.to_dynamic("john@example.com")),
      #("phone", types.to_dynamic(Nil)),
      #("roles", types.to_dynamic(["admin", "user"])),
    ])
  let result = ok_encode(types.to_dynamic(user))
  let assert True =
    string.contains(result, "\"id\":\"1\"")
    && string.contains(result, "\"name\":\"John Doe\"")
    && string.contains(result, "\"age\":30")
    && string.contains(result, "\"active\":true")
    && string.contains(result, "\"email\":\"john@example.com\"")
    && string.contains(result, "\"roles\"")
}

pub fn encode_array_of_objects_test() {
  let users = [
    dict.from_list([
      #("id", types.to_dynamic("1")),
      #("name", types.to_dynamic("Alice")),
    ]),
    dict.from_list([
      #("id", types.to_dynamic("2")),
      #("name", types.to_dynamic("Bob")),
    ]),
  ]
  let result = ok_encode(types.to_dynamic(users))
  let assert True =
    string.contains(result, "Alice")
    && string.contains(result, "Bob")
    && string.starts_with(result, "[")
    && string.ends_with(result, "]")
}

// ============================================================================
// Error Response JSON Tests
// ============================================================================

pub fn error_with_path_json_test() {
  let err = error.new_at("Field not found", ["user", "email"])
  let resp = response.failure([err])
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"path\"") && string.contains(result, "\"user\"")
}

pub fn error_with_location_json_test() {
  let err =
    error.new("Syntax error")
    |> error.at_location(10, 5)
  let resp = response.failure([err])
  let assert True = string.contains(response.to_json(resp), "\"locations\"")
}

pub fn error_with_extensions_json_test() {
  let err =
    error.new("Custom error")
    |> error.with_code("CUSTOM_ERROR")
  let resp = response.failure([err])
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"extensions\"")
    && string.contains(result, "CUSTOM_ERROR")
}
