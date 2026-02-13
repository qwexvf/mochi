// Tests for JSON serialization

import gleam/dict
import gleam/string
import mochi/error
import mochi/json
import mochi/response
import mochi/types

// ============================================================================
// Primitive Type Tests
// ============================================================================

pub fn encode_null_test() {
  let result = json.encode(types.to_dynamic(Nil))
  let assert True = result == "null"
}

pub fn encode_true_test() {
  let result = json.encode(types.to_dynamic(True))
  let assert True = result == "true"
}

pub fn encode_false_test() {
  let result = json.encode(types.to_dynamic(False))
  let assert True = result == "false"
}

pub fn encode_positive_int_test() {
  let result = json.encode(types.to_dynamic(42))
  let assert True = result == "42"
}

pub fn encode_negative_int_test() {
  let result = json.encode(types.to_dynamic(-123))
  let assert True = result == "-123"
}

pub fn encode_zero_test() {
  let result = json.encode(types.to_dynamic(0))
  let assert True = result == "0"
}

pub fn encode_float_test() {
  let result = json.encode(types.to_dynamic(3.14))
  // Floats may have different string representations
  let assert True = string.contains(result, "3.14")
}

pub fn encode_negative_float_test() {
  let result = json.encode(types.to_dynamic(-2.5))
  let assert True = string.contains(result, "-2.5")
}

// ============================================================================
// String Tests
// ============================================================================

pub fn encode_simple_string_test() {
  let result = json.encode(types.to_dynamic("hello"))
  let assert True = result == "\"hello\""
}

pub fn encode_empty_string_test() {
  let result = json.encode(types.to_dynamic(""))
  let assert True = result == "\"\""
}

pub fn encode_string_with_quotes_test() {
  let result = json.encode(types.to_dynamic("say \"hello\""))
  let assert True = result == "\"say \\\"hello\\\"\""
}

pub fn encode_string_with_backslash_test() {
  let result = json.encode(types.to_dynamic("path\\to\\file"))
  let assert True = result == "\"path\\\\to\\\\file\""
}

pub fn encode_string_with_newline_test() {
  let result = json.encode(types.to_dynamic("line1\nline2"))
  let assert True = result == "\"line1\\nline2\""
}

pub fn encode_string_with_tab_test() {
  let result = json.encode(types.to_dynamic("col1\tcol2"))
  let assert True = result == "\"col1\\tcol2\""
}

pub fn encode_string_with_carriage_return_test() {
  let result = json.encode(types.to_dynamic("line1\rline2"))
  let assert True = result == "\"line1\\rline2\""
}

// ============================================================================
// Array/List Tests
// ============================================================================

pub fn encode_empty_list_test() {
  let result = json.encode(types.to_dynamic([]))
  let assert True = result == "[]"
}

pub fn encode_int_list_test() {
  let result = json.encode(types.to_dynamic([1, 2, 3]))
  let assert True = result == "[1,2,3]"
}

pub fn encode_string_list_test() {
  let result = json.encode(types.to_dynamic(["a", "b", "c"]))
  let assert True = result == "[\"a\",\"b\",\"c\"]"
}

pub fn encode_mixed_list_test() {
  // Note: Gleam lists are homogeneous, so we use Dynamic
  let items = [
    types.to_dynamic(1),
    types.to_dynamic("two"),
    types.to_dynamic(True),
  ]
  let result = json.encode(types.to_dynamic(items))
  let assert True = result == "[1,\"two\",true]"
}

pub fn encode_nested_list_test() {
  let nested = [[1, 2], [3, 4]]
  let result = json.encode(types.to_dynamic(nested))
  let assert True = result == "[[1,2],[3,4]]"
}

// ============================================================================
// Object/Dict Tests
// ============================================================================

pub fn encode_empty_dict_test() {
  let result = json.encode(types.to_dynamic(dict.new()))
  let assert True = result == "{}"
}

pub fn encode_simple_dict_test() {
  let d = dict.from_list([#("name", types.to_dynamic("John"))])
  let result = json.encode(types.to_dynamic(d))
  let assert True = result == "{\"name\":\"John\"}"
}

pub fn encode_dict_with_multiple_keys_test() {
  let d =
    dict.from_list([
      #("name", types.to_dynamic("John")),
      #("age", types.to_dynamic(30)),
    ])
  let result = json.encode(types.to_dynamic(d))
  // Dict order is not guaranteed, so check both possibilities
  let assert True =
    string.contains(result, "\"name\":\"John\"")
    && string.contains(result, "\"age\":30")
}

pub fn encode_nested_dict_test() {
  let inner = dict.from_list([#("city", types.to_dynamic("NYC"))])
  let outer = dict.from_list([#("address", types.to_dynamic(inner))])
  let result = json.encode(types.to_dynamic(outer))
  let assert True =
    string.contains(result, "\"address\"")
    && string.contains(result, "\"city\":\"NYC\"")
}

// ============================================================================
// Note: Option types should be unwrapped before encoding to JSON
// ============================================================================

// ============================================================================
// Pretty Print Tests
// ============================================================================

pub fn encode_pretty_simple_object_test() {
  let d = dict.from_list([#("name", types.to_dynamic("John"))])
  let result = json.encode_pretty(types.to_dynamic(d), 2)
  // Should contain newlines and indentation
  let assert True = string.contains(result, "\n")
}

pub fn encode_pretty_empty_array_test() {
  let result = json.encode_pretty(types.to_dynamic([]), 2)
  // Empty arrays - just check it's valid JSON array
  let assert True = string.contains(result, "[") && string.contains(result, "]")
}

pub fn encode_pretty_empty_object_test() {
  let result = json.encode_pretty(types.to_dynamic(dict.new()), 2)
  // Empty objects - just check it's valid JSON object
  let assert True = string.contains(result, "{") && string.contains(result, "}")
}

pub fn encode_pretty_array_with_indent_test() {
  let result = json.encode_pretty(types.to_dynamic([1, 2, 3]), 4)
  // Check for 4-space indentation
  let assert True = string.contains(result, "    ")
}

// ============================================================================
// GraphQL Response Tests
// ============================================================================

pub fn response_to_json_success_test() {
  let data =
    types.to_dynamic(dict.from_list([#("user", types.to_dynamic("john"))]))
  let resp = response.success(data)
  let result = response.to_json(resp)
  let assert True = string.contains(result, "\"data\"")
}

pub fn response_to_json_failure_test() {
  let errors = [error.error("Something went wrong")]
  let resp = response.failure(errors)
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"errors\"")
    && string.contains(result, "Something went wrong")
}

pub fn response_to_json_partial_test() {
  let data =
    types.to_dynamic(dict.from_list([#("partial", types.to_dynamic(True))]))
  let errors = [error.error("Warning")]
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
  let result = response.to_json_pretty(resp)
  let assert True = string.contains(result, "\n")
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
  let result = json.encode(types.to_dynamic(user))

  // Verify all expected fields are present
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
  let result = json.encode(types.to_dynamic(users))
  let assert True =
    string.contains(result, "Alice")
    && string.contains(result, "Bob")
    && string.starts_with(result, "[")
    && string.ends_with(result, "]")
}

// ============================================================================
// Convenience Function Tests
// ============================================================================

pub fn encode_dict_direct_test() {
  let d = dict.from_list([#("key", types.to_dynamic("value"))])
  let result = json.encode_dict(d)
  let assert True = result == "{\"key\":\"value\"}"
}

pub fn encode_list_direct_test() {
  let items = [types.to_dynamic(1), types.to_dynamic(2), types.to_dynamic(3)]
  let result = json.encode_list(items)
  let assert True = result == "[1,2,3]"
}

pub fn encode_string_value_test() {
  let result = json.encode_string_value("test")
  let assert True = result == "\"test\""
}

pub fn encode_int_direct_test() {
  let result = json.encode_int(42)
  let assert True = result == "42"
}

pub fn encode_float_direct_test() {
  let result = json.encode_float_value(3.14)
  let assert True = string.contains(result, "3.14")
}

pub fn encode_bool_true_direct_test() {
  let result = json.encode_bool(True)
  let assert True = result == "true"
}

pub fn encode_bool_false_direct_test() {
  let result = json.encode_bool(False)
  let assert True = result == "false"
}

pub fn encode_null_direct_test() {
  let result = json.encode_null()
  let assert True = result == "null"
}

// ============================================================================
// Error Response JSON Tests
// ============================================================================

pub fn error_with_path_json_test() {
  let err = error.error_at("Field not found", ["user", "email"])
  let errors = [err]
  let resp = response.failure(errors)
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"path\"") && string.contains(result, "\"user\"")
}

pub fn error_with_location_json_test() {
  let err =
    error.error("Syntax error")
    |> error.at_location(10, 5)
  let errors = [err]
  let resp = response.failure(errors)
  let result = response.to_json(resp)
  let assert True = string.contains(result, "\"locations\"")
}

pub fn error_with_extensions_json_test() {
  let err =
    error.error("Custom error")
    |> error.with_code("CUSTOM_ERROR")
  let errors = [err]
  let resp = response.failure(errors)
  let result = response.to_json(resp)
  let assert True =
    string.contains(result, "\"extensions\"")
    && string.contains(result, "CUSTOM_ERROR")
}
