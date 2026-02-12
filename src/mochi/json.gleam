// mochi/json.gleam
// JSON serialization for Dynamic values
//
// Converts Dynamic values to JSON strings for HTTP responses.
// Supports all JSON types: null, bool, int, float, string, array, object.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list
// Option type is handled via FFI
import gleam/string

// ============================================================================
// Public API
// ============================================================================

/// Encode a Dynamic value to a JSON string
pub fn encode(value: Dynamic) -> String {
  encode_value(value)
}

/// Encode a Dynamic value to a pretty-printed JSON string
pub fn encode_pretty(value: Dynamic, indent: Int) -> String {
  encode_value_pretty(value, 0, indent)
}

// ============================================================================
// FFI for type checking
// ============================================================================

/// Check if a value is null/nil
@external(erlang, "mochi_json_ffi", "is_null")
@external(javascript, "../mochi_json_ffi.mjs", "is_null")
fn is_null(value: Dynamic) -> Bool

/// Check if a value is a boolean
@external(erlang, "mochi_json_ffi", "is_bool")
@external(javascript, "../mochi_json_ffi.mjs", "is_bool")
fn is_bool(value: Dynamic) -> Bool

/// Check if a value is an integer
@external(erlang, "mochi_json_ffi", "is_int")
@external(javascript, "../mochi_json_ffi.mjs", "is_int")
fn is_int(value: Dynamic) -> Bool

/// Check if a value is a float
@external(erlang, "mochi_json_ffi", "is_float")
@external(javascript, "../mochi_json_ffi.mjs", "is_float")
fn is_float(value: Dynamic) -> Bool

/// Check if a value is a string
@external(erlang, "mochi_json_ffi", "is_string")
@external(javascript, "../mochi_json_ffi.mjs", "is_string")
fn is_string(value: Dynamic) -> Bool

/// Check if a value is a list/array
@external(erlang, "mochi_json_ffi", "is_list")
@external(javascript, "../mochi_json_ffi.mjs", "is_list")
fn is_list_value(value: Dynamic) -> Bool

/// Check if a value is a dict/map/object
@external(erlang, "mochi_json_ffi", "is_dict")
@external(javascript, "../mochi_json_ffi.mjs", "is_dict")
fn is_dict(value: Dynamic) -> Bool

/// Check if a value is Option (Some/None)
@external(erlang, "mochi_json_ffi", "is_option")
@external(javascript, "../mochi_json_ffi.mjs", "is_option")
fn is_option(value: Dynamic) -> Bool

/// Extract boolean value
@external(erlang, "mochi_json_ffi", "get_bool")
@external(javascript, "../mochi_json_ffi.mjs", "get_bool")
fn get_bool(value: Dynamic) -> Bool

/// Extract integer value
@external(erlang, "mochi_json_ffi", "get_int")
@external(javascript, "../mochi_json_ffi.mjs", "get_int")
fn get_int(value: Dynamic) -> Int

/// Extract float value
@external(erlang, "mochi_json_ffi", "get_float")
@external(javascript, "../mochi_json_ffi.mjs", "get_float")
fn get_float(value: Dynamic) -> Float

/// Extract string value
@external(erlang, "mochi_json_ffi", "get_string")
@external(javascript, "../mochi_json_ffi.mjs", "get_string")
fn get_string(value: Dynamic) -> String

/// Extract list elements
@external(erlang, "mochi_json_ffi", "get_list")
@external(javascript, "../mochi_json_ffi.mjs", "get_list")
fn get_list(value: Dynamic) -> List(Dynamic)

/// Extract dict entries as list of tuples
@external(erlang, "mochi_json_ffi", "get_dict_entries")
@external(javascript, "../mochi_json_ffi.mjs", "get_dict_entries")
fn get_dict_entries(value: Dynamic) -> List(#(String, Dynamic))

/// Extract Option value - returns the inner value or null
@external(erlang, "mochi_json_ffi", "unwrap_option")
@external(javascript, "../mochi_json_ffi.mjs", "unwrap_option")
fn unwrap_option(value: Dynamic) -> Dynamic

// ============================================================================
// Encoding Implementation
// ============================================================================

fn encode_value(value: Dynamic) -> String {
  case is_null(value) {
    True -> "null"
    False ->
      case is_option(value) {
        True -> encode_value(unwrap_option(value))
        False ->
          case is_bool(value) {
            True ->
              case get_bool(value) {
                True -> "true"
                False -> "false"
              }
            False ->
              case is_int(value) {
                True -> int.to_string(get_int(value))
                False ->
                  case is_float(value) {
                    True -> float.to_string(get_float(value))
                    False ->
                      case is_string(value) {
                        True -> encode_string(get_string(value))
                        False ->
                          case is_list_value(value) {
                            True -> encode_array(get_list(value))
                            False ->
                              case is_dict(value) {
                                True -> encode_object(get_dict_entries(value))
                                False -> "null"
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

fn encode_string(s: String) -> String {
  "\"" <> escape_string(s) <> "\""
}

fn escape_string(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.map(escape_char)
  |> string.concat
}

fn escape_char(c: String) -> String {
  case c {
    "\"" -> "\\\""
    "\\" -> "\\\\"
    "\n" -> "\\n"
    "\r" -> "\\r"
    "\t" -> "\\t"
    _ -> c
  }
}

fn encode_array(items: List(Dynamic)) -> String {
  let encoded =
    items
    |> list.map(encode_value)
    |> string.join(",")
  "[" <> encoded <> "]"
}

fn encode_object(entries: List(#(String, Dynamic))) -> String {
  let encoded =
    entries
    |> list.map(fn(entry) {
      let #(key, val) = entry
      encode_string(key) <> ":" <> encode_value(val)
    })
    |> string.join(",")
  "{" <> encoded <> "}"
}

// ============================================================================
// Pretty Printing Implementation
// ============================================================================

fn encode_value_pretty(value: Dynamic, depth: Int, indent: Int) -> String {
  case is_null(value) {
    True -> "null"
    False ->
      case is_option(value) {
        True -> encode_value_pretty(unwrap_option(value), depth, indent)
        False ->
          case is_bool(value) {
            True ->
              case get_bool(value) {
                True -> "true"
                False -> "false"
              }
            False ->
              case is_int(value) {
                True -> int.to_string(get_int(value))
                False ->
                  case is_float(value) {
                    True -> float.to_string(get_float(value))
                    False ->
                      case is_string(value) {
                        True -> encode_string(get_string(value))
                        False ->
                          case is_list_value(value) {
                            True ->
                              encode_array_pretty(
                                get_list(value),
                                depth,
                                indent,
                              )
                            False ->
                              case is_dict(value) {
                                True ->
                                  encode_object_pretty(
                                    get_dict_entries(value),
                                    depth,
                                    indent,
                                  )
                                False -> "null"
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

fn encode_array_pretty(
  items: List(Dynamic),
  depth: Int,
  indent: Int,
) -> String {
  case items {
    [] -> "[]"
    _ -> {
      let inner_indent = make_indent(depth + 1, indent)
      let outer_indent = make_indent(depth, indent)
      let encoded =
        items
        |> list.map(fn(item) {
          inner_indent <> encode_value_pretty(item, depth + 1, indent)
        })
        |> string.join(",\n")
      "[\n" <> encoded <> "\n" <> outer_indent <> "]"
    }
  }
}

fn encode_object_pretty(
  entries: List(#(String, Dynamic)),
  depth: Int,
  indent: Int,
) -> String {
  case entries {
    [] -> "{}"
    _ -> {
      let inner_indent = make_indent(depth + 1, indent)
      let outer_indent = make_indent(depth, indent)
      let encoded =
        entries
        |> list.map(fn(entry) {
          let #(key, val) = entry
          inner_indent
          <> encode_string(key)
          <> ": "
          <> encode_value_pretty(val, depth + 1, indent)
        })
        |> string.join(",\n")
      "{\n" <> encoded <> "\n" <> outer_indent <> "}"
    }
  }
}

fn make_indent(depth: Int, indent: Int) -> String {
  string.repeat(" ", depth * indent)
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Encode a Dict directly to JSON
pub fn encode_dict(d: Dict(String, Dynamic)) -> String {
  encode_object(dict.to_list(d))
}

/// Encode a List directly to JSON
pub fn encode_list(items: List(Dynamic)) -> String {
  encode_array(items)
}

/// Encode a string value
pub fn encode_string_value(s: String) -> String {
  encode_string(s)
}

/// Encode an int value
pub fn encode_int(n: Int) -> String {
  int.to_string(n)
}

/// Encode a float value
pub fn encode_float_value(f: Float) -> String {
  float.to_string(f)
}

/// Encode a bool value
pub fn encode_bool(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

/// Encode null
pub fn encode_null() -> String {
  "null"
}
