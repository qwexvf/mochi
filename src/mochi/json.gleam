// mochi/json.gleam
// JSON serialization for Dynamic values

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/string

// ============================================================================
// Public API
// ============================================================================

/// Encode a Dynamic value to a JSON string
pub fn encode(value: Dynamic) -> String {
  dynamic_to_json(value)
  |> json.to_string
}

/// Encode a Dynamic value to a pretty-printed JSON string
pub fn encode_pretty(value: Dynamic, indent: Int) -> String {
  let json_str =
    dynamic_to_json(value)
    |> json.to_string
  format_json(json_str, indent)
}

// ============================================================================
// Dynamic to Json Conversion
// ============================================================================

fn dynamic_to_json(value: Dynamic) -> json.Json {
  // Try bool
  case decode.run(value, decode.bool) {
    Ok(b) -> json.bool(b)
    Error(_) ->
      // Try int
      case decode.run(value, decode.int) {
        Ok(i) -> json.int(i)
        Error(_) ->
          // Try float
          case decode.run(value, decode.float) {
            Ok(f) -> json.float(f)
            Error(_) ->
              // Try string
              case decode.run(value, decode.string) {
                Ok(s) -> json.string(s)
                Error(_) ->
                  // Try list
                  case decode.run(value, decode.list(decode.dynamic)) {
                    Ok(items) -> json.array(items, dynamic_to_json)
                    Error(_) ->
                      // Try dict
                      case
                        decode.run(
                          value,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(d) -> {
                          let entries =
                            dict.to_list(d)
                            |> list.map(fn(kv) {
                              #(kv.0, dynamic_to_json(kv.1))
                            })
                          json.object(entries)
                        }
                        // Unknown type -> null
                        Error(_) -> json.null()
                      }
                  }
              }
          }
      }
  }
}

// ============================================================================
// Pretty Printing
// ============================================================================

fn format_json(s: String, indent: Int) -> String {
  do_format(s, 0, indent, "", False)
}

fn do_format(
  s: String,
  depth: Int,
  indent: Int,
  acc: String,
  in_string: Bool,
) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#(c, rest)) ->
      case in_string {
        True ->
          case c {
            "\"" -> do_format(rest, depth, indent, acc <> c, False)
            "\\" ->
              case string.pop_grapheme(rest) {
                Ok(#(next, rest2)) ->
                  do_format(rest2, depth, indent, acc <> c <> next, True)
                Error(_) -> acc <> c
              }
            _ -> do_format(rest, depth, indent, acc <> c, True)
          }
        False ->
          case c {
            "\"" -> do_format(rest, depth, indent, acc <> c, True)
            "{" ->
              case string.first(rest) {
                Ok("}") -> do_format(rest, depth, indent, acc <> "{", False)
                _ -> {
                  let d = depth + 1
                  do_format(rest, d, indent, acc <> "{\n" <> spaces(d, indent), False)
                }
              }
            "}" -> {
              let d = depth - 1
              do_format(rest, d, indent, acc <> "\n" <> spaces(d, indent) <> "}", False)
            }
            "[" ->
              case string.first(rest) {
                Ok("]") -> do_format(rest, depth, indent, acc <> "[", False)
                _ -> {
                  let d = depth + 1
                  do_format(rest, d, indent, acc <> "[\n" <> spaces(d, indent), False)
                }
              }
            "]" -> {
              let d = depth - 1
              do_format(rest, d, indent, acc <> "\n" <> spaces(d, indent) <> "]", False)
            }
            "," -> do_format(rest, depth, indent, acc <> ",\n" <> spaces(depth, indent), False)
            ":" -> do_format(rest, depth, indent, acc <> ": ", False)
            " " | "\t" | "\n" | "\r" -> do_format(rest, depth, indent, acc, False)
            _ -> do_format(rest, depth, indent, acc <> c, False)
          }
      }
  }
}

fn spaces(depth: Int, indent: Int) -> String {
  string.repeat(" ", depth * indent)
}

// ============================================================================
// Convenience Functions
// ============================================================================

pub fn encode_dict(d: Dict(String, Dynamic)) -> String {
  let entries =
    dict.to_list(d)
    |> list.map(fn(kv) { #(kv.0, dynamic_to_json(kv.1)) })
  json.object(entries) |> json.to_string
}

pub fn encode_list(items: List(Dynamic)) -> String {
  json.array(items, dynamic_to_json) |> json.to_string
}

pub fn encode_string_value(s: String) -> String {
  json.string(s) |> json.to_string
}

pub fn encode_int(n: Int) -> String {
  int.to_string(n)
}

pub fn encode_float_value(f: Float) -> String {
  float.to_string(f)
}

pub fn encode_bool(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

pub fn encode_null() -> String {
  "null"
}
