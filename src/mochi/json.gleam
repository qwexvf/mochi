//// JSON serialization for Dynamic values.
////
//// The encoder converts Dynamic into the typed value tree in `mochi/output`
//// once, then walks the tree. Errors propagate instead of getting silently
//// swallowed as `null`.

import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/json as gleam_json
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree.{type StringTree}
import mochi/output.{
  type Value, VBool, VFloat, VInt, VList, VNull, VObject, VString,
}

pub type EncodeError =
  output.EncodeError

/// Human-readable description of an encode failure.
pub fn describe_error(err: EncodeError) -> String {
  case err {
    output.UnsupportedValue(path: p, kind: k) ->
      "cannot encode value of kind '" <> k <> "' at path '" <> p <> "'"
  }
}

/// Encode a Dynamic value to a compact JSON string.
///
/// Returns an error when the value contains a runtime shape that can't be
/// represented in JSON (functions, tuples, pids, references, …) — instead
/// of silently emitting `null` for it.
pub fn encode(value: Dynamic) -> Result(String, EncodeError) {
  use v <- result.map(output.from_dynamic(value))
  encode_value(v)
}

/// Encode a Dynamic value to a pretty-printed JSON string with the given
/// indent width. Walks the value tree directly — does not re-parse JSON.
pub fn encode_pretty(value: Dynamic, indent: Int) -> Result(String, EncodeError) {
  use v <- result.map(output.from_dynamic(value))
  pretty_value(v, indent, 0)
  |> string_tree.to_string
}

// ---------------------------------------------------------------------------
// Compact emission
// ---------------------------------------------------------------------------
//
// Defers to `gleam/json` for everything but the structure, so string escaping
// and number formatting stay correct on both targets.

fn encode_value(v: Value) -> String {
  to_gleam_json(v)
  |> gleam_json.to_string
}

fn to_gleam_json(v: Value) -> gleam_json.Json {
  case v {
    VBool(b) -> gleam_json.bool(b)
    VInt(i) -> gleam_json.int(i)
    VFloat(f) -> gleam_json.float(f)
    VString(s) -> gleam_json.string(s)
    VNull -> gleam_json.null()
    VList(items) -> gleam_json.array(items, to_gleam_json)
    VObject(fields) ->
      list.map(fields, fn(kv) { #(kv.0, to_gleam_json(kv.1)) })
      |> gleam_json.object
  }
}

// ---------------------------------------------------------------------------
// Pretty emission
// ---------------------------------------------------------------------------
//
// Walks the typed tree once into a `StringTree` (O(n)). Numbers/bools/null go
// through `gleam/json` so we don't reimplement number formatting; strings go
// through `gleam_json.string |> gleam_json.to_string` so escaping stays
// identical to the compact path.

fn pretty_value(v: Value, indent: Int, depth: Int) -> StringTree {
  case v {
    VBool(True) -> string_tree.from_string("true")
    VBool(False) -> string_tree.from_string("false")
    VNull -> string_tree.from_string("null")
    VInt(i) -> string_tree.from_string(int.to_string(i))
    VFloat(f) -> string_tree.from_string(float.to_string(f))
    VString(s) -> json_string(s)
    VList([]) -> string_tree.from_string("[]")
    VList(items) -> pretty_list(items, indent, depth)
    VObject([]) -> string_tree.from_string("{}")
    VObject(fields) -> pretty_object(fields, indent, depth)
  }
}

fn pretty_list(items: List(Value), indent: Int, depth: Int) -> StringTree {
  let inner_pad = pad(indent, depth + 1)
  let close_pad = pad(indent, depth)
  let parts =
    list.index_map(items, fn(item, i) {
      let prefix = case i {
        0 -> string_tree.new()
        _ -> string_tree.from_string(",\n")
      }
      prefix
      |> string_tree.append_tree(inner_pad)
      |> string_tree.append_tree(pretty_value(item, indent, depth + 1))
    })
  string_tree.from_string("[\n")
  |> string_tree.append_tree(string_tree.concat(parts))
  |> string_tree.append("\n")
  |> string_tree.append_tree(close_pad)
  |> string_tree.append("]")
}

fn pretty_object(
  fields: List(#(String, Value)),
  indent: Int,
  depth: Int,
) -> StringTree {
  let inner_pad = pad(indent, depth + 1)
  let close_pad = pad(indent, depth)
  let parts =
    list.index_map(fields, fn(kv, i) {
      let prefix = case i {
        0 -> string_tree.new()
        _ -> string_tree.from_string(",\n")
      }
      prefix
      |> string_tree.append_tree(inner_pad)
      |> string_tree.append_tree(json_string(kv.0))
      |> string_tree.append(": ")
      |> string_tree.append_tree(pretty_value(kv.1, indent, depth + 1))
    })
  string_tree.from_string("{\n")
  |> string_tree.append_tree(string_tree.concat(parts))
  |> string_tree.append("\n")
  |> string_tree.append_tree(close_pad)
  |> string_tree.append("}")
}

fn pad(indent: Int, depth: Int) -> StringTree {
  case indent * depth {
    0 -> string_tree.new()
    n -> string_tree.from_string(string.repeat(" ", n))
  }
}

fn json_string(s: String) -> StringTree {
  gleam_json.string(s)
  |> gleam_json.to_string
  |> string_tree.from_string
}
