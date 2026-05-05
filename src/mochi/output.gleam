//// Internal typed JSON value used by the encoder.
////
//// Resolvers and field extractors hand `Dynamic` to the encoder. Rather than
//// brute-forcing the runtime shape with a cascade of decoders on every node,
//// we classify once with `dynamic.classify` and convert into this tree. The
//// JSON encoder then walks the tree directly — no re-classification, no
//// re-parsing of its own output for pretty printing, no silent fallbacks
//// when something doesn't fit.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result

pub type Value {
  VBool(Bool)
  VInt(Int)
  VFloat(Float)
  VString(String)
  VNull
  VList(List(Value))
  VObject(List(#(String, Value)))
}

pub type EncodeError {
  /// The encoder hit a `Dynamic` value whose runtime shape it can't
  /// represent in JSON (a tuple, function, pid, etc). `path` is the JSON
  /// pointer to the offender, `kind` is `dynamic.classify` of the value.
  UnsupportedValue(path: String, kind: String)
}

/// Convert a `Dynamic` into the typed value tree, surfacing unsupported
/// shapes as errors instead of silently dropping them.
pub fn from_dynamic(value: Dynamic) -> Result(Value, EncodeError) {
  walk(value, "")
}

fn walk(value: Dynamic, path: String) -> Result(Value, EncodeError) {
  case dynamic.classify(value) {
    "Bool" ->
      decode.run(value, decode.bool)
      |> result.map(VBool)
      |> result.map_error(fn(_) { UnsupportedValue(path, "Bool") })
    "Int" ->
      decode.run(value, decode.int)
      |> result.map(VInt)
      |> result.map_error(fn(_) { UnsupportedValue(path, "Int") })
    "Float" ->
      decode.run(value, decode.float)
      |> result.map(VFloat)
      |> result.map_error(fn(_) { UnsupportedValue(path, "Float") })
    "String" ->
      decode.run(value, decode.string)
      |> result.map(VString)
      |> result.map_error(fn(_) { UnsupportedValue(path, "String") })
    "Nil" -> Ok(VNull)
    "List" ->
      case decode.run(value, decode.list(decode.dynamic)) {
        Error(_) -> Error(UnsupportedValue(path, "List"))
        Ok(items) -> walk_list(items, path, 0, [])
      }
    "Dict" ->
      case decode.run(value, decode.dict(decode.string, decode.dynamic)) {
        Error(_) -> Error(UnsupportedValue(path, "Dict"))
        Ok(d) -> walk_object(d, path)
      }
    other -> Error(UnsupportedValue(path, other))
  }
}

fn walk_list(
  items: List(Dynamic),
  path: String,
  i: Int,
  acc: List(Value),
) -> Result(Value, EncodeError) {
  case items {
    [] -> Ok(VList(list.reverse(acc)))
    [x, ..rest] ->
      case walk(x, path <> "/" <> int_to_str(i)) {
        Error(e) -> Error(e)
        Ok(v) -> walk_list(rest, path, i + 1, [v, ..acc])
      }
  }
}

fn walk_object(
  d: Dict(String, Dynamic),
  path: String,
) -> Result(Value, EncodeError) {
  let pairs = dict.to_list(d)
  walk_pairs(pairs, path, [])
}

fn walk_pairs(
  pairs: List(#(String, Dynamic)),
  path: String,
  acc: List(#(String, Value)),
) -> Result(Value, EncodeError) {
  case pairs {
    [] -> Ok(VObject(list.reverse(acc)))
    [#(k, v), ..rest] ->
      case walk(v, path <> "/" <> k) {
        Error(e) -> Error(e)
        Ok(value) -> walk_pairs(rest, path, [#(k, value), ..acc])
      }
  }
}

fn int_to_str(i: Int) -> String {
  int.to_string(i)
}
