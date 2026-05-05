//// Typed accessor for GraphQL field arguments.
////
//// Resolvers receive arguments as `Args`, an opaque wrapper around a
//// `Dict(String, Dynamic)`. Use the `get_*` family to read fields with
//// type checking; `to_dict` is a back-compat escape hatch.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type Args {
  Args(inner: Dict(String, Dynamic))
}

/// Why an argument lookup failed. Resolvers normally translate this into
/// a `mochi/error.GraphQLError` at the boundary — kept structural here so
/// `mochi/args` doesn't have to depend on the error module (which would
/// cycle through `types` and `schema`).
pub type ArgError {
  Missing(key: String)
  WrongType(key: String, expected: String)
  InvalidInput(key: String)
}

/// Render an `ArgError` as the same kind of message the legacy
/// `query.get_*` family produced, so downstream error reporting doesn't
/// change.
pub fn error_message(err: ArgError) -> String {
  case err {
    Missing(key) -> "Missing required argument: " <> key
    WrongType(key, expected) ->
      "Invalid type for argument '" <> key <> "': expected " <> expected
    InvalidInput(key) -> "Invalid input for '" <> key <> "'"
  }
}

/// Wrap a raw dict as `Args`. The executor calls this at the resolver
/// boundary; user code rarely needs it directly.
pub fn from_dict(d: Dict(String, Dynamic)) -> Args {
  Args(d)
}

/// Unwrap to the underlying dict — for code bridging to APIs that still
/// take `Dict(String, Dynamic)`. Avoid in new code.
pub fn to_dict(a: Args) -> Dict(String, Dynamic) {
  a.inner
}

pub fn get_string(a: Args, key: String) -> Result(String, ArgError) {
  required(a, key, decode.string, "String")
}

pub fn get_id(a: Args, key: String) -> Result(String, ArgError) {
  required(a, key, decode.string, "ID")
}

pub fn get_int(a: Args, key: String) -> Result(Int, ArgError) {
  required(a, key, decode.int, "Int")
}

pub fn get_float(a: Args, key: String) -> Result(Float, ArgError) {
  required(a, key, decode.float, "Float")
}

pub fn get_bool(a: Args, key: String) -> Result(Bool, ArgError) {
  required(a, key, decode.bool, "Bool")
}

pub fn get_optional_string(a: Args, key: String) -> Option(String) {
  optional(a, key, decode.string)
}

pub fn get_optional_int(a: Args, key: String) -> Option(Int) {
  optional(a, key, decode.int)
}

pub fn get_optional_float(a: Args, key: String) -> Option(Float) {
  optional(a, key, decode.float)
}

pub fn get_optional_bool(a: Args, key: String) -> Option(Bool) {
  optional(a, key, decode.bool)
}

pub fn get_string_list(a: Args, key: String) -> Result(List(String), ArgError) {
  required(a, key, decode.list(decode.string), "[String]")
}

pub fn get_int_list(a: Args, key: String) -> Result(List(Int), ArgError) {
  required(a, key, decode.list(decode.int), "[Int]")
}

pub fn decode_input(
  a: Args,
  key: String,
  decoder: decode.Decoder(b),
) -> Result(b, ArgError) {
  case dict.get(a.inner, key) {
    Error(_) -> Error(Missing(key))
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) { InvalidInput(key) })
  }
}

fn required(
  a: Args,
  key: String,
  decoder: decode.Decoder(b),
  type_label: String,
) -> Result(b, ArgError) {
  case dict.get(a.inner, key) {
    Error(_) -> Error(Missing(key))
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) { WrongType(key, type_label) })
  }
}

fn optional(
  a: Args,
  key: String,
  decoder: decode.Decoder(b),
) -> Option(b) {
  case dict.get(a.inner, key) {
    Error(_) -> None
    Ok(value) ->
      case decode.run(value, decoder) {
        Ok(v) -> Some(v)
        Error(_) -> None
      }
  }
}
