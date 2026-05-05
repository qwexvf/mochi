import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/result
import gleam/string
import mochi/schema.{type ScalarType}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

fn validate_string(
  value: Dynamic,
  validate: fn(String) -> Result(String, String),
) -> Result(Dynamic, String) {
  use s <- result.try(
    decode.run(value, decode.string)
    |> result.map_error(fn(_) { "Expected a string value" }),
  )
  use validated <- result.map(validate(s))
  to_dynamic(validated)
}

// All scalar formats accepted here are ASCII (ISO 8601 dates, UUIDs,
// http(s) URLs, RFC 5322-ish emails). Operate on bytes directly via
// `bit_array.byte_size` and byte access — `string.length` would walk the
// UTF-8 to count graphemes, and `string.to_graphemes` would allocate a
// list per character. Both are O(n) per check; the byte path is O(1) for
// length and O(1) per indexed lookup.

fn byte_at(bytes: BitArray, i: Int) -> Result(Int, Nil) {
  case bit_array.slice(bytes, i, 1) {
    Ok(<<b>>) -> Ok(b)
    _ -> Error(Nil)
  }
}

fn is_digit_byte(b: Int) -> Bool {
  b >= 48 && b <= 57
}

pub fn date_time() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      let bytes = bit_array.from_string(s)
      let len = bit_array.byte_size(bytes)
      let starts_with_digit = case byte_at(bytes, 0) {
        Ok(b) -> is_digit_byte(b)
        Error(_) -> False
      }
      let valid =
        len > 0 && { string.contains(s, "T") || len == 10 } && starts_with_digit
      case valid {
        True -> Ok(s)
        False -> Error("DateTime must be a valid ISO 8601 string")
      }
    })
  }
  schema.scalar("DateTime")
  |> schema.scalar_description("ISO 8601 date-time scalar")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}

pub fn date() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      let bytes = bit_array.from_string(s)
      case bit_array.byte_size(bytes) == 10 {
        False -> Error("Date must be in YYYY-MM-DD format")
        True ->
          case byte_at(bytes, 4), byte_at(bytes, 7) {
            Ok(45), Ok(45) -> Ok(s)
            _, _ -> Error("Date must be in YYYY-MM-DD format")
          }
      }
    })
  }
  schema.scalar("Date")
  |> schema.scalar_description("ISO 8601 date scalar (YYYY-MM-DD)")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}

pub fn json() -> ScalarType {
  schema.scalar("JSON")
  |> schema.scalar_description("Arbitrary JSON value")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(fn(value) { Ok(value) })
  |> schema.parse_literal(fn(value) { Ok(value) })
}

pub fn email() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      let parts = string.split(s, "@")
      case parts {
        [_, domain] ->
          case string.contains(domain, ".") {
            True -> Ok(s)
            False -> Error("Email must contain a domain with a dot")
          }
        _ -> Error("Email must contain exactly one @")
      }
    })
  }
  schema.scalar("Email")
  |> schema.scalar_description("Email address scalar")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}

pub fn url() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      case
        string.starts_with(s, "http://") || string.starts_with(s, "https://")
      {
        True -> Ok(s)
        False -> Error("URL must start with http:// or https://")
      }
    })
  }
  schema.scalar("URL")
  |> schema.scalar_description("URL scalar")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}

pub fn uuid() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      let bytes = bit_array.from_string(s)
      case bit_array.byte_size(bytes) == 36 {
        False ->
          Error(
            "UUID must be 36 characters with dashes at positions 8, 13, 18, 23",
          )
        True ->
          case
            byte_at(bytes, 8),
            byte_at(bytes, 13),
            byte_at(bytes, 18),
            byte_at(bytes, 23)
          {
            // 45 is '-'
            Ok(45), Ok(45), Ok(45), Ok(45) -> Ok(s)
            _, _, _, _ ->
              Error(
                "UUID must be 36 characters with dashes at positions 8, 13, 18, 23",
              )
          }
      }
    })
  }
  schema.scalar("UUID")
  |> schema.scalar_description("UUID scalar")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}
