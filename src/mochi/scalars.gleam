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

fn is_digit_char(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

pub fn date_time() -> ScalarType {
  let validate = fn(value: Dynamic) -> Result(Dynamic, String) {
    validate_string(value, fn(s) {
      let valid =
        string.length(s) > 0
        && { string.contains(s, "T") || string.length(s) == 10 }
        && {
          string.first(s) |> result.map(is_digit_char) |> result.unwrap(False)
        }
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
      let chars = string.to_graphemes(s)
      case string.length(s) == 10 {
        False -> Error("Date must be in YYYY-MM-DD format")
        True -> {
          let dash4 = case chars |> drop(4) |> first_char() {
            Ok(c) -> c == "-"
            Error(_) -> False
          }
          let dash7 = case chars |> drop(7) |> first_char() {
            Ok(c) -> c == "-"
            Error(_) -> False
          }
          case dash4 && dash7 {
            True -> Ok(s)
            False -> Error("Date must be in YYYY-MM-DD format")
          }
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

fn drop(chars: List(String), n: Int) -> List(String) {
  case n, chars {
    0, rest -> rest
    _, [] -> []
    n, [_, ..rest] -> drop(rest, n - 1)
  }
}

fn first_char(chars: List(String)) -> Result(String, Nil) {
  case chars {
    [c, ..] -> Ok(c)
    [] -> Error(Nil)
  }
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
      let chars = string.to_graphemes(s)
      let d8 = case chars |> drop(8) |> first_char() {
        Ok(c) -> c == "-"
        Error(_) -> False
      }
      let d13 = case chars |> drop(13) |> first_char() {
        Ok(c) -> c == "-"
        Error(_) -> False
      }
      let d18 = case chars |> drop(18) |> first_char() {
        Ok(c) -> c == "-"
        Error(_) -> False
      }
      let d23 = case chars |> drop(23) |> first_char() {
        Ok(c) -> c == "-"
        Error(_) -> False
      }
      case string.length(s) == 36 && d8 && d13 && d18 && d23 {
        True -> Ok(s)
        False ->
          Error(
            "UUID must be 36 characters with dashes at positions 8, 13, 18, 23",
          )
      }
    })
  }
  schema.scalar("UUID")
  |> schema.scalar_description("UUID scalar")
  |> schema.serialize(fn(value) { Ok(value) })
  |> schema.parse_value(validate)
  |> schema.parse_literal(validate)
}
