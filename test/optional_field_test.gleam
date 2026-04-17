// Tests for optional_* extractors and the `nullable` decoder helper.
// Regression coverage for the issue where `optional_string` emitted the
// Erlang ADT representation of `Some(_)` (e.g. `[null, "foo"]`) instead of
// the bare value, and where `types.option(None)` -> Erlang nil could not be
// decoded back through `decode.string`.

import gleam/dynamic
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/string
import mochi/json
import mochi/types

pub type Profile {
  Profile(
    id: String,
    name: Option(String),
    age: Option(Int),
    score: Option(Float),
    active: Option(Bool),
  )
}

fn profile_extractor() -> fn(Profile) -> dynamic.Dynamic {
  fn(p: Profile) {
    types.record([
      types.field("id", p.id),
      #("name", types.option(p.name)),
      #("age", types.option(p.age)),
      #("score", types.option(p.score)),
      #("active", types.option(p.active)),
    ])
  }
}

// =============================================================================
// JSON serialization — the bug surface
// =============================================================================

pub fn optional_string_some_serializes_as_string_test() {
  let dyn = types.option(Some("Alice"))
  let assert True = json.encode(dyn) == "\"Alice\""
}

pub fn optional_string_none_serializes_as_null_test() {
  let opt: Option(String) = None
  let dyn = types.option(opt)
  let assert True = json.encode(dyn) == "null"
}

pub fn optional_int_some_serializes_as_int_test() {
  let dyn = types.option(Some(42))
  let assert True = json.encode(dyn) == "42"
}

pub fn optional_int_none_serializes_as_null_test() {
  let opt: Option(Int) = None
  let dyn = types.option(opt)
  let assert True = json.encode(dyn) == "null"
}

pub fn optional_float_some_serializes_as_float_test() {
  let dyn = types.option(Some(3.14))
  let assert True = json.encode(dyn) == "3.14"
}

pub fn optional_bool_some_serializes_as_bool_test() {
  let dyn = types.option(Some(True))
  let assert True = json.encode(dyn) == "true"
}

// =============================================================================
// Round trip via build()'s decoder — the second bug
// =============================================================================

fn decode_profile(dyn: dynamic.Dynamic) -> Result(Profile, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use name <- decode.optional_field(
      "name",
      None,
      types.nullable(decode.string),
    )
    use age <- decode.optional_field("age", None, types.nullable(decode.int))
    use score <- decode.optional_field(
      "score",
      None,
      types.nullable(decode.float),
    )
    use active <- decode.optional_field(
      "active",
      None,
      types.nullable(decode.bool),
    )
    decode.success(Profile(id:, name:, age:, score:, active:))
  }
  case decode.run(dyn, decoder) {
    Ok(p) -> Ok(p)
    Error(_) -> Error("decode failed")
  }
}

pub fn nullable_decoder_handles_null_round_trip_test() {
  let p = Profile(id: "u1", name: None, age: None, score: None, active: None)
  let encoded = profile_extractor()(p)
  let assert Ok(decoded) = decode_profile(encoded)
  let assert True = decoded == p
}

pub fn nullable_decoder_handles_some_round_trip_test() {
  let p =
    Profile(
      id: "u1",
      name: Some("Alice"),
      age: Some(30),
      score: Some(99.5),
      active: Some(True),
    )
  let encoded = profile_extractor()(p)
  let assert Ok(decoded) = decode_profile(encoded)
  let assert True = decoded == p
}

pub fn nullable_decoder_handles_mixed_round_trip_test() {
  let p =
    Profile(
      id: "u1",
      name: Some("Alice"),
      age: None,
      score: Some(99.5),
      active: None,
    )
  let encoded = profile_extractor()(p)
  let assert Ok(decoded) = decode_profile(encoded)
  let assert True = decoded == p
}

// =============================================================================
// Field extractors honor nullable semantics inside a record
// =============================================================================

pub fn record_with_optional_fields_serializes_correctly_test() {
  let none_int: Option(Int) = None
  let dyn =
    types.record([
      types.field("id", "u1"),
      #("name", types.option(Some("Alice"))),
      #("age", types.option(none_int)),
    ])
  let result = json.encode(dyn)
  // dict ordering is not guaranteed — assert the relevant substrings instead
  let assert True = string.contains(result, "\"name\":\"Alice\"")
  let assert True = string.contains(result, "\"age\":null")
}
