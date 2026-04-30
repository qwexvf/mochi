import gleam/dynamic/decode
import gleam/string
import mochi/decoders as md
import mochi/types

// ---------------------------------------------------------------------------
// build_with
// ---------------------------------------------------------------------------

pub type User {
  User(id: String, name: String)
}

fn decode_user_inner() -> decode.Decoder(User) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  decode.success(User(id: id, name: name))
}

pub fn build_with_ok_test() {
  let dyn =
    types.record([
      types.field("id", "u1"),
      types.field("name", "Alice"),
    ])
  case md.build_with(decode_user_inner(), "User", dyn) {
    Ok(User(id: "u1", name: "Alice")) -> Nil
    Ok(_) -> panic as "Wrong values decoded"
    Error(e) -> panic as { "Should succeed: " <> e }
  }
}

pub fn build_with_error_includes_type_name_test() {
  // Missing required field — inner decoder fails. The error string
  // must mention the type name so logs are searchable. Asserting
  // contains rather than equality so the inner error format can
  // evolve without breaking this test.
  let dyn = types.record([types.field("id", "u1")])
  case md.build_with(decode_user_inner(), "User", dyn) {
    Error(e) ->
      case string.contains(e, "User") {
        True -> Nil
        False -> panic as { "Type name missing from error: " <> e }
      }
    Ok(_) -> panic as "Should fail on missing field"
  }
}

pub fn build_with_error_surfaces_inner_detail_test() {
  // A non-trivial decode failure should bubble at least one piece of
  // the inner decoder's complaint into the user-visible string.
  let dyn = types.record([types.field("id", "u1")])
  case md.build_with(decode_user_inner(), "User", dyn) {
    Error(e) ->
      // Either the missing field name, the expected type, or the
      // word "expected" should appear — any of these is enough to
      // confirm the inner detail wasn't dropped.
      case
        string.contains(e, "name")
        || string.contains(e, "expected")
        || string.contains(e, "String")
      {
        True -> Nil
        False -> panic as { "Inner detail missing from error: " <> e }
      }
    Ok(_) -> panic as "Should fail on missing field"
  }
}

// ---------------------------------------------------------------------------
// optional_string / optional_int / optional_bool
// ---------------------------------------------------------------------------

pub type Settings {
  Settings(label: String, count: Int, enabled: Bool)
}

fn decode_settings() -> decode.Decoder(Settings) {
  use label <- md.optional_string("label")
  use count <- md.optional_int("count")
  use enabled <- md.optional_bool("enabled")
  decode.success(Settings(label: label, count: count, enabled: enabled))
}

pub fn optional_present_test() {
  let dyn =
    types.record([
      types.field("label", "ready"),
      types.field("count", 7),
      types.field("enabled", True),
    ])
  case decode.run(dyn, decode_settings()) {
    Ok(Settings(label: "ready", count: 7, enabled: True)) -> Nil
    Ok(_) -> panic as "Wrong values decoded"
    Error(_) -> panic as "Should succeed when fields are present"
  }
}

pub fn optional_absent_uses_defaults_test() {
  // Empty record — all three fields fall back to defaults.
  let dyn = types.record([])
  case decode.run(dyn, decode_settings()) {
    Ok(Settings(label: "", count: 0, enabled: False)) -> Nil
    Ok(_) -> panic as "Defaults not applied correctly"
    Error(_) -> panic as "Should succeed with defaults"
  }
}

// ---------------------------------------------------------------------------
// list_filtering
// ---------------------------------------------------------------------------

pub type Tag {
  Tag(name: String)
}

fn decode_tag(dyn) -> Result(Tag, String) {
  let decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(Tag(name: name))
  }
  md.build_with(decoder, "Tag", dyn)
}

pub type Post {
  Post(title: String, tags: List(Tag))
}

fn decode_post() -> decode.Decoder(Post) {
  use title <- decode.field("title", decode.string)
  use tags <- md.list_filtering("tags", decode_tag)
  decode.success(Post(title: title, tags: tags))
}

pub fn list_filtering_all_valid_test() {
  let dyn =
    types.record([
      types.field("title", "Hello"),
      #(
        "tags",
        types.to_dynamic([
          types.record([types.field("name", "rust")]),
          types.record([types.field("name", "gleam")]),
        ]),
      ),
    ])
  case decode.run(dyn, decode_post()) {
    Ok(Post(title: "Hello", tags: [Tag("rust"), Tag("gleam")])) -> Nil
    Ok(_) -> panic as "Wrong tags decoded"
    Error(_) -> panic as "Should succeed with valid items"
  }
}

pub fn list_filtering_drops_malformed_test() {
  // First item is missing "name" — should be dropped silently.
  // Second item is well-formed — should survive.
  let dyn =
    types.record([
      types.field("title", "Hello"),
      #(
        "tags",
        types.to_dynamic([
          types.record([types.field("wrong_key", "rust")]),
          types.record([types.field("name", "gleam")]),
        ]),
      ),
    ])
  case decode.run(dyn, decode_post()) {
    Ok(Post(title: "Hello", tags: [Tag("gleam")])) -> Nil
    Ok(p) -> panic as { "Wrong filter result, got " <> string_of_post(p) }
    Error(_) -> panic as "Should succeed with partial valid items"
  }
}

pub fn list_filtering_absent_is_empty_test() {
  // No "tags" key at all — defaults to [].
  let dyn = types.record([types.field("title", "Hello")])
  case decode.run(dyn, decode_post()) {
    Ok(Post(title: "Hello", tags: [])) -> Nil
    Ok(_) -> panic as "Wrong default value"
    Error(_) -> panic as "Should succeed with absent list"
  }
}

fn string_of_post(p: Post) -> String {
  let tag_names = case p.tags {
    [] -> ""
    [Tag(n), ..] -> n
  }
  p.title <> " / " <> tag_names
}
