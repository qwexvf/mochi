//// Small ergonomic helpers for the decoder callback that
//// `mochi/types.{build}` requires on every object type.
////
//// mochi round-trips field values through `Dynamic` during query
//// execution, which means each `ObjectType` needs a
//// `fn(Dynamic) -> Result(t, String)` decoder. Writing those by hand
//// produces a lot of mirror-image boilerplate — list fields with
//// per-item decoding, optional fields with sensible defaults, and the
//// standard `case decode.run { Ok -> Ok; Error -> Error("Failed to
//// decode <Type>") }` wrap.
////
//// These helpers exist for one reason: to make a typical schema's
//// `build` callback short and readable without giving up any of
//// `gleam_stdlib`'s `decode` typing. They're additive — every
//// existing decoder keeps working unchanged.
////
//// All helpers follow `gleam_stdlib`'s `decode` continuation-passing
//// convention so they compose with `use` bindings:
////
//// ```gleam
//// use email <- md.optional_string("email")
//// ```
////
//// > **Output decoders only.** The `optional_*` helpers conflate
//// > "field absent" with "field present but defaulted." That's
//// > correct for `mochi/types.{build}` callbacks (mochi only invokes
//// > them on schema-conforming output values) but wrong for input
//// > validation — never use these to decode mutation arguments where
//// > "user sent nothing" must differ from "user sent an empty value."
//// > For input validation, use `gleam_stdlib`'s `decode.field` /
//// > `decode.optional_field` directly so you control the default.
////
//// ## Example
////
//// ```gleam
//// import gleam/dynamic/decode
//// import mochi/decoders as md
//// import mochi/schema
//// import mochi/types
////
//// pub fn user_type() -> schema.ObjectType {
////   types.object("User")
////   |> types.id("id", fn(u: User) { u.id })
////   |> types.string("email", fn(u: User) { u.email })
////   |> types.int("age", fn(u: User) { u.age })
////   |> types.list_object("friends", "User", fn(u) { ... })
////   |> types.build(decode_user)
//// }
////
//// fn decode_user(dyn) {
////   let decoder = {
////     use id <- decode.field("id", decode.string)
////     use email <- md.optional_string("email")
////     use age <- md.optional_int("age")
////     use friends <- md.list_filtering("friends", decode_user)
////     decode.success(User(id:, email:, age:, friends:))
////   }
////   md.build_with(decoder, "User", dyn)
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/string

/// Run a `decode.Decoder(t)` against `dyn`, returning the
/// `Result(t, String)` shape that `mochi/types.{build}` expects.
/// `type_name` is interpolated into the error message — keep it the
/// same as the GraphQL object type (e.g. `"CapabilityGraph"`) so
/// error logs are searchable.
///
/// On failure, the first decoder error is included in the message
/// so debugging doesn't degrade to "something failed somewhere in
/// this 14-field record":
///
/// ```
/// "Failed to decode User: expected String at name, got Int"
/// ```
///
/// Collapses the standard four-line wrap:
///
/// ```gleam
/// case decode.run(dyn, decoder) {
///   Ok(v) -> Ok(v)
///   Error(_) -> Error("Failed to decode CapabilityGraph")
/// }
/// ```
///
/// into a single call:
///
/// ```gleam
/// build_with(decoder, "CapabilityGraph", dyn)
/// ```
pub fn build_with(
  decoder: decode.Decoder(t),
  type_name: String,
  dyn: Dynamic,
) -> Result(t, String) {
  case decode.run(dyn, decoder) {
    Ok(v) -> Ok(v)
    Error(errors) ->
      Error("Failed to decode " <> type_name <> describe_errors(errors))
  }
}

fn describe_errors(errors: List(decode.DecodeError)) -> String {
  case errors {
    [] -> ""
    [decode.DecodeError(expected:, found:, path:), ..] -> {
      let where = case path {
        [] -> ""
        _ -> " at " <> string.join(path, ".")
      }
      ": expected " <> expected <> where <> ", got " <> found
    }
  }
}

/// Decode an optional string field, defaulting to the empty string
/// when absent. Continuation-passing form for `use`-binding:
///
/// ```gleam
/// use email <- md.optional_string("email")
/// ```
///
/// Equivalent to `decode.optional_field(name, "", decode.string, next)`
/// but reads better at call sites that have many such fields.
///
/// Output decoder only — see the module-level note. Don't use this
/// for input validation where "absent" must differ from "empty".
pub fn optional_string(
  name: String,
  next: fn(String) -> decode.Decoder(final),
) -> decode.Decoder(final) {
  decode.optional_field(name, "", decode.string, next)
}

/// Decode an optional integer field, defaulting to `0` when absent.
/// Output decoder only — see the module-level note.
pub fn optional_int(
  name: String,
  next: fn(Int) -> decode.Decoder(final),
) -> decode.Decoder(final) {
  decode.optional_field(name, 0, decode.int, next)
}

/// Decode an optional bool field, defaulting to `False` when absent.
/// Output decoder only — see the module-level note.
pub fn optional_bool(
  name: String,
  next: fn(Bool) -> decode.Decoder(final),
) -> decode.Decoder(final) {
  decode.optional_field(name, False, decode.bool, next)
}

/// Decode an optional list field where each item is a `Dynamic` that
/// must be passed through a per-item decoder. Items that fail their
/// per-item decode are **silently dropped**; missing or non-list
/// values resolve to the empty list.
///
/// Useful for top-level "list of object" GraphQL fields where one
/// malformed row shouldn't kill the whole response. Behavior:
///
/// - Field absent or null → `[]`
/// - Field present, item ok → included in result
/// - Field present, item decode fails → dropped silently
///
/// The `item` callback is the same shape as a `mochi/types.{build}`
/// callback (`fn(Dynamic) -> Result(t, String)`), so existing
/// per-type decoders can be passed directly:
///
/// ```gleam
/// use nodes <- list_filtering("nodes", decode_node)
/// use edges <- list_filtering("edges", decode_edge)
/// ```
///
/// Replaces the four-line `decode.optional_field` + `list.filter_map`
/// pattern with a single `use`-binding.
pub fn list_filtering(
  name: String,
  item: fn(Dynamic) -> Result(t, String),
  next: fn(List(t)) -> decode.Decoder(final),
) -> decode.Decoder(final) {
  use dyns <- decode.optional_field(name, [], decode.list(decode.dynamic))
  let items =
    list.filter_map(dyns, fn(d) {
      case item(d) {
        Ok(v) -> Ok(v)
        Error(_) -> Error(Nil)
      }
    })
  next(items)
}
