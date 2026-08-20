//// Reading what the client selected, so a resolver can fetch only what was
//// asked for instead of `select *`.
////
//// Everything here is a plain function over `List(SelectedField)`. Get the list
//// once with `schema.selection(ctx)`, then pipe:
////
//// ```gleam
//// resolve: fn(args, ctx) {
////   let selected = schema.selection(ctx)
////
////   selected |> selection.columns(always: ["id"])     // ["first_name", "id"]
////   selected |> selection.has("posts")                // worth a join?
////   selected |> selection.children("posts")           // the nested selection
////   selected |> selection.for_type("Cat")             // narrow a union
////   ...
//// }
//// ```
////
//// Binding the list once matters: `schema.selection` recomputes on each call.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema.{type SelectedField}

// ── Fields ────────────────────────────────────────────────────────────────────

/// The selected field names, deduplicated, in the order they were selected.
pub fn names(fields: List(SelectedField)) -> List(String) {
  list.map(fields, fn(field) { field.name }) |> list.unique
}

/// Whether `name` was selected.
pub fn has(fields: List(SelectedField), name: String) -> Bool {
  list.any(fields, fn(field) { field.name == name })
}

/// Every entry for `name` — one per distinct type condition and argument set.
///
/// ```gleam
/// // { posts(first: 1) { id } recent: posts(first: 5) { title } }
/// selected |> selection.find("posts")   // two entries
/// ```
pub fn find(fields: List(SelectedField), name: String) -> List(SelectedField) {
  list.filter(fields, fn(field) { field.name == name })
}

/// The sub-selection under `name`, pooled across every entry for it. Empty when
/// `name` was not selected, or is a leaf.
pub fn children(
  fields: List(SelectedField),
  name: String,
) -> List(SelectedField) {
  find(fields, name) |> list.flat_map(fn(field) { field.children })
}

// ── Narrowing a union or interface ────────────────────────────────────────────

/// The selection narrowed to what applies when the resolver returns `type_name`:
/// unconditional fields plus those from fragments whose type condition covers it.
///
/// An empty result means nobody asked about that type, so its table need not be
/// queried at all.
pub fn for_type(
  fields: List(SelectedField),
  type_name: String,
) -> List(SelectedField) {
  list.filter(fields, applies_to(_, type_name))
}

/// Whether one selected field applies to a resolver returning `type_name`.
pub fn applies_to(field: SelectedField, type_name: String) -> Bool {
  case field.only_for {
    None -> True
    Some(candidates) -> list.contains(candidates, type_name)
  }
}

// ── Columns ───────────────────────────────────────────────────────────────────

/// The storage columns the selection needs, as declared in the schema with
/// `types.from_columns` / `types.no_columns`.
///
/// `always` is appended unconditionally: put the primary key and any foreign key
/// a nested resolver needs there, otherwise a query selecting only computed
/// fields comes back with no key to join on.
pub fn columns(
  fields: List(SelectedField),
  always always: List(String),
) -> List(String) {
  list.flat_map(fields, fn(field) { field.columns })
  |> list.append(always)
  |> list.unique
}

// ── Arguments ─────────────────────────────────────────────────────────────────

/// An argument written on a selected field, with variables substituted.
///
/// Raw literal shape: not coerced against the schema, so schema defaults are
/// absent and enum values arrive as strings. Coerced arguments come from the
/// resolver's own `Args`.
pub fn argument(field: SelectedField, name: String) -> Option(Dynamic) {
  dict.get(field.arguments, name) |> option.from_result
}

/// An `Int` argument, e.g. a nested `first: 10` to honour when prefetching.
pub fn int_argument(field: SelectedField, name: String) -> Option(Int) {
  decoded_argument(field, name, decode.int)
}

/// A `String` argument.
pub fn string_argument(field: SelectedField, name: String) -> Option(String) {
  decoded_argument(field, name, decode.string)
}

/// A `Bool` argument.
pub fn bool_argument(field: SelectedField, name: String) -> Option(Bool) {
  decoded_argument(field, name, decode.bool)
}

fn decoded_argument(
  field: SelectedField,
  name: String,
  decoder: decode.Decoder(value),
) -> Option(value) {
  argument(field, name)
  |> option.then(fn(value) { decode.run(value, decoder) |> option.from_result })
}
