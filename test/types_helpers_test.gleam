import gleam/dict
import mochi/types

pub type Item {
  Item(
    tags: List(String),
    scores: List(Float),
    ids: List(String),
    flags: List(Bool),
  )
}

pub fn list_float_field_test() {
  let item_type =
    types.object("Item")
    |> types.list_float("scores", fn(i: Item) { i.scores })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "scores") {
    True -> Nil
    False -> panic as "Should have scores field"
  }
}

pub fn list_bool_field_test() {
  let item_type =
    types.object("Item")
    |> types.list_bool("flags", fn(i: Item) { i.flags })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "flags") {
    True -> Nil
    False -> panic as "Should have flags field"
  }
}

pub fn list_id_field_test() {
  let item_type =
    types.object("Item")
    |> types.list_id("ids", fn(i: Item) { i.ids })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "ids") {
    True -> Nil
    False -> panic as "Should have ids field"
  }
}

pub fn non_null_list_string_field_test() {
  let item_type =
    types.object("Item")
    |> types.non_null_list_string("tags", fn(i: Item) { i.tags })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "tags") {
    True -> Nil
    False -> panic as "Should have tags field"
  }
}

pub fn non_null_list_int_field_test() {
  let item_type =
    types.object("Item")
    |> types.non_null_list_int("counts", fn(_: Item) { [1, 2, 3] })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "counts") {
    True -> Nil
    False -> panic as "Should have counts field"
  }
}

pub fn non_null_list_float_field_test() {
  let item_type =
    types.object("Item")
    |> types.non_null_list_float("scores", fn(i: Item) { i.scores })
    |> types.build(fn(_) { Ok(Item([], [], [], [])) })

  case dict.has_key(item_type.fields, "scores") {
    True -> Nil
    False -> panic as "Should have scores field"
  }
}
