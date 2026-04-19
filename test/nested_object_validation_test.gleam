// Tests for query validation with nested object types.
//
// Regression tests for a bug where the validation context's current_type
// leaked from a nested object selection set into sibling fields on the parent
// type, causing valid queries to be rejected with "Cannot query field X on
// type Y" errors.

import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ── Domain types ─────────────────────────────────────────────────────────────

pub type Address {
  Address(street: String, city: String, zip: String)
}

pub type ContactInfo {
  ContactInfo(email: String, phone: String)
}

pub type User {
  User(
    id: String,
    name: String,
    address: Address,
    contact: ContactInfo,
    score: Int,
    active: Bool,
  )
}

pub type EquityByStreet {
  EquityByStreet(
    preflop: Float,
    flop: Float,
    turn: Float,
    river: Float,
  )
}

pub type DetectedHand {
  DetectedHand(
    id: String,
    hole_cards: String,
    board: String,
    equity: EquityByStreet,
    gto_deviation: Float,
    accepted: Bool,
  )
}

pub type Comment {
  Comment(id: String, body: String, author: User)
}

// ── Schema helpers ────────────────────────────────────────────────────────────

fn address_type() {
  types.object("Address")
  |> types.string("street", fn(a: Address) { a.street })
  |> types.string("city", fn(a: Address) { a.city })
  |> types.string("zip", fn(a: Address) { a.zip })
  |> types.build(fn(_) { Ok(Address("", "", "")) })
}

fn contact_type() {
  types.object("ContactInfo")
  |> types.string("email", fn(c: ContactInfo) { c.email })
  |> types.string("phone", fn(c: ContactInfo) { c.phone })
  |> types.build(fn(_) { Ok(ContactInfo("", "")) })
}

fn user_type() {
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.object_field("address", "Address", fn(u: User) {
    types.to_dynamic(u.address)
  })
  |> types.object_field("contact", "ContactInfo", fn(u: User) {
    types.to_dynamic(u.contact)
  })
  |> types.int("score", fn(u: User) { u.score })
  |> types.bool("active", fn(u: User) { u.active })
  |> types.build(fn(_) {
    Ok(User(
      "1",
      "Alice",
      Address("123 Main St", "Springfield", "12345"),
      ContactInfo("alice@example.com", "555-0100"),
      42,
      True,
    ))
  })
}

fn equity_type() {
  types.object("EquityByStreet")
  |> types.float("preflop", fn(e: EquityByStreet) { e.preflop })
  |> types.float("flop", fn(e: EquityByStreet) { e.flop })
  |> types.float("turn", fn(e: EquityByStreet) { e.turn })
  |> types.float("river", fn(e: EquityByStreet) { e.river })
  |> types.build(fn(_) { Ok(EquityByStreet(0.0, 0.0, 0.0, 0.0)) })
}

fn hand_type() {
  types.object("DetectedHand")
  |> types.id("id", fn(h: DetectedHand) { h.id })
  |> types.string("holeCards", fn(h: DetectedHand) { h.hole_cards })
  |> types.string("board", fn(h: DetectedHand) { h.board })
  |> types.object_field("equity", "EquityByStreet", fn(h: DetectedHand) {
    types.to_dynamic(h.equity)
  })
  |> types.float("gtoDeviation", fn(h: DetectedHand) { h.gto_deviation })
  |> types.bool("accepted", fn(h: DetectedHand) { h.accepted })
  |> types.build(fn(_) {
    Ok(DetectedHand("1", "As Kh", "Qd Jc Ts", EquityByStreet(0.6, 0.55, 0.7, 0.9), 0.05, False))
  })
}

fn comment_type() {
  types.object("Comment")
  |> types.id("id", fn(c: Comment) { c.id })
  |> types.string("body", fn(c: Comment) { c.body })
  |> types.object_field("author", "User", fn(c: Comment) {
    types.to_dynamic(c.author)
  })
  |> types.build(fn(_) {
    Ok(Comment(
      "1",
      "Nice post",
      User(
        "2",
        "Bob",
        Address("", "", ""),
        ContactInfo("", ""),
        0,
        True,
      ),
    ))
  })
}

fn user_schema() {
  let the_user =
    User(
      "1",
      "Alice",
      Address("123 Main St", "Springfield", "12345"),
      ContactInfo("alice@example.com", "555-0100"),
      42,
      True,
    )

  let user_query =
    query.query(
      "user",
      schema.named_type("User"),
      fn(_ctx) { Ok(the_user) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type())
  |> query.add_type(address_type())
  |> query.add_type(contact_type())
  |> query.build
}

fn hand_schema() {
  let the_hand =
    DetectedHand(
      "hand-1",
      "As Kh",
      "Qd Jc Ts",
      EquityByStreet(0.62, 0.55, 0.71, 0.9),
      0.05,
      False,
    )

  let hand_query =
    query.query(
      "hand",
      schema.named_type("DetectedHand"),
      fn(_ctx) { Ok(the_hand) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(hand_query)
  |> query.add_type(hand_type())
  |> query.add_type(equity_type())
  |> query.build
}

fn comment_schema() {
  let the_comment =
    Comment(
      "c1",
      "Nice post",
      User(
        "2",
        "Bob",
        Address("456 Elm St", "Shelbyville", "67890"),
        ContactInfo("bob@example.com", "555-0200"),
        10,
        False,
      ),
    )

  let comment_query =
    query.query(
      "comment",
      schema.named_type("Comment"),
      fn(_ctx) { Ok(the_comment) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(comment_query)
  |> query.add_type(comment_type())
  |> query.add_type(user_type())
  |> query.add_type(address_type())
  |> query.add_type(contact_type())
  |> query.build
}

// ── Regression tests: sibling field after nested object ──────────────────────

// This was the original bug: `gtoDeviation` (a sibling of `equity`) was
// incorrectly validated against `EquityByStreet` instead of `DetectedHand`.
pub fn sibling_field_after_nested_object_is_valid_test() {
  let s = hand_schema()
  let q =
    "{ hand { id equity { preflop flop turn river } gtoDeviation accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

pub fn sibling_field_before_nested_object_is_valid_test() {
  let s = hand_schema()
  let q =
    "{ hand { id gtoDeviation equity { preflop flop } accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

pub fn multiple_sibling_fields_after_nested_object_are_valid_test() {
  let s = user_schema()
  let q =
    "{ user { id address { street city zip } score active name } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

pub fn two_nested_objects_then_sibling_is_valid_test() {
  let s = user_schema()
  let q =
    "{ user { id address { street city } contact { email phone } score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

pub fn nested_object_only_no_siblings_is_valid_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop flop turn river } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

// ── Validation: invalid fields are still caught ───────────────────────────────

pub fn invalid_field_on_parent_type_is_rejected_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop } nonExistentField } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error for nonExistentField"
    _ -> Nil
  }
}

pub fn invalid_field_on_nested_type_is_rejected_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop bogus } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error for bogus field on EquityByStreet"
    _ -> Nil
  }
}

pub fn field_from_nested_type_on_parent_is_rejected_test() {
  let s = hand_schema()
  // preflop belongs to EquityByStreet, not DetectedHand
  let q = "{ hand { preflop } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error: preflop is not on DetectedHand"
    _ -> Nil
  }
}

pub fn field_from_parent_type_inside_nested_object_is_rejected_test() {
  let s = hand_schema()
  // gtoDeviation belongs to DetectedHand, not EquityByStreet
  let q = "{ hand { equity { preflop gtoDeviation } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] ->
      panic as "Expected validation error: gtoDeviation is not on EquityByStreet"
    _ -> Nil
  }
}

// ── Deeply nested objects ─────────────────────────────────────────────────────

pub fn deeply_nested_object_with_sibling_is_valid_test() {
  let s = comment_schema()
  let q =
    "{ comment { id body author { id name address { street city } score } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

pub fn sibling_after_nested_at_depth_two_is_valid_test() {
  let s = comment_schema()
  // After author.address {...}, author.score should still validate against User
  let q =
    "{ comment { id author { name address { street } active score } body } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
}

// ── Execution: correct data returned ─────────────────────────────────────────

pub fn equity_and_gto_deviation_both_resolve_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop river } gtoDeviation accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

pub fn user_siblings_after_nested_address_resolve_test() {
  let s = user_schema()
  let q = "{ user { address { city } name score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { e.message })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn string_join(items: List(a), f: fn(a) -> String) -> String {
  case items {
    [] -> ""
    [x] -> f(x)
    [x, ..rest] -> f(x) <> ", " <> string_join(rest, f)
  }
}
