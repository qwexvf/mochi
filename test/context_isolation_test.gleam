import gleam/list
import gleam/string
import mochi/query
import mochi/schema
import mochi/types
import mochi/validation

// Tests that the validation context's current_type is properly scoped so that
// fragment spreads and inline fragments do not leak their type into sibling
// field selections on the parent type.
//
// Regression coverage for two context-leak bugs in validate_fragment_spread
// and validate_inline_fragment.

// ── Schema helpers ────────────────────────────────────────────────────────────

pub type Profile {
  Profile(bio: String, website: String)
}

pub type User {
  User(id: String, name: String, email: String, score: Int, profile: Profile)
}

pub type Address {
  Address(street: String, city: String, zip: String)
}

pub type Post {
  Post(id: String, title: String, body: String, author_id: String)
}

fn profile_type() {
  types.object("Profile")
  |> types.string("bio", fn(p: Profile) { p.bio })
  |> types.string("website", fn(p: Profile) { p.website })
  |> types.build(fn(_) { Ok(Profile("", "")) })
}

fn user_type() {
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.int("score", fn(u: User) { u.score })
  |> types.object_field("profile", "Profile", fn(u: User) {
    types.to_dynamic(u.profile)
  })
  |> types.build(fn(_) { Ok(User("", "", "", 0, Profile("", ""))) })
}

fn address_type() {
  types.object("Address")
  |> types.string("street", fn(a: Address) { a.street })
  |> types.string("city", fn(a: Address) { a.city })
  |> types.string("zip", fn(a: Address) { a.zip })
  |> types.build(fn(_) { Ok(Address("", "", "")) })
}

fn post_type() {
  types.object("Post")
  |> types.id("id", fn(p: Post) { p.id })
  |> types.string("title", fn(p: Post) { p.title })
  |> types.string("body", fn(p: Post) { p.body })
  |> types.id("authorId", fn(p: Post) { p.author_id })
  |> types.build(fn(_) { Ok(Post("", "", "", "")) })
}

fn build_schema() {
  let user_query =
    query.query(
      "user",
      schema.named_type("User"),
      fn(_ctx) {
        Ok(User(
          "1",
          "Alice",
          "alice@example.com",
          42,
          Profile("dev", "example.com"),
        ))
      },
      types.to_dynamic,
    )

  let post_query =
    query.query(
      "post",
      schema.named_type("Post"),
      fn(_ctx) { Ok(Post("1", "Hello", "World", "1")) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_query(post_query)
  |> query.add_type(user_type())
  |> query.add_type(address_type())
  |> query.add_type(profile_type())
  |> query.add_type(post_type())
  |> query.build
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn assert_valid(q: String) {
  let s = build_schema()
  case validation.validate_query(q, s) {
    Ok(_) -> Nil
    Error(errors) ->
      panic as {
        "Expected valid query but got errors: "
        <> string.join(list.map(errors, validation.format_error), ", ")
      }
  }
}

fn assert_errors_do_not_contain(q: String, forbidden: String) {
  let s = build_schema()
  case validation.validate_query(q, s) {
    Ok(_) -> Nil
    Error(errors) -> {
      let messages = list.map(errors, validation.format_error)
      case list.any(messages, fn(m) { string.contains(m, forbidden) }) {
        True ->
          panic as {
            "False positive: error message contained forbidden string '"
            <> forbidden
            <> "'. Errors: "
            <> string.join(messages, ", ")
          }
        False -> Nil
      }
    }
  }
}

fn assert_has_error_for(q: String, expected: String) {
  let s = build_schema()
  case validation.validate_query(q, s) {
    Ok(_) ->
      panic as {
        "Expected validation error containing '" <> expected <> "' but got none"
      }
    Error(errors) -> {
      let messages = list.map(errors, validation.format_error)
      case list.any(messages, fn(m) { string.contains(m, expected) }) {
        True -> Nil
        False ->
          panic as {
            "Expected error containing '"
            <> expected
            <> "' but got: "
            <> string.join(messages, ", ")
          }
      }
    }
  }
}

// ── Regression: valid fragment patterns must produce no errors ────────────────

pub fn named_fragment_on_same_type_followed_by_sibling_test() {
  assert_valid(
    "{ user { id ...UserBasics score email } }
     fragment UserBasics on User { name }",
  )
}

pub fn named_fragment_with_nested_object_followed_by_sibling_test() {
  assert_valid(
    "{ user { id ...UserWithProfile score } }
     fragment UserWithProfile on User { name profile { bio website } }",
  )
}

pub fn multiple_named_fragments_on_same_type_test() {
  assert_valid(
    "{ user { ...UserName ...UserContact score } }
     fragment UserName on User { id name }
     fragment UserContact on User { email }",
  )
}

pub fn named_fragment_interspersed_with_siblings_test() {
  assert_valid(
    "{ user { id ...UserName email ...UserScore name } }
     fragment UserName on User { name }
     fragment UserScore on User { score }",
  )
}

pub fn fragment_then_nested_object_then_sibling_test() {
  assert_valid(
    "{ user { ...UserName profile { bio } score } }
     fragment UserName on User { id name }",
  )
}

pub fn nested_fragment_spread_inside_another_fragment_test() {
  assert_valid(
    "{ user { ...UserFull score } }
     fragment UserFull on User { id ...UserName email }
     fragment UserName on User { name }",
  )
}

pub fn fragment_used_in_multiple_queries_test() {
  assert_valid(
    "{ user { ...CoreFields score } post { id title body authorId } }
     fragment CoreFields on User { id name email }",
  )
}

// ── Regression: valid inline fragment patterns must produce no errors ─────────

pub fn inline_fragment_on_same_type_followed_by_sibling_test() {
  assert_valid("{ user { id ... on User { name email } score } }")
}

pub fn multiple_inline_fragments_on_same_type_test() {
  assert_valid(
    "{ user { ... on User { id name } ... on User { email } score } }",
  )
}

pub fn inline_fragment_without_type_condition_followed_by_sibling_test() {
  assert_valid("{ user { id ... { name email } score active: score } }")
}

pub fn inline_fragment_with_nested_object_followed_by_sibling_test() {
  assert_valid(
    "{ user { id ... on User { profile { bio } name } score email } }",
  )
}

pub fn inline_fragment_then_nested_object_then_sibling_test() {
  assert_valid("{ user { ... on User { id } profile { bio website } score } }")
}

pub fn alternating_inline_fragments_and_plain_fields_test() {
  assert_valid(
    "{ user { id ... on User { name } score ... on User { email } } }",
  )
}

// ── Bug regression: inline fragment type context must not leak into siblings ──
//
// These tests expose validate_inline_fragment not restoring current_type.
// A typed inline fragment (... on T { ... }) inside a parent selection must
// not contaminate validation of subsequent sibling fields with type T.
//
// Scenario: ... on Address { city } appears inside user { }, which is wrong
// but mochi has no type-compatibility check so it is accepted silently.
// What must NOT happen is sibling fields valid on User being rejected because
// they were checked against Address instead.

pub fn inline_fragment_different_type_sibling_score_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { id ... on Address { city } score } }",
    "score",
  )
}

pub fn inline_fragment_different_type_sibling_email_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { ... on Address { street } email name } }",
    "email",
  )
}

pub fn inline_fragment_different_type_multiple_siblings_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { ... on Post { title body } id name score email } }",
    "score",
  )
}

pub fn two_inline_fragments_different_types_siblings_after_both_test() {
  assert_errors_do_not_contain(
    "{ user { ... on Address { city } ... on Post { title } id name } }",
    "name",
  )
}

// ── Bug regression: fragment spread type context must not leak into siblings ──
//
// These tests expose validate_fragment_spread not restoring current_type.
// Same scenario: a fragment spread whose type_condition differs from the
// parent type must not contaminate subsequent sibling field validation.

pub fn fragment_spread_different_type_sibling_score_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { id ...AddressFields score } }
     fragment AddressFields on Address { city zip }",
    "score",
  )
}

pub fn fragment_spread_different_type_sibling_email_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { ...PostFields email name score } }
     fragment PostFields on Post { title body }",
    "email",
  )
}

pub fn fragment_spread_different_type_multiple_siblings_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { ...AddressFields id name email score } }
     fragment AddressFields on Address { street city zip }",
    "name",
  )
}

pub fn mixed_fragment_and_inline_different_types_siblings_not_rejected_test() {
  assert_errors_do_not_contain(
    "{ user { ...AddressFields ... on Post { title } id score } }
     fragment AddressFields on Address { city }",
    "score",
  )
}

// ── Invalid fields are still caught after fragments ───────────────────────────
//
// Context isolation must not accidentally suppress real errors.

pub fn invalid_field_after_valid_fragment_still_rejected_test() {
  assert_has_error_for(
    "{ user { ...UserBasics nonExistent } }
     fragment UserBasics on User { id name }",
    "nonExistent",
  )
}

pub fn invalid_field_after_inline_fragment_still_rejected_test() {
  assert_has_error_for(
    "{ user { ... on User { name } bogusField } }",
    "bogusField",
  )
}

pub fn invalid_field_inside_fragment_still_rejected_test() {
  assert_has_error_for(
    "{ user { ...BadFragment score } }
     fragment BadFragment on User { id nonExistentInUser }",
    "nonExistentInUser",
  )
}

pub fn invalid_field_inside_inline_fragment_still_rejected_test() {
  assert_has_error_for(
    "{ user { id ... on User { name bogusInUser } score } }",
    "bogusInUser",
  )
}
