import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/response
import mochi/schema
import mochi/types
import mochi/validation

pub type User {
  User(id: String, name: String, bio: String)
}

fn decode_user(_dyn) {
  Ok(User("1", "Alice", "A developer"))
}

fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("bio", fn(u: User) { u.bio })
    |> types.build(decode_user)

  let user_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_info) { Ok(types.to_dynamic(User("1", "Alice", "A developer"))) },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn defer_inline_fragment_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(schema_def, "{ user { id ... @defer { bio } } }")

  should.equal(result.errors, [])
  should.be_true(option.is_some(result.data))

  let patches = result.deferred
  should.equal(list.length(patches), 1)

  let assert [patch] = patches
  should.be_true(option.is_some(patch.data))
  should.equal(patch.errors, [])
}

pub fn defer_if_false_no_patch_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id ... @defer(if: false) { bio } } }",
    )

  should.equal(result.errors, [])
  should.be_true(option.is_some(result.data))
  should.equal(result.deferred, [])
}

pub fn defer_if_true_produces_patch_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id ... @defer(if: true) { bio } } }",
    )

  should.equal(result.errors, [])
  should.equal(list.length(result.deferred), 1)
}

pub fn defer_with_label_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id ... @defer(label: \"bio-patch\") { bio } } }",
    )

  should.equal(result.errors, [])
  should.equal(list.length(result.deferred), 1)

  let assert [patch] = result.deferred
  should.equal(patch.label, Some("bio-patch"))
}

pub fn defer_non_deferred_in_initial_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id name ... @defer { bio } } }",
    )

  should.equal(result.errors, [])
  should.be_true(option.is_some(result.data))

  let assert Some(data) = result.data
  let name_result = decode.run(data, decode.at(["user", "name"], decode.string))
  should.be_ok(name_result)

  let id_result = decode.run(data, decode.at(["user", "id"], decode.string))
  should.be_ok(id_result)

  should.equal(list.length(result.deferred), 1)
}

pub fn defer_incremental_response_format_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id ... @defer(label: \"details\") { bio } } }",
    )

  let incremental = response.from_execution_result_incremental(result)

  should.equal(list.length(incremental.patches), 1)
  should.be_true(response.has_data(incremental.initial))

  let assert [patch] = incremental.patches
  should.equal(patch.label, Some("details"))
  should.equal(patch.has_next, False)
}

pub fn defer_multiple_patches_test() {
  let schema_def = build_test_schema()

  let result =
    executor.execute_query(
      schema_def,
      "{ user { id ... @defer(label: \"first\") { name } ... @defer(label: \"second\") { bio } } }",
    )

  should.equal(result.errors, [])
  should.equal(list.length(result.deferred), 2)

  let incremental = response.from_execution_result_incremental(result)
  should.equal(list.length(incremental.patches), 2)

  let assert [first_patch, last_patch] = incremental.patches
  should.equal(first_patch.has_next, True)
  should.equal(last_patch.has_next, False)
}

// ============================================================================
// hasNext placement tests
// ============================================================================

pub fn has_next_at_top_level_in_json_test() {
  let schema_def = build_test_schema()
  let result =
    executor.execute_query(schema_def, "{ user { id ... @defer { bio } } }")
  let incremental = response.from_execution_result_incremental(result)
  let json = response.to_json(incremental.initial)

  should.be_true(string.contains(json, "\"hasNext\":true"))
  should.be_false(string.contains(json, "\"extensions\""))
}

pub fn no_has_next_when_no_deferred_test() {
  let schema_def = build_test_schema()
  let result = executor.execute_query(schema_def, "{ user { id name bio } }")
  let json = response.to_json(response.from_execution_result(result))

  should.be_false(string.contains(json, "hasNext"))
}

// ============================================================================
// Validation: @defer arg type checking
// ============================================================================

pub fn defer_if_non_boolean_fails_validation_test() {
  let schema_def = build_test_schema()
  let q = "{ user { id ... @defer(if: 42) { bio } } }"
  let result = validation.validate_query(q, schema_def)
  let errors = result |> result.unwrap_error([])
  should.be_true(
    list.any(errors, fn(e) {
      string.contains(validation.format_error(e), "invalid type")
    }),
  )
}

pub fn defer_label_non_string_fails_validation_test() {
  let schema_def = build_test_schema()
  let q = "{ user { id ... @defer(label: 99) { bio } } }"
  let result = validation.validate_query(q, schema_def)
  let errors = result |> result.unwrap_error([])
  should.be_true(
    list.any(errors, fn(e) {
      string.contains(validation.format_error(e), "invalid type")
    }),
  )
}

pub fn defer_valid_boolean_string_args_passes_validation_test() {
  let schema_def = build_test_schema()
  let q = "{ user { id ... @defer(if: true, label: \"x\") { bio } } }"
  let result = executor.execute_query(schema_def, q)
  should.equal(result.errors, [])
}

// ============================================================================
// @defer on fragment spread
// ============================================================================

pub fn defer_on_fragment_spread_test() {
  let schema_def = build_test_schema()
  let q =
    "fragment Bio on User { bio }
     { user { id ...Bio @defer } }"
  let result = executor.execute_query(schema_def, q)

  should.equal(result.errors, [])
  should.equal(list.length(result.deferred), 1)
}

// ============================================================================
// @defer in list — deferred patches propagate from list items
// ============================================================================

pub fn defer_in_list_propagates_patches_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("bio", fn(u: User) { u.bio })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.List(schema.Named("User")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            User("1", "Alice", "Dev"),
            User("2", "Bob", "Ops"),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  let schema_def =
    query.new()
    |> query.add_query(users_query)
    |> query.add_type(user_type)
    |> query.build

  let result =
    executor.execute_query(
      schema_def,
      "{ users { id ... @defer { bio } } }",
    )

  should.equal(result.errors, [])
  should.be_true(result.deferred != [])
}

// ============================================================================
// Variable label
// ============================================================================

pub fn defer_label_from_variable_test() {
  let schema_def = build_test_schema()
  let q = "query Q($lbl: String) { user { id ... @defer(label: $lbl) { bio } } }"
  let vars = dict.from_list([#("lbl", types.to_dynamic("my-label"))])
  let result = executor.execute_query_with_variables(schema_def, q, vars)

  should.equal(result.errors, [])
  let assert [patch] = result.deferred
  should.equal(patch.label, Some("my-label"))
}

pub fn defer_label_missing_variable_gives_no_label_test() {
  let schema_def = build_test_schema()
  let q = "query Q($lbl: String) { user { id ... @defer(label: $lbl) { bio } } }"
  let result = executor.execute_query_with_variables(schema_def, q, dict.new())

  should.equal(result.errors, [])
  let assert [patch] = result.deferred
  should.equal(patch.label, None)
}
