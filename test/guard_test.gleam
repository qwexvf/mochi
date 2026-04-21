// Tests for the Guard API (schema.guard and query.with_guard)

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None}
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types & Helpers
// ============================================================================

pub type User {
  User(id: String, name: String)
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Alice"))
}

fn user_to_dynamic(u: User) -> Dynamic {
  types.record([types.field("id", u.id), types.field("name", u.name)])
}

fn default_ctx() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
}

// ============================================================================
// Reusable Guards
// ============================================================================

/// A guard that always allows
fn allow_guard(_ctx: schema.ExecutionContext) -> Result(Nil, String) {
  Ok(Nil)
}

/// A guard that always denies
fn deny_guard(_ctx: schema.ExecutionContext) -> Result(Nil, String) {
  Error("Access denied")
}

/// A low-level guard (takes ResolverInfo) that always allows
fn low_level_allow(_info: schema.ResolverInfo) -> Result(Nil, String) {
  Ok(Nil)
}

/// A low-level guard (takes ResolverInfo) that always denies
fn low_level_deny(_info: schema.ResolverInfo) -> Result(Nil, String) {
  Error("Forbidden")
}

// ============================================================================
// schema.guard Tests (Low-level API)
// ============================================================================

pub fn schema_guard_allows_resolver_test() {
  let test_schema = build_schema_with_guard(low_level_allow)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn schema_guard_blocks_resolver_test() {
  let test_schema = build_schema_with_guard(low_level_deny)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

pub fn schema_guards_multiple_all_pass_test() {
  let test_schema = build_schema_with_guards([low_level_allow, low_level_allow])

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn schema_guards_first_fails_test() {
  let test_schema = build_schema_with_guards([low_level_deny, low_level_allow])

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

pub fn schema_guards_second_fails_test() {
  let test_schema = build_schema_with_guards([low_level_allow, low_level_deny])

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

pub fn schema_guards_order_first_checked_first_test() {
  // guards([deny_with_msg_a, deny_with_msg_b]) should check A first
  let guard_a = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("Guard A failed")
  }
  let guard_b = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("Guard B failed")
  }

  let test_schema = build_schema_with_guards([guard_a, guard_b])

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name } }",
      dict.new(),
      default_ctx(),
    )

  // The first guard in the list (guard_a) should be checked first
  let has_guard_a_error =
    result.errors
    |> list.any(fn(err) {
      case err {
        executor.ResolverError(message: m, ..)
        | executor.ValidationError(message: m, ..)
        | executor.TypeError(message: m, ..)
        | executor.NullValueError(message: m, ..) -> m == "Guard A failed"
        executor.RichResolverError(graphql_error: e, ..) ->
          e.message == "Guard A failed"
      }
    })
  should.be_true(has_guard_a_error)
}

pub fn schema_guard_on_field_without_resolver_is_noop_test() {
  // guard on a field with no resolver should not crash
  let field_def = schema.field_def("test", schema.string_type())
  let guarded = schema.guard(field_def, low_level_deny)
  // Should still have no resolver
  should.equal(guarded.resolver, None)
}

// ============================================================================
// query.with_guard Tests (High-level API)
// ============================================================================

pub fn query_guard_allows_test() {
  let test_schema = build_query_with_guard(allow_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ users { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn query_guard_blocks_test() {
  let test_schema = build_query_with_guard(deny_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ users { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

pub fn query_guard_stacking_test() {
  // Both guards must pass — second one denies
  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([User("1", "Alice")]) },
      fn(users) { types.to_dynamic(users) },
    )
    |> query.with_guard(allow_guard)
    |> query.with_guard(deny_guard)

  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let test_schema =
    query.new()
    |> query.add_query(users_query)
    |> query.add_type(user_type)
    |> query.build

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ users { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

// ============================================================================
// query.mutation_with_guard Tests
// ============================================================================

pub fn mutation_guard_allows_test() {
  let test_schema = build_mutation_with_guard(allow_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "mutation { createUser(name: \"Bob\") { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn mutation_guard_blocks_test() {
  let test_schema = build_mutation_with_guard(deny_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "mutation { createUser(name: \"Bob\") { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.not_equal(result.errors, [])
}

// ============================================================================
// query.field_with_guard Tests
// ============================================================================

pub fn field_guard_allows_test() {
  let test_schema = build_field_with_guard(allow_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name secret } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn field_guard_blocks_test() {
  let test_schema = build_field_with_guard(deny_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ user { id name secret } }",
      dict.new(),
      default_ctx(),
    )

  // The field-level guard error should surface
  should.not_equal(result.errors, [])
}

// ============================================================================
// Guard Error Message Test
// ============================================================================

pub fn guard_error_message_preserved_test() {
  let custom_guard = fn(_ctx: schema.ExecutionContext) -> Result(Nil, String) {
    Error("You must be an admin to access this resource")
  }

  let test_schema = build_query_with_guard(custom_guard)

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ users { id } }",
      dict.new(),
      default_ctx(),
    )

  // Check that the specific error message is preserved
  let has_admin_error =
    result.errors
    |> list.any(fn(err) {
      case err {
        executor.ResolverError(message: m, ..)
        | executor.ValidationError(message: m, ..)
        | executor.TypeError(message: m, ..)
        | executor.NullValueError(message: m, ..) ->
          m == "You must be an admin to access this resource"
        executor.RichResolverError(graphql_error: e, ..) ->
          e.message == "You must be an admin to access this resource"
      }
    })
  should.be_true(has_admin_error)
}

// ============================================================================
// Schema Builders for Tests
// ============================================================================

fn build_schema_with_guard(guard_fn: schema.Guard) -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.id_field("id")
    |> schema.required_string_field("name")

  let user_field =
    schema.field_def("user", schema.named_type("User"))
    |> schema.field_description("Get a user")
    |> schema.resolver(fn(_info: schema.ResolverInfo) {
      Ok(user_to_dynamic(User("1", "Alice")))
    })
    |> schema.guard(guard_fn)

  let query_type =
    schema.object("Query")
    |> schema.field(user_field)

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
}

fn build_schema_with_guards(guard_fns: List(schema.Guard)) -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.id_field("id")
    |> schema.required_string_field("name")

  let user_field =
    schema.field_def("user", schema.named_type("User"))
    |> schema.resolver(fn(_info: schema.ResolverInfo) {
      Ok(user_to_dynamic(User("1", "Alice")))
    })
    |> schema.guards(guard_fns)

  let query_type =
    schema.object("Query")
    |> schema.field(user_field)

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
}

fn build_query_with_guard(
  guard_fn: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([User("1", "Alice")]) },
      fn(users) { types.to_dynamic(users) },
    )
    |> query.with_guard(guard_fn)

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

fn build_mutation_with_guard(
  guard_fn: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let create_user_mutation =
    query.mutation(
      name: "createUser",
      args: [query.arg("name", schema.non_null(schema.string_type()))],
      returns: schema.named_type("User"),
      decode: fn(args) { query.get_string(args, "name") },
      resolve: fn(_name, _ctx) { Ok(User("2", "Bob")) },
      encode: fn(user) { types.to_dynamic(user) },
    )
    |> query.mutation_with_guard(guard_fn)

  query.new()
  |> query.add_mutation(create_user_mutation)
  |> query.add_type(user_type)
  |> query.build
}

fn build_field_with_guard(
  guard_fn: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let secret_field =
    query.field(
      "secret",
      schema.string_type(),
      fn(_dyn) { Ok(Nil) },
      fn(_parent, _ctx) { Ok("top-secret-value") },
      fn(s) { types.to_dynamic(s) },
    )
    |> query.field_with_guard(guard_fn)

  // Build the User type with the guarded secret field added via schema API
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)
    |> schema.field(query.field_def_to_schema(secret_field))

  let user_query =
    query.query(
      "user",
      schema.named_type("User"),
      fn(_ctx) { Ok(User("1", "Alice")) },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

// ============================================================================
// subscription_with_guard Tests
// ============================================================================

pub fn subscription_guard_allows_test() {
  let sub =
    query.subscription(
      "onMessage",
      schema.named_type("User"),
      "messages",
      fn(u) { types.to_dynamic(u) },
    )
    |> query.subscription_with_guard(allow_guard)

  let field_def = query.subscription_to_field_def(sub)
  should.equal(field_def.name, "onMessage")
}

pub fn subscription_guard_blocks_test() {
  let sub =
    query.subscription(
      "onMessage",
      schema.named_type("User"),
      "messages",
      fn(u) { types.to_dynamic(u) },
    )
    |> query.subscription_with_guard(deny_guard)

  let field_def = query.subscription_to_field_def(sub)
  // The topic_fn should fail when guard blocks
  case field_def.topic_fn {
    option.Some(topic_fn) -> {
      let result = topic_fn(dict.new(), default_ctx())
      should.be_error(result)
    }
    option.None -> panic as "expected topic_fn to be Some"
  }
}

// ============================================================================
// Guard Combinator Tests
// ============================================================================

pub fn all_guards_all_pass_test() {
  let combined = schema.all_guards([low_level_allow, low_level_allow])
  let info = make_resolver_info()
  should.be_ok(combined(info))
}

pub fn all_guards_one_fails_test() {
  let combined = schema.all_guards([low_level_allow, low_level_deny])
  let info = make_resolver_info()
  should.be_error(combined(info))
}

pub fn all_guards_first_fails_short_circuits_test() {
  let guard_a = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("A failed")
  }
  let guard_b = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("B failed")
  }
  let combined = schema.all_guards([guard_a, guard_b])
  let info = make_resolver_info()
  let result = combined(info)
  // Should fail with A's error (checked first)
  should.equal(result, Error("A failed"))
}

pub fn any_guard_one_passes_test() {
  let combined = schema.any_guard([low_level_deny, low_level_allow])
  let info = make_resolver_info()
  should.be_ok(combined(info))
}

pub fn any_guard_all_fail_test() {
  let combined = schema.any_guard([low_level_deny, low_level_deny])
  let info = make_resolver_info()
  should.be_error(combined(info))
}

pub fn any_guard_first_passes_short_circuits_test() {
  let combined = schema.any_guard([low_level_allow, low_level_deny])
  let info = make_resolver_info()
  should.be_ok(combined(info))
}

pub fn any_guard_empty_list_test() {
  let combined = schema.any_guard([])
  let info = make_resolver_info()
  should.be_error(combined(info))
}

pub fn any_guard_returns_last_error_test() {
  let guard_a = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("A failed")
  }
  let guard_b = fn(_info: schema.ResolverInfo) -> Result(Nil, String) {
    Error("B failed")
  }
  let combined = schema.any_guard([guard_a, guard_b])
  let info = make_resolver_info()
  should.equal(combined(info), Error("B failed"))
}

// High-level combinator tests

pub fn high_level_all_of_test() {
  let combined = query.all_of([allow_guard, allow_guard])
  should.be_ok(combined(default_ctx()))
}

pub fn high_level_all_of_fails_test() {
  let combined = query.all_of([allow_guard, deny_guard])
  should.be_error(combined(default_ctx()))
}

pub fn high_level_any_of_test() {
  let combined = query.any_of([deny_guard, allow_guard])
  should.be_ok(combined(default_ctx()))
}

pub fn high_level_any_of_all_fail_test() {
  let combined = query.any_of([deny_guard, deny_guard])
  should.be_error(combined(default_ctx()))
}

pub fn any_of_used_as_guard_test() {
  let test_schema =
    build_query_with_guard(query.any_of([deny_guard, allow_guard]))

  let result =
    executor.execute_query_with_context(
      test_schema,
      "{ users { id name } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

fn make_resolver_info() -> schema.ResolverInfo {
  schema.ResolverInfo(
    parent: option.None,
    arguments: dict.new(),
    context: default_ctx(),
    info: types.to_dynamic(dict.new()),
  )
}
