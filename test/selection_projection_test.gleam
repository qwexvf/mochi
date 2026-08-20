// Tests for selection projection — what the client asked for, surfaced to the
// resolver via ExecutionContext.selection so it can project columns.

import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import mochi/error
import mochi/executor
import mochi/query
import mochi/response
import mochi/schema
import mochi/selection
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

// `types.to_dynamic` is an identity coercion, so the Dynamic handed back to
// the decoder is the User the resolver returned — coerce it straight back so the
// assertions can read what the resolver actually built.
@external(erlang, "gleam_stdlib", "identity")
fn unsafe_coerce(value: dynamic.Dynamic) -> a

fn decode_user(dyn: dynamic.Dynamic) -> Result(User, String) {
  Ok(unsafe_coerce(dyn))
}

/// The resolver reports its own view of the selection through `name`, so the
/// assertions can read it straight off the response.
fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.build(decode_user)

  let user_query =
    query.query(name: "user", returns: schema.Named("User"), resolve: fn(ctx) {
      let seen =
        selection.names(schema.selection(ctx)) |> list.sort(string.compare)
      Ok(User("1", string.join(seen, ","), "alice@example.com"))
    })

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

fn json(query_str: String) -> String {
  build_test_schema()
  |> executor.execute_query(query_str)
  |> response.from_execution_result
  |> response.to_json
}

// ============================================================================
// Plain Selections
// ============================================================================

pub fn selection_reaches_resolver_test() {
  json("{ user { id name } }")
  |> string.contains("\"id,name\"")
  |> should.be_true
}

pub fn selection_omits_unselected_fields_test() {
  let out = json("{ user { name } }")
  out |> string.contains("\"name\"") |> should.be_true
  out |> string.contains("email") |> should.be_false
}

pub fn selection_uses_field_name_not_alias_test() {
  json("{ user { who: name theEmail: email } }")
  |> string.contains("\"email,name\"")
  |> should.be_true
}

pub fn selection_excludes_meta_fields_test() {
  json("{ user { __typename name } }")
  |> string.contains("\"name\"")
  |> should.be_true
}

pub fn selection_is_empty_for_leaf_fields_test() {
  // `name` itself has no sub-selection, so a resolver on it sees nothing
  let leaf_schema = {
    let user_type =
      types.object("User")
      |> types.id("id", fn(u: User) { u.id })
      |> types.string("name", fn(u: User) { u.name })
      |> types.build(decode_user)

    let user_query =
      query.query(name: "user", returns: schema.Named("User"), resolve: fn(ctx) {
        should.equal(selection.names(schema.selection(ctx)), ["name"])
        Ok(User("1", "Alice", "alice@example.com"))
      })

    query.new()
    |> query.add_query(user_query)
    |> query.add_type(user_type)
    |> query.build
  }

  let result = executor.execute_query(leaf_schema, "{ user { name } }")
  should.equal(result.errors, [])
}

// ============================================================================
// Fragments
// ============================================================================

pub fn selection_expands_fragment_spread_test() {
  json(
    "
    { user { name ...Fields } }
    fragment Fields on User { id email }
    ",
  )
  |> string.contains("\"email,id,name\"")
  |> should.be_true
}

pub fn selection_expands_inline_fragment_test() {
  json("{ user { name ... on User { id email } } }")
  |> string.contains("\"email,id,name\"")
  |> should.be_true
}

pub fn selection_dedupes_across_fragment_and_field_test() {
  // `id` appears twice in the query, once in the selection handed to the resolver
  json(
    "
    { user { id name ...Fields } }
    fragment Fields on User { id email }
    ",
  )
  |> string.contains("\"email,id,name\"")
  |> should.be_true
}

// ============================================================================
// @skip / @include
// ============================================================================

pub fn selection_drops_skipped_field_test() {
  let out = json("{ user { name email @skip(if: true) } }")
  out |> string.contains("\"name\"") |> should.be_true
  out |> string.contains("email") |> should.be_false
}

pub fn selection_keeps_included_field_test() {
  json("{ user { name email @include(if: true) } }")
  |> string.contains("\"email,name\"")
  |> should.be_true
}

// ============================================================================
// selection.columns
// ============================================================================

fn selected(
  fields: List(#(String, List(String))),
) -> List(schema.SelectedField) {
  list.map(fields, fn(field) {
    schema.SelectedField(
      name: field.0,
      only_for: option.None,
      arguments: dict.new(),
      columns: field.1,
      children: [],
    )
  })
}

pub fn columns_append_always_test() {
  selected([#("name", ["name"]), #("email", ["email"])])
  |> selection.columns(always: ["id"])
  |> should.equal(["name", "email", "id"])
}

pub fn columns_expand_one_field_to_many_test() {
  selected([#("fullName", ["first_name", "last_name"])])
  |> selection.columns(always: ["id"])
  |> should.equal(["first_name", "last_name", "id"])
}

pub fn columns_dedupe_shared_and_always_test() {
  selected([#("name", ["name"]), #("displayName", ["name"])])
  |> selection.columns(always: ["name", "id"])
  |> should.equal(["name", "id"])
}

pub fn columns_of_empty_selection_are_always_test() {
  selected([])
  |> selection.columns(always: ["id"])
  |> should.equal(["id"])
}

pub fn columns_skip_fields_claiming_none_test() {
  selected([#("name", ["name"]), #("posts", [])])
  |> selection.columns(always: ["id"])
  |> should.equal(["name", "id"])
}

pub fn unknown_field_never_reaches_the_resolver_test() {
  // validation rejects a field the type does not have, so nothing a client
  // invents can become a column
  let result =
    build_test_schema()
    |> executor.execute_query("{ user { name, id); drop table users; -- } }")

  should.not_equal(result.errors, [])
  should.equal(result.data, option.None)
}

// ============================================================================
// Nested Selection Accessors
// ============================================================================

pub fn selection_of_returns_children_test() {
  let ctx =
    schema.execution_context(Nil)
    |> schema.with_selection([
      schema.selected_field("id", []),
      schema.selected_field("posts", [schema.selected_field("title", [])]),
    ])

  selection.children(schema.selection(ctx), "posts")
  |> list.map(fn(f) { f.name })
  |> should.equal(["title"])

  selection.children(schema.selection(ctx), "id") |> should.equal([])
  selection.children(schema.selection(ctx), "missing") |> should.equal([])
  selection.has(schema.selection(ctx), "posts") |> should.be_true
  selection.has(schema.selection(ctx), "missing") |> should.be_false
}

pub fn nested_selection_reaches_resolver_test() {
  // `posts` sub-selection must be visible on the user resolver's context,
  // so it can decide whether to join
  let post_type =
    types.object("Post")
    |> types.id("id", fn(p: #(String, String)) { p.0 })
    |> types.string("title", fn(p: #(String, String)) { p.1 })
    |> types.build(fn(dyn) { Ok(unsafe_coerce(dyn)) })

  let posts_field =
    query.field(
      name: "posts",
      returns: schema.List(schema.Named("Post")),
      decode: fn(dyn) { Ok(unsafe_coerce(dyn)) },
      resolve: fn(_parent: User, _ctx) { Ok([#("p1", "Hello")]) },
    )

  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)
    |> schema.field(query.to_field_def(posts_field))

  let user_query =
    query.query(name: "user", returns: schema.Named("User"), resolve: fn(ctx) {
      let nested =
        selection.children(schema.selection(ctx), "posts")
        |> list.map(fn(f) { f.name })
        |> list.sort(string.compare)
        |> string.join(",")
      Ok(User("1", nested, "alice@example.com"))
    })

  let nested_schema =
    query.new()
    |> query.add_query(user_query)
    |> query.add_types([user_type, post_type])
    |> query.build

  nested_schema
  |> executor.execute_query("{ user { name posts { id title } } }")
  |> response.from_execution_result
  |> response.to_json
  |> string.contains("\"id,title\"")
  |> should.be_true
}

// ============================================================================
// Variables, Guards, Low-level Resolvers
// ============================================================================

pub fn selection_applies_skip_from_variable_test() {
  let out =
    build_test_schema()
    |> executor.execute_query_with_variables(
      "query Q($hide: Boolean!) { user { name email @skip(if: $hide) } }",
      dict.from_list([#("hide", types.to_dynamic(True))]),
    )
    |> response.from_execution_result
    |> response.to_json

  out |> string.contains("\"name\"") |> should.be_true
  out |> string.contains("email") |> should.be_false
}

pub fn selection_keeps_field_when_skip_variable_false_test() {
  build_test_schema()
  |> executor.execute_query_with_variables(
    "query Q($hide: Boolean!) { user { name email @skip(if: $hide) } }",
    dict.from_list([#("hide", types.to_dynamic(False))]),
  )
  |> response.from_execution_result
  |> response.to_json
  |> string.contains("\"email,name\"")
  |> should.be_true
}

pub fn guard_sees_selection_test() {
  // a guard runs before the resolver and reads the same field-scoped context,
  // so it can reject a query that asks for a field the caller may not have
  let guarded_schema = {
    let user_type =
      types.object("User")
      |> types.id("id", fn(u: User) { u.id })
      |> types.string("name", fn(u: User) { u.name })
      |> types.string("email", fn(u: User) { u.email })
      |> types.build(decode_user)

    let user_query =
      query.query(
        name: "user",
        returns: schema.Named("User"),
        resolve: fn(_ctx) { Ok(User("1", "Alice", "alice@example.com")) },
      )
      |> query.with_guard(fn(ctx) {
        case selection.has(schema.selection(ctx), "email") {
          True -> Error("email is not readable")
          False -> Ok(Nil)
        }
      })

    query.new()
    |> query.add_query(user_query)
    |> query.add_type(user_type)
    |> query.build
  }

  executor.execute_query(guarded_schema, "{ user { name } }").errors
  |> should.equal([])

  executor.execute_query(guarded_schema, "{ user { name email } }").errors
  |> should.not_equal([])
}

pub fn low_level_resolver_sees_selection_test() {
  // schema.resolver receives ResolverInfo; the selection rides on info.context
  let user_type =
    schema.object("User")
    |> schema.id_field("id")
    |> schema.required_string_field("name")
    |> schema.required_string_field("email")

  let user_field =
    schema.field_def("user", schema.named_type("User"))
    |> schema.resolver(fn(info: schema.ResolverInfo) {
      let seen =
        selection.names(schema.selection(info.context))
        |> list.sort(string.compare)
        |> string.join(",")
      Ok(
        types.record([
          types.field("id", "1"),
          types.field("name", seen),
          types.field("email", "alice@example.com"),
        ]),
      )
    })

  let low_level_schema =
    schema.schema()
    |> schema.query(schema.object("Query") |> schema.field(user_field))
    |> schema.add_type(schema.ObjectTypeDef(user_type))

  low_level_schema
  |> executor.execute_query("{ user { id name } }")
  |> response.from_execution_result
  |> response.to_json
  |> string.contains("\"id,name\"")
  |> should.be_true
}

// ============================================================================
// Type Conditions on Interfaces and Unions
// ============================================================================

pub type Cat {
  Cat(id: String, name: String, meows: Bool)
}

pub type Dog {
  Dog(id: String, name: String, barks: Bool)
}

/// Animal interface implemented by Cat and Dog. The `animal` resolver records
/// what it saw for each concrete type into `errors`, so the assertions can read
/// them without depending on how the interface value is serialized.
fn build_animal_schema(
  observe: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let animal_interface =
    schema.interface("Animal")
    |> schema.interface_field(schema.field_def("id", schema.id_type()))
    |> schema.interface_field(schema.field_def("name", schema.string_type()))
    |> schema.interface_resolve_type(fn(_) { Ok("Cat") })

  let cat_type =
    types.object("Cat")
    |> types.id("id", fn(c: Cat) { c.id })
    |> types.string("name", fn(c: Cat) { c.name })
    |> types.bool("meows", fn(c: Cat) { c.meows })
    |> types.build(fn(d) { Ok(unsafe_coerce(d)) })
    |> fn(obj) { schema.ObjectType(..obj, interfaces: [animal_interface]) }

  let dog_type =
    types.object("Dog")
    |> types.id("id", fn(d: Dog) { d.id })
    |> types.string("name", fn(d: Dog) { d.name })
    |> types.bool("barks", fn(d: Dog) { d.barks })
    |> types.build(fn(d) { Ok(unsafe_coerce(d)) })
    |> fn(obj) { schema.ObjectType(..obj, interfaces: [animal_interface]) }

  let animal_query =
    query.query(
      name: "animal",
      returns: schema.Named("Animal"),
      resolve: fn(ctx) {
        case observe(ctx) {
          Ok(Nil) -> Ok(Cat("1", "Whiskers", True))
          Error(msg) -> Error(error.new(msg))
        }
      },
    )

  query.new()
  |> query.add_query(animal_query)
  |> query.add_types([cat_type, dog_type])
  |> query.add_interface(animal_interface)
  |> query.build
}

/// Field names that apply when the resolver returns `type_name`.
fn fields_for(ctx: schema.ExecutionContext, type_name: String) -> List(String) {
  schema.selection(ctx)
  |> selection.for_type(type_name)
  |> selection.names
}

/// Fails the resolver when `seen` isn't `expected`, so a mismatch surfaces as a
/// GraphQL error carrying both lists.
fn expect_fields(
  seen: List(String),
  expected: List(String),
) -> Result(Nil, String) {
  let sorted = list.sort(seen, string.compare)
  case sorted == expected {
    True -> Ok(Nil)
    False -> Error(string.inspect(sorted) <> " != " <> string.inspect(expected))
  }
}

pub fn selection_narrows_to_interface_member_test() {
  let q =
    "
    { animal { id ... on Cat { meows } ... on Dog { barks } } }
    "

  // unconditional field only
  build_animal_schema(fn(ctx) {
    expect_fields(fields_for(ctx, "Cat"), ["id", "meows"])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])

  build_animal_schema(fn(ctx) {
    expect_fields(fields_for(ctx, "Dog"), ["barks", "id"])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])

  // the unnarrowed view stays a superset spanning both types
  build_animal_schema(fn(ctx) {
    expect_fields(selection.names(schema.selection(ctx)), [
      "barks",
      "id",
      "meows",
    ])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selection_narrows_through_interface_condition_test() {
  // a fragment on the interface applies to every implementor
  let q =
    "
    { animal { ...AnimalFields ... on Cat { meows } } }
    fragment AnimalFields on Animal { id name }
    "

  build_animal_schema(fn(ctx) {
    expect_fields(fields_for(ctx, "Dog"), ["id", "name"])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])

  build_animal_schema(fn(ctx) {
    expect_fields(fields_for(ctx, "Cat"), ["id", "meows", "name"])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selection_narrows_nested_conditions_test() {
  // `... on Cat` inside `... on Animal` intersects to Cat only
  let q = "{ animal { ... on Animal { ... on Cat { meows } } } }"

  build_animal_schema(fn(ctx) { expect_fields(fields_for(ctx, "Dog"), []) })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])

  build_animal_schema(fn(ctx) {
    expect_fields(fields_for(ctx, "Cat"), ["meows"])
  })
  |> executor.execute_query(q)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selected_columns_for_ignores_sibling_type_test() {
  let ctx =
    schema.execution_context(Nil)
    |> schema.with_selection([
      schema.SelectedField(
        name: "email",
        only_for: option.Some(["User"]),
        arguments: dict.new(),
        columns: ["email"],
        children: [],
      ),
      schema.SelectedField(
        name: "body",
        only_for: option.Some(["Post"]),
        arguments: dict.new(),
        columns: ["body"],
        children: [],
      ),
    ])

  schema.selection(ctx)
  |> selection.for_type("User")
  |> selection.columns(always: ["id"])
  |> should.equal(["email", "id"])
}

// ============================================================================
// Arguments on Selected Fields
// ============================================================================

pub fn selection_carries_nested_arguments_test() {
  build_test_schema_reporting(fn(ctx) {
    case selection.find(schema.selection(ctx), "posts") {
      [posts] ->
        case selection.int_argument(posts, "first") {
          option.Some(10) -> Ok(Nil)
          other -> Error("first = " <> string.inspect(other))
        }
      other -> Error("entries = " <> string.inspect(list.length(other)))
    }
  })
  |> executor.execute_query("{ user { posts(first: 10) { id } } }")
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selection_substitutes_variables_in_arguments_test() {
  build_test_schema_reporting(fn(ctx) {
    case selection.find(schema.selection(ctx), "posts") {
      [posts] ->
        case selection.int_argument(posts, "first") {
          option.Some(3) -> Ok(Nil)
          other -> Error("first = " <> string.inspect(other))
        }
      other -> Error("entries = " <> string.inspect(list.length(other)))
    }
  })
  |> executor.execute_query_with_variables(
    "query Q($n: Int!) { user { posts(first: $n) { id } } }",
    dict.from_list([#("n", types.to_dynamic(3))]),
  )
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selection_keeps_repeated_field_with_different_arguments_test() {
  build_test_schema_reporting(fn(ctx) {
    let firsts =
      selection.find(schema.selection(ctx), "posts")
      |> list.map(fn(f) { selection.int_argument(f, "first") })
    case firsts {
      [option.Some(1), option.Some(5)] -> Ok(Nil)
      other -> Error(string.inspect(other))
    }
  })
  |> executor.execute_query(
    "{ user { posts(first: 1) { id } recent: posts(first: 5) { title } } }",
  )
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn selection_of_pools_children_of_repeated_field_test() {
  build_test_schema_reporting(fn(ctx) {
    let names =
      selection.children(schema.selection(ctx), "posts")
      |> list.map(fn(f) { f.name })
      |> list.sort(string.compare)
    expect_fields(names, ["id", "title"])
  })
  |> executor.execute_query(
    "{ user { posts(first: 1) { id } recent: posts(first: 5) { title } } }",
  )
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

/// User with a `posts` field, whose resolver reports what it saw via `observe`.
fn build_test_schema_reporting(
  observe: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let post_type =
    types.object("Post")
    |> types.id("id", fn(p: #(String, String)) { p.0 })
    |> types.string("title", fn(p: #(String, String)) { p.1 })
    |> types.build(fn(d) { Ok(unsafe_coerce(d)) })

  let posts_field =
    query.field_with_args(
      name: "posts",
      args: [query.arg("first", schema.Named("Int"))],
      returns: schema.List(schema.Named("Post")),
      decode: fn(d) { Ok(unsafe_coerce(d)) },
      resolve: fn(_parent: User, _args, _ctx) { Ok([#("p1", "Hello")]) },
    )

  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)
    |> schema.field(query.to_field_def(posts_field))

  let user_query =
    query.query(name: "user", returns: schema.Named("User"), resolve: fn(ctx) {
      case observe(ctx) {
        Ok(Nil) -> Ok(User("1", "Alice", "alice@example.com"))
        Error(msg) -> Error(error.new(msg))
      }
    })

  query.new()
  |> query.add_query(user_query)
  |> query.add_types([user_type, post_type])
  |> query.build
}

// ============================================================================
// Schema-derived Columns
// ============================================================================

pub fn snake_case_converts_graphql_names_test() {
  schema.snake_case("fullName") |> should.equal("full_name")
  schema.snake_case("publishedAt") |> should.equal("published_at")
  schema.snake_case("id") |> should.equal("id")
  schema.snake_case("created_at") |> should.equal("created_at")
  // capital runs collapse instead of splitting per letter
  schema.snake_case("userID") |> should.equal("user_id")
  schema.snake_case("HTTPStatus") |> should.equal("http_status")
  schema.snake_case("URL") |> should.equal("url")
}

/// A type that declares its columns next to each field — no mapping table.
fn declared_user_type() {
  let posts_field =
    query.field(
      name: "posts",
      returns: schema.List(schema.Named("Post")),
      decode: fn(d) { Ok(unsafe_coerce(d)) },
      resolve: fn(_parent: User, _ctx) { Ok([#("p1", "Hello")]) },
    )

  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("email", fn(u: User) { u.email })
  // undeclared: column is the field name in snake_case
  |> types.string("memberSince", fn(_u: User) { "1843-10-01" })
  // one field, two columns
  |> types.string("fullName", fn(u: User) { u.name })
  |> types.from_columns(["first_name", "last_name"])
  // computed from columns fullName already claims
  |> types.string("initials", fn(u: User) { u.name })
  |> types.from_columns(["first_name"])
  |> types.build(decode_user)
  |> schema.field(query.to_field_def(posts_field) |> schema.field_no_columns)
}

/// Reports the columns the resolver derived, via the resolver's error message.
fn build_declared_schema(
  observe: fn(schema.ExecutionContext) -> Result(Nil, String),
) -> schema.Schema {
  let post_type =
    types.object("Post")
    |> types.id("id", fn(p: #(String, String)) { p.0 })
    |> types.string("title", fn(p: #(String, String)) { p.1 })
    |> types.build(fn(d) { Ok(unsafe_coerce(d)) })

  let user_query =
    query.query(name: "user", returns: schema.Named("User"), resolve: fn(ctx) {
      case observe(ctx) {
        Ok(Nil) -> Ok(User("1", "Alice", "alice@example.com"))
        Error(msg) -> Error(error.new(msg))
      }
    })

  query.new()
  |> query.add_query(user_query)
  |> query.add_types([declared_user_type(), post_type])
  |> query.build
}

fn expect_columns(
  seen: List(String),
  expected: List(String),
) -> Result(Nil, String) {
  let sorted = list.sort(seen, string.compare)
  case sorted == expected {
    True -> Ok(Nil)
    False -> Error(string.inspect(sorted) <> " != " <> string.inspect(expected))
  }
}

fn run_declared(gql: String, expected: List(String)) -> Nil {
  build_declared_schema(fn(ctx) {
    expect_columns(query.selected_columns(ctx, always: ["id"]), expected)
  })
  |> executor.execute_query(gql)
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

pub fn derived_columns_use_declarations_test() {
  run_declared("{ user { fullName } }", ["first_name", "id", "last_name"])
}

pub fn derived_columns_default_to_snake_case_name_test() {
  run_declared("{ user { memberSince } }", ["id", "member_since"])
}

pub fn derived_columns_same_name_needs_no_declaration_test() {
  run_declared("{ user { email } }", ["email", "id"])
}

pub fn derived_columns_skip_no_column_fields_test() {
  // `posts` is a join: it contributes no column of its own
  run_declared("{ user { posts { title } } }", ["id"])
}

pub fn derived_columns_dedupe_shared_columns_test() {
  // fullName and initials both claim first_name
  run_declared("{ user { fullName initials } }", [
    "first_name", "id", "last_name",
  ])
}

pub fn derived_columns_follow_aliases_and_fragments_test() {
  run_declared(
    "{ user { who: fullName ...Extra } } fragment Extra on User { email }",
    ["email", "first_name", "id", "last_name"],
  )
}

pub fn derived_columns_drop_skipped_fields_test() {
  run_declared("{ user { fullName email @skip(if: true) } }", [
    "first_name", "id", "last_name",
  ])
}

pub fn derived_columns_of_nested_field_test() {
  // a parent prefetching children reads the child's own columns
  build_declared_schema(fn(ctx) {
    case selection.find(schema.selection(ctx), "posts") {
      [posts, ..] ->
        expect_columns(selection.columns(posts.children, always: ["user_id"]), [
          "title", "user_id",
        ])
      [] -> Error("posts not selected")
    }
  })
  |> executor.execute_query("{ user { posts { title } } }")
  |> fn(r: executor.ExecutionResult) { r.errors }
  |> should.equal([])
}

// ============================================================================
// check_columns
// ============================================================================

pub fn check_columns_passes_when_table_has_every_column_test() {
  build_declared_schema(fn(_) { Ok(Nil) })
  |> query.check_columns("User", table: [
    "id", "email", "member_since", "first_name", "last_name",
  ])
  |> should.equal(Ok(Nil))
}

pub fn check_columns_reports_a_missing_column_test() {
  // `memberSince` is undeclared, so it asks for member_since — which this table
  // spells differently
  let result =
    build_declared_schema(fn(_) { Ok(Nil) })
    |> query.check_columns("User", table: [
      "id", "email", "created_at", "first_name", "last_name",
    ])

  case result {
    Ok(Nil) -> should.fail()
    Error(problems) -> {
      list.length(problems) |> should.equal(1)
      let message = string.join(problems, "")
      message |> string.contains("User.memberSince") |> should.be_true
      message |> string.contains("member_since") |> should.be_true
      // undeclared fields get told how to fix it
      message |> string.contains("types.from_columns") |> should.be_true
    }
  }
}

pub fn check_columns_ignores_no_column_fields_test() {
  // `posts` is a join — it must not be reported as a missing column
  build_declared_schema(fn(_) { Ok(Nil) })
  |> query.check_columns("User", table: [
    "id", "email", "member_since", "first_name", "last_name",
  ])
  |> should.equal(Ok(Nil))
}

pub fn check_columns_rejects_unknown_type_test() {
  build_declared_schema(fn(_) { Ok(Nil) })
  |> query.check_columns("Nope", table: ["id"])
  |> should.equal(Error(["type 'Nope' is not in the schema"]))
}
