// examples/codegen_example.gleam
// Example demonstrating TypeScript and SDL code generation

import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/option.{None, Some}
import mochi/codegen/sdl
import mochi/codegen/typescript
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

pub type User {
  User(
    id: String,
    name: String,
    email: String,
    age: Int,
    active: Bool,
    score: Float,
  )
}

pub type Post {
  Post(id: String, title: String, body: String, author_id: String, tags: List(String))
}

pub type Comment {
  Comment(id: String, body: String, author_id: String, post_id: String)
}

pub type Status {
  Active
  Inactive
  Pending
}

// ============================================================================
// Decoders
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  case dynamic.unsafe_coerce(dyn) {
    user -> Ok(user)
  }
}

fn decode_post(dyn: Dynamic) -> Result(Post, String) {
  case dynamic.unsafe_coerce(dyn) {
    post -> Ok(post)
  }
}

fn decode_comment(dyn: Dynamic) -> Result(Comment, String) {
  case dynamic.unsafe_coerce(dyn) {
    comment -> Ok(comment)
  }
}

// ============================================================================
// GraphQL Type Definitions
// ============================================================================

fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string_with_desc("name", "The user's display name", fn(u: User) {
    u.name
  })
  |> types.string_with_desc("email", "The user's email address", fn(u: User) {
    u.email
  })
  |> types.int_with_desc("age", "The user's age in years", fn(u: User) { u.age })
  |> types.bool("active", fn(u: User) { u.active })
  |> types.float("score", fn(u: User) { u.score })
  |> types.build(decode_user)
}

fn post_type() -> schema.ObjectType {
  types.object("Post")
  |> types.description("A blog post")
  |> types.id("id", fn(p: Post) { p.id })
  |> types.string("title", fn(p: Post) { p.title })
  |> types.string("body", fn(p: Post) { p.body })
  |> types.string("authorId", fn(p: Post) { p.author_id })
  |> types.list_string("tags", fn(p: Post) { p.tags })
  |> types.build(decode_post)
}

fn comment_type() -> schema.ObjectType {
  types.object("Comment")
  |> types.description("A comment on a post")
  |> types.id("id", fn(c: Comment) { c.id })
  |> types.string("body", fn(c: Comment) { c.body })
  |> types.string("authorId", fn(c: Comment) { c.author_id })
  |> types.string("postId", fn(c: Comment) { c.post_id })
  |> types.build(decode_comment)
}

fn status_enum() -> schema.EnumType {
  types.enum_type("Status")
  |> types.enum_description("User or entity status")
  |> types.value_with_desc("ACTIVE", "Currently active")
  |> types.value_with_desc("INACTIVE", "Not currently active")
  |> types.value_with_desc("PENDING", "Awaiting activation")
  |> types.deprecated_value_with_reason("LEGACY", "Use INACTIVE instead")
  |> types.build_enum
}

// ============================================================================
// Query Definitions
// ============================================================================

fn users_query() -> query.QueryDef(query.NoArgs, List(User)) {
  query.query(
    "users",
    schema.non_null(schema.list_type(schema.named_type("User"))),
    fn(_ctx) { Ok([]) },
    types.to_dynamic,
  )
  |> query.query_description("Get all users")
}

fn user_query() {
  query.query_with_args(
    "user",
    [query.arg_with_desc("id", schema.non_null(schema.id_type()), "User ID")],
    schema.named_type("User"),
    fn(_) { Ok("1") },
    fn(_id, _ctx) { Ok(User("1", "Test", "test@test.com", 25, True, 4.5)) },
    types.to_dynamic,
  )
  |> query.query_description("Get a user by ID")
}

fn posts_query() {
  query.query(
    "posts",
    schema.non_null(schema.list_type(schema.named_type("Post"))),
    fn(_ctx) { Ok([]) },
    types.to_dynamic,
  )
  |> query.query_description("Get all posts")
}

fn comments_query() {
  query.query_with_args(
    "comments",
    [
      query.arg_with_desc(
        "postId",
        schema.non_null(schema.id_type()),
        "Post ID to get comments for",
      ),
    ],
    schema.non_null(schema.list_type(schema.named_type("Comment"))),
    fn(_) { Ok("1") },
    fn(_id, _ctx) { Ok([]) },
    types.to_dynamic,
  )
  |> query.query_description("Get comments for a post")
}

// ============================================================================
// Mutation Definitions
// ============================================================================

fn create_user_mutation() {
  query.mutation(
    "createUser",
    [
      query.arg("name", schema.non_null(schema.string_type())),
      query.arg("email", schema.non_null(schema.string_type())),
      query.arg("age", schema.int_type()),
    ],
    schema.non_null(schema.named_type("User")),
    fn(_) { Ok(#("name", "email", 0)) },
    fn(_input, _ctx) {
      Ok(User("new", "New User", "new@test.com", 0, True, 0.0))
    },
    types.to_dynamic,
  )
  |> query.mutation_description("Create a new user")
}

fn create_post_mutation() {
  query.mutation(
    "createPost",
    [
      query.arg("title", schema.non_null(schema.string_type())),
      query.arg("body", schema.non_null(schema.string_type())),
      query.arg("tags", schema.list_type(schema.string_type())),
    ],
    schema.non_null(schema.named_type("Post")),
    fn(_) { Ok(#("title", "body", [])) },
    fn(_input, _ctx) { Ok(Post("new", "Title", "Body", "1", [])) },
    types.to_dynamic,
  )
  |> query.mutation_description("Create a new post")
}

// ============================================================================
// Build Schema
// ============================================================================

pub fn create_schema() -> schema.Schema {
  query.new()
  // Queries
  |> query.add_query(users_query())
  |> query.add_query(user_query())
  |> query.add_query(posts_query())
  |> query.add_query(comments_query())
  // Mutations
  |> query.add_mutation(create_user_mutation())
  |> query.add_mutation(create_post_mutation())
  // Types
  |> query.add_type(user_type())
  |> query.add_type(post_type())
  |> query.add_type(comment_type())
  // Enums
  |> query.add_enum(status_enum())
  // Build
  |> query.build
}

// ============================================================================
// Code Generation Examples
// ============================================================================

/// Generate TypeScript with default configuration
pub fn generate_typescript_default(my_schema: schema.Schema) -> String {
  typescript.generate(my_schema)
}

/// Generate TypeScript with custom configuration
pub fn generate_typescript_custom(my_schema: schema.Schema) -> String {
  let config =
    typescript.Config(
      use_exports: True,
      use_interfaces: True,
      readonly_properties: True,
      include_helpers: True,
      header: Some(
        "// Generated by mochi\n// Do not edit manually\n// Schema version: 1.0.0",
      ),
    )
  typescript.generate_with_config(my_schema, config)
}

/// Generate SDL with default configuration
pub fn generate_sdl_default(my_schema: schema.Schema) -> String {
  sdl.generate(my_schema)
}

/// Generate SDL with custom configuration
pub fn generate_sdl_custom(my_schema: schema.Schema) -> String {
  let config =
    sdl.Config(
      include_descriptions: True,
      include_builtin_scalars: False,
      indent: "    ",
      header: Some("# My Application Schema\n# Generated by mochi"),
    )
  sdl.generate_with_config(my_schema, config)
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  io.println("Code Generation Example")
  io.println("=======================")
  io.println("")

  let my_schema = create_schema()
  io.println("Schema created successfully!")
  io.println("")

  // Default TypeScript generation
  io.println("=== TypeScript (Default Config) ===")
  io.println("")
  let ts_default = generate_typescript_default(my_schema)
  io.println(ts_default)
  io.println("")

  // Custom TypeScript generation
  io.println("=== TypeScript (Custom Config with readonly) ===")
  io.println("")
  let ts_custom = generate_typescript_custom(my_schema)
  io.println(ts_custom)
  io.println("")

  // Default SDL generation
  io.println("=== GraphQL SDL (Default Config) ===")
  io.println("")
  let sdl_default = generate_sdl_default(my_schema)
  io.println(sdl_default)
  io.println("")

  // Custom SDL generation
  io.println("=== GraphQL SDL (Custom Config with 4-space indent) ===")
  io.println("")
  let sdl_custom = generate_sdl_custom(my_schema)
  io.println(sdl_custom)
  io.println("")

  io.println("=== Configuration Options ===")
  io.println("")
  io.println("TypeScript Config:")
  io.println("  - use_exports: Bool      - Add 'export' before declarations")
  io.println("  - use_interfaces: Bool   - Use 'interface' instead of 'type'")
  io.println("  - readonly_properties: Bool - Add 'readonly' to properties")
  io.println("  - include_helpers: Bool  - Include Maybe<T> and other helpers")
  io.println("  - header: Option(String) - Custom header comment")
  io.println("")
  io.println("SDL Config:")
  io.println("  - include_descriptions: Bool     - Include field descriptions")
  io.println("  - include_builtin_scalars: Bool  - Include String, Int, etc.")
  io.println("  - indent: String                 - Indentation string")
  io.println("  - header: Option(String)         - Custom header comment")
}
