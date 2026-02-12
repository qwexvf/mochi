// examples/basic_schema.gleam
// Basic User/Post schema example demonstrating mochi's Code First API

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/list
import mochi/codegen/sdl
import mochi/codegen/typescript
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

/// A user in the system
pub type User {
  User(id: String, name: String, email: String, age: Int)
}

/// A blog post
pub type Post {
  Post(id: String, title: String, body: String, author_id: String)
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

// ============================================================================
// Mock Data
// ============================================================================

fn get_users() -> List(User) {
  [
    User("1", "Alice", "alice@example.com", 30),
    User("2", "Bob", "bob@example.com", 25),
    User("3", "Charlie", "charlie@example.com", 35),
  ]
}

fn get_user_by_id(id: String) -> Result(User, String) {
  case list.find(get_users(), fn(u) { u.id == id }) {
    Ok(user) -> Ok(user)
    Error(_) -> Error("User not found: " <> id)
  }
}

fn get_posts() -> List(Post) {
  [
    Post("1", "Hello World", "My first post!", "1"),
    Post("2", "Gleam is Great", "Why I love Gleam...", "2"),
    Post("3", "GraphQL with mochi", "Building type-safe APIs", "1"),
  ]
}

fn get_posts_by_author(author_id: String) -> List(Post) {
  list.filter(get_posts(), fn(p) { p.author_id == author_id })
}

// ============================================================================
// GraphQL Type Definitions
// ============================================================================

/// Define the User GraphQL type
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
  |> types.build(decode_user)
}

/// Define the Post GraphQL type
fn post_type() -> schema.ObjectType {
  types.object("Post")
  |> types.description("A blog post")
  |> types.id("id", fn(p: Post) { p.id })
  |> types.string_with_desc("title", "The post title", fn(p: Post) { p.title })
  |> types.string_with_desc("body", "The post content", fn(p: Post) { p.body })
  |> types.string("authorId", fn(p: Post) { p.author_id })
  |> types.build(decode_post)
}

// ============================================================================
// Query Definitions
// ============================================================================

/// Query: users - Get all users
fn users_query() -> query.QueryDef(query.NoArgs, List(User)) {
  query.query(
    "users",
    schema.non_null(schema.list_type(schema.named_type("User"))),
    fn(_ctx) { Ok(get_users()) },
    fn(users) { types.to_dynamic(users) },
  )
  |> query.query_description("Get all users in the system")
}

/// Query: user(id: ID!) - Get a specific user by ID
fn user_query() -> query.QueryDef(String, User) {
  query.query_with_args(
    "user",
    [query.arg_with_desc("id", schema.non_null(schema.id_type()), "The user ID")],
    schema.named_type("User"),
    fn(args) {
      case dict.get(args, "id") {
        Ok(id_dyn) ->
          case dynamic.string(id_dyn) {
            Ok(id) -> Ok(id)
            Error(_) -> Error("Invalid ID type")
          }
        Error(_) -> Error("Missing id argument")
      }
    },
    fn(id, _ctx) { get_user_by_id(id) },
    fn(user) { types.to_dynamic(user) },
  )
  |> query.query_description("Get a user by their ID")
}

/// Query: posts - Get all posts
fn posts_query() -> query.QueryDef(query.NoArgs, List(Post)) {
  query.query(
    "posts",
    schema.non_null(schema.list_type(schema.named_type("Post"))),
    fn(_ctx) { Ok(get_posts()) },
    fn(posts) { types.to_dynamic(posts) },
  )
  |> query.query_description("Get all blog posts")
}

/// Query: postsByAuthor(authorId: ID!) - Get posts by a specific author
fn posts_by_author_query() -> query.QueryDef(String, List(Post)) {
  query.query_with_args(
    "postsByAuthor",
    [
      query.arg_with_desc(
        "authorId",
        schema.non_null(schema.id_type()),
        "The author's user ID",
      ),
    ],
    schema.non_null(schema.list_type(schema.named_type("Post"))),
    fn(args) {
      case dict.get(args, "authorId") {
        Ok(id_dyn) ->
          case dynamic.string(id_dyn) {
            Ok(id) -> Ok(id)
            Error(_) -> Error("Invalid authorId type")
          }
        Error(_) -> Error("Missing authorId argument")
      }
    },
    fn(author_id, _ctx) { Ok(get_posts_by_author(author_id)) },
    fn(posts) { types.to_dynamic(posts) },
  )
  |> query.query_description("Get all posts by a specific author")
}

// ============================================================================
// Build Schema
// ============================================================================

/// Build the complete GraphQL schema
pub fn create_schema() -> schema.Schema {
  query.new()
  // Add queries
  |> query.add_query(users_query())
  |> query.add_query(user_query())
  |> query.add_query(posts_query())
  |> query.add_query(posts_by_author_query())
  // Add types
  |> query.add_type(user_type())
  |> query.add_type(post_type())
  // Build
  |> query.build
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  io.println("Basic Schema Example")
  io.println("====================")
  io.println("")

  let my_schema = create_schema()
  io.println("Schema created successfully!")
  io.println("")

  // Generate SDL
  io.println("Generated GraphQL SDL:")
  io.println("----------------------")
  let sdl_output = sdl.generate(my_schema)
  io.println(sdl_output)
  io.println("")

  // Generate TypeScript
  io.println("Generated TypeScript:")
  io.println("---------------------")
  let ts_output = typescript.generate(my_schema)
  io.println(ts_output)
}
