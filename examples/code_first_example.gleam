// examples/code_first_example.gleam
// Demonstrates mochi's Code First API inspired by gqlkit

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types - Just regular Gleam types!
// ============================================================================

pub type User {
  User(id: String, name: String, email: String, age: Int, role: Role)
}

pub type Role {
  Admin
  Member
  Guest
}

pub type Post {
  Post(id: String, title: String, body: String, author_id: String)
}

pub type CreateUserInput {
  CreateUserInput(name: String, email: String, age: Int)
}

pub type CreateUserPayload {
  CreateUserPayload(user: User)
}

// ============================================================================
// Type Decoders - For extracting typed data from Dynamic
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  // In practice, you'd use dynamic.decode5 or similar
  // This is simplified for the example
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
// GraphQL Type Definitions - Clean, type-safe builders
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
  |> types.string("title", fn(p: Post) { p.title })
  |> types.string("body", fn(p: Post) { p.body })
  |> types.string("authorId", fn(p: Post) { p.author_id })
  |> types.build(decode_post)
}

/// Define the Role enum
fn role_enum() -> schema.EnumType {
  types.enum_type("Role")
  |> types.enum_description("User roles in the system")
  |> types.value_with_desc("ADMIN", "Full system access")
  |> types.value_with_desc("MEMBER", "Standard user access")
  |> types.value_with_desc("GUEST", "Limited read-only access")
  |> types.build_enum
}

// ============================================================================
// Mock Database
// ============================================================================

fn get_users() -> List(User) {
  [
    User("1", "Alice", "alice@example.com", 30, Admin),
    User("2", "Bob", "bob@example.com", 25, Member),
    User("3", "Charlie", "charlie@example.com", 35, Guest),
  ]
}

fn get_user_by_id(id: String) -> Result(User, String) {
  case list.find(get_users(), fn(u) { u.id == id }) {
    Ok(user) -> Ok(user)
    Error(_) -> Error("User not found")
  }
}

fn get_posts() -> List(Post) {
  [
    Post("1", "Hello World", "My first post!", "1"),
    Post("2", "Gleam is Great", "Why I love Gleam...", "2"),
  ]
}

// ============================================================================
// Query Definitions - gqlkit-style!
// ============================================================================

/// Query: users - Get all users
fn users_query() -> query.QueryDef(query.NoArgs, List(User)) {
  query.query(
    "users",
    schema.list_type(schema.named_type("User")),
    fn(_ctx) { Ok(get_users()) },
    fn(users) { types.to_dynamic(users) },
  )
  |> query.query_description("Get all users in the system")
}

/// Query: user(id: ID!) - Get a specific user
fn user_query() -> query.QueryDef(String, User) {
  query.query_with_args(
    "user",
    [query.arg("id", schema.non_null(schema.id_type()))],
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
    schema.list_type(schema.named_type("Post")),
    fn(_ctx) { Ok(get_posts()) },
    fn(posts) { types.to_dynamic(posts) },
  )
  |> query.query_description("Get all blog posts")
}

// ============================================================================
// Mutation Definitions
// ============================================================================

/// Mutation: createUser - Create a new user
fn create_user_mutation() -> query.MutationDef(CreateUserInput, CreateUserPayload) {
  query.mutation(
    "createUser",
    [
      query.arg_with_desc(
        "input",
        schema.non_null(schema.named_type("CreateUserInput")),
        "The user data to create",
      ),
    ],
    schema.named_type("CreateUserPayload"),
    fn(args) {
      // Simplified decoder - in practice use proper dynamic decoding
      case dict.get(args, "input") {
        Ok(_) ->
          Ok(CreateUserInput(name: "New User", email: "new@example.com", age: 20))
        Error(_) -> Error("Missing input")
      }
    },
    fn(input, _ctx) {
      // In practice, this would insert into the database
      let new_user =
        User(
          id: "new-id",
          name: input.name,
          email: input.email,
          age: input.age,
          role: Member,
        )
      Ok(CreateUserPayload(user: new_user))
    },
    fn(payload) { types.to_dynamic(payload) },
  )
  |> query.mutation_description("Create a new user")
}

// ============================================================================
// Build the Schema
// ============================================================================

pub fn create_schema() -> schema.Schema {
  query.new()
  // Add queries
  |> query.add_query(users_query())
  |> query.add_query(user_query())
  |> query.add_query(posts_query())
  // Add mutations
  |> query.add_mutation(create_user_mutation())
  // Add types
  |> query.add_type(user_type())
  |> query.add_type(post_type())
  // Build!
  |> query.build
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  io.println("🍡 mochi Code First Example")
  io.println("")

  let _schema = create_schema()
  io.println("✅ Schema created successfully!")
  io.println("")

  io.println("📋 Defined Queries:")
  io.println("  - users: [User]!")
  io.println("  - user(id: ID!): User")
  io.println("  - posts: [Post]!")
  io.println("")

  io.println("📝 Defined Mutations:")
  io.println("  - createUser(input: CreateUserInput!): CreateUserPayload")
  io.println("")

  io.println("🎉 mochi makes GraphQL type-safe and simple!")
}
