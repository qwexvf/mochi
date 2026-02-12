// examples/with_mutations.gleam
// Schema with mutations example demonstrating CRUD operations

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/codegen/sdl
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

/// A user in the system
pub type User {
  User(id: String, name: String, email: String, role: Role)
}

/// User roles
pub type Role {
  Admin
  Member
  Guest
}

/// Input for creating a new user
pub type CreateUserInput {
  CreateUserInput(name: String, email: String, role: Option(String))
}

/// Input for updating an existing user
pub type UpdateUserInput {
  UpdateUserInput(name: Option(String), email: Option(String), role: Option(String))
}

/// Payload returned after creating a user
pub type CreateUserPayload {
  CreateUserPayload(user: User, success: Bool)
}

/// Payload returned after updating a user
pub type UpdateUserPayload {
  UpdateUserPayload(user: Option(User), success: Bool, message: String)
}

/// Payload returned after deleting a user
pub type DeleteUserPayload {
  DeleteUserPayload(success: Bool, message: String, deleted_id: Option(String))
}

// ============================================================================
// Mock Database (in-memory)
// ============================================================================

fn get_users() -> List(User) {
  [
    User("1", "Alice", "alice@example.com", Admin),
    User("2", "Bob", "bob@example.com", Member),
    User("3", "Charlie", "charlie@example.com", Guest),
  ]
}

fn get_user_by_id(id: String) -> Result(User, String) {
  case list.find(get_users(), fn(u) { u.id == id }) {
    Ok(user) -> Ok(user)
    Error(_) -> Error("User not found: " <> id)
  }
}

// ============================================================================
// Decoders
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  case dynamic.unsafe_coerce(dyn) {
    user -> Ok(user)
  }
}

fn decode_create_input(args: dict.Dict(String, Dynamic)) -> Result(CreateUserInput, String) {
  case dict.get(args, "input") {
    Ok(_) -> Ok(CreateUserInput("New User", "new@example.com", None))
    Error(_) -> Error("Missing input argument")
  }
}

fn decode_update_input(args: dict.Dict(String, Dynamic)) -> Result(#(String, UpdateUserInput), String) {
  case dict.get(args, "id"), dict.get(args, "input") {
    Ok(id_dyn), Ok(_) -> {
      case dynamic.string(id_dyn) {
        Ok(id) -> Ok(#(id, UpdateUserInput(Some("Updated"), None, None)))
        Error(_) -> Error("Invalid ID")
      }
    }
    _, _ -> Error("Missing id or input argument")
  }
}

fn decode_id_arg(args: dict.Dict(String, Dynamic)) -> Result(String, String) {
  case dict.get(args, "id") {
    Ok(id_dyn) -> {
      case dynamic.string(id_dyn) {
        Ok(id) -> Ok(id)
        Error(_) -> Error("Invalid ID type")
      }
    }
    Error(_) -> Error("Missing id argument")
  }
}

// ============================================================================
// GraphQL Type Definitions
// ============================================================================

/// User type
fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.build(decode_user)
}

/// Role enum
fn role_enum() -> schema.EnumType {
  types.enum_type("Role")
  |> types.enum_description("User roles in the system")
  |> types.value_with_desc("ADMIN", "Full system access")
  |> types.value_with_desc("MEMBER", "Standard user access")
  |> types.value_with_desc("GUEST", "Limited read-only access")
  |> types.build_enum
}

/// CreateUserPayload type
fn create_user_payload_type() -> schema.ObjectType {
  types.object("CreateUserPayload")
  |> types.description("Result of creating a user")
  |> types.bool("success", fn(p: CreateUserPayload) { p.success })
  |> types.build(fn(_) { Ok(CreateUserPayload(User("", "", "", Guest), True)) })
}

/// UpdateUserPayload type
fn update_user_payload_type() -> schema.ObjectType {
  types.object("UpdateUserPayload")
  |> types.description("Result of updating a user")
  |> types.bool("success", fn(p: UpdateUserPayload) { p.success })
  |> types.string("message", fn(p: UpdateUserPayload) { p.message })
  |> types.build(fn(_) { Ok(UpdateUserPayload(None, True, "")) })
}

/// DeleteUserPayload type
fn delete_user_payload_type() -> schema.ObjectType {
  types.object("DeleteUserPayload")
  |> types.description("Result of deleting a user")
  |> types.bool("success", fn(p: DeleteUserPayload) { p.success })
  |> types.string("message", fn(p: DeleteUserPayload) { p.message })
  |> types.build(fn(_) { Ok(DeleteUserPayload(True, "", None)) })
}

// ============================================================================
// Query Definitions
// ============================================================================

fn users_query() -> query.QueryDef(query.NoArgs, List(User)) {
  query.query(
    "users",
    schema.non_null(schema.list_type(schema.named_type("User"))),
    fn(_ctx) { Ok(get_users()) },
    types.to_dynamic,
  )
  |> query.query_description("Get all users")
}

fn user_query() -> query.QueryDef(String, User) {
  query.query_with_args(
    "user",
    [query.arg("id", schema.non_null(schema.id_type()))],
    schema.named_type("User"),
    decode_id_arg,
    fn(id, _ctx) { get_user_by_id(id) },
    types.to_dynamic,
  )
  |> query.query_description("Get a user by ID")
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
    schema.non_null(schema.named_type("CreateUserPayload")),
    decode_create_input,
    fn(input, _ctx) {
      // In practice, this would insert into a database
      let new_user = User(
        id: "new-" <> input.name,
        name: input.name,
        email: input.email,
        role: Member,
      )
      Ok(CreateUserPayload(user: new_user, success: True))
    },
    types.to_dynamic,
  )
  |> query.mutation_description("Create a new user account")
}

/// Mutation: updateUser - Update an existing user
fn update_user_mutation() -> query.MutationDef(#(String, UpdateUserInput), UpdateUserPayload) {
  query.mutation(
    "updateUser",
    [
      query.arg("id", schema.non_null(schema.id_type())),
      query.arg("input", schema.non_null(schema.named_type("UpdateUserInput"))),
    ],
    schema.non_null(schema.named_type("UpdateUserPayload")),
    decode_update_input,
    fn(args, _ctx) {
      let #(id, _input) = args
      case get_user_by_id(id) {
        Ok(user) -> {
          // In practice, update the user in database
          Ok(UpdateUserPayload(
            user: Some(user),
            success: True,
            message: "User updated successfully",
          ))
        }
        Error(_) -> {
          Ok(UpdateUserPayload(
            user: None,
            success: False,
            message: "User not found",
          ))
        }
      }
    },
    types.to_dynamic,
  )
  |> query.mutation_description("Update an existing user")
}

/// Mutation: deleteUser - Delete a user
fn delete_user_mutation() -> query.MutationDef(String, DeleteUserPayload) {
  query.mutation(
    "deleteUser",
    [query.arg("id", schema.non_null(schema.id_type()))],
    schema.non_null(schema.named_type("DeleteUserPayload")),
    decode_id_arg,
    fn(id, _ctx) {
      case get_user_by_id(id) {
        Ok(_user) -> {
          // In practice, delete from database
          Ok(DeleteUserPayload(
            success: True,
            message: "User deleted successfully",
            deleted_id: Some(id),
          ))
        }
        Error(_) -> {
          Ok(DeleteUserPayload(
            success: False,
            message: "User not found",
            deleted_id: None,
          ))
        }
      }
    },
    types.to_dynamic,
  )
  |> query.mutation_description("Delete a user by ID")
}

// ============================================================================
// Build Schema
// ============================================================================

pub fn create_schema() -> schema.Schema {
  query.new()
  // Queries
  |> query.add_query(users_query())
  |> query.add_query(user_query())
  // Mutations
  |> query.add_mutation(create_user_mutation())
  |> query.add_mutation(update_user_mutation())
  |> query.add_mutation(delete_user_mutation())
  // Types
  |> query.add_type(user_type())
  |> query.add_type(create_user_payload_type())
  |> query.add_type(update_user_payload_type())
  |> query.add_type(delete_user_payload_type())
  // Enums
  |> query.add_enum(role_enum())
  // Build
  |> query.build
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  io.println("Schema with Mutations Example")
  io.println("==============================")
  io.println("")

  let my_schema = create_schema()
  io.println("Schema created successfully!")
  io.println("")

  // Show available operations
  io.println("Available Queries:")
  io.println("  - users: [User]!")
  io.println("  - user(id: ID!): User")
  io.println("")

  io.println("Available Mutations:")
  io.println("  - createUser(input: CreateUserInput!): CreateUserPayload!")
  io.println("  - updateUser(id: ID!, input: UpdateUserInput!): UpdateUserPayload!")
  io.println("  - deleteUser(id: ID!): DeleteUserPayload!")
  io.println("")

  // Generate SDL
  io.println("Generated GraphQL SDL:")
  io.println("----------------------")
  let sdl_output = sdl.generate(my_schema)
  io.println(sdl_output)
}
