// Test for SDL code generation

import gleam/io
import gleam/string
import mochi/codegen/sdl
import mochi/schema
import mochi/types

// Test types
pub type User {
  User(id: String, name: String)
}

fn decode_user(_dyn) -> Result(User, String) {
  Ok(User("1", "test"))
}

pub fn sdl_generation_test() {
  // Create a schema
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string_with_desc("name", "The user's name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let role_enum =
    types.enum_type("Role")
    |> types.enum_description("User roles")
    |> types.value("ADMIN")
    |> types.value("USER")
    |> types.build_enum

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user by ID")
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type())))
      |> schema.resolver(fn(_) { Error("test") }),
    )
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.EnumTypeDef(role_enum))

  // Generate SDL
  let sdl_code = sdl.generate(test_schema)

  // Verify output contains expected parts
  let has_type_user = string.contains(sdl_code, "type User")
  let has_enum_role = string.contains(sdl_code, "enum Role")
  let has_type_query = string.contains(sdl_code, "type Query")
  let has_field_user = string.contains(sdl_code, "user(id: ID!): User")

  case has_type_user && has_enum_role && has_type_query && has_field_user {
    True -> Nil
    False -> panic as "SDL generation missing expected parts"
  }
}

pub fn sdl_description_test() {
  let user_type =
    types.object("User")
    |> types.description("A user entity")
    |> types.id("id", fn(u: User) { u.id })
    |> types.build(decode_user)

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))

  let sdl_code = sdl.generate(test_schema)

  // Should include description
  let has_description = string.contains(sdl_code, "A user entity")

  case has_description {
    True -> Nil
    False -> panic as "SDL should include descriptions"
  }
}

pub fn sdl_list_type_test() {
  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def(
        "users",
        schema.non_null(schema.list_type(schema.named_type("User"))),
      )
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)

  let sdl_code = sdl.generate(test_schema)

  // Should have correct list syntax
  let has_list = string.contains(sdl_code, "[User]!")

  case has_list {
    True -> Nil
    False -> panic as "SDL should have correct list type syntax"
  }
}

// Print example output
pub fn main() {
  io.println("🍡 SDL Codegen Test")
  io.println("===================\n")

  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string_with_desc("name", "The user's display name", fn(u: User) {
      u.name
    })
    |> types.build(decode_user)

  let role_enum =
    types.enum_type("Role")
    |> types.enum_description("User roles in the system")
    |> types.value_with_desc("ADMIN", "Full access")
    |> types.value_with_desc("USER", "Standard access")
    |> types.value("GUEST")
    |> types.build_enum

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user by ID")
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type())))
      |> schema.resolver(fn(_) { Error("test") }),
    )
    |> schema.field(
      schema.field_def(
        "users",
        schema.non_null(schema.list_type(schema.named_type("User"))),
      )
      |> schema.field_description("Get all users")
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let mutation_type =
    schema.object("Mutation")
    |> schema.field(
      schema.field_def("createUser", schema.named_type("User"))
      |> schema.argument(schema.arg(
        "name",
        schema.non_null(schema.string_type()),
      ))
      |> schema.argument(schema.arg("email", schema.string_type()))
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.mutation(mutation_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.EnumTypeDef(role_enum))

  let sdl_code = sdl.generate(test_schema)

  io.println("Generated SDL:")
  io.println("--------------")
  io.println(sdl_code)
}
