// Test for TypeScript code generation

import gleam/io
import gleam/string
import mochi/codegen/typescript
import mochi/schema
import mochi/types

// Test types
pub type User {
  User(id: String, name: String, email: String, age: Int)
}

fn decode_user(_dyn) -> Result(User, String) {
  Ok(User("1", "test", "test@example.com", 25))
}

pub fn typescript_generation_test() {
  // Create a schema
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string_with_desc("name", "The user's display name", fn(u: User) {
      u.name
    })
    |> types.string("email", fn(u: User) { u.email })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  // Create enum
  let role_enum =
    types.enum_type("Role")
    |> types.enum_description("User roles")
    |> types.value_with_desc("ADMIN", "Administrator access")
    |> types.value_with_desc("USER", "Regular user access")
    |> types.value("GUEST")
    |> types.build_enum

  // Create query type
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
      |> schema.field_description("Get all users")
      |> schema.resolver(fn(_) { Error("test") }),
    )

  // Build schema
  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.EnumTypeDef(role_enum))

  // Generate TypeScript
  let ts_code = typescript.generate(test_schema)

  // Verify output contains expected parts
  let has_scalars = string.contains(ts_code, "type Scalars")
  let has_user = string.contains(ts_code, "interface User")
  let has_role = string.contains(ts_code, "enum Role")
  let has_query = string.contains(ts_code, "interface Query")
  let has_maybe = string.contains(ts_code, "type Maybe<T>")

  case has_scalars && has_user && has_role && has_query && has_maybe {
    True -> Nil
    False -> panic as "TypeScript generation missing expected parts"
  }
}

// Print example output
pub fn main() {
  io.println("🍡 TypeScript Codegen Test")
  io.println("==========================\n")

  // Create a simple schema
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string_with_desc("name", "The user's name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.int("age", fn(u: User) { u.age })
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
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users")
      |> schema.resolver(fn(_) { Error("test") }),
    )

  let test_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.EnumTypeDef(role_enum))

  // Generate and print TypeScript
  let ts_code = typescript.generate(test_schema)

  io.println("Generated TypeScript:")
  io.println("---------------------")
  io.println(ts_code)
}
