// mochi 🍡 - Code First GraphQL for Gleam
//
// A type-safe GraphQL library inspired by gqlkit
//
// ## Quick Start
//
// ```gleam
// import mochi
// import mochi/types
// import mochi/query
//
// // Define your types
// pub type User {
//   User(id: String, name: String, age: Int)
// }
//
// // Build GraphQL type
// let user_type = types.object("User")
//   |> types.id("id", fn(u: User) { u.id })
//   |> types.string("name", fn(u: User) { u.name })
//   |> types.int("age", fn(u: User) { u.age })
//   |> types.build(decode_user)
//
// // Define queries
// let users_query = query.query(
//   "users",
//   schema.list_type(schema.named_type("User")),
//   fn(_ctx) { Ok(get_users()) },
//   types.to_dynamic,
// )
//
// // Build schema
// let schema = query.new()
//   |> query.add_query(users_query)
//   |> query.add_type(user_type)
//   |> query.build
// ```

import gleam/io
import mochi/ast
import mochi/executor
import mochi/parser
import mochi/query as mochi_query
import mochi/schema

// ============================================================================
// Re-exports for convenient access
// ============================================================================

/// Parse a GraphQL query string
pub fn parse(query_string: String) -> Result(ast.Document, parser.ParseError) {
  parser.parse(query_string)
}

/// Execute a GraphQL query against a schema
pub fn execute(
  schema: schema.Schema,
  query_string: String,
) -> executor.ExecutionResult {
  executor.execute_query(schema, query_string)
}

/// Create a new schema builder
pub fn new_schema() -> mochi_query.SchemaBuilder {
  mochi_query.new()
}

// ============================================================================
// Demo
// ============================================================================

pub fn main() -> Nil {
  io.println("🍡 mochi - Code First GraphQL for Gleam")
  io.println("========================================")
  io.println("")

  // Demonstrate Code First API
  io.println("📦 Code First API Example:")
  io.println("")
  io.println("  // Define your Gleam type")
  io.println("  pub type User {")
  io.println("    User(id: String, name: String, age: Int)")
  io.println("  }")
  io.println("")
  io.println("  // Build GraphQL type with type-safe extractors")
  io.println("  let user_type = types.object(\"User\")")
  io.println("    |> types.id(\"id\", fn(u: User) { u.id })")
  io.println("    |> types.string(\"name\", fn(u: User) { u.name })")
  io.println("    |> types.int(\"age\", fn(u: User) { u.age })")
  io.println("    |> types.build(decode_user)")
  io.println("")

  // Test parsing
  let query = "{ user { id name } }"
  case parser.parse(query) {
    Ok(_document) -> {
      io.println("✅ GraphQL Parser: Working!")
    }
    Error(_error) -> {
      io.println("❌ Parser error")
    }
  }

  // Show schema creation
  let _simple_schema = create_demo_schema()
  io.println("✅ Schema Builder: Working!")
  io.println("")

  io.println("📚 Available Modules:")
  io.println("  - mochi/types   : Type builders (object, string, int, etc.)")
  io.println("  - mochi/query   : Query/Mutation builders")
  io.println("  - mochi/schema  : Low-level schema types")
  io.println("  - mochi/parser  : GraphQL query parser")
  io.println("  - mochi/executor: Query execution engine")
  io.println("")

  io.println("🎉 Ready to build type-safe GraphQL APIs!")
}

fn create_demo_schema() -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.resolver(fn(_info) { Error("Demo") }),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.resolver(fn(_info) { Error("Demo") }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.resolver(fn(_info) { Error("Demo") }),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
}
