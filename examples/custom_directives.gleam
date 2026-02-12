// examples/custom_directives.gleam
// Example demonstrating custom directive definitions

import gleam/dynamic.{type Dynamic}
import gleam/io
import mochi/codegen/sdl
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  case dynamic.unsafe_coerce(dyn) {
    user -> Ok(user)
  }
}

// ============================================================================
// GraphQL Types
// ============================================================================

fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.build(decode_user)
}

fn users_query() -> query.QueryDef(query.NoArgs, List(User)) {
  query.query(
    "users",
    schema.list_type(schema.named_type("User")),
    fn(_ctx) { Ok([]) },
    types.to_dynamic,
  )
}

// ============================================================================
// Custom Directive Definitions
// ============================================================================

/// @auth directive - Requires authentication to access the field or type
fn auth_directive() -> schema.DirectiveDefinition {
  schema.directive("auth", [schema.FieldLocation, schema.ObjectLocation])
  |> schema.directive_description(
    "Requires the user to be authenticated to access this field or type",
  )
}

/// @hasRole directive - Requires a specific role
fn has_role_directive() -> schema.DirectiveDefinition {
  schema.directive("hasRole", [schema.FieldLocation, schema.ObjectLocation])
  |> schema.directive_description("Requires the user to have a specific role")
  |> schema.directive_argument(
    schema.arg("role", schema.non_null(schema.string_type()))
    |> schema.arg_description("The required role (e.g., 'ADMIN', 'USER')"),
  )
}

/// @rateLimit directive - Rate limiting for fields
fn rate_limit_directive() -> schema.DirectiveDefinition {
  schema.directive("rateLimit", [schema.FieldLocation])
  |> schema.directive_description("Applies rate limiting to this field")
  |> schema.directive_argument(
    schema.arg("limit", schema.non_null(schema.int_type()))
    |> schema.arg_description("Maximum number of requests"),
  )
  |> schema.directive_argument(
    schema.arg("duration", schema.non_null(schema.int_type()))
    |> schema.arg_description("Time window in seconds"),
  )
}

/// @cache directive - Caching configuration
fn cache_directive() -> schema.DirectiveDefinition {
  schema.directive("cache", [schema.FieldLocation, schema.ObjectLocation])
  |> schema.directive_description("Configures caching for this field or type")
  |> schema.directive_argument(
    schema.arg("maxAge", schema.int_type())
    |> schema.arg_description("Maximum cache age in seconds"),
  )
  |> schema.directive_argument(
    schema.arg("scope", schema.string_type())
    |> schema.arg_description("Cache scope: 'PUBLIC' or 'PRIVATE'"),
  )
}

/// @log directive - Repeatable logging directive
fn log_directive() -> schema.DirectiveDefinition {
  schema.directive("log", [schema.FieldLocation])
  |> schema.directive_description("Logs field access for debugging")
  |> schema.directive_argument(
    schema.arg("level", schema.string_type())
    |> schema.arg_description("Log level: 'DEBUG', 'INFO', 'WARN', 'ERROR'"),
  )
  |> schema.directive_repeatable
}

/// @deprecated - Built-in deprecated directive (for reference)
fn custom_deprecated_directive() -> schema.DirectiveDefinition {
  schema.directive("deprecated", [
    schema.FieldDefinitionLocation,
    schema.EnumValueLocation,
  ])
  |> schema.directive_description(
    "Marks an element of a GraphQL schema as no longer supported",
  )
  |> schema.directive_argument(
    schema.arg("reason", schema.string_type())
    |> schema.arg_description("Explains why this element was deprecated"),
  )
}

/// @uppercase directive - Transform directive with handler
fn uppercase_directive() -> schema.DirectiveDefinition {
  schema.directive("uppercase", [schema.FieldLocation])
  |> schema.directive_description("Transforms the field value to uppercase")
  |> schema.directive_handler(fn(_args, value) {
    // In a real implementation, this would transform string values to uppercase
    Ok(value)
  })
}

/// @format directive - Date formatting directive
fn format_directive() -> schema.DirectiveDefinition {
  schema.directive("format", [schema.FieldLocation])
  |> schema.directive_description("Formats date/time values")
  |> schema.directive_argument(
    schema.arg("pattern", schema.non_null(schema.string_type()))
    |> schema.arg_description("Format pattern (e.g., 'YYYY-MM-DD')"),
  )
  |> schema.directive_handler(fn(_args, value) {
    // In a real implementation, this would format date values
    Ok(value)
  })
}

// ============================================================================
// Build Schema with Directives
// ============================================================================

pub fn create_schema() -> schema.Schema {
  // Build base schema
  let base_schema =
    query.new()
    |> query.add_query(users_query())
    |> query.add_type(user_type())
    |> query.build

  // Add custom directives
  base_schema
  |> schema.add_directive(auth_directive())
  |> schema.add_directive(has_role_directive())
  |> schema.add_directive(rate_limit_directive())
  |> schema.add_directive(cache_directive())
  |> schema.add_directive(log_directive())
  |> schema.add_directive(uppercase_directive())
  |> schema.add_directive(format_directive())
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  io.println("Custom Directives Example")
  io.println("=========================")
  io.println("")

  let my_schema = create_schema()
  io.println("Schema with custom directives created!")
  io.println("")

  io.println("Defined Custom Directives:")
  io.println("  - @auth - Requires authentication")
  io.println("  - @hasRole(role: String!) - Requires specific role")
  io.println("  - @rateLimit(limit: Int!, duration: Int!) - Rate limiting")
  io.println("  - @cache(maxAge: Int, scope: String) - Caching configuration")
  io.println(
    "  - @log(level: String) repeatable - Logging (can be applied multiple times)",
  )
  io.println("  - @uppercase - Transform to uppercase")
  io.println("  - @format(pattern: String!) - Format date/time values")
  io.println("")

  io.println("Example Usage in Schema:")
  io.println("------------------------")
  io.println("type Query {")
  io.println("  users: [User] @auth @rateLimit(limit: 100, duration: 60)")
  io.println("  adminUsers: [User] @hasRole(role: \"ADMIN\")")
  io.println("}")
  io.println("")
  io.println("type User @cache(maxAge: 3600, scope: \"PUBLIC\") {")
  io.println("  id: ID!")
  io.println("  name: String @uppercase")
  io.println("  email: String @auth")
  io.println("  createdAt: String @format(pattern: \"YYYY-MM-DD\")")
  io.println("}")
  io.println("")

  // Generate SDL
  io.println("Generated SDL with Directive Definitions:")
  io.println("------------------------------------------")
  let sdl_output = sdl.generate(my_schema)
  io.println(sdl_output)
}
