// Tests for custom directive support
// Tests directive definition and SDL generation

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{Some}
import gleam/string
import mochi/codegen/sdl
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type User {
  User(id: String, name: String)
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Test"))
}

// ============================================================================
// Directive Definition Tests
// ============================================================================

pub fn directive_definition_test() {
  let auth_directive =
    schema.directive("auth", [schema.FieldLocation, schema.ObjectLocation])
    |> schema.directive_description("Requires authentication")

  case auth_directive.name == "auth" {
    True -> Nil
    False -> panic as "Directive name should be 'auth'"
  }

  case auth_directive.description {
    Some("Requires authentication") -> Nil
    _ -> panic as "Directive description should be set"
  }
}

pub fn directive_with_arguments_test() {
  let role_directive =
    schema.directive("hasRole", [schema.FieldLocation])
    |> schema.directive_argument(
      schema.arg("role", schema.non_null(schema.string_type()))
      |> schema.arg_description("Required role"),
    )

  case dict.size(role_directive.arguments) == 1 {
    True -> Nil
    False -> panic as "Should have 1 argument"
  }

  case dict.has_key(role_directive.arguments, "role") {
    True -> Nil
    False -> panic as "Should have 'role' argument"
  }
}

pub fn repeatable_directive_test() {
  let log_directive =
    schema.directive("log", [schema.FieldLocation])
    |> schema.directive_repeatable

  case log_directive.is_repeatable {
    True -> Nil
    False -> panic as "Directive should be repeatable"
  }
}

pub fn directive_handler_test() {
  let uppercase_directive =
    schema.directive("uppercase", [schema.FieldLocation])
    |> schema.directive_handler(fn(_args, value) {
      // In a real implementation, this would transform the value
      Ok(value)
    })

  case option.is_some(uppercase_directive.handler) {
    True -> Nil
    False -> panic as "Directive should have handler"
  }
}

// ============================================================================
// Schema with Directives Tests
// ============================================================================

pub fn add_directive_to_schema_test() {
  let auth_directive = schema.directive("auth", [schema.FieldLocation])

  let test_schema =
    schema.schema()
    |> schema.add_directive(auth_directive)

  case dict.has_key(test_schema.directives, "auth") {
    True -> Nil
    False -> panic as "Schema should have 'auth' directive"
  }
}

pub fn multiple_directives_test() {
  let auth_directive = schema.directive("auth", [schema.FieldLocation])
  let log_directive = schema.directive("log", [schema.FieldLocation])
  let cache_directive = schema.directive("cache", [schema.FieldLocation])

  let test_schema =
    schema.schema()
    |> schema.add_directive(auth_directive)
    |> schema.add_directive(log_directive)
    |> schema.add_directive(cache_directive)

  case dict.size(test_schema.directives) == 3 {
    True -> Nil
    False -> panic as "Schema should have 3 directives"
  }
}

// ============================================================================
// Built-in Directive Tests
// ============================================================================

pub fn skip_directive_test() {
  let skip = schema.skip_directive()

  case skip.name == "skip" {
    True -> Nil
    False -> panic as "Should be named 'skip'"
  }

  case dict.has_key(skip.arguments, "if") {
    True -> Nil
    False -> panic as "Should have 'if' argument"
  }
}

pub fn include_directive_test() {
  let include = schema.include_directive()

  case include.name == "include" {
    True -> Nil
    False -> panic as "Should be named 'include'"
  }

  case dict.has_key(include.arguments, "if") {
    True -> Nil
    False -> panic as "Should have 'if' argument"
  }
}

pub fn deprecated_directive_test() {
  let deprecated = schema.deprecated_directive()

  case deprecated.name == "deprecated" {
    True -> Nil
    False -> panic as "Should be named 'deprecated'"
  }

  case dict.has_key(deprecated.arguments, "reason") {
    True -> Nil
    False -> panic as "Should have 'reason' argument"
  }
}

pub fn builtin_directives_test() {
  let builtins = schema.builtin_directives()

  case builtins {
    [_, _, _] -> Nil
    _ -> panic as "Should have 3 built-in directives"
  }
}

// ============================================================================
// SDL Generation Tests
// ============================================================================

pub fn directive_sdl_generation_test() {
  let rate_limit_directive =
    schema.directive("rateLimit", [schema.FieldLocation, schema.ObjectLocation])
    |> schema.directive_description("Rate limiting directive")
    |> schema.directive_argument(schema.arg(
      "limit",
      schema.non_null(schema.int_type()),
    ))
    |> schema.directive_argument(schema.arg(
      "duration",
      schema.non_null(schema.int_type()),
    ))

  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([]) },
      fn(_) { types.to_dynamic([]) },
    )

  let test_schema =
    query.new()
    |> query.add_query(users_query)
    |> query.add_type(user_type)
    |> query.build
    |> schema.add_directive(rate_limit_directive)

  let sdl_code = sdl.generate(test_schema)

  // Should include directive definition
  let has_directive = string.contains(sdl_code, "directive @rateLimit")
  let has_locations = string.contains(sdl_code, "FIELD | OBJECT")

  case has_directive && has_locations {
    True -> Nil
    False -> panic as "SDL should include directive definition with locations"
  }
}

pub fn repeatable_directive_sdl_test() {
  let log_directive =
    schema.directive("log", [schema.FieldLocation])
    |> schema.directive_repeatable

  let test_schema =
    schema.schema()
    |> schema.add_directive(log_directive)

  let sdl_code = sdl.generate(test_schema)

  let has_repeatable = string.contains(sdl_code, "repeatable")

  case has_repeatable {
    True -> Nil
    False -> panic as "SDL should include 'repeatable' keyword"
  }
}

// ============================================================================
// Directive Location Tests
// ============================================================================

pub fn directive_location_to_string_test() {
  case schema.directive_location_to_string(schema.FieldLocation) {
    "FIELD" -> Nil
    _ -> panic as "FieldLocation should be 'FIELD'"
  }

  case schema.directive_location_to_string(schema.QueryLocation) {
    "QUERY" -> Nil
    _ -> panic as "QueryLocation should be 'QUERY'"
  }

  case schema.directive_location_to_string(schema.ObjectLocation) {
    "OBJECT" -> Nil
    _ -> panic as "ObjectLocation should be 'OBJECT'"
  }

  case schema.directive_location_to_string(schema.EnumValueLocation) {
    "ENUM_VALUE" -> Nil
    _ -> panic as "EnumValueLocation should be 'ENUM_VALUE'"
  }
}
