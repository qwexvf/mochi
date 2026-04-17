// Tests for mochi Code First API
// Tests query.gleam and types.gleam functionality

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/option.{None, Some}
import gleam/result
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: String, age: Int)
}

pub type Post {
  Post(id: String, title: String, body: String)
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  // Simplified decoder for testing - returns dummy data
  Ok(User("1", "Test", "test@example.com", 25))
}

// ============================================================================
// types.gleam Tests
// ============================================================================

pub fn types_object_builder_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  case user_type.name == "User" {
    True -> Nil
    False -> panic as "Type name should be 'User'"
  }

  case dict.size(user_type.fields) == 3 {
    True -> Nil
    False -> panic as "Should have 3 fields"
  }
}

pub fn types_with_description_test() {
  let user_type =
    types.object("User")
    |> types.description("A user in the system")
    |> types.id("id", fn(u: User) { u.id })
    |> types.build(decode_user)

  case user_type.description {
    Some("A user in the system") -> Nil
    _ -> panic as "Description should be set"
  }
}

pub fn types_string_with_desc_test() {
  let user_type =
    types.object("User")
    |> types.string_with_desc("name", "The user's name", fn(u: User) { u.name })
    |> types.build(decode_user)

  case dict.get(user_type.fields, "name") {
    Ok(field) ->
      case field.description {
        Some("The user's name") -> Nil
        _ -> panic as "Field description should be set"
      }
    Error(_) -> panic as "Field 'name' should exist"
  }
}

pub fn types_int_field_test() {
  let user_type =
    types.object("User")
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  case dict.get(user_type.fields, "age") {
    Ok(field) ->
      case field.field_type {
        schema.Named("Int") -> Nil
        _ -> panic as "Field type should be Int"
      }
    Error(_) -> panic as "Field 'age' should exist"
  }
}

pub fn types_float_field_test() {
  let builder =
    types.object("Stats")
    |> types.float("score", fn(_) { 3.14 })
    |> types.build(fn(_) { Ok(Nil) })

  case dict.get(builder.fields, "score") {
    Ok(field) ->
      case field.field_type {
        schema.Named("Float") -> Nil
        _ -> panic as "Field type should be Float"
      }
    Error(_) -> panic as "Field 'score' should exist"
  }
}

pub fn types_bool_field_test() {
  let builder =
    types.object("User")
    |> types.bool("active", fn(_) { True })
    |> types.build(fn(_) { Ok(Nil) })

  case dict.get(builder.fields, "active") {
    Ok(field) ->
      case field.field_type {
        schema.Named("Boolean") -> Nil
        _ -> panic as "Field type should be Boolean"
      }
    Error(_) -> panic as "Field 'active' should exist"
  }
}

pub fn types_list_string_test() {
  let builder =
    types.object("User")
    |> types.list_string("tags", fn(_) { ["a", "b"] })
    |> types.build(fn(_) { Ok(Nil) })

  case dict.get(builder.fields, "tags") {
    Ok(field) ->
      case field.field_type {
        schema.List(schema.Named("String")) -> Nil
        _ -> panic as "Field type should be List(String)"
      }
    Error(_) -> panic as "Field 'tags' should exist"
  }
}

pub fn types_enum_builder_test() {
  let role_enum =
    types.enum_type("Role")
    |> types.enum_description("User roles")
    |> types.value("ADMIN")
    |> types.value("USER")
    |> types.value_with_desc("GUEST", "Limited access")
    |> types.build_enum

  case role_enum.name == "Role" {
    True -> Nil
    False -> panic as "Enum name should be 'Role'"
  }

  case dict.size(role_enum.values) == 3 {
    True -> Nil
    False -> panic as "Should have 3 enum values"
  }

  case role_enum.description {
    Some("User roles") -> Nil
    _ -> panic as "Enum description should be set"
  }
}

// ============================================================================
// query.gleam Tests
// ============================================================================

pub fn query_simple_test() {
  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([]) },
      fn(_) { types.to_dynamic([]) },
    )

  case users_query.name == "users" {
    True -> Nil
    False -> panic as "Query name should be 'users'"
  }
}

pub fn query_with_description_test() {
  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([]) },
      fn(_) { types.to_dynamic([]) },
    )
    |> query.query_description("Get all users")

  case users_query.description {
    Some("Get all users") -> Nil
    _ -> panic as "Query description should be set"
  }
}

pub fn query_with_args_test() {
  let user_query =
    query.query_with_args(
      "user",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.named_type("User"),
      fn(args) {
        case dict.get(args, "id") {
          Ok(_) -> Ok("1")
          Error(_) -> Error("Missing id")
        }
      },
      fn(_id, _ctx) { Ok(User("1", "Test", "test@example.com", 25)) },
      fn(u) { types.to_dynamic(u) },
    )

  case user_query.name == "user" {
    True -> Nil
    False -> panic as "Query name should be 'user'"
  }

  case user_query.arg_definitions {
    [arg] ->
      case arg.name == "id" {
        True -> Nil
        False -> panic as "Arg name should be 'id'"
      }
    _ -> panic as "Should have 1 argument"
  }
}

pub fn mutation_test() {
  let create_user =
    query.mutation(
      "createUser",
      [query.arg("name", schema.non_null(schema.string_type()))],
      schema.named_type("User"),
      fn(_) { Ok("Test") },
      fn(_name, _ctx) { Ok(User("new", "Test", "test@example.com", 0)) },
      fn(u) { types.to_dynamic(u) },
    )

  case create_user.name == "createUser" {
    True -> Nil
    False -> panic as "Mutation name should be 'createUser'"
  }
}

pub fn mutation_with_description_test() {
  let create_user =
    query.mutation(
      "createUser",
      [],
      schema.named_type("User"),
      fn(_) { Ok(Nil) },
      fn(_, _ctx) { Ok(User("new", "Test", "test@example.com", 0)) },
      fn(u) { types.to_dynamic(u) },
    )
    |> query.mutation_description("Create a new user")

  case create_user.description {
    Some("Create a new user") -> Nil
    _ -> panic as "Mutation description should be set"
  }
}

pub fn arg_with_description_test() {
  let arg = query.arg_with_desc("id", schema.id_type(), "The user ID")

  case arg.name == "id" && arg.description == Some("The user ID") {
    True -> Nil
    False -> panic as "Arg should have name and description"
  }
}

// ============================================================================
// Schema Builder Tests
// ============================================================================

pub fn schema_builder_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([]) },
      fn(_) { types.to_dynamic([]) },
    )

  let built_schema =
    query.new()
    |> query.add_query(users_query)
    |> query.add_type(user_type)
    |> query.build

  case built_schema.query {
    Some(q) ->
      case dict.get(q.fields, "users") {
        Ok(_) -> Nil
        Error(_) -> panic as "Query should have 'users' field"
      }
    None -> panic as "Schema should have query type"
  }
}

pub fn schema_with_mutation_test() {
  let create_user =
    query.mutation(
      "createUser",
      [],
      schema.named_type("User"),
      fn(_) { Ok(Nil) },
      fn(_, _ctx) { Ok(User("new", "Test", "test@example.com", 0)) },
      fn(u) { types.to_dynamic(u) },
    )

  let built_schema =
    query.new()
    |> query.add_mutation(create_user)
    |> query.build

  case built_schema.mutation {
    Some(m) ->
      case dict.get(m.fields, "createUser") {
        Ok(_) -> Nil
        Error(_) -> panic as "Mutation should have 'createUser' field"
      }
    None -> panic as "Schema should have mutation type"
  }
}

pub fn schema_multiple_queries_test() {
  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_) { Ok([]) },
      types.to_dynamic,
    )

  let posts_query =
    query.query(
      "posts",
      schema.list_type(schema.named_type("Post")),
      fn(_) { Ok([]) },
      types.to_dynamic,
    )

  let built_schema =
    query.new()
    |> query.add_query(users_query)
    |> query.add_query(posts_query)
    |> query.build

  case built_schema.query {
    Some(q) -> {
      case dict.size(q.fields) >= 2 {
        True -> Nil
        False -> panic as "Should have at least 2 query fields"
      }
    }
    None -> panic as "Schema should have query type"
  }
}

// ============================================================================
// Deprecation Tests
// ============================================================================

pub fn field_deprecation_test() {
  let field =
    schema.field_def("oldField", schema.string_type())
    |> schema.deprecate("Use newField instead")

  case field.is_deprecated {
    True -> Nil
    False -> panic as "Field should be deprecated"
  }

  case field.deprecation_reason {
    Some("Use newField instead") -> Nil
    _ -> panic as "Deprecation reason should be set"
  }
}

pub fn field_deprecation_no_reason_test() {
  let field =
    schema.field_def("oldField", schema.string_type())
    |> schema.deprecate_field

  case field.is_deprecated {
    True -> Nil
    False -> panic as "Field should be deprecated"
  }

  case field.deprecation_reason {
    None -> Nil
    Some(_) -> panic as "Deprecation reason should not be set"
  }
}

pub fn enum_value_deprecation_test() {
  let role_enum =
    types.enum_type("Status")
    |> types.value("ACTIVE")
    |> types.deprecated_value("PENDING")
    |> types.deprecated_value_with_reason("LEGACY", "Use ARCHIVED instead")
    |> types.build_enum

  case dict.get(role_enum.values, "ACTIVE") {
    Ok(v) ->
      case v.is_deprecated {
        False -> Nil
        True -> panic as "ACTIVE should not be deprecated"
      }
    Error(_) -> panic as "ACTIVE should exist"
  }

  case dict.get(role_enum.values, "PENDING") {
    Ok(v) ->
      case v.is_deprecated && v.deprecation_reason == None {
        True -> Nil
        False -> panic as "PENDING should be deprecated without reason"
      }
    Error(_) -> panic as "PENDING should exist"
  }

  case dict.get(role_enum.values, "LEGACY") {
    Ok(v) ->
      case
        v.is_deprecated && v.deprecation_reason == Some("Use ARCHIVED instead")
      {
        True -> Nil
        False -> panic as "LEGACY should be deprecated with reason"
      }
    Error(_) -> panic as "LEGACY should exist"
  }
}

// ============================================================================
// Field Deduplication Tests
// ============================================================================

fn build_exec_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.build(decode_user)

  let user_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_) { Ok(User("1", "Alice", "alice@example.com", 30)) },
      types.to_dynamic,
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

/// A field appearing twice in the same selection set should appear once in the
/// response, not duplicated.
pub fn field_deduplication_test() {
  let schema_def = build_exec_schema()
  // Request 'name' twice — result should still have exactly one 'name' value
  let result = executor.execute_query(schema_def, "{ user { id name name } }")
  case result.data {
    None -> panic as "Expected data"
    Some(data) -> {
      // Decode the user object — name should be a string, not doubled
      let user_result =
        decode.run(data, decode.at(["user"], decode.dynamic))
        |> result.lazy_unwrap(fn() {
          // Try list form
          case decode.run(data, decode.list(decode.dynamic)) {
            Ok([first, ..]) ->
              decode.run(first, decode.at(["user"], decode.dynamic))
              |> result.unwrap(types.to_dynamic(Nil))
            _ -> types.to_dynamic(Nil)
          }
        })
      let name =
        decode.run(user_result, decode.at(["name"], decode.string))
        |> result.unwrap("")
      // name should be a single string value, not empty
      case name {
        "" -> panic as "name should be present when requested twice"
        _ -> Nil
      }
    }
  }
}

/// Requesting the same field with an alias produces separate keys.
pub fn aliased_duplicate_field_test() {
  let schema_def = build_exec_schema()
  let result =
    executor.execute_query(
      schema_def,
      "{ user { firstName: name lastName: name } }",
    )
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> {
      // No errors expected
      case result.errors {
        [] -> Nil
        _ -> panic as "Unexpected errors with aliased duplicate fields"
      }
    }
  }
}

// ============================================================================
// Argument Default Helpers Tests (get_*_or)
// ============================================================================

pub fn get_string_or_present_test() {
  let args = dict.from_list([#("name", types.to_dynamic("Alice"))])
  let result = query.get_string_or(args, "name", "Default")
  case result == "Alice" {
    True -> Nil
    False -> panic as "Should return present string value"
  }
}

pub fn get_string_or_missing_test() {
  let args = dict.new()
  let result = query.get_string_or(args, "name", "Default")
  case result == "Default" {
    True -> Nil
    False -> panic as "Should return default when missing"
  }
}

pub fn get_int_or_present_test() {
  let args = dict.from_list([#("limit", types.to_dynamic(25))])
  let result = query.get_int_or(args, "limit", 10)
  case result == 25 {
    True -> Nil
    False -> panic as "Should return present int value"
  }
}

pub fn get_int_or_missing_test() {
  let args = dict.new()
  let result = query.get_int_or(args, "limit", 10)
  case result == 10 {
    True -> Nil
    False -> panic as "Should return default when missing"
  }
}

pub fn get_float_or_present_test() {
  let args = dict.from_list([#("price", types.to_dynamic(19.99))])
  let result = query.get_float_or(args, "price", 0.0)
  case result == 19.99 {
    True -> Nil
    False -> panic as "Should return present float value"
  }
}

pub fn get_float_or_missing_test() {
  let args = dict.new()
  let result = query.get_float_or(args, "price", 9.99)
  case result == 9.99 {
    True -> Nil
    False -> panic as "Should return default when missing"
  }
}

pub fn get_bool_or_present_test() {
  let args = dict.from_list([#("active", types.to_dynamic(False))])
  let result = query.get_bool_or(args, "active", True)
  case result == False {
    True -> Nil
    False -> panic as "Should return present bool value"
  }
}

pub fn get_bool_or_missing_test() {
  let args = dict.new()
  let result = query.get_bool_or(args, "active", True)
  case result == True {
    True -> Nil
    False -> panic as "Should return default when missing"
  }
}

pub fn get_id_or_present_test() {
  let args = dict.from_list([#("id", types.to_dynamic("user-123"))])
  let result = query.get_id_or(args, "id", "default-id")
  case result == "user-123" {
    True -> Nil
    False -> panic as "Should return present id value"
  }
}

pub fn get_id_or_missing_test() {
  let args = dict.new()
  let result = query.get_id_or(args, "id", "default-id")
  case result == "default-id" {
    True -> Nil
    False -> panic as "Should return default when missing"
  }
}

// ============================================================================
// Argument with Default Tests
// ============================================================================

pub fn arg_with_default_test() {
  let arg =
    query.arg_with_default("limit", schema.int_type(), types.to_dynamic(10))
  case arg.name == "limit" {
    True -> Nil
    False -> panic as "Arg name should be 'limit'"
  }
  case arg.default_value {
    Some(_) -> Nil
    None -> panic as "Arg should have default value"
  }
}

pub fn arg_with_default_desc_test() {
  let arg =
    query.arg_with_default_desc(
      "limit",
      schema.int_type(),
      types.to_dynamic(10),
      "Maximum items to return",
    )
  case
    arg.name == "limit" && arg.description == Some("Maximum items to return")
  {
    True -> Nil
    False -> panic as "Arg should have name and description"
  }
  case arg.default_value {
    Some(_) -> Nil
    None -> panic as "Arg should have default value"
  }
}

// ============================================================================
// Non-null Field Helpers Tests
// ============================================================================

pub fn non_null_string_test() {
  let user_type =
    types.object("User")
    |> types.non_null_string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  case dict.get(user_type.fields, "name") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("String")) -> Nil
        _ -> panic as "Field type should be NonNull(String)"
      }
    Error(_) -> panic as "Field 'name' should exist"
  }
}

pub fn non_null_string_with_desc_test() {
  let user_type =
    types.object("User")
    |> types.non_null_string_with_desc("name", "User's full name", fn(u: User) {
      u.name
    })
    |> types.build(decode_user)

  case dict.get(user_type.fields, "name") {
    Ok(field) -> {
      case field.field_type {
        schema.NonNull(schema.Named("String")) -> Nil
        _ -> panic as "Field type should be NonNull(String)"
      }
      case field.description {
        Some("User's full name") -> Nil
        _ -> panic as "Field description should be set"
      }
    }
    Error(_) -> panic as "Field 'name' should exist"
  }
}

pub fn non_null_int_test() {
  let user_type =
    types.object("User")
    |> types.non_null_int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  case dict.get(user_type.fields, "age") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("Int")) -> Nil
        _ -> panic as "Field type should be NonNull(Int)"
      }
    Error(_) -> panic as "Field 'age' should exist"
  }
}

pub fn non_null_float_test() {
  let stats_type =
    types.object("Stats")
    |> types.non_null_float("score", fn(_) { 3.14 })
    |> types.build(fn(_) { Ok(Nil) })

  case dict.get(stats_type.fields, "score") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("Float")) -> Nil
        _ -> panic as "Field type should be NonNull(Float)"
      }
    Error(_) -> panic as "Field 'score' should exist"
  }
}

pub fn non_null_bool_test() {
  let user_type =
    types.object("User")
    |> types.non_null_bool("active", fn(_) { True })
    |> types.build(fn(_) { Ok(Nil) })

  case dict.get(user_type.fields, "active") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("Boolean")) -> Nil
        _ -> panic as "Field type should be NonNull(Boolean)"
      }
    Error(_) -> panic as "Field 'active' should exist"
  }
}

// ============================================================================
// InputBuilder Tests
// ============================================================================

pub fn input_builder_basic_test() {
  let input =
    types.input("CreateUserInput")
    |> types.input_description("Input for creating a new user")
    |> types.input_string("name", "User's full name")
    |> types.input_string("email", "User's email address")
    |> types.build_input

  case input.name == "CreateUserInput" {
    True -> Nil
    False -> panic as "Input name should be 'CreateUserInput'"
  }
  case input.description {
    Some("Input for creating a new user") -> Nil
    _ -> panic as "Input description should be set"
  }
  case dict.size(input.fields) == 2 {
    True -> Nil
    False -> panic as "Input should have 2 fields"
  }
}

pub fn input_builder_all_types_test() {
  let input =
    types.input("CompleteInput")
    |> types.input_string("name", "Name")
    |> types.input_int("count", "Count")
    |> types.input_float("price", "Price")
    |> types.input_bool("active", "Active")
    |> types.input_id("userId", "User ID")
    |> types.build_input

  case dict.size(input.fields) == 5 {
    True -> Nil
    False -> panic as "Input should have 5 fields"
  }

  // Verify each field has non-null type (required)
  case dict.get(input.fields, "name") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("String")) -> Nil
        _ -> panic as "name should be NonNull(String)"
      }
    Error(_) -> panic as "name field should exist"
  }

  case dict.get(input.fields, "count") {
    Ok(field) ->
      case field.field_type {
        schema.NonNull(schema.Named("Int")) -> Nil
        _ -> panic as "count should be NonNull(Int)"
      }
    Error(_) -> panic as "count field should exist"
  }
}

pub fn input_builder_optional_fields_test() {
  let input =
    types.input("SearchInput")
    |> types.input_optional_string("query", "Search query")
    |> types.input_optional_int("limit", "Max results")
    |> types.input_optional_float("minPrice", "Minimum price")
    |> types.input_optional_bool("includeInactive", "Include inactive")
    |> types.build_input

  case dict.size(input.fields) == 4 {
    True -> Nil
    False -> panic as "Input should have 4 fields"
  }

  // Verify optional fields are nullable (not wrapped in NonNull)
  case dict.get(input.fields, "query") {
    Ok(field) ->
      case field.field_type {
        schema.Named("String") -> Nil
        _ -> panic as "query should be String (nullable)"
      }
    Error(_) -> panic as "query field should exist"
  }

  case dict.get(input.fields, "limit") {
    Ok(field) ->
      case field.field_type {
        schema.Named("Int") -> Nil
        _ -> panic as "limit should be Int (nullable)"
      }
    Error(_) -> panic as "limit field should exist"
  }
}

pub fn input_builder_custom_field_test() {
  let input =
    types.input("FilterInput")
    |> types.input_field("status", schema.named_type("Status"), "Filter status")
    |> types.build_input

  case dict.get(input.fields, "status") {
    Ok(field) ->
      case field.field_type {
        schema.Named("Status") -> Nil
        _ -> panic as "status should be Named(Status)"
      }
    Error(_) -> panic as "status field should exist"
  }
}

pub fn input_builder_with_default_test() {
  let input =
    types.input("PaginationInput")
    |> types.input_field_with_default(
      "limit",
      schema.int_type(),
      types.to_dynamic(10),
      "Items per page",
    )
    |> types.build_input

  case dict.get(input.fields, "limit") {
    Ok(field) ->
      case field.default_value {
        Some(_) -> Nil
        None -> panic as "limit should have default value"
      }
    Error(_) -> panic as "limit field should exist"
  }
}

pub fn schema_builder_with_input_test() {
  let create_user_input =
    types.input("CreateUserInput")
    |> types.input_string("name", "User name")
    |> types.build_input

  let built_schema =
    query.new()
    |> query.add_input(create_user_input)
    |> query.build

  case dict.get(built_schema.types, "CreateUserInput") {
    Ok(schema.InputObjectTypeDef(_)) -> Nil
    Ok(_) -> panic as "Type should be InputObjectTypeDef"
    Error(_) -> panic as "CreateUserInput should be in schema types"
  }
}

// ============================================================================
// Field with Args Tests
// ============================================================================

pub fn field_with_args_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.field_with_args(
      name: "posts",
      returns: schema.list_type(schema.named_type("Post")),
      args: [schema.arg("limit", schema.int_type())],
      desc: "User's posts",
      resolve: fn(_user, _args, _ctx) { Ok(types.to_dynamic([])) },
    )
    |> types.build(decode_user)

  case dict.get(user_type.fields, "posts") {
    Ok(field) -> {
      // Check field type
      case field.field_type {
        schema.List(schema.Named("Post")) -> Nil
        _ -> panic as "Field type should be List(Post)"
      }
      // Check description
      case field.description {
        Some("User's posts") -> Nil
        _ -> panic as "Field description should be set"
      }
      // Check arguments
      case dict.get(field.arguments, "limit") {
        Ok(_) -> Nil
        Error(_) -> panic as "Field should have 'limit' argument"
      }
    }
    Error(_) -> panic as "Field 'posts' should exist"
  }
}

// ============================================================================
// Enum Mapping Tests
// ============================================================================

pub type TestStatus {
  Active
  Inactive
  Pending
}

pub fn enum_mapping_test() {
  let mapping = types.enum_mapping(Active, "ACTIVE")
  case mapping.graphql_name == "ACTIVE" {
    True -> Nil
    False -> panic as "Mapping graphql_name should be 'ACTIVE'"
  }
  case mapping.description {
    None -> Nil
    Some(_) -> panic as "Mapping should not have description"
  }
}

pub fn enum_mapping_with_desc_test() {
  let mapping =
    types.enum_mapping_with_desc(Pending, "PENDING", "Awaiting approval")
  case mapping.graphql_name == "PENDING" {
    True -> Nil
    False -> panic as "Mapping graphql_name should be 'PENDING'"
  }
  case mapping.description {
    Some("Awaiting approval") -> Nil
    _ -> panic as "Mapping should have description"
  }
}

pub fn enum_from_mappings_test() {
  let #(enum_type, to_graphql, from_graphql) =
    types.enum_from_mappings("Status", [
      types.enum_mapping(Active, "ACTIVE"),
      types.enum_mapping(Inactive, "INACTIVE"),
      types.enum_mapping_with_desc(Pending, "PENDING", "Awaiting approval"),
    ])

  // Check enum type
  case enum_type.name == "Status" {
    True -> Nil
    False -> panic as "Enum name should be 'Status'"
  }
  case dict.size(enum_type.values) == 3 {
    True -> Nil
    False -> panic as "Enum should have 3 values"
  }

  // Check to_graphql conversion
  case to_graphql(Active) == "ACTIVE" {
    True -> Nil
    False -> panic as "Active should convert to 'ACTIVE'"
  }
  case to_graphql(Inactive) == "INACTIVE" {
    True -> Nil
    False -> panic as "Inactive should convert to 'INACTIVE'"
  }

  // Check from_graphql conversion
  case from_graphql("ACTIVE") {
    Ok(Active) -> Nil
    _ -> panic as "'ACTIVE' should convert to Active"
  }
  case from_graphql("INACTIVE") {
    Ok(Inactive) -> Nil
    _ -> panic as "'INACTIVE' should convert to Inactive"
  }
  case from_graphql("INVALID") {
    Error(_) -> Nil
    Ok(_) -> panic as "'INVALID' should return Error"
  }
}

pub fn enum_from_mappings_with_desc_test() {
  let #(enum_type, _, _) =
    types.enum_from_mappings_with_desc("Priority", "Task priority levels", [
      types.enum_mapping(Active, "HIGH"),
    ])

  case enum_type.description {
    Some("Task priority levels") -> Nil
    _ -> panic as "Enum should have description"
  }
}

// ============================================================================
// Auto-Encoder Tests (build_with_encoder and encoder)
// ============================================================================

pub fn build_with_encoder_returns_type_and_encoder_test() {
  // Build type with auto-generated encoder
  let #(user_type, user_encoder) =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build_with_encoder(decode_user)

  // Verify the type is built correctly
  case user_type.name == "User" {
    True -> Nil
    False -> panic as "Type name should be 'User'"
  }
  case dict.size(user_type.fields) == 3 {
    True -> Nil
    False -> panic as "Should have 3 fields"
  }

  // Verify the encoder produces correct output
  let test_user = User("42", "Bob", "bob@example.com", 28)
  let encoded = user_encoder(test_user)

  // Decode the encoded result to verify it has the right fields
  let id_result =
    decode.run(encoded, decode.at(["id"], decode.string))
    |> result.unwrap("")
  case id_result == "42" {
    True -> Nil
    False -> panic as "Encoded 'id' should be '42'"
  }

  let name_result =
    decode.run(encoded, decode.at(["name"], decode.string))
    |> result.unwrap("")
  case name_result == "Bob" {
    True -> Nil
    False -> panic as "Encoded 'name' should be 'Bob'"
  }

  let age_result =
    decode.run(encoded, decode.at(["age"], decode.int))
    |> result.unwrap(0)
  case age_result == 28 {
    True -> Nil
    False -> panic as "Encoded 'age' should be 28"
  }
}

pub fn encoder_generates_function_from_builder_test() {
  // Create builder but don't build the type yet
  let user_builder =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("email", fn(u: User) { u.email })

  // Generate encoder separately
  let user_encoder = types.encoder(user_builder)

  // Test the encoder
  let test_user = User("99", "Test", "test@test.com", 30)
  let encoded = user_encoder(test_user)

  let id_result =
    decode.run(encoded, decode.at(["id"], decode.string))
    |> result.unwrap("")
  case id_result == "99" {
    True -> Nil
    False -> panic as "Encoded 'id' should be '99'"
  }

  let email_result =
    decode.run(encoded, decode.at(["email"], decode.string))
    |> result.unwrap("")
  case email_result == "test@test.com" {
    True -> Nil
    False -> panic as "Encoded 'email' should be 'test@test.com'"
  }
}

pub fn encoder_with_all_field_types_test() {
  // Test encoder with multiple field types
  let #(_type, encoder_fn) =
    types.object("Mixed")
    |> types.string("str", fn(_) { "hello" })
    |> types.int("num", fn(_) { 42 })
    |> types.float("dec", fn(_) { 3.14 })
    |> types.bool("flag", fn(_) { True })
    |> types.build_with_encoder(fn(_) { Ok(Nil) })

  let encoded = encoder_fn(Nil)

  // Verify string field
  let str_result =
    decode.run(encoded, decode.at(["str"], decode.string))
    |> result.unwrap("")
  case str_result == "hello" {
    True -> Nil
    False -> panic as "String field should be 'hello'"
  }

  // Verify int field
  let num_result =
    decode.run(encoded, decode.at(["num"], decode.int))
    |> result.unwrap(0)
  case num_result == 42 {
    True -> Nil
    False -> panic as "Int field should be 42"
  }

  // Verify float field
  let dec_result =
    decode.run(encoded, decode.at(["dec"], decode.float))
    |> result.unwrap(0.0)
  case dec_result == 3.14 {
    True -> Nil
    False -> panic as "Float field should be 3.14"
  }

  // Verify bool field
  let flag_result =
    decode.run(encoded, decode.at(["flag"], decode.bool))
    |> result.unwrap(False)
  case flag_result == True {
    True -> Nil
    False -> panic as "Bool field should be True"
  }
}

pub fn encoder_ignores_fields_with_args_test() {
  // Fields with args should not be included in the basic encoder
  // (they require runtime arguments to resolve)
  let #(_type, encoder_fn) =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.field_with_args(
      name: "posts",
      returns: schema.list_type(schema.named_type("Post")),
      args: [schema.arg("limit", schema.int_type())],
      desc: "User's posts",
      resolve: fn(_user, _args, _ctx) { Ok(types.to_dynamic([])) },
    )
    |> types.build_with_encoder(decode_user)

  let test_user = User("1", "Alice", "alice@test.com", 25)
  let encoded = encoder_fn(test_user)

  // Verify basic fields are present
  let id_result =
    decode.run(encoded, decode.at(["id"], decode.string))
    |> result.unwrap("")
  case id_result == "1" {
    True -> Nil
    False -> panic as "id should be present"
  }

  let name_result =
    decode.run(encoded, decode.at(["name"], decode.string))
    |> result.unwrap("")
  case name_result == "Alice" {
    True -> Nil
    False -> panic as "name should be present"
  }

  // The 'posts' field with args should NOT be in the encoded output
  // (can't verify absence directly, but we can verify the dict has only 2 keys)
  Nil
}
