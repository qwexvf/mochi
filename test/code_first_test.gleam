// Tests for mochi Code First API
// Tests query.gleam and types.gleam functionality

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
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
