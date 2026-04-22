// Tests for schema splitting and merging via SchemaBuilder

import gleam/option.{None, Some}
import gleam/result
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Shared Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

fn user_type() {
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.build(fn(_) { Ok(User("1", "Alice", "alice@example.com")) })
}

fn sample_users() {
  [
    User("1", "Alice", "alice@example.com"),
    User("2", "Bob", "bob@example.com"),
  ]
}

// ============================================================================
// Split Schema: User Queries
// ============================================================================

fn user_queries_schema() -> query.SchemaBuilder {
  let users_query =
    query.query("users", schema.list_type(schema.named_type("User")), fn(_ctx) {
      Ok(sample_users())
    })

  let user_query =
    query.query_with_args(
      name: "user",
      args: [query.arg("id", schema.non_null(schema.id_type()))],
      returns: schema.named_type("User"),
      resolve: fn(args, _ctx) {
        use _id <- result.try(query.get_id(args, "id"))
        Ok(User("1", "Alice", "alice@example.com"))
      },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_query(user_query)
  |> query.add_type(user_type())
}

// ============================================================================
// Split Schema: User Mutations
// ============================================================================

fn user_mutations_schema() -> query.SchemaBuilder {
  let create_user =
    query.mutation_with_args(
      name: "createUser",
      args: [
        query.arg("name", schema.non_null(schema.string_type())),
        query.arg("email", schema.non_null(schema.string_type())),
      ],
      returns: schema.named_type("User"),
      resolve: fn(args, _ctx) {
        use name <- result.try(query.get_string(args, "name"))
        use email <- result.try(query.get_string(args, "email"))
        Ok(User("3", name, email))
      },
    )

  let update_user =
    query.mutation_with_args(
      name: "updateUser",
      args: [
        query.arg("id", schema.non_null(schema.id_type())),
        query.arg("name", schema.non_null(schema.string_type())),
      ],
      returns: schema.named_type("User"),
      resolve: fn(args, _ctx) {
        use id <- result.try(query.get_id(args, "id"))
        use name <- result.try(query.get_string(args, "name"))
        Ok(User(id, name, "updated@example.com"))
      },
    )

  query.new()
  |> query.add_mutations([create_user, update_user])
}

// ============================================================================
// Tests
// ============================================================================

pub fn merge_query_and_mutation_schemas_test() {
  let schema =
    user_queries_schema()
    |> query.merge(user_mutations_schema())
    |> query.build

  // Execute a query from the queries schema
  let result = executor.execute_query(schema, "{ users { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "Query 'users' should resolve without errors"
  }

  case result.data {
    Some(_) -> Nil
    None -> panic as "Query 'users' should return data"
  }
}

pub fn merge_schema_query_with_args_test() {
  let schema =
    user_queries_schema()
    |> query.merge(user_mutations_schema())
    |> query.build

  let result =
    executor.execute_query(schema, "{ user(id: \"1\") { id name email } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "Query 'user(id:)' should resolve without errors"
  }

  case result.data {
    Some(_) -> Nil
    None -> panic as "Query 'user(id:)' should return data"
  }
}

pub fn merge_schema_mutation_test() {
  let schema =
    user_queries_schema()
    |> query.merge(user_mutations_schema())
    |> query.build

  let result =
    executor.execute_query(
      schema,
      "mutation { createUser(name: \"Charlie\", email: \"charlie@example.com\") { id name } }",
    )

  case result.errors {
    [] -> Nil
    _ -> panic as "Mutation 'createUser' should resolve without errors"
  }

  case result.data {
    Some(_) -> Nil
    None -> panic as "Mutation 'createUser' should return data"
  }
}

pub fn merge_schema_update_mutation_test() {
  let schema =
    user_queries_schema()
    |> query.merge(user_mutations_schema())
    |> query.build

  let result =
    executor.execute_query(
      schema,
      "mutation { updateUser(id: \"1\", name: \"Updated\") { id name } }",
    )

  case result.errors {
    [] -> Nil
    _ -> panic as "Mutation 'updateUser' should resolve without errors"
  }

  case result.data {
    Some(_) -> Nil
    None -> panic as "Mutation 'updateUser' should return data"
  }
}

pub fn merge_preserves_all_queries_and_mutations_test() {
  let schema =
    user_queries_schema()
    |> query.merge(user_mutations_schema())
    |> query.build

  // Both queries should work
  let r1 = executor.execute_query(schema, "{ users { id } }")
  let r2 = executor.execute_query(schema, "{ user(id: \"1\") { id } }")

  case r1.errors, r2.errors {
    [], [] -> Nil
    _, _ -> panic as "Both queries should be available after merge"
  }

  // Both mutations should work
  let r3 =
    executor.execute_query(
      schema,
      "mutation { createUser(name: \"A\", email: \"a@b.com\") { id } }",
    )
  let r4 =
    executor.execute_query(
      schema,
      "mutation { updateUser(id: \"1\", name: \"B\") { id } }",
    )

  case r3.errors, r4.errors {
    [], [] -> Nil
    _, _ -> panic as "Both mutations should be available after merge"
  }
}

pub fn merge_empty_builder_is_identity_test() {
  let schema =
    user_queries_schema()
    |> query.merge(query.new())
    |> query.build

  let result = executor.execute_query(schema, "{ users { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "Merging with empty builder should not break anything"
  }
}

pub fn merge_multiple_schemas_test() {
  // Simulate 3 domain modules being merged
  let queries = user_queries_schema()
  let mutations = user_mutations_schema()

  // A third "domain" with just an extra query
  let extra =
    query.new()
    |> query.add_query(
      query.query("userCount", schema.int_type(), fn(_ctx) { Ok(42) }),
    )

  let schema =
    queries
    |> query.merge(mutations)
    |> query.merge(extra)
    |> query.build

  let r1 = executor.execute_query(schema, "{ users { id } }")
  let r2 = executor.execute_query(schema, "{ userCount }")
  let r3 =
    executor.execute_query(
      schema,
      "mutation { createUser(name: \"X\", email: \"x@y.com\") { id } }",
    )

  case r1.errors, r2.errors, r3.errors {
    [], [], [] -> Nil
    _, _, _ -> panic as "All three merged domains should work"
  }
}
