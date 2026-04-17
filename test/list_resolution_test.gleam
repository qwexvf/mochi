// Tests for list field resolution in GraphQL executor
// Tests that queries returning lists of objects properly execute selection sets
// on each item in the list

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type User {
  User(id: String, name: String, age: Int)
}

pub type Post {
  Post(id: String, title: String, author_id: String)
}

pub type Comment {
  Comment(id: String, body: String)
}

pub type UserWithTags {
  UserWithTags(id: String, name: String, tags: List(String))
}

// ============================================================================
// Simple Decoders (following existing test patterns)
// ============================================================================

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Test User", 25))
}

fn decode_post(_dyn: Dynamic) -> Result(Post, String) {
  Ok(Post("1", "Test Post", "1"))
}

fn decode_comment(_dyn: Dynamic) -> Result(Comment, String) {
  Ok(Comment("1", "Test Comment"))
}

fn decode_user_with_tags(_dyn: Dynamic) -> Result(UserWithTags, String) {
  Ok(UserWithTags("1", "Test User", ["tag1", "tag2"]))
}

// ============================================================================
// Test: Query returning list of objects with scalar fields
// ============================================================================

fn build_users_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_info) {
        // Return a list of users as dynamic dicts
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Alice")),
              #("age", types.to_dynamic(30)),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("2")),
              #("name", types.to_dynamic("Bob")),
              #("age", types.to_dynamic(25)),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("3")),
              #("name", types.to_dynamic("Charlie")),
              #("age", types.to_dynamic(35)),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn list_of_objects_with_scalar_fields_test() {
  let schema_def = build_users_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
        age
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  // Should have data and no errors
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn list_with_partial_field_selection_test() {
  let schema_def = build_users_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn list_with_single_field_selection_test() {
  let schema_def = build_users_list_schema()
  let query_str =
    "
    query {
      users {
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Query returning list of objects with nested object fields
// ============================================================================

fn build_posts_with_author_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  let post_type =
    types.object("Post")
    |> types.id("id", fn(p: Post) { p.id })
    |> types.string("title", fn(p: Post) { p.title })
    |> types.object_field("author", "User", fn(_p: Post) {
      types.to_dynamic(
        dict.from_list([
          #("id", types.to_dynamic("1")),
          #("name", types.to_dynamic("Author Name")),
          #("age", types.to_dynamic(30)),
        ]),
      )
    })
    |> types.build(decode_post)

  let posts_query =
    query.query(
      "posts",
      schema.list_type(schema.named_type("Post")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("post1")),
              #("title", types.to_dynamic("First Post")),
              #(
                "author",
                types.to_dynamic(
                  dict.from_list([
                    #("id", types.to_dynamic("1")),
                    #("name", types.to_dynamic("Alice")),
                    #("age", types.to_dynamic(30)),
                  ]),
                ),
              ),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("post2")),
              #("title", types.to_dynamic("Second Post")),
              #(
                "author",
                types.to_dynamic(
                  dict.from_list([
                    #("id", types.to_dynamic("2")),
                    #("name", types.to_dynamic("Bob")),
                    #("age", types.to_dynamic(25)),
                  ]),
                ),
              ),
            ]),
          ]),
        )
      },
      fn(p) { types.to_dynamic(p) },
    )

  query.new()
  |> query.add_query(posts_query)
  |> query.add_type(user_type)
  |> query.add_type(post_type)
  |> query.build
}

pub fn list_of_objects_with_nested_object_fields_test() {
  let schema_def = build_posts_with_author_schema()
  let query_str =
    "
    query {
      posts {
        id
        title
        author {
          id
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn list_of_objects_nested_with_all_fields_test() {
  let schema_def = build_posts_with_author_schema()
  let query_str =
    "
    query {
      posts {
        id
        title
        author {
          id
          name
          age
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Query with arguments on list query
// ============================================================================

fn build_users_with_filter_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  let users_query =
    query.query_with_args(
      "users",
      [query.arg("minAge", schema.int_type())],
      schema.list_type(schema.named_type("User")),
      fn(_args) {
        // Just return a dummy min age for the test
        Ok(0)
      },
      fn(_min_age, _ctx) {
        // Return users regardless of filter for simplicity
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Alice")),
              #("age", types.to_dynamic(30)),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("2")),
              #("name", types.to_dynamic("Bob")),
              #("age", types.to_dynamic(25)),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn list_with_arguments_test() {
  let schema_def = build_users_with_filter_schema()
  let query_str =
    "
    query {
      users(minAge: 28) {
        id
        name
        age
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn list_with_default_arguments_test() {
  let schema_def = build_users_with_filter_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Empty list handling
// ============================================================================

fn build_empty_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_info) {
        // Return an empty list
        Ok(types.to_dynamic([]))
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn empty_list_handling_test() {
  let schema_def = build_empty_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Non-null list type [User!]!
// ============================================================================

fn build_non_null_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.non_null(
        schema.list_type(schema.non_null(schema.named_type("User"))),
      ),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Alice")),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn non_null_list_type_test() {
  let schema_def = build_non_null_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Multiple list fields in same query
// ============================================================================

fn build_multi_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let comment_type =
    types.object("Comment")
    |> types.id("id", fn(c: Comment) { c.id })
    |> types.string("body", fn(c: Comment) { c.body })
    |> types.build(decode_comment)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Alice")),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  let comments_query =
    query.query(
      "comments",
      schema.list_type(schema.named_type("Comment")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("c1")),
              #("body", types.to_dynamic("Great post!")),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("c2")),
              #("body", types.to_dynamic("Thanks for sharing")),
            ]),
          ]),
        )
      },
      fn(c) { types.to_dynamic(c) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_query(comments_query)
  |> query.add_type(user_type)
  |> query.add_type(comment_type)
  |> query.build
}

pub fn multiple_list_fields_in_query_test() {
  let schema_def = build_multi_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
      comments {
        id
        body
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Aliased list field
// ============================================================================

pub fn aliased_list_field_test() {
  let schema_def = build_users_list_schema()
  let query_str =
    "
    query {
      allUsers: users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: List with __typename
// ============================================================================

pub fn list_with_typename_test() {
  let schema_def = build_users_list_schema()
  let query_str =
    "
    query {
      users {
        __typename
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: List of objects containing scalar list fields
// ============================================================================

fn build_user_with_tags_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: UserWithTags) { u.id })
    |> types.string("name", fn(u: UserWithTags) { u.name })
    |> types.list_string("tags", fn(u: UserWithTags) { u.tags })
    |> types.build(decode_user_with_tags)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Alice")),
              #("tags", types.to_dynamic(["admin", "developer"])),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("2")),
              #("name", types.to_dynamic("Bob")),
              #("tags", types.to_dynamic(["user"])),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn list_of_objects_with_list_fields_test() {
  let schema_def = build_user_with_tags_schema()
  let query_str =
    "
    query {
      users {
        id
        name
        tags
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Deeply nested lists - list containing objects with nested list fields
// ============================================================================

pub type Team {
  Team(id: String, name: String)
}

fn decode_team(_dyn: Dynamic) -> Result(Team, String) {
  Ok(Team("1", "Test Team"))
}

fn build_nested_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let team_type =
    types.object("Team")
    |> types.id("id", fn(t: Team) { t.id })
    |> types.string("name", fn(t: Team) { t.name })
    |> types.list_object("members", "User", fn(_t: Team) {
      types.to_dynamic([
        dict.from_list([
          #("id", types.to_dynamic("1")),
          #("name", types.to_dynamic("Alice")),
        ]),
        dict.from_list([
          #("id", types.to_dynamic("2")),
          #("name", types.to_dynamic("Bob")),
        ]),
      ])
    })
    |> types.build(decode_team)

  let teams_query =
    query.query(
      "teams",
      schema.list_type(schema.named_type("Team")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("team1")),
              #("name", types.to_dynamic("Engineering")),
              #(
                "members",
                types.to_dynamic([
                  dict.from_list([
                    #("id", types.to_dynamic("1")),
                    #("name", types.to_dynamic("Alice")),
                  ]),
                ]),
              ),
            ]),
            dict.from_list([
              #("id", types.to_dynamic("team2")),
              #("name", types.to_dynamic("Design")),
              #(
                "members",
                types.to_dynamic([
                  dict.from_list([
                    #("id", types.to_dynamic("2")),
                    #("name", types.to_dynamic("Bob")),
                  ]),
                  dict.from_list([
                    #("id", types.to_dynamic("3")),
                    #("name", types.to_dynamic("Charlie")),
                  ]),
                ]),
              ),
            ]),
          ]),
        )
      },
      fn(t) { types.to_dynamic(t) },
    )

  query.new()
  |> query.add_query(teams_query)
  |> query.add_type(user_type)
  |> query.add_type(team_type)
  |> query.build
}

pub fn deeply_nested_list_test() {
  let schema_def = build_nested_list_schema()
  let query_str =
    "
    query {
      teams {
        id
        name
        members {
          id
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn deeply_nested_list_partial_selection_test() {
  let schema_def = build_nested_list_schema()
  let query_str =
    "
    query {
      teams {
        name
        members {
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Test: Single item list
// ============================================================================

fn build_single_item_list_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_info) {
        Ok(
          types.to_dynamic([
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Only User")),
            ]),
          ]),
        )
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn single_item_list_test() {
  let schema_def = build_single_item_list_schema()
  let query_str =
    "
    query {
      users {
        id
        name
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)

  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}
