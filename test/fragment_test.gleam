import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/executor
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.build(decode_user)

  let users_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_info) {
        Ok(types.to_dynamic(User("1", "Alice", "alice@example.com")))
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
}

fn decode_user(_dyn: dynamic.Dynamic) -> Result(User, String) {
  Ok(User("1", "Alice", "alice@example.com"))
}

fn execute_query(
  schema_def: schema.Schema,
  query_str: String,
) -> executor.ExecutionResult {
  executor.execute_query(schema_def, query_str)
}

// ============================================================================
// Fragment Spread Tests
// ============================================================================

pub fn fragment_spread_basic_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ...UserFields
      }
    }

    fragment UserFields on User {
      id
      name
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn fragment_spread_with_all_fields_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ...AllUserFields
      }
    }

    fragment AllUserFields on User {
      id
      name
      email
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn fragment_spread_mixed_with_fields_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        ...NameFragment
      }
    }

    fragment NameFragment on User {
      name
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn undefined_fragment_error_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ...UndefinedFragment
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_false(list.is_empty(result.errors))
}

// ============================================================================
// Inline Fragment Tests
// ============================================================================

pub fn inline_fragment_with_type_condition_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        ... on User {
          name
          email
        }
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn inline_fragment_without_type_condition_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        ... {
          name
        }
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn inline_fragment_type_mismatch_test() {
  let schema_def = build_test_schema()
  // This should succeed but the inline fragment should be skipped
  let query_str =
    "
    query {
      user {
        id
        ... on OtherType {
          name
        }
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  // Type mismatch should not produce an error, just skip the fragment
  should.be_true(option.is_some(result.data))
}

// ============================================================================
// Nested Fragment Tests
// ============================================================================

pub fn nested_fragments_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ...UserBasic
      }
    }

    fragment UserBasic on User {
      id
      ...UserName
    }

    fragment UserName on User {
      name
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn multiple_fragments_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ...IdFragment
        ...NameFragment
        ...EmailFragment
      }
    }

    fragment IdFragment on User {
      id
    }

    fragment NameFragment on User {
      name
    }

    fragment EmailFragment on User {
      email
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Field Deduplication Tests (Issue #43)
// ============================================================================

pub fn duplicate_field_merged_test() {
  // When a field appears twice in a selection set, it should be merged into one
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        id
        name
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn duplicate_field_with_same_alias_test() {
  // Duplicate fields with the same alias should be merged
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        userId: id
        userId: id
        name
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn field_from_fragment_deduplication_test() {
  // Fields from fragments that duplicate direct fields should be merged
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        name
        ...UserFields
      }
    }

    fragment UserFields on User {
      id
      name
      email
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Multiple Operations Tests (Issue #43)
// ============================================================================

pub fn execute_with_operation_name_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser {
      user {
        id
        name
      }
    }

    query GetUserEmail {
      user {
        id
        email
      }
    }
    "

  // Parse the query first
  case parser.parse(query_str) {
    Ok(document) -> {
      let ctx = schema.execution_context(types.to_dynamic(dict.new()))
      // Execute the first operation by name
      let result =
        executor.execute_with_operation_name(
          schema_def,
          document,
          None,
          ctx,
          dict.new(),
          Some("GetUser"),
        )
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn execute_with_operation_name_second_operation_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser {
      user {
        id
        name
      }
    }

    query GetUserEmail {
      user {
        id
        email
      }
    }
    "

  // Parse the query first
  case parser.parse(query_str) {
    Ok(document) -> {
      let ctx = schema.execution_context(types.to_dynamic(dict.new()))
      // Execute the second operation by name
      let result =
        executor.execute_with_operation_name(
          schema_def,
          document,
          None,
          ctx,
          dict.new(),
          Some("GetUserEmail"),
        )
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn execute_with_operation_name_not_found_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    query GetUser {
      user {
        id
        name
      }
    }
    "

  // Parse the query first
  case parser.parse(query_str) {
    Ok(document) -> {
      let ctx = schema.execution_context(types.to_dynamic(dict.new()))
      // Try to execute a non-existent operation
      let result =
        executor.execute_with_operation_name(
          schema_def,
          document,
          None,
          ctx,
          dict.new(),
          Some("NonExistent"),
        )
      // Should have an error for operation not found
      should.be_false(list.is_empty(result.errors))
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn execute_anonymous_when_alone_test() {
  let schema_def = build_test_schema()
  let query_str =
    "
    {
      user {
        id
        name
      }
    }
    "

  // Anonymous operation should work when it's the only one
  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Inline Fragment Without Type Condition Additional Tests (Issue #43)
// ============================================================================

pub fn inline_fragment_without_type_multiple_fields_test() {
  // Inline fragment without type condition with multiple fields
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        ... {
          id
          name
          email
        }
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn inline_fragment_without_type_with_directive_test() {
  // Inline fragment without type condition with a directive
  let schema_def = build_test_schema()
  let query_str =
    "
    query {
      user {
        id
        ... @skip(if: false) {
          name
        }
      }
    }
    "

  let result = execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}
