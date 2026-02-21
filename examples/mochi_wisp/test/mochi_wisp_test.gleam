import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types
import mochi_wisp/graphql_handler
import mochi_wisp/schema as wisp_schema

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// GraphQL Request Parsing Tests
// ============================================================================

pub fn parse_simple_query_test() {
  let body = "{\"query\": \"{ hello }\"}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_true(case result {
    Ok(req) -> req.query == "{ hello }"
    Error(_) -> False
  })
}

pub fn parse_query_with_variables_test() {
  let body =
    "{\"query\": \"query GetUser($id: ID!) { user(id: $id) { name } }\", \"variables\": {\"id\": \"123\"}}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_ok(result)
  case result {
    Ok(req) -> {
      should.equal(
        req.query,
        "query GetUser($id: ID!) { user(id: $id) { name } }",
      )
      should.be_true(case req.variables {
        Some(_) -> True
        None -> False
      })
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_query_with_operation_name_test() {
  let body =
    "{\"query\": \"query GetUsers { users { id } }\", \"operationName\": \"GetUsers\"}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_ok(result)
  case result {
    Ok(req) -> {
      should.equal(req.operation_name, Some("GetUsers"))
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_query_without_optional_fields_test() {
  let body = "{\"query\": \"{ users { id } }\"}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_ok(result)
  case result {
    Ok(req) -> {
      should.equal(req.variables, None)
      should.equal(req.operation_name, None)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_invalid_json_test() {
  let body = "{ not valid json }"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_error(result)
}

pub fn parse_missing_query_field_test() {
  let body = "{\"variables\": {}}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_error(result)
}

pub fn parse_null_query_field_test() {
  let body = "{\"query\": null}"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_error(result)
}

pub fn parse_empty_body_test() {
  let body = ""
  let result = graphql_handler.parse_graphql_request(body)

  should.be_error(result)
}

pub fn parse_array_instead_of_object_test() {
  let body = "[{\"query\": \"{ hello }\"}]"
  let result = graphql_handler.parse_graphql_request(body)

  should.be_error(result)
}

// ============================================================================
// Schema Definition Tests
// ============================================================================

pub fn minimal_schema_test() {
  let hello_query =
    query.query(
      "hello",
      schema.NonNull(schema.Named("String")),
      fn(_ctx) { Ok("world") },
      fn(s) { types.to_dynamic(s) },
    )

  let s =
    query.new()
    |> query.add_query(hello_query)
    |> query.build

  let result = executor.execute_query(s, "{ hello }")

  should.equal(result.errors, [])
}

pub fn scalar_types_schema_test() {
  let scalars_query =
    query.query(
      "scalars",
      schema.NonNull(schema.Named("String")),
      fn(_ctx) { Ok("test") },
      fn(s) { types.to_dynamic(s) },
    )

  let s =
    query.new()
    |> query.add_query(scalars_query)
    |> query.build

  let result = executor.execute_query(s, "{ scalars }")
  should.equal(result.errors, [])
}

pub fn users_list_schema_test() {
  let users_query =
    query.query(
      "userNames",
      schema.NonNull(schema.List(schema.Named("String"))),
      fn(_ctx) { Ok(["Alice", "Bob", "Charlie"]) },
      fn(names) { types.to_dynamic(names) },
    )

  let s =
    query.new()
    |> query.add_query(users_query)
    |> query.build

  let result = executor.execute_query(s, "{ userNames }")

  should.equal(result.errors, [])
}

pub fn users_object_schema_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: #(String, String)) { u.0 })
    |> types.string("name", fn(u: #(String, String)) { u.1 })
    |> types.build(fn(_dyn) { Ok(#("1", "Alice")) })

  let users_query =
    query.query(
      "users",
      schema.NonNull(schema.List(schema.Named("User"))),
      fn(_ctx) { Ok([#("1", "Alice"), #("2", "Bob")]) },
      fn(users) {
        types.to_dynamic(
          users
          |> list.map(fn(u) {
            dict.from_list([
              #("id", types.to_dynamic(u.0)),
              #("name", types.to_dynamic(u.1)),
            ])
          }),
        )
      },
    )

  let s =
    query.new()
    |> query.add_query(users_query)
    |> query.add_type(user_type)
    |> query.build

  let result = executor.execute_query(s, "{ users { id name } }")

  should.equal(result.errors, [])
}

// ============================================================================
// Wisp Schema Tests
// ============================================================================

pub fn wisp_schema_users_query_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { id name email } }")
  should.equal(result.errors, [])
}

pub fn wisp_schema_users_with_role_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { id name email role } }")
  should.equal(result.errors, [])
}

pub fn wisp_schema_user_by_id_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ user(id: \"1\") { id name } }")
  should.equal(result.errors, [])
}

pub fn wisp_schema_user_not_found_test() {
  let s = wisp_schema.build_schema()
  let result =
    executor.execute_query(s, "{ user(id: \"nonexistent\") { id name } }")
  // Should have an error for user not found
  should.be_true(result.errors != [])
}

pub fn wisp_schema_user_all_fields_test() {
  let s = wisp_schema.build_schema()
  let result =
    executor.execute_query(s, "{ user(id: \"2\") { id name email role } }")
  should.equal(result.errors, [])
}

pub fn wisp_schema_user_id_1_details_test() {
  let s = wisp_schema.build_schema()
  let result =
    executor.execute_query(s, "{ user(id: \"1\") { id name email role } }")
  should.equal(result.errors, [])
}

pub fn wisp_schema_user_id_3_details_test() {
  let s = wisp_schema.build_schema()
  let result =
    executor.execute_query(s, "{ user(id: \"3\") { id name email role } }")
  should.equal(result.errors, [])
}

// ============================================================================
// Domain Logic Tests
// ============================================================================

pub fn sample_users_count_test() {
  let users = wisp_schema.sample_users()
  should.equal(list.length(users), 3)
}

pub fn sample_users_first_is_alice_test() {
  let users = wisp_schema.sample_users()
  case list.first(users) {
    Ok(user) -> should.equal(user.name, "Alice")
    Error(_) -> should.fail()
  }
}

pub fn find_user_success_test() {
  let result = wisp_schema.find_user_by_id("1")
  should.be_ok(result)
  case result {
    Ok(user) -> {
      should.equal(user.name, "Alice")
      should.equal(user.email, "alice@example.com")
    }
    Error(_) -> should.fail()
  }
}

pub fn find_user_bob_test() {
  let result = wisp_schema.find_user_by_id("2")
  should.be_ok(result)
  case result {
    Ok(user) -> {
      should.equal(user.name, "Bob")
      should.equal(user.role, wisp_schema.Member)
    }
    Error(_) -> should.fail()
  }
}

pub fn find_user_charlie_test() {
  let result = wisp_schema.find_user_by_id("3")
  should.be_ok(result)
  case result {
    Ok(user) -> {
      should.equal(user.name, "Charlie")
      should.equal(user.role, wisp_schema.Guest)
    }
    Error(_) -> should.fail()
  }
}

pub fn find_user_not_found_test() {
  let result = wisp_schema.find_user_by_id("999")
  should.be_error(result)
}

pub fn role_to_string_admin_test() {
  should.equal(wisp_schema.role_to_string(wisp_schema.Admin), "ADMIN")
}

pub fn role_to_string_member_test() {
  should.equal(wisp_schema.role_to_string(wisp_schema.Member), "MEMBER")
}

pub fn role_to_string_guest_test() {
  should.equal(wisp_schema.role_to_string(wisp_schema.Guest), "GUEST")
}

pub fn string_to_role_admin_test() {
  should.equal(wisp_schema.string_to_role("ADMIN"), Ok(wisp_schema.Admin))
}

pub fn string_to_role_member_test() {
  should.equal(wisp_schema.string_to_role("MEMBER"), Ok(wisp_schema.Member))
}

pub fn string_to_role_guest_test() {
  should.equal(wisp_schema.string_to_role("GUEST"), Ok(wisp_schema.Guest))
}

pub fn string_to_role_invalid_test() {
  should.be_error(wisp_schema.string_to_role("INVALID"))
}

pub fn string_to_role_lowercase_invalid_test() {
  should.be_error(wisp_schema.string_to_role("admin"))
}

// ============================================================================
// Execution Result JSON Encoding Tests
// ============================================================================

pub fn execution_result_to_json_success_test() {
  let result =
    executor.ExecutionResult(
      data: Some(types.to_dynamic("test data")),
      errors: [],
    )
  let json_str = graphql_handler.execution_result_to_json(result)
  // Should contain data field
  should.be_true(string.contains(json_str, "data"))
}

pub fn execution_result_to_json_with_errors_test() {
  let result =
    executor.ExecutionResult(data: None, errors: [
      executor.ValidationError("Test error", ["field"], None),
    ])
  let json_str = graphql_handler.execution_result_to_json(result)
  // Should contain errors field
  should.be_true(string.contains(json_str, "errors"))
  should.be_true(string.contains(json_str, "Test error"))
}

pub fn execution_result_to_json_with_path_test() {
  let result =
    executor.ExecutionResult(data: None, errors: [
      executor.ResolverError("Resolver failed", ["query", "user", "name"], None),
    ])
  let json_str = graphql_handler.execution_result_to_json(result)
  should.be_true(string.contains(json_str, "path"))
}

pub fn execution_result_to_json_null_data_test() {
  let result = executor.ExecutionResult(data: None, errors: [])
  let json_str = graphql_handler.execution_result_to_json(result)
  should.be_true(string.contains(json_str, "null"))
}

// ============================================================================
// Query Argument Decoding Tests
// ============================================================================

pub fn decode_user_by_id_args_success_test() {
  let args = dict.from_list([#("id", types.to_dynamic("123"))])
  let result = wisp_schema.decode_user_by_id_args(args)
  should.be_ok(result)
  case result {
    Ok(args_decoded) -> should.equal(args_decoded.id, "123")
    Error(_) -> should.fail()
  }
}

pub fn decode_user_by_id_args_missing_test() {
  let args = dict.new()
  let result = wisp_schema.decode_user_by_id_args(args)
  should.be_error(result)
}

pub fn decode_user_by_id_args_empty_id_test() {
  let args = dict.from_list([#("id", types.to_dynamic(""))])
  let result = wisp_schema.decode_user_by_id_args(args)
  should.be_ok(result)
  case result {
    Ok(args_decoded) -> should.equal(args_decoded.id, "")
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Enum Type Tests
// ============================================================================

pub fn role_enum_name_test() {
  let role_enum = wisp_schema.role_enum()
  should.equal(role_enum.name, "Role")
}

pub fn role_enum_values_count_test() {
  let role_enum = wisp_schema.role_enum()
  should.equal(dict.size(role_enum.values), 3)
}

pub fn role_enum_has_admin_test() {
  let role_enum = wisp_schema.role_enum()
  should.be_true(dict.has_key(role_enum.values, "ADMIN"))
}

pub fn role_enum_has_member_test() {
  let role_enum = wisp_schema.role_enum()
  should.be_true(dict.has_key(role_enum.values, "MEMBER"))
}

pub fn role_enum_has_guest_test() {
  let role_enum = wisp_schema.role_enum()
  should.be_true(dict.has_key(role_enum.values, "GUEST"))
}

// ============================================================================
// User Type Tests
// ============================================================================

pub fn user_type_name_test() {
  let user_t = wisp_schema.user_type()
  should.equal(user_t.name, "User")
}

pub fn user_type_has_id_field_test() {
  let user_t = wisp_schema.user_type()
  should.be_true(dict.has_key(user_t.fields, "id"))
}

pub fn user_type_has_name_field_test() {
  let user_t = wisp_schema.user_type()
  should.be_true(dict.has_key(user_t.fields, "name"))
}

pub fn user_type_has_email_field_test() {
  let user_t = wisp_schema.user_type()
  should.be_true(dict.has_key(user_t.fields, "email"))
}

pub fn user_type_has_role_field_test() {
  let user_t = wisp_schema.user_type()
  should.be_true(dict.has_key(user_t.fields, "role"))
}

pub fn user_type_field_count_test() {
  let user_t = wisp_schema.user_type()
  should.equal(dict.size(user_t.fields), 4)
}

// ============================================================================
// User Encoder Tests
// ============================================================================

pub fn user_to_dynamic_test() {
  let user =
    wisp_schema.User(
      id: "1",
      name: "Test User",
      email: "test@example.com",
      role: wisp_schema.Admin,
    )
  let _dynamic_val = wisp_schema.user_to_dynamic(user)
  // Just verify it doesn't crash
  should.be_true(True)
}

pub fn users_encoder_test() {
  let users = [
    wisp_schema.User(
      id: "1",
      name: "Alice",
      email: "alice@example.com",
      role: wisp_schema.Admin,
    ),
    wisp_schema.User(
      id: "2",
      name: "Bob",
      email: "bob@example.com",
      role: wisp_schema.Member,
    ),
  ]
  let _encoded = wisp_schema.users_encoder(users)
  // Just verify it doesn't crash
  should.be_true(True)
}

pub fn user_encoder_single_test() {
  let user =
    wisp_schema.User(
      id: "1",
      name: "Test",
      email: "test@test.com",
      role: wisp_schema.Guest,
    )
  let _encoded = wisp_schema.user_encoder(user)
  should.be_true(True)
}

// ============================================================================
// Multiple Queries Combined Tests
// ============================================================================

pub fn multiple_fields_query_test() {
  let s = wisp_schema.build_schema()
  // Query both users list and a specific user
  let result =
    executor.execute_query(s, "{ users { id } user(id: \"1\") { name } }")
  should.equal(result.errors, [])
}

pub fn nested_selection_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { id name } }")
  should.equal(result.errors, [])
  should.be_true(case result.data {
    Some(_) -> True
    None -> False
  })
}

// ============================================================================
// Benchmark Smoke Tests
// ============================================================================

import mochi_wisp/benchmark

pub fn benchmark_measure_time_test() {
  // Test that measure_time works correctly
  let #(time_us, result) = benchmark.measure_time(fn() { 1 + 1 })
  should.equal(result, 2)
  // Time should be non-negative
  should.be_true(time_us >= 0)
}

pub fn benchmark_monotonic_time_test() {
  // Test that monotonic time increases
  let t1 = benchmark.monotonic_time_us()
  let t2 = benchmark.monotonic_time_us()
  should.be_true(t2 >= t1)
}

// ============================================================================
// Edge Case Tests
// ============================================================================

pub fn query_only_id_field_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { id } }")
  should.equal(result.errors, [])
}

pub fn query_only_name_field_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { name } }")
  should.equal(result.errors, [])
}

pub fn query_only_email_field_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { email } }")
  should.equal(result.errors, [])
}

pub fn query_only_role_field_test() {
  let s = wisp_schema.build_schema()
  let result = executor.execute_query(s, "{ users { role } }")
  should.equal(result.errors, [])
}

// ============================================================================
// Query Cache Tests
// ============================================================================

import mochi_wisp/query_cache

pub fn query_cache_init_test() {
  // Initialize should succeed (idempotent)
  query_cache.init()
  query_cache.init()
  // Should not crash
  should.be_true(True)
}

pub fn query_cache_get_or_parse_test() {
  query_cache.init()
  query_cache.clear()

  // First call should parse and cache
  let result1 = query_cache.get_or_parse("{ hello }")
  should.be_ok(result1)

  // Second call should hit cache
  let result2 = query_cache.get_or_parse("{ hello }")
  should.be_ok(result2)

  // Both should return the same document structure
  case result1, result2 {
    Ok(doc1), Ok(doc2) -> {
      should.equal(list.length(doc1.definitions), list.length(doc2.definitions))
    }
    _, _ -> should.fail()
  }
}

pub fn query_cache_stats_test() {
  query_cache.init()
  query_cache.clear()

  // Initial stats should be zero
  let stats1 = query_cache.stats()
  should.equal(stats1.hits, 0)
  should.equal(stats1.misses, 0)
  should.equal(stats1.size, 0)

  // Parse a query - should be a miss
  let _ = query_cache.get_or_parse("{ users { id } }")
  let stats2 = query_cache.stats()
  should.equal(stats2.misses, 1)
  should.equal(stats2.hits, 0)
  should.equal(stats2.size, 1)

  // Same query again - should be a hit
  let _ = query_cache.get_or_parse("{ users { id } }")
  let stats3 = query_cache.stats()
  should.equal(stats3.hits, 1)
  should.equal(stats3.misses, 1)
  should.equal(stats3.size, 1)
}

pub fn query_cache_size_test() {
  query_cache.init()
  query_cache.clear()

  should.equal(query_cache.size(), 0)

  let _ = query_cache.get_or_parse("{ a }")
  should.equal(query_cache.size(), 1)

  let _ = query_cache.get_or_parse("{ b }")
  should.equal(query_cache.size(), 2)

  // Same query should not increase size
  let _ = query_cache.get_or_parse("{ a }")
  should.equal(query_cache.size(), 2)
}

pub fn query_cache_clear_test() {
  query_cache.init()

  let _ = query_cache.get_or_parse("{ x }")
  let _ = query_cache.get_or_parse("{ y }")
  should.be_true(query_cache.size() > 0)

  query_cache.clear()
  should.equal(query_cache.size(), 0)

  let stats = query_cache.stats()
  should.equal(stats.hits, 0)
  should.equal(stats.misses, 0)
}

pub fn query_cache_invalid_query_test() {
  query_cache.init()
  query_cache.clear()

  // Invalid query should return error and not cache
  let result = query_cache.get_or_parse("{ invalid query !!!")
  should.be_error(result)

  // Cache should still be empty (invalid queries not cached)
  should.equal(query_cache.size(), 0)
}
