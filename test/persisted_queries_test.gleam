// Tests for mochi/persisted_queries.gleam - Automatic Persisted Queries (APQ)
import gleam/option.{None, Some}
import gleeunit/should
import mochi/persisted_queries

// ============================================================================
// Store Operations Tests
// ============================================================================

pub fn new_store_test() {
  let store = persisted_queries.new()
  should.equal(persisted_queries.size(store), 0)
}

pub fn register_and_lookup_test() {
  let store = persisted_queries.new()
  let query = "{ hello }"
  let #(new_store, hash) = persisted_queries.register(store, query)

  should.equal(persisted_queries.size(new_store), 1)
  should.equal(persisted_queries.lookup(new_store, hash), Some(query))
}

pub fn lookup_nonexistent_test() {
  let store = persisted_queries.new()
  should.equal(persisted_queries.lookup(store, "fakehash"), None)
}

pub fn with_queries_test() {
  let queries = ["{ hello }", "{ world }"]
  let store = persisted_queries.with_queries(queries)
  should.equal(persisted_queries.size(store), 2)
}

pub fn register_same_query_twice_test() {
  let store = persisted_queries.new()
  let query = "{ hello }"
  let #(store2, hash1) = persisted_queries.register(store, query)
  let #(store3, hash2) = persisted_queries.register(store2, query)
  // Same query produces same hash, size stays 1
  should.equal(hash1, hash2)
  should.equal(persisted_queries.size(store3), 1)
}

// ============================================================================
// Hash Tests
// ============================================================================

pub fn hash_query_deterministic_test() {
  let query = "{ hello }"
  let hash1 = persisted_queries.hash_query(query)
  let hash2 = persisted_queries.hash_query(query)
  should.equal(hash1, hash2)
}

pub fn hash_query_different_queries_test() {
  let hash1 = persisted_queries.hash_query("{ hello }")
  let hash2 = persisted_queries.hash_query("{ world }")
  should.not_equal(hash1, hash2)
}

pub fn hash_query_is_hex_string_test() {
  let hash = persisted_queries.hash_query("{ hello }")
  // SHA256 hex is 64 characters
  should.equal(
    hash
      |> fn(s) { s |> bit_array.from_string |> bit_array.byte_size },
    64,
  )
}

// ============================================================================
// APQ Protocol Tests
// ============================================================================

pub fn apq_query_not_found_test() {
  let store = persisted_queries.new()
  let result = persisted_queries.process_apq(store, None, "nonexistent_hash")
  should.equal(result, Error(persisted_queries.PersistedQueryNotFound))
}

pub fn apq_register_new_query_test() {
  let store = persisted_queries.new()
  let query = "{ hello }"
  let hash = persisted_queries.hash_query(query)
  let result = persisted_queries.process_apq(store, Some(query), hash)

  case result {
    Ok(#(new_store, returned_query)) -> {
      should.equal(returned_query, query)
      should.equal(persisted_queries.size(new_store), 1)
    }
    Error(_) -> should.fail()
  }
}

pub fn apq_retrieve_cached_query_test() {
  let store = persisted_queries.new()
  let query = "{ hello }"
  let #(store2, hash) = persisted_queries.register(store, query)

  // Client sends just the hash, no query
  let result = persisted_queries.process_apq(store2, None, hash)

  case result {
    Ok(#(_store3, returned_query)) -> {
      should.equal(returned_query, query)
    }
    Error(_) -> should.fail()
  }
}

pub fn apq_hash_mismatch_test() {
  let store = persisted_queries.new()
  let query = "{ hello }"
  let wrong_hash =
    "0000000000000000000000000000000000000000000000000000000000000000"
  let result = persisted_queries.process_apq(store, Some(query), wrong_hash)

  case result {
    Error(persisted_queries.HashMismatch(expected, actual)) -> {
      should.equal(expected, wrong_hash)
      should.not_equal(actual, wrong_hash)
    }
    _ -> should.fail()
  }
}

import gleam/bit_array
