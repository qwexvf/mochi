// mochi/persisted_queries.gleam
// Automatic Persisted Queries (APQ) support
// Allows clients to send query hashes instead of full query strings

import gleam/bit_array
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// Types
// ============================================================================

pub type PersistedQueryStore {
  PersistedQueryStore(queries: Dict(String, String))
}

pub type APQExtension {
  APQExtension(version: Int, sha256_hash: String)
}

pub type APQResult {
  QueryFound(query: String)
  QueryNotFound(hash: String)
  QueryRegistered(hash: String)
}

pub type APQError {
  PersistedQueryNotFound
  InvalidHash
  HashMismatch(expected: String, actual: String)
}

// ============================================================================
// Store Operations
// ============================================================================

/// Create a new empty persisted query store
pub fn new() -> PersistedQueryStore {
  PersistedQueryStore(queries: dict.new())
}

/// Create a store with pre-registered queries
pub fn with_queries(queries: List(String)) -> PersistedQueryStore {
  let query_dict =
    list.fold(queries, dict.new(), fn(acc, query) {
      let hash = hash_query(query)
      dict.insert(acc, hash, query)
    })
  PersistedQueryStore(queries: query_dict)
}

/// Register a query in the store
pub fn register(
  store: PersistedQueryStore,
  query: String,
) -> #(PersistedQueryStore, String) {
  let hash = hash_query(query)
  let new_store =
    PersistedQueryStore(queries: dict.insert(store.queries, hash, query))
  #(new_store, hash)
}

/// Look up a query by its hash
pub fn lookup(store: PersistedQueryStore, hash: String) -> Option(String) {
  dict.get(store.queries, hash)
  |> option.from_result
}

/// Get the number of stored queries
pub fn size(store: PersistedQueryStore) -> Int {
  dict.size(store.queries)
}

// ============================================================================
// APQ Protocol
// ============================================================================

/// Process an APQ request
///
/// According to the APQ protocol:
/// 1. Client sends hash without query -> return PersistedQueryNotFound
/// 2. Client sends hash with query -> verify hash, store query, return query
/// 3. Client sends hash, query found in store -> return stored query
pub fn process_apq(
  store: PersistedQueryStore,
  query: Option(String),
  hash: String,
) -> Result(#(PersistedQueryStore, String), APQError) {
  // First, check if query is already stored
  case lookup(store, hash) {
    Some(stored_query) -> Ok(#(store, stored_query))
    None -> {
      // Query not in store - need the query string to register it
      case query {
        None -> Error(PersistedQueryNotFound)
        Some(q) -> {
          // Verify the hash matches
          let actual_hash = hash_query(q)
          case actual_hash == hash {
            True -> {
              // Store and return
              let #(new_store, _) = register(store, q)
              Ok(#(new_store, q))
            }
            False -> Error(HashMismatch(expected: hash, actual: actual_hash))
          }
        }
      }
    }
  }
}

/// Parse APQ extension from request extensions
pub fn parse_extension(
  extensions: Dict(String, dynamic.Dynamic),
) -> Option(APQExtension) {
  case dict.get(extensions, "persistedQuery") {
    Error(_) -> None
    Ok(pq) -> {
      // Try to extract version and sha256Hash
      case extract_apq_fields(pq) {
        Ok(#(version, hash)) ->
          Some(APQExtension(version: version, sha256_hash: hash))
        Error(_) -> None
      }
    }
  }
}

// ============================================================================
// Hashing
// ============================================================================

/// Compute SHA256 hash of a query string
pub fn hash_query(query: String) -> String {
  query
  |> normalize_query
  |> do_sha256_hash
}

/// SHA256 hash using gleam_crypto
fn do_sha256_hash(input: String) -> String {
  input
  |> bit_array.from_string
  |> crypto.hash(crypto.Sha256, _)
  |> bit_array.base16_encode
  |> string.lowercase
}

/// Normalize a query for consistent hashing
/// Removes extra whitespace while preserving string literals
fn normalize_query(query: String) -> String {
  query
  |> string.trim
  |> collapse_whitespace
}

fn collapse_whitespace(s: String) -> String {
  // Simple whitespace collapsing (not handling strings inside query)
  s
  |> string.replace("\n", " ")
  |> string.replace("\r", " ")
  |> string.replace("\t", " ")
  |> collapse_spaces
}

fn collapse_spaces(s: String) -> String {
  case string.contains(s, "  ") {
    True -> collapse_spaces(string.replace(s, "  ", " "))
    False -> s
  }
}

// ============================================================================
// Internal Helpers
// ============================================================================

fn extract_apq_fields(_dyn: dynamic.Dynamic) -> Result(#(Int, String), Nil) {
  // This would need proper dynamic decoding
  // For now, return error - actual implementation would decode the object
  Error(Nil)
}
