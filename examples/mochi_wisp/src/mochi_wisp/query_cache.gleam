// mochi_wisp/query_cache.gleam
// High-performance query caching using ETS

import gleam/option.{type Option, None, Some}
import mochi/ast
import mochi/parser
import mochi_wisp/fast_parser

/// Cache statistics
pub type CacheStats {
  CacheStats(hits: Int, misses: Int, size: Int)
}

/// Initialize the query cache (call once at startup)
@external(erlang, "mochi_query_cache_ffi", "init")
pub fn init() -> Nil

/// Get a parsed query from cache, or parse and cache it (using standard parser)
pub fn get_or_parse(query: String) -> Result(ast.Document, parser.ParseError) {
  case get_cached(query) {
    Some(doc) -> Ok(doc)
    None -> {
      case parser.parse(query) {
        Ok(doc) -> {
          cache_put(query, doc)
          Ok(doc)
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Parse error type for fast parser
pub type FastParseError {
  FastParseError(message: String)
}

/// Get a parsed query from cache, or parse using FAST parser and cache it
/// Uses O(n) binary lexer and cursor-based parser vs O(n²) standard parser
pub fn get_or_parse_fast(query: String) -> Result(ast.Document, FastParseError) {
  case get_cached(query) {
    Some(doc) -> Ok(doc)
    None -> {
      case fast_parser.parse(query) {
        Ok(doc) -> {
          cache_put(query, doc)
          Ok(doc)
        }
        Error(e) -> Error(FastParseError(format_fast_error(e)))
      }
    }
  }
}

fn format_fast_error(error: fast_parser.ParseError) -> String {
  case error {
    fast_parser.LexError(msg) -> "Lex error: " <> msg
    fast_parser.UnexpectedToken(expected, got) ->
      "Unexpected token: expected " <> expected <> ", got " <> got
    fast_parser.UnexpectedEOF(expected) ->
      "Unexpected end of input: expected " <> expected
  }
}

/// Get a cached query
@external(erlang, "mochi_query_cache_ffi", "get")
fn get_cached(query: String) -> Option(ast.Document)

/// Store a parsed query in cache
@external(erlang, "mochi_query_cache_ffi", "put")
fn cache_put(query: String, document: ast.Document) -> Nil

/// Get cache statistics
@external(erlang, "mochi_query_cache_ffi", "stats")
pub fn stats() -> CacheStats

/// Clear the cache
@external(erlang, "mochi_query_cache_ffi", "clear")
pub fn clear() -> Nil

/// Get cache size
@external(erlang, "mochi_query_cache_ffi", "size")
pub fn size() -> Int
