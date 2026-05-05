import mochi/internal/ast

pub opaque type DocumentCache {
  DocumentCache(inner: CacheInner)
}

type CacheInner

@external(erlang, "document_cache_ffi", "new")
fn ffi_new(max_size: Int, min_size: Int) -> CacheInner

@external(erlang, "document_cache_ffi", "get")
fn ffi_get(inner: CacheInner, key: String) -> Result(ast.Document, Nil)

@external(erlang, "document_cache_ffi", "put")
fn ffi_put(inner: CacheInner, key: String, value: ast.Document) -> Nil

@external(erlang, "document_cache_ffi", "size")
fn ffi_size(inner: CacheInner) -> Int

const default_max_size = 1000

/// Queries shorter than this are not cached: parsing them is ~3 µs and
/// the ETS lookup + term copy costs roughly the same. Threshold tuned
/// so the cache only stores entries where the saving outweighs the
/// lookup overhead. Override with [`new_with_min_size`](#new_with_min_size).
const default_min_size = 200

pub fn new() -> DocumentCache {
  DocumentCache(ffi_new(default_max_size, default_min_size))
}

pub fn new_with_max(max_size: Int) -> DocumentCache {
  DocumentCache(ffi_new(max_size, default_min_size))
}

/// Construct a cache with a custom byte-size threshold. Queries below
/// `min_size` bytes bypass the cache entirely; the parser is fast enough
/// that the ETS roundtrip would cost more than re-parsing.
pub fn new_with_min_size(max_size: Int, min_size: Int) -> DocumentCache {
  DocumentCache(ffi_new(max_size, min_size))
}

pub fn get(cache: DocumentCache, query: String) -> Result(ast.Document, Nil) {
  ffi_get(cache.inner, query)
}

pub fn put(cache: DocumentCache, query: String, doc: ast.Document) -> Nil {
  ffi_put(cache.inner, query, doc)
}

pub fn size(cache: DocumentCache) -> Int {
  ffi_size(cache.inner)
}
