import mochi/internal/ast

pub opaque type DocumentCache {
  DocumentCache(inner: CacheInner)
}

type CacheInner

@external(erlang, "document_cache_ffi", "new")
fn ffi_new(max_size: Int) -> CacheInner

@external(erlang, "document_cache_ffi", "get")
fn ffi_get(inner: CacheInner, key: String) -> Result(ast.Document, Nil)

@external(erlang, "document_cache_ffi", "put")
fn ffi_put(inner: CacheInner, key: String, value: ast.Document) -> Nil

@external(erlang, "document_cache_ffi", "size")
fn ffi_size(inner: CacheInner) -> Int

const default_max_size = 1000

pub fn new() -> DocumentCache {
  DocumentCache(ffi_new(default_max_size))
}

pub fn new_with_max(max_size: Int) -> DocumentCache {
  DocumentCache(ffi_new(max_size))
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
