import gleam/option.{type Option, None, Some}

pub opaque type SharedCache {
  SharedCache(inner: CacheInner)
}

type CacheInner

@external(erlang, "dataloader_cache_ffi", "new")
fn ffi_new(name: String) -> CacheInner

@external(erlang, "dataloader_cache_ffi", "get")
fn ffi_get(cache: CacheInner, key: a) -> Result(b, Nil)

@external(erlang, "dataloader_cache_ffi", "put")
fn ffi_put(cache: CacheInner, key: a, value: b) -> Nil

@external(erlang, "dataloader_cache_ffi", "put_many")
fn ffi_put_many(cache: CacheInner, pairs: List(#(a, b))) -> Nil

@external(erlang, "dataloader_cache_ffi", "invalidate")
fn ffi_invalidate(cache: CacheInner, key: a) -> Nil

@external(erlang, "dataloader_cache_ffi", "clear")
fn ffi_clear(cache: CacheInner) -> Nil

/// Create an ETS-backed shared cache. The same name always returns the same
/// underlying ETS table, so it is safe to call from multiple processes or on
/// every request — only one table is created per name.
pub fn new(name: String) -> SharedCache {
  SharedCache(ffi_new(name))
}

/// Look up a key. Returns `Some(result)` if present, `None` if not cached.
pub fn get(cache: SharedCache, key: key) -> Option(Result(value, String)) {
  case ffi_get(cache.inner, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

/// Store a single result.
pub fn put(cache: SharedCache, key: key, value: Result(value, String)) -> Nil {
  ffi_put(cache.inner, key, value)
}

/// Store multiple results at once.
pub fn put_many(
  cache: SharedCache,
  pairs: List(#(key, Result(value, String))),
) -> Nil {
  ffi_put_many(cache.inner, pairs)
}

/// Remove a key — call this after a mutation that invalidates cached data.
pub fn invalidate(cache: SharedCache, key: key) -> Nil {
  ffi_invalidate(cache.inner, key)
}

/// Wipe all entries from the cache.
pub fn clear(cache: SharedCache) -> Nil {
  ffi_clear(cache.inner)
}
