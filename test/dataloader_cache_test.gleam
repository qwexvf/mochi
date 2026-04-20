import gleam/option
import gleeunit/should
import mochi/dataloader_cache

@target(erlang)
pub fn shared_cache_stores_and_retrieves_test() {
  let cache = dataloader_cache.new("test_dl_shared_1")
  dataloader_cache.put(cache, "key1", Ok("value1"))
  dataloader_cache.get(cache, "key1")
  |> should.equal(option.Some(Ok("value1")))
  dataloader_cache.get(cache, "missing")
  |> should.equal(option.None)
  dataloader_cache.clear(cache)
}

@target(erlang)
pub fn shared_cache_put_many_test() {
  let cache = dataloader_cache.new("test_dl_shared_2")
  dataloader_cache.put_many(cache, [#("a", Ok("1")), #("b", Ok("2"))])
  dataloader_cache.get(cache, "a") |> should.equal(option.Some(Ok("1")))
  dataloader_cache.get(cache, "b") |> should.equal(option.Some(Ok("2")))
  dataloader_cache.clear(cache)
}

@target(erlang)
pub fn shared_cache_invalidate_test() {
  let cache = dataloader_cache.new("test_dl_shared_3")
  dataloader_cache.put(cache, "k", Ok("v"))
  dataloader_cache.invalidate(cache, "k")
  dataloader_cache.get(cache, "k") |> should.equal(option.None)
}

@target(erlang)
pub fn shared_cache_same_name_returns_same_table_test() {
  let cache1 = dataloader_cache.new("test_dl_shared_4")
  let cache2 = dataloader_cache.new("test_dl_shared_4")
  dataloader_cache.put(cache1, "x", Ok(42))
  dataloader_cache.get(cache2, "x") |> should.equal(option.Some(Ok(42)))
  dataloader_cache.clear(cache1)
}
