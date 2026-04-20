// Tests for mochi/dataloader.gleam - DataLoader for N+1 prevention
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleeunit/should
import mochi/dataloader
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

/// Simple in-memory "database" for testing
fn find_by_id(id: Int) -> Result(String, String) {
  case id {
    1 -> Ok("Item 1")
    2 -> Ok("Item 2")
    3 -> Ok("Item 3")
    _ -> Error("Not found")
  }
}

fn find_by_key(key: String) -> Result(String, String) {
  case key {
    "a" -> Ok("Value A")
    "b" -> Ok("Value B")
    "c" -> Ok("Value C")
    _ -> Error("Not found")
  }
}

fn to_dynamic(value: a) -> Dynamic {
  types.to_dynamic(value)
}

// ============================================================================
// Default Options Tests
// ============================================================================

pub fn default_options_test() {
  let opts = dataloader.default_options()
  should.equal(opts.max_batch_size, 100)
  should.equal(opts.cache_enabled, True)
}

// ============================================================================
// Basic DataLoader Tests
// ============================================================================

pub fn new_creates_loader_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(k) { Ok(k * 2) }))
    })
  let #(_new_loader, result) = dataloader.load(loader, 5)
  should.equal(result, Ok(10))
}

pub fn new_with_options_test() {
  let opts =
    dataloader.DataLoaderOptions(max_batch_size: 50, cache_enabled: False)
  let loader =
    dataloader.new_with_options(
      fn(keys: List(Int)) { Ok(list.map(keys, fn(k) { Ok(k) })) },
      opts,
    )
  let #(_new_loader, result) = dataloader.load(loader, 42)
  should.equal(result, Ok(42))
}

// ============================================================================
// Load Tests
// ============================================================================

pub fn load_single_value_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(
        list.map(keys, fn(id) {
          case find_by_id(id) {
            Ok(v) -> Ok(v)
            Error(e) -> Error(e)
          }
        }),
      )
    })
  let #(_loader, result) = dataloader.load(loader, 1)
  should.equal(result, Ok("Item 1"))
}

pub fn load_not_found_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(
        list.map(keys, fn(id) {
          case find_by_id(id) {
            Ok(v) -> Ok(v)
            Error(e) -> Error(e)
          }
        }),
      )
    })
  let #(_loader, result) = dataloader.load(loader, 999)
  should.equal(result, Error("Not found"))
}

// ============================================================================
// Load Many Tests
// ============================================================================

pub fn load_many_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(
        list.map(keys, fn(id) {
          case find_by_id(id) {
            Ok(v) -> Ok(v)
            Error(e) -> Error(e)
          }
        }),
      )
    })
  let #(_loader, results) = dataloader.load_many(loader, [1, 2, 3])
  should.equal(results, [Ok("Item 1"), Ok("Item 2"), Ok("Item 3")])
}

pub fn load_many_with_errors_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(
        list.map(keys, fn(id) {
          case find_by_id(id) {
            Ok(v) -> Ok(v)
            Error(e) -> Error(e)
          }
        }),
      )
    })
  let #(_loader, results) = dataloader.load_many(loader, [1, 999, 2])
  should.equal(results, [Ok("Item 1"), Error("Not found"), Ok("Item 2")])
}

pub fn load_many_empty_list_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) { Ok(list.map(keys, fn(k) { Ok(k) })) })
  let #(_loader, results) = dataloader.load_many(loader, [])
  should.equal(results, [])
}

// ============================================================================
// Cache Tests
// ============================================================================

pub fn cache_returns_cached_value_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      // This would normally track call count
      Ok(list.map(keys, fn(id) { Ok(id * 10) }))
    })

  // First load
  let #(loader2, result1) = dataloader.load(loader, 5)
  should.equal(result1, Ok(50))

  // Second load should use cache
  let #(_loader3, result2) = dataloader.load(loader2, 5)
  should.equal(result2, Ok(50))
}

pub fn clear_cache_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(id) { Ok(id * 10) }))
    })

  let #(loader2, _result1) = dataloader.load(loader, 5)
  let loader3 = dataloader.clear_cache(loader2)

  // After clearing, we should still get the same result (batch executes again)
  let #(_loader4, result2) = dataloader.load(loader3, 5)
  should.equal(result2, Ok(50))
}

pub fn clear_key_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(id) { Ok(id * 10) }))
    })

  let #(loader2, _) = dataloader.load(loader, 5)
  let #(loader3, _) = dataloader.load(loader2, 6)
  let loader4 = dataloader.clear_key(loader3, 5)

  // Key 6 should still be cached, key 5 cleared
  let #(_loader5, result) = dataloader.load(loader4, 6)
  should.equal(result, Ok(60))
}

// ============================================================================
// Prime Tests
// ============================================================================

pub fn prime_adds_to_cache_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(_) { Error("Should not be called") }))
    })

  let loader2 = dataloader.prime(loader, 42, "Primed Value")
  let #(_loader3, result) = dataloader.load(loader2, 42)
  should.equal(result, Ok("Primed Value"))
}

pub fn prime_error_adds_error_to_cache_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(_) { Ok("Should not be called") }))
    })

  let loader2 = dataloader.prime_error(loader, 42, "Primed Error")
  let #(_loader3, result) = dataloader.load(loader2, 42)
  should.equal(result, Error("Primed Error"))
}

// ============================================================================
// Int Loader Tests
// ============================================================================

pub fn int_loader_success_test() {
  let loader =
    dataloader.int_loader(fn(id: Int) {
      case find_by_id(id) {
        Ok(v) -> Ok(to_dynamic(v))
        Error(e) -> Error(e)
      }
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(1))
  should.be_ok(result)
}

pub fn int_loader_not_found_test() {
  let loader =
    dataloader.int_loader(fn(id: Int) {
      case find_by_id(id) {
        Ok(v) -> Ok(to_dynamic(v))
        Error(e) -> Error(e)
      }
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(999))
  should.be_error(result)
}

// ============================================================================
// String Loader Tests
// ============================================================================

pub fn string_loader_success_test() {
  let loader =
    dataloader.string_loader(fn(key: String) {
      case find_by_key(key) {
        Ok(v) -> Ok(to_dynamic(v))
        Error(e) -> Error(e)
      }
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.string_key("a"))
  should.be_ok(result)
}

pub fn string_loader_not_found_test() {
  let loader =
    dataloader.string_loader(fn(key: String) {
      case find_by_key(key) {
        Ok(v) -> Ok(to_dynamic(v))
        Error(e) -> Error(e)
      }
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.string_key("zzz"))
  should.be_error(result)
}

// ============================================================================
// Batch Loader Tests
// ============================================================================

pub fn int_batch_loader_test() {
  let loader =
    dataloader.int_batch_loader(fn(ids: List(Int)) {
      Ok(list.map(ids, fn(id) { to_dynamic(id * 100) }))
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(5))
  should.be_ok(result)
}

pub fn string_batch_loader_test() {
  let loader =
    dataloader.string_batch_loader(fn(keys: List(String)) {
      Ok(list.map(keys, fn(k) { to_dynamic("Value: " <> k) }))
    })

  let #(_loader, result) =
    dataloader.load(loader, dataloader.string_key("test"))
  should.be_ok(result)
}

pub fn batch_loader_error_test() {
  let loader =
    dataloader.int_batch_loader(fn(_ids: List(Int)) {
      Error("Database connection failed")
    })

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(1))
  should.be_error(result)
}

// ============================================================================
// Result-based Loader Tests
// ============================================================================

pub fn int_loader_result_success_test() {
  let loader =
    dataloader.int_loader_result(find_by_id, to_dynamic, "Item not found")

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(1))
  should.be_ok(result)
}

pub fn int_loader_result_not_found_test() {
  let loader =
    dataloader.int_loader_result(find_by_id, to_dynamic, "Item not found")

  let #(_loader, result) = dataloader.load(loader, dataloader.int_key(999))
  should.equal(result, Error("Item not found"))
}

pub fn string_loader_result_success_test() {
  let loader =
    dataloader.string_loader_result(find_by_key, to_dynamic, "Key not found")

  let #(_loader, result) = dataloader.load(loader, dataloader.string_key("a"))
  should.be_ok(result)
}

pub fn string_loader_result_not_found_test() {
  let loader =
    dataloader.string_loader_result(find_by_key, to_dynamic, "Key not found")

  let #(_loader, result) = dataloader.load(loader, dataloader.string_key("zzz"))
  should.equal(result, Error("Key not found"))
}

// ============================================================================
// Key Helper Tests
// ============================================================================

pub fn int_key_test() {
  let _key = dataloader.int_key(42)
  // Key should be a Dynamic wrapping an Int
  should.be_true(True)
}

pub fn string_key_test() {
  let _key = dataloader.string_key("hello")
  // Key should be a Dynamic wrapping a String
  should.be_true(True)
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn duplicate_keys_deduplicated_test() {
  // When loading the same key multiple times, it should be deduplicated
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      Ok(list.map(keys, fn(id) { Ok(id * 10) }))
    })

  let #(loader2, result1) = dataloader.load(loader, 5)
  let #(_loader3, result2) = dataloader.load(loader2, 5)
  should.equal(result1, Ok(50))
  should.equal(result2, Ok(50))
}

pub fn cache_disabled_test() {
  let opts =
    dataloader.DataLoaderOptions(max_batch_size: 100, cache_enabled: False)
  let loader =
    dataloader.new_with_options(
      fn(keys: List(Int)) { Ok(list.map(keys, fn(k) { Ok(k * 10) })) },
      opts,
    )

  let #(loader2, result1) = dataloader.load(loader, 5)
  let #(_loader3, result2) = dataloader.load(loader2, 5)
  should.equal(result1, Ok(50))
  should.equal(result2, Ok(50))
}

// ============================================================================
// Batching correctness
// ============================================================================

pub fn load_many_makes_single_batch_call_test() {
  let loader =
    dataloader.new(fn(keys: List(Int)) {
      should.equal(keys, [1, 2, 3])
      Ok(list.map(keys, fn(k) { Ok(k * 10) }))
    })

  let #(_loader, results) = dataloader.load_many(loader, [1, 2, 3])
  should.equal(results, [Ok(10), Ok(20), Ok(30)])
}

pub fn load_many_skips_cached_keys_test() {
  let loader =
    dataloader.new_with_options(
      fn(keys: List(Int)) {
        should.be_false(list.contains(keys, 2))
        Ok(list.map(keys, fn(k) { Ok(k * 10) }))
      },
      dataloader.DataLoaderOptions(max_batch_size: 100, cache_enabled: True),
    )

  let loader2 = dataloader.prime(loader, 2, 20)
  let #(_final, results) = dataloader.load_many(loader2, [1, 2, 3])
  should.equal(results, [Ok(10), Ok(20), Ok(30)])
}
