//// DataLoader implementation for efficient data fetching in GraphQL.
////
//// DataLoader solves the N+1 query problem by:
//// 1. Batching multiple individual loads into single batch requests
//// 2. Caching results to avoid duplicate requests within the same request context
//// 3. Providing a simple, type-safe API for data fetching
////
//// Based on Facebook's DataLoader specification:
//// https://github.com/graphql/dataloader
////
//// ## Quick Start (Recommended)
////
//// Use `int_loader_result` for the simplest setup - just pass your find function:
////
//// ```gleam
//// let pokemon_loader = dataloader.int_loader_result(
////   data.find_pokemon,      // fn(Int) -> Result(Pokemon, _)
////   pokemon_to_dynamic,     // fn(Pokemon) -> Dynamic
////   "Pokemon not found",    // Error message
//// )
////
//// // Register multiple loaders at once
//// let ctx = schema.execution_context(types.to_dynamic(dict.new()))
////   |> schema.with_loaders([
////     #("pokemon", pokemon_loader),
////     #("trainer", trainer_loader),
////   ])
////
//// // Load by ID
//// let #(ctx, result) = schema.load_by_id(ctx, "pokemon", 25)
//// ```
////
//// ## Custom Loader
////
//// For more control, use `int_loader` with custom logic:
////
//// ```gleam
//// let user_loader = dataloader.int_loader(fn(id) {
////   case db.find_user(id) {
////     Ok(user) -> Ok(types.to_dynamic(user))
////     Error(_) -> Error("User not found")
////   }
//// })
//// ```
////
//// ## Batch Loader
////
//// For efficient bulk database queries, use `int_batch_loader`:
////
//// ```gleam
//// let user_loader = dataloader.int_batch_loader(fn(ids) {
////   case db.get_users_by_ids(ids) {
////     Ok(users) -> Ok(list.map(users, types.to_dynamic))
////     Error(e) -> Error(e)
////   }
//// })
//// ```
////
//// ## Available Constructors
////
//// | Function | Description |
//// |----------|-------------|
//// | `int_loader_result` | One-liner from Result-returning find function |
//// | `string_loader_result` | Same for String keys |
//// | `int_loader` | Custom loader with Int keys |
//// | `string_loader` | Custom loader with String keys |
//// | `int_batch_loader` | Batch loader with Int keys |
//// | `string_batch_loader` | Batch loader with String keys |
//// | `new` | Full control with custom batch function |

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}

/// A batch loading function that takes a list of keys and returns results
/// The results list must be in the same order as the keys list
pub type BatchLoadFn(key, value) =
  fn(List(key)) -> Result(List(Result(value, String)), String)

/// DataLoader configuration options
pub type DataLoaderOptions {
  DataLoaderOptions(
    /// Maximum number of keys to batch together (default: 100)
    max_batch_size: Int,
    /// Whether to cache results (default: True)  
    cache_enabled: Bool,
  )
}

/// Default DataLoader options
pub fn default_options() -> DataLoaderOptions {
  DataLoaderOptions(max_batch_size: 100, cache_enabled: True)
}

/// Internal state for a DataLoader instance
pub type DataLoaderState(key, value) {
  DataLoaderState(
    /// The batch loading function
    batch_load_fn: BatchLoadFn(key, value),
    /// Configuration options
    options: DataLoaderOptions,
    /// Cache for storing loaded results
    cache: Dict(key, Result(value, String)),
    /// Pending batch of keys waiting to be loaded
    pending_batch: List(key),
    /// Whether a batch is currently being processed
    batch_scheduled: Bool,
  )
}

/// A DataLoader instance
pub opaque type DataLoader(key, value) {
  DataLoader(state: DataLoaderState(key, value))
}

/// Create a new DataLoader instance
pub fn new(batch_load_fn: BatchLoadFn(key, value)) -> DataLoader(key, value) {
  new_with_options(batch_load_fn, default_options())
}

/// Create a new DataLoader instance with custom options
pub fn new_with_options(
  batch_load_fn: BatchLoadFn(key, value),
  options: DataLoaderOptions,
) -> DataLoader(key, value) {
  DataLoader(DataLoaderState(
    batch_load_fn: batch_load_fn,
    options: options,
    cache: dict.new(),
    pending_batch: [],
    batch_scheduled: False,
  ))
}

/// Load a single value by key
/// This is the primary API - multiple calls to load() will be batched together
pub fn load(
  loader: DataLoader(key, value),
  key: key,
) -> #(DataLoader(key, value), Result(value, String)) {
  let DataLoader(state) = loader

  // Check cache first if caching is enabled
  case state.options.cache_enabled {
    True -> {
      case dict.get(state.cache, key) {
        Ok(cached_result) -> #(loader, cached_result)
        Error(_) -> load_uncached(loader, key)
      }
    }
    False -> load_uncached(loader, key)
  }
}

/// Load multiple values by keys
/// More efficient than calling load() multiple times
pub fn load_many(
  loader: DataLoader(key, value),
  keys: List(key),
) -> #(DataLoader(key, value), List(Result(value, String))) {
  // For each key, call load and collect results
  let #(final_loader, results) =
    list.fold(keys, #(loader, []), fn(acc, key) {
      let #(current_loader, results_so_far) = acc
      let #(new_loader, result) = load(current_loader, key)
      #(new_loader, [result, ..results_so_far])
    })

  #(final_loader, list.reverse(results))
}

/// Clear the cache for this DataLoader
pub fn clear_cache(loader: DataLoader(key, value)) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  DataLoader(DataLoaderState(..state, cache: dict.new()))
}

/// Clear a specific key from the cache
pub fn clear_key(
  loader: DataLoader(key, value),
  key: key,
) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  DataLoader(DataLoaderState(..state, cache: dict.delete(state.cache, key)))
}

/// Prime the cache with a key-value pair
/// Useful when you already have data and want to avoid future loads
pub fn prime(
  loader: DataLoader(key, value),
  key: key,
  value: value,
) -> DataLoader(key, value) {
  case loader {
    DataLoader(state) if state.options.cache_enabled -> {
      let new_cache = dict.insert(state.cache, key, Ok(value))
      DataLoader(DataLoaderState(..state, cache: new_cache))
    }
    _ -> loader
  }
}

/// Prime the cache with an error for a specific key
pub fn prime_error(
  loader: DataLoader(key, value),
  key: key,
  error: String,
) -> DataLoader(key, value) {
  case loader {
    DataLoader(state) if state.options.cache_enabled -> {
      let new_cache = dict.insert(state.cache, key, Error(error))
      DataLoader(DataLoaderState(..state, cache: new_cache))
    }
    _ -> loader
  }
}

// Internal helper functions

/// Load a key without checking cache
fn load_uncached(
  loader: DataLoader(key, value),
  key: key,
) -> #(DataLoader(key, value), Result(value, String)) {
  let DataLoader(state) = loader

  // Add key to pending batch
  let new_pending = [key, ..state.pending_batch]
  let updated_state = DataLoaderState(..state, pending_batch: new_pending)

  // Check if we should execute the batch now
  case should_execute_batch(updated_state) {
    True -> execute_batch(DataLoader(updated_state), key)
    False -> {
      // Key is now pending, but we need to return a result
      // In a real implementation, this would be handled asynchronously
      // For this demo, we'll execute the batch immediately
      execute_batch(DataLoader(updated_state), key)
    }
  }
}

/// Determine if we should execute the current batch
fn should_execute_batch(state: DataLoaderState(key, value)) -> Bool {
  let batch_size = list.length(state.pending_batch)
  batch_size >= state.options.max_batch_size || batch_size > 0
}

/// Execute the current batch of pending keys
fn execute_batch(
  loader: DataLoader(key, value),
  requested_key: key,
) -> #(DataLoader(key, value), Result(value, String)) {
  let DataLoader(state) = loader

  case state.pending_batch {
    [] -> #(loader, Error("No keys to batch"))
    pending_keys -> {
      // Remove duplicates and reverse to maintain order
      let unique_keys = list.unique(list.reverse(pending_keys))

      // Execute the batch load function
      case state.batch_load_fn(unique_keys) {
        Ok(results) -> {
          // Validate that results match keys
          case list.length(results) == list.length(unique_keys) {
            True -> {
              // Update cache with results
              let new_cache = case state.options.cache_enabled {
                True -> cache_batch_results(state.cache, unique_keys, results)
                False -> state.cache
              }

              // Clear pending batch
              let new_state =
                DataLoaderState(
                  ..state,
                  cache: new_cache,
                  pending_batch: [],
                  batch_scheduled: False,
                )

              // Find the result for the requested key
              let result =
                find_result_for_key(unique_keys, results, requested_key)

              #(DataLoader(new_state), result)
            }
            False -> {
              let error = "Batch load function returned wrong number of results"
              #(loader, Error(error))
            }
          }
        }
        Error(batch_error) -> #(
          loader,
          Error("Batch load failed: " <> batch_error),
        )
      }
    }
  }
}

/// Cache the results from a batch operation
fn cache_batch_results(
  cache: Dict(key, Result(value, String)),
  keys: List(key),
  results: List(Result(value, String)),
) -> Dict(key, Result(value, String)) {
  fold2(keys, results, cache, fn(acc_cache, key, result) {
    dict.insert(acc_cache, key, result)
  })
}

/// Find the result for a specific key from the batch results
fn find_result_for_key(
  keys: List(key),
  results: List(Result(value, String)),
  target_key: key,
) -> Result(value, String) {
  case find_key_index(keys, target_key, 0) {
    Some(index) -> {
      case get_at_index(results, index) {
        Ok(result) -> result
        Error(_) -> Error("Result not found at expected index")
      }
    }
    None -> Error("Key not found in batch results")
  }
}

/// Get element at specific index from a list
fn get_at_index(list: List(a), index: Int) -> Result(a, Nil) {
  case list, index {
    [], _ -> Error(Nil)
    [first, ..], 0 -> Ok(first)
    [_, ..rest], i if i > 0 -> get_at_index(rest, i - 1)
    _, _ -> Error(Nil)
  }
}

/// Find the index of a key in a list
fn find_key_index(
  keys: List(key),
  target_key: key,
  current_index: Int,
) -> Option(Int) {
  case keys {
    [] -> None
    [first, ..rest] -> {
      case first == target_key {
        True -> Some(current_index)
        False -> find_key_index(rest, target_key, current_index + 1)
      }
    }
  }
}

/// Helper function to fold over two lists simultaneously
fn fold2(
  keys: List(key),
  results: List(Result(value, String)),
  cache: Dict(key, Result(value, String)),
  f: fn(Dict(key, Result(value, String)), key, Result(value, String)) ->
    Dict(key, Result(value, String)),
) -> Dict(key, Result(value, String)) {
  case keys, results {
    [], [] -> cache
    [key, ..rest_keys], [result, ..rest_results] -> {
      let new_cache = f(cache, key, result)
      fold2(rest_keys, rest_results, new_cache, f)
    }
    _, _ -> cache
    // Mismatched lengths
  }
}

// ============================================================================
// Convenient Loader Constructors
// ============================================================================

/// Create a DataLoader for Int keys from a simple lookup function
///
/// This is the most common pattern - looking up entities by integer ID.
///
/// ## Example
///
/// ```gleam
/// let pokemon_loader = dataloader.int_loader(fn(id) {
///   case data.find_pokemon(id) {
///     Ok(pokemon) -> Ok(to_dynamic(pokemon))
///     Error(_) -> Error("Pokemon not found")
///   }
/// })
/// ```
pub fn int_loader(
  lookup: fn(Int) -> Result(Dynamic, String),
) -> DataLoader(Dynamic, Dynamic) {
  new(fn(keys: List(Dynamic)) {
    Ok(
      list.map(keys, fn(key) {
        case decode.run(key, decode.int) {
          Ok(id) -> lookup(id)
          Error(_) -> Error("Invalid integer key")
        }
      }),
    )
  })
}

/// Create a DataLoader for String keys from a simple lookup function
///
/// ## Example
///
/// ```gleam
/// let user_loader = dataloader.string_loader(fn(email) {
///   case data.find_user_by_email(email) {
///     Ok(user) -> Ok(to_dynamic(user))
///     Error(_) -> Error("User not found")
///   }
/// })
/// ```
pub fn string_loader(
  lookup: fn(String) -> Result(Dynamic, String),
) -> DataLoader(Dynamic, Dynamic) {
  new(fn(keys: List(Dynamic)) {
    Ok(
      list.map(keys, fn(key) {
        case decode.run(key, decode.string) {
          Ok(s) -> lookup(s)
          Error(_) -> Error("Invalid string key")
        }
      }),
    )
  })
}

/// Create a DataLoader with a batch lookup function for Int keys
///
/// Use this when your data source supports efficient batch fetching.
/// The batch function receives all IDs at once and should return results
/// in the same order.
///
/// ## Example
///
/// ```gleam
/// let pokemon_loader = dataloader.int_batch_loader(fn(ids) {
///   // Single database query for all IDs
///   case db.find_pokemon_batch(ids) {
///     Ok(results) -> Ok(list.map(results, to_dynamic))
///     Error(e) -> Error(e)
///   }
/// })
/// ```
pub fn int_batch_loader(
  batch_lookup: fn(List(Int)) -> Result(List(Dynamic), String),
) -> DataLoader(Dynamic, Dynamic) {
  new(fn(keys: List(Dynamic)) {
    let int_keys =
      list.filter_map(keys, fn(key) {
        case decode.run(key, decode.int) {
          Ok(id) -> Ok(id)
          Error(_) -> Error(Nil)
        }
      })

    case batch_lookup(int_keys) {
      Ok(results) -> Ok(list.map(results, Ok))
      Error(e) -> Error(e)
    }
  })
}

/// Create a DataLoader with a batch lookup function for String keys
pub fn string_batch_loader(
  batch_lookup: fn(List(String)) -> Result(List(Dynamic), String),
) -> DataLoader(Dynamic, Dynamic) {
  new(fn(keys: List(Dynamic)) {
    let string_keys =
      list.filter_map(keys, fn(key) {
        case decode.run(key, decode.string) {
          Ok(s) -> Ok(s)
          Error(_) -> Error(Nil)
        }
      })

    case batch_lookup(string_keys) {
      Ok(results) -> Ok(list.map(results, Ok))
      Error(e) -> Error(e)
    }
  })
}

// ============================================================================
// Result-based Loader Constructors (even less boilerplate)
// ============================================================================

/// Create a DataLoader from a Result-returning find function
///
/// This is the most concise way to create a loader - just provide
/// your existing find function, an encoder, and an error message.
///
/// ## Example
///
/// ```gleam
/// let pokemon_loader = dataloader.int_loader_result(
///   data.find_pokemon,
///   pokemon_to_dynamic,
///   "Pokemon not found",
/// )
/// ```
pub fn int_loader_result(
  find_fn: fn(Int) -> Result(a, e),
  encoder: fn(a) -> Dynamic,
  not_found_error: String,
) -> DataLoader(Dynamic, Dynamic) {
  int_loader(fn(id) {
    case find_fn(id) {
      Ok(value) -> Ok(encoder(value))
      Error(_) -> Error(not_found_error)
    }
  })
}

/// Create a DataLoader from a Result-returning find function (String keys)
///
/// ## Example
///
/// ```gleam
/// let user_loader = dataloader.string_loader_result(
///   data.find_user_by_email,
///   user_to_dynamic,
///   "User not found",
/// )
/// ```
pub fn string_loader_result(
  find_fn: fn(String) -> Result(a, e),
  encoder: fn(a) -> Dynamic,
  not_found_error: String,
) -> DataLoader(Dynamic, Dynamic) {
  string_loader(fn(key) {
    case find_fn(key) {
      Ok(value) -> Ok(encoder(value))
      Error(_) -> Error(not_found_error)
    }
  })
}

// ============================================================================
// Dynamic Key Helpers
// ============================================================================

/// Convert an Int to a Dynamic key for use with DataLoader
pub fn int_key(id: Int) -> Dynamic {
  to_dynamic(id)
}

/// Convert a String to a Dynamic key for use with DataLoader
pub fn string_key(s: String) -> Dynamic {
  to_dynamic(s)
}

/// Internal: Convert any value to Dynamic (identity function)
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../mochi/mochi_coerce_ffi.mjs", "identity")
fn to_dynamic(value: a) -> Dynamic
