import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

export class DataLoaderOptions extends $CustomType {
  constructor(max_batch_size, cache_enabled) {
    super();
    this.max_batch_size = max_batch_size;
    this.cache_enabled = cache_enabled;
  }
}
export const DataLoaderOptions$DataLoaderOptions = (max_batch_size, cache_enabled) =>
  new DataLoaderOptions(max_batch_size, cache_enabled);
export const DataLoaderOptions$isDataLoaderOptions = (value) =>
  value instanceof DataLoaderOptions;
export const DataLoaderOptions$DataLoaderOptions$max_batch_size = (value) =>
  value.max_batch_size;
export const DataLoaderOptions$DataLoaderOptions$0 = (value) =>
  value.max_batch_size;
export const DataLoaderOptions$DataLoaderOptions$cache_enabled = (value) =>
  value.cache_enabled;
export const DataLoaderOptions$DataLoaderOptions$1 = (value) =>
  value.cache_enabled;

export class DataLoaderState extends $CustomType {
  constructor(batch_load_fn, options, cache, pending_batch, batch_scheduled) {
    super();
    this.batch_load_fn = batch_load_fn;
    this.options = options;
    this.cache = cache;
    this.pending_batch = pending_batch;
    this.batch_scheduled = batch_scheduled;
  }
}
export const DataLoaderState$DataLoaderState = (batch_load_fn, options, cache, pending_batch, batch_scheduled) =>
  new DataLoaderState(batch_load_fn,
  options,
  cache,
  pending_batch,
  batch_scheduled);
export const DataLoaderState$isDataLoaderState = (value) =>
  value instanceof DataLoaderState;
export const DataLoaderState$DataLoaderState$batch_load_fn = (value) =>
  value.batch_load_fn;
export const DataLoaderState$DataLoaderState$0 = (value) => value.batch_load_fn;
export const DataLoaderState$DataLoaderState$options = (value) => value.options;
export const DataLoaderState$DataLoaderState$1 = (value) => value.options;
export const DataLoaderState$DataLoaderState$cache = (value) => value.cache;
export const DataLoaderState$DataLoaderState$2 = (value) => value.cache;
export const DataLoaderState$DataLoaderState$pending_batch = (value) =>
  value.pending_batch;
export const DataLoaderState$DataLoaderState$3 = (value) => value.pending_batch;
export const DataLoaderState$DataLoaderState$batch_scheduled = (value) =>
  value.batch_scheduled;
export const DataLoaderState$DataLoaderState$4 = (value) =>
  value.batch_scheduled;

class DataLoader extends $CustomType {
  constructor(state) {
    super();
    this.state = state;
  }
}

/**
 * Default DataLoader options
 */
export function default_options() {
  return new DataLoaderOptions(100, true);
}

/**
 * Create a new DataLoader instance with custom options
 */
export function new_with_options(batch_load_fn, options) {
  return new DataLoader(
    new DataLoaderState(batch_load_fn, options, $dict.new$(), toList([]), false),
  );
}

/**
 * Create a new DataLoader instance
 */
export function new$(batch_load_fn) {
  return new_with_options(batch_load_fn, default_options());
}

/**
 * Clear the cache for this DataLoader
 */
export function clear_cache(loader) {
  let state;
  state = loader.state;
  return new DataLoader(
    new DataLoaderState(
      state.batch_load_fn,
      state.options,
      $dict.new$(),
      state.pending_batch,
      state.batch_scheduled,
    ),
  );
}

/**
 * Clear a specific key from the cache
 */
export function clear_key(loader, key) {
  let state;
  state = loader.state;
  return new DataLoader(
    new DataLoaderState(
      state.batch_load_fn,
      state.options,
      $dict.delete$(state.cache, key),
      state.pending_batch,
      state.batch_scheduled,
    ),
  );
}

/**
 * Prime the cache with a key-value pair
 * Useful when you already have data and want to avoid future loads
 */
export function prime(loader, key, value) {
  let state = loader.state;
  if (state.options.cache_enabled) {
    let new_cache = $dict.insert(state.cache, key, new Ok(value));
    return new DataLoader(
      new DataLoaderState(
        state.batch_load_fn,
        state.options,
        new_cache,
        state.pending_batch,
        state.batch_scheduled,
      ),
    );
  } else {
    return loader;
  }
}

/**
 * Prime the cache with an error for a specific key
 */
export function prime_error(loader, key, error) {
  let state = loader.state;
  if (state.options.cache_enabled) {
    let new_cache = $dict.insert(state.cache, key, new Error(error));
    return new DataLoader(
      new DataLoaderState(
        state.batch_load_fn,
        state.options,
        new_cache,
        state.pending_batch,
        state.batch_scheduled,
      ),
    );
  } else {
    return loader;
  }
}

/**
 * Determine if we should execute the current batch
 * 
 * @ignore
 */
function should_execute_batch(state) {
  let batch_size = $list.length(state.pending_batch);
  return (batch_size >= state.options.max_batch_size) || (batch_size > 0);
}

/**
 * Get element at specific index from a list
 * 
 * @ignore
 */
function get_at_index(loop$list, loop$index) {
  while (true) {
    let list = loop$list;
    let index = loop$index;
    if (list instanceof $Empty) {
      return new Error(undefined);
    } else if (index === 0) {
      let first = list.head;
      return new Ok(first);
    } else {
      let i = index;
      if (i > 0) {
        let rest = list.tail;
        loop$list = rest;
        loop$index = i - 1;
      } else {
        return new Error(undefined);
      }
    }
  }
}

/**
 * Find the index of a key in a list
 * 
 * @ignore
 */
function find_key_index(loop$keys, loop$target_key, loop$current_index) {
  while (true) {
    let keys = loop$keys;
    let target_key = loop$target_key;
    let current_index = loop$current_index;
    if (keys instanceof $Empty) {
      return new None();
    } else {
      let first = keys.head;
      let rest = keys.tail;
      let $ = isEqual(first, target_key);
      if ($) {
        return new Some(current_index);
      } else {
        loop$keys = rest;
        loop$target_key = target_key;
        loop$current_index = current_index + 1;
      }
    }
  }
}

/**
 * Find the result for a specific key from the batch results
 * 
 * @ignore
 */
function find_result_for_key(keys, results, target_key) {
  let $ = find_key_index(keys, target_key, 0);
  if ($ instanceof Some) {
    let index = $[0];
    let $1 = get_at_index(results, index);
    if ($1 instanceof Ok) {
      let result = $1[0];
      return result;
    } else {
      return new Error("Result not found at expected index");
    }
  } else {
    return new Error("Key not found in batch results");
  }
}

/**
 * Helper function to fold over two lists simultaneously
 * 
 * @ignore
 */
function fold2(loop$keys, loop$results, loop$cache, loop$f) {
  while (true) {
    let keys = loop$keys;
    let results = loop$results;
    let cache = loop$cache;
    let f = loop$f;
    if (keys instanceof $Empty) {
      if (results instanceof $Empty) {
        return cache;
      } else {
        return cache;
      }
    } else if (results instanceof $Empty) {
      return cache;
    } else {
      let key = keys.head;
      let rest_keys = keys.tail;
      let result = results.head;
      let rest_results = results.tail;
      let new_cache = f(cache, key, result);
      loop$keys = rest_keys;
      loop$results = rest_results;
      loop$cache = new_cache;
      loop$f = f;
    }
  }
}

/**
 * Cache the results from a batch operation
 * 
 * @ignore
 */
function cache_batch_results(cache, keys, results) {
  return fold2(
    keys,
    results,
    cache,
    (acc_cache, key, result) => { return $dict.insert(acc_cache, key, result); },
  );
}

/**
 * Execute the current batch of pending keys
 * 
 * @ignore
 */
function execute_batch(loader, requested_key) {
  let state;
  state = loader.state;
  let $ = state.pending_batch;
  if ($ instanceof $Empty) {
    return [loader, new Error("No keys to batch")];
  } else {
    let pending_keys = $;
    let unique_keys = $list.unique($list.reverse(pending_keys));
    let $1 = state.batch_load_fn(unique_keys);
    if ($1 instanceof Ok) {
      let results = $1[0];
      let $2 = $list.length(results) === $list.length(unique_keys);
      if ($2) {
        let _block;
        let $3 = state.options.cache_enabled;
        if ($3) {
          _block = cache_batch_results(state.cache, unique_keys, results);
        } else {
          _block = state.cache;
        }
        let new_cache = _block;
        let new_state = new DataLoaderState(
          state.batch_load_fn,
          state.options,
          new_cache,
          toList([]),
          false,
        );
        let result = find_result_for_key(unique_keys, results, requested_key);
        return [new DataLoader(new_state), result];
      } else {
        let error = "Batch load function returned wrong number of results";
        return [loader, new Error(error)];
      }
    } else {
      let batch_error = $1[0];
      return [loader, new Error("Batch load failed: " + batch_error)];
    }
  }
}

/**
 * Load a key without checking cache
 * 
 * @ignore
 */
function load_uncached(loader, key) {
  let state;
  state = loader.state;
  let new_pending = listPrepend(key, state.pending_batch);
  let updated_state = new DataLoaderState(
    state.batch_load_fn,
    state.options,
    state.cache,
    new_pending,
    state.batch_scheduled,
  );
  let $ = should_execute_batch(updated_state);
  if ($) {
    return execute_batch(new DataLoader(updated_state), key);
  } else {
    return execute_batch(new DataLoader(updated_state), key);
  }
}

/**
 * Load a single value by key
 * This is the primary API - multiple calls to load() will be batched together
 */
export function load(loader, key) {
  let state;
  state = loader.state;
  let $ = state.options.cache_enabled;
  if ($) {
    let $1 = $dict.get(state.cache, key);
    if ($1 instanceof Ok) {
      let cached_result = $1[0];
      return [loader, cached_result];
    } else {
      return load_uncached(loader, key);
    }
  } else {
    return load_uncached(loader, key);
  }
}

/**
 * Load multiple values by keys
 * More efficient than calling load() multiple times
 */
export function load_many(loader, keys) {
  let $ = $list.fold(
    keys,
    [loader, toList([])],
    (acc, key) => {
      let current_loader;
      let results_so_far;
      current_loader = acc[0];
      results_so_far = acc[1];
      let $1 = load(current_loader, key);
      let new_loader;
      let result;
      new_loader = $1[0];
      result = $1[1];
      return [new_loader, listPrepend(result, results_so_far)];
    },
  );
  let final_loader;
  let results;
  final_loader = $[0];
  results = $[1];
  return [final_loader, $list.reverse(results)];
}
