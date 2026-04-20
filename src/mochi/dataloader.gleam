import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list

pub type BatchLoadFn(key, value) =
  fn(List(key)) -> Result(List(Result(value, String)), String)

pub type DataLoaderOptions {
  DataLoaderOptions(max_batch_size: Int, cache_enabled: Bool)
}

pub fn default_options() -> DataLoaderOptions {
  DataLoaderOptions(max_batch_size: 100, cache_enabled: True)
}

pub type DataLoaderState(key, value) {
  DataLoaderState(
    batch_load_fn: BatchLoadFn(key, value),
    options: DataLoaderOptions,
    cache: Dict(key, Result(value, String)),
  )
}

pub opaque type DataLoader(key, value) {
  DataLoader(state: DataLoaderState(key, value))
}

pub fn new(batch_load_fn: BatchLoadFn(key, value)) -> DataLoader(key, value) {
  new_with_options(batch_load_fn, default_options())
}

pub fn new_with_options(
  batch_load_fn: BatchLoadFn(key, value),
  options: DataLoaderOptions,
) -> DataLoader(key, value) {
  DataLoader(DataLoaderState(
    batch_load_fn: batch_load_fn,
    options: options,
    cache: dict.new(),
  ))
}

pub fn load(
  loader: DataLoader(key, value),
  key: key,
) -> #(DataLoader(key, value), Result(value, String)) {
  let DataLoader(state) = loader
  case state.options.cache_enabled, dict.get(state.cache, key) {
    True, Ok(cached) -> #(loader, cached)
    _, _ -> fetch_single(loader, key)
  }
}

pub fn load_many(
  loader: DataLoader(key, value),
  keys: List(key),
) -> #(DataLoader(key, value), List(Result(value, String))) {
  case keys {
    [] -> #(loader, [])
    _ -> {
      let DataLoader(state) = loader

      let #(result_map, uncached) =
        list.fold(keys, #(dict.new(), []), fn(acc, key) {
          let #(known, to_fetch) = acc
          case state.options.cache_enabled, dict.get(state.cache, key) {
            True, Ok(r) -> #(dict.insert(known, key, r), to_fetch)
            _, _ -> #(known, [key, ..to_fetch])
          }
        })

      let unique_uncached = list.unique(list.reverse(uncached))

      case unique_uncached {
        [] -> {
          let results =
            list.map(keys, fn(k) {
              case dict.get(result_map, k) {
                Ok(r) -> r
                Error(_) -> Error("Key not found")
              }
            })
          #(loader, results)
        }
        _ ->
          case state.batch_load_fn(unique_uncached) {
            Error(e) -> {
              let err = Error("Batch load failed: " <> e)
              let results =
                list.map(keys, fn(k) {
                  case dict.get(result_map, k) {
                    Ok(r) -> r
                    Error(_) -> err
                  }
                })
              #(loader, results)
            }
            Ok(batch_results) ->
              case list.length(batch_results) == list.length(unique_uncached) {
                False -> #(
                  loader,
                  list.repeat(
                    Error("Batch returned wrong number of results"),
                    list.length(keys),
                  ),
                )
                True -> {
                  let fresh =
                    dict.from_list(list.zip(unique_uncached, batch_results))
                  let new_cache = case state.options.cache_enabled {
                    True -> dict.merge(state.cache, fresh)
                    False -> state.cache
                  }
                  let new_loader =
                    DataLoader(DataLoaderState(..state, cache: new_cache))
                  let all_results = dict.merge(result_map, fresh)
                  let results =
                    list.map(keys, fn(k) {
                      case dict.get(all_results, k) {
                        Ok(r) -> r
                        Error(_) -> Error("Key not found in batch results")
                      }
                    })
                  #(new_loader, results)
                }
              }
          }
      }
    }
  }
}

pub fn clear_cache(loader: DataLoader(key, value)) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  DataLoader(DataLoaderState(..state, cache: dict.new()))
}

pub fn clear_key(
  loader: DataLoader(key, value),
  key: key,
) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  DataLoader(DataLoaderState(..state, cache: dict.delete(state.cache, key)))
}

pub fn prime(
  loader: DataLoader(key, value),
  key: key,
  value: value,
) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  case state.options.cache_enabled {
    True ->
      DataLoader(
        DataLoaderState(
          ..state,
          cache: dict.insert(state.cache, key, Ok(value)),
        ),
      )
    False -> loader
  }
}

pub fn prime_error(
  loader: DataLoader(key, value),
  key: key,
  error: String,
) -> DataLoader(key, value) {
  let DataLoader(state) = loader
  case state.options.cache_enabled {
    True ->
      DataLoader(
        DataLoaderState(
          ..state,
          cache: dict.insert(state.cache, key, Error(error)),
        ),
      )
    False -> loader
  }
}

fn fetch_single(
  loader: DataLoader(key, value),
  key: key,
) -> #(DataLoader(key, value), Result(value, String)) {
  let DataLoader(state) = loader
  case state.batch_load_fn([key]) {
    Ok([result]) -> {
      let new_cache = case state.options.cache_enabled {
        True -> dict.insert(state.cache, key, result)
        False -> state.cache
      }
      #(DataLoader(DataLoaderState(..state, cache: new_cache)), result)
    }
    Ok(_) -> #(loader, Error("Batch returned wrong number of results"))
    Error(e) -> #(loader, Error("Batch load failed: " <> e))
  }
}

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

pub fn int_key(id: Int) -> Dynamic {
  to_dynamic(id)
}

pub fn string_key(s: String) -> Dynamic {
  to_dynamic(s)
}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic
