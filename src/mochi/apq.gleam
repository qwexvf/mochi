import gleam/bit_array
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Store {
  Store(queries: Dict(String, String))
}

pub type Extension {
  Extension(version: Int, sha256_hash: String)
}

pub type Error {
  NotFound
  HashMismatch(expected: String, actual: String)
}

pub fn new() -> Store {
  Store(queries: dict.new())
}

pub fn with_queries(queries: List(String)) -> Store {
  let query_dict =
    list.fold(queries, dict.new(), fn(acc, query) {
      dict.insert(acc, hash(query), query)
    })
  Store(queries: query_dict)
}

pub fn register(store: Store, query: String) -> #(Store, String) {
  let h = hash(query)
  #(Store(queries: dict.insert(store.queries, h, query)), h)
}

pub fn lookup(store: Store, h: String) -> Option(String) {
  dict.get(store.queries, h) |> option.from_result
}

pub fn size(store: Store) -> Int {
  dict.size(store.queries)
}

pub fn process(
  store: Store,
  query: Option(String),
  h: String,
) -> Result(#(Store, String), Error) {
  case lookup(store, h) {
    Some(stored) -> Ok(#(store, stored))
    None ->
      case query {
        None -> Error(NotFound)
        Some(q) -> {
          let actual = hash(q)
          case actual == h {
            True -> {
              let #(new_store, _) = register(store, q)
              Ok(#(new_store, q))
            }
            False -> Error(HashMismatch(expected: h, actual: actual))
          }
        }
      }
  }
}

pub fn parse_extension(
  extensions: Dict(String, dynamic.Dynamic),
) -> Option(Extension) {
  case dict.get(extensions, "persistedQuery") {
    Error(_) -> None
    Ok(pq) ->
      case
        decode.run(
          pq,
          decode.field("version", decode.int, fn(version) {
            decode.field("sha256Hash", decode.string, fn(h) {
              decode.success(#(version, h))
            })
          }),
        )
      {
        Ok(#(version, h)) -> Some(Extension(version: version, sha256_hash: h))
        Error(_) -> None
      }
  }
}

pub fn hash(query: String) -> String {
  query
  |> string.trim
  |> normalise_whitespace
  |> bit_array.from_string
  |> crypto.hash(crypto.Sha256, _)
  |> bit_array.base16_encode
  |> string.lowercase
}

fn normalise_whitespace(s: String) -> String {
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
