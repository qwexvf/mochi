import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import mochi/apq
import mochi/types

pub fn new_store_test() {
  apq.new()
  |> apq.size
  |> should.equal(0)
}

pub fn register_and_lookup_test() {
  let store = apq.new()
  let query = "{ hello }"
  let #(store, h) = apq.register(store, query)

  apq.size(store) |> should.equal(1)
  apq.lookup(store, h) |> should.equal(Some(query))
}

pub fn lookup_nonexistent_test() {
  apq.new()
  |> apq.lookup("nonexistent")
  |> should.equal(None)
}

pub fn with_queries_test() {
  apq.with_queries(["{ hello }", "{ world }"])
  |> apq.size
  |> should.equal(2)
}

pub fn register_same_query_twice_test() {
  let store = apq.new()
  let #(store, h1) = apq.register(store, "{ hello }")
  let #(store, h2) = apq.register(store, "{ hello }")
  should.equal(h1, h2)
  apq.size(store) |> should.equal(1)
}

pub fn hash_is_deterministic_test() {
  should.equal(apq.hash("{ hello }"), apq.hash("{ hello }"))
}

pub fn hash_differs_for_different_queries_test() {
  should.not_equal(apq.hash("{ hello }"), apq.hash("{ world }"))
}

pub fn hash_is_64_chars_test() {
  apq.hash("{ hello }")
  |> fn(h) { should.equal(string.length(h), 64) }
}

pub fn hash_normalises_whitespace_test() {
  should.equal(apq.hash("{ hello }"), apq.hash("  {  hello  }  "))
}

pub fn process_not_found_test() {
  apq.new()
  |> apq.process(None, "fakehash")
  |> should.equal(Error(apq.NotFound))
}

pub fn process_register_new_query_test() {
  let store = apq.new()
  let query = "{ hello }"
  let h = apq.hash(query)

  case apq.process(store, Some(query), h) {
    Ok(#(store2, returned)) -> {
      should.equal(returned, query)
      apq.size(store2) |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

pub fn process_retrieve_cached_test() {
  let store = apq.new()
  let query = "{ hello }"
  let #(store, h) = apq.register(store, query)

  case apq.process(store, None, h) {
    Ok(#(_, returned)) -> should.equal(returned, query)
    Error(_) -> should.fail()
  }
}

pub fn process_hash_mismatch_test() {
  let store = apq.new()
  let wrong = "0000000000000000000000000000000000000000000000000000000000000000"

  case apq.process(store, Some("{ hello }"), wrong) {
    Error(apq.HashMismatch(expected: e, actual: _)) -> should.equal(e, wrong)
    _ -> should.fail()
  }
}

pub fn parse_extension_present_test() {
  let ext =
    dict.from_list([
      #(
        "persistedQuery",
        types.to_dynamic(
          dict.from_list([
            #("version", types.to_dynamic(1)),
            #("sha256Hash", types.to_dynamic("abc123")),
          ]),
        ),
      ),
    ])

  case apq.parse_extension(ext) {
    Some(apq.Extension(version: v, sha256_hash: h)) -> {
      should.equal(v, 1)
      should.equal(h, "abc123")
    }
    None -> should.fail()
  }
}

pub fn parse_extension_absent_test() {
  apq.parse_extension(dict.new())
  |> should.equal(None)
}

import gleam/string
