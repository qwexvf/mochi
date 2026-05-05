//// Microbenchmark for the hot string-parsing and JSON paths.
////
//// Not a regression test — runs only on `gleam test --target erlang` and
//// emits timing lines to stdout. Treat the numbers as ballpark; for hard
//// numbers use the Docker benchmark in `examples/mochi_wisp/benchmark/`.
////
//// Includes side-by-side `before/after` runs: the legacy implementations
//// (grapheme-based pretty printing, multi-decoder JSON encode, per-char
//// string.slice lexer scan) are reproduced inline and timed against the
//// current production code on identical inputs.

@target(erlang)
import gleam/dict
@target(erlang)
import gleam/dynamic.{type Dynamic}
@target(erlang)
import gleam/dynamic/decode
@target(erlang)
import gleam/int
@target(erlang)
import gleam/io
@target(erlang)
import gleam/json as gleam_json
@target(erlang)
import gleam/list
@target(erlang)
import gleam/string
@target(erlang)
import mochi/document_cache
@target(erlang)
import mochi/internal/lexer
@target(erlang)
import mochi/json
@target(erlang)
import mochi/parser
@target(erlang)
import mochi/types

@target(erlang)
@external(erlang, "erlang", "monotonic_time")
fn now_ns() -> Int

@target(erlang)
fn time_ns(label: String, iters: Int, body: fn() -> a) -> Nil {
  let start = now_ns()
  run_iters(iters, body)
  let total = now_ns() - start
  let per = total / iters
  io.println(
    "  "
    <> label
    <> "  iters="
    <> int.to_string(iters)
    <> "  total="
    <> fmt_ns(total)
    <> "  per_op="
    <> fmt_ns(per),
  )
}

@target(erlang)
fn run_iters(n: Int, body: fn() -> a) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let _ = body()
      run_iters(n - 1, body)
    }
  }
}

@target(erlang)
fn fmt_ns(ns: Int) -> String {
  case ns {
    n if n < 1000 -> int.to_string(n) <> "ns"
    n if n < 1_000_000 -> int.to_string(n / 1000) <> "us"
    n -> int.to_string(n / 1_000_000) <> "ms"
  }
}

@target(erlang)
const sample_query = "
query LargeBenchQuery($id: ID!, $limit: Int = 50, $cursor: String) {
  user(id: $id) {
    id
    name
    email
    createdAt
    profile {
      bio
      avatarUrl
      location
      website
      socialLinks {
        platform
        handle
        url
      }
    }
    posts(first: $limit, after: $cursor) {
      edges {
        node {
          id
          title
          slug
          excerpt
          content
          publishedAt
          tags
          comments(first: 10) {
            edges {
              node {
                id
                body
                createdAt
                author {
                  id
                  name
                }
              }
            }
          }
        }
        cursor
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
    followers(first: 25) {
      totalCount
      edges {
        node {
          id
          name
          avatarUrl
        }
      }
    }
  }
}
"

@target(erlang)
pub fn lexer_tokenize_bench_test() {
  io.println("\nlexer.tokenize")
  io.println(
    "  query bytes = " <> int.to_string(string.byte_size(sample_query)),
  )
  // warmup
  let _ = lexer.tokenize(sample_query)
  time_ns("tokenize     ", 1000, fn() { lexer.tokenize(sample_query) })
}

@target(erlang)
fn range_to(n: Int) -> List(Int) {
  range_to_loop(n - 1, [])
}

@target(erlang)
fn range_to_loop(i: Int, acc: List(Int)) -> List(Int) {
  case i < 0 {
    True -> acc
    False -> range_to_loop(i - 1, [i, ..acc])
  }
}

@target(erlang)
fn build_sample_response() -> dynamic.Dynamic {
  let comments =
    range_to(10)
    |> list.map(fn(i) {
      types.to_dynamic(
        dict.from_list([
          #("id", types.to_dynamic("c-" <> int.to_string(i))),
          #(
            "body",
            types.to_dynamic("This is comment number " <> int.to_string(i)),
          ),
          #(
            "author",
            types.to_dynamic(
              dict.from_list([
                #("id", types.to_dynamic("u-" <> int.to_string(i))),
                #("name", types.to_dynamic("User " <> int.to_string(i))),
              ]),
            ),
          ),
        ]),
      )
    })

  let posts =
    range_to(50)
    |> list.map(fn(i) {
      types.to_dynamic(
        dict.from_list([
          #("id", types.to_dynamic("p-" <> int.to_string(i))),
          #("title", types.to_dynamic("Post number " <> int.to_string(i))),
          #(
            "tags",
            types.to_dynamic(["tag1", "tag2", "tag-" <> int.to_string(i)]),
          ),
          #("comments", types.to_dynamic(comments)),
          #("publishedAt", types.to_dynamic("2025-01-01T00:00:00Z")),
        ]),
      )
    })

  types.to_dynamic(
    dict.from_list([
      #(
        "data",
        types.to_dynamic(
          dict.from_list([
            #(
              "user",
              types.to_dynamic(
                dict.from_list([
                  #("id", types.to_dynamic("u-1")),
                  #("name", types.to_dynamic("Alice")),
                  #("posts", types.to_dynamic(posts)),
                ]),
              ),
            ),
          ]),
        ),
      ),
    ]),
  )
}

@target(erlang)
pub fn json_encode_bench_test() {
  io.println("\njson.encode (compact, ~50 posts × 10 comments)")
  let payload = build_sample_response()
  // warmup + size print
  let assert Ok(out) = json.encode(payload)
  io.println("  output bytes = " <> int.to_string(string.byte_size(out)))
  time_ns("encode       ", 200, fn() { json.encode(payload) })
}

@target(erlang)
pub fn json_pretty_bench_test() {
  io.println("\njson.encode_pretty (same payload, indent=2)")
  let payload = build_sample_response()
  let assert Ok(out) = json.encode_pretty(payload, 2)
  io.println("  output bytes = " <> int.to_string(string.byte_size(out)))
  time_ns("encode_pretty", 200, fn() { json.encode_pretty(payload, 2) })
}

// ===========================================================================
// Legacy implementations — reproduced verbatim for before/after timing
// ===========================================================================

// --- old: dynamic-to-json with 6-decoder cascade, silent null fallback ---

@target(erlang)
fn old_dynamic_to_json(value: Dynamic) -> gleam_json.Json {
  case
    decode.run(value, decode.bool),
    decode.run(value, decode.int),
    decode.run(value, decode.float),
    decode.run(value, decode.string),
    decode.run(value, decode.list(decode.dynamic)),
    decode.run(value, decode.dict(decode.string, decode.dynamic))
  {
    Ok(b), _, _, _, _, _ -> gleam_json.bool(b)
    _, Ok(i), _, _, _, _ -> gleam_json.int(i)
    _, _, Ok(f), _, _, _ -> gleam_json.float(f)
    _, _, _, Ok(s), _, _ -> gleam_json.string(s)
    _, _, _, _, Ok(items), _ -> gleam_json.array(items, old_dynamic_to_json)
    _, _, _, _, _, Ok(d) ->
      dict.to_list(d)
      |> list.map(fn(kv) { #(kv.0, old_dynamic_to_json(kv.1)) })
      |> gleam_json.object
    _, _, _, _, _, _ -> gleam_json.null()
  }
}

@target(erlang)
fn old_encode(value: Dynamic) -> String {
  old_dynamic_to_json(value) |> gleam_json.to_string
}

// --- old: pretty printer that re-parses JSON output by graphemes ---

@target(erlang)
fn old_encode_pretty(value: Dynamic, indent: Int) -> String {
  let json_str = old_encode(value)
  old_format(json_str, indent)
}

@target(erlang)
fn old_format(s: String, indent: Int) -> String {
  old_do_format(s, 0, indent, "", False)
}

@target(erlang)
fn old_do_format(
  s: String,
  depth: Int,
  indent: Int,
  acc: String,
  in_string: Bool,
) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#(c, rest)) ->
      case in_string {
        True ->
          case c {
            "\"" -> old_do_format(rest, depth, indent, acc <> c, False)
            "\\" ->
              case string.pop_grapheme(rest) {
                Ok(#(next, rest2)) ->
                  old_do_format(rest2, depth, indent, acc <> c <> next, True)
                Error(_) -> acc <> c
              }
            _ -> old_do_format(rest, depth, indent, acc <> c, True)
          }
        False ->
          case c {
            "\"" -> old_do_format(rest, depth, indent, acc <> c, True)
            "{" ->
              case string.first(rest) {
                Ok("}") -> old_do_format(rest, depth, indent, acc <> "{", False)
                _ -> {
                  let d = depth + 1
                  old_do_format(
                    rest,
                    d,
                    indent,
                    acc <> "{\n" <> string.repeat(" ", d * indent),
                    False,
                  )
                }
              }
            "}" -> {
              let d = depth - 1
              old_do_format(
                rest,
                d,
                indent,
                acc <> "\n" <> string.repeat(" ", d * indent) <> "}",
                False,
              )
            }
            "[" ->
              case string.first(rest) {
                Ok("]") -> old_do_format(rest, depth, indent, acc <> "[", False)
                _ -> {
                  let d = depth + 1
                  old_do_format(
                    rest,
                    d,
                    indent,
                    acc <> "[\n" <> string.repeat(" ", d * indent),
                    False,
                  )
                }
              }
            "]" -> {
              let d = depth - 1
              old_do_format(
                rest,
                d,
                indent,
                acc <> "\n" <> string.repeat(" ", d * indent) <> "]",
                False,
              )
            }
            "," ->
              old_do_format(
                rest,
                depth,
                indent,
                acc <> ",\n" <> string.repeat(" ", depth * indent),
                False,
              )
            ":" -> old_do_format(rest, depth, indent, acc <> ": ", False)
            " " | "\t" | "\n" | "\r" ->
              old_do_format(rest, depth, indent, acc, False)
            _ -> old_do_format(rest, depth, indent, acc <> c, False)
          }
      }
  }
}

// --- old: lexer state with input + position, string.slice peek, <> accum ---
//
// Reproduces just the parts the benchmark exercises (skip whitespace, names,
// numbers, punctuation, strings). Enough for representative timing.

@target(erlang)
type OldState {
  OldState(input: String, position: Int, line: Int, column: Int)
}

@target(erlang)
type OldTok {
  OldEOF
  OldName(String)
  OldInt(Int)
  OldStr(String)
  OldPunct(String)
}

@target(erlang)
fn old_peek(s: OldState) -> Result(String, Nil) {
  case string.slice(s.input, s.position, 1) {
    "" -> Error(Nil)
    c -> Ok(c)
  }
}

@target(erlang)
fn old_advance(s: OldState) -> OldState {
  case old_peek(s) {
    Ok("\n") ->
      OldState(..s, position: s.position + 1, line: s.line + 1, column: 1)
    Ok(_) -> OldState(..s, position: s.position + 1, column: s.column + 1)
    Error(_) -> s
  }
}

@target(erlang)
fn old_skip_ws(s: OldState) -> OldState {
  case old_peek(s) {
    Ok(" ") | Ok("\t") | Ok("\n") | Ok("\r") | Ok(",") ->
      old_skip_ws(old_advance(s))
    _ -> s
  }
}

@target(erlang)
fn old_is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

@target(erlang)
fn old_is_name_continue(c: String) -> Bool {
  case c {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "_" -> True
    _ -> False
  }
}

@target(erlang)
fn old_read_while(
  s: OldState,
  pred: fn(String) -> Bool,
  acc: String,
) -> #(String, OldState) {
  case old_peek(s) {
    Ok(c) ->
      case pred(c) {
        True -> old_read_while(old_advance(s), pred, acc <> c)
        False -> #(acc, s)
      }
    _ -> #(acc, s)
  }
}

@target(erlang)
fn old_read_string(s: OldState, acc: String) -> #(OldTok, OldState) {
  case old_peek(s) {
    Ok("\"") -> #(OldStr(acc), old_advance(s))
    Ok("\\") -> {
      let s2 = old_advance(s)
      case old_peek(s2) {
        Ok(c) -> old_read_string(old_advance(s2), acc <> c)
        _ -> #(OldStr(acc), s2)
      }
    }
    Ok(c) -> old_read_string(old_advance(s), acc <> c)
    _ -> #(OldStr(acc), s)
  }
}

@target(erlang)
fn old_next(s: OldState) -> #(OldTok, OldState) {
  let s = old_skip_ws(s)
  case old_peek(s) {
    Error(_) -> #(OldEOF, s)
    Ok(c) ->
      case c {
        "{"
        | "}"
        | "("
        | ")"
        | "["
        | "]"
        | ":"
        | "!"
        | "="
        | "@"
        | "|"
        | "&"
        | "$" -> #(OldPunct(c), old_advance(s))
        "\"" -> old_read_string(old_advance(s), "")
        c
          if c == "0"
          || c == "1"
          || c == "2"
          || c == "3"
          || c == "4"
          || c == "5"
          || c == "6"
          || c == "7"
          || c == "8"
          || c == "9"
        -> {
          let #(num_str, s2) = old_read_while(s, old_is_digit, "")
          case int.parse(num_str) {
            Ok(n) -> #(OldInt(n), s2)
            Error(_) -> #(OldName(num_str), s2)
          }
        }
        c -> {
          case old_is_name_continue(c) {
            True -> {
              let #(name, s2) = old_read_while(s, old_is_name_continue, "")
              #(OldName(name), s2)
            }
            False -> #(OldPunct(c), old_advance(s))
          }
        }
      }
  }
}

@target(erlang)
fn old_tokenize(input: String) -> List(OldTok) {
  old_tokenize_loop(OldState(input, 0, 1, 1), [])
}

@target(erlang)
fn old_tokenize_loop(s: OldState, acc: List(OldTok)) -> List(OldTok) {
  case old_next(s) {
    #(OldEOF, _) -> list.reverse([OldEOF, ..acc])
    #(t, s2) -> old_tokenize_loop(s2, [t, ..acc])
  }
}

// ===========================================================================
// Side-by-side timings
// ===========================================================================

@target(erlang)
pub fn lexer_before_after_test() {
  io.println(
    "\nlexer.tokenize  before/after on "
    <> int.to_string(string.byte_size(sample_query))
    <> "-byte query",
  )
  let _ = old_tokenize(sample_query)
  let _ = lexer.tokenize(sample_query)
  time_ns("BEFORE  string.slice + <> ", 200, fn() { old_tokenize(sample_query) })
  time_ns("AFTER   BitArray match     ", 200, fn() {
    lexer.tokenize(sample_query)
  })
}

@target(erlang)
pub fn json_encode_before_after_test() {
  io.println("\njson.encode  before/after on ~50 posts × 10 comments")
  let payload = build_sample_response()
  let _ = old_encode(payload)
  let assert Ok(_) = json.encode(payload)
  time_ns("BEFORE  6-decoder cascade  ", 100, fn() { old_encode(payload) })
  time_ns("AFTER   typed Value walk   ", 100, fn() {
    let assert Ok(s) = json.encode(payload)
    s
  })
}

@target(erlang)
pub fn json_pretty_before_after_test() {
  io.println("\njson.encode_pretty  before/after (indent=2)")
  let payload = build_sample_response()
  let _ = old_encode_pretty(payload, 2)
  let assert Ok(_) = json.encode_pretty(payload, 2)
  time_ns("BEFORE  grapheme reparse   ", 50, fn() {
    old_encode_pretty(payload, 2)
  })
  time_ns("AFTER   structural walk    ", 50, fn() {
    let assert Ok(s) = json.encode_pretty(payload, 2)
    s
  })
}

// ===========================================================================
// Document cache vs re-parse — investigates the wrk benchmark's "cache slower"
// observation. After the lexer rewrite, parsing a small query is ~30 µs.
// An ets:lookup must copy the entire AST from the ets heap into the calling
// process, which can be similar cost or more for compact queries.
// ===========================================================================

@target(erlang)
const small_query = "{ users { id name email posts { id title } } }"

@target(erlang)
const big_query = "
query Big {
  users(limit: 100, offset: 0, orderBy: NAME, status: ACTIVE) {
    id name email phone address city country zip
    profile { bio avatar website company role joined }
    posts(first: 20) {
      edges {
        cursor
        node {
          id title slug excerpt content publishedAt updatedAt
          author { id name avatar }
          comments(first: 10) {
            edges {
              cursor
              node { id body createdAt author { id name } }
            }
          }
          tags { id name slug }
          reactions { likes loves laughs sads }
        }
      }
    }
    followers(first: 25) { edges { node { id name avatar } } }
    following(first: 25) { edges { node { id name avatar } } }
  }
}
"

@target(erlang)
pub fn cache_vs_parse_bench_test() {
  bench_one("small", small_query)
  bench_one("big  ", big_query)
}

@target(erlang)
fn bench_one(label: String, query: String) {
  io.println(
    "\ncache hit vs re-parse — "
    <> label
    <> " (" <> int.to_string(string.byte_size(query)) <> " bytes)",
  )
  let _ = parser.parse(query)

  let cache = document_cache.new()
  let assert Ok(doc) = parser.parse(query)
  document_cache.put(cache, query, doc)
  let _ = document_cache.get(cache, query)

  time_ns("  re-parse each request   ", 5000, fn() {
    let assert Ok(d) = parser.parse(query)
    d
  })

  time_ns("  ets cache hit each req  ", 5000, fn() {
    let assert Ok(d) = document_cache.get(cache, query)
    d
  })
}
