import gleam/int
import gleam/io
import gleam/list
import gleam/string
import mochi/document_cache
import mochi/executor
import mochi/parser
import mochi/schema
import mochi/types

@external(erlang, "mochi_time_ffi", "monotonic_time_ns")
fn now_ns() -> Int

pub fn main() {
  io.println("mochi Performance Benchmark")
  io.println(string.repeat("=", 44))
  io.println("")

  let schema_def = create_benchmark_schema()
  let simple = "{ user(id: \"1\") { id name } }"
  let complex =
    "{ user(id: \"1\") { id name email active posts { id title content } } }"
  let nested = "{ users { id name posts { id title } } }"
  let n = 5000

  io.println(
    "Parse (no cache)                   "
    <> int.to_string(n)
    <> " iters",
  )
  io.println(string.repeat("-", 44))
  timed("simple ", fn() { bench_parse(simple, n) })
  timed("complex", fn() { bench_parse(complex, n) })
  timed("nested ", fn() { bench_parse(nested, n) })

  io.println("")
  io.println(
    "Parse (document cache)             "
    <> int.to_string(n)
    <> " iters",
  )
  io.println(string.repeat("-", 44))
  timed("simple ", fn() { bench_cache(simple, n) })
  timed("complex", fn() { bench_cache(complex, n) })
  timed("nested ", fn() { bench_cache(nested, n) })

  io.println("")
  io.println(
    "Execute (schema cache enabled)     "
    <> int.to_string(n)
    <> " iters",
  )
  io.println(string.repeat("-", 44))
  timed("simple ", fn() { bench_execute(simple, schema_def, n) })
  timed("complex", fn() { bench_execute(complex, schema_def, n) })
  timed("nested ", fn() { bench_execute(nested, schema_def, n) })
}

fn timed(label: String, f: fn() -> Nil) -> Nil {
  let t0 = now_ns()
  f()
  let elapsed_us = { now_ns() - t0 } / 1000
  io.println("  " <> label <> "  " <> int.to_string(elapsed_us) <> " µs total")
}

fn bench_parse(query: String, n: Int) -> Nil {
  list.each(list.repeat(Nil, n), fn(_) {
    let _ = parser.parse(query)
    Nil
  })
}

fn bench_cache(query: String, n: Int) -> Nil {
  let cache = document_cache.new()
  list.each(list.repeat(Nil, n), fn(_) {
    case document_cache.get(cache, query) {
      Ok(_) -> Nil
      Error(_) ->
        case parser.parse(query) {
          Ok(doc) -> document_cache.put(cache, query, doc)
          Error(_) -> Nil
        }
    }
  })
}

fn bench_execute(query: String, s: schema.Schema, n: Int) -> Nil {
  list.each(list.repeat(Nil, n), fn(_) {
    let _ = executor.execute_query(s, query)
    Nil
  })
}

fn create_benchmark_schema() -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("1")) }),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("Alice")) }),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("alice@example.com")) }),
    )
    |> schema.field(
      schema.field_def("active", schema.boolean_type())
      |> schema.resolver(fn(_) { Ok(types.to_dynamic(True)) }),
    )
    |> schema.field(
      schema.field_def("posts", schema.list_type(schema.named_type("Post")))
      |> schema.resolver(fn(_) { Ok(types.to_dynamic([])) }),
    )

  let post_type =
    schema.object("Post")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("1")) }),
    )
    |> schema.field(
      schema.field_def("title", schema.string_type())
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("Hello")) }),
    )
    |> schema.field(
      schema.field_def("content", schema.string_type())
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("World")) }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type())))
      |> schema.resolver(fn(_) { Ok(types.to_dynamic("user")) }),
    )
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.resolver(fn(_) { Ok(types.to_dynamic([])) }),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
  |> schema.add_type(schema.ObjectTypeDef(post_type))
}
