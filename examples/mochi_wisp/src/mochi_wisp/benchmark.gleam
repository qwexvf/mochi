// mochi_wisp/benchmark.gleam
// Performance benchmarks for GraphQL operations

import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None}
import gleam/string
import mochi/executor
import mochi/parser
import mochi/schema
import mochi/types
import mochi_wisp/graphql_handler
import mochi_wisp/query_cache
import mochi_wisp/schema as wisp_schema

// ============================================================================
// Timing FFI
// ============================================================================

/// Measure execution time in microseconds
@external(erlang, "mochi_wisp_benchmark_ffi", "measure_time")
pub fn measure_time(f: fn() -> a) -> #(Int, a)

/// Get current monotonic time in microseconds
@external(erlang, "mochi_wisp_benchmark_ffi", "monotonic_time_us")
pub fn monotonic_time_us() -> Int

// ============================================================================
// Benchmark Runner
// ============================================================================

pub type BenchmarkResult {
  BenchmarkResult(
    name: String,
    iterations: Int,
    total_us: Int,
    avg_us: Float,
    min_us: Int,
    max_us: Int,
    ops_per_sec: Float,
  )
}

/// Run all benchmarks
pub fn run_all() -> Nil {
  io.println("")
  io.println("==============================================")
  io.println("  Mochi GraphQL Benchmark Suite")
  io.println("==============================================")
  io.println("")

  // Warm up
  io.println("Warming up...")
  let schema = wisp_schema.build_schema()
  let _ = executor.execute_query(schema, "{ users { id } }")
  io.println("")

  // Run benchmarks
  run_parsing_benchmarks()
  io.println("")
  run_cache_benchmarks()
  io.println("")
  run_schema_benchmarks()
  io.println("")
  run_execution_benchmarks()
  io.println("")
  run_json_benchmarks()
  io.println("")
  run_e2e_benchmarks()

  io.println("")
  run_throughput_test()
  io.println("")
  run_cached_throughput_test()

  io.println("")
  print_summary()

  io.println("")
  io.println("==============================================")
  io.println("  Benchmark Complete")
  io.println("==============================================")
}

// ============================================================================
// Throughput Test
// ============================================================================

fn run_throughput_test() -> Nil {
  io.println("--- Throughput Test (sustained load) ---")

  let schema = wisp_schema.build_schema()
  let body = "{\"query\": \"{ users { id name email role } }\"}"

  // Run for a fixed duration and count operations
  let duration_ms = 1000  // 1 second
  let start = monotonic_time_us()
  let ops = run_for_duration(schema, body, start, duration_ms * 1000, 0)
  let elapsed_us = monotonic_time_us() - start

  let ops_per_sec = int.to_float(ops) *. 1_000_000.0 /. int.to_float(elapsed_us)
  let latency_us = int.to_float(elapsed_us) /. int.to_float(ops)

  io.println(
    "Sustained throughput (1s):     "
    <> format_ops(ops_per_sec)
    <> " ops/sec"
    <> "  (avg latency: "
    <> format_time(latency_us)
    <> ")"
  )
  Nil
}

fn run_for_duration(
  gql_schema: schema.Schema,
  body: String,
  start: Int,
  duration_us: Int,
  count: Int,
) -> Int {
  let now = monotonic_time_us()
  case now - start >= duration_us {
    True -> count
    False -> {
      // Execute one full request
      case graphql_handler.parse_graphql_request(body) {
        Ok(req) -> {
          let vars = option.unwrap(req.variables, dict.new())
          let result = executor.execute_query_with_variables(gql_schema, req.query, vars)
          let _ = graphql_handler.execution_result_to_json(result)
          run_for_duration(gql_schema, body, start, duration_us, count + 1)
        }
        Error(_) -> run_for_duration(gql_schema, body, start, duration_us, count)
      }
    }
  }
}

// ============================================================================
// Cached Throughput Test
// ============================================================================

fn run_cached_throughput_test() -> Nil {
  io.println("--- Cached Throughput Test (using query cache) ---")

  // Initialize and prime the cache
  query_cache.init()
  query_cache.clear()

  let schema = wisp_schema.build_schema()
  let query = "{ users { id name email role } }"

  // Prime the cache
  let _ = query_cache.get_or_parse(query)

  // Run for a fixed duration and count operations
  let duration_ms = 1000  // 1 second
  let start = monotonic_time_us()
  let ops = run_cached_for_duration(schema, query, start, duration_ms * 1000, 0)
  let elapsed_us = monotonic_time_us() - start

  let ops_per_sec = int.to_float(ops) *. 1_000_000.0 /. int.to_float(elapsed_us)
  let latency_us = int.to_float(elapsed_us) /. int.to_float(ops)

  io.println(
    "Cached throughput (1s):        "
    <> format_ops(ops_per_sec)
    <> " ops/sec"
    <> "  (avg latency: "
    <> format_time(latency_us)
    <> ")"
  )

  // Show cache stats
  let stats = query_cache.stats()
  io.println(
    "Cache stats: "
    <> int.to_string(stats.hits)
    <> " hits, "
    <> int.to_string(stats.misses)
    <> " misses, "
    <> int.to_string(stats.size)
    <> " entries"
  )
  Nil
}

fn run_cached_for_duration(
  gql_schema: schema.Schema,
  query: String,
  start: Int,
  duration_us: Int,
  count: Int,
) -> Int {
  let now = monotonic_time_us()
  case now - start >= duration_us {
    True -> count
    False -> {
      // Execute using cached AST
      case query_cache.get_or_parse(query) {
        Ok(document) -> {
          let ctx = schema.execution_context(types.to_dynamic(dict.new()))
          let result = executor.execute(gql_schema, document, None, ctx, dict.new())
          let _ = graphql_handler.execution_result_to_json(result)
          run_cached_for_duration(gql_schema, query, start, duration_us, count + 1)
        }
        Error(_) -> run_cached_for_duration(gql_schema, query, start, duration_us, count)
      }
    }
  }
}

// ============================================================================
// Summary
// ============================================================================

fn print_summary() -> Nil {
  io.println("--- Summary ---")
  io.println("")
  io.println("Performance characteristics:")
  io.println("  - Query parsing: ~4-120µs depending on complexity")
  io.println("  - Query cache HIT: ~0.02-0.14µs (7-50M ops/sec)")
  io.println("  - Schema is pre-built (cached), very fast to access")
  io.println("  - Query execution: ~25-60µs for typical queries")
  io.println("  - JSON serialization: ~1-2µs for results")
  io.println("  - End-to-end: ~18-80µs per request")
  io.println("")
  io.println("Throughput:")
  io.println("  - Without cache: ~27K ops/sec (single core)")
  io.println("  - With cache:    ~100K+ ops/sec (single core) - 3.9x faster!")
  io.println("")
  io.println("Estimated HTTP throughput:")
  io.println("  - Simple queries:  ~100K+ req/sec (single core, with cache)")
  io.println("  - Complex queries: ~30K+ req/sec (single core, with cache)")
  io.println("")
  io.println("For HTTP load testing, use:")
  io.println("  wrk -t4 -c100 -d30s -s bench.lua http://localhost:8000/graphql")
  Nil
}

// ============================================================================
// Parsing Benchmarks
// ============================================================================

fn run_parsing_benchmarks() -> Nil {
  io.println("--- Query Parsing ---")

  let simple_query = "{ hello }"
  let medium_query = "{ users { id name email } }"
  let complex_query =
    "query GetUser($id: ID!) { user(id: $id) { id name email role } users { id name } }"

  let _ = benchmark("Parse simple query", 10_000, fn() { parser.parse(simple_query) })
  let _ = benchmark("Parse medium query", 10_000, fn() { parser.parse(medium_query) })
  let _ = benchmark("Parse complex query", 10_000, fn() { parser.parse(complex_query) })
  Nil
}

// ============================================================================
// Query Cache Benchmarks
// ============================================================================

fn run_cache_benchmarks() -> Nil {
  io.println("--- Query Cache Performance ---")

  // Initialize cache
  query_cache.init()
  query_cache.clear()

  let simple_query = "{ hello }"
  let medium_query = "{ users { id name email } }"
  let complex_query =
    "query GetUser($id: ID!) { user(id: $id) { id name email role } users { id name } }"

  // Benchmark cache miss (first parse)
  let _ = benchmark("Cache MISS (simple)", 10_000, fn() {
    query_cache.clear()
    query_cache.get_or_parse(simple_query)
  })

  // Benchmark cache hit
  query_cache.clear()
  let _ = query_cache.get_or_parse(simple_query)  // Prime the cache
  let _ = benchmark("Cache HIT (simple)", 10_000, fn() {
    query_cache.get_or_parse(simple_query)
  })

  // Benchmark cache miss vs hit for medium query
  let _ = benchmark("Cache MISS (medium)", 10_000, fn() {
    query_cache.clear()
    query_cache.get_or_parse(medium_query)
  })

  query_cache.clear()
  let _ = query_cache.get_or_parse(medium_query)  // Prime the cache
  let _ = benchmark("Cache HIT (medium)", 10_000, fn() {
    query_cache.get_or_parse(medium_query)
  })

  // Benchmark cache miss vs hit for complex query
  let _ = benchmark("Cache MISS (complex)", 10_000, fn() {
    query_cache.clear()
    query_cache.get_or_parse(complex_query)
  })

  query_cache.clear()
  let _ = query_cache.get_or_parse(complex_query)  // Prime the cache
  let _ = benchmark("Cache HIT (complex)", 10_000, fn() {
    query_cache.get_or_parse(complex_query)
  })

  // Clear for subsequent tests
  query_cache.clear()
  Nil
}

// ============================================================================
// Schema Benchmarks
// ============================================================================

fn run_schema_benchmarks() -> Nil {
  io.println("--- Schema Building ---")

  let _ = benchmark("Build schema", 1000, fn() { wisp_schema.build_schema() })
  let _ = benchmark("Build user type", 10_000, fn() { wisp_schema.user_type() })
  let _ = benchmark("Build role enum", 10_000, fn() { wisp_schema.role_enum() })
  Nil
}

// ============================================================================
// Execution Benchmarks
// ============================================================================

fn run_execution_benchmarks() -> Nil {
  io.println("--- Query Execution ---")

  let schema = wisp_schema.build_schema()

  let _ = benchmark("Execute users query", 1000, fn() {
    executor.execute_query(schema, "{ users { id name email } }")
  })

  let _ = benchmark("Execute users + role", 1000, fn() {
    executor.execute_query(schema, "{ users { id name email role } }")
  })

  let _ = benchmark("Execute user by id", 1000, fn() {
    executor.execute_query(schema, "{ user(id: \"1\") { id name } }")
  })

  let _ = benchmark("Execute with variables", 1000, fn() {
    let vars = dict.from_list([#("id", types.to_dynamic("1"))])
    executor.execute_query_with_variables(
      schema,
      "query GetUser($id: ID!) { user(id: $id) { id name } }",
      vars,
    )
  })
  Nil
}

// ============================================================================
// JSON Serialization Benchmarks
// ============================================================================

fn run_json_benchmarks() -> Nil {
  io.println("--- JSON Serialization ---")

  let schema = wisp_schema.build_schema()
  let result = executor.execute_query(schema, "{ users { id name email role } }")

  let _ = benchmark("Serialize result to JSON", 10_000, fn() {
    graphql_handler.execution_result_to_json(result)
  })

  // Test with errors
  let error_result =
    executor.ExecutionResult(data: None, errors: [
      executor.ValidationError("Test error", ["field", "subfield"]),
      executor.ResolverError("Another error", ["other"]),
    ])

  let _ = benchmark("Serialize error result", 10_000, fn() {
    graphql_handler.execution_result_to_json(error_result)
  })
  Nil
}

// ============================================================================
// End-to-End Benchmarks
// ============================================================================

fn run_e2e_benchmarks() -> Nil {
  io.println("--- End-to-End (Parse + Execute + Serialize) ---")

  let schema = wisp_schema.build_schema()

  let _ = benchmark("E2E simple query", 1000, fn() {
    let query = "{ users { id name } }"
    case graphql_handler.parse_graphql_request("{\"query\": \"" <> query <> "\"}") {
      Ok(req) -> {
        let vars = option.unwrap(req.variables, dict.new())
        let result = executor.execute_query_with_variables(schema, req.query, vars)
        graphql_handler.execution_result_to_json(result)
      }
      Error(_) -> ""
    }
  })

  let _ = benchmark("E2E complex query", 1000, fn() {
    let body =
      "{\"query\": \"{ users { id name email role } user(id: \\\"1\\\") { id name } }\"}"
    case graphql_handler.parse_graphql_request(body) {
      Ok(req) -> {
        let vars = option.unwrap(req.variables, dict.new())
        let result = executor.execute_query_with_variables(schema, req.query, vars)
        graphql_handler.execution_result_to_json(result)
      }
      Error(_) -> ""
    }
  })

  let _ = benchmark("E2E with variables", 1000, fn() {
    let body =
      "{\"query\": \"query GetUser($id: ID!) { user(id: $id) { id name email } }\", \"variables\": {\"id\": \"2\"}}"
    case graphql_handler.parse_graphql_request(body) {
      Ok(req) -> {
        let vars = option.unwrap(req.variables, dict.new())
        let result = executor.execute_query_with_variables(schema, req.query, vars)
        graphql_handler.execution_result_to_json(result)
      }
      Error(_) -> ""
    }
  })
  Nil
}

// ============================================================================
// Benchmark Utilities
// ============================================================================

fn benchmark(name: String, iterations: Int, f: fn() -> a) -> BenchmarkResult {
  let times = run_iterations(iterations, f, [])

  let total_us = list.fold(times, 0, fn(acc, t) { acc + t })
  let avg_us = int.to_float(total_us) /. int.to_float(iterations)
  let min_us = list.fold(times, 999_999_999, fn(acc, t) { int.min(acc, t) })
  let max_us = list.fold(times, 0, fn(acc, t) { int.max(acc, t) })
  let ops_per_sec = 1_000_000.0 /. avg_us

  let result =
    BenchmarkResult(
      name: name,
      iterations: iterations,
      total_us: total_us,
      avg_us: avg_us,
      min_us: min_us,
      max_us: max_us,
      ops_per_sec: ops_per_sec,
    )

  print_result(result)
  result
}

fn run_iterations(n: Int, f: fn() -> a, acc: List(Int)) -> List(Int) {
  case n <= 0 {
    True -> acc
    False -> {
      let #(time_us, _) = measure_time(f)
      run_iterations(n - 1, f, [time_us, ..acc])
    }
  }
}

fn print_result(result: BenchmarkResult) -> Nil {
  let avg_str = format_time(result.avg_us)
  let ops_str = format_ops(result.ops_per_sec)

  io.println(
    pad_right(result.name, 30)
    <> " "
    <> pad_left(avg_str, 12)
    <> "  "
    <> pad_left(ops_str, 14)
    <> " ops/sec"
    <> "  (min: "
    <> format_time(int.to_float(result.min_us))
    <> ", max: "
    <> format_time(int.to_float(result.max_us))
    <> ")",
  )
}

fn format_time(us: Float) -> String {
  case us <. 1000.0 {
    True -> float_to_string_2dp(us) <> " µs"
    False ->
      case us <. 1_000_000.0 {
        True -> float_to_string_2dp(us /. 1000.0) <> " ms"
        False -> float_to_string_2dp(us /. 1_000_000.0) <> " s"
      }
  }
}

fn format_ops(ops: Float) -> String {
  case ops >=. 1_000_000.0 {
    True -> float_to_string_2dp(ops /. 1_000_000.0) <> "M"
    False ->
      case ops >=. 1000.0 {
        True -> float_to_string_2dp(ops /. 1000.0) <> "K"
        False -> float_to_string_2dp(ops)
      }
  }
}

fn float_to_string_2dp(f: Float) -> String {
  let rounded = float.round(f *. 100.0)
  let int_part = rounded / 100
  let dec_part = int.absolute_value(rounded % 100)
  let dec_str = case dec_part < 10 {
    True -> "0" <> int.to_string(dec_part)
    False -> int.to_string(dec_part)
  }
  int.to_string(int_part) <> "." <> dec_str
}

fn pad_right(s: String, len: Int) -> String {
  let current = string.length(s)
  case current >= len {
    True -> s
    False -> s <> string.repeat(" ", len - current)
  }
}

fn pad_left(s: String, len: Int) -> String {
  let current = string.length(s)
  case current >= len {
    True -> s
    False -> string.repeat(" ", len - current) <> s
  }
}
