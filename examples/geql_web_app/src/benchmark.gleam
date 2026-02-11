// GeQL Benchmarking Module - Performance testing for Gleam GraphQL
// Provides internal benchmarks and load testing utilities

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import mochi/ast
import mochi/executor
import mochi/parser
import mochi/schema
import schema_builder

/// Benchmark configuration
pub type BenchmarkConfig {
  BenchmarkConfig(
    iterations: Int,
    warmup_iterations: Int,
    display_progress: Bool,
  )
}

/// Benchmark result
pub type BenchmarkResult {
  BenchmarkResult(
    name: String,
    iterations: Int,
    total_time_ms: Int,
    avg_time_ms: Float,
    ops_per_second: Float,
    success_rate: Float,
  )
}

/// Default benchmark configuration
pub fn default_config() -> BenchmarkConfig {
  BenchmarkConfig(
    iterations: 1000,
    warmup_iterations: 100,
    display_progress: True,
  )
}

/// Run comprehensive GeQL benchmarks
pub fn run_benchmarks() -> Nil {
  io.println("🚀 GeQL Performance Benchmarks")
  io.println("=" |> string.repeat(50))
  io.println("")

  let schema = schema_builder.create_schema()
  let config = default_config()

  // Test queries matching Phoenix benchmark
  let simple_query = "{ user(id: \"1\") { id name email } }"
  let complex_query =
    "{ user(id: \"1\") { id name email active posts { id title content published } } }"
  let nested_query = "{ users { id name posts { id title } } }"

  // Run parsing benchmarks
  io.println("📝 Query Parsing Benchmarks")
  io.println("-" |> string.repeat(30))

  let parsing_results = [
    benchmark_parsing("Simple Query Parse", simple_query, config),
    benchmark_parsing("Complex Query Parse", complex_query, config),
    benchmark_parsing("Nested Query Parse", nested_query, config),
  ]

  display_results(parsing_results)
  io.println("")

  // Run execution benchmarks
  io.println("⚡ Query Execution Benchmarks")
  io.println("-" |> string.repeat(30))

  let execution_results = [
    benchmark_execution("Simple Execution", simple_query, schema, config),
    benchmark_execution("Complex Execution", complex_query, schema, config),
    benchmark_execution("Nested Execution", nested_query, schema, config),
  ]

  display_results(execution_results)
  io.println("")

  // Display comparison summary
  display_comparison_summary()
  display_http_testing_instructions()
}

/// Benchmark GraphQL parsing performance
fn benchmark_parsing(
  name: String,
  query: String,
  config: BenchmarkConfig,
) -> BenchmarkResult {
  case config.display_progress {
    True -> io.println("Running " <> name <> "...")
    False -> Nil
  }

  // Warmup
  let _ = run_parsing_iterations(query, config.warmup_iterations)

  // Actual benchmark
  let start_time = get_timestamp_ms()
  let results = run_parsing_iterations(query, config.iterations)
  let end_time = get_timestamp_ms()

  let total_time = end_time - start_time
  let successful = list.count(results, fn(r) { result.is_ok(r) })
  let success_rate = int.to_float(successful) /. int.to_float(config.iterations)
  let avg_time = int.to_float(total_time) /. int.to_float(config.iterations)
  let ops_per_second = 1000.0 /. avg_time

  BenchmarkResult(
    name: name,
    iterations: config.iterations,
    total_time_ms: total_time,
    avg_time_ms: avg_time,
    ops_per_second: ops_per_second,
    success_rate: success_rate,
  )
}

/// Benchmark GraphQL execution performance
fn benchmark_execution(
  name: String,
  query: String,
  schema: schema.Schema,
  config: BenchmarkConfig,
) -> BenchmarkResult {
  case config.display_progress {
    True -> io.println("Running " <> name <> "...")
    False -> Nil
  }

  // Warmup
  let _ = run_execution_iterations(query, schema, config.warmup_iterations)

  // Actual benchmark
  let start_time = get_timestamp_ms()
  let results = run_execution_iterations(query, schema, config.iterations)
  let end_time = get_timestamp_ms()

  let total_time = end_time - start_time
  let successful =
    list.count(results, fn(r) {
      case r.data {
        Some(_) -> True
        None -> False
      }
    })
  let success_rate = int.to_float(successful) /. int.to_float(config.iterations)
  let avg_time = int.to_float(total_time) /. int.to_float(config.iterations)
  let ops_per_second = 1000.0 /. avg_time

  BenchmarkResult(
    name: name,
    iterations: config.iterations,
    total_time_ms: total_time,
    avg_time_ms: avg_time,
    ops_per_second: ops_per_second,
    success_rate: success_rate,
  )
}

/// Run multiple parsing iterations
fn run_parsing_iterations(
  query: String,
  iterations: Int,
) -> List(Result(ast.Document, parser.ParseError)) {
  list.range(1, iterations)
  |> list.map(fn(_) { parser.parse(query) })
}

/// Run multiple execution iterations
fn run_execution_iterations(
  query: String,
  schema: schema.Schema,
  iterations: Int,
) -> List(executor.ExecutionResult) {
  list.range(1, iterations)
  |> list.map(fn(_) { executor.execute_query(schema, query) })
}

/// Display benchmark results
fn display_results(results: List(BenchmarkResult)) -> Nil {
  io.println("")
  io.println("Name                     Ops/sec    Avg Time    Success Rate")
  io.println("-" |> string.repeat(60))

  list.each(results, fn(result) {
    let ops_str = float_to_string_2dp(result.ops_per_second)
    let time_str = float_to_string_2dp(result.avg_time_ms) <> "ms"
    let success_str = float_to_string_1dp(result.success_rate *. 100.0) <> "%"

    io.println(
      string.pad_right(result.name, 24)
      <> " "
      <> string.pad_left(ops_str, 9)
      <> " "
      <> string.pad_left(time_str, 10)
      <> " "
      <> string.pad_left(success_str, 12),
    )
  })
  io.println("")
}

/// Display performance comparison summary
fn display_comparison_summary() -> Nil {
  io.println("📊 GeQL vs Phoenix/Absinthe Comparison")
  io.println("=" |> string.repeat(45))
  io.println("")
  io.println("Expected GeQL Advantages:")
  io.println("✅ Lower memory footprint (native compilation)")
  io.println("✅ Faster cold start times")
  io.println("✅ Compile-time type safety")
  io.println("✅ Zero-dependency core library")
  io.println("")
  io.println("Current GeQL Limitations:")
  io.println("⚠️  Dynamic serialization requires manual implementation")
  io.println("⚠️  Smaller ecosystem compared to Elixir/Phoenix")
  io.println("⚠️  HTTP server integration still in development")
  io.println("")
  io.println("Phoenix/Absinthe Reference (Ryzen 9 5950X):")
  io.println("- Simple queries: 10.32K ops/sec, 96.93μs avg")
  io.println("- Complex queries: 5.64K ops/sec, 177.40μs avg")
  io.println("- Memory usage: 185KB-361KB per query")
  io.println("")
}

/// Display HTTP testing instructions
fn display_http_testing_instructions() -> Nil {
  io.println("🔧 HTTP Load Testing Instructions")
  io.println("=" |> string.repeat(40))
  io.println("")
  io.println("1. Start the GeQL web server:")
  io.println("   cd examples/geql_web_app")
  io.println("   gleam run")
  io.println("")
  io.println("2. Test with curl:")
  io.println("   curl -X POST http://localhost:8080/graphql \\")
  io.println("        -H \"Content-Type: application/json\" \\")
  io.println("        -d '{\"query\":\"{ user(id: \\\"1\\\") { id name } }\"}'")
  io.println("")
  io.println("3. Load testing with hey:")
  io.println("   hey -n 1000 -c 10 -m POST \\")
  io.println("       -H \"Content-Type: application/json\" \\")
  io.println(
    "       -d '{\"query\":\"{ user(id: \\\"1\\\") { id name } }\"}' \\",
  )
  io.println("       http://localhost:8080/graphql")
  io.println("")
  io.println("4. Compare with Phoenix/Absinthe:")
  io.println("   hey -n 1000 -c 10 -m POST \\")
  io.println("       -H \"Content-Type: application/json\" \\")
  io.println(
    "       -d '{\"query\":\"{ user(id: \\\"1\\\") { id name } }\"}' \\",
  )
  io.println("       http://localhost:4001/api/graphql")
  io.println("")
}

// Utility functions

/// Get current timestamp in milliseconds (simplified)
fn get_timestamp_ms() -> Int {
  // In a real implementation, you'd use proper timing
  // For now, return a simple counter
  0
}

/// Convert float to string with 2 decimal places
fn float_to_string_2dp(value: Float) -> String {
  // Simplified float formatting for demo
  case value >=. 1000.0 {
    True -> {
      let k_value = value /. 1000.0
      float_to_string_1dp(k_value) <> "K"
    }
    False -> float_to_string_1dp(value)
  }
}

/// Convert float to string with 1 decimal place
fn float_to_string_1dp(value: Float) -> String {
  // Simplified float formatting - in real implementation use proper formatting
  let int_part = float.truncate(value)
  case int_part == 0 && value <. 1.0 {
    True -> "0.0"
    False -> int.to_string(int_part) <> ".0"
  }
}

// Demo function to be called from main
pub fn demo() -> Nil {
  io.println("🧪 GeQL Benchmark Demo")
  io.println("This would run comprehensive performance tests")
  io.println("Note: Actual timing requires system clock integration")
  io.println("")

  let schema = schema_builder.create_schema()
  let simple_query = "{ user(id: \"1\") { id name } }"

  io.println("Testing query parsing...")
  case parser.parse(simple_query) {
    Ok(_) -> io.println("✅ Parse successful")
    Error(_) -> io.println("❌ Parse failed")
  }

  io.println("Testing query execution...")
  let result = executor.execute_query(schema, simple_query)
  case result.data {
    Some(_) ->
      io.println("✅ Execution successful (limited by Dynamic serialization)")
    None -> io.println("⚠️  Execution limited - Dynamic serialization needed")
  }

  io.println("")
  io.println("For full benchmarks, run: gleam run -m benchmark")
}
