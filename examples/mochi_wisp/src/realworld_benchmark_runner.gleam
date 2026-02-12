// realworld_benchmark_runner.gleam
// Run real-world GraphQL benchmarks

import mochi_wisp/realworld_benchmark

pub fn main() {
  realworld_benchmark.run_all()
}
