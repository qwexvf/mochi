// benchmark_runner.gleam
// Run with: gleam run -m benchmark_runner

import mochi_wisp/benchmark

pub fn main() {
  benchmark.run_all()
}
