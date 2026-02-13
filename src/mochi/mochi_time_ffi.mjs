// Monotonic time in nanoseconds for JavaScript
export function monotonic_time_ns() {
  return Math.floor(performance.now() * 1_000_000);
}
