// mochi_wisp/supervisor.gleam
// OTP-style management for fault-tolerant GraphQL server
//
// Uses the BEAM's process model for:
// - Concurrent cache management
// - Metrics collection
// - Process isolation

import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/result
import logging
import mochi_wisp/query_cache

// ============================================================================
// Cache Manager Actor
// ============================================================================

/// Messages the cache manager can receive
pub type CacheMessage {
  /// Get cache statistics
  GetStats(reply_to: Subject(CacheStats))
  /// Clear the cache
  ClearCache
  /// Warm up cache with common queries
  WarmCache(queries: List(String))
  /// Get current cache size
  GetSize(reply_to: Subject(Int))
}

/// Cache statistics
pub type CacheStats {
  CacheStats(hits: Int, misses: Int, size: Int, hit_rate: Float)
}

/// Cache manager state
pub type CacheState {
  CacheState(initialized: Bool)
}

/// Start the cache manager actor
pub fn start_cache_manager() -> Result(Subject(CacheMessage), actor.StartError) {
  actor.new(CacheState(initialized: True))
  |> actor.on_message(handle_cache_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

fn handle_cache_message(
  state: CacheState,
  message: CacheMessage,
) -> actor.Next(CacheState, CacheMessage) {
  case message {
    GetStats(reply_to) -> {
      let stats = query_cache.stats()
      let total = stats.hits + stats.misses
      let hit_rate = case total {
        0 -> 0.0
        _ -> int.to_float(stats.hits) /. int.to_float(total) *. 100.0
      }
      actor.send(
        reply_to,
        CacheStats(
          hits: stats.hits,
          misses: stats.misses,
          size: stats.size,
          hit_rate: hit_rate,
        ),
      )
      actor.continue(state)
    }

    ClearCache -> {
      query_cache.clear()
      logging.log(logging.Info, "Query cache cleared")
      actor.continue(state)
    }

    WarmCache(queries) -> {
      list.each(queries, fn(q) {
        let _ = query_cache.get_or_parse(q)
        Nil
      })
      logging.log(
        logging.Info,
        "Cache warmed with "
          <> int.to_string(list.length(queries))
          <> " queries",
      )
      actor.continue(state)
    }

    GetSize(reply_to) -> {
      actor.send(reply_to, query_cache.size())
      actor.continue(state)
    }
  }
}

// ============================================================================
// Metrics Collector Actor
// ============================================================================

/// Messages for metrics collection
pub type MetricsMessage {
  /// Record a request completion
  RecordRequest(duration_us: Int, success: Bool)
  /// Get current metrics
  GetMetrics(reply_to: Subject(Metrics))
  /// Reset metrics
  ResetMetrics
}

/// Server metrics
pub type Metrics {
  Metrics(
    total_requests: Int,
    successful_requests: Int,
    failed_requests: Int,
    total_duration_us: Int,
    avg_duration_us: Float,
    max_duration_us: Int,
    min_duration_us: Int,
  )
}

/// Metrics state
pub type MetricsState {
  MetricsState(
    total_requests: Int,
    successful_requests: Int,
    failed_requests: Int,
    total_duration_us: Int,
    max_duration_us: Int,
    min_duration_us: Int,
  )
}

/// Start the metrics collector actor
pub fn start_metrics_collector() -> Result(
  Subject(MetricsMessage),
  actor.StartError,
) {
  actor.new(MetricsState(
    total_requests: 0,
    successful_requests: 0,
    failed_requests: 0,
    total_duration_us: 0,
    max_duration_us: 0,
    min_duration_us: 999_999_999,
  ))
  |> actor.on_message(handle_metrics_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

fn handle_metrics_message(
  state: MetricsState,
  message: MetricsMessage,
) -> actor.Next(MetricsState, MetricsMessage) {
  case message {
    RecordRequest(duration_us, success) -> {
      let new_state =
        MetricsState(
          total_requests: state.total_requests + 1,
          successful_requests: case success {
            True -> state.successful_requests + 1
            False -> state.successful_requests
          },
          failed_requests: case success {
            True -> state.failed_requests
            False -> state.failed_requests + 1
          },
          total_duration_us: state.total_duration_us + duration_us,
          max_duration_us: int.max(state.max_duration_us, duration_us),
          min_duration_us: int.min(state.min_duration_us, duration_us),
        )
      actor.continue(new_state)
    }

    GetMetrics(reply_to) -> {
      let avg = case state.total_requests {
        0 -> 0.0
        n -> int.to_float(state.total_duration_us) /. int.to_float(n)
      }
      actor.send(
        reply_to,
        Metrics(
          total_requests: state.total_requests,
          successful_requests: state.successful_requests,
          failed_requests: state.failed_requests,
          total_duration_us: state.total_duration_us,
          avg_duration_us: avg,
          max_duration_us: state.max_duration_us,
          min_duration_us: case state.total_requests {
            0 -> 0
            _ -> state.min_duration_us
          },
        ),
      )
      actor.continue(state)
    }

    ResetMetrics -> {
      logging.log(logging.Info, "Metrics reset")
      actor.continue(MetricsState(
        total_requests: 0,
        successful_requests: 0,
        failed_requests: 0,
        total_duration_us: 0,
        max_duration_us: 0,
        min_duration_us: 999_999_999,
      ))
    }
  }
}

// ============================================================================
// Application State
// ============================================================================

/// Global application state with actor references
pub type AppState {
  AppState(
    cache_manager: Subject(CacheMessage),
    metrics_collector: Subject(MetricsMessage),
  )
}

/// Initialize the application with all supervised actors
pub fn init_app() -> Result(AppState, String) {
  logging.log(logging.Info, "Initializing OTP application...")

  // Initialize ETS tables first
  query_cache.init()
  logging.log(logging.Info, "Query cache initialized")

  // Start cache manager actor
  let cache_result = start_cache_manager()
  use cache_manager <- result.try(
    cache_result
    |> result.map_error(fn(_) { "Failed to start cache manager" }),
  )
  logging.log(logging.Info, "Cache manager actor started")

  // Start metrics collector actor
  let metrics_result = start_metrics_collector()
  use metrics_collector <- result.try(
    metrics_result
    |> result.map_error(fn(_) { "Failed to start metrics collector" }),
  )
  logging.log(logging.Info, "Metrics collector actor started")

  logging.log(logging.Info, "OTP application initialized successfully")

  Ok(AppState(
    cache_manager: cache_manager,
    metrics_collector: metrics_collector,
  ))
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get cache stats (blocking call to actor)
pub fn get_cache_stats(app: AppState) -> CacheStats {
  actor.call(app.cache_manager, waiting: 5000, sending: GetStats)
}

/// Get metrics (blocking call to actor)
pub fn get_metrics(app: AppState) -> Metrics {
  actor.call(app.metrics_collector, waiting: 5000, sending: GetMetrics)
}

/// Record a request completion
pub fn record_request(app: AppState, duration_us: Int, success: Bool) -> Nil {
  actor.send(app.metrics_collector, RecordRequest(duration_us, success))
}

/// Clear the query cache
pub fn clear_cache(app: AppState) -> Nil {
  actor.send(app.cache_manager, ClearCache)
}

/// Warm up the cache with common queries
pub fn warm_cache(app: AppState, queries: List(String)) -> Nil {
  actor.send(app.cache_manager, WarmCache(queries))
}
