// mochi/telemetry.gleam
// Telemetry and instrumentation for GraphQL execution

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// Type of GraphQL operation
pub type OperationType {
  Query
  Mutation
  Subscription
}

/// Telemetry events emitted during GraphQL execution
pub type TelemetryEvent {
  /// Query parsing started
  ParseStart(timestamp: Int)
  /// Query parsing completed
  ParseEnd(timestamp: Int, success: Bool)
  /// Query validation started
  ValidationStart(timestamp: Int)
  /// Query validation completed
  ValidationEnd(timestamp: Int, success: Bool, error_count: Int)
  /// Operation execution started
  OperationStart(
    timestamp: Int,
    operation_name: Option(String),
    operation_type: OperationType,
  )
  /// Operation execution completed
  OperationEnd(
    timestamp: Int,
    operation_name: Option(String),
    success: Bool,
    error_count: Int,
  )
  /// Field resolution started
  FieldResolveStart(
    timestamp: Int,
    field_name: String,
    parent_type: String,
    path: List(String),
  )
  /// Field resolution completed
  FieldResolveEnd(
    timestamp: Int,
    field_name: String,
    parent_type: String,
    path: List(String),
    success: Bool,
    duration_ns: Int,
  )
  /// DataLoader batch executed
  DataLoaderBatch(
    timestamp: Int,
    loader_name: String,
    batch_size: Int,
    duration_ns: Int,
  )
  /// Custom event
  Custom(timestamp: Int, name: String, data: Dict(String, Dynamic))
}

/// Configuration for telemetry collection
pub type TelemetryConfig {
  TelemetryConfig(
    /// Whether telemetry is enabled
    enabled: Bool,
    /// Whether to track individual field resolutions (can be expensive)
    track_fields: Bool,
    /// Whether to track DataLoader batches
    track_dataloaders: Bool,
    /// Event handler function
    handler: fn(TelemetryEvent) -> Nil,
  )
}

/// Telemetry context that accumulates events during execution
pub type TelemetryContext {
  TelemetryContext(
    /// Configuration
    config: TelemetryConfig,
    /// Accumulated events (most recent first)
    events: List(TelemetryEvent),
    /// Start time of the operation
    operation_start: Option(Int),
    /// Field timing data: (parent_type.field_name) -> (start_time, duration)
    field_timings: Dict(String, #(Int, Int)),
    /// Current path in the query
    current_path: List(String),
  )
}

// ============================================================================
// Timestamp
// ============================================================================

/// Get current timestamp in nanoseconds (monotonic clock)
/// Uses erlang:monotonic_time/1 on Erlang and performance.now() on JavaScript
@external(erlang, "mochi_time_ffi", "monotonic_time_ns")
@external(javascript, "./mochi_time_ffi.mjs", "monotonic_time_ns")
pub fn get_timestamp_ns() -> Int

// ============================================================================
// Configuration
// ============================================================================

/// Create a disabled telemetry config
pub fn disabled() -> TelemetryConfig {
  TelemetryConfig(
    enabled: False,
    track_fields: False,
    track_dataloaders: False,
    handler: fn(_) { Nil },
  )
}

/// Create an enabled telemetry config with a handler
pub fn with_handler(handler: fn(TelemetryEvent) -> Nil) -> TelemetryConfig {
  TelemetryConfig(
    enabled: True,
    track_fields: False,
    track_dataloaders: True,
    handler: handler,
  )
}

/// Enable field-level tracking (can be expensive for large queries)
pub fn with_field_tracking(config: TelemetryConfig) -> TelemetryConfig {
  TelemetryConfig(..config, track_fields: True)
}

/// Disable field-level tracking
pub fn without_field_tracking(config: TelemetryConfig) -> TelemetryConfig {
  TelemetryConfig(..config, track_fields: False)
}

/// Enable DataLoader tracking
pub fn with_dataloader_tracking(config: TelemetryConfig) -> TelemetryConfig {
  TelemetryConfig(..config, track_dataloaders: True)
}

// ============================================================================
// Context Creation
// ============================================================================

/// Create a new telemetry context from config
pub fn new_context(config: TelemetryConfig) -> TelemetryContext {
  TelemetryContext(
    config: config,
    events: [],
    operation_start: None,
    field_timings: dict.new(),
    current_path: [],
  )
}

/// Create a context from an Option config
pub fn from_option(config: Option(TelemetryConfig)) -> Option(TelemetryContext) {
  case config {
    Some(c) -> Some(new_context(c))
    None -> None
  }
}

// ============================================================================
// Event Emission
// ============================================================================

/// Emit an event to the telemetry context
pub fn emit(ctx: TelemetryContext, event: TelemetryEvent) -> TelemetryContext {
  case ctx.config.enabled {
    False -> ctx
    True -> {
      // Call the handler
      ctx.config.handler(event)
      // Add to events list
      TelemetryContext(..ctx, events: [event, ..ctx.events])
    }
  }
}

/// Emit an event if telemetry context is present
pub fn emit_if_present(
  ctx: Option(TelemetryContext),
  event: TelemetryEvent,
) -> Option(TelemetryContext) {
  case ctx {
    Some(c) -> Some(emit(c, event))
    None -> None
  }
}

// ============================================================================
// Event Recording Helpers
// ============================================================================

/// Record parse start
pub fn record_parse_start(ctx: TelemetryContext) -> TelemetryContext {
  emit(ctx, ParseStart(get_timestamp_ns()))
}

/// Record parse end
pub fn record_parse_end(
  ctx: TelemetryContext,
  success: Bool,
) -> TelemetryContext {
  emit(ctx, ParseEnd(get_timestamp_ns(), success))
}

/// Record validation start
pub fn record_validation_start(ctx: TelemetryContext) -> TelemetryContext {
  emit(ctx, ValidationStart(get_timestamp_ns()))
}

/// Record validation end
pub fn record_validation_end(
  ctx: TelemetryContext,
  success: Bool,
  error_count: Int,
) -> TelemetryContext {
  emit(ctx, ValidationEnd(get_timestamp_ns(), success, error_count))
}

/// Record operation start
pub fn record_operation_start(
  ctx: TelemetryContext,
  operation_name: Option(String),
  operation_type: OperationType,
) -> TelemetryContext {
  let timestamp = get_timestamp_ns()
  let ctx = TelemetryContext(..ctx, operation_start: Some(timestamp))
  emit(ctx, OperationStart(timestamp, operation_name, operation_type))
}

/// Record operation end
pub fn record_operation_end(
  ctx: TelemetryContext,
  operation_name: Option(String),
  success: Bool,
  error_count: Int,
) -> TelemetryContext {
  emit(
    ctx,
    OperationEnd(get_timestamp_ns(), operation_name, success, error_count),
  )
}

/// Record field resolve start (if field tracking is enabled)
pub fn record_field_start(
  ctx: TelemetryContext,
  field_name: String,
  parent_type: String,
  path: List(String),
) -> TelemetryContext {
  case ctx.config.track_fields {
    False -> ctx
    True -> {
      let timestamp = get_timestamp_ns()
      let key = parent_type <> "." <> field_name
      let ctx =
        TelemetryContext(
          ..ctx,
          field_timings: dict.insert(ctx.field_timings, key, #(timestamp, 0)),
          current_path: path,
        )
      emit(ctx, FieldResolveStart(timestamp, field_name, parent_type, path))
    }
  }
}

/// Record field resolve end (if field tracking is enabled)
pub fn record_field_end(
  ctx: TelemetryContext,
  field_name: String,
  parent_type: String,
  path: List(String),
  success: Bool,
) -> TelemetryContext {
  case ctx.config.track_fields {
    False -> ctx
    True -> {
      let timestamp = get_timestamp_ns()
      let key = parent_type <> "." <> field_name
      let duration = case dict.get(ctx.field_timings, key) {
        Ok(#(start, _)) -> timestamp - start
        Error(_) -> 0
      }
      let ctx =
        TelemetryContext(
          ..ctx,
          field_timings: dict.insert(ctx.field_timings, key, #(0, duration)),
        )
      emit(
        ctx,
        FieldResolveEnd(
          timestamp,
          field_name,
          parent_type,
          path,
          success,
          duration,
        ),
      )
    }
  }
}

/// Record DataLoader batch execution
pub fn record_dataloader_batch(
  ctx: TelemetryContext,
  loader_name: String,
  batch_size: Int,
  duration_ns: Int,
) -> TelemetryContext {
  case ctx.config.track_dataloaders {
    False -> ctx
    True ->
      emit(
        ctx,
        DataLoaderBatch(
          get_timestamp_ns(),
          loader_name,
          batch_size,
          duration_ns,
        ),
      )
  }
}

/// Record a custom event
pub fn record_custom(
  ctx: TelemetryContext,
  name: String,
  data: Dict(String, Dynamic),
) -> TelemetryContext {
  emit(ctx, Custom(get_timestamp_ns(), name, data))
}

// ============================================================================
// Apollo Tracing Extension
// ============================================================================

/// Build an Apollo-compatible tracing extension from the telemetry context
/// This can be included in the GraphQL response extensions
pub fn build_tracing_extension(ctx: TelemetryContext) -> Dict(String, Dynamic) {
  let version = 1

  // Calculate total duration
  let #(start_time, end_time, duration) = case ctx.operation_start {
    Some(start) -> {
      let end = get_timestamp_ns()
      #(start, end, end - start)
    }
    None -> #(0, 0, 0)
  }

  // Build resolver list from field events
  let resolvers = build_resolver_list(ctx.events)

  // Build parsing info
  let parsing = build_phase_timing(ctx.events, "parse")

  // Build validation info
  let validation = build_phase_timing(ctx.events, "validation")

  dict.from_list([
    #("version", types.to_dynamic(version)),
    #("startTime", types.to_dynamic(start_time)),
    #("endTime", types.to_dynamic(end_time)),
    #("duration", types.to_dynamic(duration)),
    #("parsing", types.to_dynamic(parsing)),
    #("validation", types.to_dynamic(validation)),
    #(
      "execution",
      types.to_dynamic(
        dict.from_list([
          #("resolvers", types.to_dynamic(resolvers)),
        ]),
      ),
    ),
  ])
}

fn build_phase_timing(
  events: List(TelemetryEvent),
  phase: String,
) -> Dict(String, Dynamic) {
  let #(start, duration) = case phase {
    "parse" -> find_phase_timing(events, is_parse_start, is_parse_end)
    "validation" ->
      find_phase_timing(events, is_validation_start, is_validation_end)
    _ -> #(0, 0)
  }
  dict.from_list([
    #("startOffset", types.to_dynamic(start)),
    #("duration", types.to_dynamic(duration)),
  ])
}

fn is_parse_start(e: TelemetryEvent) -> Option(Int) {
  case e {
    ParseStart(t) -> Some(t)
    _ -> None
  }
}

fn is_parse_end(e: TelemetryEvent) -> Option(Int) {
  case e {
    ParseEnd(t, _) -> Some(t)
    _ -> None
  }
}

fn is_validation_start(e: TelemetryEvent) -> Option(Int) {
  case e {
    ValidationStart(t) -> Some(t)
    _ -> None
  }
}

fn is_validation_end(e: TelemetryEvent) -> Option(Int) {
  case e {
    ValidationEnd(t, _, _) -> Some(t)
    _ -> None
  }
}

fn find_phase_timing(
  events: List(TelemetryEvent),
  start_check: fn(TelemetryEvent) -> Option(Int),
  end_check: fn(TelemetryEvent) -> Option(Int),
) -> #(Int, Int) {
  let start =
    list.find_map(events, fn(e) {
      case start_check(e) {
        Some(v) -> Ok(v)
        None -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  let end =
    list.find_map(events, fn(e) {
      case end_check(e) {
        Some(v) -> Ok(v)
        None -> Error(Nil)
      }
    })
    |> result.unwrap(0)
  #(start, end - start)
}

fn build_resolver_list(events: List(TelemetryEvent)) -> List(Dynamic) {
  events
  |> list.filter_map(fn(e) {
    case e {
      FieldResolveEnd(_, field, parent, path, _, duration) ->
        Ok(
          types.to_dynamic(
            dict.from_list([
              #("path", types.to_dynamic(path)),
              #("parentType", types.to_dynamic(parent)),
              #("fieldName", types.to_dynamic(field)),
              #("returnType", types.to_dynamic("")),
              #("startOffset", types.to_dynamic(0)),
              #("duration", types.to_dynamic(duration)),
            ]),
          ),
        )
      _ -> Error(Nil)
    }
  })
}

import gleam/result

// ============================================================================
// Metrics Summary
// ============================================================================

/// Summary of telemetry metrics for a request
pub type MetricsSummary {
  MetricsSummary(
    /// Total execution time in nanoseconds
    total_duration_ns: Int,
    /// Parse time in nanoseconds
    parse_duration_ns: Int,
    /// Validation time in nanoseconds
    validation_duration_ns: Int,
    /// Number of fields resolved
    field_count: Int,
    /// Number of errors
    error_count: Int,
    /// Slowest field resolutions (field_key, duration_ns)
    slowest_fields: List(#(String, Int)),
  )
}

/// Build a metrics summary from the telemetry context
pub fn build_metrics_summary(ctx: TelemetryContext) -> MetricsSummary {
  let total = case ctx.operation_start {
    Some(start) -> get_timestamp_ns() - start
    None -> 0
  }

  let parse = find_phase_timing(ctx.events, is_parse_start, is_parse_end)
  let validation =
    find_phase_timing(ctx.events, is_validation_start, is_validation_end)

  let field_count =
    ctx.events
    |> list.count(fn(e) {
      case e {
        FieldResolveEnd(_, _, _, _, _, _) -> True
        _ -> False
      }
    })

  let error_count =
    ctx.events
    |> list.count(fn(e) {
      case e {
        FieldResolveEnd(_, _, _, _, False, _) -> True
        OperationEnd(_, _, False, _) -> True
        _ -> False
      }
    })

  // Get slowest fields
  let slowest =
    ctx.field_timings
    |> dict.to_list
    |> list.map(fn(kv) { #(kv.0, kv.1.1) })
    |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
    |> list.take(10)

  MetricsSummary(
    total_duration_ns: total,
    parse_duration_ns: parse.1,
    validation_duration_ns: validation.1,
    field_count: field_count,
    error_count: error_count,
    slowest_fields: slowest,
  )
}

/// Format duration in nanoseconds to human-readable string
pub fn format_duration(ns: Int) -> String {
  case ns {
    n if n < 1000 -> int.to_string(n) <> "ns"
    n if n < 1_000_000 -> int.to_string(n / 1000) <> "us"
    n if n < 1_000_000_000 -> int.to_string(n / 1_000_000) <> "ms"
    n -> int.to_string(n / 1_000_000_000) <> "s"
  }
}

/// Format metrics summary to string for logging
pub fn format_metrics_summary(summary: MetricsSummary) -> String {
  let lines = [
    "GraphQL Request Metrics:",
    "  Total: " <> format_duration(summary.total_duration_ns),
    "  Parse: " <> format_duration(summary.parse_duration_ns),
    "  Validate: " <> format_duration(summary.validation_duration_ns),
    "  Fields: " <> int.to_string(summary.field_count),
    "  Errors: " <> int.to_string(summary.error_count),
  ]

  let slowest_lines = case summary.slowest_fields {
    [] -> []
    fields -> [
      "  Slowest fields:",
      ..list.map(fields, fn(f) { "    " <> f.0 <> ": " <> format_duration(f.1) })
    ]
  }

  string.join(list.append(lines, slowest_lines), "\n")
}
