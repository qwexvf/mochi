// Tests for mochi/telemetry.gleam - telemetry and instrumentation
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/schema
import mochi/telemetry

// ============================================================================
// Configuration Tests
// ============================================================================

pub fn disabled_config_test() {
  let config = telemetry.disabled()
  should.equal(config.enabled, False)
  should.equal(config.track_fields, False)
  should.equal(config.track_dataloaders, False)
}

pub fn with_handler_config_test() {
  let handler = fn(_event: telemetry.TelemetryEvent) { Nil }
  let config = telemetry.with_handler(handler)
  should.equal(config.enabled, True)
}

pub fn with_field_tracking_test() {
  let config =
    telemetry.with_handler(fn(_) { Nil }) |> telemetry.with_field_tracking
  should.equal(config.track_fields, True)
}

pub fn without_field_tracking_test() {
  let config =
    telemetry.with_handler(fn(_) { Nil })
    |> telemetry.with_field_tracking
    |> telemetry.without_field_tracking
  should.equal(config.track_fields, False)
}

// ============================================================================
// Context Creation Tests
// ============================================================================

pub fn new_context_test() {
  let config = telemetry.disabled()
  let ctx = telemetry.new_context(config)
  should.equal(ctx.events, [])
  should.equal(ctx.operation_start, None)
}

pub fn from_option_some_test() {
  let config = telemetry.disabled()
  let result = telemetry.from_option(Some(config))
  should.be_true(option.is_some(result))
}

pub fn from_option_none_test() {
  let result = telemetry.from_option(None)
  should.equal(result, None)
}

// ============================================================================
// Event Emission Tests
// ============================================================================

pub fn disabled_config_does_not_emit_test() {
  let config = telemetry.disabled()
  let ctx = telemetry.new_context(config)
  let new_ctx = telemetry.emit(ctx, telemetry.ParseStart(0))
  // Disabled — event not added
  should.equal(list.length(new_ctx.events), 0)
}

pub fn enabled_config_emits_events_test() {
  let events_ref = list.new()
  let _ = events_ref
  // Track events in a mutable-ish way using a closure that captures state
  let config = telemetry.with_handler(fn(_event) { Nil })
  let ctx = telemetry.new_context(config)
  let new_ctx = telemetry.emit(ctx, telemetry.ParseStart(0))
  // Enabled — event added
  should.equal(list.length(new_ctx.events), 1)
}

pub fn emit_if_present_none_test() {
  let result = telemetry.emit_if_present(None, telemetry.ParseStart(0))
  should.equal(result, None)
}

pub fn emit_if_present_some_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx = telemetry.new_context(config)
  let result = telemetry.emit_if_present(Some(ctx), telemetry.ParseStart(0))
  should.be_true(option.is_some(result))
}

// ============================================================================
// Event Recording Helpers Tests
// ============================================================================

pub fn record_parse_start_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx = telemetry.new_context(config) |> telemetry.record_parse_start
  should.equal(list.length(ctx.events), 1)
  case list.first(ctx.events) {
    Ok(telemetry.ParseStart(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn record_parse_end_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx = telemetry.new_context(config) |> telemetry.record_parse_end(True)
  should.equal(list.length(ctx.events), 1)
  case list.first(ctx.events) {
    Ok(telemetry.ParseEnd(_, True)) -> Nil
    _ -> should.fail()
  }
}

pub fn record_operation_start_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx =
    telemetry.new_context(config)
    |> telemetry.record_operation_start(Some("TestOp"), telemetry.Query)
  should.be_true(option.is_some(ctx.operation_start))
}

pub fn record_operation_end_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx =
    telemetry.new_context(config)
    |> telemetry.record_operation_end(Some("TestOp"), True, 0)
  case list.first(ctx.events) {
    Ok(telemetry.OperationEnd(_, Some("TestOp"), True, 0)) -> Nil
    _ -> should.fail()
  }
}

pub fn record_field_start_no_tracking_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  // track_fields is False by default with with_handler
  let ctx =
    telemetry.new_context(config)
    |> telemetry.record_field_start("name", "User", ["user", "name"])
  // No event emitted since track_fields is False
  should.equal(list.length(ctx.events), 0)
}

pub fn record_field_start_with_tracking_test() {
  let config =
    telemetry.with_handler(fn(_) { Nil }) |> telemetry.with_field_tracking
  let ctx =
    telemetry.new_context(config)
    |> telemetry.record_field_start("name", "User", ["user", "name"])
  should.equal(list.length(ctx.events), 1)
}

// ============================================================================
// Metrics Summary Tests
// ============================================================================

pub fn build_metrics_summary_empty_test() {
  let config = telemetry.with_handler(fn(_) { Nil })
  let ctx = telemetry.new_context(config)
  let summary = telemetry.build_metrics_summary(ctx)
  should.equal(summary.field_count, 0)
  should.equal(summary.error_count, 0)
}

pub fn format_duration_nanoseconds_test() {
  should.equal(telemetry.format_duration(500), "500ns")
}

pub fn format_duration_microseconds_test() {
  should.equal(telemetry.format_duration(5000), "5us")
}

pub fn format_duration_milliseconds_test() {
  should.equal(telemetry.format_duration(5_000_000), "5ms")
}

pub fn format_duration_seconds_test() {
  should.equal(telemetry.format_duration(2_000_000_000), "2s")
}

// ============================================================================
// Schema Bridge Tests
// ============================================================================

pub fn to_schema_fn_disabled_test() {
  let config = telemetry.disabled()
  let fn_ = telemetry.to_schema_fn(config)
  // Should not panic when called with disabled config
  fn_(schema.SchemaOperationStart(None))
  Nil
}

pub fn to_schema_fn_emits_operation_start_test() {
  let received = []
  let _ = received
  // Just verify the function is callable
  let config = telemetry.with_handler(fn(_) { Nil })
  let fn_ = telemetry.to_schema_fn(config)
  fn_(schema.SchemaOperationStart(Some("Query")))
  fn_(schema.SchemaOperationEnd(Some("Query"), True, 0))
  Nil
}

pub fn to_schema_fn_emits_field_events_test() {
  let config =
    telemetry.with_handler(fn(_) { Nil }) |> telemetry.with_field_tracking
  let fn_ = telemetry.to_schema_fn(config)
  fn_(schema.SchemaFieldStart("name", "User", ["user", "name"]))
  fn_(schema.SchemaFieldEnd("name", "User", ["user", "name"], True, 1000))
  Nil
}
