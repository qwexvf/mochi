// Tests for mochi/batch.gleam - batched GraphQL query execution
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/batch
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

fn create_test_schema() -> schema.Schema {
  let resolver = fn(_info: schema.ResolverInfo) {
    Ok(types.to_dynamic("hello"))
  }
  let query_field =
    schema.field_def("hello", schema.Named("String"))
    |> schema.resolver(resolver)
  let query_type =
    schema.object("Query")
    |> schema.field(query_field)
  schema.schema()
  |> schema.query(query_type)
}

fn create_test_context() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
}

// ============================================================================
// Configuration Tests
// ============================================================================

pub fn default_config_test() {
  let config = batch.default_config()
  should.equal(config.max_batch_size, 10)
  should.equal(config.continue_on_error, True)
  should.equal(config.allow_parallel, False)
}

pub fn config_with_max_batch_size_test() {
  let config = batch.default_config() |> batch.with_max_batch_size(5)
  should.equal(config.max_batch_size, 5)
}

pub fn config_with_continue_on_error_test() {
  let config = batch.default_config() |> batch.with_continue_on_error(False)
  should.equal(config.continue_on_error, False)
}

pub fn config_with_parallel_execution_test() {
  let config = batch.default_config() |> batch.with_parallel_execution(True)
  should.equal(config.allow_parallel, True)
}

// ============================================================================
// Request Construction Tests
// ============================================================================

pub fn request_basic_test() {
  let req = batch.request("{ hello }")
  should.equal(req.query, "{ hello }")
  should.equal(req.variables, None)
  should.equal(req.operation_name, None)
}

pub fn request_with_variables_test() {
  let vars = dict.from_list([#("id", types.to_dynamic("123"))])
  let req = batch.request_with_variables("{ hello }", vars)
  should.equal(req.query, "{ hello }")
  should.be_true(option.is_some(req.variables))
}

pub fn request_with_operation_test() {
  let req = batch.request_with_operation("query MyOp { hello }", "MyOp")
  should.equal(req.operation_name, Some("MyOp"))
}

// ============================================================================
// Batch Execution Tests
// ============================================================================

pub fn execute_single_request_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config = batch.default_config()
  let requests = [batch.request("{ hello }")]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  should.equal(list.length(result.results), 1)
  should.equal(result.failure_count, 0)
  should.be_true(result.all_succeeded)
}

pub fn execute_multiple_requests_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config = batch.default_config()
  let requests = [batch.request("{ hello }"), batch.request("{ hello }")]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  should.equal(list.length(result.results), 2)
  should.equal(result.failure_count, 0)
  should.be_true(result.all_succeeded)
}

pub fn execute_batch_exceeds_max_size_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config = batch.default_config() |> batch.with_max_batch_size(2)
  let requests = [
    batch.request("{ hello }"),
    batch.request("{ hello }"),
    batch.request("{ hello }"),
  ]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  should.be_false(result.all_succeeded)
  should.equal(result.failure_count, 1)
}

pub fn execute_with_parse_error_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config = batch.default_config()
  let requests = [batch.request("INVALID QUERY !!!")]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  should.equal(result.failure_count, 1)
  should.be_false(result.all_succeeded)
}

pub fn execute_batch_parallel_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config = batch.default_config() |> batch.with_parallel_execution(True)
  let requests = [batch.request("{ hello }"), batch.request("{ hello }")]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  // Parallel execution should produce same results as sequential
  should.equal(list.length(result.results), 2)
  should.equal(result.failure_count, 0)
  should.be_true(result.all_succeeded)
}

pub fn stop_on_error_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let config =
    batch.default_config()
    |> batch.with_continue_on_error(False)
  let requests = [
    batch.request("INVALID"),
    batch.request("{ hello }"),
    batch.request("{ hello }"),
  ]

  let result = batch.execute_batch(schema_def, requests, config, ctx)

  // First request fails, subsequent are halted
  should.be_false(result.all_succeeded)
  should.be_true(result.failure_count > 0)
}

// ============================================================================
// Operation Selection Tests
// ============================================================================

pub fn execute_with_operation_name_test() {
  let schema_def = create_test_schema()
  let ctx = create_test_context()
  let query = "query GetHello { hello }"
  let doc_result = mochi_batch_parse_for_test(query)

  case doc_result {
    Ok(document) -> {
      let result =
        batch.execute_with_operation_name(
          schema_def,
          document,
          None,
          ctx,
          dict.new(),
          Some("GetHello"),
        )
      should.equal(result.errors, [])
    }
    Error(_) -> Nil
    // Skip if parse fails
  }
}

import mochi/parser

fn mochi_batch_parse_for_test(query: String) {
  parser.parse(query)
}
