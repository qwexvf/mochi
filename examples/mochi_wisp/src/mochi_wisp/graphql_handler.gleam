// mochi_wisp/graphql_handler.gleam
// GraphQL HTTP handler for Wisp integration with comprehensive logging

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import logging
import mochi/executor
import mochi/schema.{type Schema}
import mochi/types
import mochi_wisp/query_cache
import wisp.{type Request as WispRequest, type Response as WispResponse}

// ============================================================================
// Request Types
// ============================================================================

/// Parsed GraphQL request
pub type GraphQLRequest {
  GraphQLRequest(
    query: String,
    variables: Option(Dict(String, Dynamic)),
    operation_name: Option(String),
  )
}

// ============================================================================
// Main Handler
// ============================================================================

/// Handle a GraphQL HTTP request with logging
pub fn handle_graphql(req: WispRequest, schema: Schema) -> WispResponse {
  let request_id = generate_request_id()

  logging.log(logging.Info, "[" <> request_id <> "] GraphQL request received")

  use body <- wisp.require_string_body(req)

  let body_size = string.byte_size(body)
  logging.log(
    logging.Debug,
    "["
      <> request_id
      <> "] Request body size: "
      <> int.to_string(body_size)
      <> " bytes",
  )

  case parse_graphql_request(body) {
    Ok(graphql_req) -> {
      log_graphql_request(request_id, graphql_req)
      let response = execute_and_respond(request_id, graphql_req, schema)
      logging.log(
        logging.Info,
        "["
          <> request_id
          <> "] Response status: "
          <> int.to_string(response.status),
      )
      response
    }
    Error(msg) -> {
      logging.log(
        logging.Warning,
        "[" <> request_id <> "] Parse error: " <> msg,
      )
      error_response(msg)
    }
  }
}

/// Handle a GraphQL HTTP request without logging (for testing)
pub fn handle_graphql_quiet(req: WispRequest, schema: Schema) -> WispResponse {
  use body <- wisp.require_string_body(req)

  case parse_graphql_request(body) {
    Ok(graphql_req) -> execute_and_respond_quiet(graphql_req, schema)
    Error(msg) -> error_response(msg)
  }
}

// ============================================================================
// Logging Helpers
// ============================================================================

/// Generate a simple request ID for log correlation
@external(erlang, "mochi_wisp_ffi", "generate_request_id")
@external(javascript, "../../mochi_wisp_ffi.mjs", "generate_request_id")
fn generate_request_id() -> String

/// Log GraphQL request details
fn log_graphql_request(request_id: String, req: GraphQLRequest) -> Nil {
  // Log the query (truncated if too long)
  let query_preview = truncate_string(req.query, 100)
  logging.log(logging.Debug, "[" <> request_id <> "] Query: " <> query_preview)

  // Log operation name if present
  case req.operation_name {
    Some(name) ->
      logging.log(logging.Debug, "[" <> request_id <> "] Operation: " <> name)
    None -> Nil
  }

  // Log variables count if present
  case req.variables {
    Some(vars) -> {
      let var_count = dict.size(vars)
      logging.log(
        logging.Debug,
        "["
          <> request_id
          <> "] Variables: "
          <> int.to_string(var_count)
          <> " provided",
      )
    }
    None -> Nil
  }
}

/// Truncate a string for logging
fn truncate_string(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

// ============================================================================
// Request Parsing - Using minimal FFI for Dynamic preservation
// ============================================================================

/// Parse a GraphQL request from JSON body
/// Uses FFI because we need to preserve Dynamic values for variables
@external(erlang, "mochi_wisp_ffi", "parse_graphql_request_full")
@external(javascript, "../../mochi_wisp_ffi.mjs", "parse_graphql_request_full")
pub fn parse_graphql_request(body: String) -> Result(GraphQLRequest, String)

// ============================================================================
// Execution
// ============================================================================

fn execute_and_respond(
  request_id: String,
  req: GraphQLRequest,
  schema: Schema,
) -> WispResponse {
  let variables = option.unwrap(req.variables, dict.new())

  logging.log(logging.Debug, "[" <> request_id <> "] Executing query...")

  // Use cached parsing for better performance
  let result = execute_with_cache(schema, req.query, variables)

  // Log execution result
  case result.errors {
    [] ->
      logging.log(
        logging.Debug,
        "[" <> request_id <> "] Query executed successfully",
      )
    errors -> {
      let error_count = count_list(errors)
      logging.log(
        logging.Warning,
        "["
          <> request_id
          <> "] Query returned "
          <> int.to_string(error_count)
          <> " error(s)",
      )
      log_errors(request_id, errors)
    }
  }

  let json_body = execution_result_to_json(result)
  let response_size = string.byte_size(json_body)
  logging.log(
    logging.Debug,
    "["
      <> request_id
      <> "] Response size: "
      <> int.to_string(response_size)
      <> " bytes",
  )

  wisp.json_response(json_body, 200)
  |> add_cors_headers
}

fn execute_and_respond_quiet(
  req: GraphQLRequest,
  schema: Schema,
) -> WispResponse {
  let variables = option.unwrap(req.variables, dict.new())

  // Use cached parsing for better performance
  let result = execute_with_cache(schema, req.query, variables)

  let json_body = execution_result_to_json(result)

  wisp.json_response(json_body, 200)
  |> add_cors_headers
}

// ============================================================================
// Cached Execution
// ============================================================================

/// Execute a query using cached AST parsing with FAST O(n) parser
/// This significantly improves performance for:
/// - Repeated queries (cache hit)
/// - First-time queries (O(n) binary lexer vs O(n²) string concatenation)
fn execute_with_cache(
  schema_def: Schema,
  query: String,
  variables: Dict(String, Dynamic),
) -> executor.ExecutionResult {
  // Use fast O(n) parser instead of O(n²) standard parser
  case query_cache.get_or_parse_fast(query) {
    Ok(document) -> {
      let ctx = schema.execution_context(types.to_dynamic(dict.new()))
      executor.execute(schema_def, document, None, ctx, variables)
    }
    Error(_parse_error) -> {
      executor.ExecutionResult(data: None, errors: [
        executor.ValidationError("Failed to parse query", [], None),
      ])
    }
  }
}

/// Log execution errors
fn log_errors(request_id: String, errors: List(executor.ExecutionError)) -> Nil {
  case errors {
    [] -> Nil
    [error, ..rest] -> {
      let #(msg, path) = case error {
        executor.ValidationError(m, p, _loc) -> #("ValidationError: " <> m, p)
        executor.ResolverError(m, p, _loc) -> #("ResolverError: " <> m, p)
        executor.TypeError(m, p, _loc) -> #("TypeError: " <> m, p)
        executor.NullValueError(m, p, _loc) -> #("NullValueError: " <> m, p)
      }
      let path_str = string.join(path, ".")
      logging.log(
        logging.Warning,
        "[" <> request_id <> "]   - " <> msg <> " at " <> path_str,
      )
      log_errors(request_id, rest)
    }
  }
}

/// Count list elements without using list.length for efficiency awareness
fn count_list(l: List(a)) -> Int {
  count_list_acc(l, 0)
}

fn count_list_acc(l: List(a), acc: Int) -> Int {
  case l {
    [] -> acc
    [_, ..rest] -> count_list_acc(rest, acc + 1)
  }
}

/// Convert execution result to JSON string
pub fn execution_result_to_json(result: executor.ExecutionResult) -> String {
  let data_json = case result.data {
    Some(data) -> dynamic_to_json(data)
    None -> json.null()
  }

  let response = case result.errors {
    [] -> json.object([#("data", data_json)])
    errors ->
      json.object([#("data", data_json), #("errors", encode_errors(errors))])
  }

  json.to_string(response)
}

/// Convert Dynamic value to JSON - Uses FFI for proper type introspection
@external(erlang, "mochi_wisp_ffi", "dynamic_to_json")
@external(javascript, "../../mochi_wisp_ffi.mjs", "dynamic_to_json")
fn dynamic_to_json_ffi(data: Dynamic) -> json.Json

/// Convert Dynamic value to JSON
pub fn dynamic_to_json(data: Dynamic) -> json.Json {
  dynamic_to_json_ffi(data)
}

fn encode_errors(errors: List(executor.ExecutionError)) -> json.Json {
  json.array(errors, fn(err) {
    let #(msg, path) = case err {
      executor.ValidationError(m, p, _loc) -> #(m, p)
      executor.ResolverError(m, p, _loc) -> #(m, p)
      executor.TypeError(m, p, _loc) -> #(m, p)
      executor.NullValueError(m, p, _loc) -> #(m, p)
    }
    case path {
      [] -> json.object([#("message", json.string(msg))])
      _ ->
        json.object([
          #("message", json.string(msg)),
          #("path", json.array(path, json.string)),
        ])
    }
  })
}

// ============================================================================
// Error Responses
// ============================================================================

fn error_response(message: String) -> WispResponse {
  let body =
    json.to_string(
      json.object([
        #(
          "errors",
          json.array([message], fn(m) {
            json.object([#("message", json.string(m))])
          }),
        ),
      ]),
    )

  wisp.json_response(body, 400)
  |> add_cors_headers
}

// ============================================================================
// CORS Headers
// ============================================================================

fn add_cors_headers(response: WispResponse) -> WispResponse {
  response
  |> wisp.set_header("Access-Control-Allow-Origin", "*")
  |> wisp.set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  |> wisp.set_header("Access-Control-Allow-Headers", "Content-Type")
}

/// Handle CORS preflight request
pub fn handle_cors_preflight() -> WispResponse {
  logging.log(logging.Debug, "CORS preflight request")
  wisp.response(204)
  |> add_cors_headers
  |> wisp.set_header("Access-Control-Max-Age", "86400")
}
