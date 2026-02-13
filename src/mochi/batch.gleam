// mochi/batch.gleam
// Batched query execution for GraphQL

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/ast
import mochi/executor.{type ExecutionResult}
import mochi/parser
import mochi/schema.{type ExecutionContext, type Schema}

// ============================================================================
// Types
// ============================================================================

/// A single request in a batch
pub type BatchRequest {
  BatchRequest(
    /// The GraphQL query string
    query: String,
    /// Optional variables for the query
    variables: Option(Dict(String, Dynamic)),
    /// Optional operation name to execute
    operation_name: Option(String),
  )
}

/// Configuration for batch execution
pub type BatchConfig {
  BatchConfig(
    /// Maximum number of queries allowed in a single batch
    max_batch_size: Int,
    /// Whether to continue executing remaining queries if one fails
    continue_on_error: Bool,
    /// Whether to allow parallel execution (when possible)
    allow_parallel: Bool,
  )
}

/// Result of a batch execution
pub type BatchResult {
  BatchResult(
    /// Results in the same order as requests
    results: List(ExecutionResult),
    /// Whether all queries succeeded
    all_succeeded: Bool,
    /// Number of failed queries
    failure_count: Int,
  )
}

// ============================================================================
// Configuration
// ============================================================================

/// Create default batch configuration
pub fn default_config() -> BatchConfig {
  BatchConfig(
    max_batch_size: 10,
    continue_on_error: True,
    allow_parallel: False,
  )
}

/// Set maximum batch size
pub fn with_max_batch_size(config: BatchConfig, size: Int) -> BatchConfig {
  BatchConfig(..config, max_batch_size: size)
}

/// Configure whether to continue on error
pub fn with_continue_on_error(
  config: BatchConfig,
  continue: Bool,
) -> BatchConfig {
  BatchConfig(..config, continue_on_error: continue)
}

/// Configure parallel execution
pub fn with_parallel_execution(
  config: BatchConfig,
  parallel: Bool,
) -> BatchConfig {
  BatchConfig(..config, allow_parallel: parallel)
}

// ============================================================================
// Batch Request Construction
// ============================================================================

/// Create a batch request with just a query
pub fn request(query: String) -> BatchRequest {
  BatchRequest(query: query, variables: None, operation_name: None)
}

/// Create a batch request with variables
pub fn request_with_variables(
  query: String,
  variables: Dict(String, Dynamic),
) -> BatchRequest {
  BatchRequest(query: query, variables: Some(variables), operation_name: None)
}

/// Create a batch request with operation name
pub fn request_with_operation(
  query: String,
  operation_name: String,
) -> BatchRequest {
  BatchRequest(
    query: query,
    variables: None,
    operation_name: Some(operation_name),
  )
}

/// Create a full batch request
pub fn full_request(
  query: String,
  variables: Dict(String, Dynamic),
  operation_name: String,
) -> BatchRequest {
  BatchRequest(
    query: query,
    variables: Some(variables),
    operation_name: Some(operation_name),
  )
}

// ============================================================================
// Operation Selection
// ============================================================================

/// Execute a query with a specific operation name selected
/// This is the core function for operation selection in batched queries
pub fn execute_with_operation_name(
  schema_def: Schema,
  document: ast.Document,
  root_value: Option(Dynamic),
  ctx: ExecutionContext,
  variables: Dict(String, Dynamic),
  operation_name: Option(String),
) -> ExecutionResult {
  // Find the operation to execute
  case find_operation(document, operation_name) {
    Ok(operation) -> {
      // Create a new document with just this operation (and all fragments)
      let fragments = extract_fragments(document)
      let new_doc =
        ast.Document(definitions: [
          ast.OperationDefinition(operation),
          ..list.map(fragments, fn(f) { ast.FragmentDefinition(f) })
        ])
      executor.execute(schema_def, new_doc, root_value, ctx, variables)
    }
    Error(msg) ->
      executor.ExecutionResult(data: None, errors: [
        executor.ValidationError(msg, []),
      ])
  }
}

/// Find an operation in a document by name
fn find_operation(
  document: ast.Document,
  operation_name: Option(String),
) -> Result(ast.Operation, String) {
  let operations =
    document.definitions
    |> list.filter_map(fn(def) {
      case def {
        ast.OperationDefinition(op) -> Ok(op)
        ast.FragmentDefinition(_) -> Error(Nil)
      }
    })

  case operation_name, operations {
    // No name specified, must have exactly one operation
    None, [single] -> Ok(single)
    None, [] -> Error("Document contains no operations")
    None, _ ->
      Error("Document contains multiple operations, operation name is required")

    // Name specified, find matching operation
    Some(name), ops -> {
      ops
      |> list.find(fn(op) { get_operation_name(op) == Some(name) })
      |> result.map_error(fn(_) {
        "Operation '" <> name <> "' not found in document"
      })
    }
  }
}

/// Get the name of an operation
fn get_operation_name(operation: ast.Operation) -> Option(String) {
  case operation {
    ast.Operation(name: name, ..) -> name
    ast.ShorthandQuery(_) -> None
  }
}

/// Extract all fragment definitions from a document
fn extract_fragments(document: ast.Document) -> List(ast.Fragment) {
  document.definitions
  |> list.filter_map(fn(def) {
    case def {
      ast.FragmentDefinition(fragment) -> Ok(fragment)
      ast.OperationDefinition(_) -> Error(Nil)
    }
  })
}

// ============================================================================
// Batch Execution
// ============================================================================

/// Execute a batch of requests
pub fn execute_batch(
  schema_def: Schema,
  requests: List(BatchRequest),
  config: BatchConfig,
  ctx: ExecutionContext,
) -> BatchResult {
  // Check batch size
  case list.length(requests) > config.max_batch_size {
    True ->
      BatchResult(
        results: [
          executor.ExecutionResult(data: None, errors: [
            executor.ValidationError(
              "Batch size exceeds maximum of "
                <> string.inspect(config.max_batch_size),
              [],
            ),
          ]),
        ],
        all_succeeded: False,
        failure_count: 1,
      )
    False -> execute_batch_internal(schema_def, requests, config, ctx)
  }
}

fn execute_batch_internal(
  schema_def: Schema,
  requests: List(BatchRequest),
  config: BatchConfig,
  ctx: ExecutionContext,
) -> BatchResult {
  // Execute each request
  let results =
    list.fold(requests, #([], False), fn(acc, req) {
      let #(results_acc, has_failure) = acc
      case config.continue_on_error, has_failure {
        // If not continuing on error and we have a failure, skip
        False, True -> #(
          [
            executor.ExecutionResult(data: None, errors: [
              executor.ValidationError(
                "Batch execution halted due to error",
                [],
              ),
            ]),
            ..results_acc
          ],
          True,
        )
        // Otherwise execute the request
        _, _ -> {
          let result = execute_single_request(schema_def, req, ctx)
          let new_has_failure = has_failure || has_errors(result)
          #([result, ..results_acc], new_has_failure)
        }
      }
    })

  let final_results = list.reverse(results.0)

  // Calculate statistics
  let failure_count =
    final_results
    |> list.count(has_errors)

  BatchResult(
    results: final_results,
    all_succeeded: failure_count == 0,
    failure_count: failure_count,
  )
}

fn execute_single_request(
  schema_def: Schema,
  req: BatchRequest,
  ctx: ExecutionContext,
) -> ExecutionResult {
  let variables = option.unwrap(req.variables, dict.new())

  case parser.parse(req.query) {
    Ok(document) ->
      execute_with_operation_name(
        schema_def,
        document,
        None,
        ctx,
        variables,
        req.operation_name,
      )
    Error(parse_error) ->
      executor.ExecutionResult(data: None, errors: [
        executor.ValidationError(
          "Parse error: " <> format_parse_error(parse_error),
          [],
        ),
      ])
  }
}

fn has_errors(result: ExecutionResult) -> Bool {
  !list.is_empty(result.errors)
}

fn format_parse_error(error: parser.ParseError) -> String {
  case error {
    parser.LexError(_) -> "Lexer error"
    parser.UnexpectedToken(expected, _, _) -> "Expected " <> expected
    parser.UnexpectedEOF(expected) -> "Unexpected EOF, expected " <> expected
  }
}

// ============================================================================
// JSON Parsing Helpers
// ============================================================================

/// Parse a batch request from a dynamic value (e.g., from JSON)
pub fn parse_batch_request(value: Dynamic) -> Result(BatchRequest, String) {
  case extract_query(value) {
    Ok(query) -> {
      let variables = extract_variables(value)
      let operation_name = extract_operation_name(value)
      Ok(BatchRequest(
        query: query,
        variables: variables,
        operation_name: operation_name,
      ))
    }
    Error(e) -> Error(e)
  }
}

/// Parse multiple batch requests from a dynamic list
pub fn parse_batch_requests(
  value: Dynamic,
) -> Result(List(BatchRequest), String) {
  case get_list_raw(value) {
    Ok(items) -> {
      let results = list.map(items, parse_batch_request)
      let errors =
        results
        |> list.filter_map(fn(r) {
          case r {
            Error(e) -> Ok(e)
            Ok(_) -> Error(Nil)
          }
        })

      case errors {
        [] ->
          Ok(
            list.filter_map(results, fn(r) {
              case r {
                Ok(req) -> Ok(req)
                Error(_) -> Error(Nil)
              }
            }),
          )
        [first, ..] -> Error("Failed to parse batch request: " <> first)
      }
    }
    Error(_) -> {
      // Try parsing as a single request
      case parse_batch_request(value) {
        Ok(req) -> Ok([req])
        Error(e) -> Error(e)
      }
    }
  }
}

fn extract_query(value: Dynamic) -> Result(String, String) {
  case extract_string_field(value, "query") {
    Ok(q) -> Ok(q)
    Error(_) -> Error("Missing or invalid 'query' field")
  }
}

fn extract_variables(value: Dynamic) -> Option(Dict(String, Dynamic)) {
  case extract_dict_field(value, "variables") {
    Ok(vars) -> Some(vars)
    Error(_) -> None
  }
}

fn extract_operation_name(value: Dynamic) -> Option(String) {
  case extract_string_field(value, "operationName") {
    Ok(name) -> Some(name)
    Error(_) -> None
  }
}

fn get_list_raw(value: Dynamic) -> Result(List(Dynamic), Nil) {
  decode.run(value, decode.list(decode.dynamic))
  |> result.map_error(fn(_) { Nil })
}

fn extract_string_field(value: Dynamic, field: String) -> Result(String, Nil) {
  decode.run(value, decode.at([field], decode.string))
  |> result.map_error(fn(_) { Nil })
}

fn extract_dict_field(
  value: Dynamic,
  field: String,
) -> Result(Dict(String, Dynamic), Nil) {
  decode.run(value, decode.at([field], decode.dict(decode.string, decode.dynamic)))
  |> result.map_error(fn(_) { Nil })
}
