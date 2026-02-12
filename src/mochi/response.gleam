// mochi/response.gleam
// GraphQL response serialization
//
// Per the GraphQL spec, responses should have the shape:
// {
//   "data": { ... } | null,
//   "errors": [ ... ] | undefined,
//   "extensions": { ... } | undefined
// }

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/error.{type GraphQLError}
import mochi/executor.{type ExecutionError, type ExecutionResult}
import mochi/json
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// A complete GraphQL response
pub type GraphQLResponse {
  GraphQLResponse(
    /// The data returned from executing the requested operation
    data: Option(Dynamic),
    /// Errors that occurred during execution
    errors: Option(List(GraphQLError)),
    /// Additional metadata from extensions
    extensions: Option(Dict(String, Dynamic)),
  )
}

// ============================================================================
// Response Builders
// ============================================================================

/// Create a successful response with data
pub fn success(data: Dynamic) -> GraphQLResponse {
  GraphQLResponse(data: Some(data), errors: None, extensions: None)
}

/// Create an error-only response
pub fn failure(errors: List(GraphQLError)) -> GraphQLResponse {
  GraphQLResponse(data: None, errors: Some(errors), extensions: None)
}

/// Create a partial response with data and errors
pub fn partial(data: Dynamic, errors: List(GraphQLError)) -> GraphQLResponse {
  GraphQLResponse(data: Some(data), errors: Some(errors), extensions: None)
}

/// Create a response from an ExecutionResult
pub fn from_execution_result(result: ExecutionResult) -> GraphQLResponse {
  let errors = case result.errors {
    [] -> None
    errs -> Some(list.map(errs, execution_error_to_graphql_error))
  }

  GraphQLResponse(data: result.data, errors: errors, extensions: None)
}

/// Add extensions to a response
pub fn with_extensions(
  response: GraphQLResponse,
  extensions: Dict(String, Dynamic),
) -> GraphQLResponse {
  GraphQLResponse(..response, extensions: Some(extensions))
}

/// Add a single extension value
pub fn with_extension(
  response: GraphQLResponse,
  key: String,
  value: Dynamic,
) -> GraphQLResponse {
  let extensions = case response.extensions {
    Some(ext) -> dict.insert(ext, key, value)
    None -> dict.from_list([#(key, value)])
  }
  GraphQLResponse(..response, extensions: Some(extensions))
}

/// Add tracing data to response extensions
pub fn with_tracing(
  response: GraphQLResponse,
  start_time: Int,
  end_time: Int,
) -> GraphQLResponse {
  let tracing =
    types.to_dynamic(
      dict.from_list([
        #("version", types.to_dynamic(1)),
        #("startTime", types.to_dynamic(start_time)),
        #("endTime", types.to_dynamic(end_time)),
        #("duration", types.to_dynamic(end_time - start_time)),
      ]),
    )
  with_extension(response, "tracing", tracing)
}

// ============================================================================
// Serialization
// ============================================================================

/// Convert a GraphQLResponse to a Dynamic representation for JSON serialization
pub fn to_dynamic(response: GraphQLResponse) -> Dynamic {
  let parts = []

  // Always include data (even if null)
  let parts = [
    #(
      "data",
      case response.data {
        Some(d) -> d
        None -> types.to_dynamic(Nil)
      },
    ),
    ..parts
  ]

  // Only include errors if present
  let parts = case response.errors {
    Some(errors) -> [#("errors", error.errors_to_dynamic(errors)), ..parts]
    None -> parts
  }

  // Only include extensions if present
  let parts = case response.extensions {
    Some(ext) -> [#("extensions", types.to_dynamic(ext)), ..parts]
    None -> parts
  }

  types.to_dynamic(dict.from_list(parts))
}

/// Serialize response to JSON string (requires external JSON encoder)
/// This returns the Dynamic representation - use with a JSON library
pub fn serialize(response: GraphQLResponse) -> Dynamic {
  to_dynamic(response)
}

/// Convert a GraphQLResponse to a JSON string
pub fn to_json(response: GraphQLResponse) -> String {
  response
  |> to_dynamic
  |> json.encode
}

/// Convert a GraphQLResponse to a pretty-printed JSON string
pub fn to_json_pretty(response: GraphQLResponse) -> String {
  response
  |> to_dynamic
  |> json.encode_pretty(2)
}

// ============================================================================
// Error Conversion
// ============================================================================

/// Convert an ExecutionError to a GraphQLError
pub fn execution_error_to_graphql_error(err: ExecutionError) -> GraphQLError {
  case err {
    executor.ValidationError(message, path) ->
      error.error_at(message, path)
      |> error.with_category(error.ValidationErrorCategory)
    executor.ResolverError(message, path) ->
      error.error_at(message, path)
      |> error.with_category(error.ResolverErrorCategory)
    executor.TypeError(message, path) ->
      error.error_at(message, path)
      |> error.with_category(error.TypeErrorCategory)
    executor.NullValueError(message, path) ->
      error.error_at(message, path)
      |> error.with_code("NULL_VALUE_ERROR")
      |> error.with_category(error.ResolverErrorCategory)
  }
}

// ============================================================================
// Response Inspection
// ============================================================================

/// Check if response has errors
pub fn has_errors(response: GraphQLResponse) -> Bool {
  case response.errors {
    Some(errors) -> list.length(errors) > 0
    None -> False
  }
}

/// Check if response has data
pub fn has_data(response: GraphQLResponse) -> Bool {
  case response.data {
    Some(_) -> True
    None -> False
  }
}

/// Get error count
pub fn error_count(response: GraphQLResponse) -> Int {
  case response.errors {
    Some(errors) -> list.length(errors)
    None -> 0
  }
}

/// Check if response is successful (has data, no errors)
pub fn is_success(response: GraphQLResponse) -> Bool {
  has_data(response) && !has_errors(response)
}

/// Check if response is partial (has data and errors)
pub fn is_partial(response: GraphQLResponse) -> Bool {
  has_data(response) && has_errors(response)
}

// ============================================================================
// Pretty Formatting
// ============================================================================

/// Format response for debugging/logging
pub fn format(response: GraphQLResponse) -> String {
  let data_str = case response.data {
    Some(_) -> "data: <present>"
    None -> "data: null"
  }

  let errors_str = case response.errors {
    Some(errors) -> {
      let count = list.length(errors)
      case count {
        1 -> "errors: [1 error]"
        n -> "errors: [" <> int_to_string(n) <> " errors]"
      }
    }
    None -> "errors: none"
  }

  let ext_str = case response.extensions {
    Some(_) -> "extensions: <present>"
    None -> "extensions: none"
  }

  "GraphQLResponse { " <> data_str <> ", " <> errors_str <> ", " <> ext_str <> " }"
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ -> {
      case n < 0 {
        True -> "-" <> int_to_string(-n)
        False -> int_to_string(n / 10) <> int_to_string(n % 10)
      }
    }
  }
}
