// Tests for GraphQL response serialization

import gleam/dict
import gleam/option.{None, Some}
import mochi/error
import mochi/executor
import mochi/response
import mochi/types

// ============================================================================
// Response Construction Tests
// ============================================================================

pub fn success_response_test() {
  let data =
    types.to_dynamic(dict.from_list([#("user", types.to_dynamic("test"))]))
  let resp = response.success(data)

  case response.has_data(resp) {
    True -> Nil
    False -> panic as "Success response should have data"
  }

  case response.has_errors(resp) {
    False -> Nil
    True -> panic as "Success response should not have errors"
  }

  case response.is_success(resp) {
    True -> Nil
    False -> panic as "Should be classified as success"
  }
}

pub fn failure_response_test() {
  let errors = [error.error("Something went wrong")]
  let resp = response.failure(errors)

  case response.has_data(resp) {
    False -> Nil
    True -> panic as "Failure response should not have data"
  }

  case response.has_errors(resp) {
    True -> Nil
    False -> panic as "Failure response should have errors"
  }

  case response.error_count(resp) == 1 {
    True -> Nil
    False -> panic as "Should have 1 error"
  }
}

pub fn partial_response_test() {
  let data =
    types.to_dynamic(dict.from_list([#("partial", types.to_dynamic("data"))]))
  let errors = [error.error("Partial error")]
  let resp = response.partial(data, errors)

  case response.has_data(resp) {
    True -> Nil
    False -> panic as "Partial response should have data"
  }

  case response.has_errors(resp) {
    True -> Nil
    False -> panic as "Partial response should have errors"
  }

  case response.is_partial(resp) {
    True -> Nil
    False -> panic as "Should be classified as partial"
  }
}

// ============================================================================
// From ExecutionResult Tests
// ============================================================================

pub fn from_execution_result_success_test() {
  let data =
    types.to_dynamic(dict.from_list([#("result", types.to_dynamic("ok"))]))
  let exec_result = executor.ExecutionResult(data: Some(data), errors: [])

  let resp = response.from_execution_result(exec_result)

  case response.is_success(resp) {
    True -> Nil
    False -> panic as "Should be success"
  }
}

pub fn from_execution_result_with_errors_test() {
  let data =
    types.to_dynamic(dict.from_list([#("partial", types.to_dynamic("data"))]))
  let errors = [
    executor.ValidationError("Field not found", ["user", "email"], location: None),
    executor.ResolverError("Database error", ["query", "users"], location: None),
  ]
  let exec_result = executor.ExecutionResult(data: Some(data), errors: errors)

  let resp = response.from_execution_result(exec_result)

  case response.is_partial(resp) {
    True -> Nil
    False -> panic as "Should be partial"
  }

  case response.error_count(resp) == 2 {
    True -> Nil
    False -> panic as "Should have 2 errors"
  }
}

pub fn from_execution_result_failure_test() {
  let errors = [executor.TypeError("Type mismatch", ["field"], location: None)]
  let exec_result = executor.ExecutionResult(data: None, errors: errors)

  let resp = response.from_execution_result(exec_result)

  case response.has_data(resp) {
    False -> Nil
    True -> panic as "Should not have data"
  }

  case response.has_errors(resp) {
    True -> Nil
    False -> panic as "Should have errors"
  }
}

// ============================================================================
// Extension Tests
// ============================================================================

pub fn response_with_extension_test() {
  let data = types.to_dynamic("test")
  let resp =
    response.success(data)
    |> response.with_extension("requestId", types.to_dynamic("req-123"))

  case resp.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "requestId") {
        True -> Nil
        False -> panic as "Should have requestId extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn response_with_multiple_extensions_test() {
  let data = types.to_dynamic("test")
  let resp =
    response.success(data)
    |> response.with_extension("key1", types.to_dynamic("value1"))
    |> response.with_extension("key2", types.to_dynamic("value2"))
    |> response.with_extension("key3", types.to_dynamic("value3"))

  case resp.extensions {
    Some(ext) -> {
      case dict.size(ext) == 3 {
        True -> Nil
        False -> panic as "Should have 3 extensions"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn response_with_extensions_dict_test() {
  let data = types.to_dynamic("test")
  let extensions =
    dict.from_list([
      #("a", types.to_dynamic(1)),
      #("b", types.to_dynamic(2)),
    ])

  let resp =
    response.success(data)
    |> response.with_extensions(extensions)

  case resp.extensions {
    Some(ext) -> {
      case dict.size(ext) == 2 {
        True -> Nil
        False -> panic as "Should have 2 extensions"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn response_with_tracing_test() {
  let data = types.to_dynamic("test")
  let resp =
    response.success(data)
    |> response.with_tracing(1000, 1500)

  case resp.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "tracing") {
        True -> Nil
        False -> panic as "Should have tracing extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

// ============================================================================
// Serialization Tests
// ============================================================================

pub fn to_dynamic_success_test() {
  let data =
    types.to_dynamic(dict.from_list([#("user", types.to_dynamic("john"))]))
  let resp = response.success(data)

  let _dyn = response.to_dynamic(resp)

  // Should produce a Dynamic value without crashing
  Nil
}

pub fn to_dynamic_failure_test() {
  let errors = [
    error.error("Error 1"),
    error.error("Error 2")
      |> error.at_location(5, 10),
  ]
  let resp = response.failure(errors)

  let _dyn = response.to_dynamic(resp)

  Nil
}

pub fn to_dynamic_partial_test() {
  let data =
    types.to_dynamic(dict.from_list([#("partial", types.to_dynamic(True))]))
  let errors = [error.validation_error("Warning", ["field"])]
  let resp = response.partial(data, errors)

  let _dyn = response.to_dynamic(resp)

  Nil
}

pub fn to_dynamic_with_extensions_test() {
  let data = types.to_dynamic("data")
  let resp =
    response.success(data)
    |> response.with_extension("custom", types.to_dynamic("value"))

  let _dyn = response.to_dynamic(resp)

  Nil
}

pub fn serialize_test() {
  let data = types.to_dynamic("test")
  let resp = response.success(data)

  let _dyn = response.serialize(resp)

  // Serialize should return same as to_dynamic
  Nil
}

// ============================================================================
// Inspection Tests
// ============================================================================

pub fn has_errors_empty_test() {
  let resp = response.success(types.to_dynamic("test"))

  case response.has_errors(resp) {
    False -> Nil
    True -> panic as "Should not have errors"
  }
}

pub fn has_errors_with_errors_test() {
  let resp = response.failure([error.error("error")])

  case response.has_errors(resp) {
    True -> Nil
    False -> panic as "Should have errors"
  }
}

pub fn has_data_with_data_test() {
  let resp = response.success(types.to_dynamic("data"))

  case response.has_data(resp) {
    True -> Nil
    False -> panic as "Should have data"
  }
}

pub fn has_data_without_data_test() {
  let resp = response.failure([error.error("error")])

  case response.has_data(resp) {
    False -> Nil
    True -> panic as "Should not have data"
  }
}

pub fn error_count_zero_test() {
  let resp = response.success(types.to_dynamic("test"))

  case response.error_count(resp) == 0 {
    True -> Nil
    False -> panic as "Error count should be 0"
  }
}

pub fn error_count_multiple_test() {
  let errors = [
    error.error("Error 1"),
    error.error("Error 2"),
    error.error("Error 3"),
  ]
  let resp = response.failure(errors)

  case response.error_count(resp) == 3 {
    True -> Nil
    False -> panic as "Error count should be 3"
  }
}

pub fn is_success_true_test() {
  let resp = response.success(types.to_dynamic("test"))

  case response.is_success(resp) {
    True -> Nil
    False -> panic as "Should be success"
  }
}

pub fn is_success_false_with_errors_test() {
  let resp = response.failure([error.error("error")])

  case response.is_success(resp) {
    False -> Nil
    True -> panic as "Should not be success"
  }
}

pub fn is_partial_true_test() {
  let resp =
    response.partial(types.to_dynamic("data"), [error.error("warning")])

  case response.is_partial(resp) {
    True -> Nil
    False -> panic as "Should be partial"
  }
}

pub fn is_partial_false_no_errors_test() {
  let resp = response.success(types.to_dynamic("data"))

  case response.is_partial(resp) {
    False -> Nil
    True -> panic as "Should not be partial"
  }
}

// ============================================================================
// Format Tests
// ============================================================================

pub fn format_success_test() {
  let resp = response.success(types.to_dynamic("test"))
  let formatted = response.format(resp)

  // Just verify it produces a string without crashing
  case formatted {
    "" -> panic as "Format should produce non-empty string"
    _ -> Nil
  }
}

pub fn format_failure_test() {
  let resp = response.failure([error.error("error")])
  let formatted = response.format(resp)

  case formatted {
    "" -> panic as "Format should produce non-empty string"
    _ -> Nil
  }
}

pub fn format_with_extensions_test() {
  let resp =
    response.success(types.to_dynamic("test"))
    |> response.with_extension("key", types.to_dynamic("value"))

  let formatted = response.format(resp)

  case formatted {
    "" -> panic as "Format should produce non-empty string"
    _ -> Nil
  }
}

// ============================================================================
// Error Conversion Tests
// ============================================================================

pub fn convert_validation_error_test() {
  let exec_err =
    executor.ValidationError("Field not found", ["user", "email"], location: None)
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.message == "Field not found" {
    True -> Nil
    False -> panic as "Message should be preserved"
  }

  case gql_err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "category") {
        True -> Nil
        False -> panic as "Should have category extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn convert_resolver_error_test() {
  let exec_err =
    executor.ResolverError("Database error", ["query", "users"], location: None)
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.message == "Database error" {
    True -> Nil
    False -> panic as "Message should be preserved"
  }
}

pub fn convert_type_error_test() {
  let exec_err = executor.TypeError("Type mismatch", ["field"], location: None)
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.message == "Type mismatch" {
    True -> Nil
    False -> panic as "Message should be preserved"
  }
}

// ============================================================================
// Error Location Tests (Issue #32)
// ============================================================================

pub fn error_with_location_test() {
  // Test that ValidationError with location gets serialized properly
  let exec_err =
    executor.ValidationError(
      "Field not found",
      ["user", "name"],
      location: Some(#(5, 10)),
    )
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.locations {
    Some([error.Location(line, col)]) -> {
      case line == 5 && col == 10 {
        True -> Nil
        False -> panic as "Location should be line 5, column 10"
      }
    }
    _ -> panic as "Should have exactly one location"
  }
}

pub fn error_without_location_test() {
  // Test that errors without location don't include locations field
  let exec_err =
    executor.ValidationError("Field not found", ["user"], location: None)
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.locations {
    None -> Nil
    Some(_) -> panic as "Should not have locations when None"
  }
}

pub fn resolver_error_with_location_test() {
  let exec_err =
    executor.ResolverError(
      "Database error",
      ["query", "users"],
      location: Some(#(3, 5)),
    )
  let gql_err = response.execution_error_to_graphql_error(exec_err)

  case gql_err.locations {
    Some([error.Location(line, col)]) -> {
      case line == 3 && col == 5 {
        True -> Nil
        False -> panic as "Location should be line 3, column 5"
      }
    }
    _ -> panic as "Should have exactly one location"
  }
}
