// mochi/error.gleam
// GraphQL-spec compliant error types with extensions support
//
// Per the GraphQL spec, errors should include:
// - message: Required human-readable error message
// - locations: Optional list of source locations (line, column)
// - path: Optional path to the field that caused the error
// - extensions: Optional map of additional error metadata

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mochi/types

// ============================================================================
// Core Error Types
// ============================================================================

/// A GraphQL source location
pub type Location {
  Location(line: Int, column: Int)
}

/// Path segment - can be field name or array index
pub type PathSegment {
  FieldSegment(name: String)
  IndexSegment(index: Int)
}

/// GraphQL-spec compliant error with extensions support
pub type GraphQLError {
  GraphQLError(
    /// Required: Human-readable error message
    message: String,
    /// Optional: Source locations that caused the error
    locations: Option(List(Location)),
    /// Optional: Path to the field that caused the error
    path: Option(List(PathSegment)),
    /// Optional: Additional error metadata
    extensions: Option(Dict(String, Dynamic)),
  )
}

/// Error category for extensions
pub type ErrorCategory {
  ValidationErrorCategory
  ResolverErrorCategory
  TypeErrorCategory
  AuthenticationErrorCategory
  AuthorizationErrorCategory
  InternalErrorCategory
  UserInputErrorCategory
}

// ============================================================================
// Error Builders
// ============================================================================

/// Create a simple error with just a message
pub fn error(message: String) -> GraphQLError {
  GraphQLError(message: message, locations: None, path: None, extensions: None)
}

/// Create an error with a path
pub fn error_with_path(message: String, path: List(PathSegment)) -> GraphQLError {
  GraphQLError(
    message: message,
    locations: None,
    path: Some(path),
    extensions: None,
  )
}

/// Create an error from string path segments (convenience)
pub fn error_at(message: String, path: List(String)) -> GraphQLError {
  GraphQLError(
    message: message,
    locations: None,
    path: Some(list.map(path, FieldSegment)),
    extensions: None,
  )
}

/// Add a location to an error
pub fn at_location(err: GraphQLError, line: Int, column: Int) -> GraphQLError {
  let new_loc = Location(line, column)
  let locations = case err.locations {
    Some(locs) -> Some(list.append(locs, [new_loc]))
    None -> Some([new_loc])
  }
  GraphQLError(..err, locations: locations)
}

/// Add locations to an error
pub fn with_locations(
  err: GraphQLError,
  locations: List(Location),
) -> GraphQLError {
  GraphQLError(..err, locations: Some(locations))
}

/// Set the path on an error
pub fn with_path(err: GraphQLError, path: List(PathSegment)) -> GraphQLError {
  GraphQLError(..err, path: Some(path))
}

/// Add an extension to an error
pub fn with_extension(
  err: GraphQLError,
  key: String,
  value: Dynamic,
) -> GraphQLError {
  let extensions = case err.extensions {
    Some(ext) -> dict.insert(ext, key, value)
    None -> dict.from_list([#(key, value)])
  }
  GraphQLError(..err, extensions: Some(extensions))
}

/// Set multiple extensions on an error
pub fn with_extensions(
  err: GraphQLError,
  extensions: Dict(String, Dynamic),
) -> GraphQLError {
  GraphQLError(..err, extensions: Some(extensions))
}

/// Set error category in extensions
pub fn with_category(err: GraphQLError, category: ErrorCategory) -> GraphQLError {
  with_extension(err, "category", types.to_dynamic(category_to_string(category)))
}

/// Set error code in extensions
pub fn with_code(err: GraphQLError, code: String) -> GraphQLError {
  with_extension(err, "code", types.to_dynamic(code))
}

// ============================================================================
// Convenience Error Constructors
// ============================================================================

/// Create a validation error
pub fn validation_error(message: String, path: List(String)) -> GraphQLError {
  error_at(message, path)
  |> with_category(ValidationErrorCategory)
}

/// Create a resolver error
pub fn resolver_error(message: String, path: List(String)) -> GraphQLError {
  error_at(message, path)
  |> with_category(ResolverErrorCategory)
}

/// Create a type error
pub fn type_error(message: String, path: List(String)) -> GraphQLError {
  error_at(message, path)
  |> with_category(TypeErrorCategory)
}

/// Create an authentication error
pub fn authentication_error(message: String) -> GraphQLError {
  error(message)
  |> with_category(AuthenticationErrorCategory)
  |> with_code("UNAUTHENTICATED")
}

/// Create an authorization error
pub fn authorization_error(message: String, path: List(String)) -> GraphQLError {
  error_at(message, path)
  |> with_category(AuthorizationErrorCategory)
  |> with_code("FORBIDDEN")
}

/// Create a user input error
pub fn user_input_error(
  message: String,
  field: String,
  path: List(String),
) -> GraphQLError {
  error_at(message, path)
  |> with_category(UserInputErrorCategory)
  |> with_code("BAD_USER_INPUT")
  |> with_extension("field", types.to_dynamic(field))
}

/// Create an internal server error
pub fn internal_error(message: String) -> GraphQLError {
  error(message)
  |> with_category(InternalErrorCategory)
  |> with_code("INTERNAL_SERVER_ERROR")
}

// ============================================================================
// Serialization
// ============================================================================

/// Convert a GraphQLError to a Dynamic representation for JSON serialization
pub fn to_dynamic(err: GraphQLError) -> Dynamic {
  let base = [#("message", types.to_dynamic(err.message))]

  // Add locations if present
  let with_locations = case err.locations {
    Some(locs) -> [
      #(
        "locations",
        types.to_dynamic(
          list.map(locs, fn(loc) {
            types.to_dynamic(
              dict.from_list([
                #("line", types.to_dynamic(loc.line)),
                #("column", types.to_dynamic(loc.column)),
              ]),
            )
          }),
        ),
      ),
      ..base
    ]
    None -> base
  }

  // Add path if present
  let with_path = case err.path {
    Some(path_segments) -> [
      #("path", types.to_dynamic(list.map(path_segments, path_segment_to_dynamic))),
      ..with_locations
    ]
    None -> with_locations
  }

  // Add extensions if present
  let final = case err.extensions {
    Some(ext) -> [#("extensions", types.to_dynamic(ext)), ..with_path]
    None -> with_path
  }

  types.to_dynamic(dict.from_list(final))
}

/// Convert multiple errors to a list
pub fn errors_to_dynamic(errors: List(GraphQLError)) -> Dynamic {
  types.to_dynamic(list.map(errors, to_dynamic))
}

fn path_segment_to_dynamic(segment: PathSegment) -> Dynamic {
  case segment {
    FieldSegment(name) -> types.to_dynamic(name)
    IndexSegment(index) -> types.to_dynamic(index)
  }
}

// ============================================================================
// Formatting
// ============================================================================

/// Format an error as a human-readable string
pub fn format(err: GraphQLError) -> String {
  let msg = err.message

  let with_path = case err.path {
    Some(path_segments) -> msg <> " at " <> format_path(path_segments)
    None -> msg
  }

  let with_locations = case err.locations {
    Some(locs) -> with_path <> " " <> format_locations(locs)
    None -> with_path
  }

  with_locations
}

fn format_path(segments: List(PathSegment)) -> String {
  segments
  |> list.map(fn(s) {
    case s {
      FieldSegment(name) -> name
      IndexSegment(index) -> "[" <> int.to_string(index) <> "]"
    }
  })
  |> string.join(".")
}

fn format_locations(locations: List(Location)) -> String {
  let loc_strs =
    list.map(locations, fn(loc) {
      "(" <> int.to_string(loc.line) <> ":" <> int.to_string(loc.column) <> ")"
    })
  case loc_strs {
    [] -> ""
    _ -> "[" <> string.join(loc_strs, ", ") <> "]"
  }
}

// ============================================================================
// Helpers
// ============================================================================

fn category_to_string(category: ErrorCategory) -> String {
  case category {
    ValidationErrorCategory -> "VALIDATION"
    ResolverErrorCategory -> "RESOLVER"
    TypeErrorCategory -> "TYPE"
    AuthenticationErrorCategory -> "AUTHENTICATION"
    AuthorizationErrorCategory -> "AUTHORIZATION"
    InternalErrorCategory -> "INTERNAL"
    UserInputErrorCategory -> "USER_INPUT"
  }
}

/// Convert string path to PathSegment list
pub fn path_from_strings(path: List(String)) -> List(PathSegment) {
  list.map(path, FieldSegment)
}

/// Add index segment to path
pub fn append_index(path: List(PathSegment), index: Int) -> List(PathSegment) {
  list.append(path, [IndexSegment(index)])
}

/// Add field segment to path
pub fn append_field(path: List(PathSegment), field: String) -> List(PathSegment) {
  list.append(path, [FieldSegment(field)])
}
