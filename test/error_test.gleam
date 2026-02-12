// Tests for GraphQL error extensions

import gleam/dict
import gleam/option.{None, Some}
import mochi/error
import mochi/types

// ============================================================================
// Basic Error Construction Tests
// ============================================================================

pub fn simple_error_test() {
  let err = error.error("Something went wrong")

  case err.message == "Something went wrong" {
    True -> Nil
    False -> panic as "Message should be 'Something went wrong'"
  }

  case err.locations {
    None -> Nil
    Some(_) -> panic as "Locations should be None"
  }

  case err.path {
    None -> Nil
    Some(_) -> panic as "Path should be None"
  }

  case err.extensions {
    None -> Nil
    Some(_) -> panic as "Extensions should be None"
  }
}

pub fn error_with_path_test() {
  let err =
    error.error_with_path("Field not found", [
      error.FieldSegment("user"),
      error.FieldSegment("email"),
    ])

  case err.path {
    Some([error.FieldSegment("user"), error.FieldSegment("email")]) -> Nil
    _ -> panic as "Path should be [user, email]"
  }
}

pub fn error_at_test() {
  let err = error.error_at("Invalid field", ["query", "users", "name"])

  case err.path {
    Some([
      error.FieldSegment("query"),
      error.FieldSegment("users"),
      error.FieldSegment("name"),
    ]) -> Nil
    _ -> panic as "Path should be [query, users, name]"
  }
}

// ============================================================================
// Location Tests
// ============================================================================

pub fn error_with_location_test() {
  let err =
    error.error("Syntax error")
    |> error.at_location(10, 5)

  case err.locations {
    Some([error.Location(10, 5)]) -> Nil
    _ -> panic as "Should have one location at line 10, column 5"
  }
}

pub fn error_with_multiple_locations_test() {
  let err =
    error.error("Multiple errors")
    |> error.at_location(10, 5)
    |> error.at_location(15, 3)

  case err.locations {
    Some([error.Location(10, 5), error.Location(15, 3)]) -> Nil
    _ -> panic as "Should have two locations"
  }
}

pub fn error_with_locations_list_test() {
  let locs = [error.Location(1, 1), error.Location(2, 2), error.Location(3, 3)]
  let err =
    error.error("Test")
    |> error.with_locations(locs)

  case err.locations {
    Some(locations) -> {
      case locations {
        [error.Location(1, 1), error.Location(2, 2), error.Location(3, 3)] -> Nil
        _ -> panic as "Should have three locations"
      }
    }
    None -> panic as "Should have locations"
  }
}

// ============================================================================
// Extension Tests
// ============================================================================

pub fn error_with_extension_test() {
  let err =
    error.error("Test error")
    |> error.with_extension("code", types.to_dynamic("SOME_CODE"))

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "Should have 'code' extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn error_with_multiple_extensions_test() {
  let err =
    error.error("Test error")
    |> error.with_extension("code", types.to_dynamic("ERROR_CODE"))
    |> error.with_extension("timestamp", types.to_dynamic(1234567890))
    |> error.with_extension("requestId", types.to_dynamic("req-123"))

  case err.extensions {
    Some(ext) -> {
      case dict.size(ext) == 3 {
        True -> Nil
        False -> panic as "Should have 3 extensions"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn error_with_extensions_dict_test() {
  let extensions =
    dict.from_list([
      #("key1", types.to_dynamic("value1")),
      #("key2", types.to_dynamic(42)),
    ])

  let err =
    error.error("Test")
    |> error.with_extensions(extensions)

  case err.extensions {
    Some(ext) -> {
      case dict.size(ext) == 2 {
        True -> Nil
        False -> panic as "Should have 2 extensions"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn error_with_category_test() {
  let err =
    error.error("Test")
    |> error.with_category(error.ValidationErrorCategory)

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "category") {
        True -> Nil
        False -> panic as "Should have 'category' extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn error_with_code_test() {
  let err =
    error.error("Test")
    |> error.with_code("CUSTOM_ERROR_CODE")

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "Should have 'code' extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

// ============================================================================
// Convenience Constructor Tests
// ============================================================================

pub fn validation_error_test() {
  let err = error.validation_error("Field not found", ["user", "email"])

  case err.message == "Field not found" {
    True -> Nil
    False -> panic as "Message should be 'Field not found'"
  }

  case err.path {
    Some([error.FieldSegment("user"), error.FieldSegment("email")]) -> Nil
    _ -> panic as "Path should be [user, email]"
  }

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "category") {
        True -> Nil
        False -> panic as "Should have category extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn resolver_error_test() {
  let err = error.resolver_error("Database error", ["query", "users"])

  case err.message == "Database error" {
    True -> Nil
    False -> panic as "Message should be 'Database error'"
  }
}

pub fn type_error_test() {
  let err = error.type_error("Expected String, got Int", ["user", "age"])

  case err.message == "Expected String, got Int" {
    True -> Nil
    False -> panic as "Message should be 'Expected String, got Int'"
  }
}

pub fn authentication_error_test() {
  let err = error.authentication_error("Not authenticated")

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "Should have 'code' extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn authorization_error_test() {
  let err =
    error.authorization_error("Access denied to admin field", ["user", "role"])

  case err.path {
    Some([error.FieldSegment("user"), error.FieldSegment("role")]) -> Nil
    _ -> panic as "Should have path [user, role]"
  }
}

pub fn user_input_error_test() {
  let err = error.user_input_error("Invalid email format", "email", ["input"])

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "field") && dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "Should have 'field' and 'code' extensions"
      }
    }
    None -> panic as "Should have extensions"
  }
}

pub fn internal_error_test() {
  let err = error.internal_error("Unexpected server error")

  case err.extensions {
    Some(ext) -> {
      case dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "Should have 'code' extension"
      }
    }
    None -> panic as "Should have extensions"
  }
}

// ============================================================================
// Serialization Tests
// ============================================================================

pub fn error_to_dynamic_basic_test() {
  let err = error.error("Test message")
  let dyn = error.to_dynamic(err)

  // Should produce a Dynamic value (we can't easily inspect it without FFI)
  // Just verify it doesn't crash
  Nil
}

pub fn error_to_dynamic_full_test() {
  let err =
    error.error("Complete error")
    |> error.at_location(10, 5)
    |> error.with_path([
      error.FieldSegment("query"),
      error.FieldSegment("users"),
      error.IndexSegment(0),
      error.FieldSegment("name"),
    ])
    |> error.with_extension("code", types.to_dynamic("FULL_ERROR"))
    |> error.with_extension("timestamp", types.to_dynamic(1234567890))

  let dyn = error.to_dynamic(err)

  // Should produce a Dynamic value
  Nil
}

pub fn errors_to_dynamic_test() {
  let errors = [
    error.error("Error 1"),
    error.error("Error 2"),
    error.error("Error 3"),
  ]

  let dyn = error.errors_to_dynamic(errors)

  // Should produce a list Dynamic
  Nil
}

// ============================================================================
// Formatting Tests
// ============================================================================

pub fn format_simple_error_test() {
  let err = error.error("Simple error message")
  let formatted = error.format(err)

  case formatted == "Simple error message" {
    True -> Nil
    False -> panic as "Format should be 'Simple error message'"
  }
}

pub fn format_error_with_path_test() {
  let err = error.error_at("Field error", ["user", "email"])
  let formatted = error.format(err)

  case formatted == "Field error at user.email" {
    True -> Nil
    False -> panic as { "Format should be 'Field error at user.email', got: " <> formatted }
  }
}

pub fn format_error_with_index_path_test() {
  let err =
    error.error_with_path("Array error", [
      error.FieldSegment("users"),
      error.IndexSegment(5),
      error.FieldSegment("name"),
    ])
  let formatted = error.format(err)

  case formatted == "Array error at users.[5].name" {
    True -> Nil
    False ->
      panic as { "Format should be 'Array error at users.[5].name', got: " <> formatted }
  }
}

pub fn format_error_with_location_test() {
  let err =
    error.error("Located error")
    |> error.at_location(10, 5)
  let formatted = error.format(err)

  case formatted == "Located error [(10:5)]" {
    True -> Nil
    False -> panic as { "Format should be 'Located error [(10:5)]', got: " <> formatted }
  }
}

// ============================================================================
// Path Helper Tests
// ============================================================================

pub fn path_from_strings_test() {
  let path = error.path_from_strings(["a", "b", "c"])

  case path {
    [
      error.FieldSegment("a"),
      error.FieldSegment("b"),
      error.FieldSegment("c"),
    ] -> Nil
    _ -> panic as "Should convert strings to field segments"
  }
}

pub fn append_index_test() {
  let path = [error.FieldSegment("users")]
  let new_path = error.append_index(path, 5)

  case new_path {
    [error.FieldSegment("users"), error.IndexSegment(5)] -> Nil
    _ -> panic as "Should append index segment"
  }
}

pub fn append_field_test() {
  let path = [error.FieldSegment("query")]
  let new_path = error.append_field(path, "users")

  case new_path {
    [error.FieldSegment("query"), error.FieldSegment("users")] -> Nil
    _ -> panic as "Should append field segment"
  }
}
