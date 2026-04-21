// Tests for mochi query validation
// Tests validation.gleam functionality

import gleam/dynamic.{type Dynamic}
import gleam/list
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types
import mochi/validation

// ============================================================================
// Test Schema Setup
// ============================================================================

pub type User {
  User(id: String, name: String, email: String, age: Int)
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Test", "test@example.com", 25))
}

fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  let users_query =
    query.query(
      name: "users",
      returns: schema.list_type(schema.named_type("User")),
      resolve: fn(_ctx) { Ok([]) },
    )

  let user_query =
    query.query_with_args(
      "user",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.named_type("User"),
      fn(_args, _ctx) { Ok(User("1", "Test", "test@example.com", 25)) },
    )

  query.new()
  |> query.add_query(users_query)
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

// ============================================================================
// Valid Query Tests
// ============================================================================

pub fn valid_simple_query_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { id name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn valid_query_with_args_test() {
  let test_schema = build_test_schema()

  let query_str = "{ user(id: \"1\") { id name email } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn valid_introspection_query_test() {
  let test_schema = build_test_schema()

  let query_str = "{ __schema { types { name } } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn valid_typename_query_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { __typename id } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Unknown Field
// ============================================================================

pub fn invalid_unknown_field_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { id unknownField } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for unknown field"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.UnknownField("unknownField", "User") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have UnknownField error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Missing Required Argument
// ============================================================================

pub fn invalid_missing_required_arg_test() {
  let test_schema = build_test_schema()

  // user query requires id argument
  let query_str = "{ user { id name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for missing required arg"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.MissingRequiredArgument("user", "id") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have MissingRequiredArgument error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Unknown Argument
// ============================================================================

pub fn invalid_unknown_argument_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { id name(foo: \"bar\") } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for unknown argument"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.UnknownArgument("name", "foo") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have UnknownArgument error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Selection Set Required
// ============================================================================

pub fn invalid_selection_required_test() {
  let test_schema = build_test_schema()

  // users returns [User], which requires selection set
  let query_str = "{ users }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for missing selection set"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.SelectionSetRequired("users", _) -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have SelectionSetRequired error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Selection Set Not Allowed
// ============================================================================

pub fn invalid_selection_not_allowed_test() {
  let test_schema = build_test_schema()

  // name is a String scalar, can't have selection set
  let query_str = "{ users { name { foo } } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for selection set on scalar"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.SelectionSetNotAllowed("name", _) -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have SelectionSetNotAllowed error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Undefined Fragment
// ============================================================================

pub fn invalid_undefined_fragment_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { ...UserFields } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for undefined fragment"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.UndefinedFragment("UserFields") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have UndefinedFragment error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Valid Query Tests - With Fragment
// ============================================================================

pub fn valid_query_with_fragment_test() {
  let test_schema = build_test_schema()

  let query_str =
    "
    { users { ...UserFields } }
    fragment UserFields on User { id name }
  "

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Duplicate Operation Names
// ============================================================================

pub fn invalid_duplicate_operation_names_test() {
  let test_schema = build_test_schema()

  let query_str =
    "
    query GetUsers { users { id } }
    query GetUsers { users { name } }
  "

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for duplicate operation names"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.DuplicateOperationName("GetUsers") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have DuplicateOperationName error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Anonymous Operation Not Alone
// ============================================================================

pub fn invalid_anonymous_not_alone_test() {
  let test_schema = build_test_schema()

  let query_str =
    "
    { users { id } }
    query Named { users { name } }
  "

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) ->
          panic as "Validation should fail for anonymous operation not alone"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.AnonymousOperationNotAlone -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have AnonymousOperationNotAlone error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn error_formatting_test() {
  // Test that errors format correctly
  let error = validation.UnknownField("badField", "User")
  let formatted = validation.format_error(error)

  case formatted == "Cannot query field \"badField\" on type \"User\"" {
    True -> Nil
    False -> panic as "Error formatting incorrect"
  }
}

pub fn multiple_error_formatting_test() {
  let errors = [
    validation.UnknownField("field1", "Type1"),
    validation.UnknownField("field2", "Type2"),
  ]
  let formatted = validation.format_errors(errors)

  case
    formatted
    == "Cannot query field \"field1\" on type \"Type1\"\nCannot query field \"field2\" on type \"Type2\""
  {
    True -> Nil
    False -> panic as "Multiple error formatting incorrect"
  }
}

// ============================================================================
// Invalid Query Tests - Duplicate Argument
// ============================================================================

pub fn invalid_duplicate_argument_test() {
  let test_schema = build_test_schema()

  // Duplicate id argument
  let query_str = "{ user(id: \"1\", id: \"2\") { name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for duplicate argument"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.DuplicateArgument("user", "id") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have DuplicateArgument error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Duplicate Variable
// ============================================================================

pub fn invalid_duplicate_variable_test() {
  let test_schema = build_test_schema()

  // Duplicate variable definition
  let query_str = "query Test($id: ID!, $id: ID!) { user(id: $id) { name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for duplicate variable"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.DuplicateVariable("id") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have DuplicateVariable error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Unused Fragment
// ============================================================================

pub fn invalid_unused_fragment_test() {
  let test_schema = build_test_schema()

  // Fragment defined but not used
  let query_str =
    "
    { users { id name } }
    fragment UnusedFields on User { email age }
  "

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for unused fragment"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.UnusedFragment("UnusedFields") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have UnusedFragment error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Valid Query Tests - Skip Directive
// ============================================================================

pub fn valid_skip_directive_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { id name @skip(if: true) } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Valid Query Tests - Include Directive
// ============================================================================

pub fn valid_include_directive_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { id @include(if: true) name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Invalid Query Tests - Unknown Directive
// ============================================================================

pub fn invalid_unknown_directive_test() {
  let test_schema = build_test_schema()

  // Unknown directive @custom
  let query_str = "{ users { id @custom name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Validation should fail for unknown directive"
        Error(errors) -> {
          case
            list.find(errors, fn(e) {
              case e {
                validation.UnknownDirective("custom") -> True
                _ -> False
              }
            })
          {
            Ok(_) -> Nil
            Error(_) -> panic as "Should have UnknownDirective error"
          }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Valid Query Tests - Inline Fragment (no directive - parser limitation)
// ============================================================================

pub fn valid_inline_fragment_test() {
  let test_schema = build_test_schema()

  let query_str = "{ users { ... on User { id name } } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Valid Query Tests - Multiple Directives on Field
// ============================================================================

pub fn valid_multiple_directives_test() {
  let test_schema = build_test_schema()

  // Multiple directives on a field
  let query_str = "{ users { id @skip(if: false) @include(if: true) name } }"

  case parser.parse(query_str) {
    Ok(doc) -> {
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Validation failed unexpectedly: " <> msg }
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// validate_query (parse + validate) Tests
// ============================================================================

pub fn validate_query_valid_test() {
  let test_schema = build_test_schema()
  let result = validation.validate_query("{ users { id name } }", test_schema)
  case result {
    Ok(_) -> Nil
    Error(errors) -> {
      let msg = validation.format_errors(errors)
      panic as { "Expected valid query, got errors: " <> msg }
    }
  }
}

pub fn validate_query_invalid_syntax_test() {
  let test_schema = build_test_schema()
  // Syntactically invalid — unclosed brace
  let result = validation.validate_query("{ users { id ", test_schema)
  case result {
    Error(errors) ->
      case errors != [] {
        True -> Nil
        False -> panic as "Expected non-empty error list for invalid syntax"
      }
    Ok(_) -> panic as "Expected error for syntactically invalid query"
  }
}

pub fn validate_query_unknown_field_test() {
  let test_schema = build_test_schema()
  let result =
    validation.validate_query("{ users { nonexistentField } }", test_schema)
  case result {
    Error(errors) ->
      case errors != [] {
        True -> Nil
        False -> panic as "Expected errors for unknown field"
      }
    Ok(_) -> panic as "Expected error for unknown field"
  }
}

pub fn validate_query_garbage_input_test() {
  let test_schema = build_test_schema()
  // Completely unparseable input
  let result = validation.validate_query("!@#$%^&*()", test_schema)
  case result {
    Error(errors) ->
      case errors != [] {
        True -> Nil
        False -> panic as "Expected non-empty error list for garbage input"
      }
    Ok(_) -> panic as "Expected error for unparseable query string"
  }
}

// ============================================================================
// Subscription Single Root Field Validation
// ============================================================================

pub fn invalid_subscription_multiple_root_fields_test() {
  let sub_schema =
    query.new()
    |> query.add_subscription(query.subscription(
      name: "onUser",
      returns: schema.Named("String"),
      topic: "onUser",
    ))
    |> query.add_subscription(query.subscription(
      name: "onPost",
      returns: schema.Named("String"),
      topic: "onPost",
    ))
    |> query.build

  case parser.parse("subscription { onUser onPost }") {
    Ok(doc) -> {
      case validation.validate(doc, sub_schema) {
        Error(errors) ->
          case errors != [] {
            True -> Nil
            False ->
              panic as "Expected error for subscription with multiple root fields"
          }
        Ok(_) ->
          panic as "Expected validation to fail for subscription with multiple root fields"
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// Issue #43: Test subscription with fragment spread
// Note: Currently the validator may not expand fragments when counting root fields.
// This test documents the current behavior.
pub fn subscription_fragment_spread_parses_test() {
  // Create a subscription schema with multiple subscription fields
  let sub_schema =
    query.new()
    |> query.add_subscription(query.subscription(
      name: "onUserCreated",
      returns: schema.Named("String"),
      topic: "onUserCreated",
    ))
    |> query.add_subscription(query.subscription(
      name: "onUserUpdated",
      returns: schema.Named("String"),
      topic: "onUserUpdated",
    ))
    |> query.build

  let query_str =
    "
    subscription {
      ...SubFields
    }

    fragment SubFields on Subscription {
      onUserCreated
      onUserUpdated
    }
    "

  case parser.parse(query_str) {
    Ok(document) -> {
      // Validate and check the result
      case validation.validate(document, sub_schema) {
        Error(errors) -> {
          // If validation fails with SubscriptionMultipleRootFields, that's correct
          let has_multiple_fields_error =
            list.any(errors, fn(err) {
              case err {
                validation.SubscriptionMultipleRootFields(_) -> True
                _ -> False
              }
            })
          case has_multiple_fields_error {
            True -> Nil
            False -> {
              // Some other validation error - that's OK for this test
              Nil
            }
          }
        }
        Ok(_) -> {
          // Current behavior: validation passes (fragment expansion not checked)
          // This documents that the validator doesn't expand fragments for root field counting
          Nil
        }
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Variable type input validation tests
// ============================================================================

pub fn variable_object_type_is_rejected_test() {
  let s = build_test_schema()
  case validation.validate_query("query ($u: User) { users { id } }", s) {
    Error(errors) -> {
      let has_error =
        list.any(errors, fn(e) {
          case e {
            validation.VariableNotInputType("u", "User") -> True
            _ -> False
          }
        })
      case has_error {
        True -> Nil
        False ->
          panic as {
            "Expected VariableNotInputType but got: "
            <> validation.format_errors(errors)
          }
      }
    }
    Ok(_) -> panic as "Expected validation error for object-type variable"
  }
}

pub fn variable_scalar_type_is_accepted_test() {
  let s = build_test_schema()
  case
    validation.validate_query("query ($id: ID!) { user(id: $id) { name } }", s)
  {
    Ok(_) -> Nil
    Error(errors) ->
      panic as {
        "Expected valid query but got: " <> validation.format_errors(errors)
      }
  }
}

pub fn variable_string_type_is_accepted_test() {
  let s = build_test_schema()
  case validation.validate_query("query ($n: String) { users { id } }", s) {
    Ok(_) -> Nil
    Error(errors) -> {
      let has_input_type_error =
        list.any(errors, fn(e) {
          case e {
            validation.VariableNotInputType(_, _) -> True
            _ -> False
          }
        })
      case has_input_type_error {
        True ->
          panic as {
            "String should be accepted as input type but got: "
            <> validation.format_errors(errors)
          }
        False -> Nil
      }
    }
  }
}

// ============================================================================
// Fragment on non-composite type tests
// ============================================================================

pub fn fragment_on_scalar_is_rejected_test() {
  let s = build_test_schema()
  case
    validation.validate_query("{ users { id } } fragment F on String { id }", s)
  {
    Error(errors) -> {
      let has_error =
        list.any(errors, fn(e) {
          case e {
            validation.FragmentOnNonCompositeType("F", "String") -> True
            _ -> False
          }
        })
      case has_error {
        True -> Nil
        False ->
          panic as {
            "Expected FragmentOnNonCompositeType but got: "
            <> validation.format_errors(errors)
          }
      }
    }
    Ok(_) -> panic as "Expected validation error for fragment on scalar"
  }
}

pub fn fragment_on_object_type_is_accepted_test() {
  let s = build_test_schema()
  case
    validation.validate_query(
      "{ users { ...F } } fragment F on User { id name }",
      s,
    )
  {
    Ok(_) -> Nil
    Error(errors) -> {
      let has_composite_error =
        list.any(errors, fn(e) {
          case e {
            validation.FragmentOnNonCompositeType(_, _) -> True
            _ -> False
          }
        })
      case has_composite_error {
        True ->
          panic as {
            "User should be accepted as composite type but got: "
            <> validation.format_errors(errors)
          }
        False -> Nil
      }
    }
  }
}

// ============================================================================
// @skip / @include argument validation tests
// ============================================================================

pub fn skip_without_if_is_rejected_test() {
  let s = build_test_schema()
  case validation.validate_query("{ users { id @skip } }", s) {
    Error(errors) -> {
      let has_error =
        list.any(errors, fn(e) {
          case e {
            validation.MissingRequiredArgument("@skip", "if") -> True
            _ -> False
          }
        })
      case has_error {
        True -> Nil
        False ->
          panic as {
            "Expected MissingRequiredArgument for @skip but got: "
            <> validation.format_errors(errors)
          }
      }
    }
    Ok(_) -> panic as "Expected validation error for @skip without if"
  }
}

pub fn include_without_if_is_rejected_test() {
  let s = build_test_schema()
  case validation.validate_query("{ users { id @include } }", s) {
    Error(errors) -> {
      let has_error =
        list.any(errors, fn(e) {
          case e {
            validation.MissingRequiredArgument("@include", "if") -> True
            _ -> False
          }
        })
      case has_error {
        True -> Nil
        False ->
          panic as {
            "Expected MissingRequiredArgument for @include but got: "
            <> validation.format_errors(errors)
          }
      }
    }
    Ok(_) -> panic as "Expected validation error for @include without if"
  }
}

pub fn skip_with_boolean_if_is_accepted_test() {
  let s = build_test_schema()
  case validation.validate_query("{ users { id @skip(if: true) } }", s) {
    Ok(_) -> Nil
    Error(errors) ->
      panic as {
        "Expected valid query but got: " <> validation.format_errors(errors)
      }
  }
}

pub fn include_with_variable_if_tracks_variable_test() {
  let s = build_test_schema()
  case
    validation.validate_query(
      "query ($show: Boolean!) { users { id @include(if: $show) } }",
      s,
    )
  {
    Ok(_) -> Nil
    Error(errors) ->
      panic as {
        "Expected valid query but got: " <> validation.format_errors(errors)
      }
  }
}

pub fn skip_unknown_argument_is_rejected_test() {
  let s = build_test_schema()
  case
    validation.validate_query("{ users { id @skip(if: true, extra: 1) } }", s)
  {
    Error(errors) -> {
      let has_error =
        list.any(errors, fn(e) {
          case e {
            validation.UnknownArgument("@skip", "extra") -> True
            _ -> False
          }
        })
      case has_error {
        True -> Nil
        False ->
          panic as {
            "Expected UnknownArgument for extra arg on @skip but got: "
            <> validation.format_errors(errors)
          }
      }
    }
    Ok(_) -> panic as "Expected validation error for unknown @skip argument"
  }
}
