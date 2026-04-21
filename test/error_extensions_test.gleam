import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import mochi/error
import mochi/executor
import mochi/query
import mochi/response
import mochi/schema
import mochi/types

pub fn extensions_serialized_in_json_test() {
  let err =
    error.error("Something failed")
    |> error.with_code("NOT_FOUND")
    |> error.with_extension("timestamp", types.to_dynamic(12_345))

  let json = response.to_json(response.failure([err]))

  case
    string.contains(json, "\"extensions\""),
    string.contains(json, "NOT_FOUND"),
    string.contains(json, "12345")
  {
    True, True, True -> Nil
    _, _, _ -> panic as "extensions not serialized correctly"
  }
}

pub fn no_extensions_omits_field_test() {
  let err = error.error("Simple error")
  let json = response.to_json(response.failure([err]))

  case string.contains(json, "\"extensions\"") {
    False -> Nil
    True -> panic as "extensions field should be omitted when None"
  }
}

pub fn with_code_helper_test() {
  let err =
    error.error("Not found")
    |> error.with_code("USER_NOT_FOUND")

  case err.extensions {
    Some(ext) ->
      case dict.get(ext, "code") {
        Ok(_) -> Nil
        Error(_) -> panic as "code key missing from extensions"
      }
    None -> panic as "extensions should not be None"
  }
}

pub fn error_code_type_test() {
  let err =
    error.error("Unauthorized")
    |> error.with_error_code(error.Unauthorized)

  case err.extensions {
    Some(ext) ->
      case dict.get(ext, "code") {
        Ok(v) ->
          case decode.run(v, decode.string) {
            Ok("UNAUTHORIZED") -> Nil
            Ok(other) -> panic as { "expected UNAUTHORIZED, got: " <> other }
            Error(_) -> panic as "code value not a string"
          }
        Error(_) -> panic as "code key missing"
      }
    None -> panic as "extensions should not be None"
  }
}

pub fn code_to_string_covers_all_codes_test() {
  let pairs = [
    #(error.NotFound, "NOT_FOUND"),
    #(error.Unauthorized, "UNAUTHORIZED"),
    #(error.Forbidden, "FORBIDDEN"),
    #(error.BadRequest, "BAD_REQUEST"),
    #(error.InternalError, "INTERNAL_SERVER_ERROR"),
  ]
  list.each(pairs, fn(pair) {
    let #(code, expected) = pair
    case error.code_to_string(code) == expected {
      True -> Nil
      False ->
        panic as {
          "code_to_string mismatch for "
          <> expected
          <> " got "
          <> error.code_to_string(code)
        }
    }
  })
}

pub fn to_payload_preserves_message_and_extensions_test() {
  let err =
    error.error("Test payload")
    |> error.with_code("TEST_CODE")

  let #(msg, extensions) = error.to_payload(err)

  case msg == "Test payload" {
    True -> Nil
    False -> panic as "message should be preserved in payload"
  }

  case extensions {
    Some(ext) ->
      case dict.has_key(ext, "code") {
        True -> Nil
        False -> panic as "extensions should have code key"
      }
    None -> panic as "extensions should not be None"
  }
}

pub fn rich_resolver_surfaces_extensions_in_response_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: #(String, String)) { u.0 })
    |> types.string("name", fn(u: #(String, String)) { u.1 })
    |> types.build(fn(_) { Ok(#("1", "Test")) })

  let user_schema =
    query.new()
    |> query.add_rich_query(
      "user",
      [],
      schema.named_type("User"),
      fn(_) { Ok(Nil) },
      fn(_, _) {
        Error(
          error.error("User not found")
          |> error.with_error_code(error.NotFound)
          |> error.to_payload,
        )
      },
      types.to_dynamic,
    )
    |> query.add_type(user_type)
    |> query.build

  let result = executor.execute_query(user_schema, "{ user { id name } }")

  case result.errors {
    [] -> panic as "expected a resolver error"
    [err, ..] -> {
      let gql_err = response.execution_error_to_graphql_error(err)
      case gql_err.message == "User not found" {
        True -> Nil
        False -> panic as { "unexpected message: " <> gql_err.message }
      }
      case gql_err.extensions {
        Some(ext) ->
          case dict.get(ext, "code") {
            Ok(v) ->
              case decode.run(v, decode.string) {
                Ok("NOT_FOUND") -> Nil
                Ok(other) ->
                  panic as {
                    "expected NOT_FOUND in extensions, got: " <> other
                  }
                Error(_) -> panic as "code value not a string"
              }
            Error(_) -> panic as "extensions should have code key"
          }
        None -> panic as "rich resolver error should carry extensions"
      }
    }
  }
}

pub fn rich_resolver_ok_path_works_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: #(String, String)) { u.0 })
    |> types.string("name", fn(u: #(String, String)) { u.1 })
    |> types.build(fn(_) { Ok(#("1", "Test")) })

  let user_schema =
    query.new()
    |> query.add_rich_query(
      "user",
      [],
      schema.named_type("User"),
      fn(_) { Ok(Nil) },
      fn(_, _) { Ok(#("42", "Alice")) },
      types.to_dynamic,
    )
    |> query.add_type(user_type)
    |> query.build

  let result = executor.execute_query(user_schema, "{ user { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "expected no errors"
  }

  case result.data {
    Some(_) -> Nil
    None -> panic as "expected data"
  }
}
