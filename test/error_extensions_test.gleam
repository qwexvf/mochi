import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
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

  should.be_true(string.contains(json, "\"extensions\""))
  should.be_true(string.contains(json, "NOT_FOUND"))
  should.be_true(string.contains(json, "12345"))
}

pub fn no_extensions_omits_field_test() {
  let err = error.error("Simple error")
  let json = response.to_json(response.failure([err]))

  should.be_false(string.contains(json, "\"extensions\""))
}

pub fn with_code_helper_test() {
  let err =
    error.error("Not found")
    |> error.with_code("USER_NOT_FOUND")

  let assert Some(ext) = err.extensions
  should.be_ok(dict.get(ext, "code"))
}

pub fn error_code_type_test() {
  let err =
    error.error("Unauthorized")
    |> error.with_error_code(error.Unauthorized)

  let assert Some(ext) = err.extensions
  let assert Ok(v) = dict.get(ext, "code")
  decode.run(v, decode.string)
  |> should.equal(Ok("UNAUTHORIZED"))
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
    should.equal(error.code_to_string(code), expected)
  })
}

pub fn to_payload_preserves_message_and_extensions_test() {
  let err =
    error.error("Test payload")
    |> error.with_code("TEST_CODE")

  let #(msg, extensions) = error.to_payload(err)

  should.equal(msg, "Test payload")
  let assert Some(ext) = extensions
  should.be_true(dict.has_key(ext, "code"))
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

  let assert [err, ..] = result.errors
  let gql_err = response.execution_error_to_graphql_error(err)
  should.equal(gql_err.message, "User not found")

  let assert Some(ext) = gql_err.extensions
  let assert Ok(v) = dict.get(ext, "code")
  decode.run(v, decode.string)
  |> should.equal(Ok("NOT_FOUND"))
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

  should.equal(result.errors, [])
  should.be_true(result.data != None)
}

pub fn rich_resolver_error_path_uses_executor_path_test() {
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
          error.error("Fail")
          |> error.with_error_code(error.NotFound)
          |> error.to_payload,
        )
      },
      types.to_dynamic,
    )
    |> query.add_type(user_type)
    |> query.build

  let result = executor.execute_query(user_schema, "{ user { id } }")

  let assert [err] = result.errors
  let gql = response.execution_error_to_graphql_error(err)
  should.equal(gql.path, Some([error.FieldSegment("user")]))
}

pub fn with_extension_merges_with_existing_extensions_test() {
  let err =
    error.error("Err")
    |> error.with_code("CODE_A")
    |> error.with_extension("extra", types.to_dynamic("val"))

  let assert Some(ext) = err.extensions
  should.be_true(dict.has_key(ext, "code"))
  should.be_true(dict.has_key(ext, "extra"))
}

pub fn with_extensions_replaces_all_extensions_test() {
  let err =
    error.error("Err")
    |> error.with_code("OLD")
    |> error.with_extensions(
      dict.from_list([#("code", types.to_dynamic("NEW"))]),
    )

  let assert Some(ext) = err.extensions
  let assert Ok(v) = dict.get(ext, "code")
  decode.run(v, decode.string)
  |> should.equal(Ok("NEW"))
}
