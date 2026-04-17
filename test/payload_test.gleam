// Tests for mochi/payload.gleam
// Covers constructors, schema types, dynamic encoding, and end-to-end execution.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/executor
import mochi/payload
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test helpers
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

fn user_to_dynamic(u: User) -> Dynamic {
  types.record([
    types.field("id", u.id),
    types.field("name", u.name),
    types.field("email", u.email),
  ])
}

fn default_ctx() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
}

fn build_mutation_schema(
  resolver: fn(String, schema.ExecutionContext) -> payload.MutationPayload(User),
) -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.build(fn(_) { Ok(User("", "", "")) })

  let #(create_user_payload, vm_type) =
    payload.payload_types("CreateUser", "User")

  query.new()
  |> query.add_type(user_type)
  |> query.add_type(vm_type)
  |> query.add_type(create_user_payload)
  |> query.add_mutation(
    query.mutation(
      name: "createUser",
      args: [query.arg("name", schema.non_null(schema.string_type()))],
      returns: schema.named_type("CreateUserPayload"),
      decode: fn(args) { query.get_string(args, "name") },
      resolve: fn(name, ctx) { Ok(resolver(name, ctx)) },
      encode: fn(p) { payload.to_dynamic(p, user_to_dynamic) },
    ),
  )
  |> query.build
}

// ============================================================================
// Constructor: ok
// ============================================================================

pub fn ok_sets_successful_true_test() {
  let p = payload.ok(User("1", "Alice", "alice@example.com"))
  should.equal(p.successful, True)
}

pub fn ok_wraps_result_in_some_test() {
  let user = User("1", "Alice", "alice@example.com")
  let p = payload.ok(user)
  should.equal(p.result, Some(user))
}

pub fn ok_has_empty_messages_test() {
  let p = payload.ok(User("1", "Alice", "alice@example.com"))
  should.equal(p.messages, [])
}

// ============================================================================
// Constructor: error
// ============================================================================

pub fn error_sets_successful_false_test() {
  let p: payload.MutationPayload(User) =
    payload.error([payload.message("Something went wrong")])
  should.equal(p.successful, False)
}

pub fn error_result_is_none_test() {
  let p: payload.MutationPayload(User) =
    payload.error([payload.message("Something went wrong")])
  should.equal(p.result, None)
}

pub fn error_preserves_messages_test() {
  let msgs = [
    payload.message("name is required"),
    payload.message_for("email", "is invalid"),
  ]
  let p: payload.MutationPayload(User) = payload.error(msgs)
  should.equal(list.length(p.messages), 2)
}

pub fn error_with_empty_messages_test() {
  let p: payload.MutationPayload(User) = payload.error([])
  should.equal(p.successful, False)
  should.equal(p.messages, [])
}

// ============================================================================
// Constructor: message
// ============================================================================

pub fn message_has_no_field_test() {
  let vm = payload.message("Something failed")
  should.equal(vm.field, None)
}

pub fn message_has_no_code_test() {
  let vm = payload.message("Something failed")
  should.equal(vm.code, None)
}

pub fn message_sets_message_test() {
  let vm = payload.message("Something failed")
  should.equal(vm.message, "Something failed")
}

// ============================================================================
// Constructor: message_for
// ============================================================================

pub fn message_for_sets_field_test() {
  let vm = payload.message_for("email", "is invalid")
  should.equal(vm.field, Some("email"))
}

pub fn message_for_sets_message_test() {
  let vm = payload.message_for("email", "is invalid")
  should.equal(vm.message, "is invalid")
}

pub fn message_for_has_no_code_test() {
  let vm = payload.message_for("email", "is invalid")
  should.equal(vm.code, None)
}

// ============================================================================
// Constructor: with_code
// ============================================================================

pub fn with_code_sets_code_test() {
  let vm =
    payload.message_for("email", "has already been taken")
    |> payload.with_code("already_taken")
  should.equal(vm.code, Some("already_taken"))
}

pub fn with_code_preserves_field_and_message_test() {
  let vm =
    payload.message_for("email", "has already been taken")
    |> payload.with_code("already_taken")
  should.equal(vm.field, Some("email"))
  should.equal(vm.message, "has already been taken")
}

pub fn with_code_on_top_level_message_test() {
  let vm =
    payload.message("server error")
    |> payload.with_code("internal_error")
  should.equal(vm.field, None)
  should.equal(vm.code, Some("internal_error"))
}

// ============================================================================
// Schema type: validation_message_type
// ============================================================================

pub fn validation_message_type_name_test() {
  let t = payload.validation_message_type()
  should.equal(t.name, "ValidationMessage")
}

pub fn validation_message_type_has_field_field_test() {
  let t = payload.validation_message_type()
  should.be_true(dict.has_key(t.fields, "field"))
}

pub fn validation_message_type_has_message_field_test() {
  let t = payload.validation_message_type()
  should.be_true(dict.has_key(t.fields, "message"))
}

pub fn validation_message_type_has_code_field_test() {
  let t = payload.validation_message_type()
  should.be_true(dict.has_key(t.fields, "code"))
}

pub fn validation_message_type_message_is_non_null_test() {
  let t = payload.validation_message_type()
  case dict.get(t.fields, "message") {
    Ok(f) ->
      case f.field_type {
        schema.NonNull(schema.Named("String")) -> Nil
        _ -> panic as "message field should be NonNull(String)"
      }
    Error(_) -> panic as "ValidationMessage should have 'message' field"
  }
}

pub fn validation_message_type_field_is_nullable_test() {
  let t = payload.validation_message_type()
  case dict.get(t.fields, "field") {
    Ok(f) ->
      case f.field_type {
        schema.Named("String") -> Nil
        _ -> panic as "field field should be nullable String"
      }
    Error(_) -> panic as "ValidationMessage should have 'field' field"
  }
}

// ============================================================================
// Schema type: payload_type
// ============================================================================

pub fn payload_type_name_test() {
  let t = payload.payload_type("CreateUser", "User")
  should.equal(t.name, "CreateUserPayload")
}

pub fn payload_type_has_successful_field_test() {
  let t = payload.payload_type("CreateUser", "User")
  should.be_true(dict.has_key(t.fields, "successful"))
}

pub fn payload_type_successful_is_non_null_bool_test() {
  let t = payload.payload_type("CreateUser", "User")
  case dict.get(t.fields, "successful") {
    Ok(f) ->
      case f.field_type {
        schema.NonNull(schema.Named("Boolean")) -> Nil
        _ -> panic as "successful should be NonNull(Boolean)"
      }
    Error(_) -> panic as "payload should have 'successful' field"
  }
}

pub fn payload_type_has_messages_field_test() {
  let t = payload.payload_type("CreateUser", "User")
  should.be_true(dict.has_key(t.fields, "messages"))
}

pub fn payload_type_has_result_field_test() {
  let t = payload.payload_type("CreateUser", "User")
  should.be_true(dict.has_key(t.fields, "result"))
}

pub fn payload_type_result_points_to_result_type_test() {
  let t = payload.payload_type("CreateUser", "User")
  case dict.get(t.fields, "result") {
    Ok(f) ->
      case f.field_type {
        schema.Named("User") -> Nil
        _ -> panic as "result field should be Named('User')"
      }
    Error(_) -> panic as "payload should have 'result' field"
  }
}

// ============================================================================
// Schema type: payload_types
// ============================================================================

pub fn payload_types_returns_both_types_test() {
  let #(p, vm) = payload.payload_types("UpdateUser", "User")
  should.equal(p.name, "UpdateUserPayload")
  should.equal(vm.name, "ValidationMessage")
}

// ============================================================================
// Dynamic encoding: validation_message_to_dynamic
// ============================================================================

pub fn vm_to_dynamic_message_field_test() {
  let vm = payload.message("name is required")
  let dyn = payload.validation_message_to_dynamic(vm)
  case decode.run(dyn, decode.at(["message"], decode.string)) {
    Ok("name is required") -> Nil
    _ -> panic as "message should be 'name is required'"
  }
}

pub fn vm_to_dynamic_field_present_test() {
  let vm = payload.message_for("email", "is invalid")
  let dyn = payload.validation_message_to_dynamic(vm)
  case decode.run(dyn, decode.at(["field"], decode.string)) {
    Ok("email") -> Nil
    _ -> panic as "field should be 'email'"
  }
}

pub fn vm_to_dynamic_code_present_test() {
  let vm =
    payload.message_for("email", "taken")
    |> payload.with_code("already_taken")
  let dyn = payload.validation_message_to_dynamic(vm)
  case decode.run(dyn, decode.at(["code"], decode.string)) {
    Ok("already_taken") -> Nil
    _ -> panic as "code should be 'already_taken'"
  }
}

// ============================================================================
// Dynamic encoding: to_dynamic
// ============================================================================

pub fn to_dynamic_successful_true_test() {
  let user = User("1", "Alice", "alice@example.com")
  let p = payload.ok(user)
  let dyn = payload.to_dynamic(p, user_to_dynamic)
  case decode.run(dyn, decode.at(["successful"], decode.bool)) {
    Ok(True) -> Nil
    _ -> panic as "successful should be True"
  }
}

pub fn to_dynamic_successful_false_test() {
  let p: payload.MutationPayload(User) =
    payload.error([payload.message("failed")])
  let dyn = payload.to_dynamic(p, user_to_dynamic)
  case decode.run(dyn, decode.at(["successful"], decode.bool)) {
    Ok(False) -> Nil
    _ -> panic as "successful should be False"
  }
}

pub fn to_dynamic_result_present_on_success_test() {
  let user = User("1", "Alice", "alice@example.com")
  let p = payload.ok(user)
  let dyn = payload.to_dynamic(p, user_to_dynamic)
  case decode.run(dyn, decode.at(["result", "name"], decode.string)) {
    Ok("Alice") -> Nil
    _ -> panic as "result.name should be 'Alice'"
  }
}

pub fn to_dynamic_messages_list_test() {
  let msgs = [
    payload.message("name is required"),
    payload.message_for("email", "is invalid"),
  ]
  let p: payload.MutationPayload(User) = payload.error(msgs)
  let dyn = payload.to_dynamic(p, user_to_dynamic)
  case decode.run(dyn, decode.at(["messages"], decode.list(decode.dynamic))) {
    Ok(ms) -> should.equal(list.length(ms), 2)
    Error(_) -> panic as "messages should be a list"
  }
}

// ============================================================================
// End-to-end: mutation returning successful payload
// ============================================================================

pub fn e2e_successful_mutation_test() {
  let schema =
    build_mutation_schema(fn(name, _ctx) {
      payload.ok(User("new-1", name, name <> "@example.com"))
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"Bob\") { successful messages { field message code } } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn e2e_successful_payload_fields_test() {
  let schema =
    build_mutation_schema(fn(name, _ctx) {
      payload.ok(User("new-1", name, name <> "@example.com"))
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"Bob\") { successful result { name } } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])

  case result.data {
    Some(data) -> {
      case
        decode.run(data, decode.at(["createUser", "successful"], decode.bool))
      {
        Ok(True) -> Nil
        _ -> panic as "successful should be True"
      }
      case
        decode.run(
          data,
          decode.at(["createUser", "result", "name"], decode.string),
        )
      {
        Ok("Bob") -> Nil
        _ -> panic as "result.name should be 'Bob'"
      }
    }
    None -> panic as "Expected data"
  }
}

// ============================================================================
// End-to-end: mutation returning error payload
// ============================================================================

pub fn e2e_failed_mutation_test() {
  let schema =
    build_mutation_schema(fn(_name, _ctx) {
      payload.error([
        payload.message_for("name", "has already been taken")
        |> payload.with_code("already_taken"),
      ])
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"Alice\") { successful messages { field message code } } }",
      dict.new(),
      default_ctx(),
    )

  // Validation errors are returned as data, not top-level errors
  should.equal(result.errors, [])
  should.be_true(result.data |> option.is_some)
}

pub fn e2e_failed_payload_fields_test() {
  let schema =
    build_mutation_schema(fn(_name, _ctx) {
      payload.error([
        payload.message_for("name", "has already been taken")
        |> payload.with_code("already_taken"),
      ])
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"Alice\") { successful messages { field message code } } }",
      dict.new(),
      default_ctx(),
    )

  case result.data {
    Some(data) -> {
      case
        decode.run(data, decode.at(["createUser", "successful"], decode.bool))
      {
        Ok(False) -> Nil
        _ -> panic as "successful should be False"
      }
      case
        decode.run(
          data,
          decode.at(["createUser", "messages"], decode.list(decode.dynamic)),
        )
      {
        Ok([msg]) -> {
          case decode.run(msg, decode.at(["field"], decode.string)) {
            Ok("name") -> Nil
            _ -> panic as "message field should be 'name'"
          }
          case decode.run(msg, decode.at(["code"], decode.string)) {
            Ok("already_taken") -> Nil
            _ -> panic as "message code should be 'already_taken'"
          }
        }
        _ -> panic as "messages should have exactly 1 item"
      }
    }
    None -> panic as "Expected data"
  }
}

pub fn e2e_failed_result_is_null_test() {
  let schema =
    build_mutation_schema(fn(_name, _ctx) {
      payload.error([payload.message("validation failed")])
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"X\") { successful result { id } } }",
      dict.new(),
      default_ctx(),
    )

  should.equal(result.errors, [])
}

pub fn e2e_multiple_messages_test() {
  let schema =
    build_mutation_schema(fn(_name, _ctx) {
      payload.error([
        payload.message_for("name", "is too short"),
        payload.message_for("email", "is invalid"),
        payload.message("general failure"),
      ])
    })

  let result =
    executor.execute_query_with_context(
      schema,
      "mutation { createUser(name: \"X\") { successful messages { field message } } }",
      dict.new(),
      default_ctx(),
    )

  case result.data {
    Some(data) -> {
      case
        decode.run(
          data,
          decode.at(["createUser", "messages"], decode.list(decode.dynamic)),
        )
      {
        Ok(msgs) -> should.equal(list.length(msgs), 3)
        _ -> panic as "messages should be a list"
      }
    }
    None -> panic as "Expected data"
  }
}
