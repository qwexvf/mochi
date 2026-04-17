//// A reference implementation of the structured mutation payload pattern.
////
//// Inspired by [absinthe_error_payload](https://github.com/mirego/absinthe_error_payload)
//// by Mirego.
////
//// GraphQL has two ways to signal failure from a mutation:
////
//// 1. **Top-level errors** — raised as `Error(msg)` from a resolver. These are
////    for unexpected failures (bugs, auth errors, infrastructure problems).
////
//// 2. **Payload errors** — returned as DATA inside the response. These are for
////    predictable validation failures the client should handle (e.g. "email
////    already taken"). The client can pattern-match on `successful` and inspect
////    `messages` without treating them as exceptions.
////
//// This module ships a ready-to-use `MutationPayload(result)` type. It is a
//// **reference implementation** — copy or adapt it freely. Your own payload
//// type can have a completely different shape (e.g. a union, an `errors`-only
//// field, or domain-specific codes). The only requirement mochi has is that
//// your resolver returns something your encoder can turn into a `Dynamic`.
////
//// ## Quick start
////
//// ```gleam
//// import mochi/payload
//// import mochi/query
//// import mochi/schema
////
//// fn create_user_resolver(name: String, _ctx) {
////   case validate_name(name) {
////     Ok(user) -> payload.ok(user)
////     Error(_) ->
////       payload.error([
////         payload.message_for("name", "has already been taken")
////         |> payload.with_code("already_taken"),
////       ])
////   }
//// }
////
//// let #(create_user_payload, vm_type) = payload.payload_types("CreateUser", "User")
////
//// query.new()
////   |> query.add_type(vm_type)              // shared — register once per schema
////   |> query.add_type(create_user_payload)  // one per mutation
////   |> query.add_mutation(
////     query.mutation(
////       name: "createUser",
////       args: [query.arg("name", schema.non_null(schema.string_type()))],
////       returns: schema.named_type("CreateUserPayload"),
////       decode: fn(args) { query.get_string(args, "name") },
////       resolve: fn(name, ctx) { Ok(create_user_resolver(name, ctx)) },
////       encode: fn(p) { payload.to_dynamic(p, user_to_dynamic) },
////     )
////   )
//// ```
////
//// ## Building a custom payload
////
//// You are not required to use this module. Any type that can be encoded to
//// `Dynamic` works as a mutation result. A minimal custom example:
////
//// ```gleam
//// pub type MyPayload(a) {
////   Success(data: a)
////   Failure(errors: List(String))
//// }
////
//// fn my_payload_to_dynamic(p: MyPayload(a), encode: fn(a) -> Dynamic) -> Dynamic {
////   case p {
////     Success(data) ->
////       types.record([
////         types.field("ok", True),
////         types.field("data", encode(data)),
////       ])
////     Failure(errors) ->
////       types.record([
////         types.field("ok", False),
////         types.field("errors", types.to_dynamic(errors)),
////       ])
////   }
//// }
//// ```
////
//// Register the matching GraphQL type with `schema.object(...)` and
//// `query.add_type(...)` the same way you would any other type.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema.{type ObjectType, Named, NonNull}

// ============================================================================
// Core Types
// ============================================================================

/// A validation error message associated with a mutation field.
pub type ValidationMessage {
  ValidationMessage(
    field: Option(String),
    message: String,
    code: Option(String),
  )
}

/// Standard mutation response envelope.
pub type MutationPayload(result) {
  MutationPayload(
    successful: Bool,
    result: Option(result),
    messages: List(ValidationMessage),
  )
}

// ============================================================================
// Constructors
// ============================================================================

/// Build a successful payload wrapping the given result.
pub fn ok(result: a) -> MutationPayload(a) {
  MutationPayload(successful: True, result: Some(result), messages: [])
}

/// Build a failed payload with validation messages and no result.
pub fn error(messages: List(ValidationMessage)) -> MutationPayload(a) {
  MutationPayload(successful: False, result: None, messages: messages)
}

/// Create a top-level (non-field-specific) validation message.
pub fn message(msg: String) -> ValidationMessage {
  ValidationMessage(field: None, message: msg, code: None)
}

/// Create a field-specific validation message.
pub fn message_for(field: String, msg: String) -> ValidationMessage {
  ValidationMessage(field: Some(field), message: msg, code: None)
}

/// Attach a machine-readable error code to a validation message.
pub fn with_code(vm: ValidationMessage, code: String) -> ValidationMessage {
  ValidationMessage(..vm, code: Some(code))
}

// ============================================================================
// Schema Types
// ============================================================================

@external(erlang, "gleam_stdlib", "identity")
fn identity(value: a) -> Dynamic

/// The shared `ValidationMessage` GraphQL object type.
///
/// Register this once per schema:
/// ```gleam
/// query.new()
///   |> query.add_type(payload.validation_message_type())
/// ```
pub fn validation_message_type() -> ObjectType {
  schema.object("ValidationMessage")
  |> schema.description("A validation error associated with a mutation field")
  |> schema.field(
    schema.field_def("field", Named("String"))
    |> schema.field_description("The input field that triggered the error"),
  )
  |> schema.field(
    schema.field_def("message", NonNull(Named("String")))
    |> schema.field_description("A human-readable description of the error"),
  )
  |> schema.field(
    schema.field_def("code", Named("String"))
    |> schema.field_description("A machine-readable error code"),
  )
}

/// Create a payload type for a specific mutation result type.
///
/// `name` is used to build `<name>Payload` (e.g. `"CreateUser"` → `"CreateUserPayload"`).
/// `result_type_name` is the GraphQL type name of the success result.
///
/// ```gleam
/// let user_payload = payload.payload_type("CreateUser", "User")
/// // Produces CreateUserPayload { successful, messages, result }
/// ```
pub fn payload_type(name: String, result_type_name: String) -> ObjectType {
  schema.object(name <> "Payload")
  |> schema.description("The result of the " <> name <> " mutation")
  |> schema.field(
    schema.field_def("successful", NonNull(Named("Boolean")))
    |> schema.field_description("Whether the mutation succeeded"),
  )
  |> schema.field(
    schema.field_def(
      "messages",
      NonNull(schema.List(Named("ValidationMessage"))),
    )
    |> schema.field_description("Validation errors, empty on success"),
  )
  |> schema.field(
    schema.field_def("result", Named(result_type_name))
    |> schema.field_description("The mutation result, present only on success"),
  )
}

/// Create both the payload type and the shared ValidationMessage type at once.
///
/// Returns `#(payload_type, validation_message_type)`.
/// Register `validation_message_type` once per schema even if you have
/// multiple mutations.
///
/// ```gleam
/// let #(create_user_payload, vm_type) = payload.payload_types("CreateUser", "User")
///
/// query.new()
///   |> query.add_type(vm_type)
///   |> query.add_type(create_user_payload)
/// ```
pub fn payload_types(
  name: String,
  result_type_name: String,
) -> #(ObjectType, ObjectType) {
  #(payload_type(name, result_type_name), validation_message_type())
}

// ============================================================================
// Dynamic Encoding
// ============================================================================

/// Encode a `ValidationMessage` to Dynamic for the GraphQL response.
pub fn validation_message_to_dynamic(vm: ValidationMessage) -> Dynamic {
  identity(
    dict.from_list([
      #("field", case vm.field {
        Some(f) -> identity(f)
        None -> identity(Nil)
      }),
      #("message", identity(vm.message)),
      #("code", case vm.code {
        Some(c) -> identity(c)
        None -> identity(Nil)
      }),
    ]),
  )
}

/// Encode a `MutationPayload` to Dynamic for the GraphQL response.
///
/// Pass the encoder for the inner result type:
/// ```gleam
/// payload.to_dynamic(p, user_to_dynamic)
/// ```
pub fn to_dynamic(
  payload: MutationPayload(a),
  encode_result: fn(a) -> Dynamic,
) -> Dynamic {
  identity(
    dict.from_list([
      #("successful", identity(payload.successful)),
      #(
        "messages",
        identity(list.map(payload.messages, validation_message_to_dynamic)),
      ),
      #("result", case payload.result {
        Some(r) -> encode_result(r)
        None -> identity(Nil)
      }),
    ]),
  )
}
