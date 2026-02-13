// mochi/transport/websocket.gleam
// WebSocket transport for GraphQL subscriptions using graphql-ws protocol
//
// This module implements the graphql-ws protocol (https://github.com/enisdenjo/graphql-ws)
// for real-time GraphQL subscriptions over WebSocket connections.
//
// Protocol messages:
// - Client -> Server: ConnectionInit, Subscribe, Complete, Ping, Pong
// - Server -> Client: ConnectionAck, Next, Error, Complete, Ping, Pong
//
// Usage:
//   let state = websocket.new_connection(schema, pubsub)
//   let #(new_state, response) = websocket.handle_message(state, message)

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json as gj
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/error.{type GraphQLError}
import mochi/executor.{type ExecutionResult}
import mochi/json
import mochi/schema.{type Schema}
import mochi/subscription.{type PubSub, type SubscriptionId}
import mochi/types

// ============================================================================
// Protocol Types - Client Messages
// ============================================================================

/// Messages sent from client to server
pub type ClientMessage {
  /// First message from client to initialize connection
  /// Payload may contain connection parameters (auth tokens, etc.)
  ConnectionInit(payload: Option(Dict(String, Dynamic)))

  /// Subscribe to a GraphQL operation
  Subscribe(id: String, payload: SubscriptionPayload)

  /// Client unsubscribing from an operation
  Complete(id: String)

  /// Ping message for keep-alive
  Ping(payload: Option(Dict(String, Dynamic)))

  /// Pong response to server ping
  Pong(payload: Option(Dict(String, Dynamic)))
}

/// Payload for Subscribe message
pub type SubscriptionPayload {
  SubscriptionPayload(
    query: String,
    variables: Option(Dict(String, Dynamic)),
    operation_name: Option(String),
    extensions: Option(Dict(String, Dynamic)),
  )
}

// ============================================================================
// Protocol Types - Server Messages
// ============================================================================

/// Messages sent from server to client
pub type ServerMessage {
  /// Acknowledge successful connection initialization
  ConnectionAck(payload: Option(Dict(String, Dynamic)))

  /// Send subscription data to client
  Next(id: String, payload: ExecutionResult)

  /// Send subscription errors to client
  SubscriptionError(id: String, payload: List(GraphQLError))

  /// Server completing a subscription (or acknowledging client complete)
  ServerComplete(id: String)

  /// Ping message for keep-alive
  ServerPing(payload: Option(Dict(String, Dynamic)))

  /// Pong response to client ping
  ServerPong(payload: Option(Dict(String, Dynamic)))
}

// ============================================================================
// Connection State
// ============================================================================

/// State of a WebSocket connection
pub type ConnectionState {
  ConnectionState(
    /// The GraphQL schema
    schema: Schema,
    /// The PubSub instance for subscription management
    pubsub: PubSub,
    /// Execution context for resolvers
    execution_context: schema.ExecutionContext,
    /// Map of subscription IDs to active subscriptions
    active_subscriptions: Dict(String, SubscriptionId),
    /// Whether connection has been acknowledged
    acknowledged: Bool,
    /// Connection parameters from ConnectionInit
    connection_params: Option(Dict(String, Dynamic)),
  )
}

/// Result of handling a message
pub type HandleResult {
  /// Successfully handled, returns updated state and optional response
  HandleOk(state: ConnectionState, response: Option(ServerMessage))

  /// Multiple responses needed (e.g., acknowledging then sending data)
  HandleMultiple(state: ConnectionState, responses: List(ServerMessage))

  /// Connection should be closed with error
  HandleClose(reason: String)
}

// ============================================================================
// Connection Management
// ============================================================================

/// Create a new connection state
pub fn new_connection(
  schema: Schema,
  pubsub: PubSub,
  execution_context: schema.ExecutionContext,
) -> ConnectionState {
  ConnectionState(
    schema: schema,
    pubsub: pubsub,
    execution_context: execution_context,
    active_subscriptions: dict.new(),
    acknowledged: False,
    connection_params: None,
  )
}

/// Create connection state with custom parameters
pub fn new_connection_with_params(
  schema: Schema,
  pubsub: PubSub,
  execution_context: schema.ExecutionContext,
  params: Dict(String, Dynamic),
) -> ConnectionState {
  ConnectionState(
    schema: schema,
    pubsub: pubsub,
    execution_context: execution_context,
    active_subscriptions: dict.new(),
    acknowledged: False,
    connection_params: Some(params),
  )
}

// ============================================================================
// Message Handling
// ============================================================================

/// Handle an incoming client message
pub fn handle_message(
  state: ConnectionState,
  message: ClientMessage,
) -> HandleResult {
  case message {
    ConnectionInit(payload) -> handle_connection_init(state, payload)
    Subscribe(id, payload) -> handle_subscribe(state, id, payload)
    Complete(id) -> handle_complete(state, id)
    Ping(payload) -> handle_ping(state, payload)
    Pong(_payload) -> HandleOk(state: state, response: None)
  }
}

/// Handle ConnectionInit message
fn handle_connection_init(
  state: ConnectionState,
  payload: Option(Dict(String, Dynamic)),
) -> HandleResult {
  case state.acknowledged {
    True ->
      // Already acknowledged - protocol violation
      HandleClose("Too many initialization requests")
    False -> {
      let new_state =
        ConnectionState(..state, acknowledged: True, connection_params: payload)
      HandleOk(state: new_state, response: Some(ConnectionAck(None)))
    }
  }
}

/// Handle Subscribe message
fn handle_subscribe(
  state: ConnectionState,
  id: String,
  _payload: SubscriptionPayload,
) -> HandleResult {
  use <- require_acknowledged(state)
  use <- check_duplicate_subscription(state, id)

  // For now, we just acknowledge the subscription was received
  // The actual subscription setup would integrate with subscription_executor
  // This is a placeholder that shows the structure
  let new_state =
    ConnectionState(
      ..state,
      active_subscriptions: dict.insert(
        state.active_subscriptions,
        id,
        "pending_" <> id,
      ),
    )

  HandleOk(state: new_state, response: None)
}

/// Handle Complete message (client unsubscribing)
fn handle_complete(state: ConnectionState, id: String) -> HandleResult {
  case dict.get(state.active_subscriptions, id) {
    Ok(subscription_id) -> {
      // Unsubscribe from PubSub
      let new_pubsub = subscription.unsubscribe(state.pubsub, subscription_id)
      let new_state =
        ConnectionState(
          ..state,
          pubsub: new_pubsub,
          active_subscriptions: dict.delete(state.active_subscriptions, id),
        )
      HandleOk(state: new_state, response: Some(ServerComplete(id)))
    }
    Error(_) ->
      // Subscription not found - just acknowledge
      HandleOk(state: state, response: Some(ServerComplete(id)))
  }
}

/// Handle Ping message
fn handle_ping(
  state: ConnectionState,
  payload: Option(Dict(String, Dynamic)),
) -> HandleResult {
  HandleOk(state: state, response: Some(ServerPong(payload)))
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Require connection to be acknowledged before processing
fn require_acknowledged(
  state: ConnectionState,
  next: fn() -> HandleResult,
) -> HandleResult {
  case state.acknowledged {
    True -> next()
    False -> HandleClose("Unauthorized")
  }
}

/// Check for duplicate subscription ID
fn check_duplicate_subscription(
  state: ConnectionState,
  id: String,
  next: fn() -> HandleResult,
) -> HandleResult {
  case dict.has_key(state.active_subscriptions, id) {
    True -> HandleClose("Subscriber for " <> id <> " already exists")
    False -> next()
  }
}

/// Send a Next message to the client
pub fn send_next(id: String, result: ExecutionResult) -> ServerMessage {
  Next(id: id, payload: result)
}

/// Send an Error message to the client
pub fn send_error(id: String, errors: List(GraphQLError)) -> ServerMessage {
  SubscriptionError(id: id, payload: errors)
}

/// Send a Complete message to the client
pub fn send_complete(id: String) -> ServerMessage {
  ServerComplete(id)
}

/// Get all active subscription IDs
pub fn get_active_subscriptions(state: ConnectionState) -> List(String) {
  dict.keys(state.active_subscriptions)
}

/// Check if a subscription is active
pub fn has_subscription(state: ConnectionState, id: String) -> Bool {
  dict.has_key(state.active_subscriptions, id)
}

/// Clean up all subscriptions (e.g., on disconnect)
pub fn cleanup(state: ConnectionState) -> ConnectionState {
  let new_pubsub =
    dict.fold(state.active_subscriptions, state.pubsub, fn(pubsub, _id, sub_id) {
      subscription.unsubscribe(pubsub, sub_id)
    })

  ConnectionState(..state, pubsub: new_pubsub, active_subscriptions: dict.new())
}

// ============================================================================
// Message Serialization Helpers
// ============================================================================

/// Message type constants for serialization
pub const msg_type_connection_init = "connection_init"

pub const msg_type_connection_ack = "connection_ack"

pub const msg_type_ping = "ping"

pub const msg_type_pong = "pong"

pub const msg_type_subscribe = "subscribe"

pub const msg_type_next = "next"

pub const msg_type_error = "error"

pub const msg_type_complete = "complete"

/// Get the message type string for a server message
pub fn server_message_type(message: ServerMessage) -> String {
  case message {
    ConnectionAck(_) -> msg_type_connection_ack
    Next(_, _) -> msg_type_next
    SubscriptionError(_, _) -> msg_type_error
    ServerComplete(_) -> msg_type_complete
    ServerPing(_) -> msg_type_ping
    ServerPong(_) -> msg_type_pong
  }
}

/// Get the message type string for a client message
pub fn client_message_type(message: ClientMessage) -> String {
  case message {
    ConnectionInit(_) -> msg_type_connection_init
    Subscribe(_, _) -> msg_type_subscribe
    Complete(_) -> msg_type_complete
    Ping(_) -> msg_type_ping
    Pong(_) -> msg_type_pong
  }
}

// ============================================================================
// JSON Encoding - Server Messages
// ============================================================================

/// Encode a server message to JSON string
pub fn encode_server_message(message: ServerMessage) -> String {
  message
  |> server_message_to_dynamic
  |> json.encode
}

/// Convert a server message to Dynamic for JSON encoding
pub fn server_message_to_dynamic(message: ServerMessage) -> Dynamic {
  case message {
    ConnectionAck(payload) -> encode_connection_ack(payload)
    Next(id, result) -> encode_next(id, result)
    SubscriptionError(id, errors) -> encode_error(id, errors)
    ServerComplete(id) -> encode_complete(id)
    ServerPing(payload) -> encode_ping_pong(msg_type_ping, payload)
    ServerPong(payload) -> encode_ping_pong(msg_type_pong, payload)
  }
}

fn encode_connection_ack(payload: Option(Dict(String, Dynamic))) -> Dynamic {
  case payload {
    Some(p) ->
      types.to_dynamic(
        dict.from_list([
          #("type", types.to_dynamic(msg_type_connection_ack)),
          #("payload", types.to_dynamic(p)),
        ]),
      )
    None ->
      types.to_dynamic(
        dict.from_list([#("type", types.to_dynamic(msg_type_connection_ack))]),
      )
  }
}

fn encode_next(id: String, result: ExecutionResult) -> Dynamic {
  let payload_dict = case result.data, result.errors {
    Some(data), [] -> dict.from_list([#("data", data)])
    Some(data), errors ->
      dict.from_list([
        #("data", data),
        #("errors", encode_execution_errors(errors)),
      ])
    None, errors ->
      dict.from_list([
        #("data", types.to_dynamic(Nil)),
        #("errors", encode_execution_errors(errors)),
      ])
  }

  types.to_dynamic(
    dict.from_list([
      #("type", types.to_dynamic(msg_type_next)),
      #("id", types.to_dynamic(id)),
      #("payload", types.to_dynamic(payload_dict)),
    ]),
  )
}

fn encode_execution_errors(errors: List(executor.ExecutionError)) -> Dynamic {
  types.to_dynamic(
    list.map(errors, fn(err) {
      let #(msg, path) = case err {
        executor.ValidationError(m, p) -> #(m, p)
        executor.ResolverError(m, p) -> #(m, p)
        executor.TypeError(m, p) -> #(m, p)
        executor.NullValueError(m, p) -> #(m, p)
      }
      types.to_dynamic(
        dict.from_list([
          #("message", types.to_dynamic(msg)),
          #("path", types.to_dynamic(path)),
        ]),
      )
    }),
  )
}

fn encode_error(id: String, errors: List(GraphQLError)) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("type", types.to_dynamic(msg_type_error)),
      #("id", types.to_dynamic(id)),
      #("payload", error.errors_to_dynamic(errors)),
    ]),
  )
}

fn encode_complete(id: String) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("type", types.to_dynamic(msg_type_complete)),
      #("id", types.to_dynamic(id)),
    ]),
  )
}

fn encode_ping_pong(
  msg_type: String,
  payload: Option(Dict(String, Dynamic)),
) -> Dynamic {
  case payload {
    Some(p) ->
      types.to_dynamic(
        dict.from_list([
          #("type", types.to_dynamic(msg_type)),
          #("payload", types.to_dynamic(p)),
        ]),
      )
    None ->
      types.to_dynamic(dict.from_list([#("type", types.to_dynamic(msg_type))]))
  }
}

// ============================================================================
// JSON Decoding - Client Messages
// ============================================================================

/// Error type for message decoding
pub type DecodeError {
  InvalidJson(String)
  MissingField(String)
  InvalidMessageType(String)
  InvalidPayload(String)
}

/// Decode a JSON string to a client message
pub fn decode_client_message(
  json_str: String,
) -> Result(ClientMessage, DecodeError) {
  case parse_json(json_str) {
    Ok(dyn) -> decode_client_message_dynamic(dyn)
    Error(_) -> Error(InvalidJson("Failed to parse JSON"))
  }
}

/// Decode a Dynamic value to a client message
pub fn decode_client_message_dynamic(
  dyn: Dynamic,
) -> Result(ClientMessage, DecodeError) {
  use msg_type <- result.try(get_string_field(dyn, "type"))

  case msg_type {
    "connection_init" -> decode_connection_init(dyn)
    "subscribe" -> decode_subscribe(dyn)
    "complete" -> decode_complete_msg(dyn)
    "ping" -> decode_ping(dyn)
    "pong" -> decode_pong(dyn)
    other -> Error(InvalidMessageType(other))
  }
}

fn decode_connection_init(dyn: Dynamic) -> Result(ClientMessage, DecodeError) {
  let payload = get_optional_dict_field(dyn, "payload")
  Ok(ConnectionInit(payload))
}

fn decode_subscribe(dyn: Dynamic) -> Result(ClientMessage, DecodeError) {
  use id <- result.try(get_string_field(dyn, "id"))
  use payload_dyn <- result.try(get_field(dyn, "payload"))
  use query <- result.try(get_string_field(payload_dyn, "query"))

  let variables = get_optional_dict_field(payload_dyn, "variables")
  let operation_name = get_optional_string_field(payload_dyn, "operationName")
  let extensions = get_optional_dict_field(payload_dyn, "extensions")

  Ok(Subscribe(
    id,
    SubscriptionPayload(
      query: query,
      variables: variables,
      operation_name: operation_name,
      extensions: extensions,
    ),
  ))
}

fn decode_complete_msg(dyn: Dynamic) -> Result(ClientMessage, DecodeError) {
  use id <- result.try(get_string_field(dyn, "id"))
  Ok(Complete(id))
}

fn decode_ping(dyn: Dynamic) -> Result(ClientMessage, DecodeError) {
  let payload = get_optional_dict_field(dyn, "payload")
  Ok(Ping(payload))
}

fn decode_pong(dyn: Dynamic) -> Result(ClientMessage, DecodeError) {
  let payload = get_optional_dict_field(dyn, "payload")
  Ok(Pong(payload))
}

// ============================================================================
// JSON Parsing Helpers
// ============================================================================

fn parse_json(json_str: String) -> Result(Dynamic, String) {
  gj.parse(json_str, decode.dynamic)
  |> result.map_error(fn(_) { "Invalid JSON" })
}

fn get_field(dyn: Dynamic, field: String) -> Result(Dynamic, DecodeError) {
  decode.run(dyn, decode.at([field], decode.dynamic))
  |> result.map_error(fn(_) { MissingField(field) })
}

fn get_string_field(dyn: Dynamic, field: String) -> Result(String, DecodeError) {
  // First check if field exists
  case decode.run(dyn, decode.at([field], decode.dynamic)) {
    Error(_) -> Error(MissingField(field))
    Ok(field_dyn) ->
      // Then check if it's a string
      case decode.run(field_dyn, decode.string) {
        Ok(s) -> Ok(s)
        Error(_) -> Error(InvalidPayload("Expected string for " <> field))
      }
  }
}

fn get_dict_raw(dyn: Dynamic) -> Result(Dict(String, Dynamic), Nil) {
  decode.run(dyn, decode.dict(decode.string, decode.dynamic))
  |> result.map_error(fn(_) { Nil })
}

fn get_field_raw(dyn: Dynamic, field: String) -> Result(Dynamic, Nil) {
  decode.run(dyn, decode.at([field], decode.dynamic))
  |> result.map_error(fn(_) { Nil })
}

fn get_string_raw(dyn: Dynamic) -> Result(String, Nil) {
  decode.run(dyn, decode.string)
  |> result.map_error(fn(_) { Nil })
}

fn get_optional_string_field(dyn: Dynamic, field: String) -> Option(String) {
  case get_field_raw(dyn, field) {
    Ok(field_dyn) ->
      case get_string_raw(field_dyn) {
        Ok(s) -> Some(s)
        Error(_) -> None
      }
    Error(_) -> None
  }
}

fn get_optional_dict_field(
  dyn: Dynamic,
  field: String,
) -> Option(Dict(String, Dynamic)) {
  case get_field_raw(dyn, field) {
    Ok(field_dyn) ->
      case get_dict_raw(field_dyn) {
        Ok(d) -> Some(d)
        Error(_) -> None
      }
    Error(_) -> None
  }
}

/// Format a decode error as a string
pub fn format_decode_error(err: DecodeError) -> String {
  case err {
    InvalidJson(msg) -> "Invalid JSON: " <> msg
    MissingField(field) -> "Missing required field: " <> field
    InvalidMessageType(t) -> "Invalid message type: " <> t
    InvalidPayload(msg) -> "Invalid payload: " <> msg
  }
}
