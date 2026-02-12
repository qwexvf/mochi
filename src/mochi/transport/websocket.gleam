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
import gleam/option.{type Option, None, Some}
import mochi/error.{type GraphQLError}
import mochi/executor.{type ExecutionResult}
import mochi/schema.{type Schema}
import mochi/subscription.{type PubSub, type SubscriptionId}

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
        ConnectionState(
          ..state,
          acknowledged: True,
          connection_params: payload,
        )
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

  ConnectionState(
    ..state,
    pubsub: new_pubsub,
    active_subscriptions: dict.new(),
  )
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
