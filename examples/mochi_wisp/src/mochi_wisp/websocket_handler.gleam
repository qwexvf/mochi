// mochi_wisp/websocket_handler.gleam
// WebSocket handler for GraphQL subscriptions using graphql-ws protocol

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/list
import gleam/option.{None, Some}
import logging
import mist.{type Connection, type ResponseData}
import mochi/schema
import mochi/subscription
import mochi/transport/websocket
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// Internal messages for the WebSocket actor
pub type WebSocketMessage {
  /// Send message to client
  SendMessage(String)
}

/// State for the WebSocket connection actor
pub type WebSocketState {
  WebSocketState(conn: websocket.ConnectionState)
}

// ============================================================================
// WebSocket Upgrade Handler
// ============================================================================

/// Handle WebSocket upgrade request
pub fn upgrade_websocket(
  req: Request(Connection),
  gql_schema: schema.Schema,
) -> Response(ResponseData) {
  logging.log(logging.Info, "WebSocket upgrade request")

  let pubsub = get_global_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  let ws_state = websocket.new_connection(gql_schema, pubsub, ctx)

  mist.websocket(
    request: req,
    on_init: fn(_conn) {
      logging.log(logging.Info, "WebSocket connection initialized")
      let selector = process.new_selector()
      #(WebSocketState(conn: ws_state), Some(selector))
    },
    on_close: fn(_state) {
      logging.log(logging.Info, "WebSocket connection closed")
    },
    handler: handle_websocket_message,
  )
}

/// Handle incoming WebSocket messages
/// Note: mist handler signature is fn(state, message, conn) -> Next(state, msg)
fn handle_websocket_message(
  state: WebSocketState,
  message: mist.WebsocketMessage(WebSocketMessage),
  conn: mist.WebsocketConnection,
) -> mist.Next(WebSocketState, WebSocketMessage) {
  case message {
    mist.Text(text) -> {
      logging.log(logging.Debug, "WS received: " <> text)
      handle_client_message(state, conn, text)
    }
    mist.Binary(_) -> {
      // Binary messages not supported by graphql-ws
      mist.continue(state)
    }
    mist.Custom(SendMessage(msg)) -> {
      let _ = mist.send_text_frame(conn, msg)
      mist.continue(state)
    }
    mist.Closed | mist.Shutdown -> {
      // Clean up subscriptions
      let _ = websocket.cleanup(state.conn)
      mist.stop()
    }
  }
}

/// Process a client message according to graphql-ws protocol
fn handle_client_message(
  state: WebSocketState,
  conn: mist.WebsocketConnection,
  text: String,
) -> mist.Next(WebSocketState, WebSocketMessage) {
  case websocket.decode_client_message(text) {
    Ok(client_msg) -> {
      let result = websocket.handle_message(state.conn, client_msg)
      handle_protocol_result(state, conn, result)
    }
    Error(err) -> {
      logging.log(
        logging.Warning,
        "Failed to decode WS message: " <> websocket.format_decode_error(err),
      )
      mist.continue(state)
    }
  }
}

/// Handle the result of protocol message processing
fn handle_protocol_result(
  _state: WebSocketState,
  conn: mist.WebsocketConnection,
  result: websocket.HandleResult,
) -> mist.Next(WebSocketState, WebSocketMessage) {
  case result {
    websocket.HandleOk(new_conn_state, response) -> {
      // Send response if any
      case response {
        Some(msg) -> {
          let json = websocket.encode_server_message(msg)
          logging.log(logging.Debug, "WS sending: " <> json)
          let _ = mist.send_text_frame(conn, json)
          Nil
        }
        None -> Nil
      }
      mist.continue(WebSocketState(conn: new_conn_state))
    }

    websocket.HandleMultiple(new_conn_state, responses) -> {
      // Send all responses
      list.each(responses, fn(msg) {
        let json = websocket.encode_server_message(msg)
        let _ = mist.send_text_frame(conn, json)
        Nil
      })
      mist.continue(WebSocketState(conn: new_conn_state))
    }

    websocket.HandleClose(reason) -> {
      logging.log(logging.Warning, "WS closing: " <> reason)
      // Send close frame with reason
      let _ =
        mist.send_text_frame(
          conn,
          "{\"type\":\"error\",\"payload\":{\"message\":\"" <> reason <> "\"}}",
        )
      mist.stop()
    }
  }
}

// ============================================================================
// Global PubSub Management
// ============================================================================

/// Get the global PubSub instance (stored in persistent_term)
@external(erlang, "mochi_pubsub_ffi", "get_pubsub")
fn get_global_pubsub() -> subscription.PubSub

/// Set the global PubSub instance
@external(erlang, "mochi_pubsub_ffi", "set_pubsub")
pub fn set_global_pubsub(pubsub: subscription.PubSub) -> Nil

/// Initialize the global PubSub - call at startup
pub fn init_pubsub() -> Nil {
  let pubsub = subscription.new_pubsub()
  set_global_pubsub(pubsub)
  logging.log(logging.Info, "Global PubSub initialized")
}

/// Publish an event to all subscribers of a topic
pub fn publish(topic: String, data: Dynamic) -> Nil {
  let pubsub = get_global_pubsub()
  subscription.publish(pubsub, topic, data)
}
