// WebSocket transport tests
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import mochi/executor
import mochi/schema
import mochi/subscription
import mochi/transport/websocket.{
  Complete, ConnectionAck, ConnectionInit, HandleClose, HandleOk, Next, Ping,
  Pong, ServerComplete, ServerPong, Subscribe, SubscriptionPayload,
}
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

fn create_test_schema() -> schema.Schema {
  schema.schema()
}

fn create_test_state() -> websocket.ConnectionState {
  let test_schema = create_test_schema()
  let pubsub = subscription.new_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  websocket.new_connection(test_schema, pubsub, ctx)
}

fn create_acknowledged_state() -> websocket.ConnectionState {
  let state = create_test_state()
  case websocket.handle_message(state, ConnectionInit(None)) {
    HandleOk(new_state, _) -> new_state
    _ -> state
  }
}

// ============================================================================
// ConnectionInit Tests
// ============================================================================

pub fn connection_init_success_test() {
  let state = create_test_state()
  should.equal(state.acknowledged, False)

  let result = websocket.handle_message(state, ConnectionInit(None))

  case result {
    HandleOk(new_state, response) -> {
      should.equal(new_state.acknowledged, True)
      should.equal(response, Some(ConnectionAck(None)))
    }
    _ -> should.fail()
  }
}

pub fn connection_init_with_payload_test() {
  let state = create_test_state()
  let payload = dict.from_list([#("token", types.to_dynamic("secret123"))])

  let result = websocket.handle_message(state, ConnectionInit(Some(payload)))

  case result {
    HandleOk(new_state, _) -> {
      should.equal(new_state.acknowledged, True)
      should.be_true(option.is_some(new_state.connection_params))
    }
    _ -> should.fail()
  }
}

pub fn connection_init_duplicate_rejected_test() {
  let state = create_acknowledged_state()

  let result = websocket.handle_message(state, ConnectionInit(None))

  case result {
    HandleClose(reason) -> {
      should.equal(reason, "Too many initialization requests")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Subscribe Tests
// ============================================================================

pub fn subscribe_requires_auth_test() {
  let state = create_test_state()
  // Not acknowledged yet

  let payload =
    SubscriptionPayload(
      query: "subscription { onMessage { id } }",
      variables: None,
      operation_name: None,
      extensions: None,
    )

  let result = websocket.handle_message(state, Subscribe("sub-1", payload))

  case result {
    HandleClose(reason) -> {
      should.equal(reason, "Unauthorized")
    }
    _ -> should.fail()
  }
}

pub fn subscribe_success_test() {
  let state = create_acknowledged_state()

  let payload =
    SubscriptionPayload(
      query: "subscription { onMessage { id } }",
      variables: None,
      operation_name: None,
      extensions: None,
    )

  let result = websocket.handle_message(state, Subscribe("sub-1", payload))

  case result {
    HandleOk(new_state, _) -> {
      should.be_true(websocket.has_subscription(new_state, "sub-1"))
    }
    _ -> should.fail()
  }
}

pub fn subscribe_duplicate_rejected_test() {
  let state = create_acknowledged_state()

  let payload =
    SubscriptionPayload(
      query: "subscription { onMessage { id } }",
      variables: None,
      operation_name: None,
      extensions: None,
    )

  // First subscription
  let result1 = websocket.handle_message(state, Subscribe("sub-1", payload))
  let state2 = case result1 {
    HandleOk(s, _) -> s
    _ -> state
  }

  // Duplicate subscription
  let result2 = websocket.handle_message(state2, Subscribe("sub-1", payload))

  case result2 {
    HandleClose(reason) -> {
      should.equal(reason, "Subscriber for sub-1 already exists")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Complete Tests
// ============================================================================

pub fn complete_existing_subscription_test() {
  let state = create_acknowledged_state()

  let payload =
    SubscriptionPayload(
      query: "subscription { onMessage { id } }",
      variables: None,
      operation_name: None,
      extensions: None,
    )

  // Create subscription
  let state2 = case websocket.handle_message(state, Subscribe("sub-1", payload)) {
    HandleOk(s, _) -> s
    _ -> state
  }

  should.be_true(websocket.has_subscription(state2, "sub-1"))

  // Complete it
  let result = websocket.handle_message(state2, Complete("sub-1"))

  case result {
    HandleOk(new_state, response) -> {
      should.be_false(websocket.has_subscription(new_state, "sub-1"))
      should.equal(response, Some(ServerComplete("sub-1")))
    }
    _ -> should.fail()
  }
}

pub fn complete_nonexistent_subscription_test() {
  let state = create_acknowledged_state()

  let result = websocket.handle_message(state, Complete("nonexistent"))

  case result {
    HandleOk(_, response) -> {
      // Should still send Complete acknowledgement
      should.equal(response, Some(ServerComplete("nonexistent")))
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Ping/Pong Tests
// ============================================================================

pub fn ping_responds_with_pong_test() {
  let state = create_test_state()

  let result = websocket.handle_message(state, Ping(None))

  case result {
    HandleOk(_, response) -> {
      should.equal(response, Some(ServerPong(None)))
    }
    _ -> should.fail()
  }
}

pub fn ping_with_payload_echoed_test() {
  let state = create_test_state()
  let payload = dict.from_list([#("timestamp", types.to_dynamic(12_345))])

  let result = websocket.handle_message(state, Ping(Some(payload)))

  case result {
    HandleOk(_, response) -> {
      should.equal(response, Some(ServerPong(Some(payload))))
    }
    _ -> should.fail()
  }
}

pub fn pong_no_response_test() {
  let state = create_test_state()

  let result = websocket.handle_message(state, Pong(None))

  case result {
    HandleOk(_, response) -> {
      should.equal(response, None)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Cleanup Tests
// ============================================================================

pub fn cleanup_removes_all_subscriptions_test() {
  let state = create_acknowledged_state()

  let payload =
    SubscriptionPayload(
      query: "subscription { onMessage { id } }",
      variables: None,
      operation_name: None,
      extensions: None,
    )

  // Create multiple subscriptions
  let state2 = case websocket.handle_message(state, Subscribe("sub-1", payload)) {
    HandleOk(s, _) -> s
    _ -> state
  }
  let state3 = case websocket.handle_message(state2, Subscribe("sub-2", payload)) {
    HandleOk(s, _) -> s
    _ -> state2
  }

  should.be_true(websocket.has_subscription(state3, "sub-1"))
  should.be_true(websocket.has_subscription(state3, "sub-2"))

  let cleaned = websocket.cleanup(state3)

  should.be_false(websocket.has_subscription(cleaned, "sub-1"))
  should.be_false(websocket.has_subscription(cleaned, "sub-2"))
  should.equal(websocket.get_active_subscriptions(cleaned), [])
}

// ============================================================================
// JSON Encoding Tests
// ============================================================================

pub fn encode_connection_ack_test() {
  let msg = ConnectionAck(None)
  let json = websocket.encode_server_message(msg)

  should.be_true(json |> string_contains("\"type\":\"connection_ack\""))
}

pub fn encode_next_test() {
  let result =
    executor.ExecutionResult(
      data: Some(types.to_dynamic(dict.from_list([#("id", types.to_dynamic("1"))]))),
      errors: [],
    )
  let msg = Next("sub-1", result)
  let json = websocket.encode_server_message(msg)

  should.be_true(json |> string_contains("\"type\":\"next\""))
  should.be_true(json |> string_contains("\"id\":\"sub-1\""))
}

pub fn encode_complete_test() {
  let msg = ServerComplete("sub-1")
  let json = websocket.encode_server_message(msg)

  should.be_true(json |> string_contains("\"type\":\"complete\""))
  should.be_true(json |> string_contains("\"id\":\"sub-1\""))
}

pub fn encode_pong_test() {
  let msg = ServerPong(None)
  let json = websocket.encode_server_message(msg)

  should.be_true(json |> string_contains("\"type\":\"pong\""))
}

// ============================================================================
// JSON Decoding Tests
// ============================================================================

pub fn decode_connection_init_test() {
  let json = "{\"type\":\"connection_init\"}"
  let result = websocket.decode_client_message(json)

  case result {
    Ok(ConnectionInit(None)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_subscribe_test() {
  let json =
    "{\"type\":\"subscribe\",\"id\":\"1\",\"payload\":{\"query\":\"subscription { test }\"}}"
  let result = websocket.decode_client_message(json)

  case result {
    Ok(Subscribe(id, payload)) -> {
      should.equal(id, "1")
      should.equal(payload.query, "subscription { test }")
    }
    _ -> should.fail()
  }
}

pub fn decode_complete_test() {
  let json = "{\"type\":\"complete\",\"id\":\"sub-1\"}"
  let result = websocket.decode_client_message(json)

  case result {
    Ok(Complete(id)) -> should.equal(id, "sub-1")
    _ -> should.fail()
  }
}

pub fn decode_ping_test() {
  let json = "{\"type\":\"ping\"}"
  let result = websocket.decode_client_message(json)

  case result {
    Ok(Ping(None)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_pong_test() {
  let json = "{\"type\":\"pong\"}"
  let result = websocket.decode_client_message(json)

  case result {
    Ok(Pong(None)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_invalid_json_test() {
  let json = "not valid json"
  let result = websocket.decode_client_message(json)

  case result {
    Error(websocket.InvalidJson(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_invalid_message_type_test() {
  let json = "{\"type\":\"unknown_type\"}"
  let result = websocket.decode_client_message(json)

  case result {
    Error(websocket.InvalidMessageType("unknown_type")) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn decode_missing_field_test() {
  let json = "{\"type\":\"complete\"}"
  // Missing "id" field
  let result = websocket.decode_client_message(json)

  case result {
    Error(websocket.MissingField("id")) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

import gleam/string

fn string_contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
