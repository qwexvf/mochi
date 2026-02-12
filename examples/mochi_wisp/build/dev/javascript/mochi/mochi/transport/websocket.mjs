import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $error from "../../mochi/error.mjs";
import * as $executor from "../../mochi/executor.mjs";
import * as $schema from "../../mochi/schema.mjs";
import * as $subscription from "../../mochi/subscription.mjs";

/**
 * First message from client to initialize connection
 * Payload may contain connection parameters (auth tokens, etc.)
 */
export class ConnectionInit extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ClientMessage$ConnectionInit = (payload) =>
  new ConnectionInit(payload);
export const ClientMessage$isConnectionInit = (value) =>
  value instanceof ConnectionInit;
export const ClientMessage$ConnectionInit$payload = (value) => value.payload;
export const ClientMessage$ConnectionInit$0 = (value) => value.payload;

/**
 * Subscribe to a GraphQL operation
 */
export class Subscribe extends $CustomType {
  constructor(id, payload) {
    super();
    this.id = id;
    this.payload = payload;
  }
}
export const ClientMessage$Subscribe = (id, payload) =>
  new Subscribe(id, payload);
export const ClientMessage$isSubscribe = (value) => value instanceof Subscribe;
export const ClientMessage$Subscribe$id = (value) => value.id;
export const ClientMessage$Subscribe$0 = (value) => value.id;
export const ClientMessage$Subscribe$payload = (value) => value.payload;
export const ClientMessage$Subscribe$1 = (value) => value.payload;

/**
 * Client unsubscribing from an operation
 */
export class Complete extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}
export const ClientMessage$Complete = (id) => new Complete(id);
export const ClientMessage$isComplete = (value) => value instanceof Complete;
export const ClientMessage$Complete$id = (value) => value.id;
export const ClientMessage$Complete$0 = (value) => value.id;

/**
 * Ping message for keep-alive
 */
export class Ping extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ClientMessage$Ping = (payload) => new Ping(payload);
export const ClientMessage$isPing = (value) => value instanceof Ping;
export const ClientMessage$Ping$payload = (value) => value.payload;
export const ClientMessage$Ping$0 = (value) => value.payload;

/**
 * Pong response to server ping
 */
export class Pong extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ClientMessage$Pong = (payload) => new Pong(payload);
export const ClientMessage$isPong = (value) => value instanceof Pong;
export const ClientMessage$Pong$payload = (value) => value.payload;
export const ClientMessage$Pong$0 = (value) => value.payload;

export class SubscriptionPayload extends $CustomType {
  constructor(query, variables, operation_name, extensions) {
    super();
    this.query = query;
    this.variables = variables;
    this.operation_name = operation_name;
    this.extensions = extensions;
  }
}
export const SubscriptionPayload$SubscriptionPayload = (query, variables, operation_name, extensions) =>
  new SubscriptionPayload(query, variables, operation_name, extensions);
export const SubscriptionPayload$isSubscriptionPayload = (value) =>
  value instanceof SubscriptionPayload;
export const SubscriptionPayload$SubscriptionPayload$query = (value) =>
  value.query;
export const SubscriptionPayload$SubscriptionPayload$0 = (value) => value.query;
export const SubscriptionPayload$SubscriptionPayload$variables = (value) =>
  value.variables;
export const SubscriptionPayload$SubscriptionPayload$1 = (value) =>
  value.variables;
export const SubscriptionPayload$SubscriptionPayload$operation_name = (value) =>
  value.operation_name;
export const SubscriptionPayload$SubscriptionPayload$2 = (value) =>
  value.operation_name;
export const SubscriptionPayload$SubscriptionPayload$extensions = (value) =>
  value.extensions;
export const SubscriptionPayload$SubscriptionPayload$3 = (value) =>
  value.extensions;

/**
 * Acknowledge successful connection initialization
 */
export class ConnectionAck extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ServerMessage$ConnectionAck = (payload) =>
  new ConnectionAck(payload);
export const ServerMessage$isConnectionAck = (value) =>
  value instanceof ConnectionAck;
export const ServerMessage$ConnectionAck$payload = (value) => value.payload;
export const ServerMessage$ConnectionAck$0 = (value) => value.payload;

/**
 * Send subscription data to client
 */
export class Next extends $CustomType {
  constructor(id, payload) {
    super();
    this.id = id;
    this.payload = payload;
  }
}
export const ServerMessage$Next = (id, payload) => new Next(id, payload);
export const ServerMessage$isNext = (value) => value instanceof Next;
export const ServerMessage$Next$id = (value) => value.id;
export const ServerMessage$Next$0 = (value) => value.id;
export const ServerMessage$Next$payload = (value) => value.payload;
export const ServerMessage$Next$1 = (value) => value.payload;

/**
 * Send subscription errors to client
 */
export class SubscriptionError extends $CustomType {
  constructor(id, payload) {
    super();
    this.id = id;
    this.payload = payload;
  }
}
export const ServerMessage$SubscriptionError = (id, payload) =>
  new SubscriptionError(id, payload);
export const ServerMessage$isSubscriptionError = (value) =>
  value instanceof SubscriptionError;
export const ServerMessage$SubscriptionError$id = (value) => value.id;
export const ServerMessage$SubscriptionError$0 = (value) => value.id;
export const ServerMessage$SubscriptionError$payload = (value) => value.payload;
export const ServerMessage$SubscriptionError$1 = (value) => value.payload;

/**
 * Server completing a subscription (or acknowledging client complete)
 */
export class ServerComplete extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}
export const ServerMessage$ServerComplete = (id) => new ServerComplete(id);
export const ServerMessage$isServerComplete = (value) =>
  value instanceof ServerComplete;
export const ServerMessage$ServerComplete$id = (value) => value.id;
export const ServerMessage$ServerComplete$0 = (value) => value.id;

/**
 * Ping message for keep-alive
 */
export class ServerPing extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ServerMessage$ServerPing = (payload) => new ServerPing(payload);
export const ServerMessage$isServerPing = (value) =>
  value instanceof ServerPing;
export const ServerMessage$ServerPing$payload = (value) => value.payload;
export const ServerMessage$ServerPing$0 = (value) => value.payload;

/**
 * Pong response to client ping
 */
export class ServerPong extends $CustomType {
  constructor(payload) {
    super();
    this.payload = payload;
  }
}
export const ServerMessage$ServerPong = (payload) => new ServerPong(payload);
export const ServerMessage$isServerPong = (value) =>
  value instanceof ServerPong;
export const ServerMessage$ServerPong$payload = (value) => value.payload;
export const ServerMessage$ServerPong$0 = (value) => value.payload;

export class ConnectionState extends $CustomType {
  constructor(schema, pubsub, execution_context, active_subscriptions, acknowledged, connection_params) {
    super();
    this.schema = schema;
    this.pubsub = pubsub;
    this.execution_context = execution_context;
    this.active_subscriptions = active_subscriptions;
    this.acknowledged = acknowledged;
    this.connection_params = connection_params;
  }
}
export const ConnectionState$ConnectionState = (schema, pubsub, execution_context, active_subscriptions, acknowledged, connection_params) =>
  new ConnectionState(schema,
  pubsub,
  execution_context,
  active_subscriptions,
  acknowledged,
  connection_params);
export const ConnectionState$isConnectionState = (value) =>
  value instanceof ConnectionState;
export const ConnectionState$ConnectionState$schema = (value) => value.schema;
export const ConnectionState$ConnectionState$0 = (value) => value.schema;
export const ConnectionState$ConnectionState$pubsub = (value) => value.pubsub;
export const ConnectionState$ConnectionState$1 = (value) => value.pubsub;
export const ConnectionState$ConnectionState$execution_context = (value) =>
  value.execution_context;
export const ConnectionState$ConnectionState$2 = (value) =>
  value.execution_context;
export const ConnectionState$ConnectionState$active_subscriptions = (value) =>
  value.active_subscriptions;
export const ConnectionState$ConnectionState$3 = (value) =>
  value.active_subscriptions;
export const ConnectionState$ConnectionState$acknowledged = (value) =>
  value.acknowledged;
export const ConnectionState$ConnectionState$4 = (value) => value.acknowledged;
export const ConnectionState$ConnectionState$connection_params = (value) =>
  value.connection_params;
export const ConnectionState$ConnectionState$5 = (value) =>
  value.connection_params;

/**
 * Successfully handled, returns updated state and optional response
 */
export class HandleOk extends $CustomType {
  constructor(state, response) {
    super();
    this.state = state;
    this.response = response;
  }
}
export const HandleResult$HandleOk = (state, response) =>
  new HandleOk(state, response);
export const HandleResult$isHandleOk = (value) => value instanceof HandleOk;
export const HandleResult$HandleOk$state = (value) => value.state;
export const HandleResult$HandleOk$0 = (value) => value.state;
export const HandleResult$HandleOk$response = (value) => value.response;
export const HandleResult$HandleOk$1 = (value) => value.response;

/**
 * Multiple responses needed (e.g., acknowledging then sending data)
 */
export class HandleMultiple extends $CustomType {
  constructor(state, responses) {
    super();
    this.state = state;
    this.responses = responses;
  }
}
export const HandleResult$HandleMultiple = (state, responses) =>
  new HandleMultiple(state, responses);
export const HandleResult$isHandleMultiple = (value) =>
  value instanceof HandleMultiple;
export const HandleResult$HandleMultiple$state = (value) => value.state;
export const HandleResult$HandleMultiple$0 = (value) => value.state;
export const HandleResult$HandleMultiple$responses = (value) => value.responses;
export const HandleResult$HandleMultiple$1 = (value) => value.responses;

/**
 * Connection should be closed with error
 */
export class HandleClose extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const HandleResult$HandleClose = (reason) => new HandleClose(reason);
export const HandleResult$isHandleClose = (value) =>
  value instanceof HandleClose;
export const HandleResult$HandleClose$reason = (value) => value.reason;
export const HandleResult$HandleClose$0 = (value) => value.reason;

/**
 * Message type constants for serialization
 */
export const msg_type_connection_init = "connection_init";

export const msg_type_connection_ack = "connection_ack";

export const msg_type_ping = "ping";

export const msg_type_pong = "pong";

export const msg_type_subscribe = "subscribe";

export const msg_type_next = "next";

export const msg_type_error = "error";

export const msg_type_complete = "complete";

/**
 * Create a new connection state
 */
export function new_connection(schema, pubsub, execution_context) {
  return new ConnectionState(
    schema,
    pubsub,
    execution_context,
    $dict.new$(),
    false,
    new None(),
  );
}

/**
 * Create connection state with custom parameters
 */
export function new_connection_with_params(
  schema,
  pubsub,
  execution_context,
  params
) {
  return new ConnectionState(
    schema,
    pubsub,
    execution_context,
    $dict.new$(),
    false,
    new Some(params),
  );
}

/**
 * Handle ConnectionInit message
 * 
 * @ignore
 */
function handle_connection_init(state, payload) {
  let $ = state.acknowledged;
  if ($) {
    return new HandleClose("Too many initialization requests");
  } else {
    let new_state = new ConnectionState(
      state.schema,
      state.pubsub,
      state.execution_context,
      state.active_subscriptions,
      true,
      payload,
    );
    return new HandleOk(new_state, new Some(new ConnectionAck(new None())));
  }
}

/**
 * Handle Complete message (client unsubscribing)
 * 
 * @ignore
 */
function handle_complete(state, id) {
  let $ = $dict.get(state.active_subscriptions, id);
  if ($ instanceof Ok) {
    let subscription_id = $[0];
    let new_pubsub = $subscription.unsubscribe(state.pubsub, subscription_id);
    let new_state = new ConnectionState(
      state.schema,
      new_pubsub,
      state.execution_context,
      $dict.delete$(state.active_subscriptions, id),
      state.acknowledged,
      state.connection_params,
    );
    return new HandleOk(new_state, new Some(new ServerComplete(id)));
  } else {
    return new HandleOk(state, new Some(new ServerComplete(id)));
  }
}

/**
 * Handle Ping message
 * 
 * @ignore
 */
function handle_ping(state, payload) {
  return new HandleOk(state, new Some(new ServerPong(payload)));
}

/**
 * Require connection to be acknowledged before processing
 * 
 * @ignore
 */
function require_acknowledged(state, next) {
  let $ = state.acknowledged;
  if ($) {
    return next();
  } else {
    return new HandleClose("Unauthorized");
  }
}

/**
 * Check for duplicate subscription ID
 * 
 * @ignore
 */
function check_duplicate_subscription(state, id, next) {
  let $ = $dict.has_key(state.active_subscriptions, id);
  if ($) {
    return new HandleClose(("Subscriber for " + id) + " already exists");
  } else {
    return next();
  }
}

/**
 * Handle Subscribe message
 * 
 * @ignore
 */
function handle_subscribe(state, id, _) {
  return require_acknowledged(
    state,
    () => {
      return check_duplicate_subscription(
        state,
        id,
        () => {
          let new_state = new ConnectionState(
            state.schema,
            state.pubsub,
            state.execution_context,
            $dict.insert(state.active_subscriptions, id, "pending_" + id),
            state.acknowledged,
            state.connection_params,
          );
          return new HandleOk(new_state, new None());
        },
      );
    },
  );
}

/**
 * Handle an incoming client message
 */
export function handle_message(state, message) {
  if (message instanceof ConnectionInit) {
    let payload = message.payload;
    return handle_connection_init(state, payload);
  } else if (message instanceof Subscribe) {
    let id = message.id;
    let payload = message.payload;
    return handle_subscribe(state, id, payload);
  } else if (message instanceof Complete) {
    let id = message.id;
    return handle_complete(state, id);
  } else if (message instanceof Ping) {
    let payload = message.payload;
    return handle_ping(state, payload);
  } else {
    return new HandleOk(state, new None());
  }
}

/**
 * Send a Next message to the client
 */
export function send_next(id, result) {
  return new Next(id, result);
}

/**
 * Send an Error message to the client
 */
export function send_error(id, errors) {
  return new SubscriptionError(id, errors);
}

/**
 * Send a Complete message to the client
 */
export function send_complete(id) {
  return new ServerComplete(id);
}

/**
 * Get all active subscription IDs
 */
export function get_active_subscriptions(state) {
  return $dict.keys(state.active_subscriptions);
}

/**
 * Check if a subscription is active
 */
export function has_subscription(state, id) {
  return $dict.has_key(state.active_subscriptions, id);
}

/**
 * Clean up all subscriptions (e.g., on disconnect)
 */
export function cleanup(state) {
  let new_pubsub = $dict.fold(
    state.active_subscriptions,
    state.pubsub,
    (pubsub, _, sub_id) => { return $subscription.unsubscribe(pubsub, sub_id); },
  );
  return new ConnectionState(
    state.schema,
    new_pubsub,
    state.execution_context,
    $dict.new$(),
    state.acknowledged,
    state.connection_params,
  );
}

/**
 * Get the message type string for a server message
 */
export function server_message_type(message) {
  if (message instanceof ConnectionAck) {
    return msg_type_connection_ack;
  } else if (message instanceof Next) {
    return msg_type_next;
  } else if (message instanceof SubscriptionError) {
    return msg_type_error;
  } else if (message instanceof ServerComplete) {
    return msg_type_complete;
  } else if (message instanceof ServerPing) {
    return msg_type_ping;
  } else {
    return msg_type_pong;
  }
}

/**
 * Get the message type string for a client message
 */
export function client_message_type(message) {
  if (message instanceof ConnectionInit) {
    return msg_type_connection_init;
  } else if (message instanceof Subscribe) {
    return msg_type_subscribe;
  } else if (message instanceof Complete) {
    return msg_type_complete;
  } else if (message instanceof Ping) {
    return msg_type_ping;
  } else {
    return msg_type_pong;
  }
}
