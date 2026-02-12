// mochi_wisp.gleam
// Entry point for the Mochi + Wisp GraphQL server example
//
// Architecture:
//   - OTP supervision for fault tolerance
//   - Actor-based cache and metrics management
//   - Process isolation for request handling
//   - ETS for high-performance caching
//   - persistent_term for schema storage
//
// Run with: gleam run
// Test with: curl -X POST http://localhost:8000/graphql \
//   -H "Content-Type: application/json" \
//   -d '{"query": "{ users { id name email } }"}'

import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/request
import gleam/http/response
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import logging
import mist.{type Connection, type ResponseData}
import wisp
import wisp/wisp_mist

import mochi_wisp/router
import mochi_wisp/supervisor
import mochi_wisp/websocket_handler

const port = 8000

pub fn main() {
  // Start the logger with info level by default
  wisp.configure_logger()
  logging.set_level(logging.Info)

  logging.log(logging.Info, "===========================================")
  logging.log(logging.Info, "  Mochi GraphQL Server (OTP Edition)")
  logging.log(logging.Info, "===========================================")
  logging.log(logging.Info, "")
  logging.log(logging.Info, "BEAM VM Features Enabled:")
  logging.log(logging.Info, "  - OTP Actors for cache & metrics")
  logging.log(logging.Info, "  - ETS tables with read/write concurrency")
  logging.log(logging.Info, "  - persistent_term for schema caching")
  logging.log(logging.Info, "  - Process isolation for fault tolerance")
  logging.log(logging.Info, "")

  // Initialize OTP application (actors, ETS tables, etc.)
  logging.log(logging.Info, "Initializing OTP application...")
  let app_result = supervisor.init_app()

  case app_result {
    Ok(app_state) -> {
      // Store app state for request handlers
      set_app_state(app_state)

      // Initialize and cache the schema (persistent_term)
      logging.log(logging.Info, "Building and caching GraphQL schema...")
      router.init_schema()

      // Warm up the cache with common queries
      logging.log(logging.Info, "Warming up query cache...")
      supervisor.warm_cache(app_state, common_queries())

      start_http_server(app_state)
    }
    Error(msg) -> {
      logging.log(logging.Error, "Failed to initialize application: " <> msg)
      panic as "Application initialization failed"
    }
  }
}

fn start_http_server(app_state: supervisor.AppState) -> Nil {
  // Initialize WebSocket PubSub
  websocket_handler.init_pubsub()

  // Create the router
  let wisp_handler = router.handle_request

  // Create a secret key for signing cookies (not used in this example)
  let secret_key_base = wisp.random_string(64)

  // Get the GraphQL schema for WebSocket handler
  let gql_schema = router.get_schema()

  logging.log(logging.Info, "Configuring HTTP server...")

  // Create a combined handler that routes WebSocket upgrades separately
  let handler = fn(req: request.Request(Connection)) -> response.Response(ResponseData) {
    // Check if this is a WebSocket upgrade request to /graphql
    case is_websocket_upgrade(req), request.path_segments(req) {
      True, ["graphql"] -> {
        // Handle WebSocket upgrade for GraphQL subscriptions
        websocket_handler.upgrade_websocket(req, gql_schema)
      }
      _, _ -> {
        // Regular HTTP request - pass to wisp
        wisp_mist.handler(wisp_handler, secret_key_base)(req)
      }
    }
  }

  // Start the server (bind to 0.0.0.0 for Docker)
  let assert Ok(_) =
    mist.new(handler)
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start

  logging.log(logging.Info, "Server started successfully on port " <> int.to_string(port))
  logging.log(logging.Info, "")
  logging.log(logging.Info, "Endpoints:")
  logging.log(logging.Info, "  POST /graphql     - Execute GraphQL queries")
  logging.log(logging.Info, "  WS /graphql       - WebSocket subscriptions (graphql-ws)")
  logging.log(logging.Info, "  OPTIONS /graphql  - CORS preflight")
  logging.log(logging.Info, "  GET /health       - Health check")
  logging.log(logging.Info, "")
  logging.log(logging.Info, "Example query:")
  logging.log(logging.Info, "  curl -X POST http://localhost:" <> int.to_string(port) <> "/graphql \\")
  logging.log(logging.Info, "    -H \"Content-Type: application/json\" \\")
  logging.log(logging.Info, "    -d '{\"query\": \"{ users { id name email } }\"}'")
  logging.log(logging.Info, "")
  logging.log(logging.Info, "Set DEBUG=1 for verbose logging")

  // Also print to stdout for immediate feedback
  io.println("")
  io.println("GraphQL server running at http://localhost:" <> int.to_string(port) <> "/graphql")
  io.println("")

  // Print initial metrics
  let stats = supervisor.get_cache_stats(app_state)
  logging.log(
    logging.Info,
    "Cache stats: " <>
      int.to_string(stats.size) <>
      " queries cached",
  )

  // Keep the process alive
  process.sleep_forever()
}

// ============================================================================
// Application State Management (using persistent_term)
// ============================================================================

/// Store the app state in persistent_term for access from request handlers
@external(erlang, "mochi_app_state_ffi", "set_app_state")
fn set_app_state(state: supervisor.AppState) -> Nil

/// Get the app state from persistent_term
@external(erlang, "mochi_app_state_ffi", "get_app_state")
pub fn get_app_state() -> supervisor.AppState

// ============================================================================
// Common Queries for Cache Warming
// ============================================================================

fn common_queries() -> List(String) {
  [
    // Simple queries that are likely to be used frequently
    "{ users { id name } }",
    "{ users { id name email } }",
    "{ users { id name email role } }",
    "{ user(id: \"1\") { id name } }",
    "{ user(id: \"1\") { id name email role } }",
    // Introspection queries (common in GraphQL clients)
    "{ __schema { types { name } } }",
    "{ __type(name: \"User\") { name fields { name type { name } } } }",
  ]
}

// ============================================================================
// WebSocket Helpers
// ============================================================================

/// Check if a request is a WebSocket upgrade request
fn is_websocket_upgrade(req: request.Request(Connection)) -> Bool {
  let headers = req.headers
  let upgrade_header =
    list.find(headers, fn(h) {
      let #(name, _) = h
      string.lowercase(name) == "upgrade"
    })

  case upgrade_header {
    Ok(#(_, value)) -> string.lowercase(value) == "websocket"
    Error(_) -> False
  }
}
