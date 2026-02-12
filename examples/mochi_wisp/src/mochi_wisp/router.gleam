// mochi_wisp/router.gleam
// HTTP router for the GraphQL server with request logging

import gleam/http
import gleam/int
import gleam/json
import gleam/string
import logging
import mochi/playground
import mochi/schema as mochi_schema
import wisp.{type Request, type Response}

import mochi_wisp/graphql_handler
import mochi_wisp/query_cache
import mochi_wisp/schema

// Global cached schema - initialized at startup via init_schema()
// This avoids rebuilding the schema on every request
@external(erlang, "mochi_router_ffi", "get_schema")
fn get_cached_schema() -> mochi_schema.Schema

@external(erlang, "mochi_router_ffi", "set_schema")
fn set_cached_schema(schema: mochi_schema.Schema) -> Nil

/// Get the cached GraphQL schema (for use by WebSocket handler)
pub fn get_schema() -> mochi_schema.Schema {
  get_cached_schema()
}

/// Initialize the schema cache - call once at startup
pub fn init_schema() -> Nil {
  let graphql_schema = schema.build_schema()
  set_cached_schema(graphql_schema)
  logging.log(logging.Info, "GraphQL schema initialized and cached")
}

/// Main request handler with request logging
pub fn handle_request(req: Request) -> Response {
  // Log incoming request
  let method = http.method_to_string(req.method)
  let path = "/" <> string.join(wisp.path_segments(req), "/")
  logging.log(logging.Info, method <> " " <> path)

  // Get the cached schema (initialized at startup)
  let graphql_schema = get_cached_schema()

  // Route the request and log response
  let response = route_request(req, graphql_schema)

  response
}

/// Route the request to the appropriate handler
fn route_request(req: Request, graphql_schema: mochi_schema.Schema) -> Response {
  case wisp.path_segments(req) {
    // GraphQL endpoint
    ["graphql"] -> handle_graphql_route(req, graphql_schema)

    // Health check - simple OK
    ["health"] -> {
      logging.log(logging.Debug, "Health check OK")
      wisp.ok()
    }

    // Detailed health with cache stats
    ["health", "detailed"] -> handle_health_detailed()

    // Metrics endpoint - returns cache and performance stats
    ["metrics"] -> handle_metrics()

    // Cache management
    ["cache", "clear"] ->
      case req.method {
        http.Post -> {
          query_cache.clear()
          logging.log(logging.Info, "Cache cleared via API")
          wisp.json_response("{\"status\": \"cleared\"}", 200)
        }
        _ -> wisp.method_not_allowed([http.Post])
      }

    ["cache", "stats"] -> handle_cache_stats()

    // GraphQL Playgrounds
    ["graphiql"] -> wisp.html_response(playground.graphiql("/graphql"), 200)
    ["playground"] -> wisp.html_response(playground.playground("/graphql"), 200)
    ["sandbox"] -> wisp.html_response(playground.apollo_sandbox("/graphql"), 200)
    ["explorer"] -> wisp.html_response(playground.simple_explorer("/graphql"), 200)

    // Home page with links
    [] ->
      wisp.html_response(
        "<!DOCTYPE html><html><head><title>Mochi GraphQL</title>
        <style>
          body { font-family: -apple-system, sans-serif; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
          h1 { color: #333; } h2 { color: #666; margin-top: 2rem; }
          a { color: #e94560; } code { background: #f4f4f4; padding: 0.2rem 0.4rem; border-radius: 3px; }
          pre { background: #1a1a2e; color: #eee; padding: 1rem; border-radius: 5px; overflow-x: auto; }
          .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; }
          .card { border: 1px solid #ddd; padding: 1rem; border-radius: 5px; }
          .card h3 { margin: 0 0 0.5rem; }
        </style></head><body>"
        <> "<h1>🍡 Mochi GraphQL Server</h1>"
        <> "<p>Code-first GraphQL for Gleam, powered by the BEAM VM.</p>"
        <> "<h2>Interactive Playgrounds</h2>"
        <> "<div class=\"grid\">"
        <> "<div class=\"card\"><h3><a href=\"/graphiql\">GraphiQL</a></h3><p>The classic GraphQL IDE</p></div>"
        <> "<div class=\"card\"><h3><a href=\"/playground\">Playground</a></h3><p>GraphQL Playground (legacy)</p></div>"
        <> "<div class=\"card\"><h3><a href=\"/sandbox\">Apollo Sandbox</a></h3><p>Modern Apollo explorer</p></div>"
        <> "<div class=\"card\"><h3><a href=\"/explorer\">Simple Explorer</a></h3><p>Lightweight, no deps</p></div>"
        <> "</div>"
        <> "<h2>API Endpoints</h2>"
        <> "<ul>"
        <> "<li><code>POST /graphql</code> - Execute GraphQL queries</li>"
        <> "<li><code>GET /health</code> - Health check</li>"
        <> "<li><code>GET /metrics</code> - Performance metrics</li>"
        <> "<li><code>GET /cache/stats</code> - Cache statistics</li>"
        <> "</ul>"
        <> "<h2>Example Query</h2>"
        <> "<pre>curl -X POST http://localhost:8000/graphql \\\n"
        <> "  -H \"Content-Type: application/json\" \\\n"
        <> "  -d '{\"query\": \"{ users { id name email } }\"}'</pre>"
        <> "</body></html>",
        200,
      )

    // 404 for everything else
    _ -> {
      logging.log(logging.Warning, "404 Not Found")
      wisp.not_found()
    }
  }
}

/// Handle detailed health check with system info
fn handle_health_detailed() -> Response {
  let cache_stats = query_cache.stats()

  let body =
    json.to_string(
      json.object([
        #("status", json.string("healthy")),
        #("cache", json.object([
          #("hits", json.int(cache_stats.hits)),
          #("misses", json.int(cache_stats.misses)),
          #("size", json.int(cache_stats.size)),
          #("hit_rate", json.float(calculate_hit_rate(cache_stats.hits, cache_stats.misses))),
        ])),
        #("vm", json.object([
          #("otp_release", json.string(get_otp_release())),
          #("schedulers", json.int(get_scheduler_count())),
        ])),
      ]),
    )

  wisp.json_response(body, 200)
}

/// Handle metrics endpoint
fn handle_metrics() -> Response {
  let cache_stats = query_cache.stats()

  let body =
    json.to_string(
      json.object([
        #("cache", json.object([
          #("queries_cached", json.int(cache_stats.size)),
          #("cache_hits", json.int(cache_stats.hits)),
          #("cache_misses", json.int(cache_stats.misses)),
          #("hit_rate_percent", json.float(calculate_hit_rate(cache_stats.hits, cache_stats.misses))),
        ])),
      ]),
    )

  wisp.json_response(body, 200)
}

/// Handle cache stats endpoint
fn handle_cache_stats() -> Response {
  let stats = query_cache.stats()
  let total = stats.hits + stats.misses

  let body =
    json.to_string(
      json.object([
        #("hits", json.int(stats.hits)),
        #("misses", json.int(stats.misses)),
        #("size", json.int(stats.size)),
        #("total_requests", json.int(total)),
        #("hit_rate_percent", json.float(calculate_hit_rate(stats.hits, stats.misses))),
      ]),
    )

  wisp.json_response(body, 200)
}

fn calculate_hit_rate(hits: Int, misses: Int) -> Float {
  let total = hits + misses
  case total {
    0 -> 0.0
    _ -> int.to_float(hits) /. int.to_float(total) *. 100.0
  }
}

/// Get OTP release version
@external(erlang, "mochi_system_ffi", "otp_release")
fn get_otp_release() -> String

/// Get number of schedulers
@external(erlang, "mochi_system_ffi", "scheduler_count")
fn get_scheduler_count() -> Int

fn handle_graphql_route(req: Request, gql_schema: mochi_schema.Schema) -> Response {
  case req.method {
    // Use quiet handler for benchmarking (no logging overhead)
    http.Post -> graphql_handler.handle_graphql_quiet(req, gql_schema)
    http.Options -> graphql_handler.handle_cors_preflight()
    _ -> {
      logging.log(logging.Warning, "Method not allowed: " <> http.method_to_string(req.method))
      wisp.method_not_allowed([http.Post, http.Options])
    }
  }
}
