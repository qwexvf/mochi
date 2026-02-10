import mochi/executor
import mochi/schema
import gleam/bytes_builder
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/result
import gleam/string
import mist

/// Main entry point for the GraphQL web application
pub fn main() {
  io.println("🚀 Starting GeQL Web Application...")

  // Create simplified GraphQL schema for benchmarking
  let schema = create_benchmark_schema()
  io.println("✅ GraphQL schema created for benchmarking")

  // Start web server
  let assert Ok(_) =
    mist.new(handle_request(_, schema))
    |> mist.port(8080)
    |> mist.start_http

  io.println("🌐 GraphQL server running on http://localhost:8080")
  io.println("📍 GraphQL endpoint: POST /graphql")
  io.println("📍 Health check: GET /health")

  process.sleep_forever()
}

/// Create a simple GraphQL schema for benchmarking
fn create_benchmark_schema() -> schema.Schema {
  // Create a simple User type
  let user_type =
    schema.object("User")
    |> schema.description("A user for benchmarking")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("User ID")
      |> schema.resolver(fn(_info) {
        io.println("🔍 Resolving user.id")
        Error("Benchmark: Dynamic serialization needed")
      }),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("User name")
      |> schema.resolver(fn(_info) {
        io.println("🔍 Resolving user.name")
        Error("Benchmark: Dynamic serialization needed")
      }),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.field_description("User email")
      |> schema.resolver(fn(_info) {
        io.println("🔍 Resolving user.email")
        Error("Benchmark: Dynamic serialization needed")
      }),
    )

  // Create Query type
  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user")
      |> schema.argument(
        schema.arg("id", schema.non_null(schema.id_type()))
        |> schema.arg_description("User ID"),
      )
      |> schema.resolver(fn(_info) {
        io.println("🔍 Resolving query.user")
        Error("Benchmark: Dynamic serialization needed")
      }),
    )
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users")
      |> schema.resolver(fn(_info) {
        io.println("🔍 Resolving query.users")
        Error("Benchmark: Dynamic serialization needed")
      }),
    )

  // Create complete schema
  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
}

/// HTTP request handler
fn handle_request(
  req: Request(mist.Connection),
  schema: schema.Schema,
) -> Response(mist.ResponseData) {
  case request.path_segments(req) {
    ["graphql"] -> handle_graphql(req, schema)
    ["health"] -> handle_health(req)
    [] -> handle_root(req)
    _ ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_builder.from_string("Not Found")))
  }
}

/// Handle GraphQL queries
fn handle_graphql(
  req: Request(mist.Connection),
  schema: schema.Schema,
) -> Response(mist.ResponseData) {
  case req.method {
    http.Post -> {
      // For demo, use a simple hardcoded query
      let query_string = "{ user(id: \"1\") { id name } }"
      io.println("📝 Executing demo query: " <> query_string)

      // Execute GraphQL query
      let result = executor.execute_query(schema, query_string)

      // Convert result to JSON response
      let json_response = case result.data {
        Some(_data) -> {
          io.println("✅ Query executed successfully")
          "{\"data\":{\"message\":\"Query executed - Dynamic serialization needed for actual data\",\"note\":\"See examples/geql_phoenix_benchmark for working GraphQL server\"},\"errors\":null}"
        }
        None -> {
          io.println("❌ Query execution failed")
          "{\"data\":null,\"errors\":[{\"message\":\"Execution failed - check server logs\"}]}"
        }
      }

      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_header("access-control-allow-origin", "*")
      |> response.set_header("access-control-allow-headers", "Content-Type")
      |> response.set_body(mist.Bytes(bytes_builder.from_string(json_response)))
    }
    http.Options -> {
      // Handle CORS preflight
      response.new(200)
      |> response.set_header("access-control-allow-origin", "*")
      |> response.set_header("access-control-allow-methods", "POST, OPTIONS")
      |> response.set_header("access-control-allow-headers", "Content-Type")
      |> response.set_body(mist.Bytes(bytes_builder.new()))
    }
    _ ->
      response.new(405)
      |> response.set_body(
        mist.Bytes(bytes_builder.from_string("Method Not Allowed")),
      )
  }
}

/// Handle health check endpoint
fn handle_health(_req: Request(mist.Connection)) -> Response(mist.ResponseData) {
  let json_response =
    "{\"status\":\"healthy\",\"service\":\"geql-web-app\",\"graphql\":\"available\",\"timestamp\":\"2024-demo\"}"

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_builder.from_string(json_response)))
}

/// Handle root endpoint
fn handle_root(_req: Request(mist.Connection)) -> Response(mist.ResponseData) {
  let html =
    "
<!DOCTYPE html>
<html>
<head>
  <title>GeQL Web App</title>
</head>
<body>
  <h1>🧚‍♀️ GeQL GraphQL Web Application</h1>
  <p>Welcome to the GeQL example web application!</p>
  <ul>
    <li><a href=\"/graphql\">GraphQL Endpoint</a> (POST queries here)</li>
    <li><a href=\"/health\">Health Check</a> (Service status)</li>
  </ul>
  <h2>Example Query</h2>
  <pre>
{
  user(id: \"1\") {
    id
    name
    email
  }
}
  </pre>
  <p><strong>Note:</strong> This demo server uses hardcoded queries. For full functionality, see the Phoenix benchmark.</p>
</body>
</html>
  "

  response.new(200)
  |> response.set_header("content-type", "text/html")
  |> response.set_body(mist.Bytes(bytes_builder.from_string(html)))
}
