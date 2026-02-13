# mochi - Code First GraphQL for Gleam

[![Package Version](https://img.shields.io/hexpm/v/mochi)](https://hex.pm/packages/mochi)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/mochi/)

**mochi** is a type-safe, Code First GraphQL library for Gleam. Define your GraphQL schemas using Gleam types and automatically generate TypeScript types and GraphQL SDL.

Inspired by [gqlkit](https://zenn.dev/izumin/articles/da27a6dfffba0b).

## Installation

```sh
gleam add mochi
```

## Quick Start

```gleam
import mochi/query
import mochi/schema
import mochi/types

// 1. Define your Gleam types
pub type User {
  User(id: String, name: String, email: String, age: Int)
}

// 2. Create GraphQL type with type-safe field extractors
fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.int("age", fn(u: User) { u.age })
  |> types.build(decode_user)
}

// 3. Define queries
fn users_query() {
  query.query(
    "users",
    schema.list_type(schema.named_type("User")),
    fn(_ctx) { Ok(get_users()) },
    fn(users) { types.to_dynamic(users) },
  )
}

fn user_query() {
  query.query_with_args(
    "user",
    [query.arg("id", schema.non_null(schema.id_type()))],
    schema.named_type("User"),
    decode_id_arg,
    fn(id, _ctx) { get_user_by_id(id) },
    types.to_dynamic,
  )
}

// 4. Build the schema
pub fn create_schema() -> schema.Schema {
  query.new()
  |> query.add_query(users_query())
  |> query.add_query(user_query())
  |> query.add_type(user_type())
  |> query.build
}

// 5. Execute queries
pub fn main() {
  let my_schema = create_schema()
  let result = mochi.execute(my_schema, "{ users { id name } }")
}
```

## Performance

Mochi is built for performance on the BEAM VM.

**Test System:** AMD Ryzen 7 PRO 7840U (8 cores, 16 threads) | 64GB RAM | Docker containers (no resource limits)

### Simple Query: `{ users { id name } }`

| Server | Runtime | Req/sec | Latency (avg) |
|--------|---------|---------|---------------|
| **Mercurius** | Node.js + Fastify | 36,485 | 2.28ms |
| **Mochi** | Gleam/Erlang | 27,770 | 3.13ms |
| **Bun + Yoga** | Bun | 19,067 | 4.68ms |
| **GraphQL Yoga** | Node.js | 13,798 | 6.78ms |
| **Apollo Server** | Node.js | 6,659 | 14.53ms |
| **graphql-js** | Node.js (reference) | 4,604 | 21.21ms |

### Medium Query: `{ users { id name email posts { id title } } }`

| Server | Runtime | Req/sec | Latency (avg) |
|--------|---------|---------|---------------|
| **Mercurius** | Node.js + Fastify | 32,696 | 2.50ms |
| **Mochi** | Gleam/Erlang | 25,850 | 3.39ms |
| **Bun + Yoga** | Bun | 14,854 | 6.25ms |
| **GraphQL Yoga** | Node.js | 11,705 | 8.09ms |
| **Apollo Server** | Node.js | 5,286 | 18.43ms |
| **graphql-js** | Node.js (reference) | 3,463 | 28.37ms |

Mochi achieves **4x faster** performance than Apollo Server and **6x faster** than the reference graphql-js implementation, while providing:

- **Type safety** - Full compile-time guarantees from Gleam
- **Fault tolerance** - BEAM VM supervision and process isolation
- **Scalability** - Leverages Erlang's lightweight process model
- **Zero N+1** - Built-in DataLoader support

Run benchmarks yourself:
```bash
cd examples/mochi_wisp/benchmark
docker compose up -d --build
./run-host-bench.sh
```

## Features

- **Code First Schema Definition** - Define GraphQL schemas using Gleam types with type-safe field extractors
- **TypeScript Codegen** - Generate `.d.ts` type definitions from your schema
- **SDL Generation** - Generate `.graphql` schema files
- **Query Validation** - Validate GraphQL queries against your schema
- **Custom Directives** - Define and execute custom directives with handlers
- **@deprecated Support** - Mark fields and enum values as deprecated
- **Interface Types** - Define GraphQL interfaces with type resolution
- **Union Types** - Define union types with runtime type resolution
- **Fragment Support** - Full support for GraphQL fragments
- **Subscriptions** - Real-time updates with PubSub pattern
- **Error Extensions** - GraphQL-spec compliant errors with locations, path, and extensions
- **JSON Serialization** - Built-in JSON encoding for responses
- **Null Propagation** - Proper null bubbling per GraphQL specification
- **DataLoader** - N+1 query prevention with automatic batching and caching
- **Query Security** - Depth limiting, complexity analysis, alias limits
- **Persisted Queries** - Automatic Persisted Queries (APQ) with SHA256 hashing
- **GraphQL Playgrounds** - Built-in GraphiQL, Playground, Apollo Sandbox, and simple explorer
- **Dual Target** - Works on BEAM (Erlang) and JavaScript runtimes
- **Zero Config** - Simple, intuitive API with sensible defaults

## TypeScript Codegen

Generate TypeScript type definitions from your schema:

```gleam
import mochi/codegen/typescript

let ts_code = typescript.generate(schema)
// Write to: types.generated.ts
```

**Output:**

```typescript
// Generated by mochi

export type Maybe<T> = T | null | undefined;

export type Scalars = {
  ID: string;
  String: string;
  Int: number;
  Float: number;
  Boolean: boolean;
};

export interface User {
  id: Scalars["ID"];
  name?: Maybe<Scalars["String"]>;
  email?: Maybe<Scalars["String"]>;
  age?: Maybe<Scalars["Int"]>;
}

export interface QueryUserArgs {
  id: Scalars["ID"];
}

export interface Query {
  user(args: QueryUserArgs): Maybe<User>;
  users: Maybe<Maybe<User>[]>;
}
```

## SDL Generation

Generate GraphQL SDL from your schema:

```gleam
import mochi/codegen/sdl

let graphql_schema = sdl.generate(schema)
// Write to: schema.graphql
```

**Output:**

```graphql
# Generated by mochi

"A user in the system"
type User {
  id: ID!
  name: String
  email: String
  age: Int
}

type Query {
  "Get a user by ID"
  user(id: ID!): User
  "Get all users"
  users: [User]!
}
```

## API Reference

### Type Builders (`mochi/types`)

Build GraphQL object types with type-safe field extractors:

```gleam
import mochi/types
import mochi/schema

// Create object type
let user_type = types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string_with_desc("email", "User's email address", fn(u: User) { u.email })
  |> types.int("age", fn(u: User) { u.age })
  |> types.float("score", fn(u: User) { u.score })
  |> types.bool("active", fn(u: User) { u.active })
  |> types.list_string("tags", fn(u: User) { u.tags })
  |> types.list_int("scores", fn(u: User) { u.scores })
  |> types.optional_string("nickname", fn(u: User) { u.nickname })
  |> types.object_field("profile", "Profile", fn(u: User) { types.to_dynamic(u.profile) })
  |> types.list_object("posts", "Post", fn(u: User) { types.to_dynamic(u.posts) })
  |> types.build(decode_user)

// Create enum type
let role_enum = types.enum_type("Role")
  |> types.enum_description("User roles in the system")
  |> types.value("ADMIN")
  |> types.value_with_desc("USER", "Standard user access")
  |> types.deprecated_value("GUEST")
  |> types.deprecated_value_with_reason("LEGACY", "Use MEMBER instead")
  |> types.build_enum
```

#### Dynamic Conversion Helpers

When building DataLoader encoders or custom resolvers, use these helpers for cleaner code:

```gleam
import mochi/types

// Convert any value to Dynamic
let dyn = types.to_dynamic(user)

// Build a record (dict) from field tuples - great for DataLoader encoders
fn user_to_dynamic(u: User) -> Dynamic {
  types.record([
    types.field("id", u.id),
    types.field("name", u.name),
    types.field("email", u.email),
    types.field("age", u.age),
    #("profile", profile_to_dynamic(u.profile)),  // Nested object
  ])
}

// Handle Option types (None becomes null)
fn post_to_dynamic(p: Post) -> Dynamic {
  types.record([
    types.field("id", p.id),
    types.field("title", p.title),
    #("published_at", types.option(p.published_at)),  // Option(Time)
  ])
}
```

| Function | Description |
|----------|-------------|
| `to_dynamic(value)` | Convert any Gleam value to Dynamic |
| `record(fields)` | Build Dynamic dict from `List(#(String, Dynamic))` |
| `field(name, value)` | Shorthand for `#(name, to_dynamic(value))` |
| `option(opt)` | Convert `Option(a)` to Dynamic (None → null) |

### Query Builders (`mochi/query`)

Define queries and mutations with type-safe resolvers:

```gleam
import mochi/query
import mochi/schema

// Simple query (no arguments)
let users_query = query.query(
  "users",
  schema.list_type(schema.named_type("User")),
  fn(ctx) { Ok(get_users()) },
  types.to_dynamic,
)
|> query.query_description("Get all users")

// Query with arguments
let user_query = query.query_with_args(
  "user",
  [
    query.arg("id", schema.non_null(schema.id_type())),
    query.arg_with_desc("includeProfile", schema.boolean_type(), "Include user profile"),
  ],
  schema.named_type("User"),
  decode_args,
  fn(args, ctx) { get_user_by_id(args.id) },
  types.to_dynamic,
)

// Mutation with arguments
let create_user = query.mutation(
  "createUser",
  [query.arg("input", schema.non_null(schema.named_type("CreateUserInput")))],
  schema.named_type("User"),
  decode_input,
  fn(input, ctx) { create_user(input) },
  types.to_dynamic,
)
|> query.mutation_description("Create a new user")

// Build schema
let my_schema = query.new()
  |> query.add_query(users_query)
  |> query.add_query(user_query)
  |> query.add_mutation(create_user)
  |> query.add_type(user_type)
  |> query.add_enum(role_enum)
  |> query.build
```

### Schema Module (`mochi/schema`)

Low-level schema building and type definitions:

```gleam
import mochi/schema

// Field types
schema.string_type()      // String
schema.int_type()         // Int
schema.float_type()       // Float
schema.boolean_type()     // Boolean
schema.id_type()          // ID
schema.named_type("User") // Custom type reference
schema.list_type(inner)   // [Type]
schema.non_null(inner)    // Type!

// Field definitions with deprecation
let field = schema.field_def("oldField", schema.string_type())
  |> schema.field_description("This field is deprecated")
  |> schema.deprecate("Use newField instead")

// Interface types
let node_interface = schema.interface("Node")
  |> schema.interface_description("An object with an ID")
  |> schema.interface_field(schema.field_def("id", schema.non_null(schema.id_type())))
  |> schema.interface_resolve_type(fn(value) {
    // Return the concrete type name
    Ok("User")
  })

// Union types
let search_result = schema.union("SearchResult")
  |> schema.union_description("Search result types")
  |> schema.union_member(user_type)
  |> schema.union_member(post_type)
  |> schema.union_resolve_type(fn(value) {
    // Return the concrete type name
    Ok("User")
  })

// Add to schema
let my_schema = query.new()
  |> query.add_interface(node_interface)
  |> query.add_union(search_result)
  |> query.build
```

### Custom Directives

Define custom directives for your schema:

```gleam
import mochi/schema

// Define a custom directive
let auth_directive = schema.directive("auth", [schema.FieldLocation, schema.ObjectLocation])
  |> schema.directive_description("Requires authentication")
  |> schema.directive_argument(
    schema.arg("role", schema.string_type())
    |> schema.arg_description("Required role"),
  )

// Repeatable directive
let log_directive = schema.directive("log", [schema.FieldLocation])
  |> schema.directive_repeatable

// Directive with handler
let uppercase_directive = schema.directive("uppercase", [schema.FieldLocation])
  |> schema.directive_handler(fn(args, value) {
    // Transform the field value
    Ok(transform_value(value))
  })

// Add to schema
let my_schema = schema.schema()
  |> schema.add_directive(auth_directive)
  |> schema.add_directive(log_directive)
```

### Subscriptions (`mochi/subscription`)

Real-time updates with a PubSub pattern:

```gleam
import mochi/subscription
import mochi/subscription_executor
import mochi/query

// Define a subscription
let on_user_created = query.subscription(
  "onUserCreated",
  schema.named_type("User"),
  "user:created",  // Topic to subscribe to
  fn(user) { types.to_dynamic(user) },
)

// With arguments for filtered subscriptions
let on_message = query.subscription_with_args(
  "onMessageSent",
  [query.arg("roomId", schema.non_null(schema.id_type()))],
  schema.named_type("Message"),
  fn(args) { Ok(args.room_id) },
  fn(room_id, _ctx) { Ok("messages:" <> room_id) },
  fn(msg) { types.to_dynamic(msg) },
)

// Build schema with subscriptions
let my_schema = query.new()
  |> query.add_subscription(on_user_created)
  |> query.add_subscription(on_message)
  |> query.build

// Create PubSub and subscribe
let pubsub = subscription.new_pubsub()
let result = subscription.subscribe(
  pubsub,
  "user:created",
  "onUserCreated",
  dict.new(),
  fn(event) { send_to_client(event) },
)

// Publish events to subscribers
subscription.publish(result.pubsub, "user:created", user_data)

// Topic helpers
subscription.topic("user:created")           // "user:created"
subscription.topic_with_id("user", "123")    // "user:123"
subscription.topic_from_parts(["chat", "room", "456"])  // "chat:room:456"
```

### Error Handling (`mochi/error`)

GraphQL-spec compliant errors with extensions:

```gleam
import mochi/error

// Basic error
let err = error.error("Something went wrong")

// Error with path (shows where in the query the error occurred)
let err = error.error_at("Field not found", ["user", "email"])

// Error with source location
let err = error.error("Syntax error")
  |> error.at_location(10, 5)

// Error with extensions (custom metadata)
let err = error.error("Rate limited")
  |> error.with_code("RATE_LIMITED")
  |> error.with_extension("retryAfter", types.to_dynamic(60))

// Convenience constructors
let err = error.validation_error("Field not found", ["user", "email"])
let err = error.resolver_error("Database error", ["query", "users"])
let err = error.authentication_error("Not authenticated")
let err = error.authorization_error("Access denied", ["user", "role"])
let err = error.user_input_error("Invalid email", "email", ["input"])

// Format for logging
error.format(err)  // "Field not found at user.email"

// Serialize to Dynamic for JSON output
error.to_dynamic(err)
```

### Response Handling (`mochi/response`)

Construct and serialize GraphQL responses:

```gleam
import mochi/response
import mochi/error

// Success response
let resp = response.success(data)

// Error response
let resp = response.failure([error.error("Something failed")])

// Partial response (data with errors)
let resp = response.partial(data, errors)

// From execution result
let resp = response.from_execution_result(exec_result)

// Add extensions
let resp = response.success(data)
  |> response.with_extension("requestId", types.to_dynamic("req-123"))
  |> response.with_tracing(start_time, end_time)

// Serialize to JSON
let json_string = response.to_json(resp)
let pretty_json = response.to_json_pretty(resp)

// Inspect response
response.has_data(resp)      // True
response.has_errors(resp)    // False
response.is_success(resp)    // True
response.is_partial(resp)    // False
response.error_count(resp)   // 0
```

### JSON Serialization (`mochi/json`)

Built-in JSON encoding:

```gleam
import mochi/json

// Encode Dynamic values to JSON strings
let json_string = json.encode(dynamic_value)

// Pretty-print with indentation
let pretty_json = json.encode_pretty(dynamic_value, 2)

// Supports all JSON types:
// - Strings (with proper escaping)
// - Numbers (Int and Float)
// - Booleans
// - Null
// - Arrays
// - Objects
```

### Query Security (`mochi/security`)

Protect against malicious queries with depth limiting, complexity analysis, and more:

```gleam
import mochi/security

// Analyze a query
let analysis = security.analyze(document)
// Returns: SecurityAnalysis(depth: 4, complexity: 12, alias_count: 2, root_field_count: 3)

// Validate against limits
let config = security.SecurityConfig(
  max_depth: Some(10),           // Maximum query depth
  max_complexity: Some(100),     // Maximum field count
  max_aliases: Some(20),         // Maximum alias count
  max_root_fields: Some(10),     // Maximum root fields
)

case security.validate(document, config) {
  Ok(_) -> execute_query(document)
  Error(security.DepthLimitExceeded(actual, max)) ->
    error_response("Query depth " <> int.to_string(actual) <> " exceeds max " <> int.to_string(max))
  Error(security.ComplexityLimitExceeded(actual, max)) ->
    error_response("Query too complex")
  // ... other error types
}

// Convenience configs
let strict = security.strict_config()    // Low limits for public APIs
let default = security.default_config()  // Reasonable defaults (depth: 10, complexity: 100)
let none = security.no_limits()          // No limits (use with caution)
```

### Persisted Queries (`mochi/persisted_queries`)

Automatic Persisted Queries (APQ) for caching and security:

```gleam
import mochi/persisted_queries as apq

// Create a persisted query store
let store = apq.new_store()

// Hash a query (SHA256)
let hash = apq.hash_query("{ users { id name } }")
// "abc123..." (64-char hex string)

// Process incoming APQ request
// Client sends: { "extensions": { "persistedQuery": { "sha256Hash": "abc123" } } }
case apq.process_apq(store, query_opt, hash_opt) {
  apq.ExecuteQuery(query, new_store) ->
    // Execute the query and return result
    execute(query)

  apq.PersistedQueryNotFound ->
    // Client needs to send full query with hash
    error_response("PersistedQueryNotFound")

  apq.HashMismatch(expected, got) ->
    // Query hash doesn't match provided hash
    error_response("Hash mismatch")
}

// Manual store management
let store = apq.store_query(store, hash, query)
let query_opt = apq.get_query(store, hash)
let store = apq.remove_query(store, hash)
let store = apq.clear_store(store)
```

### GraphQL Playgrounds (`mochi/playground`)

Built-in interactive GraphQL explorers:

```gleam
import mochi/playground

// GraphiQL - The classic GraphQL IDE
let html = playground.graphiql("/graphql")

// GraphQL Playground - Popular alternative (legacy)
let html = playground.playground("/graphql")

// Apollo Sandbox - Modern Apollo explorer
let html = playground.apollo_sandbox("/graphql")

// Simple Explorer - Lightweight, no external dependencies
let html = playground.simple_explorer("/graphql")

// Use with your web framework
fn handle_request(req) {
  case path {
    "/graphiql" -> html_response(playground.graphiql("/graphql"))
    "/playground" -> html_response(playground.playground("/graphql"))
    "/sandbox" -> html_response(playground.apollo_sandbox("/graphql"))
    "/explorer" -> html_response(playground.simple_explorer("/graphql"))
    _ -> not_found()
  }
}
```

### WebSocket Transport (`mochi/transport/websocket`)

Real-time subscriptions over WebSocket using the graphql-ws protocol:

```gleam
import mochi/transport/websocket

// Create connection state
let state = websocket.new_connection(schema, pubsub, execution_context)

// Handle incoming client messages
case websocket.decode_client_message(json_text) {
  Ok(client_msg) -> {
    let result = websocket.handle_message(state, client_msg)
    case result {
      websocket.HandleOk(new_state, Some(response)) -> {
        let json = websocket.encode_server_message(response)
        send_to_client(json)
        new_state
      }
      websocket.HandleClose(reason) -> close_connection(reason)
      // ...
    }
  }
  Error(err) -> log_error(websocket.format_decode_error(err))
}

// Message types supported:
// Client -> Server: ConnectionInit, Subscribe, Complete, Ping, Pong
// Server -> Client: ConnectionAck, Next, Error, Complete, Ping, Pong

// Helper functions
websocket.send_next(id, result)      // Send subscription data
websocket.send_error(id, errors)     // Send subscription errors
websocket.send_complete(id)          // Complete a subscription
websocket.cleanup(state)             // Clean up on disconnect
```

### DataLoader (`mochi/dataloader`)

Prevent N+1 queries with automatic batching:

```gleam
import mochi/dataloader
import mochi/schema
import mochi/types

// === Quick Start (Recommended) ===

// Create loader from an existing find function - one line!
let pokemon_loader = dataloader.int_loader_result(
  data.find_pokemon,      // fn(Int) -> Result(Pokemon, _)
  pokemon_to_dynamic,     // fn(Pokemon) -> Dynamic
  "Pokemon not found",    // Error message
)

// Same for String keys
let user_loader = dataloader.string_loader_result(
  data.find_user_by_email,
  user_to_dynamic,
  "User not found",
)

// Register multiple loaders at once
let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  |> schema.with_loaders([
    #("pokemon", pokemon_loader),
    #("user", user_loader),
    #("trainer", trainer_loader),
  ])

// Load by ID (convenience helper)
let #(ctx, result) = schema.load_by_id(ctx, "pokemon", 25)
let #(ctx, results) = schema.load_many_by_id(ctx, "pokemon", [1, 4, 7])

// === Custom Loader (Advanced) ===

// Create loader with custom logic
let user_loader = dataloader.int_loader(fn(id) {
  case db.find_user(id) {
    Ok(user) -> Ok(types.to_dynamic(user))
    Error(_) -> Error("User not found")
  }
})

// Batch loader for efficient bulk fetching
let user_loader = dataloader.int_batch_loader(fn(ids) {
  case db.get_users_by_ids(ids) {
    Ok(users) -> Ok(list.map(users, types.to_dynamic))
    Error(e) -> Error(e)
  }
})

// === Full Control (Low-Level) ===

// Create a batch loading function
fn batch_load_users(ids: List(String)) -> Result(List(Result(User, String)), String) {
  let users = db.get_users_by_ids(ids)
  Ok(list.map(ids, fn(id) {
    case list.find(users, fn(u) { u.id == id }) {
      Ok(user) -> Ok(user)
      Error(_) -> Error("User not found")
    }
  }))
}

let user_loader = dataloader.new(batch_load_users)

// With custom options
let user_loader = dataloader.new_with_options(
  batch_load_users,
  dataloader.DataLoaderOptions(max_batch_size: 50, cache_enabled: True),
)

// Cache management
let loader = dataloader.prime(loader, "user-4", user)  // Pre-populate cache
let loader = dataloader.clear_key(loader, "user-1")    // Clear specific key
let loader = dataloader.clear_cache(loader)            // Clear all cache
```

#### DataLoader Helper Summary

| Function | Description |
|----------|-------------|
| `int_loader_result(find, encode, err)` | One-liner from Result-returning find function |
| `string_loader_result(find, encode, err)` | Same for String keys |
| `int_loader(fn)` | Custom loader with Int keys |
| `string_loader(fn)` | Custom loader with String keys |
| `int_batch_loader(fn)` | Batch loader with Int keys |
| `string_batch_loader(fn)` | Batch loader with String keys |
| `schema.with_loaders(ctx, loaders)` | Register multiple loaders |
| `schema.load_by_id(ctx, name, id)` | Load by Int ID |
| `schema.load_many_by_id(ctx, name, ids)` | Load multiple by Int IDs |

### Codegen Configuration

Customize code generation output:

```gleam
import mochi/codegen/typescript
import mochi/codegen/sdl
import gleam/option.{Some}

// TypeScript with custom config
let ts_config = typescript.Config(
  use_exports: True,
  use_interfaces: True,
  readonly_properties: True,
  include_helpers: True,
  header: Some("// Custom header\n// Generated types"),
)
let ts_code = typescript.generate_with_config(schema, ts_config)

// SDL with custom config
let sdl_config = sdl.Config(
  include_descriptions: True,
  include_builtin_scalars: False,
  indent: "  ",
  header: Some("# My GraphQL Schema"),
)
let graphql_code = sdl.generate_with_config(schema, sdl_config)
```

## Examples

See the [`examples/`](examples/) directory for complete working examples:

- **`code_first_example.gleam`** - Basic User/Post schema with queries and mutations
- **`core_library_examples/`** - Pure GraphQL functionality demonstrations
- **`geql_web_app/`** - Full web application with database integration

### Basic Schema Example

```gleam
import mochi/query
import mochi/schema
import mochi/types

pub type User {
  User(id: String, name: String, email: String)
}

pub type Post {
  Post(id: String, title: String, author_id: String)
}

fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.build(fn(_) { Ok(User("", "", "")) })
}

fn post_type() -> schema.ObjectType {
  types.object("Post")
  |> types.id("id", fn(p: Post) { p.id })
  |> types.string("title", fn(p: Post) { p.title })
  |> types.string("authorId", fn(p: Post) { p.author_id })
  |> types.build(fn(_) { Ok(Post("", "", "")) })
}

pub fn create_schema() -> schema.Schema {
  query.new()
  |> query.add_query(query.query(
    "users",
    schema.list_type(schema.named_type("User")),
    fn(_) { Ok([]) },
    types.to_dynamic,
  ))
  |> query.add_type(user_type())
  |> query.add_type(post_type())
  |> query.build
}
```

### Mutation Example

```gleam
pub type CreateUserInput {
  CreateUserInput(name: String, email: String)
}

fn create_user_mutation() {
  query.mutation(
    "createUser",
    [query.arg("input", schema.non_null(schema.named_type("CreateUserInput")))],
    schema.named_type("User"),
    decode_create_input,
    fn(input, ctx) {
      // Insert into database
      let user = User(id: generate_id(), name: input.name, email: input.email)
      db.insert_user(user)
      Ok(user)
    },
    types.to_dynamic,
  )
  |> query.mutation_description("Create a new user")
}

let schema = query.new()
  |> query.add_mutation(create_user_mutation())
  |> query.build
```

## Module Structure

```
mochi/
|-- query.gleam              # Query/Mutation/Subscription builders (Code First API)
|-- types.gleam              # Type builders (object, enum, fields)
|-- schema.gleam             # Core schema types and low-level API
|-- parser.gleam             # GraphQL query parser
|-- executor.gleam           # Query execution engine with null propagation
|-- validation.gleam         # Query validation
|-- subscription.gleam       # Subscription PubSub system
|-- subscription_executor.gleam  # Subscription execution
|-- error.gleam              # GraphQL-spec compliant errors
|-- response.gleam           # Response construction and serialization
|-- json.gleam               # JSON encoding
|-- dataloader.gleam         # N+1 query prevention
|-- security.gleam           # Query security (depth, complexity limits)
|-- persisted_queries.gleam  # Automatic Persisted Queries (APQ)
|-- playground.gleam         # GraphQL IDE HTML generators
|-- codegen/
|   |-- typescript.gleam     # TypeScript .d.ts generation
|   +-- sdl.gleam            # GraphQL SDL generation
|-- transport/
|   +-- websocket.gleam      # WebSocket transport (graphql-ws protocol)
+-- ...
```

## Running Tests

```sh
gleam test
```

## Development

```sh
gleam build          # Build the library
gleam test           # Run all tests
gleam run            # Run demo
```

## License

Apache 2.0
