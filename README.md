# mochi - Code First GraphQL for Gleam

> **Under development** — APIs may change, use main branch.


**mochi** is a type-safe, Code First GraphQL library for Gleam. Define your GraphQL schemas using Gleam types and automatically generate TypeScript types and GraphQL SDL.

Inspired by:
- [Absinthe](https://github.com/absinthe-graphql/absinthe) - The GraphQL toolkit for Elixir
- [dataloader](https://github.com/graphql/dataloader) - DataLoader pattern for batching and caching
- [gqlkit](https://github.com/izumin5210/gqlkit) - Write TypeScript, generate GraphQL
- [Pothos](https://github.com/hayes/pothos) - Code First GraphQL for TypeScript
- [TypeGraphQL](https://github.com/MichalLytek/type-graphql) - Code First GraphQL with TypeScript decorators
- [Nexus](https://github.com/graphql-nexus/nexus) - Declarative, Code First GraphQL for TypeScript

## Installation

Add to your `gleam.toml`:

```toml
mochi = { git = "https://github.com/qwexvf/mochi", ref = "main" }
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
    name: "user",
    args: [query.arg("id", schema.non_null(schema.id_type()))],
    returns: schema.named_type("User"),
    resolve: fn(args, _ctx) {
      use id <- result.try(query.get_id(args, "id"))
      get_user_by_id(id)
    },
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
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  let result = executor.execute(my_schema, ctx, "{ users { id name } }", dict.new(), option.None)
}
```

## Performance

Mochi is built for performance on the BEAM VM.

**Test system:** AMD Ryzen 7 PRO 7840U (8 cores) · 64 GB RAM · 4 wrk threads · 100 connections · 10s runs · Docker containers for JS competitors

The benchmark runs two rounds so the comparison is fair: once with **no document cache** on any framework, and once with **document cache enabled** on all frameworks (Apollo and Mercurius cache by default; Yoga gets `documentStore: new Map()`; mochi uses ETS).

### No document cache — parse + validate + execute every request

#### Simple query: `{ users { id name } }`

| Server | Runtime | Req/sec | Latency |
|--------|---------|---------|---------|
| **mochi** | Gleam / BEAM | **75,690** | **1.38ms** |
| bun + yoga | Bun | 14,365 | 6.95ms |
| yoga (node) | Node.js | 11,709 | 9.06ms |
| mercurius | Node.js + Fastify | 5,494 | 21.09ms |
| apollo | Node.js | 4,021 | 30.32ms |

#### Medium query: `{ users { id name email posts { id title } } }`

| Server | Runtime | Req/sec | Latency |
|--------|---------|---------|---------|
| **mochi** | Gleam / BEAM | **20,554** | **4.98ms** |
| bun + yoga | Bun | 8,263 | 12.08ms |
| yoga (node) | Node.js | 5,366 | 20.40ms |
| mercurius | Node.js + Fastify | 3,103 | 44.17ms |
| apollo | Node.js | 2,132 | 78.53ms |

### With document cache — skip parse + validate on repeated queries

#### Simple query: `{ users { id name } }`

| Server | Runtime | Req/sec | Latency |
|--------|---------|---------|---------|
| **mochi** | Gleam / BEAM | **76,106** | **1.39ms** |
| bun + yoga | Bun | 14,667 | 6.81ms |
| mercurius | Node.js + Fastify | 13,213 | 10.00ms |
| yoga (node) | Node.js | 11,408 | 9.87ms |
| apollo | Node.js | 8,069 | 14.61ms |

#### Medium query: `{ users { id name email posts { id title } } }`

| Server | Runtime | Req/sec | Latency |
|--------|---------|---------|---------|
| **mochi** | Gleam / BEAM | **19,355** | **5.39ms** |
| bun + yoga | Bun | 7,834 | 12.73ms |
| yoga (node) | Node.js | 5,415 | 22.15ms |
| mercurius | Node.js + Fastify | 5,345 | 28.73ms |
| apollo | Node.js | 3,044 | 60.34ms |

### Why mochi is fast — even without cache

The notable result is that mochi's numbers barely change between the two rounds (~75k vs ~76k req/s on simple queries). Parse and validation are a negligible fraction of mochi's request budget. The JS frameworks gain 2–3× from caching because their parse/validate overhead is large relative to execution.

**BEAM scheduler vs. single-threaded JS event loop.** Node.js serialises all requests through one event loop. The BEAM runs a scheduler per CPU core, each handling thousands of lightweight processes simultaneously. Under 100 concurrent connections mochi saturates all cores; Node cannot without clustering.

**No shared-heap GC pauses.** Every request on the BEAM lives in its own process heap. When a request finishes, that heap is reclaimed instantly — no stop-the-world pause. V8 has a single shared heap across all in-flight requests, so GC pauses add latency spikes for every request in flight.

**Flat execution path.** Gleam pattern matching on union types compiles to native BEAM tagged-tuple dispatch. There are no promise chains, middleware stacks, or resolver-wrapping layers. The code path from HTTP request to response is essentially a tight recursive descent.

**Executor optimised for the hot path.** Each field in a selection set previously built a single-entry `Dict`, accumulated them into a list, then merged the list with a second decode pass. That is now a single `Dict` accumulation — one pass, no merge. Error lists use cons + flatten instead of O(n) `list.append` per field. Fragment cycle detection uses a `Set` instead of a `List` scan.

### Running the benchmarks

```bash
# from the mochi-examples repo
cd mochi_wisp/benchmark
./run-host-bench.sh   # runs both rounds automatically
```

## Features

- **Code First Schema Definition** - Define GraphQL schemas using Gleam types with type-safe field extractors
- **Parse Caching** - ETS-backed document cache avoids re-parsing repeated queries (53–231× speedup on cached queries)
- **Batch Execution** - Execute multiple GraphQL requests in one call, with optional parallel dispatch
- **TypeScript Codegen** - Generate `.d.ts` type definitions from your schema
- **SDL Generation** - Generate `.graphql` schema files
- **SDL Type Extensions** - Split schemas across multiple files using `extend type`, `extend union`, `extend enum`, etc.
- **Operation Resolver Codegen** - Generate mochi resolver boilerplate from `.gql` client operation files
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
- **Guards** - Composable precondition checks for authorization, feature flags, and access control
- **Query Security** - Depth limiting, complexity analysis, alias limits
- **Persisted Queries** - Automatic Persisted Queries (APQ) with SHA256 hashing
- **GraphQL Playgrounds** - Built-in GraphiQL, Playground, Apollo Sandbox, and simple explorer
- **BEAM Powered** - Built for performance on Erlang/OTP
- **Zero Config** - Simple, intuitive API with sensible defaults

## TypeScript Codegen

Generate TypeScript type definitions from your schema:

```gleam
import mochi_codegen

let ts_code = mochi_codegen.to_typescript(schema)
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
import mochi_codegen

let graphql_schema = mochi_codegen.to_sdl(schema)
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

## SDL Type Extensions

Split large schemas across multiple files using GraphQL type extensions. The CLI merges all files and resolves extensions before codegen.

```graphql
# schema.graphql
type Mutation {
  login(email: String!, password: String!): String!
}

# tournament.graphql
extend type Mutation {
  finalizeTournament(tournamentId: ID!): Tournament!
}
```

All six extension kinds are supported: `extend type`, `extend interface`, `extend union`, `extend enum`, `extend input`, `extend scalar`.

**Rules:**
- Extension fields are merged into the matching base type. Duplicate field names are ignored.
- An extension with no matching base type is treated as a standalone type definition (orphan extension).
- Multiple extensions for the same type are applied in file order.

Configure multiple schema files in `mochi.config.json`:

```json
{
  "schema": ["schema.graphql", "tournament.graphql"]
}
```

Or use a glob pattern:

```json
{
  "schema": ["graphql/**/*.graphql"]
}
```

## API Reference

### Type Builders (`mochi/types`)

Build GraphQL object types with type-safe field extractors. See module docs for full API.

```gleam
import mochi/types

// Object type with field extractors
let user_type = types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.int("age", fn(u: User) { u.age })
  |> types.build(decode_user)

// Enum type
let role_enum = types.enum_type("Role")
  |> types.value("ADMIN")
  |> types.value("USER")
  |> types.build_enum

// Dynamic conversion helpers for DataLoader encoders
fn user_to_dynamic(u: User) -> Dynamic {
  types.record([
    types.field("id", u.id),
    types.field("name", u.name),
    #("age", types.option(u.age)),  // Option -> null if None
  ])
}
```

### Query Builders (`mochi/query`)

Define queries and mutations with type-safe resolvers. See module docs for full API.

```gleam
import mochi/query

// Query with arguments
let user_query = query.query_with_args(
  name: "user",
  args: [query.arg("id", schema.non_null(schema.id_type()))],
  returns: schema.named_type("User"),
  resolve: fn(args, ctx) {
    use id <- result.try(query.get_id(args, "id"))
    get_user_by_id(id)
  },
)

// Build schema
let my_schema = query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
```

### Argument Parsing Helpers

Convenient helpers for extracting arguments in resolvers:

```gleam
// Required arguments (return Result)
query.get_string(args, "name")   // -> Result(String, String)
query.get_id(args, "id")         // -> Result(String, String)
query.get_int(args, "age")       // -> Result(Int, String)
query.get_float(args, "price")   // -> Result(Float, String)
query.get_bool(args, "active")   // -> Result(Bool, String)

// Optional arguments (return Option)
query.get_optional_string(args, "filter")  // -> Option(String)
query.get_optional_int(args, "limit")      // -> Option(Int)

// List arguments
query.get_string_list(args, "tags")  // -> Result(List(String), String)
query.get_int_list(args, "ids")      // -> Result(List(Int), String)
```

### Schema Module (`mochi/schema`)

Low-level schema building and type definitions. See module docs for full API.

```gleam
import mochi/schema

// Field types
schema.string_type()      // String
schema.int_type()         // Int
schema.list_type(inner)   // [Type]
schema.non_null(inner)    // Type!
schema.named_type("User") // Custom type

// Interface and union types
let node = schema.interface("Node")
  |> schema.interface_field(schema.field_def("id", schema.non_null(schema.id_type())))

let search = schema.union("SearchResult")
  |> schema.union_member(user_type)
  |> schema.union_member(post_type)
```

### Custom Directives

Define custom directives for your schema. See module docs for full API.

```gleam
import mochi/schema

let auth = schema.directive("auth", [schema.FieldLocation])
  |> schema.directive_argument(schema.arg("role", schema.string_type()))
  |> schema.directive_handler(fn(args, value) { Ok(value) })
```

### Guards

Guards are lightweight precondition checks that run before a resolver. If a
guard returns `Ok(Nil)`, the resolver proceeds. If it returns `Error(message)`,
the resolver is skipped entirely. See [mochi/docs/guards.md](mochi/docs/guards.md) for the
full guide.

```gleam
// Define a reusable guard
fn require_auth(ctx: schema.ExecutionContext) -> Result(Nil, String) {
  case get_current_user(ctx) {
    Some(_) -> Ok(Nil)
    None -> Error("Authentication required")
  }
}

// High-level API: attach to queries, mutations, or fields
let my_posts = query.query_with_args(name: "myPosts", ...)
  |> query.with_guard(require_auth)

let create_post = query.mutation(name: "createPost", ...)
  |> query.mutation_with_guard(require_auth)

// Low-level API: attach directly to field definitions
schema.field_def("secret", schema.string_type())
  |> schema.resolver(my_resolver)
  |> schema.guard(require_auth_guard)

// Multiple guards (checked in list order)
  |> schema.guards([require_auth_guard, require_admin_guard])
```

### Subscriptions (`mochi_websocket`)

Real-time updates with a PubSub pattern over WebSocket. Requires the `mochi_websocket` package.

```gleam
import mochi_websocket

let pubsub = mochi_websocket.new_pubsub()
let state = mochi_websocket.new_connection(schema, pubsub, ctx)
mochi_websocket.publish(pubsub, mochi_websocket.topic("user:created"), user_data)
```

### Error Handling (`mochi/error`)

GraphQL-spec compliant errors with extensions. See module docs for full API.

```gleam
import mochi/error

let err = error.error("Something went wrong")
  |> error.with_code("INTERNAL_ERROR")
  |> error.with_extension("retryAfter", types.to_dynamic(60))
```

### Response Handling (`mochi/response`)

Construct and serialize GraphQL responses. See module docs for full API.

```gleam
import mochi/response

let resp = response.from_execution_result(exec_result)
let json_string = response.to_json(resp)
```

### JSON Serialization (`mochi/json`)

Built-in JSON encoding. See module docs for full API.

```gleam
import mochi/json

let json_string = json.encode(dynamic_value)
```

### Parse Caching (`mochi/document_cache`)

The document cache is enabled automatically when you build a schema with `query.build`. It stores parsed `ast.Document` values in an ETS table (Erlang) or `Map` (JavaScript) keyed by the query string. Repeated requests for the same query skip the parser entirely.

The cache is bounded to 1000 entries by default. Once full, new unique queries fall back to parsing without caching.

```gleam
import mochi/document_cache

// Automatic (via query.build — recommended)
let schema = query.new() |> ... |> query.build
// Cache is live; no extra config needed

// Manual size override via the low-level schema API
let cache = document_cache.new_with_max(500)
let schema = schema.schema()
  |> schema.with_document_cache(cache)
  |> ...
```

### Batch Execution (`mochi/batch`)

Execute multiple GraphQL requests in a single call. Useful for HTTP batch endpoints.

```gleam
import mochi/batch

let requests = [
  batch.request("{ users { id name } }"),
  batch.request_with_variables("{ user(id: $id) { name } }", vars),
  batch.request_with_operation("query GetMe { me { id } }", "GetMe"),
]

let config = batch.default_config()
  |> batch.with_max_batch_size(10)
  |> batch.with_parallel_execution(True)  // spawn one Erlang process per request

let result = batch.execute_batch(schema, requests, config, ctx)
// result.results  -> List(ExecutionResult) in original order
// result.all_succeeded -> Bool
// result.failure_count -> Int
```

### Query Security (`mochi/security`)

Protect against malicious queries. See module docs for full API.

```gleam
import mochi/security

case security.validate(document, security.default_config()) {
  Ok(_) -> execute_query(document)
  Error(err) -> error_response(err)
}
```

### Automatic Persisted Queries (`mochi/apq`)

APQ is built into the core — no extra package needed. Clients send a SHA256 hash instead of the full query string on repeat requests, reducing bandwidth on mobile or high-latency networks.

The protocol works in two passes:

1. **First request** — client sends `{ "extensions": { "persistedQuery": { "version": 1, "sha256Hash": "<hash>" } } }` with no `query` field. Server responds with `PersistedQueryNotFound`.
2. **Second request** — client resends with both `query` and `extensions`. Server verifies the hash, stores the query, and executes it.
3. **All subsequent requests** — client sends hash only. Server looks it up and executes directly.

Wire it into your HTTP handler by holding an `apq.Store` in your server state:

```gleam
import gleam/dict
import gleam/option.{None, Some}
import mochi/apq
import mochi/executor

// Server state — hold this across requests (e.g. in an Agent or ETS)
let store = apq.new()

// In your request handler:
fn handle_graphql(body: String, store: apq.Store) -> #(apq.Store, String) {
  let #(query_opt, extensions) = parse_request(body)

  case apq.parse_extension(extensions) {
    None -> {
      // Normal request — no APQ
      let result = executor.execute_query_with_context(schema, query_opt, ...)
      #(store, respond(result))
    }
    Some(ext) -> {
      case apq.process(store, query_opt, ext.sha256_hash) {
        Ok(#(store, query)) -> {
          let result = executor.execute_query_with_context(schema, query, ...)
          #(store, respond(result))
        }
        Error(apq.NotFound) ->
          #(store, persisted_query_not_found_response())
        Error(apq.HashMismatch(..)) ->
          #(store, bad_request_response("hash mismatch"))
      }
    }
  }
}
```

The `apq.Store` is an immutable dict — you get a new one back from `apq.process` whenever a query is registered. Store it in an Erlang `Agent` or ETS table to share it across the process pool.

### GraphQL Playgrounds (`mochi_codegen`)

Built-in interactive GraphQL explorers. Requires the `mochi_codegen` package.

```gleam
import mochi_codegen

mochi_codegen.graphiql("/graphql")       // GraphiQL IDE
mochi_codegen.apollo_sandbox("/graphql") // Apollo Sandbox
```

### WebSocket Transport (`mochi_websocket`)

Real-time subscriptions over WebSocket using the graphql-ws protocol. Requires the `mochi_websocket` package.

```gleam
import mochi_websocket

let state = mochi_websocket.new_connection(schema, pubsub, ctx)
let result = mochi_websocket.handle_message(state, client_msg)
```

### DataLoader (`mochi/dataloader`)

Prevent N+1 queries with automatic batching. See module docs for full API.

```gleam
import mochi/dataloader
import mochi/schema

// Create loader from find function (one-liner)
let pokemon_loader = dataloader.int_loader_result(
  data.find_pokemon, pokemon_to_dynamic, "Pokemon not found",
)

// Register loaders and load data
let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  |> schema.with_loaders([#("pokemon", pokemon_loader)])

let #(ctx, result) = schema.load_by_id(ctx, "pokemon", 25)
```

### Codegen (`mochi_codegen`)

Generate TypeScript types, GraphQL SDL, Gleam resolver stubs, and serve playground UIs. Requires the `mochi_codegen` package.

```gleam
import mochi_codegen

let ts_code = mochi_codegen.to_typescript(schema)
let graphql_code = mochi_codegen.to_sdl(schema)
```

**CLI** — generate everything from a config file:

```sh
gleam run -m mochi_codegen/cli -- init      # create mochi.config.yaml
gleam run -m mochi_codegen/cli -- generate  # generate from config
```

**Operation resolver generation** — point the CLI at your `.gql` client operation files and it emits complete mochi field-builder boilerplate (decode blocks, resolve stubs, encoder stubs, and a `register()` function). Only fill in the `resolve:` body.

```yaml
# mochi.config.yaml
schema: "graphql/schema.graphql"
operations_input: "src/graphql/**/*.gql"
output:
  gleam_types: "src/api/domain/"
  resolvers: "src/api/schema/"
  operations: "src/api/schema/"
  typescript: "apps/web/src/generated/types.ts"
```

## Examples

See the [`mochi-examples`](https://github.com/qwexvf/mochi-examples) repository for complete working examples:

- **`code_first_example.gleam`** - Basic User/Post schema with queries and mutations
- **`core_library_examples/`** - Pure GraphQL functionality demonstrations
- **`mochi_wisp/`** - Full web application with Wisp framework

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
    name: "createUser",
    args: [query.arg("input", schema.non_null(schema.named_type("CreateUserInput")))],
    returns: schema.named_type("User"),
    resolve: fn(args, ctx) {
      use input <- result.try(query.decode_input(args, "input", input_decoder))
      let user = User(id: generate_id(), name: input.name, email: input.email)
      db.insert_user(user)
      Ok(user)
    },
  )
  |> query.with_description("Create a new user")
}

let schema = query.new()
  |> query.add_mutation(create_user_mutation())
  |> query.build
```

## Package Structure

Install only the packages you need — each is a separate repository:

```toml
mochi = { git = "https://github.com/qwexvf/mochi", ref = "main" }                    # Core (required)
mochi_relay = { git = "https://github.com/qwexvf/mochi_relay", ref = "main" }        # Relay-style cursor pagination
mochi_websocket = { git = "https://github.com/qwexvf/mochi_websocket", ref = "main" } # WebSocket subscriptions (graphql-ws)
mochi_upload = { git = "https://github.com/qwexvf/mochi_upload", ref = "main" }      # Multipart file uploads
mochi_codegen = { git = "https://github.com/qwexvf/mochi_codegen", ref = "main" }    # SDL + TypeScript codegen + GraphiQL
# mochi/apq is built into the core — no separate package needed
```

```
mochi/                       # Core GraphQL engine
├── query.gleam              # Query/Mutation/Subscription builders
├── types.gleam              # Type builders (object, enum, fields)
├── schema.gleam             # Core schema types and low-level API
├── executor.gleam           # Query execution with null propagation
├── validation.gleam         # Query validation
├── document_cache.gleam     # ETS-backed parse cache (Erlang + JS)
├── batch.gleam              # Batch query execution with parallel dispatch
├── dataloader.gleam         # N+1 query prevention
├── error.gleam              # GraphQL-spec compliant errors
├── response.gleam           # Response serialization
└── security.gleam           # Depth/complexity/alias limits

mochi_relay/                 # Relay cursor pagination
mochi_websocket/             # graphql-ws WebSocket transport + PubSub
mochi_upload/                # GraphQL multipart file uploads
mochi_codegen/               # SDL + TypeScript + Gleam codegen + GraphiQL + CLI
mochi_apq/                   # Automatic Persisted Queries (standalone)
```

## Running Tests

```sh
gleam test
```

## Development

```sh
gleam build   # Build
gleam check   # Check for warnings
gleam format src test  # Format
```

## License

Apache 2.0
