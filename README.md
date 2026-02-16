# mochi - Code First GraphQL for Gleam

[![Package Version](https://img.shields.io/hexpm/v/mochi)](https://hex.pm/packages/mochi)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/mochi/)

**mochi** is a type-safe, Code First GraphQL library for Gleam. Define your GraphQL schemas using Gleam types and automatically generate TypeScript types and GraphQL SDL.

Inspired by:
- [Absinthe](https://github.com/absinthe-graphql/absinthe) - The GraphQL toolkit for Elixir
- [dataloader](https://github.com/graphql/dataloader) - DataLoader pattern for batching and caching
- [gqlkit](https://github.com/izumin5210/gqlkit) - Write TypeScript, generate GraphQL
- [Pothos](https://github.com/hayes/pothos) - Code First GraphQL for TypeScript
- [TypeGraphQL](https://github.com/MichalLytek/type-graphql) - Code First GraphQL with TypeScript decorators
- [Nexus](https://github.com/graphql-nexus/nexus) - Declarative, Code First GraphQL for TypeScript

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
  "user",
  [query.arg("id", schema.non_null(schema.id_type()))],
  schema.named_type("User"),
  decode_args,
  fn(args, ctx) { get_user_by_id(args.id) },
  types.to_dynamic,
)

// Build schema
let my_schema = query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
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

### Subscriptions (`mochi/subscription`)

Real-time updates with a PubSub pattern. See module docs for full API.

```gleam
import mochi/subscription

let pubsub = subscription.new_pubsub()
let result = subscription.subscribe(pubsub, "user:created", "onUserCreated", dict.new(), handler)
subscription.publish(result.pubsub, "user:created", user_data)
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

### Query Security (`mochi/security`)

Protect against malicious queries. See module docs for full API.

```gleam
import mochi/security

case security.validate(document, security.default_config()) {
  Ok(_) -> execute_query(document)
  Error(err) -> error_response(err)
}
```

### Persisted Queries (`mochi/persisted_queries`)

Automatic Persisted Queries (APQ) for caching and security. See module docs for full API.

```gleam
import mochi/persisted_queries as apq

let store = apq.new_store()
case apq.process_apq(store, query_opt, hash_opt) {
  apq.ExecuteQuery(query, new_store) -> execute(query)
  apq.PersistedQueryNotFound -> error_response("PersistedQueryNotFound")
  apq.HashMismatch(_, _) -> error_response("Hash mismatch")
}
```

### GraphQL Playgrounds (`mochi/playground`)

Built-in interactive GraphQL explorers.

```gleam
import mochi/playground

playground.graphiql("/graphql")      // GraphiQL IDE
playground.playground("/graphql")    // GraphQL Playground
playground.apollo_sandbox("/graphql") // Apollo Sandbox
playground.simple_explorer("/graphql") // Lightweight explorer
```

### WebSocket Transport (`mochi/transport/websocket`)

Real-time subscriptions over WebSocket using the graphql-ws protocol. See module docs for full API.

```gleam
import mochi/transport/websocket

let state = websocket.new_connection(schema, pubsub, execution_context)
let result = websocket.handle_message(state, client_msg)
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

### Codegen Configuration

Customize code generation output. See module docs for full API.

```gleam
import mochi/codegen/typescript
import mochi/codegen/sdl

let ts_code = typescript.generate(schema)
let graphql_code = sdl.generate(schema)
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
