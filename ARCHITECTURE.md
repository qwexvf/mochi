# Mochi Architecture

This document describes how Mochi processes GraphQL requests from receiving a query to returning a response.

## High-Level Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CLIENT REQUEST                                  │
│                                                                             │
│   POST /graphql                                                             │
│   { "query": "{ user(id: 1) { name email } }", "variables": {} }           │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            1. HTTP HANDLER                                   │
│                           (Your Web Framework)                               │
│                                                                             │
│   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐                    │
│   │    Wisp     │    │    Mist     │    │   Custom    │                    │
│   └─────────────┘    └─────────────┘    └─────────────┘                    │
│                              │                                              │
│                              ▼                                              │
│                    Parse JSON request body                                  │
│                    Extract query + variables                                │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                             2. PARSER                                        │
│                          mochi/parser.gleam                                  │
│                                                                             │
│   Input: "{ user(id: 1) { name email } }"                                  │
│                                                                             │
│   ┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐         │
│   │  Lexer   │────▶│  Tokens  │────▶│  Parser  │────▶│   AST    │         │
│   └──────────┘     └──────────┘     └──────────┘     └──────────┘         │
│                                                                             │
│   Output: Document(definitions: [OperationDefinition(...)])                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           3. VALIDATION                                      │
│                        mochi/validation.gleam                                │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Checks:                                                     │          │
│   │  • Field exists on type                                      │          │
│   │  • Arguments are valid                                       │          │
│   │  • Types match                                               │          │
│   │  • Fragments are valid                                       │          │
│   │  • Variables are defined                                     │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                                                                             │
│   ┌─────────────────┐              ┌─────────────────┐                     │
│   │  Valid Query    │              │  Invalid Query  │                     │
│   │  Continue ──────┼──────────────┼▶ Return Errors  │                     │
│   └─────────────────┘              └─────────────────┘                     │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      4. SECURITY CHECKS (Optional)                           │
│                         mochi/security.gleam                                 │
│                                                                             │
│   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐       │
│   │ Max Depth   │  │ Complexity  │  │ Max Aliases │  │ Root Fields │       │
│   │   Check     │  │   Check     │  │   Check     │  │   Check     │       │
│   └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘       │
│                                                                             │
│   Reject malicious queries before execution                                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            5. EXECUTION                                      │
│                         mochi/executor.gleam                                 │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │                    ExecutionContext                          │          │
│   │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐        │          │
│   │  │ user_context │ │ data_loaders │ │  middleware  │        │          │
│   │  └──────────────┘ └──────────────┘ └──────────────┘        │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                                      │                                      │
│                                      ▼                                      │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │                   Field Resolution                           │          │
│   │                                                              │          │
│   │   Query                                                      │          │
│   │     │                                                        │          │
│   │     ├── user(id: 1)  ◀── Resolver fetches User              │          │
│   │     │     │                                                  │          │
│   │     │     ├── name   ◀── Field extractor                    │          │
│   │     │     │                                                  │          │
│   │     │     └── email  ◀── Field extractor                    │          │
│   │                                                              │          │
│   └─────────────────────────────────────────────────────────────┘          │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                       6. RESPONSE CONSTRUCTION                               │
│                         mochi/response.gleam                                 │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  {                                                           │          │
│   │    "data": {                                                 │          │
│   │      "user": {                                               │          │
│   │        "name": "Alice",                                      │          │
│   │        "email": "alice@example.com"                          │          │
│   │      }                                                       │          │
│   │    },                                                        │          │
│   │    "errors": null,                                           │          │
│   │    "extensions": { ... }                                     │          │
│   │  }                                                           │          │
│   └─────────────────────────────────────────────────────────────┘          │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CLIENT RESPONSE                                 │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Detailed Component Diagrams

### Parser Pipeline

```
                          GraphQL Query String
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────┐
│                         LEXER                                │
│                                                              │
│  "{ user(id: 1) { name } }"                                 │
│       │                                                      │
│       ▼                                                      │
│  ┌────────────────────────────────────────────────────────┐ │
│  │ LBRACE, NAME("user"), LPAREN, NAME("id"), COLON,       │ │
│  │ INT(1), RPAREN, LBRACE, NAME("name"), RBRACE, RBRACE   │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────┐
│                         PARSER                               │
│                                                              │
│  Recursive descent parser builds AST                         │
│                                                              │
│  Document                                                    │
│    └── OperationDefinition(Query)                           │
│          └── SelectionSet                                    │
│                └── Field("user")                             │
│                      ├── Arguments: [("id", IntValue(1))]   │
│                      └── SelectionSet                        │
│                            └── Field("name")                 │
└─────────────────────────────────────────────────────────────┘
```

### Executor Field Resolution

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FIELD RESOLUTION                                   │
│                                                                             │
│                           ┌─────────────┐                                   │
│                           │    Query    │ (Root Type)                       │
│                           └──────┬──────┘                                   │
│                                  │                                          │
│              ┌───────────────────┼───────────────────┐                     │
│              │                   │                   │                     │
│              ▼                   ▼                   ▼                     │
│       ┌──────────┐        ┌──────────┐        ┌──────────┐                 │
│       │  users   │        │   user   │        │  posts   │                 │
│       └────┬─────┘        └────┬─────┘        └────┬─────┘                 │
│            │                   │                   │                       │
│            ▼                   ▼                   ▼                       │
│     ┌────────────┐      ┌────────────┐      ┌────────────┐                 │
│     │  Resolver  │      │  Resolver  │      │  Resolver  │                 │
│     │ List<User> │      │   User     │      │ List<Post> │                 │
│     └────────────┘      └─────┬──────┘      └────────────┘                 │
│                               │                                            │
│                    ┌──────────┼──────────┐                                 │
│                    │          │          │                                 │
│                    ▼          ▼          ▼                                 │
│              ┌────────┐ ┌────────┐ ┌────────┐                              │
│              │  name  │ │ email  │ │  age   │                              │
│              └───┬────┘ └───┬────┘ └───┬────┘                              │
│                  │          │          │                                   │
│                  ▼          ▼          ▼                                   │
│              Extractor  Extractor  Extractor                               │
│              fn(u) {    fn(u) {    fn(u) {                                 │
│                u.name     u.email    u.age                                 │
│              }          }          }                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

### DataLoader Batching

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           WITHOUT DATALOADER                                 │
│                                                                             │
│   Query: { posts { author { name } } }                                      │
│                                                                             │
│   ┌──────┐     ┌──────┐     ┌──────┐     ┌──────┐     ┌──────┐            │
│   │Post 1│     │Post 2│     │Post 3│     │Post 4│     │Post 5│            │
│   └──┬───┘     └──┬───┘     └──┬───┘     └──┬───┘     └──┬───┘            │
│      │            │            │            │            │                 │
│      ▼            ▼            ▼            ▼            ▼                 │
│   DB Query     DB Query     DB Query     DB Query     DB Query             │
│   user #1      user #2      user #1      user #3      user #2              │
│                                                                             │
│   Total: 5 database queries (N+1 problem!)                                  │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                            WITH DATALOADER                                   │
│                                                                             │
│   Query: { posts { author { name } } }                                      │
│                                                                             │
│   ┌──────┐     ┌──────┐     ┌──────┐     ┌──────┐     ┌──────┐            │
│   │Post 1│     │Post 2│     │Post 3│     │Post 4│     │Post 5│            │
│   └──┬───┘     └──┬───┘     └──┬───┘     └──┬───┘     └──┬───┘            │
│      │            │            │            │            │                 │
│      └────────────┴─────┬──────┴────────────┴────────────┘                 │
│                         │                                                   │
│                         ▼                                                   │
│              ┌─────────────────────┐                                        │
│              │     DataLoader      │                                        │
│              │                     │                                        │
│              │  Collect keys:      │                                        │
│              │  [1, 2, 1, 3, 2]    │                                        │
│              │                     │                                        │
│              │  Dedupe:            │                                        │
│              │  [1, 2, 3]          │                                        │
│              └──────────┬──────────┘                                        │
│                         │                                                   │
│                         ▼                                                   │
│              ┌─────────────────────┐                                        │
│              │   Single DB Query   │                                        │
│              │   WHERE id IN       │                                        │
│              │   (1, 2, 3)         │                                        │
│              └─────────────────────┘                                        │
│                                                                             │
│   Total: 1 database query!                                                  │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Schema Definition Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CODE FIRST SCHEMA                                    │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Gleam Types (Your Domain)                                   │          │
│   │                                                              │          │
│   │  pub type User {                                             │          │
│   │    User(id: String, name: String, email: String)            │          │
│   │  }                                                           │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                              │                                              │
│                              ▼                                              │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Type Builders (mochi/types)                                 │          │
│   │                                                              │          │
│   │  let user_type = types.object("User")                        │          │
│   │    |> types.id("id", fn(u) { u.id })                        │          │
│   │    |> types.string("name", fn(u) { u.name })                │          │
│   │    |> types.string("email", fn(u) { u.email })              │          │
│   │    |> types.build(decode_user)                               │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                              │                                              │
│                              ▼                                              │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Query Builders (mochi/query)                                │          │
│   │                                                              │          │
│   │  let schema = query.new()                                    │          │
│   │    |> query.add_query(user_query)                           │          │
│   │    |> query.add_type(user_type)                             │          │
│   │    |> query.build                                            │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                              │                                              │
│                              ▼                                              │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Schema (Runtime)                                            │          │
│   │                                                              │          │
│   │  Schema(                                                     │          │
│   │    query: Some(QueryType),                                   │          │
│   │    mutation: Some(MutationType),                             │          │
│   │    types: Dict<String, TypeDefinition>,                      │          │
│   │    directives: Dict<String, DirectiveDefinition>,            │          │
│   │  )                                                           │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                              │                                              │
│               ┌──────────────┴──────────────┐                              │
│               │                             │                              │
│               ▼                             ▼                              │
│   ┌─────────────────────┐       ┌─────────────────────┐                    │
│   │  TypeScript Codegen │       │    SDL Codegen      │                    │
│   │                     │       │                     │                    │
│   │  export interface   │       │  type User {        │                    │
│   │    User {           │       │    id: ID!          │                    │
│   │    id: string;      │       │    name: String     │                    │
│   │    name?: string;   │       │    email: String    │                    │
│   │  }                  │       │  }                  │                    │
│   └─────────────────────┘       └─────────────────────┘                    │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Subscription Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          SUBSCRIPTION FLOW                                   │
│                                                                             │
│   ┌─────────┐                                         ┌─────────┐          │
│   │ Client  │◀────────── WebSocket ─────────────────▶│ Server  │          │
│   └────┬────┘                                         └────┬────┘          │
│        │                                                   │               │
│        │  1. Subscribe                                     │               │
│        │  ──────────────────────────────────────────────▶ │               │
│        │  { "type": "subscribe",                           │               │
│        │    "id": "1",                                     │               │
│        │    "payload": {                                   │               │
│        │      "query": "subscription { onUserCreated }"    │               │
│        │    }                                              │               │
│        │  }                                                │               │
│        │                                                   │               │
│        │                      ┌────────────────────────────┤               │
│        │                      │                            │               │
│        │                      ▼                            │               │
│        │              ┌─────────────┐                      │               │
│        │              │   PubSub    │                      │               │
│        │              │             │                      │               │
│        │              │ Topics:     │                      │               │
│        │              │ • user:*    │◀──── Mutation        │               │
│        │              │ • post:*    │      creates user    │               │
│        │              │ • chat:123  │                      │               │
│        │              └──────┬──────┘                      │               │
│        │                     │                             │               │
│        │                     │ Event published             │               │
│        │                     ▼                             │               │
│        │  2. Next (data)                                   │               │
│        │ ◀────────────────────────────────────────────────│               │
│        │  { "type": "next",                                │               │
│        │    "id": "1",                                     │               │
│        │    "payload": {                                   │               │
│        │      "data": { "onUserCreated": { ... } }        │               │
│        │    }                                              │               │
│        │  }                                                │               │
│        │                                                   │               │
│        │  3. Complete (optional)                           │               │
│        │  ──────────────────────────────────────────────▶ │               │
│        │  { "type": "complete", "id": "1" }               │               │
│        │                                                   │               │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Module Dependencies

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           MODULE DEPENDENCY GRAPH                            │
│                                                                             │
│                              ┌──────────┐                                   │
│                              │  mochi   │ (Main export)                     │
│                              └────┬─────┘                                   │
│                                   │                                         │
│          ┌────────────────────────┼────────────────────────┐               │
│          │                        │                        │               │
│          ▼                        ▼                        ▼               │
│   ┌────────────┐           ┌────────────┐           ┌────────────┐         │
│   │   query    │           │   types    │           │  executor  │         │
│   │ (builders) │           │ (builders) │           │  (runtime) │         │
│   └─────┬──────┘           └─────┬──────┘           └─────┬──────┘         │
│         │                        │                        │                │
│         └────────────┬───────────┘                        │                │
│                      │                                    │                │
│                      ▼                                    │                │
│               ┌────────────┐                              │                │
│               │   schema   │◀─────────────────────────────┘                │
│               │   (core)   │                                               │
│               └─────┬──────┘                                               │
│                     │                                                      │
│     ┌───────────────┼───────────────┐                                     │
│     │               │               │                                     │
│     ▼               ▼               ▼                                     │
│ ┌──────────┐  ┌──────────┐   ┌────────────┐                               │
│ │ parser   │  │validation│   │ dataloader │                               │
│ └──────────┘  └──────────┘   └────────────┘                               │
│                                                                             │
│                        Supporting Modules                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │
│  │  error   │  │ response │  │   json   │  │ security │  │playground│    │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘  └──────────┘    │
│                                                                             │
│                          Codegen Modules                                    │
│               ┌────────────────┐  ┌────────────────┐                       │
│               │   typescript   │  │      sdl       │                       │
│               └────────────────┘  └────────────────┘                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Null Propagation

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          NULL PROPAGATION                                    │
│                                                                             │
│   Query: { user(id: 999) { name posts { title } } }                        │
│                                                                             │
│   Schema:                                                                   │
│   type Query {                                                              │
│     user(id: ID!): User        # Nullable                                  │
│   }                                                                         │
│   type User {                                                               │
│     name: String!              # Non-null                                  │
│     posts: [Post!]!            # Non-null list of non-null                 │
│   }                                                                         │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Case 1: User not found                                      │          │
│   │                                                              │          │
│   │  { "data": { "user": null } }                               │          │
│   │                                                              │          │
│   │  Result: null bubbles up to nullable "user" field           │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────┐          │
│   │  Case 2: User found, but name resolver fails                 │          │
│   │                                                              │          │
│   │  name is non-null (String!) so null bubbles up              │          │
│   │                                                              │          │
│   │  { "data": { "user": null },                                │          │
│   │    "errors": [{ "message": "...", "path": ["user","name"] }]│          │
│   │  }                                                           │          │
│   │                                                              │          │
│   │  Result: Entire user becomes null due to non-null violation │          │
│   └─────────────────────────────────────────────────────────────┘          │
│                                                                             │
│   Bubble Direction:                                                         │
│                                                                             │
│   Field fails ──▶ Is field non-null? ──▶ Bubble to parent                  │
│        │                 │                      │                          │
│        │                 │ No                   │                          │
│        │                 ▼                      │                          │
│        │           Return null                  │                          │
│        │           for this field               │                          │
│        │                                        │                          │
│        └────────────────────────────────────────┘                          │
│                         │                                                   │
│                         ▼                                                   │
│                 Repeat until nullable parent found                          │
│                 or root reached                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## File Structure

```
src/mochi/
├── query.gleam              # Query/Mutation/Subscription builders
├── types.gleam              # Type builders (object, enum, fields)
├── schema.gleam             # Core schema types, ExecutionContext
├── parser.gleam             # GraphQL query parser
├── executor.gleam           # Query execution engine
├── validation.gleam         # Query validation
├── dataloader.gleam         # N+1 query prevention
├── subscription.gleam       # PubSub subscription system
├── subscription_executor.gleam
├── error.gleam              # GraphQL-spec errors
├── response.gleam           # Response serialization
├── json.gleam               # JSON encoding
├── security.gleam           # Query security
├── persisted_queries.gleam  # APQ support
├── playground.gleam         # GraphQL IDEs
├── codegen/
│   ├── typescript.gleam     # TypeScript codegen
│   └── sdl.gleam            # SDL codegen
└── transport/
    └── websocket.gleam      # WebSocket transport
```
