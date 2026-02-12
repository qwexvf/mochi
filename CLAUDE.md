# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**mochi** 🍡 is a Code First GraphQL library for Gleam. It allows developers to define GraphQL schemas using Gleam types and automatically generates TypeScript types and GraphQL SDL.

Inspired by [gqlkit](https://zenn.dev/izumin/articles/da27a6dfffba0b).

## Commands

```bash
gleam build              # Build the library
gleam test               # Run all tests (278 tests)
gleam run                # Run main demo
```

### Run specific examples
```bash
gleam run -m typescript_codegen_test  # TypeScript generation demo
gleam run -m sdl_codegen_test         # SDL generation demo
```

## Architecture

### Core Modules (`src/mochi/`)

| Module | Purpose |
|--------|---------|
| `query.gleam` | **Code First API** - Query/Mutation/Subscription builders |
| `types.gleam` | **Type Builders** - Object, enum, field definitions |
| `schema.gleam` | Core schema types and low-level builder API |
| `parser.gleam` | GraphQL query parsing |
| `executor.gleam` | Query execution engine with null propagation |
| `validation.gleam` | Query validation against schema |
| `subscription.gleam` | Subscription PubSub system |
| `subscription_executor.gleam` | Subscription execution |
| `error.gleam` | GraphQL-spec compliant errors with extensions |
| `response.gleam` | Response construction and serialization |
| `json.gleam` | JSON encoding |
| `dataloader.gleam` | N+1 query batching |

### Codegen Modules (`src/mochi/codegen/`)

| Module | Output |
|--------|--------|
| `typescript.gleam` | `.d.ts` TypeScript type definitions |
| `sdl.gleam` | `.graphql` Schema Definition Language |

## Key Concepts

### Code First API

The main API for defining schemas:

```gleam
import mochi/query
import mochi/types

// 1. Define Gleam type
pub type User {
  User(id: String, name: String, age: Int)
}

// 2. Create GraphQL type with extractors
let user_type = types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.int("age", fn(u: User) { u.age })
  |> types.build(decode_user)

// 3. Define queries
let users_query = query.query("users", return_type, resolver, encoder)

// 4. Build schema
let schema = query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
```

### Type Builders (`types.gleam`)

```gleam
types.object("TypeName")
|> types.description("...")
|> types.id("field", extractor)
|> types.string("field", extractor)
|> types.int("field", extractor)
|> types.float("field", extractor)
|> types.bool("field", extractor)
|> types.list_string("field", extractor)
|> types.build(decoder)
```

### Query Builders (`query.gleam`)

```gleam
// No args
query.query(name, return_type, resolver, encoder)

// With args
query.query_with_args(name, args, return_type, args_decoder, resolver, encoder)

// Mutation
query.mutation(name, args, return_type, args_decoder, resolver, encoder)
```

### Codegen

```gleam
import mochi/codegen/typescript
import mochi/codegen/sdl

// Generate TypeScript
let ts = typescript.generate(schema)

// Generate SDL
let gql = sdl.generate(schema)
```

## File Structure

```
src/
├── mochi.gleam                  # Main module, re-exports
├── mochi/
│   ├── query.gleam              # Code First query/mutation/subscription API
│   ├── types.gleam              # Type builders
│   ├── schema.gleam             # Core schema types
│   ├── parser.gleam             # GraphQL parser
│   ├── executor.gleam           # Query execution with null propagation
│   ├── validation.gleam         # Query validation
│   ├── subscription.gleam       # PubSub subscription system
│   ├── subscription_executor.gleam  # Subscription execution
│   ├── error.gleam              # GraphQL-spec errors
│   ├── response.gleam           # Response serialization
│   ├── json.gleam               # JSON encoding
│   ├── dataloader.gleam         # N+1 prevention
│   ├── codegen/
│   │   ├── typescript.gleam     # TS codegen
│   │   └── sdl.gleam            # SDL codegen
│   └── ...
├── mochi_ffi.mjs                # JavaScript FFI
├── mochi_ffi.erl                # Erlang FFI
test/
├── mochi_test.gleam             # Parser tests
├── code_first_test.gleam        # Code First API tests
├── subscription_test.gleam      # Subscription tests
├── error_test.gleam             # Error handling tests
├── response_test.gleam          # Response tests
├── null_propagation_test.gleam  # Null propagation tests
└── ...
```

## Development Notes

- Uses `gleeunit` for testing
- Uses `birdie` for snapshot testing
- Core library has minimal dependencies (`gleam_stdlib` only)
- `types.to_dynamic` converts Gleam values to Dynamic for resolvers
- Tests cover: parsing, schema building, codegen, subscriptions, error handling, null propagation, validation

## Common Patterns

### Decoder Function
```gleam
fn decode_user(dyn: Dynamic) -> Result(User, String) {
  // Decode Dynamic to typed value
  Ok(User(...))
}
```

### Resolver Function
```gleam
fn resolver(ctx: ExecutionContext) -> Result(a, String) {
  // Fetch data and return
  Ok(data)
}
```

### Encoder Function
```gleam
fn encoder(value: a) -> Dynamic {
  types.to_dynamic(value)
}
```
