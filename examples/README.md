# Mochi Examples

This directory contains complete examples showing how to use Mochi in real-world applications.

## Pokemon API

**[`pokemon_api/`](pokemon_api/)** - A complete GraphQL API demonstrating:

- **Code First Schema** - Define types with Gleam, generate GraphQL SDL
- **TypeScript Codegen** - Auto-generate TypeScript types
- **Wisp Integration** - HTTP server with Wisp 2.0
- **GraphiQL/Playground** - Interactive query development
- **Complex Types** - Enums, nested objects, lists, nullable fields

### Quick Start

```sh
cd pokemon_api
gleam run                    # Start server at http://localhost:8000
gleam run -- codegen         # Generate types.ts and schema.graphql
```

### Example Queries

```graphql
# Get Pikachu with stats
{
  pokemon(id: 25) {
    name
    types
    stats { hp attack speed total }
    moves { name power type }
  }
}

# Get all Fire type Pokemon
{
  pokemonByType(type: FIRE) {
    name
    pokedexNumber
  }
}

# Get trainer Red's team
{
  trainer(id: 1) {
    name
    badges
    team { name types }
  }
}
```

### Endpoints

- `POST /graphql` - Execute queries
- `GET /graphiql` - GraphiQL IDE
- `GET /playground` - GraphQL Playground
- `GET /schema.graphql` - Download SDL
- `GET /types.ts` - Download TypeScript types

---

## Mochi + Wisp Integration

**[`mochi_wisp/`](mochi_wisp/)** - Advanced Wisp integration with benchmarks:

- **GraphQL Handler** - HTTP request handling
- **Benchmark Suite** - Performance testing
- **Complex Schema** - Real-world schema patterns

```sh
cd mochi_wisp
gleam run
```

---

## Core Library Examples

**[`core_library_examples/`](core_library_examples/)** - Pure GraphQL functionality:

- **GraphQL Parsing** - Parse queries into AST
- **Schema Definition** - Fluent builder API
- **Query Execution** - Struct-based resolvers
- **Error Handling** - GraphQL-spec compliant errors

```sh
cd core_library_examples
gleam run
```

---

## Standalone Examples

Single-file examples demonstrating specific features:

| File | Description |
|------|-------------|
| `basic_schema.gleam` | Simple schema definition |
| `code_first_example.gleam` | Code First API usage |
| `codegen_example.gleam` | TypeScript/SDL generation |
| `custom_directives.gleam` | Custom directive handlers |
| `with_mutations.gleam` | Mutations and input types |

---

## Architecture

Mochi follows a **clean separation philosophy**:

```
┌─────────────────────────────────────┐
│        Your Application            │
│  - Web server (Wisp/Mist)          │
│  - Database integration            │
│  - Business logic & resolvers      │
│  - Authentication                  │
└─────────────────────────────────────┘
               │ uses
               ▼
┌─────────────────────────────────────┐
│         Mochi Library              │
│  - GraphQL parsing & execution     │
│  - Schema definition               │
│  - TypeScript/SDL codegen          │
│  - Subscriptions & DataLoader      │
└─────────────────────────────────────┘
```

**Mochi handles GraphQL** - your application handles everything else.
