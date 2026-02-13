# Pokemon GraphQL API

A complete GraphQL API for Pokemon data, built with **Mochi** and **Wisp**.

## Quick Start

```bash
# Start the server
gleam run

# Generate TypeScript types and GraphQL SDL
gleam run -- codegen

# Or generate individually
gleam run -- gen types     # Generate types.ts
gleam run -- gen sdl       # Generate schema.graphql
```

## Endpoints

| Endpoint | Description |
|----------|-------------|
| `POST /graphql` | Execute GraphQL queries |
| `GET /graphiql` | GraphiQL IDE |
| `GET /playground` | GraphQL Playground |
| `GET /explorer` | Simple query explorer |
| `GET /schema.graphql` | Download SDL |
| `GET /types.ts` | Download TypeScript types |

## Example Queries

### Get a Pokemon

```graphql
{
  pokemon(id: 25) {
    name
    types
    spriteUrl
    stats {
      hp
      attack
      defense
      speed
      total
    }
    moves {
      name
      type
      power
      category
    }
  }
}
```

### Get Pokemon by Type

```graphql
{
  pokemonByType(type: FIRE) {
    name
    pokedexNumber
    stats { total }
  }
}
```

### Get a Trainer

```graphql
{
  trainer(id: 1) {
    name
    badges
    pokedexCaught
    team {
      name
      types
      stats { total }
    }
  }
}
```

### Get All Pokemon

```graphql
{
  allPokemon {
    name
    types
    pokedexNumber
  }
}
```

## cURL Example

```bash
curl -X POST http://localhost:8000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ pokemon(id: 25) { name types } }"}'
```

## Schema

The API includes:

- **Pokemon** - Name, types, stats, moves, evolution chain
- **Move** - Name, type, power, accuracy, category
- **Trainer** - Name, team, badges
- **Stats** - HP, Attack, Defense, Sp.Atk, Sp.Def, Speed

### Enums

- `PokemonType` - NORMAL, FIRE, WATER, ELECTRIC, GRASS, etc.
- `MoveCategory` - PHYSICAL, SPECIAL, STATUS

## Project Structure

```
pokemon_api/
├── src/
│   ├── pokemon_api.gleam       # Main entry point, HTTP server
│   └── pokemon_api/
│       ├── schema.gleam        # GraphQL schema definition
│       ├── data.gleam          # Pokemon data store
│       └── types.gleam         # Gleam type definitions
├── gleam.toml                  # Project config
├── schema.graphql              # Generated SDL
└── types.ts                    # Generated TypeScript
```

## Features Demonstrated

- **Code First Schema** - Define GraphQL types using Gleam
- **TypeScript Codegen** - Generate type-safe client types
- **SDL Generation** - Generate GraphQL schema files
- **Wisp Integration** - HTTP server with routing
- **GraphQL Playgrounds** - Multiple IDE options
- **Complex Types** - Enums, lists, nullable fields, nested objects
- **Field Resolvers** - Custom resolution logic per field
- **DataLoader** - Batch loading infrastructure for N+1 prevention

## DataLoader

The example includes DataLoader setup for efficient data fetching:

```gleam
// src/pokemon_api/loaders.gleam

// Create loaders for each entity type
pub fn create_pokemon_loader() -> DataLoader(Dynamic, Dynamic)
pub fn create_move_loader() -> DataLoader(Dynamic, Dynamic)
pub fn create_trainer_loader() -> DataLoader(Dynamic, Dynamic)

// Create an ExecutionContext with all loaders initialized
pub fn create_context() -> schema.ExecutionContext

// Load helpers that handle context threading
pub fn load_pokemon(ctx, id) -> #(ExecutionContext, Result(Dynamic, String))
pub fn load_pokemon_many(ctx, ids) -> #(ExecutionContext, List(Result))
```

This solves the N+1 problem when fetching nested data:

```graphql
{
  trainer(id: 1) {
    team {        # Would be 5 separate queries without batching
      moves {     # Each Pokemon's moves - more queries
        name
      }
    }
  }
}
```

With DataLoader, all Pokemon and Move fetches are batched into minimal queries.
