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

The example includes DataLoader setup for efficient data fetching using Mochi's helper functions:

```gleam
// src/pokemon_api/loaders.gleam

// Create loaders with one-liner helper functions
fn create_pokemon_loader() {
  dataloader.int_loader_result(data.find_pokemon, pokemon_to_dynamic, "Pokemon not found")
}

fn create_move_loader() {
  dataloader.int_loader_result(data.find_move, move_to_dynamic, "Move not found")
}

// Register all loaders at once
pub fn create_context() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
  |> schema.with_loaders([
    #("pokemon", create_pokemon_loader()),
    #("move", create_move_loader()),
    #("trainer", create_trainer_loader()),
  ])
}

// Type converters using types.record and types.field helpers
fn pokemon_to_dynamic(p: Pokemon) -> Dynamic {
  types.record([
    types.field("id", p.id),
    types.field("name", p.name),
    types.field("pokemon_types", list.map(p.pokemon_types, type_to_string)),
    #("stats", stats_to_dynamic(p.stats)),
  ])
}
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

### Key Helper Functions

| Function | Purpose |
|----------|---------|
| `dataloader.int_loader_result(find, encode, err)` | Create loader from find function |
| `schema.with_loaders(ctx, loaders)` | Register multiple loaders at once |
| `types.record(fields)` | Build Dynamic dict for encoders |
| `types.field(name, value)` | Shorthand for field tuples |
| `types.option(opt)` | Convert Option to Dynamic |
