// pokemon_api/loaders.gleam
// DataLoader setup for batching Pokemon data fetches
//
// Using mochi's simplified DataLoader API to solve N+1 queries.

import gleam/dict
import gleam/dynamic
import gleam/list
import mochi/dataloader
import mochi/schema
import mochi/types
import pokemon_api/data
import pokemon_api/types as pokemon_types

// ============================================================================
// Loader Names
// ============================================================================

pub const pokemon_loader = "pokemon"

pub const move_loader = "move"

pub const trainer_loader = "trainer"

// ============================================================================
// Create Execution Context with All Loaders
// ============================================================================

/// Create an ExecutionContext with all DataLoaders initialized
pub fn create_context() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
  |> schema.with_loaders([
    #(pokemon_loader, create_pokemon_loader()),
    #(move_loader, create_move_loader()),
    #(trainer_loader, create_trainer_loader()),
  ])
}

// ============================================================================
// Loader Definitions (using mochi's int_loader_result helper)
// ============================================================================

fn create_pokemon_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader_result(
    data.find_pokemon,
    pokemon_to_dynamic,
    "Pokemon not found",
  )
}

fn create_move_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader_result(
    data.find_move,
    move_to_dynamic,
    "Move not found",
  )
}

fn create_trainer_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader_result(
    data.find_trainer,
    trainer_to_dynamic,
    "Trainer not found",
  )
}

// ============================================================================
// Type Converters (using types.record and types.field helpers)
// ============================================================================

fn pokemon_to_dynamic(p: pokemon_types.Pokemon) -> dynamic.Dynamic {
  types.record([
    types.field("id", p.id),
    types.field("name", p.name),
    types.field("pokedex_number", p.pokedex_number),
    types.field(
      "pokemon_types",
      list.map(p.pokemon_types, pokemon_types.type_to_string),
    ),
    #("stats", stats_to_dynamic(p.stats)),
    types.field("moves", p.moves),
    types.field("sprite_url", p.sprite_url),
    types.field("height", p.height),
    types.field("weight", p.weight),
  ])
}

fn stats_to_dynamic(s: pokemon_types.Stats) -> dynamic.Dynamic {
  types.record([
    types.field("hp", s.hp),
    types.field("attack", s.attack),
    types.field("defense", s.defense),
    types.field("special_attack", s.special_attack),
    types.field("special_defense", s.special_defense),
    types.field("speed", s.speed),
  ])
}

fn move_to_dynamic(m: pokemon_types.Move) -> dynamic.Dynamic {
  types.record([
    types.field("id", m.id),
    types.field("name", m.name),
    types.field("move_type", pokemon_types.type_to_string(m.move_type)),
    #("power", types.option(m.power)),
    #("accuracy", types.option(m.accuracy)),
    types.field("pp", m.pp),
    types.field("description", m.description),
    types.field("category", pokemon_types.category_to_string(m.category)),
  ])
}

fn trainer_to_dynamic(t: pokemon_types.Trainer) -> dynamic.Dynamic {
  types.record([
    types.field("id", t.id),
    types.field("name", t.name),
    types.field("team", t.team),
    types.field("badges", t.badges),
    types.field("pokedex_caught", t.pokedex_caught),
  ])
}
