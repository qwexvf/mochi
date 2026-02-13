// pokemon_api/loaders.gleam
// DataLoader setup for batching Pokemon data fetches
//
// Using mochi's simplified DataLoader API to solve N+1 queries.

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
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
// Loader Definitions (using mochi's int_loader helper)
// ============================================================================

fn create_pokemon_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader(fn(id) {
    case data.find_pokemon(id) {
      Ok(pokemon) -> Ok(pokemon_to_dynamic(pokemon))
      Error(_) -> Error("Pokemon not found")
    }
  })
}

fn create_move_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader(fn(id) {
    case data.find_move(id) {
      Ok(move) -> Ok(move_to_dynamic(move))
      Error(_) -> Error("Move not found")
    }
  })
}

fn create_trainer_loader() -> dataloader.DataLoader(
  dynamic.Dynamic,
  dynamic.Dynamic,
) {
  dataloader.int_loader(fn(id) {
    case data.find_trainer(id) {
      Ok(trainer) -> Ok(trainer_to_dynamic(trainer))
      Error(_) -> Error("Trainer not found")
    }
  })
}

// ============================================================================
// Type Converters
// ============================================================================

import gleam/dynamic

fn pokemon_to_dynamic(p: pokemon_types.Pokemon) -> dynamic.Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(p.id)),
      #("name", types.to_dynamic(p.name)),
      #("pokedex_number", types.to_dynamic(p.pokedex_number)),
      #(
        "pokemon_types",
        types.to_dynamic(list.map(p.pokemon_types, pokemon_types.type_to_string)),
      ),
      #("stats", stats_to_dynamic(p.stats)),
      #("moves", types.to_dynamic(p.moves)),
      #("sprite_url", types.to_dynamic(p.sprite_url)),
      #("height", types.to_dynamic(p.height)),
      #("weight", types.to_dynamic(p.weight)),
    ]),
  )
}

fn stats_to_dynamic(s: pokemon_types.Stats) -> dynamic.Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("hp", types.to_dynamic(s.hp)),
      #("attack", types.to_dynamic(s.attack)),
      #("defense", types.to_dynamic(s.defense)),
      #("special_attack", types.to_dynamic(s.special_attack)),
      #("special_defense", types.to_dynamic(s.special_defense)),
      #("speed", types.to_dynamic(s.speed)),
    ]),
  )
}

fn move_to_dynamic(m: pokemon_types.Move) -> dynamic.Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(m.id)),
      #("name", types.to_dynamic(m.name)),
      #(
        "move_type",
        types.to_dynamic(pokemon_types.type_to_string(m.move_type)),
      ),
      #("power", option_to_dynamic(m.power)),
      #("accuracy", option_to_dynamic(m.accuracy)),
      #("pp", types.to_dynamic(m.pp)),
      #("description", types.to_dynamic(m.description)),
      #(
        "category",
        types.to_dynamic(pokemon_types.category_to_string(m.category)),
      ),
    ]),
  )
}

fn trainer_to_dynamic(t: pokemon_types.Trainer) -> dynamic.Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(t.id)),
      #("name", types.to_dynamic(t.name)),
      #("team", types.to_dynamic(t.team)),
      #("badges", types.to_dynamic(t.badges)),
      #("pokedex_caught", types.to_dynamic(t.pokedex_caught)),
    ]),
  )
}

fn option_to_dynamic(opt: Option(Int)) -> dynamic.Dynamic {
  case opt {
    Some(v) -> types.to_dynamic(v)
    None -> types.to_dynamic(Nil)
  }
}
