// pokemon_api/loaders.gleam
// DataLoader implementations for batching Pokemon data fetches
//
// This solves the N+1 problem when fetching nested data like:
//   trainer { team { moves { ... } } }
//
// Instead of fetching each Pokemon/Move individually, DataLoader
// batches all requests within the same GraphQL operation.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import mochi/dataloader.{type DataLoader}
import mochi/schema
import mochi/types
import pokemon_api/data
import pokemon_api/types as pokemon_types

// ============================================================================
// Loader Names (used as keys in ExecutionContext)
// ============================================================================

pub const pokemon_loader = "pokemon"

pub const move_loader = "move"

pub const trainer_loader = "trainer"

// ============================================================================
// Pokemon Loader
// ============================================================================

/// Create a DataLoader for loading Pokemon by ID
pub fn create_pokemon_loader() -> DataLoader(Dynamic, Dynamic) {
  dataloader.new(batch_load_pokemon)
}

fn batch_load_pokemon(
  keys: List(Dynamic),
) -> Result(List(Result(Dynamic, String)), String) {
  // Log the batch for demonstration
  io.println(
    "[DataLoader] Batch loading "
    <> int.to_string(list.length(keys))
    <> " Pokemon",
  )

  // Convert Dynamic keys to Int IDs
  let results =
    list.map(keys, fn(key) {
      case decode.run(key, decode.int) {
        Ok(id) -> {
          case data.find_pokemon(id) {
            Ok(pokemon) -> Ok(pokemon_to_dynamic(pokemon))
            Error(_) -> Error("Pokemon not found: " <> int.to_string(id))
          }
        }
        Error(_) -> Error("Invalid Pokemon ID")
      }
    })

  Ok(results)
}

// ============================================================================
// Move Loader
// ============================================================================

/// Create a DataLoader for loading Moves by ID
pub fn create_move_loader() -> DataLoader(Dynamic, Dynamic) {
  dataloader.new(batch_load_moves)
}

fn batch_load_moves(
  keys: List(Dynamic),
) -> Result(List(Result(Dynamic, String)), String) {
  io.println(
    "[DataLoader] Batch loading "
    <> int.to_string(list.length(keys))
    <> " Moves",
  )

  let results =
    list.map(keys, fn(key) {
      case decode.run(key, decode.int) {
        Ok(id) -> {
          case data.find_move(id) {
            Ok(move) -> Ok(move_to_dynamic(move))
            Error(_) -> Error("Move not found: " <> int.to_string(id))
          }
        }
        Error(_) -> Error("Invalid Move ID")
      }
    })

  Ok(results)
}

// ============================================================================
// Trainer Loader
// ============================================================================

/// Create a DataLoader for loading Trainers by ID
pub fn create_trainer_loader() -> DataLoader(Dynamic, Dynamic) {
  dataloader.new(batch_load_trainers)
}

fn batch_load_trainers(
  keys: List(Dynamic),
) -> Result(List(Result(Dynamic, String)), String) {
  io.println(
    "[DataLoader] Batch loading "
    <> int.to_string(list.length(keys))
    <> " Trainers",
  )

  let results =
    list.map(keys, fn(key) {
      case decode.run(key, decode.int) {
        Ok(id) -> {
          case data.find_trainer(id) {
            Ok(trainer) -> Ok(trainer_to_dynamic(trainer))
            Error(_) -> Error("Trainer not found: " <> int.to_string(id))
          }
        }
        Error(_) -> Error("Invalid Trainer ID")
      }
    })

  Ok(results)
}

// ============================================================================
// Context Builder
// ============================================================================

/// Create an ExecutionContext with all DataLoaders initialized
pub fn create_context() -> schema.ExecutionContext {
  schema.execution_context(types.to_dynamic(dict.new()))
  |> schema.add_data_loader(pokemon_loader, create_pokemon_loader())
  |> schema.add_data_loader(move_loader, create_move_loader())
  |> schema.add_data_loader(trainer_loader, create_trainer_loader())
}

// ============================================================================
// Helper: Load Pokemon via DataLoader
// ============================================================================

/// Load a Pokemon by ID using the DataLoader from context
pub fn load_pokemon(
  ctx: schema.ExecutionContext,
  id: Int,
) -> #(schema.ExecutionContext, Result(Dynamic, String)) {
  case schema.get_data_loader(ctx, pokemon_loader) {
    Ok(loader) -> {
      let #(new_loader, result) = dataloader.load(loader, types.to_dynamic(id))
      let new_ctx = schema.update_data_loader(ctx, pokemon_loader, new_loader)
      #(new_ctx, result)
    }
    Error(e) -> #(ctx, Error(e))
  }
}

/// Load multiple Pokemon by IDs using the DataLoader
pub fn load_pokemon_many(
  ctx: schema.ExecutionContext,
  ids: List(Int),
) -> #(schema.ExecutionContext, List(Result(Dynamic, String))) {
  case schema.get_data_loader(ctx, pokemon_loader) {
    Ok(loader) -> {
      let keys = list.map(ids, types.to_dynamic)
      let #(new_loader, results) = dataloader.load_many(loader, keys)
      let new_ctx = schema.update_data_loader(ctx, pokemon_loader, new_loader)
      #(new_ctx, results)
    }
    Error(e) -> #(ctx, list.map(ids, fn(_) { Error(e) }))
  }
}

/// Load a Move by ID using the DataLoader
pub fn load_move(
  ctx: schema.ExecutionContext,
  id: Int,
) -> #(schema.ExecutionContext, Result(Dynamic, String)) {
  case schema.get_data_loader(ctx, move_loader) {
    Ok(loader) -> {
      let #(new_loader, result) = dataloader.load(loader, types.to_dynamic(id))
      let new_ctx = schema.update_data_loader(ctx, move_loader, new_loader)
      #(new_ctx, result)
    }
    Error(e) -> #(ctx, Error(e))
  }
}

/// Load multiple Moves by IDs using the DataLoader
pub fn load_moves_many(
  ctx: schema.ExecutionContext,
  ids: List(Int),
) -> #(schema.ExecutionContext, List(Result(Dynamic, String))) {
  case schema.get_data_loader(ctx, move_loader) {
    Ok(loader) -> {
      let keys = list.map(ids, types.to_dynamic)
      let #(new_loader, results) = dataloader.load_many(loader, keys)
      let new_ctx = schema.update_data_loader(ctx, move_loader, new_loader)
      #(new_ctx, results)
    }
    Error(e) -> #(ctx, list.map(ids, fn(_) { Error(e) }))
  }
}

// ============================================================================
// Type Converters (same as schema.gleam but needed here to avoid cycles)
// ============================================================================

fn pokemon_to_dynamic(p: pokemon_types.Pokemon) -> Dynamic {
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

fn stats_to_dynamic(s: pokemon_types.Stats) -> Dynamic {
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

fn move_to_dynamic(m: pokemon_types.Move) -> Dynamic {
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

fn trainer_to_dynamic(t: pokemon_types.Trainer) -> Dynamic {
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

import gleam/option.{type Option, None, Some}

fn option_to_dynamic(opt: Option(Int)) -> Dynamic {
  case opt {
    Some(v) -> types.to_dynamic(v)
    None -> types.to_dynamic(Nil)
  }
}
