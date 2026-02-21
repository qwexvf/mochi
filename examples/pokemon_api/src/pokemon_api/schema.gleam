// pokemon_api/schema.gleam
// GraphQL schema for the Pokemon API - Using high-level types API

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import mochi/query
import mochi/schema
import mochi/types
import pokemon_api/data
import pokemon_api/types as pokemon_types

// ============================================================================
// Decoders - Convert Dynamic (from encoders) back to typed values
// ============================================================================

fn decode_stats(dyn: Dynamic) -> Result(pokemon_types.Stats, String) {
  let decoder = {
    use hp <- decode.field("hp", decode.int)
    use attack <- decode.field("attack", decode.int)
    use defense <- decode.field("defense", decode.int)
    use special_attack <- decode.field("special_attack", decode.int)
    use special_defense <- decode.field("special_defense", decode.int)
    use speed <- decode.field("speed", decode.int)
    decode.success(pokemon_types.Stats(
      hp: hp,
      attack: attack,
      defense: defense,
      special_attack: special_attack,
      special_defense: special_defense,
      speed: speed,
    ))
  }
  decode.run(dyn, decoder)
  |> result_to_string_error("Failed to decode Stats")
}

fn decode_move(dyn: Dynamic) -> Result(pokemon_types.Move, String) {
  let decoder = {
    use id <- decode.field("id", decode.int)
    use name <- decode.field("name", decode.string)
    use move_type_str <- decode.field("move_type", decode.string)
    use power <- decode.field("power", decode.optional(decode.int))
    use accuracy <- decode.field("accuracy", decode.optional(decode.int))
    use pp <- decode.field("pp", decode.int)
    use description <- decode.field("description", decode.string)
    use category_str <- decode.field("category", decode.string)
    decode.success(#(
      id,
      name,
      move_type_str,
      power,
      accuracy,
      pp,
      description,
      category_str,
    ))
  }
  case decode.run(dyn, decoder) {
    Ok(#(id, name, move_type_str, power, accuracy, pp, description, category_str)) -> {
      case
        pokemon_types.string_to_type(move_type_str),
        pokemon_types.string_to_category(category_str)
      {
        Ok(move_type), Ok(category) ->
          Ok(pokemon_types.Move(
            id: id,
            name: name,
            move_type: move_type,
            power: power,
            accuracy: accuracy,
            pp: pp,
            description: description,
            category: category,
          ))
        _, _ -> Error("Failed to decode Move enums")
      }
    }
    Error(_) -> Error("Failed to decode Move")
  }
}

fn decode_pokemon(dyn: Dynamic) -> Result(pokemon_types.Pokemon, String) {
  let decoder = {
    use id <- decode.field("id", decode.int)
    use name <- decode.field("name", decode.string)
    use pokedex_number <- decode.field("pokedex_number", decode.int)
    use pokemon_types_strs <- decode.field(
      "pokemon_types",
      decode.list(decode.string),
    )
    use stats <- decode.field("stats", decode.dynamic)
    use moves <- decode.field("moves", decode.list(decode.int))
    use sprite_url <- decode.field("sprite_url", decode.string)
    use height <- decode.field("height", decode.float)
    use weight <- decode.field("weight", decode.float)
    decode.success(#(
      id,
      name,
      pokedex_number,
      pokemon_types_strs,
      stats,
      moves,
      sprite_url,
      height,
      weight,
    ))
  }
  case decode.run(dyn, decoder) {
    Ok(#(
      id,
      name,
      pokedex_number,
      pokemon_types_strs,
      stats_dyn,
      moves,
      sprite_url,
      height,
      weight,
    )) -> {
      let pokemon_types_result =
        list.try_map(pokemon_types_strs, pokemon_types.string_to_type)
      case pokemon_types_result, decode_stats(stats_dyn) {
        Ok(ptypes), Ok(stats) ->
          Ok(pokemon_types.Pokemon(
            id: id,
            name: name,
            pokedex_number: pokedex_number,
            pokemon_types: ptypes,
            stats: stats,
            moves: moves,
            evolution_chain_id: None,
            sprite_url: sprite_url,
            height: height,
            weight: weight,
          ))
        _, _ -> Error("Failed to decode Pokemon nested types")
      }
    }
    Error(_) -> Error("Failed to decode Pokemon")
  }
}

fn decode_trainer(dyn: Dynamic) -> Result(pokemon_types.Trainer, String) {
  let decoder = {
    use id <- decode.field("id", decode.int)
    use name <- decode.field("name", decode.string)
    use team <- decode.field("team", decode.list(decode.int))
    use badges <- decode.field("badges", decode.int)
    use pokedex_caught <- decode.field("pokedex_caught", decode.int)
    decode.success(pokemon_types.Trainer(
      id: id,
      name: name,
      team: team,
      badges: badges,
      pokedex_caught: pokedex_caught,
    ))
  }
  decode.run(dyn, decoder)
  |> result_to_string_error("Failed to decode Trainer")
}

fn result_to_string_error(
  result: Result(a, b),
  error_msg: String,
) -> Result(a, String) {
  case result {
    Ok(v) -> Ok(v)
    Error(_) -> Error(error_msg)
  }
}

// ============================================================================
// Encoders - Convert typed values to Dynamic using types helpers
// ============================================================================

pub fn stats_to_dynamic(s: pokemon_types.Stats) -> Dynamic {
  types.record([
    types.field("hp", s.hp),
    types.field("attack", s.attack),
    types.field("defense", s.defense),
    types.field("special_attack", s.special_attack),
    types.field("special_defense", s.special_defense),
    types.field("speed", s.speed),
  ])
}

pub fn move_to_dynamic(m: pokemon_types.Move) -> Dynamic {
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

pub fn pokemon_to_dynamic(p: pokemon_types.Pokemon) -> Dynamic {
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

pub fn trainer_to_dynamic(t: pokemon_types.Trainer) -> Dynamic {
  types.record([
    types.field("id", t.id),
    types.field("name", t.name),
    types.field("team", t.team),
    types.field("badges", t.badges),
    types.field("pokedex_caught", t.pokedex_caught),
  ])
}

// ============================================================================
// GraphQL Enum Type Definitions
// ============================================================================

fn pokemon_type_enum() -> schema.EnumType {
  types.enum_type("PokemonType")
  |> types.enum_description("The elemental type of a Pokemon")
  |> types.value("NORMAL")
  |> types.value("FIRE")
  |> types.value("WATER")
  |> types.value("ELECTRIC")
  |> types.value("GRASS")
  |> types.value("ICE")
  |> types.value("FIGHTING")
  |> types.value("POISON")
  |> types.value("GROUND")
  |> types.value("FLYING")
  |> types.value("PSYCHIC")
  |> types.value("BUG")
  |> types.value("ROCK")
  |> types.value("GHOST")
  |> types.value("DRAGON")
  |> types.value("DARK")
  |> types.value("STEEL")
  |> types.value("FAIRY")
  |> types.build_enum
}

fn move_category_enum() -> schema.EnumType {
  types.enum_type("MoveCategory")
  |> types.enum_description("The category of a move")
  |> types.value_with_desc("PHYSICAL", "Uses Attack and Defense stats")
  |> types.value_with_desc(
    "SPECIAL",
    "Uses Special Attack and Special Defense stats",
  )
  |> types.value_with_desc("STATUS", "Does not deal damage directly")
  |> types.build_enum
}

// ============================================================================
// GraphQL Object Type Definitions - Using High-Level types API
// ============================================================================

fn stats_type() -> schema.ObjectType {
  types.object("Stats")
  |> types.description("Base stats of a Pokemon")
  |> types.int_with_desc("hp", "Hit Points", fn(s: pokemon_types.Stats) { s.hp })
  |> types.int_with_desc("attack", "Physical attack power", fn(s: pokemon_types.Stats) {
    s.attack
  })
  |> types.int_with_desc("defense", "Physical defense", fn(s: pokemon_types.Stats) {
    s.defense
  })
  |> types.int_with_desc(
    "specialAttack",
    "Special attack power",
    fn(s: pokemon_types.Stats) { s.special_attack },
  )
  |> types.int_with_desc(
    "specialDefense",
    "Special defense",
    fn(s: pokemon_types.Stats) { s.special_defense },
  )
  |> types.int_with_desc(
    "speed",
    "Speed determines turn order",
    fn(s: pokemon_types.Stats) { s.speed },
  )
  |> types.build(decode_stats)
  // Add computed total field
  |> schema.field(
    schema.field_def("total", schema.non_null(schema.int_type()))
    |> schema.field_description("Total of all base stats")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_stats(p) {
            Ok(s) -> {
              let total =
                s.hp
                + s.attack
                + s.defense
                + s.special_attack
                + s.special_defense
                + s.speed
              Ok(types.to_dynamic(total))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
}

fn move_type() -> schema.ObjectType {
  types.object("Move")
  |> types.description("A move that a Pokemon can learn")
  |> types.int("id", fn(m: pokemon_types.Move) { m.id })
  |> types.string("name", fn(m: pokemon_types.Move) { m.name })
  |> types.string_with_desc("type", "The elemental type of this move", fn(m: pokemon_types.Move) {
    pokemon_types.type_to_string(m.move_type)
  })
  |> types.optional_int("power", fn(m: pokemon_types.Move) { m.power })
  |> types.optional_int("accuracy", fn(m: pokemon_types.Move) { m.accuracy })
  |> types.int("pp", fn(m: pokemon_types.Move) { m.pp })
  |> types.string("description", fn(m: pokemon_types.Move) { m.description })
  |> types.string("category", fn(m: pokemon_types.Move) {
    pokemon_types.category_to_string(m.category)
  })
  |> types.build(decode_move)
}

fn pokemon_object_type() -> schema.ObjectType {
  types.object("Pokemon")
  |> types.description("A Pokemon creature")
  |> types.int("id", fn(p: pokemon_types.Pokemon) { p.id })
  |> types.string_with_desc("name", "The name of this Pokemon", fn(p: pokemon_types.Pokemon) {
    p.name
  })
  |> types.int_with_desc(
    "pokedexNumber",
    "National Pokedex number",
    fn(p: pokemon_types.Pokemon) { p.pokedex_number },
  )
  |> types.list_string("types", fn(p: pokemon_types.Pokemon) {
    list.map(p.pokemon_types, pokemon_types.type_to_string)
  })
  |> types.object_field("stats", "Stats", fn(p: pokemon_types.Pokemon) {
    stats_to_dynamic(p.stats)
  })
  |> types.string_with_desc(
    "spriteUrl",
    "URL to the Pokemon's sprite image",
    fn(p: pokemon_types.Pokemon) { p.sprite_url },
  )
  |> types.float("height", fn(p: pokemon_types.Pokemon) { p.height })
  |> types.float("weight", fn(p: pokemon_types.Pokemon) { p.weight })
  |> types.build(decode_pokemon)
  // Add moves field with data loading
  |> schema.field(
    schema.field_def(
      "moves",
      schema.non_null(schema.List(schema.non_null(schema.Named("Move")))),
    )
    |> schema.field_description("Moves this Pokemon can learn")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_pokemon(p) {
            Ok(pokemon) -> {
              let moves =
                list.filter_map(pokemon.moves, fn(id) {
                  case data.find_move(id) {
                    Ok(m) -> Ok(move_to_dynamic(m))
                    Error(_) -> Error(Nil)
                  }
                })
              Ok(types.to_dynamic(moves))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  // Add evolvesTo field with data loading
  |> schema.field(
    schema.field_def("evolvesTo", schema.Named("Pokemon"))
    |> schema.field_description("The Pokemon this one evolves into")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_pokemon(p) {
            Ok(pokemon) -> {
              let evolution = case pokemon.id {
                1 -> data.find_pokemon(2)
                2 -> data.find_pokemon(3)
                4 -> data.find_pokemon(5)
                5 -> data.find_pokemon(6)
                7 -> data.find_pokemon(8)
                8 -> data.find_pokemon(9)
                25 -> data.find_pokemon(26)
                _ -> Error("No evolution")
              }
              case evolution {
                Ok(pkmn) -> Ok(pokemon_to_dynamic(pkmn))
                Error(_) -> Ok(types.to_dynamic(Nil))
              }
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
}

fn trainer_type() -> schema.ObjectType {
  types.object("Trainer")
  |> types.description("A Pokemon trainer")
  |> types.int("id", fn(t: pokemon_types.Trainer) { t.id })
  |> types.string("name", fn(t: pokemon_types.Trainer) { t.name })
  |> types.int_with_desc("badges", "Number of gym badges earned", fn(t: pokemon_types.Trainer) {
    t.badges
  })
  |> types.int_with_desc(
    "pokedexCaught",
    "Number of Pokemon caught",
    fn(t: pokemon_types.Trainer) { t.pokedex_caught },
  )
  |> types.build(decode_trainer)
  // Add team field with data loading
  |> schema.field(
    schema.field_def(
      "team",
      schema.non_null(schema.List(schema.non_null(schema.Named("Pokemon")))),
    )
    |> schema.field_description("The trainer's Pokemon team")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_trainer(p) {
            Ok(trainer) -> {
              let pokemon =
                list.filter_map(trainer.team, fn(id) {
                  case data.find_pokemon(id) {
                    Ok(pkmn) -> Ok(pokemon_to_dynamic(pkmn))
                    Error(_) -> Error(Nil)
                  }
                })
              Ok(types.to_dynamic(pokemon))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
}

// ============================================================================
// Query Definitions
// ============================================================================

fn pokemon_query() {
  query.query_with_args(
    "pokemon",
    [
      query.arg_with_desc(
        "id",
        schema.non_null(schema.int_type()),
        "Pokemon ID",
      ),
    ],
    schema.Named("Pokemon"),
    decode_id_args,
    fn(args: IdArgs, _ctx) { data.find_pokemon(args.id) },
    pokemon_to_dynamic,
  )
  |> query.query_description("Get a Pokemon by ID")
}

fn pokemon_by_name_query() {
  query.query_with_args(
    "pokemonByName",
    [
      query.arg_with_desc(
        "name",
        schema.non_null(schema.string_type()),
        "Pokemon name",
      ),
    ],
    schema.Named("Pokemon"),
    decode_name_args,
    fn(args: NameArgs, _ctx) { data.find_pokemon_by_name(args.name) },
    pokemon_to_dynamic,
  )
  |> query.query_description("Get a Pokemon by name")
}

fn all_pokemon_query() {
  query.query(
    "allPokemon",
    schema.non_null(schema.List(schema.non_null(schema.Named("Pokemon")))),
    fn(_ctx) { Ok(data.all_pokemon()) },
    fn(pokemon) { types.to_dynamic(list.map(pokemon, pokemon_to_dynamic)) },
  )
  |> query.query_description("Get all Pokemon")
}

fn pokemon_by_type_query() {
  query.query_with_args(
    "pokemonByType",
    [
      query.arg_with_desc(
        "type",
        schema.non_null(schema.Named("PokemonType")),
        "Pokemon type to filter by",
      ),
    ],
    schema.non_null(schema.List(schema.non_null(schema.Named("Pokemon")))),
    decode_type_args,
    fn(args: TypeArgs, _ctx) { Ok(data.pokemon_by_type(args.pokemon_type)) },
    fn(pokemon) { types.to_dynamic(list.map(pokemon, pokemon_to_dynamic)) },
  )
  |> query.query_description("Get all Pokemon of a specific type")
}

fn move_query() {
  query.query_with_args(
    "move",
    [query.arg_with_desc("id", schema.non_null(schema.int_type()), "Move ID")],
    schema.Named("Move"),
    decode_id_args,
    fn(args: IdArgs, _ctx) { data.find_move(args.id) },
    move_to_dynamic,
  )
  |> query.query_description("Get a move by ID")
}

fn all_moves_query() {
  query.query(
    "allMoves",
    schema.non_null(schema.List(schema.non_null(schema.Named("Move")))),
    fn(_ctx) { Ok(data.all_moves()) },
    fn(moves) { types.to_dynamic(list.map(moves, move_to_dynamic)) },
  )
  |> query.query_description("Get all moves")
}

fn trainer_query() {
  query.query_with_args(
    "trainer",
    [
      query.arg_with_desc(
        "id",
        schema.non_null(schema.int_type()),
        "Trainer ID",
      ),
    ],
    schema.Named("Trainer"),
    decode_id_args,
    fn(args: IdArgs, _ctx) { data.find_trainer(args.id) },
    trainer_to_dynamic,
  )
  |> query.query_description("Get a trainer by ID")
}

fn all_trainers_query() {
  query.query(
    "allTrainers",
    schema.non_null(schema.List(schema.non_null(schema.Named("Trainer")))),
    fn(_ctx) { Ok(data.all_trainers()) },
    fn(trainers) { types.to_dynamic(list.map(trainers, trainer_to_dynamic)) },
  )
  |> query.query_description("Get all trainers")
}

// ============================================================================
// Schema Builder
// ============================================================================

pub fn build_schema() -> schema.Schema {
  query.new()
  // Queries
  |> query.add_query(pokemon_query())
  |> query.add_query(pokemon_by_name_query())
  |> query.add_query(all_pokemon_query())
  |> query.add_query(pokemon_by_type_query())
  |> query.add_query(move_query())
  |> query.add_query(all_moves_query())
  |> query.add_query(trainer_query())
  |> query.add_query(all_trainers_query())
  // Types
  |> query.add_type(pokemon_object_type())
  |> query.add_type(stats_type())
  |> query.add_type(move_type())
  |> query.add_type(trainer_type())
  // Enums
  |> query.add_enum(pokemon_type_enum())
  |> query.add_enum(move_category_enum())
  |> query.build
}

// ============================================================================
// Argument Types and Decoders
// ============================================================================

pub type IdArgs {
  IdArgs(id: Int)
}

pub type NameArgs {
  NameArgs(name: String)
}

pub type TypeArgs {
  TypeArgs(pokemon_type: pokemon_types.PokemonType)
}

fn decode_id_args(args: Dict(String, Dynamic)) -> Result(IdArgs, String) {
  case dict.get(args, "id") {
    Ok(id_dyn) ->
      case decode.run(id_dyn, decode.int) {
        Ok(id) -> Ok(IdArgs(id: id))
        Error(_) -> Error("Invalid id argument")
      }
    Error(_) -> Error("Missing required argument: id")
  }
}

fn decode_name_args(args: Dict(String, Dynamic)) -> Result(NameArgs, String) {
  case dict.get(args, "name") {
    Ok(name_dyn) ->
      case decode.run(name_dyn, decode.string) {
        Ok(name) -> Ok(NameArgs(name: name))
        Error(_) -> Error("Invalid name argument")
      }
    Error(_) -> Error("Missing required argument: name")
  }
}

fn decode_type_args(args: Dict(String, Dynamic)) -> Result(TypeArgs, String) {
  case dict.get(args, "type") {
    Ok(type_dyn) ->
      case decode.run(type_dyn, decode.string) {
        Ok(type_str) ->
          case pokemon_types.string_to_type(type_str) {
            Ok(t) -> Ok(TypeArgs(pokemon_type: t))
            Error(e) -> Error(e)
          }
        Error(_) -> Error("Invalid type argument")
      }
    Error(_) -> Error("Missing required argument: type")
  }
}
