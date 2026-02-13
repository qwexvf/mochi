// pokemon_api/schema.gleam
// GraphQL schema for the Pokemon API - No FFI version

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/query
import mochi/schema
import mochi/types
import pokemon_api/data
import pokemon_api/types as pokemon_types

// ============================================================================
// GraphQL Type Definitions
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

fn stats_type() -> schema.ObjectType {
  schema.object("Stats")
  |> schema.description("Base stats of a Pokemon")
  |> schema.field(
    schema.field_def("hp", schema.non_null(schema.int_type()))
    |> schema.field_description("Hit Points")
    |> schema.resolver(fn(info) { get_field(info.parent, "hp") }),
  )
  |> schema.field(
    schema.field_def("attack", schema.non_null(schema.int_type()))
    |> schema.field_description("Physical attack power")
    |> schema.resolver(fn(info) { get_field(info.parent, "attack") }),
  )
  |> schema.field(
    schema.field_def("defense", schema.non_null(schema.int_type()))
    |> schema.field_description("Physical defense")
    |> schema.resolver(fn(info) { get_field(info.parent, "defense") }),
  )
  |> schema.field(
    schema.field_def("specialAttack", schema.non_null(schema.int_type()))
    |> schema.field_description("Special attack power")
    |> schema.resolver(fn(info) { get_field(info.parent, "special_attack") }),
  )
  |> schema.field(
    schema.field_def("specialDefense", schema.non_null(schema.int_type()))
    |> schema.field_description("Special defense")
    |> schema.resolver(fn(info) { get_field(info.parent, "special_defense") }),
  )
  |> schema.field(
    schema.field_def("speed", schema.non_null(schema.int_type()))
    |> schema.field_description("Speed determines turn order")
    |> schema.resolver(fn(info) { get_field(info.parent, "speed") }),
  )
  |> schema.field(
    schema.field_def("total", schema.non_null(schema.int_type()))
    |> schema.field_description("Total of all base stats")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) -> {
          // Decode parent as dict, then sum up stats
          case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
            Ok(d) -> {
              let get_int = fn(key) {
                case dict.get(d, key) {
                  Ok(v) ->
                    case decode.run(v, decode.int) {
                      Ok(i) -> i
                      Error(_) -> 0
                    }
                  Error(_) -> 0
                }
              }
              let total =
                get_int("hp")
                + get_int("attack")
                + get_int("defense")
                + get_int("special_attack")
                + get_int("special_defense")
                + get_int("speed")
              Ok(types.to_dynamic(total))
            }
            Error(_) -> Error("Failed to calculate total")
          }
        }
        None -> Error("No parent")
      }
    }),
  )
}

fn move_type() -> schema.ObjectType {
  schema.object("Move")
  |> schema.description("A move that a Pokemon can learn")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.int_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "id") }),
  )
  |> schema.field(
    schema.field_def("name", schema.non_null(schema.string_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "name") }),
  )
  |> schema.field(
    schema.field_def("type", schema.non_null(schema.Named("PokemonType")))
    |> schema.field_description("The elemental type of this move")
    |> schema.resolver(fn(info) { get_field(info.parent, "move_type") }),
  )
  |> schema.field(
    schema.field_def("power", schema.int_type())
    |> schema.field_description("Base power (null for status moves)")
    |> schema.resolver(fn(info) { get_optional_field(info.parent, "power") }),
  )
  |> schema.field(
    schema.field_def("accuracy", schema.int_type())
    |> schema.field_description("Accuracy percentage")
    |> schema.resolver(fn(info) { get_optional_field(info.parent, "accuracy") }),
  )
  |> schema.field(
    schema.field_def("pp", schema.non_null(schema.int_type()))
    |> schema.field_description("Power Points")
    |> schema.resolver(fn(info) { get_field(info.parent, "pp") }),
  )
  |> schema.field(
    schema.field_def("description", schema.non_null(schema.string_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "description") }),
  )
  |> schema.field(
    schema.field_def("category", schema.non_null(schema.Named("MoveCategory")))
    |> schema.resolver(fn(info) { get_field(info.parent, "category") }),
  )
}

fn pokemon_object_type() -> schema.ObjectType {
  schema.object("Pokemon")
  |> schema.description("A Pokemon creature")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.int_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "id") }),
  )
  |> schema.field(
    schema.field_def("name", schema.non_null(schema.string_type()))
    |> schema.field_description("The name of this Pokemon")
    |> schema.resolver(fn(info) { get_field(info.parent, "name") }),
  )
  |> schema.field(
    schema.field_def("pokedexNumber", schema.non_null(schema.int_type()))
    |> schema.field_description("National Pokedex number")
    |> schema.resolver(fn(info) { get_field(info.parent, "pokedex_number") }),
  )
  |> schema.field(
    schema.field_def(
      "types",
      schema.non_null(schema.List(schema.non_null(schema.Named("PokemonType")))),
    )
    |> schema.field_description("The elemental type(s) of this Pokemon")
    |> schema.resolver(fn(info) { get_field(info.parent, "pokemon_types") }),
  )
  |> schema.field(
    schema.field_def("stats", schema.non_null(schema.Named("Stats")))
    |> schema.field_description("Base stats")
    |> schema.resolver(fn(info) { get_field(info.parent, "stats") }),
  )
  |> schema.field(
    schema.field_def(
      "moves",
      schema.non_null(schema.List(schema.non_null(schema.Named("Move")))),
    )
    |> schema.field_description("Moves this Pokemon can learn")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) -> {
          case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
            Ok(d) -> {
              case dict.get(d, "moves") {
                Ok(moves_dyn) -> {
                  case decode.run(moves_dyn, decode.list(decode.int)) {
                    Ok(move_ids) -> {
                      let moves =
                        list.filter_map(move_ids, fn(id) {
                          case data.find_move(id) {
                            Ok(m) -> Ok(move_to_dynamic(m))
                            Error(_) -> Error(Nil)
                          }
                        })
                      Ok(types.to_dynamic(moves))
                    }
                    Error(_) -> Ok(types.to_dynamic([]))
                  }
                }
                Error(_) -> Ok(types.to_dynamic([]))
              }
            }
            Error(_) -> Ok(types.to_dynamic([]))
          }
        }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def("spriteUrl", schema.non_null(schema.string_type()))
    |> schema.field_description("URL to the Pokemon's sprite image")
    |> schema.resolver(fn(info) { get_field(info.parent, "sprite_url") }),
  )
  |> schema.field(
    schema.field_def("height", schema.non_null(schema.float_type()))
    |> schema.field_description("Height in meters")
    |> schema.resolver(fn(info) { get_field(info.parent, "height") }),
  )
  |> schema.field(
    schema.field_def("weight", schema.non_null(schema.float_type()))
    |> schema.field_description("Weight in kilograms")
    |> schema.resolver(fn(info) { get_field(info.parent, "weight") }),
  )
  |> schema.field(
    schema.field_def("evolvesTo", schema.Named("Pokemon"))
    |> schema.field_description("The Pokemon this one evolves into")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) -> {
          case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
            Ok(d) -> {
              case dict.get(d, "id") {
                Ok(id_dyn) -> {
                  case decode.run(id_dyn, decode.int) {
                    Ok(id) -> {
                      // Simple evolution chain lookup
                      let evolution = case id {
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
                    Error(_) -> Ok(types.to_dynamic(Nil))
                  }
                }
                Error(_) -> Ok(types.to_dynamic(Nil))
              }
            }
            Error(_) -> Ok(types.to_dynamic(Nil))
          }
        }
        None -> Error("No parent")
      }
    }),
  )
}

fn trainer_type() -> schema.ObjectType {
  schema.object("Trainer")
  |> schema.description("A Pokemon trainer")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.int_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "id") }),
  )
  |> schema.field(
    schema.field_def("name", schema.non_null(schema.string_type()))
    |> schema.resolver(fn(info) { get_field(info.parent, "name") }),
  )
  |> schema.field(
    schema.field_def(
      "team",
      schema.non_null(schema.List(schema.non_null(schema.Named("Pokemon")))),
    )
    |> schema.field_description("The trainer's Pokemon team")
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) -> {
          case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
            Ok(d) -> {
              case dict.get(d, "team") {
                Ok(team_dyn) -> {
                  case decode.run(team_dyn, decode.list(decode.int)) {
                    Ok(pokemon_ids) -> {
                      let pokemon =
                        list.filter_map(pokemon_ids, fn(id) {
                          case data.find_pokemon(id) {
                            Ok(pkmn) -> Ok(pokemon_to_dynamic(pkmn))
                            Error(_) -> Error(Nil)
                          }
                        })
                      Ok(types.to_dynamic(pokemon))
                    }
                    Error(_) -> Ok(types.to_dynamic([]))
                  }
                }
                Error(_) -> Ok(types.to_dynamic([]))
              }
            }
            Error(_) -> Ok(types.to_dynamic([]))
          }
        }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def("badges", schema.non_null(schema.int_type()))
    |> schema.field_description("Number of gym badges earned")
    |> schema.resolver(fn(info) { get_field(info.parent, "badges") }),
  )
  |> schema.field(
    schema.field_def("pokedexCaught", schema.non_null(schema.int_type()))
    |> schema.field_description("Number of Pokemon caught")
    |> schema.resolver(fn(info) { get_field(info.parent, "pokedex_caught") }),
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

// ============================================================================
// Encoders
// ============================================================================

pub fn pokemon_to_dynamic(p: pokemon_types.Pokemon) -> Dynamic {
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

pub fn move_to_dynamic(m: pokemon_types.Move) -> Dynamic {
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

pub fn trainer_to_dynamic(t: pokemon_types.Trainer) -> Dynamic {
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

fn option_to_dynamic(opt: Option(Int)) -> Dynamic {
  case opt {
    Some(v) -> types.to_dynamic(v)
    None -> types.to_dynamic(Nil)
  }
}

// ============================================================================
// Field Extraction Helpers (No FFI - uses Gleam decoders)
// ============================================================================

fn get_field(parent: Option(Dynamic), field: String) -> Result(Dynamic, String) {
  case parent {
    Some(p) -> {
      // Decode as a dict, then look up the field
      case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
        Ok(d) -> {
          case dict.get(d, field) {
            Ok(value) -> Ok(value)
            Error(_) -> Error("Field not found: " <> field)
          }
        }
        Error(_) -> Error("Failed to decode as dict: " <> field)
      }
    }
    None -> Error("No parent value")
  }
}

fn get_optional_field(
  parent: Option(Dynamic),
  field: String,
) -> Result(Dynamic, String) {
  case parent {
    Some(p) -> {
      case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
        Ok(d) -> {
          case dict.get(d, field) {
            Ok(value) -> Ok(value)
            Error(_) -> Ok(types.to_dynamic(Nil))
          }
        }
        Error(_) -> Ok(types.to_dynamic(Nil))
      }
    }
    None -> Error("No parent value")
  }
}
