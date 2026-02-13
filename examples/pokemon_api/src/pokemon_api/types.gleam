// pokemon_api/types.gleam
// Domain types for the Pokemon API

import gleam/option.{type Option}

// ============================================================================
// Pokemon Types
// ============================================================================

pub type Pokemon {
  Pokemon(
    id: Int,
    name: String,
    pokedex_number: Int,
    pokemon_types: List(PokemonType),
    stats: Stats,
    moves: List(Int),
    // Move IDs
    evolution_chain_id: Option(Int),
    sprite_url: String,
    height: Float,
    // in meters
    weight: Float,
    // in kg
  )
}

pub type PokemonType {
  Normal
  Fire
  Water
  Electric
  Grass
  Ice
  Fighting
  Poison
  Ground
  Flying
  Psychic
  Bug
  Rock
  Ghost
  Dragon
  Dark
  Steel
  Fairy
}

pub type Stats {
  Stats(
    hp: Int,
    attack: Int,
    defense: Int,
    special_attack: Int,
    special_defense: Int,
    speed: Int,
  )
}

pub type Move {
  Move(
    id: Int,
    name: String,
    move_type: PokemonType,
    power: Option(Int),
    accuracy: Option(Int),
    pp: Int,
    description: String,
    category: MoveCategory,
  )
}

pub type MoveCategory {
  Physical
  Special
  Status
}

pub type Trainer {
  Trainer(
    id: Int,
    name: String,
    team: List(Int),
    // Pokemon IDs
    badges: Int,
    pokedex_caught: Int,
  )
}

pub type Evolution {
  Evolution(
    id: Int,
    pokemon_id: Int,
    evolves_to_id: Option(Int),
    evolution_trigger: String,
    min_level: Option(Int),
  )
}

pub type Battle {
  Battle(
    id: Int,
    trainer1_id: Int,
    trainer2_id: Int,
    winner_id: Option(Int),
    pokemon1_id: Int,
    pokemon2_id: Int,
    log: List(String),
  )
}

// ============================================================================
// Helper Functions
// ============================================================================

pub fn type_to_string(t: PokemonType) -> String {
  case t {
    Normal -> "NORMAL"
    Fire -> "FIRE"
    Water -> "WATER"
    Electric -> "ELECTRIC"
    Grass -> "GRASS"
    Ice -> "ICE"
    Fighting -> "FIGHTING"
    Poison -> "POISON"
    Ground -> "GROUND"
    Flying -> "FLYING"
    Psychic -> "PSYCHIC"
    Bug -> "BUG"
    Rock -> "ROCK"
    Ghost -> "GHOST"
    Dragon -> "DRAGON"
    Dark -> "DARK"
    Steel -> "STEEL"
    Fairy -> "FAIRY"
  }
}

pub fn string_to_type(s: String) -> Result(PokemonType, String) {
  case s {
    "NORMAL" -> Ok(Normal)
    "FIRE" -> Ok(Fire)
    "WATER" -> Ok(Water)
    "ELECTRIC" -> Ok(Electric)
    "GRASS" -> Ok(Grass)
    "ICE" -> Ok(Ice)
    "FIGHTING" -> Ok(Fighting)
    "POISON" -> Ok(Poison)
    "GROUND" -> Ok(Ground)
    "FLYING" -> Ok(Flying)
    "PSYCHIC" -> Ok(Psychic)
    "BUG" -> Ok(Bug)
    "ROCK" -> Ok(Rock)
    "GHOST" -> Ok(Ghost)
    "DRAGON" -> Ok(Dragon)
    "DARK" -> Ok(Dark)
    "STEEL" -> Ok(Steel)
    "FAIRY" -> Ok(Fairy)
    _ -> Error("Unknown Pokemon type: " <> s)
  }
}

pub fn category_to_string(c: MoveCategory) -> String {
  case c {
    Physical -> "PHYSICAL"
    Special -> "SPECIAL"
    Status -> "STATUS"
  }
}

pub fn string_to_category(s: String) -> Result(MoveCategory, String) {
  case s {
    "PHYSICAL" -> Ok(Physical)
    "SPECIAL" -> Ok(Special)
    "STATUS" -> Ok(Status)
    _ -> Error("Unknown move category: " <> s)
  }
}
