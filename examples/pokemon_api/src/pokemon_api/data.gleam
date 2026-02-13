// pokemon_api/data.gleam
// Sample Pokemon data

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import pokemon_api/types.{
  type Move, type Pokemon, type PokemonType, type Trainer, Electric, Fire,
  Flying, Grass, Move, Normal, Physical, Poison, Pokemon, Psychic, Special,
  Stats, Status, Trainer, Water,
}

// ============================================================================
// Pokemon Database
// ============================================================================

pub fn all_pokemon() -> List(Pokemon) {
  [
    // Gen 1 Starters
    Pokemon(
      id: 1,
      name: "Bulbasaur",
      pokedex_number: 1,
      pokemon_types: [Grass, Poison],
      stats: Stats(
        hp: 45,
        attack: 49,
        defense: 49,
        special_attack: 65,
        special_defense: 65,
        speed: 45,
      ),
      moves: [1, 2, 3],
      evolution_chain_id: Some(1),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/1.png",
      height: 0.7,
      weight: 6.9,
    ),
    Pokemon(
      id: 2,
      name: "Ivysaur",
      pokedex_number: 2,
      pokemon_types: [Grass, Poison],
      stats: Stats(
        hp: 60,
        attack: 62,
        defense: 63,
        special_attack: 80,
        special_defense: 80,
        speed: 60,
      ),
      moves: [1, 2, 3, 4],
      evolution_chain_id: Some(1),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/2.png",
      height: 1.0,
      weight: 13.0,
    ),
    Pokemon(
      id: 3,
      name: "Venusaur",
      pokedex_number: 3,
      pokemon_types: [Grass, Poison],
      stats: Stats(
        hp: 80,
        attack: 82,
        defense: 83,
        special_attack: 100,
        special_defense: 100,
        speed: 80,
      ),
      moves: [1, 2, 3, 4, 5],
      evolution_chain_id: Some(1),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/3.png",
      height: 2.0,
      weight: 100.0,
    ),
    Pokemon(
      id: 4,
      name: "Charmander",
      pokedex_number: 4,
      pokemon_types: [Fire],
      stats: Stats(
        hp: 39,
        attack: 52,
        defense: 43,
        special_attack: 60,
        special_defense: 50,
        speed: 65,
      ),
      moves: [6, 7, 8],
      evolution_chain_id: Some(2),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/4.png",
      height: 0.6,
      weight: 8.5,
    ),
    Pokemon(
      id: 5,
      name: "Charmeleon",
      pokedex_number: 5,
      pokemon_types: [Fire],
      stats: Stats(
        hp: 58,
        attack: 64,
        defense: 58,
        special_attack: 80,
        special_defense: 65,
        speed: 80,
      ),
      moves: [6, 7, 8, 9],
      evolution_chain_id: Some(2),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/5.png",
      height: 1.1,
      weight: 19.0,
    ),
    Pokemon(
      id: 6,
      name: "Charizard",
      pokedex_number: 6,
      pokemon_types: [Fire, Flying],
      stats: Stats(
        hp: 78,
        attack: 84,
        defense: 78,
        special_attack: 109,
        special_defense: 85,
        speed: 100,
      ),
      moves: [6, 7, 8, 9, 10],
      evolution_chain_id: Some(2),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/6.png",
      height: 1.7,
      weight: 90.5,
    ),
    Pokemon(
      id: 7,
      name: "Squirtle",
      pokedex_number: 7,
      pokemon_types: [Water],
      stats: Stats(
        hp: 44,
        attack: 48,
        defense: 65,
        special_attack: 50,
        special_defense: 64,
        speed: 43,
      ),
      moves: [11, 12, 13],
      evolution_chain_id: Some(3),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/7.png",
      height: 0.5,
      weight: 9.0,
    ),
    Pokemon(
      id: 8,
      name: "Wartortle",
      pokedex_number: 8,
      pokemon_types: [Water],
      stats: Stats(
        hp: 59,
        attack: 63,
        defense: 80,
        special_attack: 65,
        special_defense: 80,
        speed: 58,
      ),
      moves: [11, 12, 13, 14],
      evolution_chain_id: Some(3),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/8.png",
      height: 1.0,
      weight: 22.5,
    ),
    Pokemon(
      id: 9,
      name: "Blastoise",
      pokedex_number: 9,
      pokemon_types: [Water],
      stats: Stats(
        hp: 79,
        attack: 83,
        defense: 100,
        special_attack: 85,
        special_defense: 105,
        speed: 78,
      ),
      moves: [11, 12, 13, 14, 15],
      evolution_chain_id: Some(3),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/9.png",
      height: 1.6,
      weight: 85.5,
    ),
    Pokemon(
      id: 25,
      name: "Pikachu",
      pokedex_number: 25,
      pokemon_types: [Electric],
      stats: Stats(
        hp: 35,
        attack: 55,
        defense: 40,
        special_attack: 50,
        special_defense: 50,
        speed: 90,
      ),
      moves: [16, 17, 18, 19],
      evolution_chain_id: Some(4),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/25.png",
      height: 0.4,
      weight: 6.0,
    ),
    Pokemon(
      id: 26,
      name: "Raichu",
      pokedex_number: 26,
      pokemon_types: [Electric],
      stats: Stats(
        hp: 60,
        attack: 90,
        defense: 55,
        special_attack: 90,
        special_defense: 80,
        speed: 110,
      ),
      moves: [16, 17, 18, 19, 20],
      evolution_chain_id: Some(4),
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/26.png",
      height: 0.8,
      weight: 30.0,
    ),
    Pokemon(
      id: 150,
      name: "Mewtwo",
      pokedex_number: 150,
      pokemon_types: [Psychic],
      stats: Stats(
        hp: 106,
        attack: 110,
        defense: 90,
        special_attack: 154,
        special_defense: 90,
        speed: 130,
      ),
      moves: [21, 22, 23, 24],
      evolution_chain_id: None,
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/150.png",
      height: 2.0,
      weight: 122.0,
    ),
    Pokemon(
      id: 151,
      name: "Mew",
      pokedex_number: 151,
      pokemon_types: [Psychic],
      stats: Stats(
        hp: 100,
        attack: 100,
        defense: 100,
        special_attack: 100,
        special_defense: 100,
        speed: 100,
      ),
      moves: [21, 22, 25, 26],
      evolution_chain_id: None,
      sprite_url: "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/151.png",
      height: 0.4,
      weight: 4.0,
    ),
  ]
}

// ============================================================================
// Moves Database
// ============================================================================

pub fn all_moves() -> List(Move) {
  [
    // Grass moves
    Move(
      id: 1,
      name: "Tackle",
      move_type: Normal,
      power: Some(40),
      accuracy: Some(100),
      pp: 35,
      description: "A physical attack in which the user charges and slams into the target with its whole body.",
      category: Physical,
    ),
    Move(
      id: 2,
      name: "Vine Whip",
      move_type: Grass,
      power: Some(45),
      accuracy: Some(100),
      pp: 25,
      description: "The target is struck with slender, whiplike vines to inflict damage.",
      category: Physical,
    ),
    Move(
      id: 3,
      name: "Razor Leaf",
      move_type: Grass,
      power: Some(55),
      accuracy: Some(95),
      pp: 25,
      description: "Sharp-edged leaves are launched to slash at opposing Pokemon.",
      category: Physical,
    ),
    Move(
      id: 4,
      name: "Sleep Powder",
      move_type: Grass,
      power: None,
      accuracy: Some(75),
      pp: 15,
      description: "The user scatters a big cloud of sleep-inducing dust around the target.",
      category: Status,
    ),
    Move(
      id: 5,
      name: "Solar Beam",
      move_type: Grass,
      power: Some(120),
      accuracy: Some(100),
      pp: 10,
      description: "In this two-turn attack, the user gathers light, then blasts a bundled beam on the next turn.",
      category: Special,
    ),
    // Fire moves
    Move(
      id: 6,
      name: "Scratch",
      move_type: Normal,
      power: Some(40),
      accuracy: Some(100),
      pp: 35,
      description: "Hard, pointed, sharp claws rake the target to inflict damage.",
      category: Physical,
    ),
    Move(
      id: 7,
      name: "Ember",
      move_type: Fire,
      power: Some(40),
      accuracy: Some(100),
      pp: 25,
      description: "The target is attacked with small flames.",
      category: Special,
    ),
    Move(
      id: 8,
      name: "Flamethrower",
      move_type: Fire,
      power: Some(90),
      accuracy: Some(100),
      pp: 15,
      description: "The target is scorched with an intense blast of fire.",
      category: Special,
    ),
    Move(
      id: 9,
      name: "Fire Spin",
      move_type: Fire,
      power: Some(35),
      accuracy: Some(85),
      pp: 15,
      description: "The target becomes trapped within a fierce vortex of fire that rages for four to five turns.",
      category: Special,
    ),
    Move(
      id: 10,
      name: "Fire Blast",
      move_type: Fire,
      power: Some(110),
      accuracy: Some(85),
      pp: 5,
      description: "The target is attacked with an intense blast of all-consuming fire.",
      category: Special,
    ),
    // Water moves
    Move(
      id: 11,
      name: "Bubble",
      move_type: Water,
      power: Some(40),
      accuracy: Some(100),
      pp: 30,
      description: "A spray of countless bubbles is jetted at the opposing Pokemon.",
      category: Special,
    ),
    Move(
      id: 12,
      name: "Water Gun",
      move_type: Water,
      power: Some(40),
      accuracy: Some(100),
      pp: 25,
      description: "The target is blasted with a forceful shot of water.",
      category: Special,
    ),
    Move(
      id: 13,
      name: "Bite",
      move_type: types.Dark,
      power: Some(60),
      accuracy: Some(100),
      pp: 25,
      description: "The target is bitten with viciously sharp fangs.",
      category: Physical,
    ),
    Move(
      id: 14,
      name: "Hydro Pump",
      move_type: Water,
      power: Some(110),
      accuracy: Some(80),
      pp: 5,
      description: "The target is blasted by a huge volume of water launched under great pressure.",
      category: Special,
    ),
    Move(
      id: 15,
      name: "Skull Bash",
      move_type: Normal,
      power: Some(130),
      accuracy: Some(100),
      pp: 10,
      description: "The user tucks in its head to raise its Defense stat on the first turn, then rams the target on the next turn.",
      category: Physical,
    ),
    // Electric moves
    Move(
      id: 16,
      name: "Thunder Shock",
      move_type: Electric,
      power: Some(40),
      accuracy: Some(100),
      pp: 30,
      description: "A jolt of electricity crashes down on the target to inflict damage.",
      category: Special,
    ),
    Move(
      id: 17,
      name: "Quick Attack",
      move_type: Normal,
      power: Some(40),
      accuracy: Some(100),
      pp: 30,
      description: "The user lunges at the target at a speed that makes it almost invisible.",
      category: Physical,
    ),
    Move(
      id: 18,
      name: "Thunderbolt",
      move_type: Electric,
      power: Some(90),
      accuracy: Some(100),
      pp: 15,
      description: "A strong electric blast crashes down on the target.",
      category: Special,
    ),
    Move(
      id: 19,
      name: "Thunder",
      move_type: Electric,
      power: Some(110),
      accuracy: Some(70),
      pp: 10,
      description: "A wicked thunderbolt is dropped on the target to inflict damage.",
      category: Special,
    ),
    Move(
      id: 20,
      name: "Thunder Wave",
      move_type: Electric,
      power: None,
      accuracy: Some(90),
      pp: 20,
      description: "The user launches a weak jolt of electricity that paralyzes the target.",
      category: Status,
    ),
    // Psychic moves
    Move(
      id: 21,
      name: "Confusion",
      move_type: Psychic,
      power: Some(50),
      accuracy: Some(100),
      pp: 25,
      description: "The target is hit by a weak telekinetic force.",
      category: Special,
    ),
    Move(
      id: 22,
      name: "Psychic",
      move_type: Psychic,
      power: Some(90),
      accuracy: Some(100),
      pp: 10,
      description: "The target is hit by a strong telekinetic force.",
      category: Special,
    ),
    Move(
      id: 23,
      name: "Recover",
      move_type: Normal,
      power: None,
      accuracy: None,
      pp: 10,
      description: "Restoring its own cells, the user restores its own HP by half of its max HP.",
      category: Status,
    ),
    Move(
      id: 24,
      name: "Psystrike",
      move_type: Psychic,
      power: Some(100),
      accuracy: Some(100),
      pp: 10,
      description: "The user materializes an odd psychic wave to attack the target.",
      category: Special,
    ),
    Move(
      id: 25,
      name: "Transform",
      move_type: Normal,
      power: None,
      accuracy: None,
      pp: 10,
      description: "The user transforms into a copy of the target right down to having the same move set.",
      category: Status,
    ),
    Move(
      id: 26,
      name: "Metronome",
      move_type: Normal,
      power: None,
      accuracy: None,
      pp: 10,
      description: "The user waggles a finger and stimulates its brain into randomly using nearly any move.",
      category: Status,
    ),
  ]
}

// ============================================================================
// Trainers Database
// ============================================================================

pub fn all_trainers() -> List(Trainer) {
  [
    Trainer(
      id: 1,
      name: "Red",
      team: [25, 3, 6, 9, 150],
      badges: 8,
      pokedex_caught: 151,
    ),
    Trainer(
      id: 2,
      name: "Blue",
      team: [26, 6, 9, 3],
      badges: 8,
      pokedex_caught: 145,
    ),
    Trainer(
      id: 3,
      name: "Ash Ketchum",
      team: [25, 6, 7, 1],
      badges: 8,
      pokedex_caught: 80,
    ),
    Trainer(
      id: 4,
      name: "Misty",
      team: [7, 8, 9],
      badges: 0,
      pokedex_caught: 30,
    ),
    Trainer(id: 5, name: "Brock", team: [], badges: 0, pokedex_caught: 25),
  ]
}

// ============================================================================
// Lookup Functions
// ============================================================================

pub fn pokemon_by_id() -> Dict(Int, Pokemon) {
  all_pokemon()
  |> list.map(fn(p) { #(p.id, p) })
  |> dict.from_list
}

pub fn pokemon_by_name() -> Dict(String, Pokemon) {
  all_pokemon()
  |> list.map(fn(p) { #(p.name, p) })
  |> dict.from_list
}

pub fn moves_by_id() -> Dict(Int, Move) {
  all_moves()
  |> list.map(fn(m) { #(m.id, m) })
  |> dict.from_list
}

pub fn trainers_by_id() -> Dict(Int, Trainer) {
  all_trainers()
  |> list.map(fn(t) { #(t.id, t) })
  |> dict.from_list
}

pub fn find_pokemon(id: Int) -> Result(Pokemon, String) {
  case dict.get(pokemon_by_id(), id) {
    Ok(p) -> Ok(p)
    Error(_) -> Error("Pokemon not found: " <> int_to_string(id))
  }
}

pub fn find_pokemon_by_name(name: String) -> Result(Pokemon, String) {
  case dict.get(pokemon_by_name(), name) {
    Ok(p) -> Ok(p)
    Error(_) -> Error("Pokemon not found: " <> name)
  }
}

pub fn find_move(id: Int) -> Result(Move, String) {
  case dict.get(moves_by_id(), id) {
    Ok(m) -> Ok(m)
    Error(_) -> Error("Move not found: " <> int_to_string(id))
  }
}

pub fn find_trainer(id: Int) -> Result(Trainer, String) {
  case dict.get(trainers_by_id(), id) {
    Ok(t) -> Ok(t)
    Error(_) -> Error("Trainer not found: " <> int_to_string(id))
  }
}

pub fn pokemon_by_type(t: PokemonType) -> List(Pokemon) {
  all_pokemon()
  |> list.filter(fn(p) { list.contains(p.pokemon_types, t) })
}

import gleam/int

fn int_to_string(i: Int) -> String {
  int.to_string(i)
}
