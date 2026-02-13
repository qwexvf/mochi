// pokemon_api.gleam
// Pokemon GraphQL API with Mochi + Wisp
//
// Commands:
//   gleam run                      Start the server
//   gleam run -- codegen           Generate types.ts and schema.graphql
//   gleam run -- codegen -o ./gen  Output to ./gen directory
//
// Test with: curl -X POST http://localhost:8000/graphql \
//   -H "Content-Type: application/json" \
//   -d '{"query": "{ pokemon(id: 25) { name types stats { hp attack speed } } }"}'

import argv
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{None}
import gleam/string
import logging
import mist
import mochi/codegen/sdl
import mochi/codegen/typescript
import mochi/executor
import mochi/parser
import mochi/playground
import mochi/response
import mochi/schema
import pokemon_api/loaders
import pokemon_api/schema as pokemon_schema
import simplifile
import wisp.{type Request, type Response}
import wisp/wisp_mist

const port = 8000

pub fn main() {
  let args = argv.load().arguments

  case args {
    ["gen", "types"] | ["gen", "ts"] -> gen_types("types.ts")
    ["gen", "types", "-o", path] | ["gen", "ts", "-o", path] -> gen_types(path)
    ["gen", "sdl"] | ["gen", "schema"] -> gen_sdl("schema.graphql")
    ["gen", "sdl", "-o", path] | ["gen", "schema", "-o", path] -> gen_sdl(path)
    ["gen", "all"] | ["gen"] | ["codegen"] -> run_codegen([])
    ["codegen", ..rest] -> run_codegen(rest)
    ["--help"] | ["-h"] | ["help"] -> print_help()
    _ -> run_server()
  }
}

fn gen_types(path: String) {
  let schema = pokemon_schema.build_schema()
  let content = typescript.generate(schema)
  case simplifile.write(path, content) {
    Ok(_) -> io.println("Created: " <> path)
    Error(e) -> io.println("Error: " <> string.inspect(e))
  }
}

fn gen_sdl(path: String) {
  let schema = pokemon_schema.build_schema()
  let content = sdl.generate(schema)
  case simplifile.write(path, content) {
    Ok(_) -> io.println("Created: " <> path)
    Error(e) -> io.println("Error: " <> string.inspect(e))
  }
}

fn print_help() {
  io.println("Pokemon GraphQL API - Mochi + Wisp")
  io.println("")
  io.println("Usage:")
  io.println("  gleam run                    Start the server")
  io.println("  gleam run -- gen types       Generate types.ts")
  io.println("  gleam run -- gen sdl         Generate schema.graphql")
  io.println("  gleam run -- gen all         Generate both files")
  io.println("  gleam run -- help            Show this help")
  io.println("")
  io.println("Options:")
  io.println("  -o <file>                    Output file path")
  io.println("")
  io.println("Examples:")
  io.println("  gleam run -- gen types")
  io.println("  gleam run -- gen types -o ./generated/schema.d.ts")
  io.println("  gleam run -- gen sdl -o ./generated/schema.graphql")
  io.println("  gleam run -- gen all")
}

fn run_codegen(args: List(String)) {
  let schema = pokemon_schema.build_schema()
  let #(output_dir, ts_only, sdl_only) =
    parse_codegen_args(args, ".", False, False)

  io.println("Generating code...")

  case ts_only, sdl_only {
    True, _ -> {
      let path = output_dir <> "/types.ts"
      let content = typescript.generate(schema)
      case simplifile.write(path, content) {
        Ok(_) -> io.println("  Created: " <> path)
        Error(e) ->
          io.println("  Error writing " <> path <> ": " <> string.inspect(e))
      }
    }
    _, True -> {
      let path = output_dir <> "/schema.graphql"
      let content = sdl.generate(schema)
      case simplifile.write(path, content) {
        Ok(_) -> io.println("  Created: " <> path)
        Error(e) ->
          io.println("  Error writing " <> path <> ": " <> string.inspect(e))
      }
    }
    False, False -> {
      let ts_path = output_dir <> "/types.ts"
      let sdl_path = output_dir <> "/schema.graphql"

      case simplifile.write(ts_path, typescript.generate(schema)) {
        Ok(_) -> io.println("  Created: " <> ts_path)
        Error(e) -> io.println("  Error: " <> string.inspect(e))
      }

      case simplifile.write(sdl_path, sdl.generate(schema)) {
        Ok(_) -> io.println("  Created: " <> sdl_path)
        Error(e) -> io.println("  Error: " <> string.inspect(e))
      }
    }
  }

  io.println("Done!")
}

fn parse_codegen_args(
  args: List(String),
  output_dir: String,
  ts_only: Bool,
  sdl_only: Bool,
) -> #(String, Bool, Bool) {
  case args {
    [] -> #(output_dir, ts_only, sdl_only)
    ["-o", dir, ..rest] | ["--output", dir, ..rest] ->
      parse_codegen_args(rest, dir, ts_only, sdl_only)
    ["--ts-only", ..rest] ->
      parse_codegen_args(rest, output_dir, True, sdl_only)
    ["--sdl-only", ..rest] ->
      parse_codegen_args(rest, output_dir, ts_only, True)
    [_, ..rest] -> parse_codegen_args(rest, output_dir, ts_only, sdl_only)
  }
}

fn run_server() {
  wisp.configure_logger()
  logging.set_level(logging.Info)

  io.println("")
  io.println("========================================")
  io.println("  Pokemon GraphQL API")
  io.println("  Powered by Mochi + Wisp")
  io.println("========================================")
  io.println("")

  // Build the schema once at startup
  let gql_schema = pokemon_schema.build_schema()

  // Create handler with schema captured in closure
  let handler = fn(req: Request) -> Response { handle_request(req, gql_schema) }

  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start

  io.println("Server running at http://localhost:" <> int.to_string(port))
  io.println("")
  io.println("Endpoints:")
  io.println("  POST /graphql     - Execute GraphQL queries")
  io.println("  GET /graphiql     - GraphiQL IDE")
  io.println("  GET /playground   - GraphQL Playground")
  io.println("  GET /schema.graphql - Download SDL")
  io.println("  GET /types.ts     - Download TypeScript types")
  io.println("")
  io.println("Example queries:")
  io.println("  { allPokemon { name types } }")
  io.println(
    "  { pokemon(id: 25) { name stats { hp attack speed } moves { name power } } }",
  )
  io.println("  { pokemonByType(type: FIRE) { name pokedexNumber } }")
  io.println("  { trainer(id: 1) { name team { name types } badges } }")
  io.println("")
  io.println("Generate types: gleam run -- codegen")
  io.println("")

  process.sleep_forever()
}

fn handle_request(req: Request, gql_schema: schema.Schema) -> Response {
  let path = "/" <> string.join(wisp.path_segments(req), "/")
  logging.log(logging.Info, http.method_to_string(req.method) <> " " <> path)

  case wisp.path_segments(req) {
    ["graphql"] -> handle_graphql(req, gql_schema)
    ["graphiql"] -> wisp.html_response(playground.graphiql("/graphql"), 200)
    ["playground"] -> wisp.html_response(playground.playground("/graphql"), 200)
    ["explorer"] ->
      wisp.html_response(playground.simple_explorer("/graphql"), 200)
    ["schema.graphql"] -> {
      let sdl_content = sdl.generate(gql_schema)
      wisp.response(200)
      |> wisp.set_header("content-type", "text/plain")
      |> wisp.set_body(wisp.Text(sdl_content))
    }
    ["types.ts"] -> {
      let ts_content = typescript.generate(gql_schema)
      wisp.response(200)
      |> wisp.set_header("content-type", "text/plain")
      |> wisp.set_body(wisp.Text(ts_content))
    }
    [] -> home_page()
    _ -> wisp.not_found()
  }
}

fn handle_graphql(req: Request, gql_schema: schema.Schema) -> Response {
  case req.method {
    http.Post -> {
      use body <- wisp.require_string_body(req)

      case parse_graphql_request(body) {
        Ok(#(query, variables)) -> {
          // Create execution context with DataLoaders for batching
          let ctx = loaders.create_context()

          case parser.parse(query) {
            Ok(document) -> {
              let result =
                executor.execute(gql_schema, document, None, ctx, variables)
              let resp = response.from_execution_result(result)
              let json_body = response.to_json(resp)
              wisp.json_response(json_body, 200)
              |> wisp.set_header("access-control-allow-origin", "*")
            }
            Error(_parse_error) -> {
              let error_json =
                json.to_string(
                  json.object([
                    #(
                      "errors",
                      json.array(
                        [
                          json.object([#("message", json.string("Parse error"))]),
                        ],
                        fn(x) { x },
                      ),
                    ),
                  ]),
                )
              wisp.json_response(error_json, 400)
            }
          }
        }
        Error(msg) -> {
          let error_json =
            json.to_string(
              json.object([
                #(
                  "errors",
                  json.array(
                    [json.object([#("message", json.string(msg))])],
                    fn(x) { x },
                  ),
                ),
              ]),
            )
          wisp.json_response(error_json, 400)
        }
      }
    }
    http.Options -> {
      wisp.response(200)
      |> wisp.set_header("access-control-allow-origin", "*")
      |> wisp.set_header("access-control-allow-methods", "POST, OPTIONS")
      |> wisp.set_header("access-control-allow-headers", "content-type")
    }
    _ -> wisp.method_not_allowed([http.Post, http.Options])
  }
}

fn parse_graphql_request(
  body: String,
) -> Result(#(String, dict.Dict(String, Dynamic)), String) {
  case json.parse(body, decode.dict(decode.string, decode.dynamic)) {
    Ok(parsed) -> {
      case dict.get(parsed, "query") {
        Ok(query_dyn) -> {
          case decode.run(query_dyn, decode.string) {
            Ok(query) -> {
              let variables = case dict.get(parsed, "variables") {
                Ok(vars_dyn) -> {
                  case
                    decode.run(
                      vars_dyn,
                      decode.dict(decode.string, decode.dynamic),
                    )
                  {
                    Ok(vars) -> vars
                    Error(_) -> dict.new()
                  }
                }
                Error(_) -> dict.new()
              }
              Ok(#(query, variables))
            }
            Error(_) -> Error("Invalid query field")
          }
        }
        Error(_) -> Error("Missing query field")
      }
    }
    Error(_) -> Error("Invalid JSON body")
  }
}

fn home_page() -> Response {
  wisp.html_response(
    "<!DOCTYPE html>
<html>
<head>
  <title>Pokemon GraphQL API</title>
  <style>
    body { font-family: -apple-system, sans-serif; max-width: 900px; margin: 2rem auto; padding: 0 1rem; background: #1a1a2e; color: #eee; }
    h1 { color: #ffcb05; text-shadow: 2px 2px #3466af; }
    h2 { color: #3466af; margin-top: 2rem; }
    a { color: #ffcb05; }
    code { background: #16213e; padding: 0.2rem 0.5rem; border-radius: 3px; color: #fff; }
    pre { background: #16213e; padding: 1rem; border-radius: 8px; overflow-x: auto; border-left: 4px solid #ffcb05; }
    .pokemon-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(80px, 1fr)); gap: 0.5rem; margin: 1rem 0; }
    .pokemon-sprite { text-align: center; }
    .pokemon-sprite img { width: 64px; height: 64px; image-rendering: pixelated; }
    .endpoints { background: #16213e; padding: 1rem; border-radius: 8px; }
    .endpoint { margin: 0.5rem 0; }
  </style>
</head>
<body>
  <h1>Pokemon GraphQL API</h1>
  <p>A GraphQL API for Pokemon data, built with <strong>Mochi</strong> and <strong>Wisp</strong>.</p>

  <div class='pokemon-grid'>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/1.png' alt='Bulbasaur'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/4.png' alt='Charmander'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/7.png' alt='Squirtle'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/25.png' alt='Pikachu'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/6.png' alt='Charizard'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/150.png' alt='Mewtwo'></div>
    <div class='pokemon-sprite'><img src='https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/151.png' alt='Mew'></div>
  </div>

  <h2>Interactive Playgrounds</h2>
  <div class='endpoints'>
    <div class='endpoint'><a href='/graphiql'>GraphiQL</a> - The classic GraphQL IDE</div>
    <div class='endpoint'><a href='/playground'>GraphQL Playground</a> - Alternative IDE</div>
    <div class='endpoint'><a href='/explorer'>Simple Explorer</a> - Lightweight query tool</div>
  </div>

  <h2>Schema Downloads</h2>
  <div class='endpoints'>
    <div class='endpoint'><a href='/schema.graphql'>schema.graphql</a> - GraphQL SDL</div>
    <div class='endpoint'><a href='/types.ts'>types.ts</a> - TypeScript type definitions</div>
  </div>

  <h2>Example Queries</h2>
  <pre>
# Get Pikachu with stats and moves
{
  pokemon(id: 25) {
    name
    types
    spriteUrl
    stats {
      hp
      attack
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
  </pre>

  <pre>
# Get all Fire type Pokemon
{
  pokemonByType(type: FIRE) {
    name
    pokedexNumber
    stats { total }
  }
}
  </pre>

  <pre>
# Get trainer Red's team
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
  </pre>

  <h2>cURL Example</h2>
  <pre>
curl -X POST http://localhost:8000/graphql \\
  -H 'Content-Type: application/json' \\
  -d '{\"query\": \"{ pokemon(id: 25) { name types } }\"}'
  </pre>
</body>
</html>",
    200,
  )
}
