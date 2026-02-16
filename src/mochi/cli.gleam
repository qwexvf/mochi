// mochi/cli.gleam
// CLI for generating code from GraphQL schemas
//
// Usage:
//   gleam run -m mochi/cli -- <schema.graphql> [options]
//
// Options:
//   --typescript, -t <file>   Generate TypeScript types
//   --gleam, -g <file>        Generate Gleam types
//   --resolvers, -r <file>    Generate resolver stubs
//   --sdl, -s <file>          Generate SDL (normalized)
//   --all, -a <prefix>        Generate all files with prefix
//   --help, -h                Show help

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import mochi/codegen/gleam as gleam_gen
import mochi/codegen/sdl
import mochi/codegen/typescript
import mochi/schema.{type Schema}
import mochi/sdl_ast.{type SDLDocument}
import mochi/sdl_parser
import simplifile

/// CLI configuration
pub type CliConfig {
  CliConfig(
    schema_path: String,
    typescript_output: Result(String, Nil),
    gleam_output: Result(String, Nil),
    resolvers_output: Result(String, Nil),
    sdl_output: Result(String, Nil),
  )
}

/// CLI errors
pub type CliError {
  NoSchemaFile
  FileReadError(path: String, reason: String)
  ParseError(message: String)
  WriteError(path: String, reason: String)
  InvalidArgs(message: String)
}

/// Main entry point
pub fn main() {
  case run() {
    Ok(msg) -> io.println(msg)
    Error(err) -> {
      io.println_error(format_error(err))
      halt(1)
    }
  }
}

/// Run CLI with command line arguments
pub fn run() -> Result(String, CliError) {
  let args = get_args()
  run_with_args(args)
}

/// Run CLI with provided arguments
pub fn run_with_args(args: List(String)) -> Result(String, CliError) {
  use config <- result.try(parse_args(args))
  use doc <- result.try(read_and_parse_schema(config.schema_path))

  let gleam_config = gleam_gen.default_config()

  // Generate all requested outputs
  use messages <- result.try(generate_outputs(config, doc, gleam_config))

  case messages {
    [] -> Ok("No output files specified. Use --help for usage.")
    _ -> Ok(string.join(list.reverse(messages), "\n"))
  }
}

fn generate_outputs(
  config: CliConfig,
  doc: SDLDocument,
  gleam_config: gleam_gen.GleamGenConfig,
) -> Result(List(String), CliError) {
  let messages = []

  // Generate TypeScript
  use messages <- result.try(case config.typescript_output {
    Ok(path) -> {
      let content = generate_typescript_from_sdl(doc)
      use _ <- result.try(write_file(path, content))
      Ok(["Generated TypeScript: " <> path, ..messages])
    }
    Error(_) -> Ok(messages)
  })

  // Generate Gleam types
  use messages <- result.try(case config.gleam_output {
    Ok(path) -> {
      let content = gleam_gen.generate_types(doc, gleam_config)
      use _ <- result.try(write_file(path, content))
      Ok(["Generated Gleam types: " <> path, ..messages])
    }
    Error(_) -> Ok(messages)
  })

  // Generate resolver stubs
  use messages <- result.try(case config.resolvers_output {
    Ok(path) -> {
      let content = gleam_gen.generate_resolvers(doc, gleam_config)
      use _ <- result.try(write_file(path, content))
      Ok(["Generated resolvers: " <> path, ..messages])
    }
    Error(_) -> Ok(messages)
  })

  // Generate SDL
  use messages <- result.try(case config.sdl_output {
    Ok(path) -> {
      let content = generate_sdl_from_sdl(doc)
      use _ <- result.try(write_file(path, content))
      Ok(["Generated SDL: " <> path, ..messages])
    }
    Error(_) -> Ok(messages)
  })

  Ok(messages)
}

/// Parse command line arguments
fn parse_args(args: List(String)) -> Result(CliConfig, CliError) {
  case args {
    [] -> Error(InvalidArgs(help_text()))
    ["--help"] | ["-h"] -> Error(InvalidArgs(help_text()))
    [schema_path, ..rest] -> parse_options(schema_path, rest)
  }
}

fn parse_options(
  schema_path: String,
  options: List(String),
) -> Result(CliConfig, CliError) {
  let config =
    CliConfig(
      schema_path: schema_path,
      typescript_output: Error(Nil),
      gleam_output: Error(Nil),
      resolvers_output: Error(Nil),
      sdl_output: Error(Nil),
    )

  parse_options_loop(config, options)
}

fn parse_options_loop(
  config: CliConfig,
  options: List(String),
) -> Result(CliConfig, CliError) {
  case options {
    [] -> Ok(config)

    ["--typescript", path, ..rest] | ["-t", path, ..rest] ->
      parse_options_loop(CliConfig(..config, typescript_output: Ok(path)), rest)

    ["--gleam", path, ..rest] | ["-g", path, ..rest] ->
      parse_options_loop(CliConfig(..config, gleam_output: Ok(path)), rest)

    ["--resolvers", path, ..rest] | ["-r", path, ..rest] ->
      parse_options_loop(CliConfig(..config, resolvers_output: Ok(path)), rest)

    ["--sdl", path, ..rest] | ["-s", path, ..rest] ->
      parse_options_loop(CliConfig(..config, sdl_output: Ok(path)), rest)

    ["--all", prefix, ..rest] | ["-a", prefix, ..rest] ->
      parse_options_loop(
        CliConfig(
          ..config,
          typescript_output: Ok(prefix <> ".ts"),
          gleam_output: Ok(prefix <> "_types.gleam"),
          resolvers_output: Ok(prefix <> "_resolvers.gleam"),
          sdl_output: Ok(prefix <> ".graphql"),
        ),
        rest,
      )

    [unknown, ..] -> Error(InvalidArgs("Unknown option: " <> unknown))
  }
}

/// Read and parse schema file
fn read_and_parse_schema(path: String) -> Result(SDLDocument, CliError) {
  case simplifile.read(path) {
    Ok(content) -> {
      case sdl_parser.parse_sdl(content) {
        Ok(doc) -> Ok(doc)
        Error(e) -> Error(ParseError(format_parse_error(e)))
      }
    }
    Error(e) -> Error(FileReadError(path, simplifile_error_to_string(e)))
  }
}

/// Write content to file
fn write_file(path: String, content: String) -> Result(Nil, CliError) {
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error(WriteError(path, simplifile_error_to_string(e)))
  }
}

// TypeScript generation from SDL (simplified - generates basic types)
fn generate_typescript_from_sdl(doc: SDLDocument) -> String {
  let header = "// Generated by mochi - DO NOT EDIT\n\n"

  let maybe_helper =
    "export type Maybe<T> = T | null | undefined;\n\n"
    <> "export type Scalars = {\n"
    <> "  ID: string;\n"
    <> "  String: string;\n"
    <> "  Int: number;\n"
    <> "  Float: number;\n"
    <> "  Boolean: boolean;\n"
    <> "};\n\n"

  let types =
    doc.definitions
    |> list.filter_map(fn(def) {
      case def {
        sdl_ast.TypeDefinition(type_def) -> Ok(type_def_to_typescript(type_def))
        _ -> Error(Nil)
      }
    })
    |> string.join("\n\n")

  header <> maybe_helper <> types
}

fn type_def_to_typescript(type_def: sdl_ast.TypeDef) -> String {
  case type_def {
    sdl_ast.ObjectTypeDefinition(obj) -> object_to_typescript(obj)
    sdl_ast.InterfaceTypeDefinition(iface) -> interface_to_typescript(iface)
    sdl_ast.EnumTypeDefinition(enum) -> enum_to_typescript(enum)
    sdl_ast.InputObjectTypeDefinition(input) -> input_to_typescript(input)
    sdl_ast.UnionTypeDefinition(union) -> union_to_typescript(union)
    sdl_ast.ScalarTypeDefinition(scalar) -> scalar_to_typescript(scalar)
  }
}

fn object_to_typescript(obj: sdl_ast.ObjectTypeDef) -> String {
  let fields =
    obj.fields
    |> list.map(field_to_typescript)
    |> string.join("\n")

  "export interface " <> obj.name <> " {\n" <> fields <> "\n}"
}

fn interface_to_typescript(iface: sdl_ast.InterfaceTypeDef) -> String {
  let fields =
    iface.fields
    |> list.map(field_to_typescript)
    |> string.join("\n")

  "export interface " <> iface.name <> " {\n" <> fields <> "\n}"
}

fn field_to_typescript(field: sdl_ast.FieldDef) -> String {
  let optional = case is_non_null(field.field_type) {
    True -> ""
    False -> "?"
  }
  "  "
  <> field.name
  <> optional
  <> ": "
  <> sdl_type_to_typescript(field.field_type)
  <> ";"
}

fn enum_to_typescript(enum: sdl_ast.EnumTypeDef) -> String {
  let values =
    enum.values
    |> list.map(fn(v) { "  " <> v.name <> " = \"" <> v.name <> "\"," })
    |> string.join("\n")

  "export enum " <> enum.name <> " {\n" <> values <> "\n}"
}

fn input_to_typescript(input: sdl_ast.InputObjectTypeDef) -> String {
  let fields =
    input.fields
    |> list.map(fn(f) {
      let optional = case is_non_null(f.field_type) {
        True -> ""
        False -> "?"
      }
      "  "
      <> f.name
      <> optional
      <> ": "
      <> sdl_type_to_typescript(f.field_type)
      <> ";"
    })
    |> string.join("\n")

  "export interface " <> input.name <> " {\n" <> fields <> "\n}"
}

fn union_to_typescript(union: sdl_ast.UnionTypeDef) -> String {
  let types = string.join(union.member_types, " | ")
  "export type " <> union.name <> " = " <> types <> ";"
}

fn scalar_to_typescript(scalar: sdl_ast.ScalarTypeDef) -> String {
  "export type " <> scalar.name <> " = string;"
}

fn sdl_type_to_typescript(sdl_type: sdl_ast.SDLType) -> String {
  case sdl_type {
    sdl_ast.NamedType(name) -> scalar_to_ts_type(name)
    sdl_ast.NonNullType(inner) -> sdl_type_to_typescript(inner)
    sdl_ast.ListType(inner) ->
      "Maybe<" <> sdl_type_to_typescript(inner) <> ">[]"
  }
}

fn scalar_to_ts_type(name: String) -> String {
  case name {
    "String" -> "Scalars[\"String\"]"
    "Int" -> "Scalars[\"Int\"]"
    "Float" -> "Scalars[\"Float\"]"
    "Boolean" -> "Scalars[\"Boolean\"]"
    "ID" -> "Scalars[\"ID\"]"
    other -> other
  }
}

fn is_non_null(sdl_type: sdl_ast.SDLType) -> Bool {
  case sdl_type {
    sdl_ast.NonNullType(_) -> True
    _ -> False
  }
}

// SDL regeneration (for normalization)
fn generate_sdl_from_sdl(doc: SDLDocument) -> String {
  let header = "# Generated by mochi\n\n"

  let types =
    doc.definitions
    |> list.filter_map(fn(def) {
      case def {
        sdl_ast.TypeDefinition(type_def) -> Ok(type_def_to_sdl(type_def))
        _ -> Error(Nil)
      }
    })
    |> string.join("\n\n")

  header <> types
}

fn type_def_to_sdl(type_def: sdl_ast.TypeDef) -> String {
  case type_def {
    sdl_ast.ObjectTypeDefinition(obj) -> object_to_sdl(obj)
    sdl_ast.InterfaceTypeDefinition(iface) -> interface_to_sdl(iface)
    sdl_ast.EnumTypeDefinition(enum) -> enum_to_sdl(enum)
    sdl_ast.InputObjectTypeDefinition(input) -> input_to_sdl(input)
    sdl_ast.UnionTypeDefinition(union) -> union_to_sdl(union)
    sdl_ast.ScalarTypeDefinition(scalar) -> scalar_to_sdl(scalar)
  }
}

fn object_to_sdl(obj: sdl_ast.ObjectTypeDef) -> String {
  let desc = case obj.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  let implements = case obj.interfaces {
    [] -> ""
    ifaces -> " implements " <> string.join(ifaces, " & ")
  }

  let fields =
    obj.fields
    |> list.map(field_to_sdl)
    |> string.join("\n")

  desc <> "type " <> obj.name <> implements <> " {\n" <> fields <> "\n}"
}

fn interface_to_sdl(iface: sdl_ast.InterfaceTypeDef) -> String {
  let desc = case iface.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  let fields =
    iface.fields
    |> list.map(field_to_sdl)
    |> string.join("\n")

  desc <> "interface " <> iface.name <> " {\n" <> fields <> "\n}"
}

fn field_to_sdl(field: sdl_ast.FieldDef) -> String {
  let desc = case field.description {
    Some(d) -> "  \"" <> d <> "\"\n"
    None -> ""
  }

  let args = case field.arguments {
    [] -> ""
    args -> "(" <> string.join(list.map(args, arg_to_sdl), ", ") <> ")"
  }

  desc
  <> "  "
  <> field.name
  <> args
  <> ": "
  <> sdl_type_to_string(field.field_type)
}

fn arg_to_sdl(arg: sdl_ast.ArgumentDef) -> String {
  arg.name <> ": " <> sdl_type_to_string(arg.arg_type)
}

fn enum_to_sdl(enum: sdl_ast.EnumTypeDef) -> String {
  let desc = case enum.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  let values =
    enum.values
    |> list.map(fn(v) { "  " <> v.name })
    |> string.join("\n")

  desc <> "enum " <> enum.name <> " {\n" <> values <> "\n}"
}

fn input_to_sdl(input: sdl_ast.InputObjectTypeDef) -> String {
  let desc = case input.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  let fields =
    input.fields
    |> list.map(fn(f) {
      "  " <> f.name <> ": " <> sdl_type_to_string(f.field_type)
    })
    |> string.join("\n")

  desc <> "input " <> input.name <> " {\n" <> fields <> "\n}"
}

fn union_to_sdl(union: sdl_ast.UnionTypeDef) -> String {
  let desc = case union.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  desc
  <> "union "
  <> union.name
  <> " = "
  <> string.join(union.member_types, " | ")
}

fn scalar_to_sdl(scalar: sdl_ast.ScalarTypeDef) -> String {
  let desc = case scalar.description {
    Some(d) -> "\"\"\"" <> d <> "\"\"\"\n"
    None -> ""
  }

  desc <> "scalar " <> scalar.name
}

fn sdl_type_to_string(sdl_type: sdl_ast.SDLType) -> String {
  case sdl_type {
    sdl_ast.NamedType(name) -> name
    sdl_ast.NonNullType(inner) -> sdl_type_to_string(inner) <> "!"
    sdl_ast.ListType(inner) -> "[" <> sdl_type_to_string(inner) <> "]"
  }
}

// Error formatting

fn format_error(err: CliError) -> String {
  case err {
    NoSchemaFile -> "Error: No schema file specified"
    FileReadError(path, reason) ->
      "Error reading file '" <> path <> "': " <> reason
    ParseError(msg) -> "Parse error: " <> msg
    WriteError(path, reason) ->
      "Error writing file '" <> path <> "': " <> reason
    InvalidArgs(msg) -> msg
  }
}

fn format_parse_error(err: sdl_parser.SDLParseError) -> String {
  case err {
    sdl_parser.SDLLexError(e) -> "Lexer error: " <> format_lex_error(e)
    sdl_parser.UnexpectedToken(expected, _, pos) ->
      "Unexpected token at line "
      <> int_to_string(pos.line)
      <> ", expected "
      <> expected
    sdl_parser.UnexpectedEOF(expected) ->
      "Unexpected end of file, expected " <> expected
    sdl_parser.InvalidTypeDefinition(msg, pos) ->
      "Invalid type at line " <> int_to_string(pos.line) <> ": " <> msg
  }
}

fn format_lex_error(_err) -> String {
  "Lexer error"
}

fn simplifile_error_to_string(_err) -> String {
  "File system error"
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ -> "?"
  }
}

fn help_text() -> String {
  "mochi - GraphQL Code Generator for Gleam

Usage:
  gleam run -m mochi/cli -- <schema.graphql> [options]

Options:
  --typescript, -t <file>   Generate TypeScript types
  --gleam, -g <file>        Generate Gleam types
  --resolvers, -r <file>    Generate resolver stubs
  --sdl, -s <file>          Generate normalized SDL
  --all, -a <prefix>        Generate all files with prefix
  --help, -h                Show this help

Examples:
  gleam run -m mochi/cli -- schema.graphql -t types.ts
  gleam run -m mochi/cli -- schema.graphql -g src/types.gleam -r src/resolvers.gleam
  gleam run -m mochi/cli -- schema.graphql --all generated/schema
"
}

// FFI stubs (these would be implemented via Erlang/JS FFI)
@external(erlang, "erlang", "halt")
@external(javascript, "./mochi_ffi.mjs", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "mochi_ffi", "get_args")
@external(javascript, "./mochi_ffi.mjs", "getArgs")
fn get_args() -> List(String)

// For use from code (not CLI)

/// Generate TypeScript types from a schema
pub fn typescript(schema: Schema) -> String {
  typescript.generate(schema)
}

/// Generate GraphQL SDL from a schema
pub fn sdl(schema: Schema) -> String {
  sdl.generate(schema)
}

/// Print TypeScript types to stdout
pub fn print_typescript(schema: Schema) -> Nil {
  io.println(typescript.generate(schema))
}

/// Print SDL to stdout
pub fn print_sdl(schema: Schema) -> Nil {
  io.println(sdl.generate(schema))
}
