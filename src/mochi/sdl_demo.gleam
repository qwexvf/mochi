// SDL Complete Demo - Shows full parsing and printing of GraphQL SDL
// Tests all the SDL functionality: lexing, parsing, and schema printing

import mochi/sdl_ast
import mochi/sdl_lexer
import mochi/sdl_parser
import gleam/io

/// Comprehensive SDL demo showing parsing and error handling
pub fn demo_sdl_parsing() -> Nil {
  io.println("=== GeQL SDL Complete Demo ===")
  io.println("")

  // Test 1: Basic types
  demo_basic_types()

  // Test 2: Complex object with arguments
  demo_complex_object()

  // Test 3: Union types
  demo_union_type()

  // Test 4: Enum types
  demo_enum_type()

  // Test 5: Input types
  demo_input_type()

  // Test 6: Error handling
  demo_error_handling()
}

fn demo_basic_types() -> Nil {
  io.println("📝 Test 1: Basic Types")
  io.println("======================")

  let schema_sdl =
    "
    scalar DateTime
    
    type User {
      id: ID!
      name: String
      email: String!
    }
    
    interface Node {
      id: ID!
    }
  "

  io.println("Input SDL:")
  io.println(schema_sdl)

  case sdl_parser.parse_sdl(schema_sdl) {
    Ok(document) -> {
      io.println("✅ Parsing successful!")
      print_document_summary(document)
    }
    Error(error) -> {
      io.println("❌ Parsing failed:")
      print_parse_error(error)
    }
  }

  io.println("")
}

fn demo_complex_object() -> Nil {
  io.println("📝 Test 2: Complex Object with Arguments")
  io.println("========================================")

  let schema_sdl =
    "
    type Query {
      user(id: ID!): User
      posts(limit: Int = 10, offset: Int = 0): [Post!]!
      search(query: String!, type: SearchType = USER): SearchResult
    }
    
    type Post {
      id: ID!
      title: String!
      content: String
      author: User!
    }
  "

  io.println("Input SDL:")
  io.println(schema_sdl)

  case sdl_parser.parse_sdl(schema_sdl) {
    Ok(document) -> {
      io.println("✅ Complex parsing successful!")
      print_document_summary(document)
    }
    Error(error) -> {
      io.println("❌ Complex parsing failed:")
      print_parse_error(error)
    }
  }

  io.println("")
}

fn demo_union_type() -> Nil {
  io.println("📝 Test 3: Union Types")
  io.println("======================")

  let schema_sdl =
    "
    union SearchResult = User | Post | Comment
    
    union MediaItem = Photo | Video | Audio
  "

  io.println("Input SDL:")
  io.println(schema_sdl)

  case sdl_parser.parse_sdl(schema_sdl) {
    Ok(document) -> {
      io.println("✅ Union parsing successful!")
      print_document_summary(document)
    }
    Error(error) -> {
      io.println("❌ Union parsing failed:")
      print_parse_error(error)
    }
  }

  io.println("")
}

fn demo_enum_type() -> Nil {
  io.println("📝 Test 4: Enum Types")
  io.println("=====================")

  let schema_sdl =
    "
    enum Status {
      ACTIVE
      INACTIVE
      PENDING
    }
    
    enum UserRole {
      ADMIN
      USER
      MODERATOR
    }
  "

  io.println("Input SDL:")
  io.println(schema_sdl)

  case sdl_parser.parse_sdl(schema_sdl) {
    Ok(document) -> {
      io.println("✅ Enum parsing successful!")
      print_document_summary(document)
    }
    Error(error) -> {
      io.println("❌ Enum parsing failed:")
      print_parse_error(error)
    }
  }

  io.println("")
}

fn demo_input_type() -> Nil {
  io.println("📝 Test 5: Input Types")
  io.println("======================")

  let schema_sdl =
    "
    input CreateUserInput {
      name: String!
      email: String!
      age: Int = 18
      role: UserRole = USER
    }
    
    input UpdateUserInput {
      id: ID!
      name: String
      email: String
    }
  "

  io.println("Input SDL:")
  io.println(schema_sdl)

  case sdl_parser.parse_sdl(schema_sdl) {
    Ok(document) -> {
      io.println("✅ Input parsing successful!")
      print_document_summary(document)
    }
    Error(error) -> {
      io.println("❌ Input parsing failed:")
      print_parse_error(error)
    }
  }

  io.println("")
}

fn demo_error_handling() -> Nil {
  io.println("📝 Test 6: Error Handling")
  io.println("=========================")

  let invalid_sdl =
    "
    type User {
      id: ID!
      name: String
      # Missing closing brace intentionally
  "

  io.println("Invalid SDL (missing closing brace):")
  io.println(invalid_sdl)

  case sdl_parser.parse_sdl(invalid_sdl) {
    Ok(_) -> {
      io.println("❌ Should have failed but didn't!")
    }
    Error(error) -> {
      io.println("✅ Correctly caught error:")
      print_parse_error(error)
    }
  }

  io.println("")
  io.println("🎯 SDL Demo Complete!")
}

fn print_document_summary(document: sdl_ast.SDLDocument) -> Nil {
  io.println("Document contains:")
  let count = count_definitions(document.definitions, 0)
  io.println("  - " <> count <> " type definitions")

  case document.definitions {
    [] -> io.println("  (no definitions)")
    definitions -> {
      print_definition_list(definitions, 1)
    }
  }
}

fn count_definitions(
  definitions: List(sdl_ast.TypeSystemDefinition),
  acc: Int,
) -> String {
  case definitions {
    [] -> int_to_string(acc)
    [_, ..rest] -> count_definitions(rest, acc + 1)
  }
}

fn int_to_string(value: Int) -> String {
  case value {
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
    _ -> "many"
  }
}

fn print_definition_list(
  definitions: List(sdl_ast.TypeSystemDefinition),
  index: Int,
) -> Nil {
  case definitions {
    [] -> Nil
    [def, ..rest] -> {
      print_definition(def, index)
      print_definition_list(rest, index + 1)
    }
  }
}

fn print_definition(definition: sdl_ast.TypeSystemDefinition, index: Int) -> Nil {
  let index_str = int_to_string(index)
  case definition {
    sdl_ast.TypeDefinition(type_def) -> {
      let type_name = sdl_ast.get_type_name(type_def)
      let type_kind = get_type_kind(type_def)
      io.println("  " <> index_str <> ". " <> type_kind <> " " <> type_name)
    }
    _ -> io.println("  " <> index_str <> ". Other definition")
  }
}

fn get_type_kind(type_def: sdl_ast.TypeDef) -> String {
  case type_def {
    sdl_ast.ObjectTypeDefinition(_) -> "type"
    sdl_ast.InterfaceTypeDefinition(_) -> "interface"
    sdl_ast.UnionTypeDefinition(_) -> "union"
    sdl_ast.ScalarTypeDefinition(_) -> "scalar"
    sdl_ast.EnumTypeDefinition(_) -> "enum"
    sdl_ast.InputObjectTypeDefinition(_) -> "input"
  }
}

fn print_parse_error(error: sdl_parser.SDLParseError) -> Nil {
  case error {
    sdl_parser.SDLLexError(lex_error) -> {
      io.println("  Lexer error: " <> format_lex_error(lex_error))
    }
    sdl_parser.UnexpectedToken(expected, got, position) -> {
      io.println("  Expected: " <> expected)
      io.println("  Got: " <> format_token(got))
      io.println(
        "  At: line "
        <> int_to_string(position.line)
        <> ", column "
        <> int_to_string(position.column),
      )
    }
    sdl_parser.UnexpectedEOF(expected) -> {
      io.println("  Unexpected end of file, expected: " <> expected)
    }
    sdl_parser.InvalidTypeDefinition(message, position) -> {
      io.println("  Invalid type definition: " <> message)
      io.println(
        "  At: line "
        <> int_to_string(position.line)
        <> ", column "
        <> int_to_string(position.column),
      )
    }
  }
}

fn format_lex_error(error: sdl_lexer.SDLLexerError) -> String {
  case error {
    sdl_lexer.UnexpectedCharacter(char, _) -> "Unexpected character: " <> char
    sdl_lexer.InvalidNumber(value, _) -> "Invalid number: " <> value
    sdl_lexer.UnterminatedString(_) -> "Unterminated string"
    sdl_lexer.UnterminatedDescription(_) -> "Unterminated description"
  }
}

fn format_token(token: sdl_lexer.SDLToken) -> String {
  case token {
    sdl_lexer.Name(name) -> "Name(" <> name <> ")"
    sdl_lexer.Type -> "type"
    sdl_lexer.Interface -> "interface"
    sdl_lexer.Union -> "union"
    sdl_lexer.Scalar -> "scalar"
    sdl_lexer.Enum -> "enum"
    sdl_lexer.Input -> "input"
    sdl_lexer.LeftBrace -> "{"
    sdl_lexer.RightBrace -> "}"
    sdl_lexer.LeftParen -> "("
    sdl_lexer.RightParen -> ")"
    sdl_lexer.LeftBracket -> "["
    sdl_lexer.RightBracket -> "]"
    sdl_lexer.Colon -> ":"
    sdl_lexer.Bang -> "!"
    sdl_lexer.Equals -> "="
    sdl_lexer.Pipe -> "|"
    sdl_lexer.Amp -> "&"
    sdl_lexer.EOF -> "EOF"
    _ -> "Token"
  }
}
