import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import { Ok, Empty as $Empty } from "../gleam.mjs";
import * as $sdl_ast from "../mochi/sdl_ast.mjs";
import * as $sdl_lexer from "../mochi/sdl_lexer.mjs";
import * as $sdl_parser from "../mochi/sdl_parser.mjs";

function int_to_string(value) {
  if (value === 0) {
    return "0";
  } else if (value === 1) {
    return "1";
  } else if (value === 2) {
    return "2";
  } else if (value === 3) {
    return "3";
  } else if (value === 4) {
    return "4";
  } else if (value === 5) {
    return "5";
  } else if (value === 6) {
    return "6";
  } else if (value === 7) {
    return "7";
  } else if (value === 8) {
    return "8";
  } else if (value === 9) {
    return "9";
  } else {
    return "many";
  }
}

function count_definitions(loop$definitions, loop$acc) {
  while (true) {
    let definitions = loop$definitions;
    let acc = loop$acc;
    if (definitions instanceof $Empty) {
      return int_to_string(acc);
    } else {
      let rest = definitions.tail;
      loop$definitions = rest;
      loop$acc = acc + 1;
    }
  }
}

function get_type_kind(type_def) {
  if (type_def instanceof $sdl_ast.ObjectTypeDefinition) {
    return "type";
  } else if (type_def instanceof $sdl_ast.InterfaceTypeDefinition) {
    return "interface";
  } else if (type_def instanceof $sdl_ast.UnionTypeDefinition) {
    return "union";
  } else if (type_def instanceof $sdl_ast.ScalarTypeDefinition) {
    return "scalar";
  } else if (type_def instanceof $sdl_ast.EnumTypeDefinition) {
    return "enum";
  } else {
    return "input";
  }
}

function print_definition(definition, index) {
  let index_str = int_to_string(index);
  if (definition instanceof $sdl_ast.TypeDefinition) {
    let type_def = definition.type_def;
    let type_name = $sdl_ast.get_type_name(type_def);
    let type_kind = get_type_kind(type_def);
    return $io.println(
      (((("  " + index_str) + ". ") + type_kind) + " ") + type_name,
    );
  } else {
    return $io.println(("  " + index_str) + ". Other definition");
  }
}

function print_definition_list(loop$definitions, loop$index) {
  while (true) {
    let definitions = loop$definitions;
    let index = loop$index;
    if (definitions instanceof $Empty) {
      return undefined;
    } else {
      let def = definitions.head;
      let rest = definitions.tail;
      print_definition(def, index);
      loop$definitions = rest;
      loop$index = index + 1;
    }
  }
}

function print_document_summary(document) {
  $io.println("Document contains:");
  let count = count_definitions(document.definitions, 0);
  $io.println(("  - " + count) + " type definitions");
  let $ = document.definitions;
  if ($ instanceof $Empty) {
    return $io.println("  (no definitions)");
  } else {
    let definitions = $;
    return print_definition_list(definitions, 1);
  }
}

function format_lex_error(error) {
  if (error instanceof $sdl_lexer.UnexpectedCharacter) {
    let char = error.character;
    return "Unexpected character: " + char;
  } else if (error instanceof $sdl_lexer.InvalidNumber) {
    let value = error.value;
    return "Invalid number: " + value;
  } else if (error instanceof $sdl_lexer.UnterminatedString) {
    return "Unterminated string";
  } else {
    return "Unterminated description";
  }
}

function format_token(token) {
  if (token instanceof $sdl_lexer.LeftBrace) {
    return "{";
  } else if (token instanceof $sdl_lexer.RightBrace) {
    return "}";
  } else if (token instanceof $sdl_lexer.LeftParen) {
    return "(";
  } else if (token instanceof $sdl_lexer.RightParen) {
    return ")";
  } else if (token instanceof $sdl_lexer.LeftBracket) {
    return "[";
  } else if (token instanceof $sdl_lexer.RightBracket) {
    return "]";
  } else if (token instanceof $sdl_lexer.Colon) {
    return ":";
  } else if (token instanceof $sdl_lexer.Bang) {
    return "!";
  } else if (token instanceof $sdl_lexer.Equals) {
    return "=";
  } else if (token instanceof $sdl_lexer.Pipe) {
    return "|";
  } else if (token instanceof $sdl_lexer.Amp) {
    return "&";
  } else if (token instanceof $sdl_lexer.Type) {
    return "type";
  } else if (token instanceof $sdl_lexer.Interface) {
    return "interface";
  } else if (token instanceof $sdl_lexer.Union) {
    return "union";
  } else if (token instanceof $sdl_lexer.Scalar) {
    return "scalar";
  } else if (token instanceof $sdl_lexer.Enum) {
    return "enum";
  } else if (token instanceof $sdl_lexer.Input) {
    return "input";
  } else if (token instanceof $sdl_lexer.Name) {
    let name = token.value;
    return ("Name(" + name) + ")";
  } else if (token instanceof $sdl_lexer.EOF) {
    return "EOF";
  } else {
    return "Token";
  }
}

function print_parse_error(error) {
  if (error instanceof $sdl_parser.SDLLexError) {
    let lex_error = error.error;
    return $io.println("  Lexer error: " + format_lex_error(lex_error));
  } else if (error instanceof $sdl_parser.UnexpectedToken) {
    let expected = error.expected;
    let got = error.got;
    let position = error.position;
    $io.println("  Expected: " + expected);
    $io.println("  Got: " + format_token(got));
    return $io.println(
      (("  At: line " + int_to_string(position.line)) + ", column ") + int_to_string(
        position.column,
      ),
    );
  } else if (error instanceof $sdl_parser.UnexpectedEOF) {
    let expected = error.expected;
    return $io.println("  Unexpected end of file, expected: " + expected);
  } else {
    let message = error.message;
    let position = error.position;
    $io.println("  Invalid type definition: " + message);
    return $io.println(
      (("  At: line " + int_to_string(position.line)) + ", column ") + int_to_string(
        position.column,
      ),
    );
  }
}

function demo_basic_types() {
  $io.println("📝 Test 1: Basic Types");
  $io.println("======================");
  let schema_sdl = "\n    scalar DateTime\n    \n    type User {\n      id: ID!\n      name: String\n      email: String!\n    }\n    \n    interface Node {\n      id: ID!\n    }\n  ";
  $io.println("Input SDL:");
  $io.println(schema_sdl);
  let $ = $sdl_parser.parse_sdl(schema_sdl);
  if ($ instanceof Ok) {
    let document = $[0];
    $io.println("✅ Parsing successful!");
    print_document_summary(document)
  } else {
    let error = $[0];
    $io.println("❌ Parsing failed:");
    print_parse_error(error)
  }
  return $io.println("");
}

function demo_complex_object() {
  $io.println("📝 Test 2: Complex Object with Arguments");
  $io.println("========================================");
  let schema_sdl = "\n    type Query {\n      user(id: ID!): User\n      posts(limit: Int = 10, offset: Int = 0): [Post!]!\n      search(query: String!, type: SearchType = USER): SearchResult\n    }\n    \n    type Post {\n      id: ID!\n      title: String!\n      content: String\n      author: User!\n    }\n  ";
  $io.println("Input SDL:");
  $io.println(schema_sdl);
  let $ = $sdl_parser.parse_sdl(schema_sdl);
  if ($ instanceof Ok) {
    let document = $[0];
    $io.println("✅ Complex parsing successful!");
    print_document_summary(document)
  } else {
    let error = $[0];
    $io.println("❌ Complex parsing failed:");
    print_parse_error(error)
  }
  return $io.println("");
}

function demo_union_type() {
  $io.println("📝 Test 3: Union Types");
  $io.println("======================");
  let schema_sdl = "\n    union SearchResult = User | Post | Comment\n    \n    union MediaItem = Photo | Video | Audio\n  ";
  $io.println("Input SDL:");
  $io.println(schema_sdl);
  let $ = $sdl_parser.parse_sdl(schema_sdl);
  if ($ instanceof Ok) {
    let document = $[0];
    $io.println("✅ Union parsing successful!");
    print_document_summary(document)
  } else {
    let error = $[0];
    $io.println("❌ Union parsing failed:");
    print_parse_error(error)
  }
  return $io.println("");
}

function demo_enum_type() {
  $io.println("📝 Test 4: Enum Types");
  $io.println("=====================");
  let schema_sdl = "\n    enum Status {\n      ACTIVE\n      INACTIVE\n      PENDING\n    }\n    \n    enum UserRole {\n      ADMIN\n      USER\n      MODERATOR\n    }\n  ";
  $io.println("Input SDL:");
  $io.println(schema_sdl);
  let $ = $sdl_parser.parse_sdl(schema_sdl);
  if ($ instanceof Ok) {
    let document = $[0];
    $io.println("✅ Enum parsing successful!");
    print_document_summary(document)
  } else {
    let error = $[0];
    $io.println("❌ Enum parsing failed:");
    print_parse_error(error)
  }
  return $io.println("");
}

function demo_input_type() {
  $io.println("📝 Test 5: Input Types");
  $io.println("======================");
  let schema_sdl = "\n    input CreateUserInput {\n      name: String!\n      email: String!\n      age: Int = 18\n      role: UserRole = USER\n    }\n    \n    input UpdateUserInput {\n      id: ID!\n      name: String\n      email: String\n    }\n  ";
  $io.println("Input SDL:");
  $io.println(schema_sdl);
  let $ = $sdl_parser.parse_sdl(schema_sdl);
  if ($ instanceof Ok) {
    let document = $[0];
    $io.println("✅ Input parsing successful!");
    print_document_summary(document)
  } else {
    let error = $[0];
    $io.println("❌ Input parsing failed:");
    print_parse_error(error)
  }
  return $io.println("");
}

function demo_error_handling() {
  $io.println("📝 Test 6: Error Handling");
  $io.println("=========================");
  let invalid_sdl = "\n    type User {\n      id: ID!\n      name: String\n      # Missing closing brace intentionally\n  ";
  $io.println("Invalid SDL (missing closing brace):");
  $io.println(invalid_sdl);
  let $ = $sdl_parser.parse_sdl(invalid_sdl);
  if ($ instanceof Ok) {
    $io.println("❌ Should have failed but didn't!")
  } else {
    let error = $[0];
    $io.println("✅ Correctly caught error:");
    print_parse_error(error)
  }
  $io.println("");
  return $io.println("🎯 SDL Demo Complete!");
}

/**
 * Comprehensive SDL demo showing parsing and error handling
 */
export function demo_sdl_parsing() {
  $io.println("=== GeQL SDL Complete Demo ===");
  $io.println("");
  demo_basic_types();
  demo_complex_object();
  demo_union_type();
  demo_enum_type();
  demo_input_type();
  return demo_error_handling();
}
