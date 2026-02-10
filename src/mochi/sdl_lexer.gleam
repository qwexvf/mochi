// SDL (Schema Definition Language) Lexer
// Tokenizes GraphQL SDL syntax like: type User { name: String! }

import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// SDL-specific tokens (extends the basic GraphQL tokens)
pub type SDLToken {
  // Basic tokens (reused from query lexer)
  LeftBrace
  RightBrace
  LeftParen
  RightParen
  LeftBracket
  RightBracket
  Colon
  Bang
  Equals
  At
  Pipe
  Amp

  // SDL Keywords
  Type
  Interface
  Union
  Scalar
  Enum
  Input
  Directive
  Schema
  Extend
  Implements

  // SDL Values
  Name(value: String)
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)
  BooleanValue(value: Bool)

  // Special
  EOF
  Comment(content: String)
  Description(content: String)
  // Triple-quoted descriptions
}

pub type Position {
  Position(line: Int, column: Int)
}

pub type SDLTokenWithPosition {
  SDLTokenWithPosition(token: SDLToken, position: Position)
}

pub type SDLLexerError {
  UnexpectedCharacter(character: String, position: Position)
  InvalidNumber(value: String, position: Position)
  UnterminatedString(position: Position)
  UnterminatedDescription(position: Position)
}

pub type SDLLexerState {
  SDLLexerState(input: String, position: Int, line: Int, column: Int)
}

pub fn new_sdl_lexer(input: String) -> SDLLexerState {
  SDLLexerState(input: input, position: 0, line: 1, column: 1)
}

pub fn tokenize_sdl(
  input: String,
) -> Result(List(SDLTokenWithPosition), SDLLexerError) {
  let lexer = new_sdl_lexer(input)
  tokenize_loop(lexer, [])
}

fn tokenize_loop(
  lexer: SDLLexerState,
  tokens: List(SDLTokenWithPosition),
) -> Result(List(SDLTokenWithPosition), SDLLexerError) {
  case next_sdl_token(lexer) {
    Ok(#(SDLTokenWithPosition(EOF, _), _)) ->
      list.reverse([
        SDLTokenWithPosition(EOF, Position(lexer.line, lexer.column)),
        ..tokens
      ])
      |> Ok
    Ok(#(token, new_lexer)) -> tokenize_loop(new_lexer, [token, ..tokens])
    Error(err) -> Error(err)
  }
}

pub fn next_sdl_token(
  lexer: SDLLexerState,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let lexer = skip_whitespace_and_comments(lexer)

  case peek_char(lexer) {
    Error(_) ->
      Ok(#(SDLTokenWithPosition(EOF, Position(lexer.line, lexer.column)), lexer))
    Ok(char) -> {
      let position = Position(lexer.line, lexer.column)
      case char {
        "{" ->
          Ok(#(SDLTokenWithPosition(LeftBrace, position), advance_char(lexer)))
        "}" ->
          Ok(#(SDLTokenWithPosition(RightBrace, position), advance_char(lexer)))
        "(" ->
          Ok(#(SDLTokenWithPosition(LeftParen, position), advance_char(lexer)))
        ")" ->
          Ok(#(SDLTokenWithPosition(RightParen, position), advance_char(lexer)))
        "[" ->
          Ok(#(SDLTokenWithPosition(LeftBracket, position), advance_char(lexer)))
        "]" ->
          Ok(#(
            SDLTokenWithPosition(RightBracket, position),
            advance_char(lexer),
          ))
        ":" -> Ok(#(SDLTokenWithPosition(Colon, position), advance_char(lexer)))
        "!" -> Ok(#(SDLTokenWithPosition(Bang, position), advance_char(lexer)))
        "=" ->
          Ok(#(SDLTokenWithPosition(Equals, position), advance_char(lexer)))
        "@" -> Ok(#(SDLTokenWithPosition(At, position), advance_char(lexer)))
        "|" -> Ok(#(SDLTokenWithPosition(Pipe, position), advance_char(lexer)))
        "&" -> Ok(#(SDLTokenWithPosition(Amp, position), advance_char(lexer)))

        "\"" -> {
          case peek_string(lexer) {
            "\"\"\"" -> read_description(lexer, position)
            _ -> read_string(lexer, position)
          }
        }

        "#" -> read_comment(lexer, position)

        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "-" ->
          read_number(lexer, position)

        // Names and keywords (including SDL keywords)
        "a"
        | "b"
        | "c"
        | "d"
        | "e"
        | "f"
        | "g"
        | "h"
        | "i"
        | "j"
        | "k"
        | "l"
        | "m"
        | "n"
        | "o"
        | "p"
        | "q"
        | "r"
        | "s"
        | "t"
        | "u"
        | "v"
        | "w"
        | "x"
        | "y"
        | "z"
        | "A"
        | "B"
        | "C"
        | "D"
        | "E"
        | "F"
        | "G"
        | "H"
        | "I"
        | "J"
        | "K"
        | "L"
        | "M"
        | "N"
        | "O"
        | "P"
        | "Q"
        | "R"
        | "S"
        | "T"
        | "U"
        | "V"
        | "W"
        | "X"
        | "Y"
        | "Z"
        | "_" -> read_name_or_keyword(lexer, position)

        _ -> Error(UnexpectedCharacter(char, position))
      }
    }
  }
}

fn skip_whitespace_and_comments(lexer: SDLLexerState) -> SDLLexerState {
  case peek_char(lexer) {
    Ok(" ") | Ok("\t") | Ok("\n") | Ok("\r") | Ok(",") ->
      skip_whitespace_and_comments(advance_char(lexer))
    _ -> lexer
  }
}

fn read_name_or_keyword(
  lexer: SDLLexerState,
  position: Position,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let #(name, new_lexer) = read_name_chars(lexer, "")
  let token = case name {
    // SDL Keywords
    "type" -> Type
    "interface" -> Interface
    "union" -> Union
    "scalar" -> Scalar
    "enum" -> Enum
    "input" -> Input
    "directive" -> Directive
    "schema" -> Schema
    "extend" -> Extend
    "implements" -> Implements

    // Boolean values
    "true" -> BooleanValue(True)
    "false" -> BooleanValue(False)

    // Regular name
    _ -> Name(name)
  }

  Ok(#(SDLTokenWithPosition(token, position), new_lexer))
}

fn read_name_chars(
  lexer: SDLLexerState,
  acc: String,
) -> #(String, SDLLexerState) {
  case peek_char(lexer) {
    Ok(char) -> {
      case is_name_continue(char) {
        True -> read_name_chars(advance_char(lexer), acc <> char)
        False -> #(acc, lexer)
      }
    }
    _ -> #(acc, lexer)
  }
}

fn read_string(
  lexer: SDLLexerState,
  position: Position,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let lexer = advance_char(lexer)
  // Skip opening quote
  case read_string_chars(lexer, "") {
    Ok(#(value, new_lexer)) -> {
      case peek_char(new_lexer) {
        Ok("\"") -> {
          let final_lexer = advance_char(new_lexer)
          Ok(#(SDLTokenWithPosition(StringValue(value), position), final_lexer))
        }
        _ -> Error(UnterminatedString(position))
      }
    }
    Error(_) -> Error(UnterminatedString(position))
  }
}

fn read_description(
  lexer: SDLLexerState,
  position: Position,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let lexer = advance_char(advance_char(advance_char(lexer)))
  // Skip """
  case read_description_chars(lexer, "") {
    Ok(#(content, new_lexer)) -> {
      case peek_string_n(new_lexer, 3) {
        "\"\"\"" -> {
          let final_lexer = advance_char(advance_char(advance_char(new_lexer)))
          Ok(#(
            SDLTokenWithPosition(Description(content), position),
            final_lexer,
          ))
        }
        _ -> Error(UnterminatedDescription(position))
      }
    }
    Error(_) -> Error(UnterminatedDescription(position))
  }
}

fn read_comment(
  lexer: SDLLexerState,
  position: Position,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let lexer = advance_char(lexer)
  // Skip #
  let #(content, new_lexer) = read_comment_chars(lexer, "")
  Ok(#(SDLTokenWithPosition(Comment(content), position), new_lexer))
}

fn read_number(
  lexer: SDLLexerState,
  position: Position,
) -> Result(#(SDLTokenWithPosition, SDLLexerState), SDLLexerError) {
  let #(number_str, new_lexer) = read_number_chars(lexer, "")
  case string.contains(number_str, ".") {
    True -> {
      case float.parse(number_str) {
        Ok(value) ->
          Ok(#(SDLTokenWithPosition(FloatValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
    False -> {
      case int.parse(number_str) {
        Ok(value) ->
          Ok(#(SDLTokenWithPosition(IntValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
  }
}

// Helper functions

fn peek_char(lexer: SDLLexerState) -> Result(String, Nil) {
  case string.slice(lexer.input, lexer.position, 1) {
    "" -> Error(Nil)
    char -> Ok(char)
  }
}

fn peek_string(lexer: SDLLexerState) -> String {
  string.slice(lexer.input, lexer.position, 3)
}

fn peek_string_n(lexer: SDLLexerState, n: Int) -> String {
  string.slice(lexer.input, lexer.position, n)
}

fn advance_char(lexer: SDLLexerState) -> SDLLexerState {
  case peek_char(lexer) {
    Ok("\n") ->
      SDLLexerState(
        ..lexer,
        position: lexer.position + 1,
        line: lexer.line + 1,
        column: 1,
      )
    _ ->
      SDLLexerState(
        ..lexer,
        position: lexer.position + 1,
        column: lexer.column + 1,
      )
  }
}

fn is_name_continue(char: String) -> Bool {
  is_letter(char) || is_digit(char) || char == "_"
}

fn is_letter(char: String) -> Bool {
  case char {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    _ -> False
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn read_string_chars(
  lexer: SDLLexerState,
  acc: String,
) -> Result(#(String, SDLLexerState), Nil) {
  case peek_char(lexer) {
    Ok("\"") -> Ok(#(acc, lexer))
    Ok("\n") -> Error(Nil)
    // Unterminated string
    Ok(char) -> read_string_chars(advance_char(lexer), acc <> char)
    Error(_) -> Error(Nil)
  }
}

fn read_description_chars(
  lexer: SDLLexerState,
  acc: String,
) -> Result(#(String, SDLLexerState), Nil) {
  case peek_string_n(lexer, 3) {
    "\"\"\"" -> Ok(#(acc, lexer))
    _ -> {
      case peek_char(lexer) {
        Ok(char) -> read_description_chars(advance_char(lexer), acc <> char)
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn read_comment_chars(
  lexer: SDLLexerState,
  acc: String,
) -> #(String, SDLLexerState) {
  case peek_char(lexer) {
    Ok("\n") -> #(acc, lexer)
    Ok(char) -> read_comment_chars(advance_char(lexer), acc <> char)
    Error(_) -> #(acc, lexer)
  }
}

fn read_number_chars(
  lexer: SDLLexerState,
  acc: String,
) -> #(String, SDLLexerState) {
  case peek_char(lexer) {
    Ok(char) -> {
      case is_digit(char) || char == "." || char == "-" {
        True -> read_number_chars(advance_char(lexer), acc <> char)
        False -> #(acc, lexer)
      }
    }
    _ -> #(acc, lexer)
  }
}
