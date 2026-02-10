import gleam/float
import gleam/int
import gleam/list
import gleam/string

pub type Token {
  // Punctuators
  Bang
  Dollar
  Amp
  LeftParen
  RightParen
  Spread
  Colon
  Equals
  At
  LeftBracket
  RightBracket
  LeftBrace
  RightBrace
  Pipe

  // Keywords
  Query
  Mutation
  Subscription
  Fragment
  On
  TrueKeyword
  FalseKeyword
  NullKeyword

  // Literals
  Name(value: String)
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)

  // Special
  EOF
}

pub type Position {
  Position(line: Int, column: Int)
}

pub type TokenWithPosition {
  TokenWithPosition(token: Token, position: Position)
}

pub type LexerError {
  UnexpectedCharacter(character: String, position: Position)
  InvalidNumber(value: String, position: Position)
  UnterminatedString(position: Position)
}

pub type LexerState {
  LexerState(input: String, position: Int, line: Int, column: Int)
}

pub fn new_lexer(input: String) -> LexerState {
  LexerState(input: input, position: 0, line: 1, column: 1)
}

pub fn tokenize(input: String) -> Result(List(TokenWithPosition), LexerError) {
  let lexer = new_lexer(input)
  tokenize_loop(lexer, [])
}

fn tokenize_loop(
  lexer: LexerState,
  tokens: List(TokenWithPosition),
) -> Result(List(TokenWithPosition), LexerError) {
  case next_token(lexer) {
    Ok(#(TokenWithPosition(EOF, _), _)) ->
      list.reverse([
        TokenWithPosition(EOF, Position(lexer.line, lexer.column)),
        ..tokens
      ])
      |> Ok
    Ok(#(token, new_lexer)) -> tokenize_loop(new_lexer, [token, ..tokens])
    Error(err) -> Error(err)
  }
}

pub fn next_token(
  lexer: LexerState,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let lexer = skip_whitespace(lexer)

  case peek_char(lexer) {
    Error(_) ->
      Ok(#(TokenWithPosition(EOF, Position(lexer.line, lexer.column)), lexer))
    Ok(char) -> {
      let position = Position(lexer.line, lexer.column)
      case char {
        "!" -> Ok(#(TokenWithPosition(Bang, position), advance_char(lexer)))
        "$" -> Ok(#(TokenWithPosition(Dollar, position), advance_char(lexer)))
        "&" -> Ok(#(TokenWithPosition(Amp, position), advance_char(lexer)))
        "(" ->
          Ok(#(TokenWithPosition(LeftParen, position), advance_char(lexer)))
        ")" ->
          Ok(#(TokenWithPosition(RightParen, position), advance_char(lexer)))
        ":" -> Ok(#(TokenWithPosition(Colon, position), advance_char(lexer)))
        "=" -> Ok(#(TokenWithPosition(Equals, position), advance_char(lexer)))
        "@" -> Ok(#(TokenWithPosition(At, position), advance_char(lexer)))
        "[" ->
          Ok(#(TokenWithPosition(LeftBracket, position), advance_char(lexer)))
        "]" ->
          Ok(#(TokenWithPosition(RightBracket, position), advance_char(lexer)))
        "{" ->
          Ok(#(TokenWithPosition(LeftBrace, position), advance_char(lexer)))
        "}" ->
          Ok(#(TokenWithPosition(RightBrace, position), advance_char(lexer)))
        "|" -> Ok(#(TokenWithPosition(Pipe, position), advance_char(lexer)))
        "." -> read_spread(lexer, position)
        "\"" -> read_string(lexer, position)
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          read_number(lexer, position)
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
        | "_" -> read_name(lexer, position)
        _ -> Error(UnexpectedCharacter(char, position))
      }
    }
  }
}

fn skip_whitespace(lexer: LexerState) -> LexerState {
  case peek_char(lexer) {
    Ok(" ") | Ok("\t") | Ok("\n") | Ok("\r") | Ok(",") ->
      skip_whitespace(advance_char(lexer))
    _ -> lexer
  }
}

fn peek_char(lexer: LexerState) -> Result(String, Nil) {
  case string.slice(lexer.input, lexer.position, 1) {
    "" -> Error(Nil)
    char -> Ok(char)
  }
}

fn advance_char(lexer: LexerState) -> LexerState {
  case peek_char(lexer) {
    Ok("\n") ->
      LexerState(
        ..lexer,
        position: lexer.position + 1,
        line: lexer.line + 1,
        column: 1,
      )
    Ok(_) ->
      LexerState(
        ..lexer,
        position: lexer.position + 1,
        column: lexer.column + 1,
      )
    Error(_) -> lexer
  }
}

fn read_spread(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let lexer1 = advance_char(lexer)
  let lexer2 = advance_char(lexer1)
  let lexer3 = advance_char(lexer2)

  case string.slice(lexer.input, lexer.position, 3) {
    "..." -> Ok(#(TokenWithPosition(Spread, position), lexer3))
    _ -> Error(UnexpectedCharacter(".", position))
  }
}

fn read_string(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  read_string_loop(advance_char(lexer), position, "")
}

fn read_string_loop(
  lexer: LexerState,
  position: Position,
  acc: String,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case peek_char(lexer) {
    Error(_) -> Error(UnterminatedString(position))
    Ok("\"") ->
      Ok(#(TokenWithPosition(StringValue(acc), position), advance_char(lexer)))
    Ok("\\") -> {
      let lexer = advance_char(lexer)
      case peek_char(lexer) {
        Ok("\"") -> read_string_loop(advance_char(lexer), position, acc <> "\"")
        Ok("\\") -> read_string_loop(advance_char(lexer), position, acc <> "\\")
        Ok("/") -> read_string_loop(advance_char(lexer), position, acc <> "/")
        Ok("b") ->
          read_string_loop(advance_char(lexer), position, acc <> "\u{0008}")
        Ok("f") ->
          read_string_loop(advance_char(lexer), position, acc <> "\u{000C}")
        Ok("n") -> read_string_loop(advance_char(lexer), position, acc <> "\n")
        Ok("r") -> read_string_loop(advance_char(lexer), position, acc <> "\r")
        Ok("t") -> read_string_loop(advance_char(lexer), position, acc <> "\t")
        Ok(char) -> read_string_loop(advance_char(lexer), position, acc <> char)
        Error(_) -> Error(UnterminatedString(position))
      }
    }
    Ok(char) -> read_string_loop(advance_char(lexer), position, acc <> char)
  }
}

fn read_number(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let #(number_str, new_lexer) = read_while(lexer, is_digit_or_dot)

  case string.contains(number_str, ".") {
    True -> {
      case float.parse(number_str) {
        Ok(value) ->
          Ok(#(TokenWithPosition(FloatValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
    False -> {
      case int.parse(number_str) {
        Ok(value) ->
          Ok(#(TokenWithPosition(IntValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
  }
}

fn is_digit_or_dot(c: String) -> Bool {
  is_digit(c) || c == "."
}

fn read_name(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let #(name, new_lexer) = read_while(lexer, is_name_continue)

  let token = case name {
    "query" -> Query
    "mutation" -> Mutation
    "subscription" -> Subscription
    "fragment" -> Fragment
    "on" -> On
    "true" -> TrueKeyword
    "false" -> FalseKeyword
    "null" -> NullKeyword
    _ -> Name(name)
  }

  Ok(#(TokenWithPosition(token, position), new_lexer))
}

fn read_while(
  lexer: LexerState,
  predicate: fn(String) -> Bool,
) -> #(String, LexerState) {
  read_while_loop(lexer, predicate, "")
}

fn read_while_loop(
  lexer: LexerState,
  predicate: fn(String) -> Bool,
  acc: String,
) -> #(String, LexerState) {
  case peek_char(lexer) {
    Ok(char) ->
      case predicate(char) {
        True -> read_while_loop(advance_char(lexer), predicate, acc <> char)
        False -> #(acc, lexer)
      }
    _ -> #(acc, lexer)
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
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
    | "z" -> True
    "A"
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
