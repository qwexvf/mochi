//// SDL (Schema Definition Language) lexer.
////
//// Same byte-level pattern-matching strategy as `mochi/internal/lexer` — operates
//// on `BitArray` prefixes, captures lexemes via span slicing rather than
//// per-byte string concatenation.

import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}

pub type SdlToken {
  // Punctuators
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

  // SDL keywords
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

  // Values
  Name(value: String)
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)
  BooleanValue(value: Bool)

  // Special
  EOF
  Comment(content: String)
  Description(content: String)
}

pub type Position {
  Position(line: Int, column: Int)
}

pub type SdlTokenWithPosition {
  SdlTokenWithPosition(token: SdlToken, position: Position)
}

pub type SdlLexerError {
  UnexpectedCharacter(character: String, position: Position)
  InvalidNumber(value: String, position: Position)
  UnterminatedString(position: Position)
  UnterminatedDescription(position: Position)
}

pub type SdlLexerState {
  SdlLexerState(remaining: BitArray, line: Int, column: Int)
}

pub fn new_sdl_lexer(input: String) -> SdlLexerState {
  SdlLexerState(remaining: bit_array.from_string(input), line: 1, column: 1)
}

pub fn tokenize_sdl(
  input: String,
) -> Result(List(SdlTokenWithPosition), SdlLexerError) {
  tokenize_loop(new_sdl_lexer(input), [])
}

fn tokenize_loop(
  lexer: SdlLexerState,
  tokens: List(SdlTokenWithPosition),
) -> Result(List(SdlTokenWithPosition), SdlLexerError) {
  case next_sdl_token(lexer) {
    Ok(#(SdlTokenWithPosition(EOF, _) as eof, _)) ->
      Ok(list.reverse([eof, ..tokens]))
    Ok(#(token, new_lexer)) -> tokenize_loop(new_lexer, [token, ..tokens])
    Error(err) -> Error(err)
  }
}

pub fn next_sdl_token(
  lexer: SdlLexerState,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  let lexer = skip_whitespace(lexer)
  let position = Position(lexer.line, lexer.column)

  case lexer.remaining {
    <<>> -> Ok(#(SdlTokenWithPosition(EOF, position), lexer))
    <<"{":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(LeftBrace, position), advance(lexer, rest, 1)))
    <<"}":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(RightBrace, position), advance(lexer, rest, 1)))
    <<"(":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(LeftParen, position), advance(lexer, rest, 1)))
    <<")":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(RightParen, position), advance(lexer, rest, 1)))
    <<"[":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(LeftBracket, position), advance(lexer, rest, 1)))
    <<"]":utf8, rest:bits>> ->
      Ok(#(
        SdlTokenWithPosition(RightBracket, position),
        advance(lexer, rest, 1),
      ))
    <<":":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(Colon, position), advance(lexer, rest, 1)))
    <<"!":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(Bang, position), advance(lexer, rest, 1)))
    <<"=":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(Equals, position), advance(lexer, rest, 1)))
    <<"@":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(At, position), advance(lexer, rest, 1)))
    <<"|":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(Pipe, position), advance(lexer, rest, 1)))
    <<"&":utf8, rest:bits>> ->
      Ok(#(SdlTokenWithPosition(Amp, position), advance(lexer, rest, 1)))
    <<"\"\"\"":utf8, rest:bits>> ->
      read_description(advance(lexer, rest, 3), position)
    <<"\"":utf8, rest:bits>> -> read_string(advance(lexer, rest, 1), position)
    <<"#":utf8, rest:bits>> -> read_comment(advance(lexer, rest, 1), position)
    <<"-":utf8, _:bits>> -> read_number(lexer, position)
    <<b, _:bits>> if b >= 48 && b <= 57 -> read_number(lexer, position)
    <<b, _:bits>> if b >= 97 && b <= 122 ->
      read_name_or_keyword(lexer, position)
    <<b, _:bits>> if b >= 65 && b <= 90 -> read_name_or_keyword(lexer, position)
    <<95, _:bits>> -> read_name_or_keyword(lexer, position)
    <<b, _:bits>> -> Error(UnexpectedCharacter(byte_to_string(b), position))
    _ -> Error(UnexpectedCharacter("", position))
  }
}

// ---------------------------------------------------------------------------
// Whitespace
// ---------------------------------------------------------------------------

fn skip_whitespace(lexer: SdlLexerState) -> SdlLexerState {
  case lexer.remaining {
    <<" ":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\t":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\r":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<",":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\n":utf8, rest:bits>> ->
      skip_whitespace(SdlLexerState(rest, lexer.line + 1, 1))
    _ -> lexer
  }
}

// ---------------------------------------------------------------------------
// Names + keywords
// ---------------------------------------------------------------------------

fn read_name_or_keyword(
  lexer: SdlLexerState,
  position: Position,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  let len = scan_name_bytes(lexer.remaining, 0)
  let assert Ok(slice) = bit_array.slice(lexer.remaining, 0, len)
  let assert Ok(name) = bit_array.to_string(slice)
  let new_lexer =
    SdlLexerState(
      drop_bytes(lexer.remaining, len),
      lexer.line,
      lexer.column + len,
    )
  let token = case name {
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
    "true" -> BooleanValue(True)
    "false" -> BooleanValue(False)
    _ -> Name(name)
  }
  Ok(#(SdlTokenWithPosition(token, position), new_lexer))
}

fn scan_name_bytes(b: BitArray, acc: Int) -> Int {
  case b {
    <<byte, rest:bits>> if byte >= 97 && byte <= 122 ->
      scan_name_bytes(rest, acc + 1)
    <<byte, rest:bits>> if byte >= 65 && byte <= 90 ->
      scan_name_bytes(rest, acc + 1)
    <<byte, rest:bits>> if byte >= 48 && byte <= 57 ->
      scan_name_bytes(rest, acc + 1)
    <<95, rest:bits>> -> scan_name_bytes(rest, acc + 1)
    _ -> acc
  }
}

// ---------------------------------------------------------------------------
// Strings + descriptions + comments
// ---------------------------------------------------------------------------

fn read_string(
  lexer: SdlLexerState,
  position: Position,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  let len = scan_until_quote_or_newline(lexer.remaining, 0)
  case bit_array.slice(lexer.remaining, 0, len) {
    Error(_) -> Error(UnterminatedString(position))
    Ok(slice) -> {
      let assert Ok(value) = bit_array.to_string(slice)
      let after = drop_bytes(lexer.remaining, len)
      case after {
        <<"\"":utf8, rest:bits>> ->
          Ok(#(
            SdlTokenWithPosition(StringValue(value), position),
            SdlLexerState(rest, lexer.line, lexer.column + len + 1),
          ))
        _ -> Error(UnterminatedString(position))
      }
    }
  }
}

fn scan_until_quote_or_newline(b: BitArray, acc: Int) -> Int {
  case b {
    <<"\"":utf8, _:bits>> -> acc
    <<"\n":utf8, _:bits>> -> acc
    <<>> -> acc
    <<_, rest:bits>> -> scan_until_quote_or_newline(rest, acc + 1)
    _ -> acc
  }
}

fn read_description(
  lexer: SdlLexerState,
  position: Position,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  read_description_loop(lexer, position, string_tree.new())
}

fn read_description_loop(
  lexer: SdlLexerState,
  position: Position,
  acc: StringTree,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  case lexer.remaining {
    <<"\"\"\"":utf8, rest:bits>> ->
      Ok(#(
        SdlTokenWithPosition(Description(string_tree.to_string(acc)), position),
        advance(lexer, rest, 3),
      ))
    <<>> -> Error(UnterminatedDescription(position))
    <<"\n":utf8, rest:bits>> ->
      read_description_loop(
        SdlLexerState(rest, lexer.line + 1, 1),
        position,
        string_tree.append(acc, "\n"),
      )
    <<b, rest:bits>> ->
      read_description_loop(
        advance(lexer, rest, 1),
        position,
        string_tree.append(acc, byte_to_string(b)),
      )
    _ -> Error(UnterminatedDescription(position))
  }
}

fn read_comment(
  lexer: SdlLexerState,
  position: Position,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  let len = scan_until_newline(lexer.remaining, 0)
  let assert Ok(slice) = bit_array.slice(lexer.remaining, 0, len)
  let assert Ok(content) = bit_array.to_string(slice)
  let new_lexer =
    SdlLexerState(
      drop_bytes(lexer.remaining, len),
      lexer.line,
      lexer.column + len,
    )
  Ok(#(SdlTokenWithPosition(Comment(content), position), new_lexer))
}

fn scan_until_newline(b: BitArray, acc: Int) -> Int {
  case b {
    <<"\n":utf8, _:bits>> -> acc
    <<>> -> acc
    <<_, rest:bits>> -> scan_until_newline(rest, acc + 1)
    _ -> acc
  }
}

// ---------------------------------------------------------------------------
// Numbers
// ---------------------------------------------------------------------------

fn read_number(
  lexer: SdlLexerState,
  position: Position,
) -> Result(#(SdlTokenWithPosition, SdlLexerState), SdlLexerError) {
  let len = scan_number_bytes(lexer.remaining, 0)
  let assert Ok(slice) = bit_array.slice(lexer.remaining, 0, len)
  let assert Ok(number_str) = bit_array.to_string(slice)
  let new_lexer =
    SdlLexerState(
      drop_bytes(lexer.remaining, len),
      lexer.line,
      lexer.column + len,
    )
  case string.contains(number_str, ".") {
    True ->
      case float.parse(number_str) {
        Ok(value) ->
          Ok(#(SdlTokenWithPosition(FloatValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    False ->
      case int.parse(number_str) {
        Ok(value) ->
          Ok(#(SdlTokenWithPosition(IntValue(value), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
  }
}

fn scan_number_bytes(b: BitArray, acc: Int) -> Int {
  case b {
    <<byte, rest:bits>> if byte >= 48 && byte <= 57 ->
      scan_number_bytes(rest, acc + 1)
    <<".":utf8, rest:bits>> -> scan_number_bytes(rest, acc + 1)
    <<"-":utf8, rest:bits>> -> scan_number_bytes(rest, acc + 1)
    _ -> acc
  }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn advance(lexer: SdlLexerState, rest: BitArray, cols: Int) -> SdlLexerState {
  SdlLexerState(rest, lexer.line, lexer.column + cols)
}

fn drop_bytes(b: BitArray, n: Int) -> BitArray {
  case bit_array.slice(b, n, bit_array.byte_size(b) - n) {
    Ok(slice) -> slice
    Error(_) -> <<>>
  }
}

fn byte_to_string(b: Int) -> String {
  case bit_array.to_string(<<b>>) {
    Ok(s) -> s
    Error(_) -> ""
  }
}
