//// GraphQL query lexer.
////
//// Operates on a `BitArray` and pattern-matches byte prefixes — no
//// `string.slice(input, position, 1)` per character (which walks UTF-8
//// boundaries), no `acc <> char` accumulation per character (O(n²) on
//// Erlang). Lexemes (names, numbers, strings) are captured as byte spans
//// of the *remaining* input and converted to `String` once at token
//// boundaries.
////
//// GraphQL identifiers, numbers, and most of the punctuation set are
//// ASCII; non-ASCII is only allowed inside strings/block strings, which
//// we still handle correctly via codepoint pattern matching in the
//// escape paths.

import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}

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
  LexerState(remaining: BitArray, line: Int, column: Int)
}

pub fn new_lexer(input: String) -> LexerState {
  LexerState(remaining: bit_array.from_string(input), line: 1, column: 1)
}

pub fn tokenize(input: String) -> Result(List(TokenWithPosition), LexerError) {
  tokenize_loop(new_lexer(input), [])
}

fn tokenize_loop(
  lexer: LexerState,
  tokens: List(TokenWithPosition),
) -> Result(List(TokenWithPosition), LexerError) {
  case next_token(lexer) {
    Ok(#(TokenWithPosition(EOF, _) as eof, _)) ->
      Ok(list.reverse([eof, ..tokens]))
    Ok(#(token, new_lexer)) -> tokenize_loop(new_lexer, [token, ..tokens])
    Error(err) -> Error(err)
  }
}

pub fn next_token(
  lexer: LexerState,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let lexer = skip_whitespace(lexer)
  let position = Position(lexer.line, lexer.column)

  case lexer.remaining {
    <<>> -> Ok(#(TokenWithPosition(EOF, position), lexer))
    <<"!":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Bang, position), advance(lexer, rest, 1)))
    <<"$":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Dollar, position), advance(lexer, rest, 1)))
    <<"&":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Amp, position), advance(lexer, rest, 1)))
    <<"(":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(LeftParen, position), advance(lexer, rest, 1)))
    <<")":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(RightParen, position), advance(lexer, rest, 1)))
    <<":":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Colon, position), advance(lexer, rest, 1)))
    <<"=":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Equals, position), advance(lexer, rest, 1)))
    <<"@":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(At, position), advance(lexer, rest, 1)))
    <<"[":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(LeftBracket, position), advance(lexer, rest, 1)))
    <<"]":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(RightBracket, position), advance(lexer, rest, 1)))
    <<"{":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(LeftBrace, position), advance(lexer, rest, 1)))
    <<"}":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(RightBrace, position), advance(lexer, rest, 1)))
    <<"|":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Pipe, position), advance(lexer, rest, 1)))
    <<"...":utf8, rest:bits>> ->
      Ok(#(TokenWithPosition(Spread, position), advance(lexer, rest, 3)))
    <<".":utf8, _:bits>> -> Error(UnexpectedCharacter(".", position))
    <<"\"\"\"":utf8, rest:bits>> ->
      read_block_string(advance(lexer, rest, 3), position)
    <<"\"":utf8, rest:bits>> -> read_string(advance(lexer, rest, 1), position)
    <<"-":utf8, _:bits>> -> read_negative_number(lexer, position)
    <<b, _:bits>> if b >= 48 && b <= 57 -> read_number(lexer, position, "")
    // a-z | A-Z | _
    <<b, _:bits>> if b >= 97 && b <= 122 -> read_name(lexer, position)
    <<b, _:bits>> if b >= 65 && b <= 90 -> read_name(lexer, position)
    <<95, _:bits>> -> read_name(lexer, position)
    <<b, _:bits>> -> Error(UnexpectedCharacter(byte_to_string(b), position))
    _ -> Error(UnexpectedCharacter("", position))
  }
}

// ---------------------------------------------------------------------------
// Whitespace + comments
// ---------------------------------------------------------------------------

fn skip_whitespace(lexer: LexerState) -> LexerState {
  case lexer.remaining {
    <<" ":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\t":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\r":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<",":utf8, rest:bits>> -> skip_whitespace(advance(lexer, rest, 1))
    <<"\n":utf8, rest:bits>> ->
      skip_whitespace(LexerState(rest, lexer.line + 1, 1))
    <<"#":utf8, rest:bits>> ->
      skip_whitespace(skip_comment(advance(lexer, rest, 1)))
    _ -> lexer
  }
}

fn skip_comment(lexer: LexerState) -> LexerState {
  case lexer.remaining {
    <<>> -> lexer
    <<"\n":utf8, _:bits>> -> lexer
    <<_, rest:bits>> -> skip_comment(advance(lexer, rest, 1))
    _ -> lexer
  }
}

// ---------------------------------------------------------------------------
// Names + numbers — span capture via byte count, single slice at end
// ---------------------------------------------------------------------------

fn read_name(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let len = scan_name_bytes(lexer.remaining, 0)
  let assert Ok(slice) = bit_array.slice(lexer.remaining, 0, len)
  let assert Ok(name) = bit_array.to_string(slice)
  let new_lexer =
    LexerState(
      remaining: drop_bytes(lexer.remaining, len),
      line: lexer.line,
      column: lexer.column + len,
    )
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

fn read_negative_number(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case lexer.remaining {
    <<"-":utf8, b, _:bits>> if b >= 48 && b <= 57 -> {
      let assert Ok(rest) =
        bit_array.slice(lexer.remaining, 1, byte_size_of(lexer.remaining) - 1)
      read_number(advance(lexer, rest, 1), position, "-")
    }
    _ -> Error(UnexpectedCharacter("-", position))
  }
}

fn read_number(
  lexer: LexerState,
  position: Position,
  prefix: String,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let int_len = scan_digits(lexer.remaining, 0)
  let assert Ok(int_slice) = bit_array.slice(lexer.remaining, 0, int_len)
  let assert Ok(int_part) = bit_array.to_string(int_slice)
  let after_int = drop_bytes(lexer.remaining, int_len)

  let #(frac_part, after_frac, frac_consumed) = case after_int {
    <<".":utf8, b, _:bits>> if b >= 48 && b <= 57 -> {
      let frac_digits = scan_digits(drop_bytes(after_int, 1), 0)
      let assert Ok(frac_slice) = bit_array.slice(after_int, 1, frac_digits)
      let assert Ok(frac_str) = bit_array.to_string(frac_slice)
      #(
        "." <> frac_str,
        drop_bytes(after_int, 1 + frac_digits),
        1 + frac_digits,
      )
    }
    _ -> #("", after_int, 0)
  }

  let #(exp_part, after_exp, exp_consumed) = case after_frac {
    <<"e":utf8, _:bits>> -> read_exponent(after_frac, "e")
    <<"E":utf8, _:bits>> -> read_exponent(after_frac, "E")
    _ -> #("", after_frac, 0)
  }

  let new_col = lexer.column + int_len + frac_consumed + exp_consumed
  let new_lexer = LexerState(after_exp, lexer.line, new_col)

  let number_str = prefix <> int_part <> frac_part <> exp_part
  let is_float = frac_part != "" || exp_part != ""

  case is_float {
    True -> {
      let parse_str = case string.contains(number_str, ".") {
        True -> number_str
        False ->
          case string.split_once(number_str, "e") {
            Ok(#(base, exp)) -> base <> ".0e" <> exp
            Error(_) ->
              case string.split_once(number_str, "E") {
                Ok(#(base, exp)) -> base <> ".0E" <> exp
                Error(_) -> number_str
              }
          }
      }
      case float.parse(parse_str) {
        Ok(v) -> Ok(#(TokenWithPosition(FloatValue(v), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
    False ->
      case int.parse(number_str) {
        Ok(v) -> Ok(#(TokenWithPosition(IntValue(v), position), new_lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
  }
}

fn read_exponent(input: BitArray, e_char: String) -> #(String, BitArray, Int) {
  let after_e = drop_bytes(input, 1)
  let #(sign_str, after_sign, sign_len) = case after_e {
    <<"+":utf8, _:bits>> -> #("+", drop_bytes(after_e, 1), 1)
    <<"-":utf8, _:bits>> -> #("-", drop_bytes(after_e, 1), 1)
    _ -> #("", after_e, 0)
  }
  let digit_len = scan_digits(after_sign, 0)
  let assert Ok(digit_slice) = bit_array.slice(after_sign, 0, digit_len)
  let assert Ok(digits) = bit_array.to_string(digit_slice)
  #(
    e_char <> sign_str <> digits,
    drop_bytes(after_sign, digit_len),
    1 + sign_len + digit_len,
  )
}

fn scan_digits(b: BitArray, acc: Int) -> Int {
  case b {
    <<byte, rest:bits>> if byte >= 48 && byte <= 57 ->
      scan_digits(rest, acc + 1)
    _ -> acc
  }
}

// ---------------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------------

fn read_string(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  read_string_loop(lexer, position, string_tree.new())
}

fn read_string_loop(
  lexer: LexerState,
  position: Position,
  acc: StringTree,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case lexer.remaining {
    <<>> -> Error(UnterminatedString(position))
    <<"\"":utf8, rest:bits>> ->
      Ok(#(
        TokenWithPosition(StringValue(string_tree.to_string(acc)), position),
        advance(lexer, rest, 1),
      ))
    <<"\\":utf8, rest:bits>> -> {
      let lexer = advance(lexer, rest, 1)
      case lexer.remaining {
        <<"\"":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\""),
          )
        <<"\\":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\\"),
          )
        <<"/":utf8, r:bits>> ->
          read_string_loop(advance(lexer, r, 1), position, append_str(acc, "/"))
        <<"b":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\u{0008}"),
          )
        <<"f":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\u{000C}"),
          )
        <<"n":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\n"),
          )
        <<"r":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\r"),
          )
        <<"t":utf8, r:bits>> ->
          read_string_loop(
            advance(lexer, r, 1),
            position,
            append_str(acc, "\t"),
          )
        <<"u":utf8, r:bits>> ->
          read_unicode_escape(advance(lexer, r, 1), position, acc)
        _ -> Error(UnterminatedString(position))
      }
    }
    _ -> {
      // Capture the longest run of plain (non-quote, non-backslash) bytes
      // so we don't re-enter the recursive loop for every byte.
      let run = scan_string_run(lexer.remaining, 0)
      let assert Ok(slice) = bit_array.slice(lexer.remaining, 0, run)
      let assert Ok(text) = bit_array.to_string(slice)
      // count newlines for line/col bookkeeping — uncommon in single-line
      // strings, but still cheap
      let #(new_line, new_col) =
        advance_line_col(slice, lexer.line, lexer.column)
      let next = LexerState(drop_bytes(lexer.remaining, run), new_line, new_col)
      read_string_loop(next, position, append_str(acc, text))
    }
  }
}

fn scan_string_run(b: BitArray, acc: Int) -> Int {
  case b {
    <<"\"":utf8, _:bits>> -> acc
    <<"\\":utf8, _:bits>> -> acc
    <<>> -> acc
    <<_, rest:bits>> -> scan_string_run(rest, acc + 1)
    _ -> acc
  }
}

fn read_unicode_escape(
  lexer: LexerState,
  position: Position,
  acc: StringTree,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case read_hex_digit(lexer, position) {
    Error(e) -> Error(e)
    Ok(#(h1, lexer)) ->
      case read_hex_digit(lexer, position) {
        Error(e) -> Error(e)
        Ok(#(h2, lexer)) ->
          case read_hex_digit(lexer, position) {
            Error(e) -> Error(e)
            Ok(#(h3, lexer)) ->
              case read_hex_digit(lexer, position) {
                Error(e) -> Error(e)
                Ok(#(h4, lexer)) -> {
                  let codepoint = h1 * 4096 + h2 * 256 + h3 * 16 + h4
                  case string.utf_codepoint(codepoint) {
                    Ok(cp) ->
                      read_string_loop(
                        lexer,
                        position,
                        append_str(acc, string.from_utf_codepoints([cp])),
                      )
                    Error(_) -> Error(InvalidNumber("\\uXXXX", position))
                  }
                }
              }
          }
      }
  }
}

fn read_hex_digit(
  lexer: LexerState,
  position: Position,
) -> Result(#(Int, LexerState), LexerError) {
  case lexer.remaining {
    <<b, rest:bits>> if b >= 48 && b <= 57 ->
      Ok(#(b - 48, advance(lexer, rest, 1)))
    <<b, rest:bits>> if b >= 65 && b <= 70 ->
      Ok(#(b - 55, advance(lexer, rest, 1)))
    <<b, rest:bits>> if b >= 97 && b <= 102 ->
      Ok(#(b - 87, advance(lexer, rest, 1)))
    <<b, _:bits>> -> Error(UnexpectedCharacter(byte_to_string(b), position))
    _ -> Error(UnterminatedString(position))
  }
}

// ---------------------------------------------------------------------------
// Block strings
// ---------------------------------------------------------------------------

fn read_block_string(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  read_block_string_loop(lexer, position, string_tree.new())
}

fn read_block_string_loop(
  lexer: LexerState,
  position: Position,
  acc: StringTree,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case lexer.remaining {
    <<"\\\"\"\"":utf8, rest:bits>> ->
      read_block_string_loop(
        advance(lexer, rest, 4),
        position,
        append_str(acc, "\"\"\""),
      )
    <<"\"\"\"":utf8, rest:bits>> ->
      Ok(#(
        TokenWithPosition(
          StringValue(block_string_value(string_tree.to_string(acc))),
          position,
        ),
        advance(lexer, rest, 3),
      ))
    <<>> -> Error(UnterminatedString(position))
    <<"\n":utf8, rest:bits>> ->
      read_block_string_loop(
        LexerState(rest, lexer.line + 1, 1),
        position,
        append_str(acc, "\n"),
      )
    <<b, rest:bits>> ->
      read_block_string_loop(
        advance(lexer, rest, 1),
        position,
        append_str(acc, byte_to_string(b)),
      )
    _ -> Error(UnterminatedString(position))
  }
}

fn block_string_value(raw: String) -> String {
  let lines = string.split(raw, "\n")
  let lines = list.map(lines, fn(l) { string.replace(l, "\r", "") })

  let common_indent = find_common_indent(lines)

  let lines =
    list.index_map(lines, fn(line, i) {
      case i == 0 {
        True -> line
        False -> string.drop_start(line, common_indent)
      }
    })

  let lines = drop_leading_blank(lines)
  let lines = drop_leading_blank(list.reverse(lines)) |> list.reverse

  string.join(lines, "\n")
}

fn find_common_indent(lines: List(String)) -> Int {
  list.fold(lines, -1, fn(acc, line) {
    let bytes = bit_array.from_string(line)
    let indent = count_leading_ws_bytes(bytes, 0)
    let is_blank = indent == byte_size_of(bytes)
    case is_blank {
      True -> acc
      False ->
        case acc == -1 || indent < acc {
          True -> indent
          False -> acc
        }
    }
  })
  |> fn(n) {
    case n == -1 {
      True -> 0
      False -> n
    }
  }
}

fn count_leading_ws_bytes(b: BitArray, acc: Int) -> Int {
  case b {
    <<" ":utf8, rest:bits>> -> count_leading_ws_bytes(rest, acc + 1)
    <<"\t":utf8, rest:bits>> -> count_leading_ws_bytes(rest, acc + 1)
    _ -> acc
  }
}

fn drop_leading_blank(lines: List(String)) -> List(String) {
  case lines {
    [] -> []
    [first, ..rest] ->
      case string.trim(first) == "" {
        True -> drop_leading_blank(rest)
        False -> lines
      }
  }
}

// ---------------------------------------------------------------------------
// Byte / state helpers
// ---------------------------------------------------------------------------

fn advance(lexer: LexerState, rest: BitArray, cols: Int) -> LexerState {
  LexerState(rest, lexer.line, lexer.column + cols)
}

fn drop_bytes(b: BitArray, n: Int) -> BitArray {
  case bit_array.slice(b, n, byte_size_of(b) - n) {
    Ok(slice) -> slice
    Error(_) -> <<>>
  }
}

fn byte_size_of(b: BitArray) -> Int {
  bit_array.byte_size(b)
}

fn append_str(t: StringTree, s: String) -> StringTree {
  string_tree.append(t, s)
}

fn advance_line_col(b: BitArray, line: Int, col: Int) -> #(Int, Int) {
  case b {
    <<>> -> #(line, col)
    <<"\n":utf8, rest:bits>> -> advance_line_col(rest, line + 1, 1)
    <<_, rest:bits>> -> advance_line_col(rest, line, col + 1)
    _ -> #(line, col)
  }
}

fn byte_to_string(b: Int) -> String {
  case bit_array.to_string(<<b>>) {
    Ok(s) -> s
    Error(_) -> ""
  }
}
