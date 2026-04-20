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
        "-" -> read_negative_number(lexer, position)
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
    Ok("#") -> skip_whitespace(skip_comment(advance_char(lexer)))
    _ -> lexer
  }
}

fn skip_comment(lexer: LexerState) -> LexerState {
  case peek_char(lexer) {
    Ok("\n") | Error(_) -> lexer
    Ok(_) -> skip_comment(advance_char(lexer))
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
  case string.slice(lexer.input, lexer.position, 3) {
    "\"\"\"" ->
      read_block_string(
        advance_char(advance_char(advance_char(lexer))),
        position,
        "",
      )
    _ -> read_string_loop(advance_char(lexer), position, "")
  }
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
        Ok("u") -> read_unicode_escape(advance_char(lexer), position, acc)
        Ok(char) -> read_string_loop(advance_char(lexer), position, acc <> char)
        Error(_) -> Error(UnterminatedString(position))
      }
    }
    Ok(char) -> read_string_loop(advance_char(lexer), position, acc <> char)
  }
}

fn read_unicode_escape(
  lexer: LexerState,
  position: Position,
  acc: String,
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
                        acc <> string.from_utf_codepoints([cp]),
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
  case peek_char(lexer) {
    Ok("0") -> Ok(#(0, advance_char(lexer)))
    Ok("1") -> Ok(#(1, advance_char(lexer)))
    Ok("2") -> Ok(#(2, advance_char(lexer)))
    Ok("3") -> Ok(#(3, advance_char(lexer)))
    Ok("4") -> Ok(#(4, advance_char(lexer)))
    Ok("5") -> Ok(#(5, advance_char(lexer)))
    Ok("6") -> Ok(#(6, advance_char(lexer)))
    Ok("7") -> Ok(#(7, advance_char(lexer)))
    Ok("8") -> Ok(#(8, advance_char(lexer)))
    Ok("9") -> Ok(#(9, advance_char(lexer)))
    Ok("a") | Ok("A") -> Ok(#(10, advance_char(lexer)))
    Ok("b") | Ok("B") -> Ok(#(11, advance_char(lexer)))
    Ok("c") | Ok("C") -> Ok(#(12, advance_char(lexer)))
    Ok("d") | Ok("D") -> Ok(#(13, advance_char(lexer)))
    Ok("e") | Ok("E") -> Ok(#(14, advance_char(lexer)))
    Ok("f") | Ok("F") -> Ok(#(15, advance_char(lexer)))
    Ok(c) -> Error(UnexpectedCharacter(c, position))
    Error(_) -> Error(UnterminatedString(position))
  }
}

fn read_block_string(
  lexer: LexerState,
  position: Position,
  acc: String,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  case string.slice(lexer.input, lexer.position, 4) {
    "\\\"\"\"" -> {
      let lexer = advance_char(advance_char(advance_char(advance_char(lexer))))
      read_block_string(lexer, position, acc <> "\"\"\"")
    }
    _ ->
      case string.slice(lexer.input, lexer.position, 3) {
        "\"\"\"" -> {
          let lexer = advance_char(advance_char(advance_char(lexer)))
          Ok(#(
            TokenWithPosition(StringValue(block_string_value(acc)), position),
            lexer,
          ))
        }
        _ ->
          case peek_char(lexer) {
            Error(_) -> Error(UnterminatedString(position))
            Ok(char) ->
              read_block_string(advance_char(lexer), position, acc <> char)
          }
      }
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
  let lines = drop_trailing_blank(list.reverse(lines)) |> list.reverse

  string.join(lines, "\n")
}

fn find_common_indent(lines: List(String)) -> Int {
  list.fold(lines, -1, fn(acc, line) {
    case list.is_empty(lines) {
      True -> acc
      False -> {
        let indent = count_leading_whitespace(line)
        let is_blank = indent == string.length(line)
        case is_blank {
          True -> acc
          False ->
            case acc == -1 || indent < acc {
              True -> indent
              False -> acc
            }
        }
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

fn count_leading_whitespace(s: String) -> Int {
  count_leading_whitespace_loop(s, 0)
}

fn count_leading_whitespace_loop(s: String, count: Int) -> Int {
  case string.slice(s, count, 1) {
    " " | "\t" -> count_leading_whitespace_loop(s, count + 1)
    _ -> count
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

fn drop_trailing_blank(lines: List(String)) -> List(String) {
  drop_leading_blank(lines)
}

fn read_negative_number(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let lexer = advance_char(lexer)
  case peek_char(lexer) {
    Ok("0")
    | Ok("1")
    | Ok("2")
    | Ok("3")
    | Ok("4")
    | Ok("5")
    | Ok("6")
    | Ok("7")
    | Ok("8")
    | Ok("9") -> read_number_with_prefix(lexer, position, "-")
    _ -> Error(UnexpectedCharacter("-", position))
  }
}

fn read_number(
  lexer: LexerState,
  position: Position,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  read_number_with_prefix(lexer, position, "")
}

fn read_number_with_prefix(
  lexer: LexerState,
  position: Position,
  prefix: String,
) -> Result(#(TokenWithPosition, LexerState), LexerError) {
  let #(int_part, lexer) = read_while(lexer, is_digit)

  let #(frac_part, lexer) = case peek_char(lexer) {
    Ok(".") -> {
      let lexer = advance_char(lexer)
      let #(digits, lexer) = read_while(lexer, is_digit)
      #("." <> digits, lexer)
    }
    _ -> #("", lexer)
  }

  let #(exp_part, lexer) = case peek_char(lexer) {
    Ok("e") | Ok("E") -> {
      let e_char = case peek_char(lexer) {
        Ok(c) -> c
        _ -> "e"
      }
      let lexer = advance_char(lexer)
      let #(sign, lexer) = case peek_char(lexer) {
        Ok("+") | Ok("-") -> {
          let s = case peek_char(lexer) {
            Ok(c) -> c
            _ -> ""
          }
          #(s, advance_char(lexer))
        }
        _ -> #("", lexer)
      }
      let #(exp_digits, lexer) = read_while(lexer, is_digit)
      #(e_char <> sign <> exp_digits, lexer)
    }
    _ -> #("", lexer)
  }

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
        Ok(value) ->
          Ok(#(TokenWithPosition(FloatValue(value), position), lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
    }
    False ->
      case int.parse(number_str) {
        Ok(value) -> Ok(#(TokenWithPosition(IntValue(value), position), lexer))
        Error(_) -> Error(InvalidNumber(number_str, position))
      }
  }
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
