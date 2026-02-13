// mochi_wisp/fast_parser.gleam
// High-performance GraphQL parser using:
// 1. Binary-based lexer (Erlang FFI) - O(n) vs O(n²)
// 2. Cursor-based token stream - O(1) vs O(n) per token access
//
// This achieves linear time complexity for parsing.

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/ast.{
  type Argument, type Definition, type Document, type Field, type Fragment,
  type Operation, type OperationType, type Selection, type SelectionSet,
  type Type, type Value, type VariableDefinition,
}

// ============================================================================
// Types
// ============================================================================

pub type ParseError {
  LexError(message: String)
  UnexpectedToken(expected: String, got: String)
  UnexpectedEOF(expected: String)
}

/// Token types matching lexer output
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
  FragmentKw
  On
  TrueKw
  FalseKw
  NullKw
  // Values
  Name(String)
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  // Special
  EOF
}

pub type Position {
  Position(line: Int, column: Int)
}

pub type TokenWithPos {
  TokenWithPos(token: Token, position: Position)
}

/// Cursor-based parser state - just holds remaining tokens
/// This is the key optimization: no index, just the tail of the list
pub type Cursor {
  Cursor(remaining: List(TokenWithPos))
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Parse GraphQL query using fast binary lexer
pub fn parse(input: String) -> Result(Document, ParseError) {
  case fast_tokenize(input) {
    Ok(tokens) -> {
      let cursor = Cursor(remaining: tokens)
      case parse_document(cursor) {
        Ok(#(doc, _)) -> Ok(doc)
        Error(e) -> Error(e)
      }
    }
    Error(msg) -> Error(LexError(msg))
  }
}

/// FFI to fast Erlang lexer
@external(erlang, "mochi_fast_lexer_ffi", "tokenize")
fn tokenize_ffi(input: String) -> Result(List(TokenWithPos), String)

fn fast_tokenize(input: String) -> Result(List(TokenWithPos), String) {
  case tokenize_ffi(input) {
    Ok(tokens) -> Ok(convert_tokens(tokens))
    Error(e) -> Error(format_lex_error(e))
  }
}

@external(erlang, "mochi_fast_parser_ffi", "convert_tokens")
fn convert_tokens(tokens: List(TokenWithPos)) -> List(TokenWithPos)

@external(erlang, "mochi_fast_parser_ffi", "format_error")
fn format_lex_error(error: a) -> String

// ============================================================================
// Cursor Operations - All O(1)
// ============================================================================

fn peek(cursor: Cursor) -> Result(TokenWithPos, Nil) {
  case cursor.remaining {
    [first, ..] -> Ok(first)
    [] -> Error(Nil)
  }
}

fn advance(cursor: Cursor) -> Result(#(TokenWithPos, Cursor), Nil) {
  case cursor.remaining {
    [first, ..rest] -> Ok(#(first, Cursor(remaining: rest)))
    [] -> Error(Nil)
  }
}

fn expect(
  cursor: Cursor,
  expected: Token,
  description: String,
) -> Result(#(TokenWithPos, Cursor), ParseError) {
  case advance(cursor) {
    Ok(#(tok, new_cursor)) ->
      case tokens_match(tok.token, expected) {
        True -> Ok(#(tok, new_cursor))
        False -> Error(UnexpectedToken(description, token_to_string(tok.token)))
      }
    Error(_) -> Error(UnexpectedEOF(description))
  }
}

fn tokens_match(actual: Token, expected: Token) -> Bool {
  case actual, expected {
    Name(_), Name(_) -> True
    IntValue(_), IntValue(_) -> True
    FloatValue(_), FloatValue(_) -> True
    StringValue(_), StringValue(_) -> True
    a, b -> a == b
  }
}

fn token_to_string(token: Token) -> String {
  case token {
    Bang -> "!"
    Dollar -> "$"
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    LeftBracket -> "["
    RightBracket -> "]"
    Colon -> ":"
    Spread -> "..."
    Equals -> "="
    At -> "@"
    Pipe -> "|"
    Amp -> "&"
    Query -> "query"
    Mutation -> "mutation"
    Subscription -> "subscription"
    FragmentKw -> "fragment"
    On -> "on"
    TrueKw -> "true"
    FalseKw -> "false"
    NullKw -> "null"
    Name(n) -> "Name(" <> n <> ")"
    IntValue(_) -> "Int"
    FloatValue(_) -> "Float"
    StringValue(_) -> "String"
    EOF -> "EOF"
  }
}

// ============================================================================
// Document Parsing
// ============================================================================

fn parse_document(cursor: Cursor) -> Result(#(Document, Cursor), ParseError) {
  use #(definitions, cursor) <- result.try(parse_definitions(cursor, []))
  Ok(#(ast.Document(definitions), cursor))
}

fn parse_definitions(
  cursor: Cursor,
  acc: List(Definition),
) -> Result(#(List(Definition), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(EOF, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(def, cursor) <- result.try(parse_definition(cursor))
      parse_definitions(cursor, [def, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_definition(cursor: Cursor) -> Result(#(Definition, Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(FragmentKw, _)) -> {
      use #(fragment, cursor) <- result.try(parse_fragment_definition(cursor))
      Ok(#(ast.FragmentDefinition(fragment), cursor))
    }
    _ -> {
      use #(operation, cursor) <- result.try(parse_operation_definition(cursor))
      Ok(#(ast.OperationDefinition(operation), cursor))
    }
  }
}

fn parse_fragment_definition(
  cursor: Cursor,
) -> Result(#(Fragment, Cursor), ParseError) {
  use #(_, cursor) <- result.try(expect(cursor, FragmentKw, "'fragment'"))
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(_, cursor) <- result.try(expect(cursor, On, "'on'"))
  use #(type_condition, cursor) <- result.try(parse_name(cursor))
  use #(directives, cursor) <- result.try(parse_directives(cursor))
  use #(selection_set, cursor) <- result.try(parse_selection_set(cursor))
  Ok(#(
    ast.Fragment(
      name: name,
      type_condition: type_condition,
      directives: directives,
      selection_set: selection_set,
    ),
    cursor,
  ))
}

fn parse_operation_definition(
  cursor: Cursor,
) -> Result(#(Operation, Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(LeftBrace, _)) -> {
      use #(selection_set, cursor) <- result.try(parse_selection_set(cursor))
      Ok(#(ast.ShorthandQuery(selection_set), cursor))
    }
    Ok(TokenWithPos(Query, _))
    | Ok(TokenWithPos(Mutation, _))
    | Ok(TokenWithPos(Subscription, _)) -> {
      use #(op_type, cursor) <- result.try(parse_operation_type(cursor))
      use #(name, cursor) <- result.try(parse_optional_name(cursor))
      use #(variable_defs, cursor) <- result.try(parse_variable_definitions(
        cursor,
      ))
      use #(directives, cursor) <- result.try(parse_directives(cursor))
      use #(selection_set, cursor) <- result.try(parse_selection_set(cursor))
      Ok(#(
        ast.Operation(
          operation_type: op_type,
          name: name,
          variable_definitions: variable_defs,
          directives: directives,
          selection_set: selection_set,
        ),
        cursor,
      ))
    }
    Ok(TokenWithPos(tok, _)) ->
      Error(UnexpectedToken("operation or '{'", token_to_string(tok)))
    Error(_) -> Error(UnexpectedEOF("operation or '{'"))
  }
}

fn parse_operation_type(
  cursor: Cursor,
) -> Result(#(OperationType, Cursor), ParseError) {
  case advance(cursor) {
    Ok(#(TokenWithPos(Query, _), cursor)) -> Ok(#(ast.Query, cursor))
    Ok(#(TokenWithPos(Mutation, _), cursor)) -> Ok(#(ast.Mutation, cursor))
    Ok(#(TokenWithPos(Subscription, _), cursor)) ->
      Ok(#(ast.Subscription, cursor))
    Ok(#(TokenWithPos(tok, _), _)) ->
      Error(UnexpectedToken("query/mutation/subscription", token_to_string(tok)))
    Error(_) -> Error(UnexpectedEOF("operation type"))
  }
}

fn parse_optional_name(
  cursor: Cursor,
) -> Result(#(Option(String), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(Name(n), _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> Ok(#(Some(n), cursor))
        Error(_) -> Ok(#(None, cursor))
      }
    }
    _ -> Ok(#(None, cursor))
  }
}

fn parse_name(cursor: Cursor) -> Result(#(String, Cursor), ParseError) {
  case advance(cursor) {
    Ok(#(TokenWithPos(Name(n), _), cursor)) -> Ok(#(n, cursor))
    Ok(#(TokenWithPos(tok, _), _)) ->
      Error(UnexpectedToken("name", token_to_string(tok)))
    Error(_) -> Error(UnexpectedEOF("name"))
  }
}

// ============================================================================
// Selection Set Parsing
// ============================================================================

fn parse_selection_set(
  cursor: Cursor,
) -> Result(#(SelectionSet, Cursor), ParseError) {
  use #(_, cursor) <- result.try(expect(cursor, LeftBrace, "'{'"))
  use #(selections, cursor) <- result.try(parse_selections(cursor, []))
  use #(_, cursor) <- result.try(expect(cursor, RightBrace, "'}'"))
  Ok(#(ast.SelectionSet(selections), cursor))
}

fn parse_selections(
  cursor: Cursor,
  acc: List(Selection),
) -> Result(#(List(Selection), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(RightBrace, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(selection, cursor) <- result.try(parse_selection(cursor))
      parse_selections(cursor, [selection, ..acc])
    }
    Error(_) -> Error(UnexpectedEOF("selection or '}'"))
  }
}

fn parse_selection(cursor: Cursor) -> Result(#(Selection, Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(Spread, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) ->
          case peek(cursor) {
            Ok(TokenWithPos(On, _)) | Ok(TokenWithPos(LeftBrace, _)) ->
              parse_inline_fragment(cursor)
            Ok(TokenWithPos(Name(_), _)) -> parse_fragment_spread(cursor)
            _ -> parse_inline_fragment(cursor)
          }
        Error(_) -> Error(UnexpectedEOF("fragment spread or inline fragment"))
      }
    }
    _ -> {
      use #(field, cursor) <- result.try(parse_field(cursor))
      Ok(#(ast.FieldSelection(field), cursor))
    }
  }
}

fn parse_fragment_spread(
  cursor: Cursor,
) -> Result(#(Selection, Cursor), ParseError) {
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(directives, cursor) <- result.try(parse_directives(cursor))
  Ok(#(
    ast.FragmentSpread(ast.FragmentSpreadValue(
      name: name,
      directives: directives,
    )),
    cursor,
  ))
}

fn parse_inline_fragment(
  cursor: Cursor,
) -> Result(#(Selection, Cursor), ParseError) {
  use #(type_condition, cursor) <- result.try(case peek(cursor) {
    Ok(TokenWithPos(On, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(name, cursor) <- result.try(parse_name(cursor))
          Ok(#(Some(name), cursor))
        }
        Error(_) -> Ok(#(None, cursor))
      }
    }
    _ -> Ok(#(None, cursor))
  })
  use #(directives, cursor) <- result.try(parse_directives(cursor))
  use #(selection_set, cursor) <- result.try(parse_selection_set(cursor))
  Ok(#(
    ast.InlineFragment(ast.InlineFragmentValue(
      type_condition: type_condition,
      directives: directives,
      selection_set: selection_set,
    )),
    cursor,
  ))
}

fn parse_field(cursor: Cursor) -> Result(#(Field, Cursor), ParseError) {
  use #(name_or_alias, cursor) <- result.try(parse_name(cursor))

  // Check for alias
  use #(name, alias, cursor) <- result.try(case peek(cursor) {
    Ok(TokenWithPos(Colon, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(actual_name, cursor) <- result.try(parse_name(cursor))
          Ok(#(actual_name, Some(name_or_alias), cursor))
        }
        Error(_) -> Ok(#(name_or_alias, None, cursor))
      }
    }
    _ -> Ok(#(name_or_alias, None, cursor))
  })

  use #(arguments, cursor) <- result.try(parse_arguments(cursor))
  use #(directives, cursor) <- result.try(parse_directives(cursor))
  use #(selection_set, cursor) <- result.try(parse_optional_selection_set(
    cursor,
  ))

  Ok(#(
    ast.Field(
      alias: alias,
      name: name,
      arguments: arguments,
      directives: directives,
      selection_set: selection_set,
    ),
    cursor,
  ))
}

fn parse_optional_selection_set(
  cursor: Cursor,
) -> Result(#(Option(SelectionSet), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(LeftBrace, _)) -> {
      use #(selection_set, cursor) <- result.try(parse_selection_set(cursor))
      Ok(#(Some(selection_set), cursor))
    }
    _ -> Ok(#(None, cursor))
  }
}

// ============================================================================
// Arguments Parsing
// ============================================================================

fn parse_arguments(
  cursor: Cursor,
) -> Result(#(List(Argument), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(LeftParen, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(args, cursor) <- result.try(parse_argument_list(cursor, []))
          use #(_, cursor) <- result.try(expect(cursor, RightParen, "')'"))
          Ok(#(args, cursor))
        }
        Error(_) -> Ok(#([], cursor))
      }
    }
    _ -> Ok(#([], cursor))
  }
}

fn parse_argument_list(
  cursor: Cursor,
  acc: List(Argument),
) -> Result(#(List(Argument), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(RightParen, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(arg, cursor) <- result.try(parse_argument(cursor))
      parse_argument_list(cursor, [arg, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_argument(cursor: Cursor) -> Result(#(Argument, Cursor), ParseError) {
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(_, cursor) <- result.try(expect(cursor, Colon, "':'"))
  use #(value, cursor) <- result.try(parse_value(cursor))
  Ok(#(ast.Argument(name: name, value: value), cursor))
}

// ============================================================================
// Value Parsing
// ============================================================================

fn parse_value(cursor: Cursor) -> Result(#(Value, Cursor), ParseError) {
  case advance(cursor) {
    Ok(#(TokenWithPos(IntValue(n), _), cursor)) ->
      Ok(#(ast.IntValue(n), cursor))
    Ok(#(TokenWithPos(FloatValue(f), _), cursor)) ->
      Ok(#(ast.FloatValue(f), cursor))
    Ok(#(TokenWithPos(StringValue(s), _), cursor)) ->
      Ok(#(ast.StringValue(s), cursor))
    Ok(#(TokenWithPos(TrueKw, _), cursor)) ->
      Ok(#(ast.BooleanValue(True), cursor))
    Ok(#(TokenWithPos(FalseKw, _), cursor)) ->
      Ok(#(ast.BooleanValue(False), cursor))
    Ok(#(TokenWithPos(NullKw, _), cursor)) -> Ok(#(ast.NullValue, cursor))
    Ok(#(TokenWithPos(Dollar, _), cursor)) -> {
      use #(name, cursor) <- result.try(parse_name(cursor))
      Ok(#(ast.VariableValue(name), cursor))
    }
    Ok(#(TokenWithPos(LeftBracket, _), cursor)) -> parse_list_value(cursor)
    Ok(#(TokenWithPos(LeftBrace, _), cursor)) -> parse_object_value(cursor)
    Ok(#(TokenWithPos(Name(n), _), cursor)) -> Ok(#(ast.EnumValue(n), cursor))
    Ok(#(TokenWithPos(tok, _), _)) ->
      Error(UnexpectedToken("value", token_to_string(tok)))
    Error(_) -> Error(UnexpectedEOF("value"))
  }
}

fn parse_list_value(cursor: Cursor) -> Result(#(Value, Cursor), ParseError) {
  use #(values, cursor) <- result.try(parse_list_values(cursor, []))
  use #(_, cursor) <- result.try(expect(cursor, RightBracket, "']'"))
  Ok(#(ast.ListValue(values), cursor))
}

fn parse_list_values(
  cursor: Cursor,
  acc: List(Value),
) -> Result(#(List(Value), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(RightBracket, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(value, cursor) <- result.try(parse_value(cursor))
      parse_list_values(cursor, [value, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_object_value(cursor: Cursor) -> Result(#(Value, Cursor), ParseError) {
  use #(fields, cursor) <- result.try(parse_object_fields(cursor, []))
  use #(_, cursor) <- result.try(expect(cursor, RightBrace, "'}'"))
  Ok(#(ast.ObjectValue(fields), cursor))
}

fn parse_object_fields(
  cursor: Cursor,
  acc: List(ast.ObjectField),
) -> Result(#(List(ast.ObjectField), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(RightBrace, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(field, cursor) <- result.try(parse_object_field(cursor))
      parse_object_fields(cursor, [field, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_object_field(
  cursor: Cursor,
) -> Result(#(ast.ObjectField, Cursor), ParseError) {
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(_, cursor) <- result.try(expect(cursor, Colon, "':'"))
  use #(value, cursor) <- result.try(parse_value(cursor))
  Ok(#(ast.ObjectField(name: name, value: value), cursor))
}

// ============================================================================
// Variable Definitions
// ============================================================================

fn parse_variable_definitions(
  cursor: Cursor,
) -> Result(#(List(VariableDefinition), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(LeftParen, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(defs, cursor) <- result.try(parse_var_def_list(cursor, []))
          use #(_, cursor) <- result.try(expect(cursor, RightParen, "')'"))
          Ok(#(defs, cursor))
        }
        Error(_) -> Ok(#([], cursor))
      }
    }
    _ -> Ok(#([], cursor))
  }
}

fn parse_var_def_list(
  cursor: Cursor,
  acc: List(VariableDefinition),
) -> Result(#(List(VariableDefinition), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(RightParen, _)) -> Ok(#(list.reverse(acc), cursor))
    Ok(_) -> {
      use #(def, cursor) <- result.try(parse_variable_definition(cursor))
      parse_var_def_list(cursor, [def, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_variable_definition(
  cursor: Cursor,
) -> Result(#(VariableDefinition, Cursor), ParseError) {
  use #(_, cursor) <- result.try(expect(cursor, Dollar, "'$'"))
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(_, cursor) <- result.try(expect(cursor, Colon, "':'"))
  use #(var_type, cursor) <- result.try(parse_type(cursor))
  use #(default, cursor) <- result.try(parse_default_value(cursor))
  use #(directives, cursor) <- result.try(parse_directives(cursor))
  Ok(#(
    ast.VariableDefinition(
      variable: name,
      type_: var_type,
      default_value: default,
      directives: directives,
    ),
    cursor,
  ))
}

fn parse_default_value(
  cursor: Cursor,
) -> Result(#(Option(Value), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(Equals, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(value, cursor) <- result.try(parse_value(cursor))
          Ok(#(Some(value), cursor))
        }
        Error(_) -> Ok(#(None, cursor))
      }
    }
    _ -> Ok(#(None, cursor))
  }
}

fn parse_type(cursor: Cursor) -> Result(#(Type, Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(LeftBracket, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> {
          use #(inner, cursor) <- result.try(parse_type(cursor))
          use #(_, cursor) <- result.try(expect(cursor, RightBracket, "']'"))
          use #(is_non_null, cursor) <- result.try(parse_non_null(cursor))
          let list_type = ast.ListType(inner)
          case is_non_null {
            True -> Ok(#(ast.NonNullType(list_type), cursor))
            False -> Ok(#(list_type, cursor))
          }
        }
        Error(_) -> Error(UnexpectedEOF("type"))
      }
    }
    _ -> {
      use #(name, cursor) <- result.try(parse_name(cursor))
      use #(is_non_null, cursor) <- result.try(parse_non_null(cursor))
      let named_type = ast.NamedType(name)
      case is_non_null {
        True -> Ok(#(ast.NonNullType(named_type), cursor))
        False -> Ok(#(named_type, cursor))
      }
    }
  }
}

fn parse_non_null(cursor: Cursor) -> Result(#(Bool, Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(Bang, _)) -> {
      case advance(cursor) {
        Ok(#(_, cursor)) -> Ok(#(True, cursor))
        Error(_) -> Ok(#(False, cursor))
      }
    }
    _ -> Ok(#(False, cursor))
  }
}

// ============================================================================
// Directives
// ============================================================================

fn parse_directives(
  cursor: Cursor,
) -> Result(#(List(ast.Directive), Cursor), ParseError) {
  parse_directives_loop(cursor, [])
}

fn parse_directives_loop(
  cursor: Cursor,
  acc: List(ast.Directive),
) -> Result(#(List(ast.Directive), Cursor), ParseError) {
  case peek(cursor) {
    Ok(TokenWithPos(At, _)) -> {
      use #(directive, cursor) <- result.try(parse_directive(cursor))
      parse_directives_loop(cursor, [directive, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), cursor))
  }
}

fn parse_directive(
  cursor: Cursor,
) -> Result(#(ast.Directive, Cursor), ParseError) {
  use #(_, cursor) <- result.try(expect(cursor, At, "'@'"))
  use #(name, cursor) <- result.try(parse_name(cursor))
  use #(arguments, cursor) <- result.try(parse_arguments(cursor))
  Ok(#(ast.Directive(name: name, arguments: arguments), cursor))
}
