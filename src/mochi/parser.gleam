import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/ast.{
  type Document, type Field, type Operation, type OperationType, type Selection,
  type SelectionSet,
}
import mochi/lexer.{
  type LexerError, type Position, type Token, type TokenWithPosition,
}

pub type ParseError {
  LexError(error: LexerError)
  UnexpectedToken(expected: String, got: Token, position: Position)
  UnexpectedEOF(expected: String)
}

pub type Parser {
  Parser(tokens: List(TokenWithPosition), position: Int)
}

pub fn parse(input: String) -> Result(Document, ParseError) {
  use tokens <- result.try(lexer.tokenize(input) |> result.map_error(LexError))
  let parser = Parser(tokens: tokens, position: 0)
  parse_document(parser)
  |> result.map(fn(result) { result.0 })
}

fn parse_document(parser: Parser) -> Result(#(Document, Parser), ParseError) {
  use #(definitions, parser) <- result.try(parse_definitions(parser, []))
  Ok(#(ast.Document(definitions), parser))
}

fn parse_definitions(
  parser: Parser,
  acc: List(ast.Definition),
) -> Result(#(List(ast.Definition), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.EOF, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(definition, parser) <- result.try(parse_definition(parser))
      parse_definitions(parser, [definition, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_definition(
  parser: Parser,
) -> Result(#(ast.Definition, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Fragment, _)) ->
      parse_fragment_definition(parser)
      |> result.map(fn(result) { #(ast.FragmentDefinition(result.0), result.1) })
    _ ->
      parse_operation_definition(parser)
      |> result.map(fn(result) {
        #(ast.OperationDefinition(result.0), result.1)
      })
  }
}

fn parse_fragment_definition(
  parser: Parser,
) -> Result(#(ast.Fragment, Parser), ParseError) {
  // Consume 'fragment' keyword
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.Fragment,
    "'fragment' keyword",
  ))
  // Parse fragment name
  use #(name, parser) <- result.try(parse_name_from_parser(parser))
  // Expect 'on' keyword
  use #(_, parser) <- result.try(expect_token(parser, lexer.On, "'on' keyword"))
  // Parse type condition
  use #(type_condition, parser) <- result.try(parse_name_from_parser(parser))
  // Parse selection set
  use #(selection_set, parser) <- result.try(parse_selection_set(parser))
  Ok(#(
    ast.Fragment(
      name: name,
      type_condition: type_condition,
      directives: [],
      selection_set: selection_set,
    ),
    parser,
  ))
}

fn parse_operation_definition(
  parser: Parser,
) -> Result(#(Operation, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(ast.ShorthandQuery(selection_set), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.Query, _))
    | Ok(lexer.TokenWithPosition(lexer.Mutation, _))
    | Ok(lexer.TokenWithPosition(lexer.Subscription, _)) -> {
      use #(op_type, parser) <- result.try(parse_operation_type(parser))
      use #(name, parser) <- result.try(parse_optional_name(parser))
      use #(variable_defs, parser) <- result.try(parse_variable_definitions(
        parser,
      ))
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(
        ast.Operation(
          operation_type: op_type,
          name: name,
          variable_definitions: variable_defs,
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("operation or selection set", token, position))
    Error(_) -> Error(UnexpectedEOF("operation or selection set"))
  }
}

fn parse_operation_type(
  parser: Parser,
) -> Result(#(OperationType, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(lexer.Query, _), parser)) ->
      Ok(#(ast.Query, parser))
    Ok(#(lexer.TokenWithPosition(lexer.Mutation, _), parser)) ->
      Ok(#(ast.Mutation, parser))
    Ok(#(lexer.TokenWithPosition(lexer.Subscription, _), parser)) ->
      Ok(#(ast.Subscription, parser))
    Ok(#(lexer.TokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken("query, mutation, or subscription", token, position))
    Error(_) -> Error(UnexpectedEOF("operation type"))
  }
}

fn parse_optional_name(
  parser: Parser,
) -> Result(#(Option(String), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Name(name), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("name") }),
      )
      Ok(#(Some(name), parser))
    }
    _ -> Ok(#(None, parser))
  }
}

fn parse_selection_set(
  parser: Parser,
) -> Result(#(SelectionSet, Parser), ParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.LeftBrace,
    "'{' to start selection set",
  ))
  use #(selections, parser) <- result.try(parse_selections(parser, []))
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.RightBrace,
    "'}' to end selection set",
  ))
  Ok(#(ast.SelectionSet(selections), parser))
}

fn parse_selections(
  parser: Parser,
  acc: List(Selection),
) -> Result(#(List(Selection), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(selection, parser) <- result.try(parse_selection(parser))
      parse_selections(parser, [selection, ..acc])
    }
    Error(_) -> Error(UnexpectedEOF("selection or '}'"))
  }
}

fn parse_selection(parser: Parser) -> Result(#(Selection, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Spread, _)) ->
      parse_fragment_spread_or_inline(parser)
    _ ->
      parse_field(parser)
      |> result.map(fn(result) { #(ast.FieldSelection(result.0), result.1) })
  }
}

fn parse_fragment_spread_or_inline(
  parser: Parser,
) -> Result(#(Selection, Parser), ParseError) {
  // Consume '...'
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.Spread,
    "'...' for fragment",
  ))

  case peek_token(parser) {
    // Inline fragment with type condition: ... on TypeName { ... }
    Ok(lexer.TokenWithPosition(lexer.On, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'on'") }),
      )
      use #(type_name, parser) <- result.try(parse_name_from_parser(parser))
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(
        ast.InlineFragment(ast.InlineFragmentValue(
          type_condition: Some(type_name),
          directives: [],
          selection_set: selection_set,
        )),
        parser,
      ))
    }
    // Inline fragment without type condition: ... { ... }
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(
        ast.InlineFragment(ast.InlineFragmentValue(
          type_condition: None,
          directives: [],
          selection_set: selection_set,
        )),
        parser,
      ))
    }
    // Fragment spread: ...FragmentName
    Ok(lexer.TokenWithPosition(lexer.Name(name), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("fragment name") }),
      )
      Ok(#(
        ast.FragmentSpread(ast.FragmentSpreadValue(name: name, directives: [])),
        parser,
      ))
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("fragment name or 'on' or '{'", token, position))
    Error(_) -> Error(UnexpectedEOF("fragment name or 'on' or '{'"))
  }
}

fn parse_field(parser: Parser) -> Result(#(Field, Parser), ParseError) {
  use #(first_name, parser) <- result.try(parse_name_from_parser(parser))

  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Colon, _)) -> {
      // Field with alias: alias: fieldName(args) { ... }
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("colon") }),
      )
      use #(second_name, parser) <- result.try(parse_name_from_parser(parser))
      use #(arguments, parser) <- result.try(parse_arguments(parser))
      let #(selection_set, parser) = parse_optional_selection_set(parser)

      Ok(#(
        ast.Field(
          alias: Some(first_name),
          name: second_name,
          arguments: arguments,
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
    _ -> {
      // Field without alias: fieldName(args) { ... }
      use #(arguments, parser) <- result.try(parse_arguments(parser))
      let #(selection_set, parser) = parse_optional_selection_set(parser)

      Ok(#(
        ast.Field(
          alias: None,
          name: first_name,
          arguments: arguments,
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
  }
}

fn parse_optional_selection_set(
  parser: Parser,
) -> #(Option(SelectionSet), Parser) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      case parse_selection_set(parser) {
        Ok(#(ss, p)) -> #(Some(ss), p)
        Error(_) -> #(None, parser)
      }
    }
    _ -> #(None, parser)
  }
}

fn parse_name_from_parser(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(lexer.Name(name), _), parser)) ->
      Ok(#(name, parser))
    Ok(#(lexer.TokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken("name", token, position))
    Error(_) -> Error(UnexpectedEOF("name"))
  }
}

fn peek_token(parser: Parser) -> Result(TokenWithPosition, Nil) {
  get_token_at(parser.tokens, parser.position)
}

fn consume_token(parser: Parser) -> Result(#(TokenWithPosition, Parser), Nil) {
  case get_token_at(parser.tokens, parser.position) {
    Ok(token) -> Ok(#(token, Parser(..parser, position: parser.position + 1)))
    Error(_) -> Error(Nil)
  }
}

fn get_token_at(
  tokens: List(TokenWithPosition),
  index: Int,
) -> Result(TokenWithPosition, Nil) {
  get_token_at_helper(tokens, index, 0)
}

fn get_token_at_helper(
  tokens: List(TokenWithPosition),
  target_index: Int,
  current_index: Int,
) -> Result(TokenWithPosition, Nil) {
  case tokens {
    [] -> Error(Nil)
    [first, ..] if current_index == target_index -> Ok(first)
    [_, ..rest] -> get_token_at_helper(rest, target_index, current_index + 1)
  }
}

fn expect_token(
  parser: Parser,
  expected: Token,
  description: String,
) -> Result(#(TokenWithPosition, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(token, position), parser)) -> {
      case token == expected {
        True -> Ok(#(lexer.TokenWithPosition(token, position), parser))
        False -> Error(UnexpectedToken(description, token, position))
      }
    }
    Error(_) -> Error(UnexpectedEOF(description))
  }
}

// ============================================================================
// Variable Definitions Parsing
// ============================================================================

fn parse_variable_definitions(
  parser: Parser,
) -> Result(#(List(ast.VariableDefinition), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'('") }),
      )
      use #(defs, parser) <- result.try(
        parse_variable_definitions_list(parser, []),
      )
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.RightParen,
        "')' to end variable definitions",
      ))
      Ok(#(defs, parser))
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_variable_definitions_list(
  parser: Parser,
  acc: List(ast.VariableDefinition),
) -> Result(#(List(ast.VariableDefinition), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(lexer.TokenWithPosition(lexer.Dollar, _)) -> {
      use #(var_def, parser) <- result.try(parse_variable_definition(parser))
      parse_variable_definitions_list(parser, [var_def, ..acc])
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("variable definition or ')'", token, position))
    Error(_) -> Error(UnexpectedEOF("variable definition or ')'"))
  }
}

fn parse_variable_definition(
  parser: Parser,
) -> Result(#(ast.VariableDefinition, Parser), ParseError) {
  // Expect $
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.Dollar,
    "'$' for variable",
  ))
  // Parse variable name
  use #(name, parser) <- result.try(parse_name_from_parser(parser))
  // Expect :
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.Colon,
    "':' after variable name",
  ))
  // Parse type
  use #(var_type, parser) <- result.try(parse_type(parser))
  // Parse optional default value
  use #(default_value, parser) <- result.try(parse_optional_default_value(
    parser,
  ))

  Ok(#(
    ast.VariableDefinition(
      variable: name,
      type_: var_type,
      default_value: default_value,
      directives: [],
    ),
    parser,
  ))
}

fn parse_type(parser: Parser) -> Result(#(ast.Type, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftBracket, _)) -> {
      // List type
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'['") }),
      )
      use #(inner_type, parser) <- result.try(parse_type(parser))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.RightBracket,
        "']' to close list type",
      ))
      // Check for non-null
      use #(final_type, parser) <- result.try(parse_optional_non_null(
        parser,
        ast.ListType(inner_type),
      ))
      Ok(#(final_type, parser))
    }
    Ok(lexer.TokenWithPosition(lexer.Name(_), _)) -> {
      use #(name, parser) <- result.try(parse_name_from_parser(parser))
      // Check for non-null
      use #(final_type, parser) <- result.try(parse_optional_non_null(
        parser,
        ast.NamedType(name),
      ))
      Ok(#(final_type, parser))
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("type name or '['", token, position))
    Error(_) -> Error(UnexpectedEOF("type"))
  }
}

fn parse_optional_non_null(
  parser: Parser,
  base_type: ast.Type,
) -> Result(#(ast.Type, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Bang, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'!'") }),
      )
      Ok(#(ast.NonNullType(base_type), parser))
    }
    _ -> Ok(#(base_type, parser))
  }
}

fn parse_optional_default_value(
  parser: Parser,
) -> Result(#(Option(ast.Value), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Equals, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'='") }),
      )
      use #(value, parser) <- result.try(parse_value(parser))
      Ok(#(Some(value), parser))
    }
    _ -> Ok(#(None, parser))
  }
}

// ============================================================================
// Value Parsing
// ============================================================================

fn parse_value(parser: Parser) -> Result(#(ast.Value, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Dollar, _)) -> {
      // Variable reference
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'$'") }),
      )
      use #(name, parser) <- result.try(parse_name_from_parser(parser))
      Ok(#(ast.VariableValue(name), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.IntValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("int value") }),
      )
      Ok(#(ast.IntValue(value), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.FloatValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("float value") }),
      )
      Ok(#(ast.FloatValue(value), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.StringValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("string value") }),
      )
      Ok(#(ast.StringValue(value), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.TrueKeyword, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("true") }),
      )
      Ok(#(ast.BooleanValue(True), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.FalseKeyword, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("false") }),
      )
      Ok(#(ast.BooleanValue(False), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.NullKeyword, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("null") }),
      )
      Ok(#(ast.NullValue, parser))
    }
    Ok(lexer.TokenWithPosition(lexer.LeftBracket, _)) -> {
      // List value
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'['") }),
      )
      use #(values, parser) <- result.try(parse_list_values(parser, []))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.RightBracket,
        "']' to close list value",
      ))
      Ok(#(ast.ListValue(values), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      // Object value
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'{'") }),
      )
      use #(fields, parser) <- result.try(parse_object_fields(parser, []))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.RightBrace,
        "'}' to close object value",
      ))
      Ok(#(ast.ObjectValue(fields), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.Name(value), _)) -> {
      // Enum value
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("enum value") }),
      )
      Ok(#(ast.EnumValue(value), parser))
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("value", token, position))
    Error(_) -> Error(UnexpectedEOF("value"))
  }
}

fn parse_list_values(
  parser: Parser,
  acc: List(ast.Value),
) -> Result(#(List(ast.Value), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightBracket, _)) ->
      Ok(#(list.reverse(acc), parser))
    _ -> {
      use #(value, parser) <- result.try(parse_value(parser))
      parse_list_values(parser, [value, ..acc])
    }
  }
}

fn parse_object_fields(
  parser: Parser,
  acc: List(ast.ObjectField),
) -> Result(#(List(ast.ObjectField), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(lexer.TokenWithPosition(lexer.Name(_), _)) -> {
      use #(name, parser) <- result.try(parse_name_from_parser(parser))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.Colon,
        "':' after field name",
      ))
      use #(value, parser) <- result.try(parse_value(parser))
      parse_object_fields(parser, [ast.ObjectField(name, value), ..acc])
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("object field or '}'", token, position))
    Error(_) -> Error(UnexpectedEOF("object field or '}'"))
  }
}

// ============================================================================
// Arguments Parsing
// ============================================================================

fn parse_arguments(
  parser: Parser,
) -> Result(#(List(ast.Argument), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("'('") }),
      )
      use #(args, parser) <- result.try(parse_arguments_list(parser, []))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.RightParen,
        "')' to close arguments",
      ))
      Ok(#(args, parser))
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_arguments_list(
  parser: Parser,
  acc: List(ast.Argument),
) -> Result(#(List(ast.Argument), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(lexer.TokenWithPosition(lexer.Name(_), _)) -> {
      use #(name, parser) <- result.try(parse_name_from_parser(parser))
      use #(_, parser) <- result.try(expect_token(
        parser,
        lexer.Colon,
        "':' after argument name",
      ))
      use #(value, parser) <- result.try(parse_value(parser))
      parse_arguments_list(parser, [ast.Argument(name, value), ..acc])
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("argument or ')'", token, position))
    Error(_) -> Error(UnexpectedEOF("argument or ')'"))
  }
}
