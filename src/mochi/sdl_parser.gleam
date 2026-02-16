// SDL Parser - Parses GraphQL Schema Definition Language
// Converts SDL tokens into an AST that can be used to build schemas

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/sdl_ast.{type SDLDocument, type TypeSystemDefinition}
import mochi/sdl_lexer.{
  type Position, type SDLLexerError, type SDLToken, type SDLTokenWithPosition,
}

pub type SDLParseError {
  SDLLexError(error: SDLLexerError)
  UnexpectedToken(expected: String, got: SDLToken, position: Position)
  UnexpectedEOF(expected: String)
  InvalidTypeDefinition(message: String, position: Position)
}

pub type SDLParser {
  SDLParser(tokens: List(SDLTokenWithPosition), position: Int)
}

/// Parse SDL string into an AST
pub fn parse_sdl(input: String) -> Result(SDLDocument, SDLParseError) {
  use tokens <- result.try(
    sdl_lexer.tokenize_sdl(input) |> result.map_error(SDLLexError),
  )
  let parser = SDLParser(tokens: tokens, position: 0)
  parse_document(parser)
  |> result.map(fn(result) { result.0 })
}

fn parse_document(
  parser: SDLParser,
) -> Result(#(SDLDocument, SDLParser), SDLParseError) {
  use #(definitions, parser) <- result.try(parse_definitions(parser, []))
  Ok(#(sdl_ast.SDLDocument(definitions), parser))
}

fn parse_definitions(
  parser: SDLParser,
  acc: List(TypeSystemDefinition),
) -> Result(#(List(TypeSystemDefinition), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.EOF, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(definition, parser) <- result.try(parse_type_system_definition(
        parser,
      ))
      parse_definitions(parser, [definition, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_type_system_definition(
  parser: SDLParser,
) -> Result(#(TypeSystemDefinition, SDLParser), SDLParseError) {
  // First, check for an optional description
  let #(description, parser) = parse_optional_description(parser)

  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Type, _)) ->
      parse_object_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Interface, _)) ->
      parse_interface_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.InterfaceTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Union, _)) ->
      parse_union_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.UnionTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Scalar, _)) ->
      parse_scalar_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.ScalarTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Enum, _)) ->
      parse_enum_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.EnumTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Input, _)) ->
      parse_input_object_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.InputObjectTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("type definition", token, position))

    Error(_) -> Error(UnexpectedEOF("type definition"))
  }
}

/// Parse optional description (triple-quoted string)
fn parse_optional_description(parser: SDLParser) -> #(Option(String), SDLParser) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Description(content), _)) -> {
      case consume_token(parser) {
        Ok(#(_, new_parser)) -> #(Some(content), new_parser)
        Error(_) -> #(None, parser)
      }
    }
    // Also handle single-line string descriptions (GraphQL spec allows this)
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(content), _)) -> {
      case consume_token(parser) {
        Ok(#(_, new_parser)) -> #(Some(content), new_parser)
        Error(_) -> #(None, parser)
      }
    }
    _ -> #(None, parser)
  }
}

/// Parse optional directives (@name or @name(args...))
fn parse_optional_directives(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.DirectiveUsage), SDLParser), SDLParseError) {
  parse_directives_loop(parser, [])
}

fn parse_directives_loop(
  parser: SDLParser,
  acc: List(sdl_ast.DirectiveUsage),
) -> Result(#(List(sdl_ast.DirectiveUsage), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.At, _)) -> {
      use #(directive, parser) <- result.try(parse_directive_usage(parser))
      parse_directives_loop(parser, [directive, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), parser))
  }
}

/// Parse a single directive usage: @name or @name(arg: value, ...)
fn parse_directive_usage(
  parser: SDLParser,
) -> Result(#(sdl_ast.DirectiveUsage, SDLParser), SDLParseError) {
  // Consume '@'
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.At, "@"))
  // Parse directive name
  use #(name, parser) <- result.try(parse_name(parser))
  // Parse optional arguments
  use #(arguments, parser) <- result.try(
    parse_optional_directive_arguments(parser),
  )

  Ok(#(sdl_ast.DirectiveUsage(name: name, arguments: arguments), parser))
}

/// Parse optional directive arguments: (arg: value, ...)
fn parse_optional_directive_arguments(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.DirectiveArgument), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("(") }),
      )
      use #(arguments, parser) <- result.try(
        parse_directive_arguments_loop(parser, []),
      )
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.RightParen,
        ")",
      ))
      Ok(#(arguments, parser))
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_directive_arguments_loop(
  parser: SDLParser,
  acc: List(sdl_ast.DirectiveArgument),
) -> Result(#(List(sdl_ast.DirectiveArgument), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(_), _)) -> {
      use #(argument, parser) <- result.try(parse_directive_argument(parser))
      parse_directive_arguments_loop(parser, [argument, ..acc])
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("argument or ')'", token, position))
    Error(_) -> Error(UnexpectedEOF("argument or ')'"))
  }
}

fn parse_directive_argument(
  parser: SDLParser,
) -> Result(#(sdl_ast.DirectiveArgument, SDLParser), SDLParseError) {
  use #(name, parser) <- result.try(parse_name(parser))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
  use #(value, parser) <- result.try(parse_value(parser))
  Ok(#(sdl_ast.DirectiveArgument(name: name, value: value), parser))
}

/// Parse: type User { ... }
fn parse_object_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.ObjectTypeDef, SDLParser), SDLParseError) {
  // Consume 'type' keyword
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Type, "type"))

  // Parse type name
  use #(name, parser) <- result.try(parse_name(parser))

  // Parse optional implements clause
  use #(interfaces, parser) <- result.try(parse_optional_implements(parser))

  // Parse optional directives
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  // Parse field definitions
  use #(fields, parser) <- result.try(parse_fields_definition(parser))

  Ok(#(
    sdl_ast.ObjectTypeDef(
      name: name,
      description: description,
      interfaces: interfaces,
      directives: directives,
      fields: fields,
    ),
    parser,
  ))
}

/// Parse: interface Node { ... }
fn parse_interface_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.InterfaceTypeDef, SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    sdl_lexer.Interface,
    "interface",
  ))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(directives, parser) <- result.try(parse_optional_directives(parser))
  use #(fields, parser) <- result.try(parse_fields_definition(parser))

  Ok(#(
    sdl_ast.InterfaceTypeDef(
      name: name,
      description: description,
      directives: directives,
      fields: fields,
    ),
    parser,
  ))
}

/// Parse: union SearchResult = User | Post
fn parse_union_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.UnionTypeDef, SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Union, "union"))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(directives, parser) <- result.try(parse_optional_directives(parser))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Equals, "="))
  use #(members, parser) <- result.try(parse_union_member_types(parser))

  Ok(#(
    sdl_ast.UnionTypeDef(
      name: name,
      description: description,
      directives: directives,
      member_types: members,
    ),
    parser,
  ))
}

/// Parse: scalar DateTime
fn parse_scalar_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.ScalarTypeDef, SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    sdl_lexer.Scalar,
    "scalar",
  ))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  Ok(#(
    sdl_ast.ScalarTypeDef(
      name: name,
      description: description,
      directives: directives,
    ),
    parser,
  ))
}

/// Parse: enum Status { ACTIVE INACTIVE }
fn parse_enum_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.EnumTypeDef, SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Enum, "enum"))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(directives, parser) <- result.try(parse_optional_directives(parser))
  use #(values, parser) <- result.try(parse_enum_values_definition(parser))

  Ok(#(
    sdl_ast.EnumTypeDef(
      name: name,
      description: description,
      directives: directives,
      values: values,
    ),
    parser,
  ))
}

/// Parse: input CreateUserInput { ... }
fn parse_input_object_type_definition(
  parser: SDLParser,
  description: Option(String),
) -> Result(#(sdl_ast.InputObjectTypeDef, SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Input, "input"))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(directives, parser) <- result.try(parse_optional_directives(parser))
  use #(fields, parser) <- result.try(parse_input_fields_definition(parser))

  Ok(#(
    sdl_ast.InputObjectTypeDef(
      name: name,
      description: description,
      directives: directives,
      fields: fields,
    ),
    parser,
  ))
}

// Helper parsing functions

fn parse_name(parser: SDLParser) -> Result(#(String, SDLParser), SDLParseError) {
  case consume_token(parser) {
    Ok(#(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(name), _), parser)) ->
      Ok(#(name, parser))
    Ok(#(sdl_lexer.SDLTokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken("name", token, position))
    Error(_) -> Error(UnexpectedEOF("name"))
  }
}

fn parse_optional_implements(
  parser: SDLParser,
) -> Result(#(List(String), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Implements, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("implements") }),
      )
      parse_implements_interfaces(parser, [])
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_implements_interfaces(
  parser: SDLParser,
  acc: List(String),
) -> Result(#(List(String), SDLParser), SDLParseError) {
  use #(interface_name, parser) <- result.try(parse_name(parser))
  let new_acc = [interface_name, ..acc]

  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Amp, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("&") }),
      )
      parse_implements_interfaces(parser, new_acc)
    }
    _ -> Ok(#(list.reverse(new_acc), parser))
  }
}

fn parse_fields_definition(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.FieldDef), SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(fields, parser) <- result.try(parse_field_definitions(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(fields, parser))
}

fn parse_field_definitions(
  parser: SDLParser,
  acc: List(sdl_ast.FieldDef),
) -> Result(#(List(sdl_ast.FieldDef), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    // Field can start with description, name, or string literal (for description)
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(field, parser) <- result.try(parse_field_definition(parser))
      parse_field_definitions(parser, [field, ..acc])
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("field definition or '}'", token, position))
    Error(_) -> Error(UnexpectedEOF("field definition or '}'"))
  }
}

fn parse_field_definition(
  parser: SDLParser,
) -> Result(#(sdl_ast.FieldDef, SDLParser), SDLParseError) {
  // Parse optional description
  let #(description, parser) = parse_optional_description(parser)
  use #(name, parser) <- result.try(parse_name(parser))
  use #(arguments, parser) <- result.try(parse_optional_arguments_definition(
    parser,
  ))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
  use #(field_type, parser) <- result.try(parse_type(parser))
  // Parse optional directives after type
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  Ok(#(
    sdl_ast.FieldDef(
      name: name,
      description: description,
      arguments: arguments,
      field_type: field_type,
      directives: directives,
    ),
    parser,
  ))
}

fn parse_type(
  parser: SDLParser,
) -> Result(#(sdl_ast.SDLType, SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.LeftBracket, _)) -> {
      // List type: [Type]
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("[") }),
      )
      use #(inner_type, parser) <- result.try(parse_type(parser))
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.RightBracket,
        "]",
      ))

      let list_type = sdl_ast.ListType(inner_type)

      // Check for non-null modifier
      case peek_token(parser) {
        Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Bang, _)) -> {
          use #(_, parser) <- result.try(
            consume_token(parser)
            |> result.map_error(fn(_) { UnexpectedEOF("!") }),
          )
          Ok(#(sdl_ast.NonNullType(list_type), parser))
        }
        _ -> Ok(#(list_type, parser))
      }
    }
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(name), _)) -> {
      // Named type
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("name") }),
      )
      let named_type = sdl_ast.NamedType(name)

      // Check for non-null modifier
      case peek_token(parser) {
        Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Bang, _)) -> {
          use #(_, parser) <- result.try(
            consume_token(parser)
            |> result.map_error(fn(_) { UnexpectedEOF("!") }),
          )
          Ok(#(sdl_ast.NonNullType(named_type), parser))
        }
        _ -> Ok(#(named_type, parser))
      }
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("type", token, position))
    Error(_) -> Error(UnexpectedEOF("type"))
  }
}

fn parse_optional_arguments_definition(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.ArgumentDef), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("(") }),
      )
      use #(arguments, parser) <- result.try(
        parse_argument_definitions(parser, []),
      )
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.RightParen,
        ")",
      ))
      Ok(#(arguments, parser))
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_argument_definitions(
  parser: SDLParser,
  acc: List(sdl_ast.ArgumentDef),
) -> Result(#(List(sdl_ast.ArgumentDef), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(argument, parser) <- result.try(parse_argument_definition(parser))
      parse_argument_definitions(parser, [argument, ..acc])
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("argument definition or ')'", token, position))
    Error(_) -> Error(UnexpectedEOF("argument definition or ')'"))
  }
}

fn parse_argument_definition(
  parser: SDLParser,
) -> Result(#(sdl_ast.ArgumentDef, SDLParser), SDLParseError) {
  // Parse optional description
  let #(description, parser) = parse_optional_description(parser)
  use #(name, parser) <- result.try(parse_name(parser))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
  use #(arg_type, parser) <- result.try(parse_type(parser))
  use #(default_value, parser) <- result.try(parse_optional_default_value(
    parser,
  ))
  // Parse optional directives after default value
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  Ok(#(
    sdl_ast.ArgumentDef(
      name: name,
      description: description,
      arg_type: arg_type,
      default_value: default_value,
      directives: directives,
    ),
    parser,
  ))
}

fn parse_optional_default_value(
  parser: SDLParser,
) -> Result(#(option.Option(sdl_ast.SDLValue), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Equals, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("=") }),
      )
      use #(value, parser) <- result.try(parse_value(parser))
      Ok(#(option.Some(value), parser))
    }
    _ -> Ok(#(option.None, parser))
  }
}

fn parse_value(
  parser: SDLParser,
) -> Result(#(sdl_ast.SDLValue, SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.IntValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("int value") }),
      )
      Ok(#(sdl_ast.IntValue(value), parser))
    }
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.FloatValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("float value") }),
      )
      Ok(#(sdl_ast.FloatValue(value), parser))
    }
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("string value") }),
      )
      Ok(#(sdl_ast.StringValue(value), parser))
    }
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.BooleanValue(value), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("boolean value") }),
      )
      Ok(#(sdl_ast.BooleanValue(value), parser))
    }
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(name), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("enum value") }),
      )
      Ok(#(sdl_ast.EnumValue(name), parser))
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("value", token, position))
    Error(_) -> Error(UnexpectedEOF("value"))
  }
}

fn parse_union_member_types(
  parser: SDLParser,
) -> Result(#(List(String), SDLParser), SDLParseError) {
  use #(first_member, parser) <- result.try(parse_name(parser))
  parse_union_member_types_helper(parser, [first_member])
}

fn parse_union_member_types_helper(
  parser: SDLParser,
  acc: List(String),
) -> Result(#(List(String), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Pipe, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser) |> result.map_error(fn(_) { UnexpectedEOF("|") }),
      )
      use #(member, parser) <- result.try(parse_name(parser))
      parse_union_member_types_helper(parser, [member, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_enum_values_definition(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.EnumValueDef), SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(values, parser) <- result.try(parse_enum_values(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(values, parser))
}

fn parse_enum_values(
  parser: SDLParser,
  acc: List(sdl_ast.EnumValueDef),
) -> Result(#(List(sdl_ast.EnumValueDef), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(enum_value, parser) <- result.try(parse_enum_value_definition(
        parser,
      ))
      parse_enum_values(parser, [enum_value, ..acc])
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("enum value or '}'", token, position))
    Error(_) -> Error(UnexpectedEOF("enum value or '}'"))
  }
}

fn parse_enum_value_definition(
  parser: SDLParser,
) -> Result(#(sdl_ast.EnumValueDef, SDLParser), SDLParseError) {
  // Parse optional description
  let #(description, parser) = parse_optional_description(parser)
  use #(name, parser) <- result.try(parse_name(parser))
  // Parse optional directives
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  Ok(#(
    sdl_ast.EnumValueDef(
      name: name,
      description: description,
      directives: directives,
    ),
    parser,
  ))
}

fn parse_input_fields_definition(
  parser: SDLParser,
) -> Result(#(List(sdl_ast.InputFieldDef), SDLParser), SDLParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(fields, parser) <- result.try(parse_input_fields(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(fields, parser))
}

fn parse_input_fields(
  parser: SDLParser,
  acc: List(sdl_ast.InputFieldDef),
) -> Result(#(List(sdl_ast.InputFieldDef), SDLParser), SDLParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SDLTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(field, parser) <- result.try(parse_input_field_definition(parser))
      parse_input_fields(parser, [field, ..acc])
    }
    Ok(sdl_lexer.SDLTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("input field or '}'", token, position))
    Error(_) -> Error(UnexpectedEOF("input field or '}'"))
  }
}

fn parse_input_field_definition(
  parser: SDLParser,
) -> Result(#(sdl_ast.InputFieldDef, SDLParser), SDLParseError) {
  // Parse optional description
  let #(description, parser) = parse_optional_description(parser)
  use #(name, parser) <- result.try(parse_name(parser))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
  use #(field_type, parser) <- result.try(parse_type(parser))
  use #(default_value, parser) <- result.try(parse_optional_default_value(
    parser,
  ))
  // Parse optional directives after default value
  use #(directives, parser) <- result.try(parse_optional_directives(parser))

  Ok(#(
    sdl_ast.InputFieldDef(
      name: name,
      description: description,
      field_type: field_type,
      default_value: default_value,
      directives: directives,
    ),
    parser,
  ))
}

// Utility functions

fn peek_token(parser: SDLParser) -> Result(SDLTokenWithPosition, Nil) {
  get_token_at(parser.tokens, parser.position)
}

fn consume_token(
  parser: SDLParser,
) -> Result(#(SDLTokenWithPosition, SDLParser), Nil) {
  case get_token_at(parser.tokens, parser.position) {
    Ok(token) ->
      Ok(#(token, SDLParser(..parser, position: parser.position + 1)))
    Error(_) -> Error(Nil)
  }
}

fn expect_token(
  parser: SDLParser,
  expected: SDLToken,
  description: String,
) -> Result(#(SDLTokenWithPosition, SDLParser), SDLParseError) {
  case consume_token(parser) {
    Ok(#(sdl_lexer.SDLTokenWithPosition(token, position), parser))
      if token == expected
    -> Ok(#(sdl_lexer.SDLTokenWithPosition(token, position), parser))
    Ok(#(sdl_lexer.SDLTokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken(description, token, position))
    Error(_) -> Error(UnexpectedEOF(description))
  }
}

fn get_token_at(
  tokens: List(SDLTokenWithPosition),
  target_index: Int,
) -> Result(SDLTokenWithPosition, Nil) {
  get_token_at_helper(tokens, target_index, 0)
}

fn get_token_at_helper(
  tokens: List(SDLTokenWithPosition),
  target_index: Int,
  current_index: Int,
) -> Result(SDLTokenWithPosition, Nil) {
  case tokens {
    [] -> Error(Nil)
    [first, ..] if current_index == target_index -> Ok(first)
    [_, ..rest] -> get_token_at_helper(rest, target_index, current_index + 1)
  }
}
