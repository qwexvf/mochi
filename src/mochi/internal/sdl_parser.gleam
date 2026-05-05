// SDL Parser - Parses GraphQL Schema Definition Language
// Converts SDL tokens into an AST that can be used to build schemas

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/internal/sdl_ast.{type SdlDocument, type TypeSystemDefinition}
import mochi/internal/sdl_lexer.{
  type Position, type SdlLexerError, type SdlLexerState, type SdlToken,
  type SdlTokenWithPosition,
}

pub type SdlParseError {
  SdlLexError(error: SdlLexerError)
  UnexpectedToken(expected: String, got: SdlToken, position: Position)
  UnexpectedEOF(expected: String)
  InvalidTypeDefinition(message: String, position: Position)
}

pub type SdlParser {
  SdlParser(
    lexer: SdlLexerState,
    current: Result(SdlTokenWithPosition, SdlLexerError),
  )
}

fn advance(lexer: SdlLexerState) -> SdlParser {
  case sdl_lexer.next_sdl_token(lexer) {
    Ok(#(token, next_lexer)) -> SdlParser(lexer: next_lexer, current: Ok(token))
    Error(err) -> SdlParser(lexer: lexer, current: Error(err))
  }
}

/// Parse SDL string into an AST
pub fn parse_sdl(input: String) -> Result(SdlDocument, SdlParseError) {
  let parser = advance(sdl_lexer.new_sdl_lexer(input))
  case parser.current {
    Error(err) -> Error(SdlLexError(err))
    Ok(_) ->
      parse_document(parser)
      |> result.map(fn(result) { result.0 })
  }
}

fn parse_document(
  parser: SdlParser,
) -> Result(#(SdlDocument, SdlParser), SdlParseError) {
  use #(definitions, parser) <- result.try(parse_definitions(parser, []))
  Ok(#(sdl_ast.SdlDocument(definitions), parser))
}

fn parse_definitions(
  parser: SdlParser,
  acc: List(TypeSystemDefinition),
) -> Result(#(List(TypeSystemDefinition), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.EOF, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(definition, parser) <- result.try(parse_type_system_definition(
        parser,
      ))
      parse_definitions(parser, [definition, ..acc])
    }
    Error(err) -> Error(err)
  }
}

fn parse_type_system_definition(
  parser: SdlParser,
) -> Result(#(TypeSystemDefinition, SdlParser), SdlParseError) {
  // First, check for an optional description
  let #(description, parser) = parse_optional_description(parser)

  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Type, _)) ->
      parse_object_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Interface, _)) ->
      parse_interface_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.InterfaceTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Union, _)) ->
      parse_union_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.UnionTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Scalar, _)) ->
      parse_scalar_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.ScalarTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Enum, _)) ->
      parse_enum_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.EnumTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Input, _)) ->
      parse_input_object_type_definition(parser, description)
      |> result.map(fn(result) {
        #(
          sdl_ast.TypeDefinition(sdl_ast.InputObjectTypeDefinition(result.0)),
          result.1,
        )
      })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Extend, _)) ->
      parse_type_extension(parser)
      |> result.map(fn(result) { #(sdl_ast.TypeExtension(result.0), result.1) })

    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Directive, _)) ->
      parse_directive_definition(parser, description)
      |> result.map(fn(result) {
        #(sdl_ast.DirectiveDefinition(result.0), result.1)
      })

    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("type definition", token, position))

    Error(err) -> Error(err)
  }
}

/// Parse optional description (triple-quoted string)
fn parse_optional_description(parser: SdlParser) -> #(Option(String), SdlParser) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Description(content), _)) -> {
      case consume_token(parser) {
        Ok(#(_, new_parser)) -> #(Some(content), new_parser)
        Error(_) -> #(None, parser)
      }
    }
    // Also handle single-line string descriptions (GraphQL spec allows this)
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(content), _)) -> {
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
  parser: SdlParser,
) -> Result(#(List(sdl_ast.DirectiveUsage), SdlParser), SdlParseError) {
  parse_directives_loop(parser, [])
}

fn parse_directives_loop(
  parser: SdlParser,
  acc: List(sdl_ast.DirectiveUsage),
) -> Result(#(List(sdl_ast.DirectiveUsage), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.At, _)) -> {
      use #(directive, parser) <- result.try(parse_directive_usage(parser))
      parse_directives_loop(parser, [directive, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), parser))
  }
}

/// Parse a single directive usage: @name or @name(arg: value, ...)
fn parse_directive_usage(
  parser: SdlParser,
) -> Result(#(sdl_ast.DirectiveUsage, SdlParser), SdlParseError) {
  // Consume '@'
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.At, "@"))
  // Parse directive name
  use #(name, parser) <- result.try(parse_name(parser))
  // Parse optional arguments
  use #(arguments, parser) <- result.try(parse_optional_directive_arguments(
    parser,
  ))

  Ok(#(sdl_ast.DirectiveUsage(name: name, arguments: arguments), parser))
}

/// Parse optional directive arguments: (arg: value, ...)
fn parse_optional_directive_arguments(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.DirectiveArgument), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
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
  parser: SdlParser,
  acc: List(sdl_ast.DirectiveArgument),
) -> Result(#(List(sdl_ast.DirectiveArgument), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(_), _)) -> {
      use #(argument, parser) <- result.try(parse_directive_argument(parser))
      parse_directive_arguments_loop(parser, [argument, ..acc])
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("argument or ')'", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_directive_argument(
  parser: SdlParser,
) -> Result(#(sdl_ast.DirectiveArgument, SdlParser), SdlParseError) {
  use #(name, parser) <- result.try(parse_name(parser))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
  use #(value, parser) <- result.try(parse_value(parser))
  Ok(#(sdl_ast.DirectiveArgument(name: name, value: value), parser))
}

/// Parse: type User { ... }
fn parse_object_type_definition(
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.ObjectTypeDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.InterfaceTypeDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.UnionTypeDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.ScalarTypeDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.EnumTypeDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.InputObjectTypeDef, SdlParser), SdlParseError) {
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

fn parse_type_extension(
  parser: SdlParser,
) -> Result(#(sdl_ast.TypeExtensionDef, SdlParser), SdlParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    sdl_lexer.Extend,
    "extend",
  ))
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Type, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Type,
        "type",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(interfaces, parser) <- result.try(parse_optional_implements(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use #(fields, parser) <- result.try(parse_optional_fields_definition(
        parser,
      ))
      use _ <- result.try(case interfaces, directives, fields {
        [], [], [] ->
          Error(
            InvalidTypeDefinition(
              "extend type '"
                <> name
                <> "' must have implements, directives, or fields",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _, _, _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.ObjectTypeExtension(
          name: name,
          interfaces: interfaces,
          directives: directives,
          fields: fields,
        ),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Interface, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Interface,
        "interface",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use #(fields, parser) <- result.try(parse_optional_fields_definition(
        parser,
      ))
      use _ <- result.try(case directives, fields {
        [], [] ->
          Error(
            InvalidTypeDefinition(
              "extend interface '" <> name <> "' must have directives or fields",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _, _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.InterfaceTypeExtension(
          name: name,
          directives: directives,
          fields: fields,
        ),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Union, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Union,
        "union",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use #(members, parser) <- result.try(case peek_token(parser) {
        Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Equals, _)) -> {
          use #(_, parser) <- result.try(consume_token(parser))
          parse_union_member_types(parser)
        }
        _ -> Ok(#([], parser))
      })
      use _ <- result.try(case directives, members {
        [], [] ->
          Error(
            InvalidTypeDefinition(
              "extend union '"
                <> name
                <> "' must have directives or member types",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _, _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.UnionTypeExtension(
          name: name,
          directives: directives,
          member_types: members,
        ),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Enum, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Enum,
        "enum",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use #(values, parser) <- result.try(parse_optional_enum_values_definition(
        parser,
      ))
      use _ <- result.try(case directives, values {
        [], [] ->
          Error(
            InvalidTypeDefinition(
              "extend enum '" <> name <> "' must have directives or values",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _, _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.EnumTypeExtension(
          name: name,
          directives: directives,
          values: values,
        ),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Input, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Input,
        "input",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use #(fields, parser) <- result.try(
        parse_optional_input_fields_definition(parser),
      )
      use _ <- result.try(case directives, fields {
        [], [] ->
          Error(
            InvalidTypeDefinition(
              "extend input '" <> name <> "' must have directives or fields",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _, _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.InputObjectTypeExtension(
          name: name,
          directives: directives,
          fields: fields,
        ),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Scalar, _)) -> {
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.Scalar,
        "scalar",
      ))
      use #(name, parser) <- result.try(parse_name(parser))
      use #(directives, parser) <- result.try(parse_optional_directives(parser))
      use _ <- result.try(case directives {
        [] ->
          Error(
            InvalidTypeDefinition(
              "extend scalar '" <> name <> "' must have at least one directive",
              case peek_token(parser) {
                Ok(sdl_lexer.SdlTokenWithPosition(_, pos)) -> pos
                _ -> sdl_lexer.Position(0, 0)
              },
            ),
          )
        _ -> Ok(Nil)
      })
      Ok(#(
        sdl_ast.ScalarTypeExtension(name: name, directives: directives),
        parser,
      ))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken(
        "type/interface/union/enum/input/scalar after extend",
        token,
        position,
      ))
    Error(err) -> Error(err)
  }
}

// Helper parsing functions

fn parse_name(parser: SdlParser) -> Result(#(String, SdlParser), SdlParseError) {
  case consume_token(parser) {
    Ok(#(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(name), _), parser)) ->
      Ok(#(name, parser))
    // Keywords can be used as names in GraphQL (contextual keywords)
    Ok(#(sdl_lexer.SdlTokenWithPosition(token, position), parser)) ->
      case keyword_to_name(token) {
        Ok(name) -> Ok(#(name, parser))
        Error(_) -> Error(UnexpectedToken("name", token, position))
      }
    Error(err) -> Error(err)
  }
}

/// Convert a keyword token to its string representation
/// GraphQL keywords are contextual and can be used as names
fn keyword_to_name(token: sdl_lexer.SdlToken) -> Result(String, Nil) {
  case token {
    sdl_lexer.Type -> Ok("type")
    sdl_lexer.Interface -> Ok("interface")
    sdl_lexer.Union -> Ok("union")
    sdl_lexer.Scalar -> Ok("scalar")
    sdl_lexer.Enum -> Ok("enum")
    sdl_lexer.Input -> Ok("input")
    sdl_lexer.Directive -> Ok("directive")
    sdl_lexer.Schema -> Ok("schema")
    sdl_lexer.Extend -> Ok("extend")
    sdl_lexer.Implements -> Ok("implements")
    _ -> Error(Nil)
  }
}

fn parse_optional_implements(
  parser: SdlParser,
) -> Result(#(List(String), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Implements, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      parse_implements_interfaces(parser, [])
    }
    _ -> Ok(#([], parser))
  }
}

fn parse_implements_interfaces(
  parser: SdlParser,
  acc: List(String),
) -> Result(#(List(String), SdlParser), SdlParseError) {
  use #(interface_name, parser) <- result.try(parse_name(parser))
  let new_acc = [interface_name, ..acc]

  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Amp, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      parse_implements_interfaces(parser, new_acc)
    }
    _ -> Ok(#(list.reverse(new_acc), parser))
  }
}

fn parse_fields_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.FieldDef), SdlParser), SdlParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(fields, parser) <- result.try(parse_field_definitions(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(fields, parser))
}

fn parse_optional_fields_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.FieldDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBrace, _)) ->
      parse_fields_definition(parser)
    _ -> Ok(#([], parser))
  }
}

fn parse_optional_enum_values_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.EnumValueDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBrace, _)) ->
      parse_enum_values_definition(parser)
    _ -> Ok(#([], parser))
  }
}

fn parse_optional_input_fields_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.InputFieldDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBrace, _)) ->
      parse_input_fields_definition(parser)
    _ -> Ok(#([], parser))
  }
}

fn parse_field_definitions(
  parser: SdlParser,
  acc: List(sdl_ast.FieldDef),
) -> Result(#(List(sdl_ast.FieldDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    // Field can start with description, name, string literal, or a contextual
    // keyword used as a field name (e.g. `type`, `input`, `enum`).
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Type, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Interface, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Union, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Scalar, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Enum, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Input, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Directive, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Schema, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Implements, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Extend, _)) -> {
      use #(field, parser) <- result.try(parse_field_definition(parser))
      parse_field_definitions(parser, [field, ..acc])
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("field definition or '}'", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_field_definition(
  parser: SdlParser,
) -> Result(#(sdl_ast.FieldDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
) -> Result(#(sdl_ast.SdlType, SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBracket, _)) -> {
      // List type: [Type]
      use #(_, parser) <- result.try(consume_token(parser))
      use #(inner_type, parser) <- result.try(parse_type(parser))
      use #(_, parser) <- result.try(expect_token(
        parser,
        sdl_lexer.RightBracket,
        "]",
      ))

      let list_type = sdl_ast.ListType(inner_type)

      // Check for non-null modifier
      case peek_token(parser) {
        Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Bang, _)) -> {
          use #(_, parser) <- result.try(consume_token(parser))
          Ok(#(sdl_ast.NonNullType(list_type), parser))
        }
        _ -> Ok(#(list_type, parser))
      }
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(name), _)) -> {
      // Named type
      use #(_, parser) <- result.try(consume_token(parser))
      let named_type = sdl_ast.NamedType(name)

      // Check for non-null modifier
      case peek_token(parser) {
        Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Bang, _)) -> {
          use #(_, parser) <- result.try(consume_token(parser))
          Ok(#(sdl_ast.NonNullType(named_type), parser))
        }
        _ -> Ok(#(named_type, parser))
      }
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("type", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_optional_arguments_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.ArgumentDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftParen, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
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
  parser: SdlParser,
  acc: List(sdl_ast.ArgumentDef),
) -> Result(#(List(sdl_ast.ArgumentDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightParen, _)) ->
      Ok(#(list.reverse(acc), parser))
    // Accept Name tokens
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(_), _))
    | // Accept keywords (which can be used as names in GraphQL)
      Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Type, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Interface, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Union, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Scalar, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Enum, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Input, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Directive, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Schema, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Extend, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Implements, _))
    | // Accept description strings (for documented arguments)
      Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(argument, parser) <- result.try(parse_argument_definition(parser))
      parse_argument_definitions(parser, [argument, ..acc])
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("argument definition or ')'", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_argument_definition(
  parser: SdlParser,
) -> Result(#(sdl_ast.ArgumentDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
) -> Result(#(option.Option(sdl_ast.SdlValue), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Equals, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      use #(value, parser) <- result.try(parse_value(parser))
      Ok(#(option.Some(value), parser))
    }
    _ -> Ok(#(option.None, parser))
  }
}

fn parse_value(
  parser: SdlParser,
) -> Result(#(sdl_ast.SdlValue, SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.IntValue(value), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.IntValue(value), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.FloatValue(value), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.FloatValue(value), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(value), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.StringValue(value), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.BooleanValue(value), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.BooleanValue(value), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name("null"), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.NullValue, parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(name), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(sdl_ast.EnumValue(name), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBracket, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      use #(values, parser) <- result.try(parse_list_values(parser, []))
      Ok(#(sdl_ast.ListValue(values), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.LeftBrace, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      use #(fields, parser) <- result.try(parse_object_field_values(parser, []))
      Ok(#(sdl_ast.ObjectValue(fields), parser))
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("value", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_directive_definition(
  parser: SdlParser,
  description: Option(String),
) -> Result(#(sdl_ast.DirectiveDef, SdlParser), SdlParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    sdl_lexer.Directive,
    "directive",
  ))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.At, "@"))
  use #(name, parser) <- result.try(parse_name(parser))
  use #(arguments, parser) <- result.try(parse_optional_arguments_definition(
    parser,
  ))
  // Optional `repeatable` keyword per spec, before `on`
  use #(repeatable, parser) <- result.try(case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name("repeatable"), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(True, parser))
    }
    _ -> Ok(#(False, parser))
  })
  use #(locations, parser) <- result.try(case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name("on"), _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      parse_directive_locations(parser)
    }
    _ -> Ok(#([], parser))
  })
  Ok(#(
    sdl_ast.DirectiveDef(
      name: name,
      description: description,
      locations: locations,
      arguments: arguments,
      repeatable: repeatable,
    ),
    parser,
  ))
}

fn parse_directive_locations(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.DirectiveLocation), SdlParser), SdlParseError) {
  // Optional leading pipe per spec: DirectiveLocations: `|`? DirectiveLocation (`|` DirectiveLocation)*
  use parser <- result.try(case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Pipe, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(parser)
    }
    _ -> Ok(parser)
  })
  use #(first, parser) <- result.try(parse_directive_location(parser))
  parse_directive_locations_tail(parser, [first])
}

fn parse_directive_locations_tail(
  parser: SdlParser,
  acc: List(sdl_ast.DirectiveLocation),
) -> Result(#(List(sdl_ast.DirectiveLocation), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Pipe, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      use #(location, parser) <- result.try(parse_directive_location(parser))
      parse_directive_locations_tail(parser, [location, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_directive_location(
  parser: SdlParser,
) -> Result(#(sdl_ast.DirectiveLocation, SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(name), position)) ->
      case directive_location_from_name(name) {
        Ok(location) -> {
          use #(_, parser) <- result.try(consume_token(parser))
          Ok(#(location, parser))
        }
        Error(_) ->
          Error(InvalidTypeDefinition(
            "Invalid directive location: " <> name,
            position,
          ))
      }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("directive location", token, position))
    Error(err) -> Error(err)
  }
}

fn directive_location_from_name(
  name: String,
) -> Result(sdl_ast.DirectiveLocation, Nil) {
  case name {
    "QUERY" -> Ok(sdl_ast.QUERY)
    "MUTATION" -> Ok(sdl_ast.MUTATION)
    "SUBSCRIPTION" -> Ok(sdl_ast.SUBSCRIPTION)
    "FIELD" -> Ok(sdl_ast.FIELD)
    "FRAGMENT_DEFINITION" -> Ok(sdl_ast.FRAGMENTDEFINITION)
    "FRAGMENT_SPREAD" -> Ok(sdl_ast.FRAGMENTSPREAD)
    "INLINE_FRAGMENT" -> Ok(sdl_ast.INLINEFRAGMENT)
    "VARIABLE_DEFINITION" -> Ok(sdl_ast.VARIABLEDEFINITION)
    "SCHEMA" -> Ok(sdl_ast.SCHEMA)
    "SCALAR" -> Ok(sdl_ast.SCALAR)
    "OBJECT" -> Ok(sdl_ast.OBJECT)
    "FIELD_DEFINITION" -> Ok(sdl_ast.FIELDDEFINITION)
    "ARGUMENT_DEFINITION" -> Ok(sdl_ast.ARGUMENTDEFINITION)
    "INTERFACE" -> Ok(sdl_ast.INTERFACE)
    "UNION" -> Ok(sdl_ast.UNION)
    "ENUM" -> Ok(sdl_ast.ENUM)
    "ENUM_VALUE" -> Ok(sdl_ast.ENUMVALUE)
    "INPUT_OBJECT" -> Ok(sdl_ast.INPUTOBJECT)
    "INPUT_FIELD_DEFINITION" -> Ok(sdl_ast.INPUTFIELDDEFINITION)
    _ -> Error(Nil)
  }
}

fn parse_list_values(
  parser: SdlParser,
  acc: List(sdl_ast.SdlValue),
) -> Result(#(List(sdl_ast.SdlValue), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightBracket, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(list.reverse(acc), parser))
    }
    Ok(_) -> {
      use #(value, parser) <- result.try(parse_value(parser))
      parse_list_values(parser, [value, ..acc])
    }
    Error(err) -> Error(err)
  }
}

fn parse_object_field_values(
  parser: SdlParser,
  acc: List(sdl_ast.ObjectFieldValue),
) -> Result(#(List(sdl_ast.ObjectFieldValue), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightBrace, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(#(list.reverse(acc), parser))
    }
    Ok(_) -> {
      use #(name, parser) <- result.try(parse_name(parser))
      use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.Colon, ":"))
      use #(value, parser) <- result.try(parse_value(parser))
      parse_object_field_values(parser, [
        sdl_ast.ObjectFieldValue(name: name, value: value),
        ..acc
      ])
    }
    Error(err) -> Error(err)
  }
}

fn parse_union_member_types(
  parser: SdlParser,
) -> Result(#(List(String), SdlParser), SdlParseError) {
  // Optional leading pipe per spec
  use parser <- result.try(case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Pipe, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      Ok(parser)
    }
    _ -> Ok(parser)
  })
  use #(first_member, parser) <- result.try(parse_name(parser))
  parse_union_member_types_helper(parser, [first_member])
}

fn parse_union_member_types_helper(
  parser: SdlParser,
  acc: List(String),
) -> Result(#(List(String), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Pipe, _)) -> {
      use #(_, parser) <- result.try(consume_token(parser))
      use #(member, parser) <- result.try(parse_name(parser))
      parse_union_member_types_helper(parser, [member, ..acc])
    }
    _ -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_enum_values_definition(
  parser: SdlParser,
) -> Result(#(List(sdl_ast.EnumValueDef), SdlParser), SdlParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(values, parser) <- result.try(parse_enum_values(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(values, parser))
}

fn parse_enum_values(
  parser: SdlParser,
  acc: List(sdl_ast.EnumValueDef),
) -> Result(#(List(sdl_ast.EnumValueDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(_), _)) -> {
      use #(enum_value, parser) <- result.try(parse_enum_value_definition(
        parser,
      ))
      parse_enum_values(parser, [enum_value, ..acc])
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("enum value or '}'", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_enum_value_definition(
  parser: SdlParser,
) -> Result(#(sdl_ast.EnumValueDef, SdlParser), SdlParseError) {
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
  parser: SdlParser,
) -> Result(#(List(sdl_ast.InputFieldDef), SdlParser), SdlParseError) {
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.LeftBrace, "{"))
  use #(fields, parser) <- result.try(parse_input_fields(parser, []))
  use #(_, parser) <- result.try(expect_token(parser, sdl_lexer.RightBrace, "}"))
  Ok(#(fields, parser))
}

fn parse_input_fields(
  parser: SdlParser,
  acc: List(sdl_ast.InputFieldDef),
) -> Result(#(List(sdl_ast.InputFieldDef), SdlParser), SdlParseError) {
  case peek_token(parser) {
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Name(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Description(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.StringValue(_), _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Type, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Interface, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Union, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Scalar, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Enum, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Input, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Directive, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Schema, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Implements, _))
    | Ok(sdl_lexer.SdlTokenWithPosition(sdl_lexer.Extend, _)) -> {
      use #(field, parser) <- result.try(parse_input_field_definition(parser))
      parse_input_fields(parser, [field, ..acc])
    }
    Ok(sdl_lexer.SdlTokenWithPosition(token, position)) ->
      Error(UnexpectedToken("input field or '}'", token, position))
    Error(err) -> Error(err)
  }
}

fn parse_input_field_definition(
  parser: SdlParser,
) -> Result(#(sdl_ast.InputFieldDef, SdlParser), SdlParseError) {
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

fn peek_token(parser: SdlParser) -> Result(SdlTokenWithPosition, SdlParseError) {
  case parser.current {
    Ok(token) -> Ok(token)
    Error(err) -> Error(SdlLexError(err))
  }
}

fn consume_token(
  parser: SdlParser,
) -> Result(#(SdlTokenWithPosition, SdlParser), SdlParseError) {
  case parser.current {
    Ok(token) -> Ok(#(token, advance(parser.lexer)))
    Error(err) -> Error(SdlLexError(err))
  }
}

fn expect_token(
  parser: SdlParser,
  expected: SdlToken,
  description: String,
) -> Result(#(SdlTokenWithPosition, SdlParser), SdlParseError) {
  use #(sdl_lexer.SdlTokenWithPosition(token, position), next_parser) <- result.try(
    consume_token(parser),
  )
  case token == expected {
    True -> Ok(#(sdl_lexer.SdlTokenWithPosition(token, position), next_parser))
    False -> Error(UnexpectedToken(description, token, position))
  }
}
