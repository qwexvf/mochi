import gleam/list
import gleam/string
import mochi/internal/sdl_ast
import mochi/internal/sdl_parser

fn parse_ok(sdl: String) -> sdl_ast.SdlDocument {
  case sdl_parser.parse_sdl(sdl) {
    Ok(doc) -> doc
    Error(e) ->
      panic as { "Expected parse success but got: " <> string.inspect(e) }
  }
}

fn first_extension(doc: sdl_ast.SdlDocument) -> sdl_ast.TypeExtensionDef {
  case doc.definitions {
    [sdl_ast.TypeExtension(ext), ..] -> ext
    _ -> panic as "Expected TypeExtension as first definition"
  }
}

pub fn extend_object_type_test() {
  let doc =
    parse_ok(
      "extend type Mutation { finalizeTournament(tournamentId: ID!): String! }",
    )
  case first_extension(doc) {
    sdl_ast.ObjectTypeExtension(name, _, _, fields) -> {
      let assert "Mutation" = name
      let assert 1 = list.length(fields)
    }
    _ -> panic as "Expected ObjectTypeExtension"
  }
}

pub fn extend_object_with_implements_test() {
  let doc = parse_ok("extend type User implements Node { extra: String }")
  case first_extension(doc) {
    sdl_ast.ObjectTypeExtension(name, interfaces, _, _) -> {
      let assert "User" = name
      let assert ["Node"] = interfaces
    }
    _ -> panic as "Expected ObjectTypeExtension"
  }
}

pub fn extend_interface_type_test() {
  let doc = parse_ok("extend interface Node { extra: String }")
  case first_extension(doc) {
    sdl_ast.InterfaceTypeExtension(name, _, fields) -> {
      let assert "Node" = name
      let assert 1 = list.length(fields)
    }
    _ -> panic as "Expected InterfaceTypeExtension"
  }
}

pub fn extend_union_type_test() {
  let doc = parse_ok("extend union SearchResult = Video")
  case first_extension(doc) {
    sdl_ast.UnionTypeExtension(name, _, members) -> {
      let assert "SearchResult" = name
      let assert ["Video"] = members
    }
    _ -> panic as "Expected UnionTypeExtension"
  }
}

pub fn extend_enum_type_test() {
  let doc = parse_ok("extend enum Status { PENDING }")
  case first_extension(doc) {
    sdl_ast.EnumTypeExtension(name, _, values) -> {
      let assert "Status" = name
      let assert 1 = list.length(values)
    }
    _ -> panic as "Expected EnumTypeExtension"
  }
}

pub fn extend_input_type_test() {
  let doc = parse_ok("extend input CreateUserInput { extra: String }")
  case first_extension(doc) {
    sdl_ast.InputObjectTypeExtension(name, _, fields) -> {
      let assert "CreateUserInput" = name
      let assert 1 = list.length(fields)
    }
    _ -> panic as "Expected InputObjectTypeExtension"
  }
}

pub fn extend_scalar_type_test() {
  let doc =
    parse_ok("extend scalar DateTime @specifiedBy(url: \"http://example.com\")")
  case first_extension(doc) {
    sdl_ast.ScalarTypeExtension(name, directives) -> {
      let assert "DateTime" = name
      let assert 1 = list.length(directives)
    }
    _ -> panic as "Expected ScalarTypeExtension"
  }
}

pub fn extend_with_directive_test() {
  let doc = parse_ok("extend type Query @deprecated { extra: String }")
  case first_extension(doc) {
    sdl_ast.ObjectTypeExtension(_, _, directives, _) -> {
      let assert 1 = list.length(directives)
    }
    _ -> panic as "Expected ObjectTypeExtension"
  }
}
