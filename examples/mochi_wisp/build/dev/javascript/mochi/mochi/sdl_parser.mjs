import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";
import * as $sdl_ast from "../mochi/sdl_ast.mjs";
import * as $sdl_lexer from "../mochi/sdl_lexer.mjs";

export class SDLLexError extends $CustomType {
  constructor(error) {
    super();
    this.error = error;
  }
}
export const SDLParseError$SDLLexError = (error) => new SDLLexError(error);
export const SDLParseError$isSDLLexError = (value) =>
  value instanceof SDLLexError;
export const SDLParseError$SDLLexError$error = (value) => value.error;
export const SDLParseError$SDLLexError$0 = (value) => value.error;

export class UnexpectedToken extends $CustomType {
  constructor(expected, got, position) {
    super();
    this.expected = expected;
    this.got = got;
    this.position = position;
  }
}
export const SDLParseError$UnexpectedToken = (expected, got, position) =>
  new UnexpectedToken(expected, got, position);
export const SDLParseError$isUnexpectedToken = (value) =>
  value instanceof UnexpectedToken;
export const SDLParseError$UnexpectedToken$expected = (value) => value.expected;
export const SDLParseError$UnexpectedToken$0 = (value) => value.expected;
export const SDLParseError$UnexpectedToken$got = (value) => value.got;
export const SDLParseError$UnexpectedToken$1 = (value) => value.got;
export const SDLParseError$UnexpectedToken$position = (value) => value.position;
export const SDLParseError$UnexpectedToken$2 = (value) => value.position;

export class UnexpectedEOF extends $CustomType {
  constructor(expected) {
    super();
    this.expected = expected;
  }
}
export const SDLParseError$UnexpectedEOF = (expected) =>
  new UnexpectedEOF(expected);
export const SDLParseError$isUnexpectedEOF = (value) =>
  value instanceof UnexpectedEOF;
export const SDLParseError$UnexpectedEOF$expected = (value) => value.expected;
export const SDLParseError$UnexpectedEOF$0 = (value) => value.expected;

export class InvalidTypeDefinition extends $CustomType {
  constructor(message, position) {
    super();
    this.message = message;
    this.position = position;
  }
}
export const SDLParseError$InvalidTypeDefinition = (message, position) =>
  new InvalidTypeDefinition(message, position);
export const SDLParseError$isInvalidTypeDefinition = (value) =>
  value instanceof InvalidTypeDefinition;
export const SDLParseError$InvalidTypeDefinition$message = (value) =>
  value.message;
export const SDLParseError$InvalidTypeDefinition$0 = (value) => value.message;
export const SDLParseError$InvalidTypeDefinition$position = (value) =>
  value.position;
export const SDLParseError$InvalidTypeDefinition$1 = (value) => value.position;

export class SDLParser extends $CustomType {
  constructor(tokens, position) {
    super();
    this.tokens = tokens;
    this.position = position;
  }
}
export const SDLParser$SDLParser = (tokens, position) =>
  new SDLParser(tokens, position);
export const SDLParser$isSDLParser = (value) => value instanceof SDLParser;
export const SDLParser$SDLParser$tokens = (value) => value.tokens;
export const SDLParser$SDLParser$0 = (value) => value.tokens;
export const SDLParser$SDLParser$position = (value) => value.position;
export const SDLParser$SDLParser$1 = (value) => value.position;

function get_token_at_helper(loop$tokens, loop$target_index, loop$current_index) {
  while (true) {
    let tokens = loop$tokens;
    let target_index = loop$target_index;
    let current_index = loop$current_index;
    if (tokens instanceof $Empty) {
      return new Error(undefined);
    } else if (current_index === target_index) {
      let first = tokens.head;
      return new Ok(first);
    } else {
      let rest = tokens.tail;
      loop$tokens = rest;
      loop$target_index = target_index;
      loop$current_index = current_index + 1;
    }
  }
}

function get_token_at(tokens, target_index) {
  return get_token_at_helper(tokens, target_index, 0);
}

function peek_token(parser) {
  return get_token_at(parser.tokens, parser.position);
}

function consume_token(parser) {
  let $ = get_token_at(parser.tokens, parser.position);
  if ($ instanceof Ok) {
    let token = $[0];
    return new Ok([token, new SDLParser(parser.tokens, parser.position + 1)]);
  } else {
    return new Error(undefined);
  }
}

function parse_name(parser) {
  let $ = consume_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0][0].token;
    if ($1 instanceof $sdl_lexer.Name) {
      let parser$1 = $[0][1];
      let name = $1.value;
      return new Ok([name, parser$1]);
    } else {
      let token = $1;
      let position = $[0][0].position;
      return new Error(new UnexpectedToken("name", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("name"));
  }
}

function parse_implements_interfaces(parser, acc) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let interface_name;
      let parser$1;
      interface_name = _use0[0];
      parser$1 = _use0[1];
      let new_acc = listPrepend(interface_name, acc);
      let $ = peek_token(parser$1);
      if ($ instanceof Ok) {
        let $1 = $[0].token;
        if ($1 instanceof $sdl_lexer.Amp) {
          return $result.try$(
            (() => {
              let _pipe = consume_token(parser$1);
              return $result.map_error(
                _pipe,
                (_) => { return new UnexpectedEOF("&"); },
              );
            })(),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return parse_implements_interfaces(parser$2, new_acc);
            },
          );
        } else {
          return new Ok([$list.reverse(new_acc), parser$1]);
        }
      } else {
        return new Ok([$list.reverse(new_acc), parser$1]);
      }
    },
  );
}

function parse_optional_implements(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.Implements) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("implements"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return parse_implements_interfaces(parser$1, toList([]));
        },
      );
    } else {
      return new Ok([toList([]), parser]);
    }
  } else {
    return new Ok([toList([]), parser]);
  }
}

function parse_value(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.Name) {
      let name = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("enum value"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $sdl_ast.EnumValue(name), parser$1]);
        },
      );
    } else if ($1 instanceof $sdl_lexer.IntValue) {
      let value = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("int value"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $sdl_ast.IntValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $sdl_lexer.FloatValue) {
      let value = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("float value"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $sdl_ast.FloatValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $sdl_lexer.StringValue) {
      let value = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("string value"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $sdl_ast.StringValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $sdl_lexer.BooleanValue) {
      let value = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("boolean value"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $sdl_ast.BooleanValue(value), parser$1]);
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(new UnexpectedToken("value", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("value"));
  }
}

function parse_optional_default_value(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.Equals) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("="); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_value(parser$1),
            (_use0) => {
              let value;
              let parser$2;
              value = _use0[0];
              parser$2 = _use0[1];
              return new Ok([new $option.Some(value), parser$2]);
            },
          );
        },
      );
    } else {
      return new Ok([new $option.None(), parser]);
    }
  } else {
    return new Ok([new $option.None(), parser]);
  }
}

function parse_union_member_types_helper(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.Pipe) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("|"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_name(parser$1),
            (_use0) => {
              let member;
              let parser$2;
              member = _use0[0];
              parser$2 = _use0[1];
              return parse_union_member_types_helper(
                parser$2,
                listPrepend(member, acc),
              );
            },
          );
        },
      );
    } else {
      return new Ok([$list.reverse(acc), parser]);
    }
  } else {
    return new Ok([$list.reverse(acc), parser]);
  }
}

function parse_union_member_types(parser) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let first_member;
      let parser$1;
      first_member = _use0[0];
      parser$1 = _use0[1];
      return parse_union_member_types_helper(parser$1, toList([first_member]));
    },
  );
}

function parse_enum_value_definition(parser) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let name;
      let parser$1;
      name = _use0[0];
      parser$1 = _use0[1];
      return new Ok(
        [new $sdl_ast.EnumValueDef(name, new None(), toList([])), parser$1],
      );
    },
  );
}

function parse_enum_values(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.RightBrace) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $sdl_lexer.Name) {
      return $result.try$(
        parse_enum_value_definition(parser),
        (_use0) => {
          let enum_value;
          let parser$1;
          enum_value = _use0[0];
          parser$1 = _use0[1];
          return parse_enum_values(parser$1, listPrepend(enum_value, acc));
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("enum value or '}'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("enum value or '}'"));
  }
}

function expect_token(parser, expected, description) {
  let $ = consume_token(parser);
  if ($ instanceof Ok) {
    let token = $[0][0].token;
    if (isEqual(token, expected)) {
      return $;
    } else {
      let token = $[0][0].token;
      let position = $[0][0].position;
      return new Error(new UnexpectedToken(description, token, position));
    }
  } else {
    return new Error(new UnexpectedEOF(description));
  }
}

/**
 * Parse: union SearchResult = User | Post
 * 
 * @ignore
 */
function parse_union_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Union(), "union"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $sdl_lexer.Equals(), "="),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return $result.try$(
                parse_union_member_types(parser$3),
                (_use0) => {
                  let members;
                  let parser$4;
                  members = _use0[0];
                  parser$4 = _use0[1];
                  return new Ok(
                    [
                      new $sdl_ast.UnionTypeDef(
                        name,
                        new None(),
                        toList([]),
                        members,
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Parse: scalar DateTime
 * 
 * @ignore
 */
function parse_scalar_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Scalar(), "scalar"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return new Ok(
            [new $sdl_ast.ScalarTypeDef(name, new None(), toList([])), parser$2],
          );
        },
      );
    },
  );
}

function parse_type(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.LeftBracket) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("["); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_type(parser$1),
            (_use0) => {
              let inner_type;
              let parser$2;
              inner_type = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(parser$2, new $sdl_lexer.RightBracket(), "]"),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  let list_type = new $sdl_ast.ListType(inner_type);
                  let $2 = peek_token(parser$3);
                  if ($2 instanceof Ok) {
                    let $3 = $2[0].token;
                    if ($3 instanceof $sdl_lexer.Bang) {
                      return $result.try$(
                        (() => {
                          let _pipe = consume_token(parser$3);
                          return $result.map_error(
                            _pipe,
                            (_) => { return new UnexpectedEOF("!"); },
                          );
                        })(),
                        (_use0) => {
                          let parser$4;
                          parser$4 = _use0[1];
                          return new Ok(
                            [new $sdl_ast.NonNullType(list_type), parser$4],
                          );
                        },
                      );
                    } else {
                      return new Ok([list_type, parser$3]);
                    }
                  } else {
                    return new Ok([list_type, parser$3]);
                  }
                },
              );
            },
          );
        },
      );
    } else if ($1 instanceof $sdl_lexer.Name) {
      let name = $1.value;
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("name"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          let named_type = new $sdl_ast.NamedType(name);
          let $2 = peek_token(parser$1);
          if ($2 instanceof Ok) {
            let $3 = $2[0].token;
            if ($3 instanceof $sdl_lexer.Bang) {
              return $result.try$(
                (() => {
                  let _pipe = consume_token(parser$1);
                  return $result.map_error(
                    _pipe,
                    (_) => { return new UnexpectedEOF("!"); },
                  );
                })(),
                (_use0) => {
                  let parser$2;
                  parser$2 = _use0[1];
                  return new Ok(
                    [new $sdl_ast.NonNullType(named_type), parser$2],
                  );
                },
              );
            } else {
              return new Ok([named_type, parser$1]);
            }
          } else {
            return new Ok([named_type, parser$1]);
          }
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(new UnexpectedToken("type", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("type"));
  }
}

function parse_argument_definition(parser) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let name;
      let parser$1;
      name = _use0[0];
      parser$1 = _use0[1];
      return $result.try$(
        expect_token(parser$1, new $sdl_lexer.Colon(), ":"),
        (_use0) => {
          let parser$2;
          parser$2 = _use0[1];
          return $result.try$(
            parse_type(parser$2),
            (_use0) => {
              let arg_type;
              let parser$3;
              arg_type = _use0[0];
              parser$3 = _use0[1];
              return $result.try$(
                parse_optional_default_value(parser$3),
                (_use0) => {
                  let default_value;
                  let parser$4;
                  default_value = _use0[0];
                  parser$4 = _use0[1];
                  return new Ok(
                    [
                      new $sdl_ast.ArgumentDef(
                        name,
                        new None(),
                        arg_type,
                        default_value,
                        toList([]),
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

function parse_argument_definitions(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.RightParen) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $sdl_lexer.Name) {
      return $result.try$(
        parse_argument_definition(parser),
        (_use0) => {
          let argument;
          let parser$1;
          argument = _use0[0];
          parser$1 = _use0[1];
          return parse_argument_definitions(
            parser$1,
            listPrepend(argument, acc),
          );
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("argument definition or ')'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("argument definition or ')'"));
  }
}

function parse_optional_arguments_definition(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.LeftParen) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("("); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_argument_definitions(parser$1, toList([])),
            (_use0) => {
              let arguments$;
              let parser$2;
              arguments$ = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(parser$2, new $sdl_lexer.RightParen(), ")"),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return new Ok([arguments$, parser$3]);
                },
              );
            },
          );
        },
      );
    } else {
      return new Ok([toList([]), parser]);
    }
  } else {
    return new Ok([toList([]), parser]);
  }
}

function parse_field_definition(parser) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let name;
      let parser$1;
      name = _use0[0];
      parser$1 = _use0[1];
      return $result.try$(
        parse_optional_arguments_definition(parser$1),
        (_use0) => {
          let arguments$;
          let parser$2;
          arguments$ = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $sdl_lexer.Colon(), ":"),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return $result.try$(
                parse_type(parser$3),
                (_use0) => {
                  let field_type;
                  let parser$4;
                  field_type = _use0[0];
                  parser$4 = _use0[1];
                  return new Ok(
                    [
                      new $sdl_ast.FieldDef(
                        name,
                        new None(),
                        arguments$,
                        field_type,
                        toList([]),
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

function parse_field_definitions(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.RightBrace) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $sdl_lexer.Name) {
      return $result.try$(
        parse_field_definition(parser),
        (_use0) => {
          let field;
          let parser$1;
          field = _use0[0];
          parser$1 = _use0[1];
          return parse_field_definitions(parser$1, listPrepend(field, acc));
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("field definition or '}'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("field definition or '}'"));
  }
}

function parse_fields_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.LeftBrace(), "{"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_field_definitions(parser$1, toList([])),
        (_use0) => {
          let fields;
          let parser$2;
          fields = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $sdl_lexer.RightBrace(), "}"),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return new Ok([fields, parser$3]);
            },
          );
        },
      );
    },
  );
}

/**
 * Parse: type User { ... }
 * 
 * @ignore
 */
function parse_object_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Type(), "type"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            parse_optional_implements(parser$2),
            (_use0) => {
              let interfaces;
              let parser$3;
              interfaces = _use0[0];
              parser$3 = _use0[1];
              return $result.try$(
                parse_fields_definition(parser$3),
                (_use0) => {
                  let fields;
                  let parser$4;
                  fields = _use0[0];
                  parser$4 = _use0[1];
                  return new Ok(
                    [
                      new $sdl_ast.ObjectTypeDef(
                        name,
                        new None(),
                        interfaces,
                        toList([]),
                        fields,
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Parse: interface Node { ... }
 * 
 * @ignore
 */
function parse_interface_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Interface(), "interface"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            parse_fields_definition(parser$2),
            (_use0) => {
              let fields;
              let parser$3;
              fields = _use0[0];
              parser$3 = _use0[1];
              return new Ok(
                [
                  new $sdl_ast.InterfaceTypeDef(
                    name,
                    new None(),
                    toList([]),
                    fields,
                  ),
                  parser$3,
                ],
              );
            },
          );
        },
      );
    },
  );
}

function parse_enum_values_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.LeftBrace(), "{"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_enum_values(parser$1, toList([])),
        (_use0) => {
          let values;
          let parser$2;
          values = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $sdl_lexer.RightBrace(), "}"),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return new Ok([values, parser$3]);
            },
          );
        },
      );
    },
  );
}

/**
 * Parse: enum Status { ACTIVE INACTIVE }
 * 
 * @ignore
 */
function parse_enum_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Enum(), "enum"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            parse_enum_values_definition(parser$2),
            (_use0) => {
              let values;
              let parser$3;
              values = _use0[0];
              parser$3 = _use0[1];
              return new Ok(
                [
                  new $sdl_ast.EnumTypeDef(name, new None(), toList([]), values),
                  parser$3,
                ],
              );
            },
          );
        },
      );
    },
  );
}

function parse_input_field_definition(parser) {
  return $result.try$(
    parse_name(parser),
    (_use0) => {
      let name;
      let parser$1;
      name = _use0[0];
      parser$1 = _use0[1];
      return $result.try$(
        expect_token(parser$1, new $sdl_lexer.Colon(), ":"),
        (_use0) => {
          let parser$2;
          parser$2 = _use0[1];
          return $result.try$(
            parse_type(parser$2),
            (_use0) => {
              let field_type;
              let parser$3;
              field_type = _use0[0];
              parser$3 = _use0[1];
              return $result.try$(
                parse_optional_default_value(parser$3),
                (_use0) => {
                  let default_value;
                  let parser$4;
                  default_value = _use0[0];
                  parser$4 = _use0[1];
                  return new Ok(
                    [
                      new $sdl_ast.InputFieldDef(
                        name,
                        new None(),
                        field_type,
                        default_value,
                        toList([]),
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

function parse_input_fields(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.RightBrace) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $sdl_lexer.Name) {
      return $result.try$(
        parse_input_field_definition(parser),
        (_use0) => {
          let field;
          let parser$1;
          field = _use0[0];
          parser$1 = _use0[1];
          return parse_input_fields(parser$1, listPrepend(field, acc));
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("input field or '}'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("input field or '}'"));
  }
}

function parse_input_fields_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.LeftBrace(), "{"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_input_fields(parser$1, toList([])),
        (_use0) => {
          let fields;
          let parser$2;
          fields = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $sdl_lexer.RightBrace(), "}"),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return new Ok([fields, parser$3]);
            },
          );
        },
      );
    },
  );
}

/**
 * Parse: input CreateUserInput { ... }
 * 
 * @ignore
 */
function parse_input_object_type_definition(parser) {
  return $result.try$(
    expect_token(parser, new $sdl_lexer.Input(), "input"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            parse_input_fields_definition(parser$2),
            (_use0) => {
              let fields;
              let parser$3;
              fields = _use0[0];
              parser$3 = _use0[1];
              return new Ok(
                [
                  new $sdl_ast.InputObjectTypeDef(
                    name,
                    new None(),
                    toList([]),
                    fields,
                  ),
                  parser$3,
                ],
              );
            },
          );
        },
      );
    },
  );
}

function parse_type_system_definition(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.Type) {
      let _pipe = parse_object_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.ObjectTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else if ($1 instanceof $sdl_lexer.Interface) {
      let _pipe = parse_interface_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.InterfaceTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else if ($1 instanceof $sdl_lexer.Union) {
      let _pipe = parse_union_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.UnionTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else if ($1 instanceof $sdl_lexer.Scalar) {
      let _pipe = parse_scalar_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.ScalarTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else if ($1 instanceof $sdl_lexer.Enum) {
      let _pipe = parse_enum_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.EnumTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else if ($1 instanceof $sdl_lexer.Input) {
      let _pipe = parse_input_object_type_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [
            new $sdl_ast.TypeDefinition(
              new $sdl_ast.InputObjectTypeDefinition(result[0]),
            ),
            result[1],
          ];
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(new UnexpectedToken("type definition", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("type definition"));
  }
}

function parse_definitions(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $sdl_lexer.EOF) {
      return new Ok([$list.reverse(acc), parser]);
    } else {
      return $result.try$(
        parse_type_system_definition(parser),
        (_use0) => {
          let definition;
          let parser$1;
          definition = _use0[0];
          parser$1 = _use0[1];
          return parse_definitions(parser$1, listPrepend(definition, acc));
        },
      );
    }
  } else {
    return new Ok([$list.reverse(acc), parser]);
  }
}

function parse_document(parser) {
  return $result.try$(
    parse_definitions(parser, toList([])),
    (_use0) => {
      let definitions;
      let parser$1;
      definitions = _use0[0];
      parser$1 = _use0[1];
      return new Ok([new $sdl_ast.SDLDocument(definitions), parser$1]);
    },
  );
}

/**
 * Parse SDL string into an AST
 */
export function parse_sdl(input) {
  return $result.try$(
    (() => {
      let _pipe = $sdl_lexer.tokenize_sdl(input);
      return $result.map_error(
        _pipe,
        (var0) => { return new SDLLexError(var0); },
      );
    })(),
    (tokens) => {
      let parser = new SDLParser(tokens, 0);
      let _pipe = parse_document(parser);
      return $result.map(_pipe, (result) => { return result[0]; });
    },
  );
}
