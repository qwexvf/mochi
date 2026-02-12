import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
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
import * as $ast from "../mochi/ast.mjs";
import * as $lexer from "../mochi/lexer.mjs";

export class LexError extends $CustomType {
  constructor(error) {
    super();
    this.error = error;
  }
}
export const ParseError$LexError = (error) => new LexError(error);
export const ParseError$isLexError = (value) => value instanceof LexError;
export const ParseError$LexError$error = (value) => value.error;
export const ParseError$LexError$0 = (value) => value.error;

export class UnexpectedToken extends $CustomType {
  constructor(expected, got, position) {
    super();
    this.expected = expected;
    this.got = got;
    this.position = position;
  }
}
export const ParseError$UnexpectedToken = (expected, got, position) =>
  new UnexpectedToken(expected, got, position);
export const ParseError$isUnexpectedToken = (value) =>
  value instanceof UnexpectedToken;
export const ParseError$UnexpectedToken$expected = (value) => value.expected;
export const ParseError$UnexpectedToken$0 = (value) => value.expected;
export const ParseError$UnexpectedToken$got = (value) => value.got;
export const ParseError$UnexpectedToken$1 = (value) => value.got;
export const ParseError$UnexpectedToken$position = (value) => value.position;
export const ParseError$UnexpectedToken$2 = (value) => value.position;

export class UnexpectedEOF extends $CustomType {
  constructor(expected) {
    super();
    this.expected = expected;
  }
}
export const ParseError$UnexpectedEOF = (expected) =>
  new UnexpectedEOF(expected);
export const ParseError$isUnexpectedEOF = (value) =>
  value instanceof UnexpectedEOF;
export const ParseError$UnexpectedEOF$expected = (value) => value.expected;
export const ParseError$UnexpectedEOF$0 = (value) => value.expected;

export class Parser extends $CustomType {
  constructor(tokens, position) {
    super();
    this.tokens = tokens;
    this.position = position;
  }
}
export const Parser$Parser = (tokens, position) => new Parser(tokens, position);
export const Parser$isParser = (value) => value instanceof Parser;
export const Parser$Parser$tokens = (value) => value.tokens;
export const Parser$Parser$0 = (value) => value.tokens;
export const Parser$Parser$position = (value) => value.position;
export const Parser$Parser$1 = (value) => value.position;

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

function get_token_at(tokens, index) {
  return get_token_at_helper(tokens, index, 0);
}

function peek_token(parser) {
  return get_token_at(parser.tokens, parser.position);
}

function consume_token(parser) {
  let $ = get_token_at(parser.tokens, parser.position);
  if ($ instanceof Ok) {
    let token = $[0];
    return new Ok([token, new Parser(parser.tokens, parser.position + 1)]);
  } else {
    return new Error(undefined);
  }
}

function parse_operation_type(parser) {
  let $ = consume_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0][0].token;
    if ($1 instanceof $lexer.Query) {
      let parser$1 = $[0][1];
      return new Ok([new $ast.Query(), parser$1]);
    } else if ($1 instanceof $lexer.Mutation) {
      let parser$1 = $[0][1];
      return new Ok([new $ast.Mutation(), parser$1]);
    } else if ($1 instanceof $lexer.Subscription) {
      let parser$1 = $[0][1];
      return new Ok([new $ast.Subscription(), parser$1]);
    } else {
      let token = $1;
      let position = $[0][0].position;
      return new Error(
        new UnexpectedToken("query, mutation, or subscription", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("operation type"));
  }
}

function parse_optional_name(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Name) {
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
          return new Ok([new Some(name), parser$1]);
        },
      );
    } else {
      return new Ok([new None(), parser]);
    }
  } else {
    return new Ok([new None(), parser]);
  }
}

function parse_name_from_parser(parser) {
  let $ = consume_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0][0].token;
    if ($1 instanceof $lexer.Name) {
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

function expect_token(parser, expected, description) {
  let $ = consume_token(parser);
  if ($ instanceof Ok) {
    let parser$1 = $[0][1];
    let token = $[0][0].token;
    let position = $[0][0].position;
    let $1 = isEqual(token, expected);
    if ($1) {
      return new Ok([new $lexer.TokenWithPosition(token, position), parser$1]);
    } else {
      return new Error(new UnexpectedToken(description, token, position));
    }
  } else {
    return new Error(new UnexpectedEOF(description));
  }
}

function parse_optional_non_null(parser, base_type) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Bang) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'!'"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $ast.NonNullType(base_type), parser$1]);
        },
      );
    } else {
      return new Ok([base_type, parser]);
    }
  } else {
    return new Ok([base_type, parser]);
  }
}

function parse_type(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.LeftBracket) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'['"); },
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
                expect_token(
                  parser$2,
                  new $lexer.RightBracket(),
                  "']' to close list type",
                ),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_optional_non_null(
                      parser$3,
                      new $ast.ListType(inner_type),
                    ),
                    (_use0) => {
                      let final_type;
                      let parser$4;
                      final_type = _use0[0];
                      parser$4 = _use0[1];
                      return new Ok([final_type, parser$4]);
                    },
                  );
                },
              );
            },
          );
        },
      );
    } else if ($1 instanceof $lexer.Name) {
      return $result.try$(
        parse_name_from_parser(parser),
        (_use0) => {
          let name;
          let parser$1;
          name = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            parse_optional_non_null(parser$1, new $ast.NamedType(name)),
            (_use0) => {
              let final_type;
              let parser$2;
              final_type = _use0[0];
              parser$2 = _use0[1];
              return new Ok([final_type, parser$2]);
            },
          );
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(new UnexpectedToken("type name or '['", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("type"));
  }
}

function parse_fragment_spread_or_inline(parser) {
  return $result.try$(
    expect_token(parser, new $lexer.Spread(), "'...' for fragment"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      let $ = peek_token(parser$1);
      if ($ instanceof Ok) {
        let $1 = $[0].token;
        if ($1 instanceof $lexer.LeftBrace) {
          return $result.try$(
            parse_selection_set(parser$1),
            (_use0) => {
              let selection_set;
              let parser$2;
              selection_set = _use0[0];
              parser$2 = _use0[1];
              return new Ok(
                [
                  new $ast.InlineFragment(
                    new $ast.InlineFragmentValue(
                      new None(),
                      toList([]),
                      selection_set,
                    ),
                  ),
                  parser$2,
                ],
              );
            },
          );
        } else if ($1 instanceof $lexer.On) {
          return $result.try$(
            (() => {
              let _pipe = consume_token(parser$1);
              return $result.map_error(
                _pipe,
                (_) => { return new UnexpectedEOF("'on' keyword"); },
              );
            })(),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return $result.try$(
                parse_name_from_parser(parser$2),
                (_use0) => {
                  let type_name;
                  let parser$3;
                  type_name = _use0[0];
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_selection_set(parser$3),
                    (_use0) => {
                      let selection_set;
                      let parser$4;
                      selection_set = _use0[0];
                      parser$4 = _use0[1];
                      return new Ok(
                        [
                          new $ast.InlineFragment(
                            new $ast.InlineFragmentValue(
                              new Some(type_name),
                              toList([]),
                              selection_set,
                            ),
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
        } else if ($1 instanceof $lexer.Name) {
          let name = $1.value;
          return $result.try$(
            (() => {
              let _pipe = consume_token(parser$1);
              return $result.map_error(
                _pipe,
                (_) => { return new UnexpectedEOF("fragment name"); },
              );
            })(),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return new Ok(
                [
                  new $ast.FragmentSpread(
                    new $ast.FragmentSpreadValue(name, toList([])),
                  ),
                  parser$2,
                ],
              );
            },
          );
        } else {
          let token = $1;
          let position = $[0].position;
          return new Error(
            new UnexpectedToken(
              "'on', '{', or fragment name after '...'",
              token,
              position,
            ),
          );
        }
      } else {
        return new Error(
          new UnexpectedEOF("'on', '{', or fragment name after '...'"),
        );
      }
    },
  );
}

function parse_selection_set(parser) {
  return $result.try$(
    expect_token(parser, new $lexer.LeftBrace(), "'{' to start selection set"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_selections(parser$1, toList([])),
        (_use0) => {
          let selections;
          let parser$2;
          selections = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(
              parser$2,
              new $lexer.RightBrace(),
              "'}' to end selection set",
            ),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return new Ok([new $ast.SelectionSet(selections), parser$3]);
            },
          );
        },
      );
    },
  );
}

function parse_selections(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.RightBrace) {
      return new Ok([$list.reverse(acc), parser]);
    } else {
      return $result.try$(
        parse_selection(parser),
        (_use0) => {
          let selection;
          let parser$1;
          selection = _use0[0];
          parser$1 = _use0[1];
          return parse_selections(parser$1, listPrepend(selection, acc));
        },
      );
    }
  } else {
    return new Error(new UnexpectedEOF("selection or '}'"));
  }
}

function parse_selection(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Spread) {
      return parse_fragment_spread_or_inline(parser);
    } else {
      let _pipe = parse_field(parser);
      return $result.map(
        _pipe,
        (result) => { return [new $ast.FieldSelection(result[0]), result[1]]; },
      );
    }
  } else {
    let _pipe = parse_field(parser);
    return $result.map(
      _pipe,
      (result) => { return [new $ast.FieldSelection(result[0]), result[1]]; },
    );
  }
}

function parse_fragment_definition(parser) {
  return $result.try$(
    expect_token(parser, new $lexer.Fragment(), "'fragment' keyword"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name_from_parser(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(parser$2, new $lexer.On(), "'on' keyword"),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return $result.try$(
                parse_name_from_parser(parser$3),
                (_use0) => {
                  let type_condition;
                  let parser$4;
                  type_condition = _use0[0];
                  parser$4 = _use0[1];
                  return $result.try$(
                    parse_selection_set(parser$4),
                    (_use0) => {
                      let selection_set;
                      let parser$5;
                      selection_set = _use0[0];
                      parser$5 = _use0[1];
                      return new Ok(
                        [
                          new $ast.Fragment(
                            name,
                            type_condition,
                            toList([]),
                            selection_set,
                          ),
                          parser$5,
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
    },
  );
}

function parse_optional_selection_set(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.LeftBrace) {
      let $2 = parse_selection_set(parser);
      if ($2 instanceof Ok) {
        let ss = $2[0][0];
        let p = $2[0][1];
        return [new Some(ss), p];
      } else {
        return [new None(), parser];
      }
    } else {
      return [new None(), parser];
    }
  } else {
    return [new None(), parser];
  }
}

function parse_list_values(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.RightBracket) {
      return new Ok([$list.reverse(acc), parser]);
    } else {
      return $result.try$(
        parse_value(parser),
        (_use0) => {
          let value;
          let parser$1;
          value = _use0[0];
          parser$1 = _use0[1];
          return parse_list_values(parser$1, listPrepend(value, acc));
        },
      );
    }
  } else {
    return $result.try$(
      parse_value(parser),
      (_use0) => {
        let value;
        let parser$1;
        value = _use0[0];
        parser$1 = _use0[1];
        return parse_list_values(parser$1, listPrepend(value, acc));
      },
    );
  }
}

function parse_value(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Dollar) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'$'"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_name_from_parser(parser$1),
            (_use0) => {
              let name;
              let parser$2;
              name = _use0[0];
              parser$2 = _use0[1];
              return new Ok([new $ast.VariableValue(name), parser$2]);
            },
          );
        },
      );
    } else if ($1 instanceof $lexer.LeftBracket) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'['"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_list_values(parser$1, toList([])),
            (_use0) => {
              let values;
              let parser$2;
              values = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(
                  parser$2,
                  new $lexer.RightBracket(),
                  "']' to close list value",
                ),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return new Ok([new $ast.ListValue(values), parser$3]);
                },
              );
            },
          );
        },
      );
    } else if ($1 instanceof $lexer.LeftBrace) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'{'"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_object_fields(parser$1, toList([])),
            (_use0) => {
              let fields;
              let parser$2;
              fields = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(
                  parser$2,
                  new $lexer.RightBrace(),
                  "'}' to close object value",
                ),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return new Ok([new $ast.ObjectValue(fields), parser$3]);
                },
              );
            },
          );
        },
      );
    } else if ($1 instanceof $lexer.TrueKeyword) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("true"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $ast.BooleanValue(true), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.FalseKeyword) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("false"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $ast.BooleanValue(false), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.NullKeyword) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("null"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return new Ok([new $ast.NullValue(), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.Name) {
      let value = $1.value;
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
          return new Ok([new $ast.EnumValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.IntValue) {
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
          return new Ok([new $ast.IntValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.FloatValue) {
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
          return new Ok([new $ast.FloatValue(value), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.StringValue) {
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
          return new Ok([new $ast.StringValue(value), parser$1]);
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
    if ($1 instanceof $lexer.Equals) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'='"); },
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
              return new Ok([new Some(value), parser$2]);
            },
          );
        },
      );
    } else {
      return new Ok([new None(), parser]);
    }
  } else {
    return new Ok([new None(), parser]);
  }
}

function parse_variable_definition(parser) {
  return $result.try$(
    expect_token(parser, new $lexer.Dollar(), "'$' for variable"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name_from_parser(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            expect_token(
              parser$2,
              new $lexer.Colon(),
              "':' after variable name",
            ),
            (_use0) => {
              let parser$3;
              parser$3 = _use0[1];
              return $result.try$(
                parse_type(parser$3),
                (_use0) => {
                  let var_type;
                  let parser$4;
                  var_type = _use0[0];
                  parser$4 = _use0[1];
                  return $result.try$(
                    parse_optional_default_value(parser$4),
                    (_use0) => {
                      let default_value;
                      let parser$5;
                      default_value = _use0[0];
                      parser$5 = _use0[1];
                      return new Ok(
                        [
                          new $ast.VariableDefinition(
                            name,
                            var_type,
                            default_value,
                            toList([]),
                          ),
                          parser$5,
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
    },
  );
}

function parse_variable_definitions_list(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Dollar) {
      return $result.try$(
        parse_variable_definition(parser),
        (_use0) => {
          let var_def;
          let parser$1;
          var_def = _use0[0];
          parser$1 = _use0[1];
          return parse_variable_definitions_list(
            parser$1,
            listPrepend(var_def, acc),
          );
        },
      );
    } else if ($1 instanceof $lexer.RightParen) {
      return new Ok([$list.reverse(acc), parser]);
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("variable definition or ')'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("variable definition or ')'"));
  }
}

function parse_variable_definitions(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.LeftParen) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'('"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_variable_definitions_list(parser$1, toList([])),
            (_use0) => {
              let defs;
              let parser$2;
              defs = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(
                  parser$2,
                  new $lexer.RightParen(),
                  "')' to end variable definitions",
                ),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return new Ok([defs, parser$3]);
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

function parse_operation_definition(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.LeftBrace) {
      return $result.try$(
        parse_selection_set(parser),
        (_use0) => {
          let selection_set;
          let parser$1;
          selection_set = _use0[0];
          parser$1 = _use0[1];
          return new Ok([new $ast.ShorthandQuery(selection_set), parser$1]);
        },
      );
    } else if ($1 instanceof $lexer.Query) {
      return $result.try$(
        parse_operation_type(parser),
        (_use0) => {
          let op_type;
          let parser$1;
          op_type = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            parse_optional_name(parser$1),
            (_use0) => {
              let name;
              let parser$2;
              name = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                parse_variable_definitions(parser$2),
                (_use0) => {
                  let variable_defs;
                  let parser$3;
                  variable_defs = _use0[0];
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_selection_set(parser$3),
                    (_use0) => {
                      let selection_set;
                      let parser$4;
                      selection_set = _use0[0];
                      parser$4 = _use0[1];
                      return new Ok(
                        [
                          new $ast.Operation(
                            op_type,
                            name,
                            variable_defs,
                            toList([]),
                            selection_set,
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
    } else if ($1 instanceof $lexer.Mutation) {
      return $result.try$(
        parse_operation_type(parser),
        (_use0) => {
          let op_type;
          let parser$1;
          op_type = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            parse_optional_name(parser$1),
            (_use0) => {
              let name;
              let parser$2;
              name = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                parse_variable_definitions(parser$2),
                (_use0) => {
                  let variable_defs;
                  let parser$3;
                  variable_defs = _use0[0];
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_selection_set(parser$3),
                    (_use0) => {
                      let selection_set;
                      let parser$4;
                      selection_set = _use0[0];
                      parser$4 = _use0[1];
                      return new Ok(
                        [
                          new $ast.Operation(
                            op_type,
                            name,
                            variable_defs,
                            toList([]),
                            selection_set,
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
    } else if ($1 instanceof $lexer.Subscription) {
      return $result.try$(
        parse_operation_type(parser),
        (_use0) => {
          let op_type;
          let parser$1;
          op_type = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            parse_optional_name(parser$1),
            (_use0) => {
              let name;
              let parser$2;
              name = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                parse_variable_definitions(parser$2),
                (_use0) => {
                  let variable_defs;
                  let parser$3;
                  variable_defs = _use0[0];
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_selection_set(parser$3),
                    (_use0) => {
                      let selection_set;
                      let parser$4;
                      selection_set = _use0[0];
                      parser$4 = _use0[1];
                      return new Ok(
                        [
                          new $ast.Operation(
                            op_type,
                            name,
                            variable_defs,
                            toList([]),
                            selection_set,
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
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("operation or selection set", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("operation or selection set"));
  }
}

function parse_definition(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.Fragment) {
      let _pipe = parse_fragment_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [new $ast.FragmentDefinition(result[0]), result[1]];
        },
      );
    } else {
      let _pipe = parse_operation_definition(parser);
      return $result.map(
        _pipe,
        (result) => {
          return [new $ast.OperationDefinition(result[0]), result[1]];
        },
      );
    }
  } else {
    let _pipe = parse_operation_definition(parser);
    return $result.map(
      _pipe,
      (result) => {
        return [new $ast.OperationDefinition(result[0]), result[1]];
      },
    );
  }
}

function parse_definitions(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.EOF) {
      return new Ok([$list.reverse(acc), parser]);
    } else {
      return $result.try$(
        parse_definition(parser),
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
      return new Ok([new $ast.Document(definitions), parser$1]);
    },
  );
}

export function parse(input) {
  return $result.try$(
    (() => {
      let _pipe = $lexer.tokenize(input);
      return $result.map_error(_pipe, (var0) => { return new LexError(var0); });
    })(),
    (tokens) => {
      let parser = new Parser(tokens, 0);
      let _pipe = parse_document(parser);
      return $result.map(_pipe, (result) => { return result[0]; });
    },
  );
}

function parse_object_fields(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.RightBrace) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $lexer.Name) {
      return $result.try$(
        parse_name_from_parser(parser),
        (_use0) => {
          let name;
          let parser$1;
          name = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            expect_token(parser$1, new $lexer.Colon(), "':' after field name"),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return $result.try$(
                parse_value(parser$2),
                (_use0) => {
                  let value;
                  let parser$3;
                  value = _use0[0];
                  parser$3 = _use0[1];
                  return parse_object_fields(
                    parser$3,
                    listPrepend(new $ast.ObjectField(name, value), acc),
                  );
                },
              );
            },
          );
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(
        new UnexpectedToken("object field or '}'", token, position),
      );
    }
  } else {
    return new Error(new UnexpectedEOF("object field or '}'"));
  }
}

function parse_arguments_list(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.RightParen) {
      return new Ok([$list.reverse(acc), parser]);
    } else if ($1 instanceof $lexer.Name) {
      return $result.try$(
        parse_name_from_parser(parser),
        (_use0) => {
          let name;
          let parser$1;
          name = _use0[0];
          parser$1 = _use0[1];
          return $result.try$(
            expect_token(
              parser$1,
              new $lexer.Colon(),
              "':' after argument name",
            ),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return $result.try$(
                parse_value(parser$2),
                (_use0) => {
                  let value;
                  let parser$3;
                  value = _use0[0];
                  parser$3 = _use0[1];
                  return parse_arguments_list(
                    parser$3,
                    listPrepend(new $ast.Argument(name, value), acc),
                  );
                },
              );
            },
          );
        },
      );
    } else {
      let token = $1;
      let position = $[0].position;
      return new Error(new UnexpectedToken("argument or ')'", token, position));
    }
  } else {
    return new Error(new UnexpectedEOF("argument or ')'"));
  }
}

function parse_arguments(parser) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.LeftParen) {
      return $result.try$(
        (() => {
          let _pipe = consume_token(parser);
          return $result.map_error(
            _pipe,
            (_) => { return new UnexpectedEOF("'('"); },
          );
        })(),
        (_use0) => {
          let parser$1;
          parser$1 = _use0[1];
          return $result.try$(
            parse_arguments_list(parser$1, toList([])),
            (_use0) => {
              let args;
              let parser$2;
              args = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                expect_token(
                  parser$2,
                  new $lexer.RightParen(),
                  "')' to close arguments",
                ),
                (_use0) => {
                  let parser$3;
                  parser$3 = _use0[1];
                  return new Ok([args, parser$3]);
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

function parse_directive(parser) {
  return $result.try$(
    expect_token(parser, new $lexer.At(), "'@' for directive"),
    (_use0) => {
      let parser$1;
      parser$1 = _use0[1];
      return $result.try$(
        parse_name_from_parser(parser$1),
        (_use0) => {
          let name;
          let parser$2;
          name = _use0[0];
          parser$2 = _use0[1];
          return $result.try$(
            parse_arguments(parser$2),
            (_use0) => {
              let arguments$;
              let parser$3;
              arguments$ = _use0[0];
              parser$3 = _use0[1];
              return new Ok([new $ast.Directive(name, arguments$), parser$3]);
            },
          );
        },
      );
    },
  );
}

function parse_directives_list(parser, acc) {
  let $ = peek_token(parser);
  if ($ instanceof Ok) {
    let $1 = $[0].token;
    if ($1 instanceof $lexer.At) {
      return $result.try$(
        parse_directive(parser),
        (_use0) => {
          let directive;
          let parser$1;
          directive = _use0[0];
          parser$1 = _use0[1];
          return parse_directives_list(parser$1, listPrepend(directive, acc));
        },
      );
    } else {
      return new Ok([$list.reverse(acc), parser]);
    }
  } else {
    return new Ok([$list.reverse(acc), parser]);
  }
}

function parse_directives(parser) {
  return parse_directives_list(parser, toList([]));
}

function parse_field(parser) {
  return $result.try$(
    parse_name_from_parser(parser),
    (_use0) => {
      let first_name;
      let parser$1;
      first_name = _use0[0];
      parser$1 = _use0[1];
      let $ = peek_token(parser$1);
      if ($ instanceof Ok) {
        let $1 = $[0].token;
        if ($1 instanceof $lexer.Colon) {
          return $result.try$(
            (() => {
              let _pipe = consume_token(parser$1);
              return $result.map_error(
                _pipe,
                (_) => { return new UnexpectedEOF("colon"); },
              );
            })(),
            (_use0) => {
              let parser$2;
              parser$2 = _use0[1];
              return $result.try$(
                parse_name_from_parser(parser$2),
                (_use0) => {
                  let second_name;
                  let parser$3;
                  second_name = _use0[0];
                  parser$3 = _use0[1];
                  return $result.try$(
                    parse_arguments(parser$3),
                    (_use0) => {
                      let arguments$;
                      let parser$4;
                      arguments$ = _use0[0];
                      parser$4 = _use0[1];
                      return $result.try$(
                        parse_directives(parser$4),
                        (_use0) => {
                          let directives;
                          let parser$5;
                          directives = _use0[0];
                          parser$5 = _use0[1];
                          let $2 = parse_optional_selection_set(parser$5);
                          let selection_set;
                          let parser$6;
                          selection_set = $2[0];
                          parser$6 = $2[1];
                          return new Ok(
                            [
                              new $ast.Field(
                                new Some(first_name),
                                second_name,
                                arguments$,
                                directives,
                                selection_set,
                              ),
                              parser$6,
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
        } else {
          return $result.try$(
            parse_arguments(parser$1),
            (_use0) => {
              let arguments$;
              let parser$2;
              arguments$ = _use0[0];
              parser$2 = _use0[1];
              return $result.try$(
                parse_directives(parser$2),
                (_use0) => {
                  let directives;
                  let parser$3;
                  directives = _use0[0];
                  parser$3 = _use0[1];
                  let $2 = parse_optional_selection_set(parser$3);
                  let selection_set;
                  let parser$4;
                  selection_set = $2[0];
                  parser$4 = $2[1];
                  return new Ok(
                    [
                      new $ast.Field(
                        new None(),
                        first_name,
                        arguments$,
                        directives,
                        selection_set,
                      ),
                      parser$4,
                    ],
                  );
                },
              );
            },
          );
        }
      } else {
        return $result.try$(
          parse_arguments(parser$1),
          (_use0) => {
            let arguments$;
            let parser$2;
            arguments$ = _use0[0];
            parser$2 = _use0[1];
            return $result.try$(
              parse_directives(parser$2),
              (_use0) => {
                let directives;
                let parser$3;
                directives = _use0[0];
                parser$3 = _use0[1];
                let $1 = parse_optional_selection_set(parser$3);
                let selection_set;
                let parser$4;
                selection_set = $1[0];
                parser$4 = $1[1];
                return new Ok(
                  [
                    new $ast.Field(
                      new None(),
                      first_name,
                      arguments$,
                      directives,
                      selection_set,
                    ),
                    parser$4,
                  ],
                );
              },
            );
          },
        );
      }
    },
  );
}
