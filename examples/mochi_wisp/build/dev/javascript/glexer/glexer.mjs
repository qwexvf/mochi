import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $splitter from "../splitter/splitter.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "./gleam.mjs";
import { string_length as length, slice_bytes, drop_byte } from "./glexer.ffi.mjs";
import * as $token from "./glexer/token.mjs";

class Lexer extends $CustomType {
  constructor(original_source, source, byte_offset, preserve_whitespace, preserve_comments, mode, newlines) {
    super();
    this.original_source = original_source;
    this.source = source;
    this.byte_offset = byte_offset;
    this.preserve_whitespace = preserve_whitespace;
    this.preserve_comments = preserve_comments;
    this.mode = mode;
    this.newlines = newlines;
  }
}

class Normal extends $CustomType {}

class CheckForMinus extends $CustomType {}

class CheckForNestedDot extends $CustomType {}

class CheckForNestedDotOrMinus extends $CustomType {}

class HasNestedDot extends $CustomType {}

export class Position extends $CustomType {
  constructor(byte_offset) {
    super();
    this.byte_offset = byte_offset;
  }
}
export const Position$Position = (byte_offset) => new Position(byte_offset);
export const Position$isPosition = (value) => value instanceof Position;
export const Position$Position$byte_offset = (value) => value.byte_offset;
export const Position$Position$0 = (value) => value.byte_offset;

class RegularComment extends $CustomType {}

class DocComment extends $CustomType {}

class ModuleComment extends $CustomType {}

class LexInt extends $CustomType {}

class LexFloat extends $CustomType {}

class LexFloatExponent extends $CustomType {}

export function new$(source) {
  return new Lexer(
    source,
    source,
    0,
    true,
    true,
    new Normal(),
    $splitter.new$(toList(["\r\n", "\n"])),
  );
}

export function discard_whitespace(lexer) {
  return new Lexer(
    lexer.original_source,
    lexer.source,
    lexer.byte_offset,
    false,
    lexer.preserve_comments,
    lexer.mode,
    lexer.newlines,
  );
}

export function discard_comments(lexer) {
  return new Lexer(
    lexer.original_source,
    lexer.source,
    lexer.byte_offset,
    lexer.preserve_whitespace,
    false,
    lexer.mode,
    lexer.newlines,
  );
}

function some_token(result) {
  let lexer;
  let token$1;
  lexer = result[0];
  token$1 = result[1];
  return [lexer, new Some(token$1)];
}

function advance(lexer, source, offset) {
  return new Lexer(
    lexer.original_source,
    source,
    lexer.byte_offset + offset,
    lexer.preserve_whitespace,
    lexer.preserve_comments,
    lexer.mode,
    lexer.newlines,
  );
}

function advanced(token, lexer, source, offset) {
  return [advance(lexer, source, offset), token];
}

function token(lexer, token, source, offset) {
  let _pipe = [token, new Position(lexer.byte_offset)];
  let _pipe$1 = advanced(_pipe, lexer, source, offset);
  return some_token(_pipe$1);
}

function check_for_minus(lexer) {
  let $ = lexer.source;
  if ($.startsWith("-")) {
    let source = $.slice(1);
    let $1 = token(lexer, new $token.Minus(), source, 1);
    let lexer$1;
    let token$1;
    lexer$1 = $1[0];
    token$1 = $1[1];
    return new Ok(
      [
        new Lexer(
          lexer$1.original_source,
          lexer$1.source,
          lexer$1.byte_offset,
          lexer$1.preserve_whitespace,
          lexer$1.preserve_comments,
          new Normal(),
          lexer$1.newlines,
        ),
        token$1,
      ],
    );
  } else {
    return new Error(undefined);
  }
}

function check_for_nested_dot(lexer) {
  let $ = lexer.source;
  if ($.startsWith("..")) {
    let source = $.slice(2);
    return new Ok(token(lexer, new $token.DotDot(), source, 2));
  } else if ($.startsWith(".")) {
    let source = $.slice(1);
    if (source.startsWith("0")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("1")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("2")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("3")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("4")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("5")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("6")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("7")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("8")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else if (source.startsWith("9")) {
      let $1 = token(lexer, new $token.Dot(), source, 1);
      let lexer$1;
      let token$1;
      lexer$1 = $1[0];
      token$1 = $1[1];
      return new Ok(
        [
          new Lexer(
            lexer$1.original_source,
            lexer$1.source,
            lexer$1.byte_offset,
            lexer$1.preserve_whitespace,
            lexer$1.preserve_comments,
            new HasNestedDot(),
            lexer$1.newlines,
          ),
          token$1,
        ],
      );
    } else {
      return new Ok(token(lexer, new $token.Dot(), source, 1));
    }
  } else {
    return new Error(undefined);
  }
}

/**
 * Turn a sequence of tokens back to their Gleam source code representation.
 */
export function to_source(tokens) {
  return $list.fold(
    tokens,
    "",
    (source, _use1) => {
      let tok;
      tok = _use1[0];
      return source + $token.to_source(tok);
    },
  );
}

/**
 * Ignores the rest of the line until it finds a newline, and signals the
 * caller to continue lexing.
 * 
 * @ignore
 */
function skip_comment(lexer) {
  let $ = $splitter.split_before(lexer.newlines, lexer.source);
  let prefix;
  let suffix;
  prefix = $[0];
  suffix = $[1];
  let eaten = length(prefix);
  let lexer$1 = advance(lexer, suffix, eaten);
  return [lexer$1, new None()];
}

function comment(lexer, kind, start) {
  let $ = $splitter.split_before(lexer.newlines, lexer.source);
  let prefix;
  let suffix;
  prefix = $[0];
  suffix = $[1];
  let eaten = length(prefix);
  let lexer$1 = advance(lexer, suffix, eaten);
  let _block;
  if (kind instanceof RegularComment) {
    _block = new $token.CommentNormal(prefix);
  } else if (kind instanceof DocComment) {
    _block = new $token.CommentDoc(prefix);
  } else {
    _block = new $token.CommentModule(prefix);
  }
  let token$1 = _block;
  return [lexer$1, new Some([token$1, new Position(start)])];
}

function lex_digits(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("8")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("9")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let digits = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, digits];
    }
  }
}

function lex_lowercase_name(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("a")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("b")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("c")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("d")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("e")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("f")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("g")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("h")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("i")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("j")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("k")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("l")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("m")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("n")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("o")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("p")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("q")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("r")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("s")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("t")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("u")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("v")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("w")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("x")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("y")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("z")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("8")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("9")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("_")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let name = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, name];
    }
  }
}

function lex_uppercase_name(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("a")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("b")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("c")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("d")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("e")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("f")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("g")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("h")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("i")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("j")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("k")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("l")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("m")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("n")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("o")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("p")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("q")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("r")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("s")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("t")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("u")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("v")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("w")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("x")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("y")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("z")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("A")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("B")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("C")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("D")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("E")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("F")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("G")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("H")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("I")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("J")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("K")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("L")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("M")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("N")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("O")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("P")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("Q")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("R")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("S")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("T")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("U")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("V")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("W")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("X")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("Y")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("Z")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("8")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("9")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let name = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, name];
    }
  }
}

function whitespace(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith(" ")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("\t")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("\n")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("\r")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let $1 = lexer.preserve_whitespace;
      if ($1) {
        let content = slice_bytes(lexer.original_source, start, slice_size);
        return [
          lexer,
          new Some([new $token.Space(content), new Position(start)]),
        ];
      } else {
        return [lexer, new None()];
      }
    }
  }
}

function lex_binary(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("_")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let content = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, new Some([new $token.Int(content), new Position(start)])];
    }
  }
}

function lex_octal(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("_")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let content = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, new Some([new $token.Int(content), new Position(start)])];
    }
  }
}

function lex_hexadecimal(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("_")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("8")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("9")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("a")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("A")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("b")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("B")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("c")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("C")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("d")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("D")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("e")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("E")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("f")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("F")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else {
      let content = slice_bytes(lexer.original_source, start, slice_size);
      return [lexer, new Some([new $token.Int(content), new Position(start)])];
    }
  }
}

function lex_number(loop$lexer, loop$mode, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let mode = loop$mode;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("_")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("0")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("1")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("2")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("3")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("4")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("5")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("6")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("7")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("8")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith("9")) {
      let source = $.slice(1);
      let _pipe = advance(lexer, source, 1);
      loop$lexer = _pipe;
      loop$mode = mode;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    } else if ($.startsWith(".")) {
      if (mode instanceof LexInt) {
        let source = $.slice(1);
        let _pipe = advance(lexer, source, 1);
        loop$lexer = _pipe;
        loop$mode = new LexFloat();
        loop$start = start;
        loop$slice_size = slice_size + 1;
      } else if (mode instanceof LexFloat) {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Float(content), new Position(start)]),
        ];
      } else {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Float(content), new Position(start)]),
        ];
      }
    } else if ($.startsWith("e-")) {
      if (mode instanceof LexInt) {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Int(content), new Position(start)]),
        ];
      } else if (mode instanceof LexFloat) {
        let source = $.slice(2);
        let _pipe = advance(lexer, source, 2);
        loop$lexer = _pipe;
        loop$mode = new LexFloatExponent();
        loop$start = start;
        loop$slice_size = slice_size + 2;
      } else {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Float(content), new Position(start)]),
        ];
      }
    } else if ($.startsWith("e")) {
      if (mode instanceof LexInt) {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Int(content), new Position(start)]),
        ];
      } else if (mode instanceof LexFloat) {
        let source = $.slice(1);
        let _pipe = advance(lexer, source, 1);
        loop$lexer = _pipe;
        loop$mode = new LexFloatExponent();
        loop$start = start;
        loop$slice_size = slice_size + 1;
      } else {
        let lexer$1 = new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new CheckForMinus(),
          lexer.newlines,
        );
        let content = slice_bytes(lexer$1.original_source, start, slice_size);
        return [
          lexer$1,
          new Some([new $token.Float(content), new Position(start)]),
        ];
      }
    } else if (mode instanceof LexInt) {
      let lexer$1 = new Lexer(
        lexer.original_source,
        lexer.source,
        lexer.byte_offset,
        lexer.preserve_whitespace,
        lexer.preserve_comments,
        new CheckForMinus(),
        lexer.newlines,
      );
      let content = slice_bytes(lexer$1.original_source, start, slice_size);
      return [lexer$1, new Some([new $token.Int(content), new Position(start)])];
    } else if (mode instanceof LexFloat) {
      let lexer$1 = new Lexer(
        lexer.original_source,
        lexer.source,
        lexer.byte_offset,
        lexer.preserve_whitespace,
        lexer.preserve_comments,
        new CheckForMinus(),
        lexer.newlines,
      );
      let content = slice_bytes(lexer$1.original_source, start, slice_size);
      return [
        lexer$1,
        new Some([new $token.Float(content), new Position(start)]),
      ];
    } else {
      let lexer$1 = new Lexer(
        lexer.original_source,
        lexer.source,
        lexer.byte_offset,
        lexer.preserve_whitespace,
        lexer.preserve_comments,
        new CheckForMinus(),
        lexer.newlines,
      );
      let content = slice_bytes(lexer$1.original_source, start, slice_size);
      return [
        lexer$1,
        new Some([new $token.Float(content), new Position(start)]),
      ];
    }
  }
}

function lex_string(loop$lexer, loop$start, loop$slice_size) {
  while (true) {
    let lexer = loop$lexer;
    let start = loop$start;
    let slice_size = loop$slice_size;
    let $ = lexer.source;
    if ($.startsWith("\"")) {
      let source = $.slice(1);
      let content = slice_bytes(lexer.original_source, start + 1, slice_size);
      let _pipe = [new $token.String(content), new Position(start)];
      let _pipe$1 = advanced(_pipe, lexer, source, 1);
      return some_token(_pipe$1);
    } else if ($.startsWith("\\")) {
      let source = $.slice(1);
      let $1 = $string.pop_grapheme(source);
      if ($1 instanceof Ok) {
        let grapheme = $1[0][0];
        let source$1 = $1[0][1];
        let offset = 1 + length(grapheme);
        let _pipe = advance(lexer, source$1, offset);
        loop$lexer = _pipe;
        loop$start = start;
        loop$slice_size = slice_size + offset;
      } else {
        let _pipe = advance(lexer, source, 1);
        loop$lexer = _pipe;
        loop$start = start;
        loop$slice_size = slice_size + 1;
      }
    } else if ($ === "") {
      let content = slice_bytes(lexer.original_source, start + 1, slice_size);
      return [
        lexer,
        new Some([new $token.UnterminatedString(content), new Position(start)]),
      ];
    } else {
      let _pipe = advance(lexer, drop_byte(lexer.source), 1);
      loop$lexer = _pipe;
      loop$start = start;
      loop$slice_size = slice_size + 1;
    }
  }
}

function next(lexer) {
  let $ = lexer.mode;
  if ($ instanceof Normal) {
    let $1 = lexer.source;
    if ($1.startsWith(" ")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return whitespace(_pipe, lexer.byte_offset, 1);
    } else if ($1.startsWith("\n")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return whitespace(_pipe, lexer.byte_offset, 1);
    } else if ($1.startsWith("\r")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return whitespace(_pipe, lexer.byte_offset, 1);
    } else if ($1.startsWith("\t")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return whitespace(_pipe, lexer.byte_offset, 1);
    } else if ($1.startsWith("////")) {
      let source = $1.slice(4);
      let $2 = lexer.preserve_comments;
      if ($2) {
        let _pipe = advance(lexer, source, 4);
        return comment(_pipe, new ModuleComment(), lexer.byte_offset);
      } else {
        return skip_comment(lexer);
      }
    } else if ($1.startsWith("///")) {
      let source = $1.slice(3);
      let $2 = lexer.preserve_comments;
      if ($2) {
        let _pipe = advance(lexer, source, 3);
        return comment(_pipe, new DocComment(), lexer.byte_offset);
      } else {
        return skip_comment(lexer);
      }
    } else if ($1.startsWith("//")) {
      let source = $1.slice(2);
      let $2 = lexer.preserve_comments;
      if ($2) {
        let _pipe = advance(lexer, source, 2);
        return comment(_pipe, new RegularComment(), lexer.byte_offset);
      } else {
        return skip_comment(lexer);
      }
    } else if ($1.startsWith("(")) {
      let source = $1.slice(1);
      return token(lexer, new $token.LeftParen(), source, 1);
    } else if ($1.startsWith(")")) {
      let source = $1.slice(1);
      return token(lexer, new $token.RightParen(), source, 1);
    } else if ($1.startsWith("{")) {
      let source = $1.slice(1);
      return token(lexer, new $token.LeftBrace(), source, 1);
    } else if ($1.startsWith("}")) {
      let source = $1.slice(1);
      return token(lexer, new $token.RightBrace(), source, 1);
    } else if ($1.startsWith("[")) {
      let source = $1.slice(1);
      return token(lexer, new $token.LeftSquare(), source, 1);
    } else if ($1.startsWith("]")) {
      let source = $1.slice(1);
      return token(lexer, new $token.RightSquare(), source, 1);
    } else if ($1.startsWith("@")) {
      let source = $1.slice(1);
      return token(lexer, new $token.At(), source, 1);
    } else if ($1.startsWith(":")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Colon(), source, 1);
    } else if ($1.startsWith(",")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Comma(), source, 1);
    } else if ($1.startsWith("..")) {
      let source = $1.slice(2);
      return token(lexer, new $token.DotDot(), source, 2);
    } else if ($1.startsWith(".")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Dot(), source, 1);
    } else if ($1.startsWith("#")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Hash(), source, 1);
    } else if ($1.startsWith("!=")) {
      let source = $1.slice(2);
      return token(lexer, new $token.NotEqual(), source, 2);
    } else if ($1.startsWith("!")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Bang(), source, 1);
    } else if ($1.startsWith("==")) {
      let source = $1.slice(2);
      return token(lexer, new $token.EqualEqual(), source, 2);
    } else if ($1.startsWith("=")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Equal(), source, 1);
    } else if ($1.startsWith("|>")) {
      let source = $1.slice(2);
      return token(lexer, new $token.Pipe(), source, 2);
    } else if ($1.startsWith("||")) {
      let source = $1.slice(2);
      return token(lexer, new $token.VBarVBar(), source, 2);
    } else if ($1.startsWith("|")) {
      let source = $1.slice(1);
      return token(lexer, new $token.VBar(), source, 1);
    } else if ($1.startsWith("&&")) {
      let source = $1.slice(2);
      return token(lexer, new $token.AmperAmper(), source, 2);
    } else if ($1.startsWith("<<")) {
      let source = $1.slice(2);
      return token(lexer, new $token.LessLess(), source, 2);
    } else if ($1.startsWith(">>")) {
      let source = $1.slice(2);
      return token(lexer, new $token.GreaterGreater(), source, 2);
    } else if ($1.startsWith("<-")) {
      let source = $1.slice(2);
      return token(lexer, new $token.LeftArrow(), source, 2);
    } else if ($1.startsWith("->")) {
      let source = $1.slice(2);
      return token(lexer, new $token.RightArrow(), source, 2);
    } else if ($1.startsWith("<>")) {
      let source = $1.slice(2);
      return token(lexer, new $token.LessGreater(), source, 2);
    } else if ($1.startsWith("+.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.PlusDot(), source, 2);
    } else if ($1.startsWith("-.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.MinusDot(), source, 2);
    } else if ($1.startsWith("*.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.StarDot(), source, 2);
    } else if ($1.startsWith("/.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.SlashDot(), source, 2);
    } else if ($1.startsWith("<=.")) {
      let source = $1.slice(3);
      return token(lexer, new $token.LessEqualDot(), source, 3);
    } else if ($1.startsWith("<.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.LessDot(), source, 2);
    } else if ($1.startsWith(">=.")) {
      let source = $1.slice(3);
      return token(lexer, new $token.GreaterEqualDot(), source, 3);
    } else if ($1.startsWith(">.")) {
      let source = $1.slice(2);
      return token(lexer, new $token.GreaterDot(), source, 2);
    } else if ($1.startsWith("0b")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_binary(_pipe, lexer.byte_offset, 2);
    } else if ($1.startsWith("0o")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_octal(_pipe, lexer.byte_offset, 2);
    } else if ($1.startsWith("0x")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_hexadecimal(_pipe, lexer.byte_offset, 2);
    } else if ($1.startsWith("0")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("1")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("2")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("3")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("4")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("5")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("6")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("7")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("8")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("9")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 1);
    } else if ($1.startsWith("-0")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-1")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-2")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-3")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-4")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-5")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-6")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-7")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-8")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("-9")) {
      let source = $1.slice(2);
      let _pipe = advance(lexer, source, 2);
      return lex_number(_pipe, new LexInt(), lexer.byte_offset, 2);
    } else if ($1.startsWith("+")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Plus(), source, 1);
    } else if ($1.startsWith("-")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Minus(), source, 1);
    } else if ($1.startsWith("*")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Star(), source, 1);
    } else if ($1.startsWith("/")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Slash(), source, 1);
    } else if ($1.startsWith("<=")) {
      let source = $1.slice(2);
      return token(lexer, new $token.LessEqual(), source, 2);
    } else if ($1.startsWith("<")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Less(), source, 1);
    } else if ($1.startsWith(">=")) {
      let source = $1.slice(2);
      return token(lexer, new $token.GreaterEqual(), source, 2);
    } else if ($1.startsWith(">")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Greater(), source, 1);
    } else if ($1.startsWith("%")) {
      let source = $1.slice(1);
      return token(lexer, new $token.Percent(), source, 1);
    } else if ($1.startsWith("\"")) {
      let source = $1.slice(1);
      let _pipe = advance(lexer, source, 1);
      return lex_string(_pipe, lexer.byte_offset, 0);
    } else if ($1.startsWith("_")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset + 1, 0);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.DiscardName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("a")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("b")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("c")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("d")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("e")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("f")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("g")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("h")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("i")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("j")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("k")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("l")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("m")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("n")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("o")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("p")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("q")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("r")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("s")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("t")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("u")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("v")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("w")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("x")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("y")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("z")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_lowercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      let _block$1;
      if (name === "as") {
        _block$1 = new $token.As();
      } else if (name === "assert") {
        _block$1 = new $token.Assert();
      } else if (name === "auto") {
        _block$1 = new $token.Auto();
      } else if (name === "case") {
        _block$1 = new $token.Case();
      } else if (name === "const") {
        _block$1 = new $token.Const();
      } else if (name === "delegate") {
        _block$1 = new $token.Delegate();
      } else if (name === "derive") {
        _block$1 = new $token.Derive();
      } else if (name === "echo") {
        _block$1 = new $token.Echo();
      } else if (name === "else") {
        _block$1 = new $token.Else();
      } else if (name === "fn") {
        _block$1 = new $token.Fn();
      } else if (name === "if") {
        _block$1 = new $token.If();
      } else if (name === "implement") {
        _block$1 = new $token.Implement();
      } else if (name === "import") {
        _block$1 = new $token.Import();
      } else if (name === "let") {
        _block$1 = new $token.Let();
      } else if (name === "macro") {
        _block$1 = new $token.Macro();
      } else if (name === "opaque") {
        _block$1 = new $token.Opaque();
      } else if (name === "panic") {
        _block$1 = new $token.Panic();
      } else if (name === "pub") {
        _block$1 = new $token.Pub();
      } else if (name === "test") {
        _block$1 = new $token.Test();
      } else if (name === "todo") {
        _block$1 = new $token.Todo();
      } else if (name === "type") {
        _block$1 = new $token.Type();
      } else if (name === "use") {
        _block$1 = new $token.Use();
      } else {
        _block$1 = new $token.Name(name);
      }
      let token$1 = _block$1;
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDotOrMinus(),
        lexer$1.newlines,
      );
      return [lexer$2, new Some([token$1, new Position(byte_offset)])];
    } else if ($1.startsWith("A")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("B")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("C")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("D")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("E")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("F")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("G")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("H")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("I")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("J")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("K")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("L")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("M")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("N")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("O")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("P")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("Q")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("R")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("S")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("T")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("U")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("V")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("W")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("X")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("Y")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("Z")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_uppercase_name(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let name;
      lexer$1 = $2[0];
      name = $2[1];
      return [
        lexer$1,
        new Some([new $token.UpperName(name), new Position(byte_offset)]),
      ];
    } else {
      let $2 = $string.pop_grapheme(lexer.source);
      if ($2 instanceof Ok) {
        let grapheme = $2[0][0];
        let source = $2[0][1];
        return token(
          lexer,
          new $token.UnexpectedGrapheme(grapheme),
          source,
          length(grapheme),
        );
      } else {
        return [
          lexer,
          new Some([new $token.EndOfFile(), new Position(lexer.byte_offset)]),
        ];
      }
    }
  } else if ($ instanceof CheckForMinus) {
    let $1 = check_for_minus(lexer);
    if ($1 instanceof Ok) {
      let result = $1[0];
      return result;
    } else {
      return [
        new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new Normal(),
          lexer.newlines,
        ),
        new None(),
      ];
    }
  } else if ($ instanceof CheckForNestedDot) {
    let $1 = check_for_nested_dot(lexer);
    if ($1 instanceof Ok) {
      let result = $1[0];
      return result;
    } else {
      return [
        new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new Normal(),
          lexer.newlines,
        ),
        new None(),
      ];
    }
  } else if ($ instanceof CheckForNestedDotOrMinus) {
    let $1 = check_for_nested_dot(lexer);
    if ($1 instanceof Ok) {
      let result = $1[0];
      return result;
    } else {
      let $2 = check_for_minus(lexer);
      if ($2 instanceof Ok) {
        let result = $2[0];
        return result;
      } else {
        return [
          new Lexer(
            lexer.original_source,
            lexer.source,
            lexer.byte_offset,
            lexer.preserve_whitespace,
            lexer.preserve_comments,
            new Normal(),
            lexer.newlines,
          ),
          new None(),
        ];
      }
    }
  } else {
    let $1 = lexer.source;
    if ($1.startsWith("0")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("1")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("2")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("3")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("4")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("5")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("6")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("7")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("8")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else if ($1.startsWith("9")) {
      let source = $1.slice(1);
      let byte_offset = lexer.byte_offset;
      let _block;
      let _pipe = advance(lexer, source, 1);
      _block = lex_digits(_pipe, byte_offset, 1);
      let $2 = _block;
      let lexer$1;
      let int;
      lexer$1 = $2[0];
      int = $2[1];
      let lexer$2 = new Lexer(
        lexer$1.original_source,
        lexer$1.source,
        lexer$1.byte_offset,
        lexer$1.preserve_whitespace,
        lexer$1.preserve_comments,
        new CheckForNestedDot(),
        lexer$1.newlines,
      );
      return [
        lexer$2,
        new Some([new $token.Int(int), new Position(byte_offset)]),
      ];
    } else {
      return [
        new Lexer(
          lexer.original_source,
          lexer.source,
          lexer.byte_offset,
          lexer.preserve_whitespace,
          lexer.preserve_comments,
          new Normal(),
          lexer.newlines,
        ),
        new None(),
      ];
    }
  }
}

function do_lex(loop$lexer, loop$tokens) {
  while (true) {
    let lexer = loop$lexer;
    let tokens = loop$tokens;
    let $ = next(lexer);
    let $1 = $[1];
    if ($1 instanceof Some) {
      let $2 = $1[0][0];
      if ($2 instanceof $token.EndOfFile) {
        return tokens;
      } else {
        let lexer$1 = $[0];
        let token$1 = $1[0];
        loop$lexer = lexer$1;
        loop$tokens = listPrepend(token$1, tokens);
      }
    } else {
      let lexer$1 = $[0];
      loop$lexer = lexer$1;
      loop$tokens = tokens;
    }
  }
}

export function lex(lexer) {
  let _pipe = do_lex(lexer, toList([]));
  return $list.reverse(_pipe);
}

function unescape_codepoint(loop$escaped, loop$unescaped, loop$codepoint) {
  while (true) {
    let escaped = loop$escaped;
    let unescaped = loop$unescaped;
    let codepoint = loop$codepoint;
    let $ = $string.pop_grapheme(escaped);
    if ($ instanceof Ok) {
      let $1 = $[0][0];
      if ($1 === "}") {
        let escaped$1 = $[0][1];
        return $result.try$(
          $int.base_parse(codepoint, 16),
          (codepoint) => {
            return $result.try$(
              $string.utf_codepoint(codepoint),
              (codepoint) => {
                let codepoint$1 = $string.from_utf_codepoints(
                  toList([codepoint]),
                );
                return unescape_loop(escaped$1, unescaped + codepoint$1);
              },
            );
          },
        );
      } else {
        let c = $1;
        let escaped$1 = $[0][1];
        loop$escaped = escaped$1;
        loop$unescaped = unescaped;
        loop$codepoint = codepoint + c;
      }
    } else {
      return $;
    }
  }
}

function unescape_loop(loop$escaped, loop$unescaped) {
  while (true) {
    let escaped = loop$escaped;
    let unescaped = loop$unescaped;
    if (escaped.startsWith("\\\"")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\"";
    } else if (escaped.startsWith("\\\\")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\\";
    } else if (escaped.startsWith("\\f")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\f";
    } else if (escaped.startsWith("\\n")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\n";
    } else if (escaped.startsWith("\\r")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\r";
    } else if (escaped.startsWith("\\t")) {
      let escaped$1 = escaped.slice(2);
      loop$escaped = escaped$1;
      loop$unescaped = unescaped + "\t";
    } else if (escaped.startsWith("\\u{")) {
      let escaped$1 = escaped.slice(3);
      return unescape_codepoint(escaped$1, unescaped, "");
    } else if (escaped.startsWith("\\")) {
      return new Error(undefined);
    } else {
      let $ = $string.pop_grapheme(escaped);
      if ($ instanceof Ok) {
        let grapheme = $[0][0];
        let escaped$1 = $[0][1];
        loop$escaped = escaped$1;
        loop$unescaped = unescaped + grapheme;
      } else {
        return new Ok(unescaped);
      }
    }
  }
}

/**
 * Convert the value of a string token to the string it represents.
 *
 * This function can fail if the original string contains invalid escape sequences.
 *
 * ```gleam
 * unescape_string("\\\"X\\\" marks the spot")
 * // --> Ok("\"X\" marks the spot")
 *
 * unescape_string("\\u{1F600}")
 * // --> Ok("😀")
 *
 * unescape_string("\\x")
 * // --> Error(Nil)
 * ```
 */
export function unescape_string(string) {
  return unescape_loop(string, "");
}
