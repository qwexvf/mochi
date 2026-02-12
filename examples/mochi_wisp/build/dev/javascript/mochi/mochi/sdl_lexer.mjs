import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";

export class LeftBrace extends $CustomType {}
export const SDLToken$LeftBrace = () => new LeftBrace();
export const SDLToken$isLeftBrace = (value) => value instanceof LeftBrace;

export class RightBrace extends $CustomType {}
export const SDLToken$RightBrace = () => new RightBrace();
export const SDLToken$isRightBrace = (value) => value instanceof RightBrace;

export class LeftParen extends $CustomType {}
export const SDLToken$LeftParen = () => new LeftParen();
export const SDLToken$isLeftParen = (value) => value instanceof LeftParen;

export class RightParen extends $CustomType {}
export const SDLToken$RightParen = () => new RightParen();
export const SDLToken$isRightParen = (value) => value instanceof RightParen;

export class LeftBracket extends $CustomType {}
export const SDLToken$LeftBracket = () => new LeftBracket();
export const SDLToken$isLeftBracket = (value) => value instanceof LeftBracket;

export class RightBracket extends $CustomType {}
export const SDLToken$RightBracket = () => new RightBracket();
export const SDLToken$isRightBracket = (value) => value instanceof RightBracket;

export class Colon extends $CustomType {}
export const SDLToken$Colon = () => new Colon();
export const SDLToken$isColon = (value) => value instanceof Colon;

export class Bang extends $CustomType {}
export const SDLToken$Bang = () => new Bang();
export const SDLToken$isBang = (value) => value instanceof Bang;

export class Equals extends $CustomType {}
export const SDLToken$Equals = () => new Equals();
export const SDLToken$isEquals = (value) => value instanceof Equals;

export class At extends $CustomType {}
export const SDLToken$At = () => new At();
export const SDLToken$isAt = (value) => value instanceof At;

export class Pipe extends $CustomType {}
export const SDLToken$Pipe = () => new Pipe();
export const SDLToken$isPipe = (value) => value instanceof Pipe;

export class Amp extends $CustomType {}
export const SDLToken$Amp = () => new Amp();
export const SDLToken$isAmp = (value) => value instanceof Amp;

export class Type extends $CustomType {}
export const SDLToken$Type = () => new Type();
export const SDLToken$isType = (value) => value instanceof Type;

export class Interface extends $CustomType {}
export const SDLToken$Interface = () => new Interface();
export const SDLToken$isInterface = (value) => value instanceof Interface;

export class Union extends $CustomType {}
export const SDLToken$Union = () => new Union();
export const SDLToken$isUnion = (value) => value instanceof Union;

export class Scalar extends $CustomType {}
export const SDLToken$Scalar = () => new Scalar();
export const SDLToken$isScalar = (value) => value instanceof Scalar;

export class Enum extends $CustomType {}
export const SDLToken$Enum = () => new Enum();
export const SDLToken$isEnum = (value) => value instanceof Enum;

export class Input extends $CustomType {}
export const SDLToken$Input = () => new Input();
export const SDLToken$isInput = (value) => value instanceof Input;

export class Directive extends $CustomType {}
export const SDLToken$Directive = () => new Directive();
export const SDLToken$isDirective = (value) => value instanceof Directive;

export class Schema extends $CustomType {}
export const SDLToken$Schema = () => new Schema();
export const SDLToken$isSchema = (value) => value instanceof Schema;

export class Extend extends $CustomType {}
export const SDLToken$Extend = () => new Extend();
export const SDLToken$isExtend = (value) => value instanceof Extend;

export class Implements extends $CustomType {}
export const SDLToken$Implements = () => new Implements();
export const SDLToken$isImplements = (value) => value instanceof Implements;

export class Name extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLToken$Name = (value) => new Name(value);
export const SDLToken$isName = (value) => value instanceof Name;
export const SDLToken$Name$value = (value) => value.value;
export const SDLToken$Name$0 = (value) => value.value;

export class IntValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLToken$IntValue = (value) => new IntValue(value);
export const SDLToken$isIntValue = (value) => value instanceof IntValue;
export const SDLToken$IntValue$value = (value) => value.value;
export const SDLToken$IntValue$0 = (value) => value.value;

export class FloatValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLToken$FloatValue = (value) => new FloatValue(value);
export const SDLToken$isFloatValue = (value) => value instanceof FloatValue;
export const SDLToken$FloatValue$value = (value) => value.value;
export const SDLToken$FloatValue$0 = (value) => value.value;

export class StringValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLToken$StringValue = (value) => new StringValue(value);
export const SDLToken$isStringValue = (value) => value instanceof StringValue;
export const SDLToken$StringValue$value = (value) => value.value;
export const SDLToken$StringValue$0 = (value) => value.value;

export class BooleanValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLToken$BooleanValue = (value) => new BooleanValue(value);
export const SDLToken$isBooleanValue = (value) => value instanceof BooleanValue;
export const SDLToken$BooleanValue$value = (value) => value.value;
export const SDLToken$BooleanValue$0 = (value) => value.value;

export class EOF extends $CustomType {}
export const SDLToken$EOF = () => new EOF();
export const SDLToken$isEOF = (value) => value instanceof EOF;

export class Comment extends $CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
}
export const SDLToken$Comment = (content) => new Comment(content);
export const SDLToken$isComment = (value) => value instanceof Comment;
export const SDLToken$Comment$content = (value) => value.content;
export const SDLToken$Comment$0 = (value) => value.content;

export class Description extends $CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
}
export const SDLToken$Description = (content) => new Description(content);
export const SDLToken$isDescription = (value) => value instanceof Description;
export const SDLToken$Description$content = (value) => value.content;
export const SDLToken$Description$0 = (value) => value.content;

export class Position extends $CustomType {
  constructor(line, column) {
    super();
    this.line = line;
    this.column = column;
  }
}
export const Position$Position = (line, column) => new Position(line, column);
export const Position$isPosition = (value) => value instanceof Position;
export const Position$Position$line = (value) => value.line;
export const Position$Position$0 = (value) => value.line;
export const Position$Position$column = (value) => value.column;
export const Position$Position$1 = (value) => value.column;

export class SDLTokenWithPosition extends $CustomType {
  constructor(token, position) {
    super();
    this.token = token;
    this.position = position;
  }
}
export const SDLTokenWithPosition$SDLTokenWithPosition = (token, position) =>
  new SDLTokenWithPosition(token, position);
export const SDLTokenWithPosition$isSDLTokenWithPosition = (value) =>
  value instanceof SDLTokenWithPosition;
export const SDLTokenWithPosition$SDLTokenWithPosition$token = (value) =>
  value.token;
export const SDLTokenWithPosition$SDLTokenWithPosition$0 = (value) =>
  value.token;
export const SDLTokenWithPosition$SDLTokenWithPosition$position = (value) =>
  value.position;
export const SDLTokenWithPosition$SDLTokenWithPosition$1 = (value) =>
  value.position;

export class UnexpectedCharacter extends $CustomType {
  constructor(character, position) {
    super();
    this.character = character;
    this.position = position;
  }
}
export const SDLLexerError$UnexpectedCharacter = (character, position) =>
  new UnexpectedCharacter(character, position);
export const SDLLexerError$isUnexpectedCharacter = (value) =>
  value instanceof UnexpectedCharacter;
export const SDLLexerError$UnexpectedCharacter$character = (value) =>
  value.character;
export const SDLLexerError$UnexpectedCharacter$0 = (value) => value.character;
export const SDLLexerError$UnexpectedCharacter$position = (value) =>
  value.position;
export const SDLLexerError$UnexpectedCharacter$1 = (value) => value.position;

export class InvalidNumber extends $CustomType {
  constructor(value, position) {
    super();
    this.value = value;
    this.position = position;
  }
}
export const SDLLexerError$InvalidNumber = (value, position) =>
  new InvalidNumber(value, position);
export const SDLLexerError$isInvalidNumber = (value) =>
  value instanceof InvalidNumber;
export const SDLLexerError$InvalidNumber$value = (value) => value.value;
export const SDLLexerError$InvalidNumber$0 = (value) => value.value;
export const SDLLexerError$InvalidNumber$position = (value) => value.position;
export const SDLLexerError$InvalidNumber$1 = (value) => value.position;

export class UnterminatedString extends $CustomType {
  constructor(position) {
    super();
    this.position = position;
  }
}
export const SDLLexerError$UnterminatedString = (position) =>
  new UnterminatedString(position);
export const SDLLexerError$isUnterminatedString = (value) =>
  value instanceof UnterminatedString;
export const SDLLexerError$UnterminatedString$position = (value) =>
  value.position;
export const SDLLexerError$UnterminatedString$0 = (value) => value.position;

export class UnterminatedDescription extends $CustomType {
  constructor(position) {
    super();
    this.position = position;
  }
}
export const SDLLexerError$UnterminatedDescription = (position) =>
  new UnterminatedDescription(position);
export const SDLLexerError$isUnterminatedDescription = (value) =>
  value instanceof UnterminatedDescription;
export const SDLLexerError$UnterminatedDescription$position = (value) =>
  value.position;
export const SDLLexerError$UnterminatedDescription$0 = (value) =>
  value.position;

export class SDLLexerState extends $CustomType {
  constructor(input, position, line, column) {
    super();
    this.input = input;
    this.position = position;
    this.line = line;
    this.column = column;
  }
}
export const SDLLexerState$SDLLexerState = (input, position, line, column) =>
  new SDLLexerState(input, position, line, column);
export const SDLLexerState$isSDLLexerState = (value) =>
  value instanceof SDLLexerState;
export const SDLLexerState$SDLLexerState$input = (value) => value.input;
export const SDLLexerState$SDLLexerState$0 = (value) => value.input;
export const SDLLexerState$SDLLexerState$position = (value) => value.position;
export const SDLLexerState$SDLLexerState$1 = (value) => value.position;
export const SDLLexerState$SDLLexerState$line = (value) => value.line;
export const SDLLexerState$SDLLexerState$2 = (value) => value.line;
export const SDLLexerState$SDLLexerState$column = (value) => value.column;
export const SDLLexerState$SDLLexerState$3 = (value) => value.column;

export function new_sdl_lexer(input) {
  return new SDLLexerState(input, 0, 1, 1);
}

function peek_char(lexer) {
  let $ = $string.slice(lexer.input, lexer.position, 1);
  if ($ === "") {
    return new Error(undefined);
  } else {
    let char = $;
    return new Ok(char);
  }
}

function peek_string(lexer) {
  return $string.slice(lexer.input, lexer.position, 3);
}

function peek_string_n(lexer, n) {
  return $string.slice(lexer.input, lexer.position, n);
}

function advance_char(lexer) {
  let $ = peek_char(lexer);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 === "\n") {
      return new SDLLexerState(
        lexer.input,
        lexer.position + 1,
        lexer.line + 1,
        1,
      );
    } else {
      return new SDLLexerState(
        lexer.input,
        lexer.position + 1,
        lexer.line,
        lexer.column + 1,
      );
    }
  } else {
    return new SDLLexerState(
      lexer.input,
      lexer.position + 1,
      lexer.line,
      lexer.column + 1,
    );
  }
}

function skip_whitespace_and_comments(loop$lexer) {
  while (true) {
    let lexer = loop$lexer;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 === " ") {
        loop$lexer = advance_char(lexer);
      } else if ($1 === "\t") {
        loop$lexer = advance_char(lexer);
      } else if ($1 === "\n") {
        loop$lexer = advance_char(lexer);
      } else if ($1 === "\r") {
        loop$lexer = advance_char(lexer);
      } else if ($1 === ",") {
        loop$lexer = advance_char(lexer);
      } else {
        return lexer;
      }
    } else {
      return lexer;
    }
  }
}

function is_letter(char) {
  if (char === "a") {
    return true;
  } else if (char === "b") {
    return true;
  } else if (char === "c") {
    return true;
  } else if (char === "d") {
    return true;
  } else if (char === "e") {
    return true;
  } else if (char === "f") {
    return true;
  } else if (char === "g") {
    return true;
  } else if (char === "h") {
    return true;
  } else if (char === "i") {
    return true;
  } else if (char === "j") {
    return true;
  } else if (char === "k") {
    return true;
  } else if (char === "l") {
    return true;
  } else if (char === "m") {
    return true;
  } else if (char === "n") {
    return true;
  } else if (char === "o") {
    return true;
  } else if (char === "p") {
    return true;
  } else if (char === "q") {
    return true;
  } else if (char === "r") {
    return true;
  } else if (char === "s") {
    return true;
  } else if (char === "t") {
    return true;
  } else if (char === "u") {
    return true;
  } else if (char === "v") {
    return true;
  } else if (char === "w") {
    return true;
  } else if (char === "x") {
    return true;
  } else if (char === "y") {
    return true;
  } else if (char === "z") {
    return true;
  } else if (char === "A") {
    return true;
  } else if (char === "B") {
    return true;
  } else if (char === "C") {
    return true;
  } else if (char === "D") {
    return true;
  } else if (char === "E") {
    return true;
  } else if (char === "F") {
    return true;
  } else if (char === "G") {
    return true;
  } else if (char === "H") {
    return true;
  } else if (char === "I") {
    return true;
  } else if (char === "J") {
    return true;
  } else if (char === "K") {
    return true;
  } else if (char === "L") {
    return true;
  } else if (char === "M") {
    return true;
  } else if (char === "N") {
    return true;
  } else if (char === "O") {
    return true;
  } else if (char === "P") {
    return true;
  } else if (char === "Q") {
    return true;
  } else if (char === "R") {
    return true;
  } else if (char === "S") {
    return true;
  } else if (char === "T") {
    return true;
  } else if (char === "U") {
    return true;
  } else if (char === "V") {
    return true;
  } else if (char === "W") {
    return true;
  } else if (char === "X") {
    return true;
  } else if (char === "Y") {
    return true;
  } else if (char === "Z") {
    return true;
  } else {
    return false;
  }
}

function is_digit(char) {
  if (char === "0") {
    return true;
  } else if (char === "1") {
    return true;
  } else if (char === "2") {
    return true;
  } else if (char === "3") {
    return true;
  } else if (char === "4") {
    return true;
  } else if (char === "5") {
    return true;
  } else if (char === "6") {
    return true;
  } else if (char === "7") {
    return true;
  } else if (char === "8") {
    return true;
  } else if (char === "9") {
    return true;
  } else {
    return false;
  }
}

function is_name_continue(char) {
  return (is_letter(char) || is_digit(char)) || (char === "_");
}

function read_name_chars(loop$lexer, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let char = $[0];
      let $1 = is_name_continue(char);
      if ($1) {
        loop$lexer = advance_char(lexer);
        loop$acc = acc + char;
      } else {
        return [acc, lexer];
      }
    } else {
      return [acc, lexer];
    }
  }
}

function read_name_or_keyword(lexer, position) {
  let $ = read_name_chars(lexer, "");
  let name;
  let new_lexer;
  name = $[0];
  new_lexer = $[1];
  let _block;
  if (name === "type") {
    _block = new Type();
  } else if (name === "interface") {
    _block = new Interface();
  } else if (name === "union") {
    _block = new Union();
  } else if (name === "scalar") {
    _block = new Scalar();
  } else if (name === "enum") {
    _block = new Enum();
  } else if (name === "input") {
    _block = new Input();
  } else if (name === "directive") {
    _block = new Directive();
  } else if (name === "schema") {
    _block = new Schema();
  } else if (name === "extend") {
    _block = new Extend();
  } else if (name === "implements") {
    _block = new Implements();
  } else if (name === "true") {
    _block = new BooleanValue(true);
  } else if (name === "false") {
    _block = new BooleanValue(false);
  } else {
    _block = new Name(name);
  }
  let token = _block;
  return new Ok([new SDLTokenWithPosition(token, position), new_lexer]);
}

function read_string_chars(loop$lexer, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 === "\"") {
        return new Ok([acc, lexer]);
      } else if ($1 === "\n") {
        return new Error(undefined);
      } else {
        let char = $1;
        loop$lexer = advance_char(lexer);
        loop$acc = acc + char;
      }
    } else {
      return new Error(undefined);
    }
  }
}

function read_string(lexer, position) {
  let lexer$1 = advance_char(lexer);
  let $ = read_string_chars(lexer$1, "");
  if ($ instanceof Ok) {
    let value = $[0][0];
    let new_lexer = $[0][1];
    let $1 = peek_char(new_lexer);
    if ($1 instanceof Ok) {
      let $2 = $1[0];
      if ($2 === "\"") {
        let final_lexer = advance_char(new_lexer);
        return new Ok(
          [
            new SDLTokenWithPosition(new StringValue(value), position),
            final_lexer,
          ],
        );
      } else {
        return new Error(new UnterminatedString(position));
      }
    } else {
      return new Error(new UnterminatedString(position));
    }
  } else {
    return new Error(new UnterminatedString(position));
  }
}

function read_description_chars(loop$lexer, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let acc = loop$acc;
    let $ = peek_string_n(lexer, 3);
    if ($ === "\"\"\"") {
      return new Ok([acc, lexer]);
    } else {
      let $1 = peek_char(lexer);
      if ($1 instanceof Ok) {
        let char = $1[0];
        loop$lexer = advance_char(lexer);
        loop$acc = acc + char;
      } else {
        return new Error(undefined);
      }
    }
  }
}

function read_description(lexer, position) {
  let lexer$1 = advance_char(advance_char(advance_char(lexer)));
  let $ = read_description_chars(lexer$1, "");
  if ($ instanceof Ok) {
    let content = $[0][0];
    let new_lexer = $[0][1];
    let $1 = peek_string_n(new_lexer, 3);
    if ($1 === "\"\"\"") {
      let final_lexer = advance_char(advance_char(advance_char(new_lexer)));
      return new Ok(
        [
          new SDLTokenWithPosition(new Description(content), position),
          final_lexer,
        ],
      );
    } else {
      return new Error(new UnterminatedDescription(position));
    }
  } else {
    return new Error(new UnterminatedDescription(position));
  }
}

function read_comment_chars(loop$lexer, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 === "\n") {
        return [acc, lexer];
      } else {
        let char = $1;
        loop$lexer = advance_char(lexer);
        loop$acc = acc + char;
      }
    } else {
      return [acc, lexer];
    }
  }
}

function read_comment(lexer, position) {
  let lexer$1 = advance_char(lexer);
  let $ = read_comment_chars(lexer$1, "");
  let content;
  let new_lexer;
  content = $[0];
  new_lexer = $[1];
  return new Ok(
    [new SDLTokenWithPosition(new Comment(content), position), new_lexer],
  );
}

function read_number_chars(loop$lexer, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let char = $[0];
      let $1 = (is_digit(char) || (char === ".")) || (char === "-");
      if ($1) {
        loop$lexer = advance_char(lexer);
        loop$acc = acc + char;
      } else {
        return [acc, lexer];
      }
    } else {
      return [acc, lexer];
    }
  }
}

function read_number(lexer, position) {
  let $ = read_number_chars(lexer, "");
  let number_str;
  let new_lexer;
  number_str = $[0];
  new_lexer = $[1];
  let $1 = $string.contains(number_str, ".");
  if ($1) {
    let $2 = $float.parse(number_str);
    if ($2 instanceof Ok) {
      let value = $2[0];
      return new Ok(
        [new SDLTokenWithPosition(new FloatValue(value), position), new_lexer],
      );
    } else {
      return new Error(new InvalidNumber(number_str, position));
    }
  } else {
    let $2 = $int.parse(number_str);
    if ($2 instanceof Ok) {
      let value = $2[0];
      return new Ok(
        [new SDLTokenWithPosition(new IntValue(value), position), new_lexer],
      );
    } else {
      return new Error(new InvalidNumber(number_str, position));
    }
  }
}

export function next_sdl_token(lexer) {
  let lexer$1 = skip_whitespace_and_comments(lexer);
  let $ = peek_char(lexer$1);
  if ($ instanceof Ok) {
    let char = $[0];
    let position = new Position(lexer$1.line, lexer$1.column);
    if (char === "{") {
      return new Ok(
        [
          new SDLTokenWithPosition(new LeftBrace(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "}") {
      return new Ok(
        [
          new SDLTokenWithPosition(new RightBrace(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "(") {
      return new Ok(
        [
          new SDLTokenWithPosition(new LeftParen(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === ")") {
      return new Ok(
        [
          new SDLTokenWithPosition(new RightParen(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "[") {
      return new Ok(
        [
          new SDLTokenWithPosition(new LeftBracket(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "]") {
      return new Ok(
        [
          new SDLTokenWithPosition(new RightBracket(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === ":") {
      return new Ok(
        [new SDLTokenWithPosition(new Colon(), position), advance_char(lexer$1)],
      );
    } else if (char === "!") {
      return new Ok(
        [new SDLTokenWithPosition(new Bang(), position), advance_char(lexer$1)],
      );
    } else if (char === "=") {
      return new Ok(
        [
          new SDLTokenWithPosition(new Equals(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "@") {
      return new Ok(
        [new SDLTokenWithPosition(new At(), position), advance_char(lexer$1)],
      );
    } else if (char === "|") {
      return new Ok(
        [new SDLTokenWithPosition(new Pipe(), position), advance_char(lexer$1)],
      );
    } else if (char === "&") {
      return new Ok(
        [new SDLTokenWithPosition(new Amp(), position), advance_char(lexer$1)],
      );
    } else if (char === "\"") {
      let $1 = peek_string(lexer$1);
      if ($1 === "\"\"\"") {
        return read_description(lexer$1, position);
      } else {
        return read_string(lexer$1, position);
      }
    } else if (char === "#") {
      return read_comment(lexer$1, position);
    } else if (char === "0") {
      return read_number(lexer$1, position);
    } else if (char === "1") {
      return read_number(lexer$1, position);
    } else if (char === "2") {
      return read_number(lexer$1, position);
    } else if (char === "3") {
      return read_number(lexer$1, position);
    } else if (char === "4") {
      return read_number(lexer$1, position);
    } else if (char === "5") {
      return read_number(lexer$1, position);
    } else if (char === "6") {
      return read_number(lexer$1, position);
    } else if (char === "7") {
      return read_number(lexer$1, position);
    } else if (char === "8") {
      return read_number(lexer$1, position);
    } else if (char === "9") {
      return read_number(lexer$1, position);
    } else if (char === "-") {
      return read_number(lexer$1, position);
    } else if (char === "a") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "b") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "c") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "d") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "e") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "f") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "g") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "h") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "i") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "j") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "k") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "l") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "m") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "n") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "o") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "p") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "q") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "r") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "s") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "t") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "u") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "v") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "w") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "x") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "y") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "z") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "A") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "B") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "C") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "D") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "E") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "F") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "G") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "H") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "I") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "J") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "K") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "L") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "M") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "N") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "O") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "P") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "Q") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "R") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "S") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "T") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "U") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "V") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "W") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "X") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "Y") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "Z") {
      return read_name_or_keyword(lexer$1, position);
    } else if (char === "_") {
      return read_name_or_keyword(lexer$1, position);
    } else {
      return new Error(new UnexpectedCharacter(char, position));
    }
  } else {
    return new Ok(
      [
        new SDLTokenWithPosition(
          new EOF(),
          new Position(lexer$1.line, lexer$1.column),
        ),
        lexer$1,
      ],
    );
  }
}

function tokenize_loop(loop$lexer, loop$tokens) {
  while (true) {
    let lexer = loop$lexer;
    let tokens = loop$tokens;
    let $ = next_sdl_token(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0][0].token;
      if ($1 instanceof EOF) {
        let _pipe = $list.reverse(
          listPrepend(
            new SDLTokenWithPosition(
              new EOF(),
              new Position(lexer.line, lexer.column),
            ),
            tokens,
          ),
        );
        return new Ok(_pipe);
      } else {
        let token = $[0][0];
        let new_lexer = $[0][1];
        loop$lexer = new_lexer;
        loop$tokens = listPrepend(token, tokens);
      }
    } else {
      return $;
    }
  }
}

export function tokenize_sdl(input) {
  let lexer = new_sdl_lexer(input);
  return tokenize_loop(lexer, toList([]));
}
