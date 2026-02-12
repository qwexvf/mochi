import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";

export class Bang extends $CustomType {}
export const Token$Bang = () => new Bang();
export const Token$isBang = (value) => value instanceof Bang;

export class Dollar extends $CustomType {}
export const Token$Dollar = () => new Dollar();
export const Token$isDollar = (value) => value instanceof Dollar;

export class Amp extends $CustomType {}
export const Token$Amp = () => new Amp();
export const Token$isAmp = (value) => value instanceof Amp;

export class LeftParen extends $CustomType {}
export const Token$LeftParen = () => new LeftParen();
export const Token$isLeftParen = (value) => value instanceof LeftParen;

export class RightParen extends $CustomType {}
export const Token$RightParen = () => new RightParen();
export const Token$isRightParen = (value) => value instanceof RightParen;

export class Spread extends $CustomType {}
export const Token$Spread = () => new Spread();
export const Token$isSpread = (value) => value instanceof Spread;

export class Colon extends $CustomType {}
export const Token$Colon = () => new Colon();
export const Token$isColon = (value) => value instanceof Colon;

export class Equals extends $CustomType {}
export const Token$Equals = () => new Equals();
export const Token$isEquals = (value) => value instanceof Equals;

export class At extends $CustomType {}
export const Token$At = () => new At();
export const Token$isAt = (value) => value instanceof At;

export class LeftBracket extends $CustomType {}
export const Token$LeftBracket = () => new LeftBracket();
export const Token$isLeftBracket = (value) => value instanceof LeftBracket;

export class RightBracket extends $CustomType {}
export const Token$RightBracket = () => new RightBracket();
export const Token$isRightBracket = (value) => value instanceof RightBracket;

export class LeftBrace extends $CustomType {}
export const Token$LeftBrace = () => new LeftBrace();
export const Token$isLeftBrace = (value) => value instanceof LeftBrace;

export class RightBrace extends $CustomType {}
export const Token$RightBrace = () => new RightBrace();
export const Token$isRightBrace = (value) => value instanceof RightBrace;

export class Pipe extends $CustomType {}
export const Token$Pipe = () => new Pipe();
export const Token$isPipe = (value) => value instanceof Pipe;

export class Query extends $CustomType {}
export const Token$Query = () => new Query();
export const Token$isQuery = (value) => value instanceof Query;

export class Mutation extends $CustomType {}
export const Token$Mutation = () => new Mutation();
export const Token$isMutation = (value) => value instanceof Mutation;

export class Subscription extends $CustomType {}
export const Token$Subscription = () => new Subscription();
export const Token$isSubscription = (value) => value instanceof Subscription;

export class Fragment extends $CustomType {}
export const Token$Fragment = () => new Fragment();
export const Token$isFragment = (value) => value instanceof Fragment;

export class On extends $CustomType {}
export const Token$On = () => new On();
export const Token$isOn = (value) => value instanceof On;

export class TrueKeyword extends $CustomType {}
export const Token$TrueKeyword = () => new TrueKeyword();
export const Token$isTrueKeyword = (value) => value instanceof TrueKeyword;

export class FalseKeyword extends $CustomType {}
export const Token$FalseKeyword = () => new FalseKeyword();
export const Token$isFalseKeyword = (value) => value instanceof FalseKeyword;

export class NullKeyword extends $CustomType {}
export const Token$NullKeyword = () => new NullKeyword();
export const Token$isNullKeyword = (value) => value instanceof NullKeyword;

export class Name extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Token$Name = (value) => new Name(value);
export const Token$isName = (value) => value instanceof Name;
export const Token$Name$value = (value) => value.value;
export const Token$Name$0 = (value) => value.value;

export class IntValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Token$IntValue = (value) => new IntValue(value);
export const Token$isIntValue = (value) => value instanceof IntValue;
export const Token$IntValue$value = (value) => value.value;
export const Token$IntValue$0 = (value) => value.value;

export class FloatValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Token$FloatValue = (value) => new FloatValue(value);
export const Token$isFloatValue = (value) => value instanceof FloatValue;
export const Token$FloatValue$value = (value) => value.value;
export const Token$FloatValue$0 = (value) => value.value;

export class StringValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Token$StringValue = (value) => new StringValue(value);
export const Token$isStringValue = (value) => value instanceof StringValue;
export const Token$StringValue$value = (value) => value.value;
export const Token$StringValue$0 = (value) => value.value;

export class EOF extends $CustomType {}
export const Token$EOF = () => new EOF();
export const Token$isEOF = (value) => value instanceof EOF;

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

export class TokenWithPosition extends $CustomType {
  constructor(token, position) {
    super();
    this.token = token;
    this.position = position;
  }
}
export const TokenWithPosition$TokenWithPosition = (token, position) =>
  new TokenWithPosition(token, position);
export const TokenWithPosition$isTokenWithPosition = (value) =>
  value instanceof TokenWithPosition;
export const TokenWithPosition$TokenWithPosition$token = (value) => value.token;
export const TokenWithPosition$TokenWithPosition$0 = (value) => value.token;
export const TokenWithPosition$TokenWithPosition$position = (value) =>
  value.position;
export const TokenWithPosition$TokenWithPosition$1 = (value) => value.position;

export class UnexpectedCharacter extends $CustomType {
  constructor(character, position) {
    super();
    this.character = character;
    this.position = position;
  }
}
export const LexerError$UnexpectedCharacter = (character, position) =>
  new UnexpectedCharacter(character, position);
export const LexerError$isUnexpectedCharacter = (value) =>
  value instanceof UnexpectedCharacter;
export const LexerError$UnexpectedCharacter$character = (value) =>
  value.character;
export const LexerError$UnexpectedCharacter$0 = (value) => value.character;
export const LexerError$UnexpectedCharacter$position = (value) =>
  value.position;
export const LexerError$UnexpectedCharacter$1 = (value) => value.position;

export class InvalidNumber extends $CustomType {
  constructor(value, position) {
    super();
    this.value = value;
    this.position = position;
  }
}
export const LexerError$InvalidNumber = (value, position) =>
  new InvalidNumber(value, position);
export const LexerError$isInvalidNumber = (value) =>
  value instanceof InvalidNumber;
export const LexerError$InvalidNumber$value = (value) => value.value;
export const LexerError$InvalidNumber$0 = (value) => value.value;
export const LexerError$InvalidNumber$position = (value) => value.position;
export const LexerError$InvalidNumber$1 = (value) => value.position;

export class UnterminatedString extends $CustomType {
  constructor(position) {
    super();
    this.position = position;
  }
}
export const LexerError$UnterminatedString = (position) =>
  new UnterminatedString(position);
export const LexerError$isUnterminatedString = (value) =>
  value instanceof UnterminatedString;
export const LexerError$UnterminatedString$position = (value) => value.position;
export const LexerError$UnterminatedString$0 = (value) => value.position;

export class LexerState extends $CustomType {
  constructor(input, position, line, column) {
    super();
    this.input = input;
    this.position = position;
    this.line = line;
    this.column = column;
  }
}
export const LexerState$LexerState = (input, position, line, column) =>
  new LexerState(input, position, line, column);
export const LexerState$isLexerState = (value) => value instanceof LexerState;
export const LexerState$LexerState$input = (value) => value.input;
export const LexerState$LexerState$0 = (value) => value.input;
export const LexerState$LexerState$position = (value) => value.position;
export const LexerState$LexerState$1 = (value) => value.position;
export const LexerState$LexerState$line = (value) => value.line;
export const LexerState$LexerState$2 = (value) => value.line;
export const LexerState$LexerState$column = (value) => value.column;
export const LexerState$LexerState$3 = (value) => value.column;

export function new_lexer(input) {
  return new LexerState(input, 0, 1, 1);
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

function advance_char(lexer) {
  let $ = peek_char(lexer);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 === "\n") {
      return new LexerState(lexer.input, lexer.position + 1, lexer.line + 1, 1);
    } else {
      return new LexerState(
        lexer.input,
        lexer.position + 1,
        lexer.line,
        lexer.column + 1,
      );
    }
  } else {
    return lexer;
  }
}

function skip_whitespace(loop$lexer) {
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

function read_spread(lexer, position) {
  let lexer1 = advance_char(lexer);
  let lexer2 = advance_char(lexer1);
  let lexer3 = advance_char(lexer2);
  let $ = $string.slice(lexer.input, lexer.position, 3);
  if ($ === "...") {
    return new Ok([new TokenWithPosition(new Spread(), position), lexer3]);
  } else {
    return new Error(new UnexpectedCharacter(".", position));
  }
}

function read_string_loop(loop$lexer, loop$position, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let position = loop$position;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 === "\"") {
        return new Ok(
          [
            new TokenWithPosition(new StringValue(acc), position),
            advance_char(lexer),
          ],
        );
      } else if ($1 === "\\") {
        let lexer$1 = advance_char(lexer);
        let $2 = peek_char(lexer$1);
        if ($2 instanceof Ok) {
          let $3 = $2[0];
          if ($3 === "\"") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\"";
          } else if ($3 === "\\") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\\";
          } else if ($3 === "/") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "/";
          } else if ($3 === "b") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\u{0008}";
          } else if ($3 === "f") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\u{000C}";
          } else if ($3 === "n") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\n";
          } else if ($3 === "r") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\r";
          } else if ($3 === "t") {
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + "\t";
          } else {
            let char = $3;
            loop$lexer = advance_char(lexer$1);
            loop$position = position;
            loop$acc = acc + char;
          }
        } else {
          return new Error(new UnterminatedString(position));
        }
      } else {
        let char = $1;
        loop$lexer = advance_char(lexer);
        loop$position = position;
        loop$acc = acc + char;
      }
    } else {
      return new Error(new UnterminatedString(position));
    }
  }
}

function read_string(lexer, position) {
  return read_string_loop(advance_char(lexer), position, "");
}

function read_while_loop(loop$lexer, loop$predicate, loop$acc) {
  while (true) {
    let lexer = loop$lexer;
    let predicate = loop$predicate;
    let acc = loop$acc;
    let $ = peek_char(lexer);
    if ($ instanceof Ok) {
      let char = $[0];
      let $1 = predicate(char);
      if ($1) {
        loop$lexer = advance_char(lexer);
        loop$predicate = predicate;
        loop$acc = acc + char;
      } else {
        return [acc, lexer];
      }
    } else {
      return [acc, lexer];
    }
  }
}

function read_while(lexer, predicate) {
  return read_while_loop(lexer, predicate, "");
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

function is_digit_or_dot(c) {
  return is_digit(c) || (c === ".");
}

function read_number(lexer, position) {
  let $ = read_while(lexer, is_digit_or_dot);
  let number_str;
  let new_lexer$1;
  number_str = $[0];
  new_lexer$1 = $[1];
  let $1 = $string.contains(number_str, ".");
  if ($1) {
    let $2 = $float.parse(number_str);
    if ($2 instanceof Ok) {
      let value = $2[0];
      return new Ok(
        [new TokenWithPosition(new FloatValue(value), position), new_lexer$1],
      );
    } else {
      return new Error(new InvalidNumber(number_str, position));
    }
  } else {
    let $2 = $int.parse(number_str);
    if ($2 instanceof Ok) {
      let value = $2[0];
      return new Ok(
        [new TokenWithPosition(new IntValue(value), position), new_lexer$1],
      );
    } else {
      return new Error(new InvalidNumber(number_str, position));
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

function is_name_continue(char) {
  return (is_letter(char) || is_digit(char)) || (char === "_");
}

function read_name(lexer, position) {
  let $ = read_while(lexer, is_name_continue);
  let name;
  let new_lexer$1;
  name = $[0];
  new_lexer$1 = $[1];
  let _block;
  if (name === "query") {
    _block = new Query();
  } else if (name === "mutation") {
    _block = new Mutation();
  } else if (name === "subscription") {
    _block = new Subscription();
  } else if (name === "fragment") {
    _block = new Fragment();
  } else if (name === "on") {
    _block = new On();
  } else if (name === "true") {
    _block = new TrueKeyword();
  } else if (name === "false") {
    _block = new FalseKeyword();
  } else if (name === "null") {
    _block = new NullKeyword();
  } else {
    _block = new Name(name);
  }
  let token = _block;
  return new Ok([new TokenWithPosition(token, position), new_lexer$1]);
}

export function next_token(lexer) {
  let lexer$1 = skip_whitespace(lexer);
  let $ = peek_char(lexer$1);
  if ($ instanceof Ok) {
    let char = $[0];
    let position = new Position(lexer$1.line, lexer$1.column);
    if (char === "!") {
      return new Ok(
        [new TokenWithPosition(new Bang(), position), advance_char(lexer$1)],
      );
    } else if (char === "$") {
      return new Ok(
        [new TokenWithPosition(new Dollar(), position), advance_char(lexer$1)],
      );
    } else if (char === "&") {
      return new Ok(
        [new TokenWithPosition(new Amp(), position), advance_char(lexer$1)],
      );
    } else if (char === "(") {
      return new Ok(
        [
          new TokenWithPosition(new LeftParen(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === ")") {
      return new Ok(
        [
          new TokenWithPosition(new RightParen(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === ":") {
      return new Ok(
        [new TokenWithPosition(new Colon(), position), advance_char(lexer$1)],
      );
    } else if (char === "=") {
      return new Ok(
        [new TokenWithPosition(new Equals(), position), advance_char(lexer$1)],
      );
    } else if (char === "@") {
      return new Ok(
        [new TokenWithPosition(new At(), position), advance_char(lexer$1)],
      );
    } else if (char === "[") {
      return new Ok(
        [
          new TokenWithPosition(new LeftBracket(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "]") {
      return new Ok(
        [
          new TokenWithPosition(new RightBracket(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "{") {
      return new Ok(
        [
          new TokenWithPosition(new LeftBrace(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "}") {
      return new Ok(
        [
          new TokenWithPosition(new RightBrace(), position),
          advance_char(lexer$1),
        ],
      );
    } else if (char === "|") {
      return new Ok(
        [new TokenWithPosition(new Pipe(), position), advance_char(lexer$1)],
      );
    } else if (char === ".") {
      return read_spread(lexer$1, position);
    } else if (char === "\"") {
      return read_string(lexer$1, position);
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
    } else if (char === "a") {
      return read_name(lexer$1, position);
    } else if (char === "b") {
      return read_name(lexer$1, position);
    } else if (char === "c") {
      return read_name(lexer$1, position);
    } else if (char === "d") {
      return read_name(lexer$1, position);
    } else if (char === "e") {
      return read_name(lexer$1, position);
    } else if (char === "f") {
      return read_name(lexer$1, position);
    } else if (char === "g") {
      return read_name(lexer$1, position);
    } else if (char === "h") {
      return read_name(lexer$1, position);
    } else if (char === "i") {
      return read_name(lexer$1, position);
    } else if (char === "j") {
      return read_name(lexer$1, position);
    } else if (char === "k") {
      return read_name(lexer$1, position);
    } else if (char === "l") {
      return read_name(lexer$1, position);
    } else if (char === "m") {
      return read_name(lexer$1, position);
    } else if (char === "n") {
      return read_name(lexer$1, position);
    } else if (char === "o") {
      return read_name(lexer$1, position);
    } else if (char === "p") {
      return read_name(lexer$1, position);
    } else if (char === "q") {
      return read_name(lexer$1, position);
    } else if (char === "r") {
      return read_name(lexer$1, position);
    } else if (char === "s") {
      return read_name(lexer$1, position);
    } else if (char === "t") {
      return read_name(lexer$1, position);
    } else if (char === "u") {
      return read_name(lexer$1, position);
    } else if (char === "v") {
      return read_name(lexer$1, position);
    } else if (char === "w") {
      return read_name(lexer$1, position);
    } else if (char === "x") {
      return read_name(lexer$1, position);
    } else if (char === "y") {
      return read_name(lexer$1, position);
    } else if (char === "z") {
      return read_name(lexer$1, position);
    } else if (char === "A") {
      return read_name(lexer$1, position);
    } else if (char === "B") {
      return read_name(lexer$1, position);
    } else if (char === "C") {
      return read_name(lexer$1, position);
    } else if (char === "D") {
      return read_name(lexer$1, position);
    } else if (char === "E") {
      return read_name(lexer$1, position);
    } else if (char === "F") {
      return read_name(lexer$1, position);
    } else if (char === "G") {
      return read_name(lexer$1, position);
    } else if (char === "H") {
      return read_name(lexer$1, position);
    } else if (char === "I") {
      return read_name(lexer$1, position);
    } else if (char === "J") {
      return read_name(lexer$1, position);
    } else if (char === "K") {
      return read_name(lexer$1, position);
    } else if (char === "L") {
      return read_name(lexer$1, position);
    } else if (char === "M") {
      return read_name(lexer$1, position);
    } else if (char === "N") {
      return read_name(lexer$1, position);
    } else if (char === "O") {
      return read_name(lexer$1, position);
    } else if (char === "P") {
      return read_name(lexer$1, position);
    } else if (char === "Q") {
      return read_name(lexer$1, position);
    } else if (char === "R") {
      return read_name(lexer$1, position);
    } else if (char === "S") {
      return read_name(lexer$1, position);
    } else if (char === "T") {
      return read_name(lexer$1, position);
    } else if (char === "U") {
      return read_name(lexer$1, position);
    } else if (char === "V") {
      return read_name(lexer$1, position);
    } else if (char === "W") {
      return read_name(lexer$1, position);
    } else if (char === "X") {
      return read_name(lexer$1, position);
    } else if (char === "Y") {
      return read_name(lexer$1, position);
    } else if (char === "Z") {
      return read_name(lexer$1, position);
    } else if (char === "_") {
      return read_name(lexer$1, position);
    } else {
      return new Error(new UnexpectedCharacter(char, position));
    }
  } else {
    return new Ok(
      [
        new TokenWithPosition(
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
    let $ = next_token(lexer);
    if ($ instanceof Ok) {
      let $1 = $[0][0].token;
      if ($1 instanceof EOF) {
        let _pipe = $list.reverse(
          listPrepend(
            new TokenWithPosition(
              new EOF(),
              new Position(lexer.line, lexer.column),
            ),
            tokens,
          ),
        );
        return new Ok(_pipe);
      } else {
        let token = $[0][0];
        let new_lexer$1 = $[0][1];
        loop$lexer = new_lexer$1;
        loop$tokens = listPrepend(token, tokens);
      }
    } else {
      return $;
    }
  }
}

export function tokenize(input) {
  let lexer = new_lexer(input);
  return tokenize_loop(lexer, toList([]));
}
