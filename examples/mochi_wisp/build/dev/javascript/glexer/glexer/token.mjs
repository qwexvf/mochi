import { CustomType as $CustomType } from "../gleam.mjs";

export class Name extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$Name = ($0) => new Name($0);
export const Token$isName = (value) => value instanceof Name;
export const Token$Name$0 = (value) => value[0];

export class UpperName extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$UpperName = ($0) => new UpperName($0);
export const Token$isUpperName = (value) => value instanceof UpperName;
export const Token$UpperName$0 = (value) => value[0];

export class DiscardName extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$DiscardName = ($0) => new DiscardName($0);
export const Token$isDiscardName = (value) => value instanceof DiscardName;
export const Token$DiscardName$0 = (value) => value[0];

export class Int extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$Int = ($0) => new Int($0);
export const Token$isInt = (value) => value instanceof Int;
export const Token$Int$0 = (value) => value[0];

export class Float extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$Float = ($0) => new Float($0);
export const Token$isFloat = (value) => value instanceof Float;
export const Token$Float$0 = (value) => value[0];

export class String extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$String = ($0) => new String($0);
export const Token$isString = (value) => value instanceof String;
export const Token$String$0 = (value) => value[0];

export class CommentDoc extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$CommentDoc = ($0) => new CommentDoc($0);
export const Token$isCommentDoc = (value) => value instanceof CommentDoc;
export const Token$CommentDoc$0 = (value) => value[0];

export class CommentNormal extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$CommentNormal = ($0) => new CommentNormal($0);
export const Token$isCommentNormal = (value) => value instanceof CommentNormal;
export const Token$CommentNormal$0 = (value) => value[0];

export class CommentModule extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$CommentModule = ($0) => new CommentModule($0);
export const Token$isCommentModule = (value) => value instanceof CommentModule;
export const Token$CommentModule$0 = (value) => value[0];

export class As extends $CustomType {}
export const Token$As = () => new As();
export const Token$isAs = (value) => value instanceof As;

export class Assert extends $CustomType {}
export const Token$Assert = () => new Assert();
export const Token$isAssert = (value) => value instanceof Assert;

export class Auto extends $CustomType {}
export const Token$Auto = () => new Auto();
export const Token$isAuto = (value) => value instanceof Auto;

export class Case extends $CustomType {}
export const Token$Case = () => new Case();
export const Token$isCase = (value) => value instanceof Case;

export class Const extends $CustomType {}
export const Token$Const = () => new Const();
export const Token$isConst = (value) => value instanceof Const;

export class Delegate extends $CustomType {}
export const Token$Delegate = () => new Delegate();
export const Token$isDelegate = (value) => value instanceof Delegate;

export class Derive extends $CustomType {}
export const Token$Derive = () => new Derive();
export const Token$isDerive = (value) => value instanceof Derive;

export class Echo extends $CustomType {}
export const Token$Echo = () => new Echo();
export const Token$isEcho = (value) => value instanceof Echo;

export class Else extends $CustomType {}
export const Token$Else = () => new Else();
export const Token$isElse = (value) => value instanceof Else;

export class Fn extends $CustomType {}
export const Token$Fn = () => new Fn();
export const Token$isFn = (value) => value instanceof Fn;

export class If extends $CustomType {}
export const Token$If = () => new If();
export const Token$isIf = (value) => value instanceof If;

export class Implement extends $CustomType {}
export const Token$Implement = () => new Implement();
export const Token$isImplement = (value) => value instanceof Implement;

export class Import extends $CustomType {}
export const Token$Import = () => new Import();
export const Token$isImport = (value) => value instanceof Import;

export class Let extends $CustomType {}
export const Token$Let = () => new Let();
export const Token$isLet = (value) => value instanceof Let;

export class Macro extends $CustomType {}
export const Token$Macro = () => new Macro();
export const Token$isMacro = (value) => value instanceof Macro;

export class Opaque extends $CustomType {}
export const Token$Opaque = () => new Opaque();
export const Token$isOpaque = (value) => value instanceof Opaque;

export class Panic extends $CustomType {}
export const Token$Panic = () => new Panic();
export const Token$isPanic = (value) => value instanceof Panic;

export class Pub extends $CustomType {}
export const Token$Pub = () => new Pub();
export const Token$isPub = (value) => value instanceof Pub;

export class Test extends $CustomType {}
export const Token$Test = () => new Test();
export const Token$isTest = (value) => value instanceof Test;

export class Todo extends $CustomType {}
export const Token$Todo = () => new Todo();
export const Token$isTodo = (value) => value instanceof Todo;

export class Type extends $CustomType {}
export const Token$Type = () => new Type();
export const Token$isType = (value) => value instanceof Type;

export class Use extends $CustomType {}
export const Token$Use = () => new Use();
export const Token$isUse = (value) => value instanceof Use;

export class LeftParen extends $CustomType {}
export const Token$LeftParen = () => new LeftParen();
export const Token$isLeftParen = (value) => value instanceof LeftParen;

export class RightParen extends $CustomType {}
export const Token$RightParen = () => new RightParen();
export const Token$isRightParen = (value) => value instanceof RightParen;

export class LeftBrace extends $CustomType {}
export const Token$LeftBrace = () => new LeftBrace();
export const Token$isLeftBrace = (value) => value instanceof LeftBrace;

export class RightBrace extends $CustomType {}
export const Token$RightBrace = () => new RightBrace();
export const Token$isRightBrace = (value) => value instanceof RightBrace;

export class LeftSquare extends $CustomType {}
export const Token$LeftSquare = () => new LeftSquare();
export const Token$isLeftSquare = (value) => value instanceof LeftSquare;

export class RightSquare extends $CustomType {}
export const Token$RightSquare = () => new RightSquare();
export const Token$isRightSquare = (value) => value instanceof RightSquare;

export class Plus extends $CustomType {}
export const Token$Plus = () => new Plus();
export const Token$isPlus = (value) => value instanceof Plus;

export class Minus extends $CustomType {}
export const Token$Minus = () => new Minus();
export const Token$isMinus = (value) => value instanceof Minus;

export class Star extends $CustomType {}
export const Token$Star = () => new Star();
export const Token$isStar = (value) => value instanceof Star;

export class Slash extends $CustomType {}
export const Token$Slash = () => new Slash();
export const Token$isSlash = (value) => value instanceof Slash;

export class Less extends $CustomType {}
export const Token$Less = () => new Less();
export const Token$isLess = (value) => value instanceof Less;

export class Greater extends $CustomType {}
export const Token$Greater = () => new Greater();
export const Token$isGreater = (value) => value instanceof Greater;

export class LessEqual extends $CustomType {}
export const Token$LessEqual = () => new LessEqual();
export const Token$isLessEqual = (value) => value instanceof LessEqual;

export class GreaterEqual extends $CustomType {}
export const Token$GreaterEqual = () => new GreaterEqual();
export const Token$isGreaterEqual = (value) => value instanceof GreaterEqual;

export class Percent extends $CustomType {}
export const Token$Percent = () => new Percent();
export const Token$isPercent = (value) => value instanceof Percent;

export class PlusDot extends $CustomType {}
export const Token$PlusDot = () => new PlusDot();
export const Token$isPlusDot = (value) => value instanceof PlusDot;

export class MinusDot extends $CustomType {}
export const Token$MinusDot = () => new MinusDot();
export const Token$isMinusDot = (value) => value instanceof MinusDot;

export class StarDot extends $CustomType {}
export const Token$StarDot = () => new StarDot();
export const Token$isStarDot = (value) => value instanceof StarDot;

export class SlashDot extends $CustomType {}
export const Token$SlashDot = () => new SlashDot();
export const Token$isSlashDot = (value) => value instanceof SlashDot;

export class LessDot extends $CustomType {}
export const Token$LessDot = () => new LessDot();
export const Token$isLessDot = (value) => value instanceof LessDot;

export class GreaterDot extends $CustomType {}
export const Token$GreaterDot = () => new GreaterDot();
export const Token$isGreaterDot = (value) => value instanceof GreaterDot;

export class LessEqualDot extends $CustomType {}
export const Token$LessEqualDot = () => new LessEqualDot();
export const Token$isLessEqualDot = (value) => value instanceof LessEqualDot;

export class GreaterEqualDot extends $CustomType {}
export const Token$GreaterEqualDot = () => new GreaterEqualDot();
export const Token$isGreaterEqualDot = (value) =>
  value instanceof GreaterEqualDot;

export class LessGreater extends $CustomType {}
export const Token$LessGreater = () => new LessGreater();
export const Token$isLessGreater = (value) => value instanceof LessGreater;

export class At extends $CustomType {}
export const Token$At = () => new At();
export const Token$isAt = (value) => value instanceof At;

export class Colon extends $CustomType {}
export const Token$Colon = () => new Colon();
export const Token$isColon = (value) => value instanceof Colon;

export class Comma extends $CustomType {}
export const Token$Comma = () => new Comma();
export const Token$isComma = (value) => value instanceof Comma;

export class Hash extends $CustomType {}
export const Token$Hash = () => new Hash();
export const Token$isHash = (value) => value instanceof Hash;

export class Bang extends $CustomType {}
export const Token$Bang = () => new Bang();
export const Token$isBang = (value) => value instanceof Bang;

export class Equal extends $CustomType {}
export const Token$Equal = () => new Equal();
export const Token$isEqual = (value) => value instanceof Equal;

export class EqualEqual extends $CustomType {}
export const Token$EqualEqual = () => new EqualEqual();
export const Token$isEqualEqual = (value) => value instanceof EqualEqual;

export class NotEqual extends $CustomType {}
export const Token$NotEqual = () => new NotEqual();
export const Token$isNotEqual = (value) => value instanceof NotEqual;

export class VBar extends $CustomType {}
export const Token$VBar = () => new VBar();
export const Token$isVBar = (value) => value instanceof VBar;

export class VBarVBar extends $CustomType {}
export const Token$VBarVBar = () => new VBarVBar();
export const Token$isVBarVBar = (value) => value instanceof VBarVBar;

export class AmperAmper extends $CustomType {}
export const Token$AmperAmper = () => new AmperAmper();
export const Token$isAmperAmper = (value) => value instanceof AmperAmper;

export class LessLess extends $CustomType {}
export const Token$LessLess = () => new LessLess();
export const Token$isLessLess = (value) => value instanceof LessLess;

export class GreaterGreater extends $CustomType {}
export const Token$GreaterGreater = () => new GreaterGreater();
export const Token$isGreaterGreater = (value) =>
  value instanceof GreaterGreater;

export class Pipe extends $CustomType {}
export const Token$Pipe = () => new Pipe();
export const Token$isPipe = (value) => value instanceof Pipe;

export class Dot extends $CustomType {}
export const Token$Dot = () => new Dot();
export const Token$isDot = (value) => value instanceof Dot;

export class DotDot extends $CustomType {}
export const Token$DotDot = () => new DotDot();
export const Token$isDotDot = (value) => value instanceof DotDot;

export class LeftArrow extends $CustomType {}
export const Token$LeftArrow = () => new LeftArrow();
export const Token$isLeftArrow = (value) => value instanceof LeftArrow;

export class RightArrow extends $CustomType {}
export const Token$RightArrow = () => new RightArrow();
export const Token$isRightArrow = (value) => value instanceof RightArrow;

export class EndOfFile extends $CustomType {}
export const Token$EndOfFile = () => new EndOfFile();
export const Token$isEndOfFile = (value) => value instanceof EndOfFile;

export class Space extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$Space = ($0) => new Space($0);
export const Token$isSpace = (value) => value instanceof Space;
export const Token$Space$0 = (value) => value[0];

export class UnterminatedString extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$UnterminatedString = ($0) => new UnterminatedString($0);
export const Token$isUnterminatedString = (value) =>
  value instanceof UnterminatedString;
export const Token$UnterminatedString$0 = (value) => value[0];

export class UnexpectedGrapheme extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Token$UnexpectedGrapheme = ($0) => new UnexpectedGrapheme($0);
export const Token$isUnexpectedGrapheme = (value) =>
  value instanceof UnexpectedGrapheme;
export const Token$UnexpectedGrapheme$0 = (value) => value[0];

/**
 * Turn a token back into its Gleam source representation.
 */
export function to_source(tok) {
  if (tok instanceof Name) {
    let str = tok[0];
    return str;
  } else if (tok instanceof UpperName) {
    let str = tok[0];
    return str;
  } else if (tok instanceof DiscardName) {
    let str = tok[0];
    return "_" + str;
  } else if (tok instanceof Int) {
    let str = tok[0];
    return str;
  } else if (tok instanceof Float) {
    let str = tok[0];
    return str;
  } else if (tok instanceof String) {
    let str = tok[0];
    return ("\"" + str) + "\"";
  } else if (tok instanceof CommentDoc) {
    let str = tok[0];
    return "///" + str;
  } else if (tok instanceof CommentNormal) {
    let str = tok[0];
    return "//" + str;
  } else if (tok instanceof CommentModule) {
    let str = tok[0];
    return "////" + str;
  } else if (tok instanceof As) {
    return "as";
  } else if (tok instanceof Assert) {
    return "assert";
  } else if (tok instanceof Auto) {
    return "auto";
  } else if (tok instanceof Case) {
    return "case";
  } else if (tok instanceof Const) {
    return "const";
  } else if (tok instanceof Delegate) {
    return "delegate";
  } else if (tok instanceof Derive) {
    return "derive";
  } else if (tok instanceof Echo) {
    return "echo";
  } else if (tok instanceof Else) {
    return "else";
  } else if (tok instanceof Fn) {
    return "fn";
  } else if (tok instanceof If) {
    return "if";
  } else if (tok instanceof Implement) {
    return "implement";
  } else if (tok instanceof Import) {
    return "import";
  } else if (tok instanceof Let) {
    return "let";
  } else if (tok instanceof Macro) {
    return "macro";
  } else if (tok instanceof Opaque) {
    return "opaque";
  } else if (tok instanceof Panic) {
    return "panic";
  } else if (tok instanceof Pub) {
    return "pub";
  } else if (tok instanceof Test) {
    return "test";
  } else if (tok instanceof Todo) {
    return "todo";
  } else if (tok instanceof Type) {
    return "type";
  } else if (tok instanceof Use) {
    return "use";
  } else if (tok instanceof LeftParen) {
    return "(";
  } else if (tok instanceof RightParen) {
    return ")";
  } else if (tok instanceof LeftBrace) {
    return "{";
  } else if (tok instanceof RightBrace) {
    return "}";
  } else if (tok instanceof LeftSquare) {
    return "[";
  } else if (tok instanceof RightSquare) {
    return "]";
  } else if (tok instanceof Plus) {
    return "+";
  } else if (tok instanceof Minus) {
    return "-";
  } else if (tok instanceof Star) {
    return "*";
  } else if (tok instanceof Slash) {
    return "/";
  } else if (tok instanceof Less) {
    return "<";
  } else if (tok instanceof Greater) {
    return ">";
  } else if (tok instanceof LessEqual) {
    return "<=";
  } else if (tok instanceof GreaterEqual) {
    return ">=";
  } else if (tok instanceof Percent) {
    return "%";
  } else if (tok instanceof PlusDot) {
    return "+.";
  } else if (tok instanceof MinusDot) {
    return "-.";
  } else if (tok instanceof StarDot) {
    return "*.";
  } else if (tok instanceof SlashDot) {
    return "/.";
  } else if (tok instanceof LessDot) {
    return "<.";
  } else if (tok instanceof GreaterDot) {
    return ">.";
  } else if (tok instanceof LessEqualDot) {
    return "<=.";
  } else if (tok instanceof GreaterEqualDot) {
    return ">=.";
  } else if (tok instanceof LessGreater) {
    return "<>";
  } else if (tok instanceof At) {
    return "@";
  } else if (tok instanceof Colon) {
    return ":";
  } else if (tok instanceof Comma) {
    return ",";
  } else if (tok instanceof Hash) {
    return "#";
  } else if (tok instanceof Bang) {
    return "!";
  } else if (tok instanceof Equal) {
    return "=";
  } else if (tok instanceof EqualEqual) {
    return "==";
  } else if (tok instanceof NotEqual) {
    return "!=";
  } else if (tok instanceof VBar) {
    return "|";
  } else if (tok instanceof VBarVBar) {
    return "||";
  } else if (tok instanceof AmperAmper) {
    return "&&";
  } else if (tok instanceof LessLess) {
    return "<<";
  } else if (tok instanceof GreaterGreater) {
    return ">>";
  } else if (tok instanceof Pipe) {
    return "|>";
  } else if (tok instanceof Dot) {
    return ".";
  } else if (tok instanceof DotDot) {
    return "..";
  } else if (tok instanceof LeftArrow) {
    return "<-";
  } else if (tok instanceof RightArrow) {
    return "->";
  } else if (tok instanceof EndOfFile) {
    return "";
  } else if (tok instanceof Space) {
    let str = tok[0];
    return str;
  } else if (tok instanceof UnterminatedString) {
    let str = tok[0];
    return "\"" + str;
  } else {
    let str = tok[0];
    return str;
  }
}
