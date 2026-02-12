import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $glexer from "../glexer/glexer.mjs";
import { Position as P } from "../glexer/glexer.mjs";
import * as $t from "../glexer/glexer/token.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
} from "./gleam.mjs";

const FILEPATH = "src/glance.gleam";

export class Definition extends $CustomType {
  constructor(attributes, definition) {
    super();
    this.attributes = attributes;
    this.definition = definition;
  }
}
export const Definition$Definition = (attributes, definition) =>
  new Definition(attributes, definition);
export const Definition$isDefinition = (value) => value instanceof Definition;
export const Definition$Definition$attributes = (value) => value.attributes;
export const Definition$Definition$0 = (value) => value.attributes;
export const Definition$Definition$definition = (value) => value.definition;
export const Definition$Definition$1 = (value) => value.definition;

export class Attribute extends $CustomType {
  constructor(name, arguments$) {
    super();
    this.name = name;
    this.arguments = arguments$;
  }
}
export const Attribute$Attribute = (name, arguments$) =>
  new Attribute(name, arguments$);
export const Attribute$isAttribute = (value) => value instanceof Attribute;
export const Attribute$Attribute$name = (value) => value.name;
export const Attribute$Attribute$0 = (value) => value.name;
export const Attribute$Attribute$arguments = (value) => value.arguments;
export const Attribute$Attribute$1 = (value) => value.arguments;

export class Module extends $CustomType {
  constructor(imports, custom_types, type_aliases, constants, functions) {
    super();
    this.imports = imports;
    this.custom_types = custom_types;
    this.type_aliases = type_aliases;
    this.constants = constants;
    this.functions = functions;
  }
}
export const Module$Module = (imports, custom_types, type_aliases, constants, functions) =>
  new Module(imports, custom_types, type_aliases, constants, functions);
export const Module$isModule = (value) => value instanceof Module;
export const Module$Module$imports = (value) => value.imports;
export const Module$Module$0 = (value) => value.imports;
export const Module$Module$custom_types = (value) => value.custom_types;
export const Module$Module$1 = (value) => value.custom_types;
export const Module$Module$type_aliases = (value) => value.type_aliases;
export const Module$Module$2 = (value) => value.type_aliases;
export const Module$Module$constants = (value) => value.constants;
export const Module$Module$3 = (value) => value.constants;
export const Module$Module$functions = (value) => value.functions;
export const Module$Module$4 = (value) => value.functions;

export class Function extends $CustomType {
  constructor(location, name, publicity, parameters, return$, body) {
    super();
    this.location = location;
    this.name = name;
    this.publicity = publicity;
    this.parameters = parameters;
    this.return = return$;
    this.body = body;
  }
}
export const Function$Function = (location, name, publicity, parameters, return$, body) =>
  new Function(location, name, publicity, parameters, return$, body);
export const Function$isFunction = (value) => value instanceof Function;
export const Function$Function$location = (value) => value.location;
export const Function$Function$0 = (value) => value.location;
export const Function$Function$name = (value) => value.name;
export const Function$Function$1 = (value) => value.name;
export const Function$Function$publicity = (value) => value.publicity;
export const Function$Function$2 = (value) => value.publicity;
export const Function$Function$parameters = (value) => value.parameters;
export const Function$Function$3 = (value) => value.parameters;
export const Function$Function$return = (value) => value.return;
export const Function$Function$4 = (value) => value.return;
export const Function$Function$body = (value) => value.body;
export const Function$Function$5 = (value) => value.body;

/**
 * A span within a file, indicated by byte offsets.
 */
export class Span extends $CustomType {
  constructor(start, end) {
    super();
    this.start = start;
    this.end = end;
  }
}
export const Span$Span = (start, end) => new Span(start, end);
export const Span$isSpan = (value) => value instanceof Span;
export const Span$Span$start = (value) => value.start;
export const Span$Span$0 = (value) => value.start;
export const Span$Span$end = (value) => value.end;
export const Span$Span$1 = (value) => value.end;

export class Use extends $CustomType {
  constructor(location, patterns, function$) {
    super();
    this.location = location;
    this.patterns = patterns;
    this.function = function$;
  }
}
export const Statement$Use = (location, patterns, function$) =>
  new Use(location, patterns, function$);
export const Statement$isUse = (value) => value instanceof Use;
export const Statement$Use$location = (value) => value.location;
export const Statement$Use$0 = (value) => value.location;
export const Statement$Use$patterns = (value) => value.patterns;
export const Statement$Use$1 = (value) => value.patterns;
export const Statement$Use$function = (value) => value.function;
export const Statement$Use$2 = (value) => value.function;

export class Assignment extends $CustomType {
  constructor(location, kind, pattern, annotation, value) {
    super();
    this.location = location;
    this.kind = kind;
    this.pattern = pattern;
    this.annotation = annotation;
    this.value = value;
  }
}
export const Statement$Assignment = (location, kind, pattern, annotation, value) =>
  new Assignment(location, kind, pattern, annotation, value);
export const Statement$isAssignment = (value) => value instanceof Assignment;
export const Statement$Assignment$location = (value) => value.location;
export const Statement$Assignment$0 = (value) => value.location;
export const Statement$Assignment$kind = (value) => value.kind;
export const Statement$Assignment$1 = (value) => value.kind;
export const Statement$Assignment$pattern = (value) => value.pattern;
export const Statement$Assignment$2 = (value) => value.pattern;
export const Statement$Assignment$annotation = (value) => value.annotation;
export const Statement$Assignment$3 = (value) => value.annotation;
export const Statement$Assignment$value = (value) => value.value;
export const Statement$Assignment$4 = (value) => value.value;

export class Assert extends $CustomType {
  constructor(location, expression, message) {
    super();
    this.location = location;
    this.expression = expression;
    this.message = message;
  }
}
export const Statement$Assert = (location, expression, message) =>
  new Assert(location, expression, message);
export const Statement$isAssert = (value) => value instanceof Assert;
export const Statement$Assert$location = (value) => value.location;
export const Statement$Assert$0 = (value) => value.location;
export const Statement$Assert$expression = (value) => value.expression;
export const Statement$Assert$1 = (value) => value.expression;
export const Statement$Assert$message = (value) => value.message;
export const Statement$Assert$2 = (value) => value.message;

export class Expression extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Statement$Expression = ($0) => new Expression($0);
export const Statement$isExpression = (value) => value instanceof Expression;
export const Statement$Expression$0 = (value) => value[0];

export class Let extends $CustomType {}
export const AssignmentKind$Let = () => new Let();
export const AssignmentKind$isLet = (value) => value instanceof Let;

export class LetAssert extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}
export const AssignmentKind$LetAssert = (message) => new LetAssert(message);
export const AssignmentKind$isLetAssert = (value) => value instanceof LetAssert;
export const AssignmentKind$LetAssert$message = (value) => value.message;
export const AssignmentKind$LetAssert$0 = (value) => value.message;

export class UsePattern extends $CustomType {
  constructor(pattern, annotation) {
    super();
    this.pattern = pattern;
    this.annotation = annotation;
  }
}
export const UsePattern$UsePattern = (pattern, annotation) =>
  new UsePattern(pattern, annotation);
export const UsePattern$isUsePattern = (value) => value instanceof UsePattern;
export const UsePattern$UsePattern$pattern = (value) => value.pattern;
export const UsePattern$UsePattern$0 = (value) => value.pattern;
export const UsePattern$UsePattern$annotation = (value) => value.annotation;
export const UsePattern$UsePattern$1 = (value) => value.annotation;

export class PatternInt extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Pattern$PatternInt = (location, value) =>
  new PatternInt(location, value);
export const Pattern$isPatternInt = (value) => value instanceof PatternInt;
export const Pattern$PatternInt$location = (value) => value.location;
export const Pattern$PatternInt$0 = (value) => value.location;
export const Pattern$PatternInt$value = (value) => value.value;
export const Pattern$PatternInt$1 = (value) => value.value;

export class PatternFloat extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Pattern$PatternFloat = (location, value) =>
  new PatternFloat(location, value);
export const Pattern$isPatternFloat = (value) => value instanceof PatternFloat;
export const Pattern$PatternFloat$location = (value) => value.location;
export const Pattern$PatternFloat$0 = (value) => value.location;
export const Pattern$PatternFloat$value = (value) => value.value;
export const Pattern$PatternFloat$1 = (value) => value.value;

export class PatternString extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Pattern$PatternString = (location, value) =>
  new PatternString(location, value);
export const Pattern$isPatternString = (value) =>
  value instanceof PatternString;
export const Pattern$PatternString$location = (value) => value.location;
export const Pattern$PatternString$0 = (value) => value.location;
export const Pattern$PatternString$value = (value) => value.value;
export const Pattern$PatternString$1 = (value) => value.value;

export class PatternDiscard extends $CustomType {
  constructor(location, name) {
    super();
    this.location = location;
    this.name = name;
  }
}
export const Pattern$PatternDiscard = (location, name) =>
  new PatternDiscard(location, name);
export const Pattern$isPatternDiscard = (value) =>
  value instanceof PatternDiscard;
export const Pattern$PatternDiscard$location = (value) => value.location;
export const Pattern$PatternDiscard$0 = (value) => value.location;
export const Pattern$PatternDiscard$name = (value) => value.name;
export const Pattern$PatternDiscard$1 = (value) => value.name;

export class PatternVariable extends $CustomType {
  constructor(location, name) {
    super();
    this.location = location;
    this.name = name;
  }
}
export const Pattern$PatternVariable = (location, name) =>
  new PatternVariable(location, name);
export const Pattern$isPatternVariable = (value) =>
  value instanceof PatternVariable;
export const Pattern$PatternVariable$location = (value) => value.location;
export const Pattern$PatternVariable$0 = (value) => value.location;
export const Pattern$PatternVariable$name = (value) => value.name;
export const Pattern$PatternVariable$1 = (value) => value.name;

export class PatternTuple extends $CustomType {
  constructor(location, elements) {
    super();
    this.location = location;
    this.elements = elements;
  }
}
export const Pattern$PatternTuple = (location, elements) =>
  new PatternTuple(location, elements);
export const Pattern$isPatternTuple = (value) => value instanceof PatternTuple;
export const Pattern$PatternTuple$location = (value) => value.location;
export const Pattern$PatternTuple$0 = (value) => value.location;
export const Pattern$PatternTuple$elements = (value) => value.elements;
export const Pattern$PatternTuple$1 = (value) => value.elements;

export class PatternList extends $CustomType {
  constructor(location, elements, tail) {
    super();
    this.location = location;
    this.elements = elements;
    this.tail = tail;
  }
}
export const Pattern$PatternList = (location, elements, tail) =>
  new PatternList(location, elements, tail);
export const Pattern$isPatternList = (value) => value instanceof PatternList;
export const Pattern$PatternList$location = (value) => value.location;
export const Pattern$PatternList$0 = (value) => value.location;
export const Pattern$PatternList$elements = (value) => value.elements;
export const Pattern$PatternList$1 = (value) => value.elements;
export const Pattern$PatternList$tail = (value) => value.tail;
export const Pattern$PatternList$2 = (value) => value.tail;

export class PatternAssignment extends $CustomType {
  constructor(location, pattern, name) {
    super();
    this.location = location;
    this.pattern = pattern;
    this.name = name;
  }
}
export const Pattern$PatternAssignment = (location, pattern, name) =>
  new PatternAssignment(location, pattern, name);
export const Pattern$isPatternAssignment = (value) =>
  value instanceof PatternAssignment;
export const Pattern$PatternAssignment$location = (value) => value.location;
export const Pattern$PatternAssignment$0 = (value) => value.location;
export const Pattern$PatternAssignment$pattern = (value) => value.pattern;
export const Pattern$PatternAssignment$1 = (value) => value.pattern;
export const Pattern$PatternAssignment$name = (value) => value.name;
export const Pattern$PatternAssignment$2 = (value) => value.name;

export class PatternConcatenate extends $CustomType {
  constructor(location, prefix, prefix_name, rest_name) {
    super();
    this.location = location;
    this.prefix = prefix;
    this.prefix_name = prefix_name;
    this.rest_name = rest_name;
  }
}
export const Pattern$PatternConcatenate = (location, prefix, prefix_name, rest_name) =>
  new PatternConcatenate(location, prefix, prefix_name, rest_name);
export const Pattern$isPatternConcatenate = (value) =>
  value instanceof PatternConcatenate;
export const Pattern$PatternConcatenate$location = (value) => value.location;
export const Pattern$PatternConcatenate$0 = (value) => value.location;
export const Pattern$PatternConcatenate$prefix = (value) => value.prefix;
export const Pattern$PatternConcatenate$1 = (value) => value.prefix;
export const Pattern$PatternConcatenate$prefix_name = (value) =>
  value.prefix_name;
export const Pattern$PatternConcatenate$2 = (value) => value.prefix_name;
export const Pattern$PatternConcatenate$rest_name = (value) => value.rest_name;
export const Pattern$PatternConcatenate$3 = (value) => value.rest_name;

export class PatternBitString extends $CustomType {
  constructor(location, segments) {
    super();
    this.location = location;
    this.segments = segments;
  }
}
export const Pattern$PatternBitString = (location, segments) =>
  new PatternBitString(location, segments);
export const Pattern$isPatternBitString = (value) =>
  value instanceof PatternBitString;
export const Pattern$PatternBitString$location = (value) => value.location;
export const Pattern$PatternBitString$0 = (value) => value.location;
export const Pattern$PatternBitString$segments = (value) => value.segments;
export const Pattern$PatternBitString$1 = (value) => value.segments;

export class PatternVariant extends $CustomType {
  constructor(location, module, constructor, arguments$, with_spread) {
    super();
    this.location = location;
    this.module = module;
    this.constructor$ = constructor;
    this.arguments = arguments$;
    this.with_spread = with_spread;
  }
}
export const Pattern$PatternVariant = (location, module, constructor, arguments$, with_spread) =>
  new PatternVariant(location, module, constructor, arguments$, with_spread);
export const Pattern$isPatternVariant = (value) =>
  value instanceof PatternVariant;
export const Pattern$PatternVariant$location = (value) => value.location;
export const Pattern$PatternVariant$0 = (value) => value.location;
export const Pattern$PatternVariant$module = (value) => value.module;
export const Pattern$PatternVariant$1 = (value) => value.module;
export const Pattern$PatternVariant$constructor = (value) => value.constructor$;
export const Pattern$PatternVariant$2 = (value) => value.constructor$;
export const Pattern$PatternVariant$arguments = (value) => value.arguments;
export const Pattern$PatternVariant$3 = (value) => value.arguments;
export const Pattern$PatternVariant$with_spread = (value) => value.with_spread;
export const Pattern$PatternVariant$4 = (value) => value.with_spread;

export const Pattern$location = (value) => value.location;

export class Int extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Expression$Int = (location, value) => new Int(location, value);
export const Expression$isInt = (value) => value instanceof Int;
export const Expression$Int$location = (value) => value.location;
export const Expression$Int$0 = (value) => value.location;
export const Expression$Int$value = (value) => value.value;
export const Expression$Int$1 = (value) => value.value;

export class Float extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Expression$Float = (location, value) => new Float(location, value);
export const Expression$isFloat = (value) => value instanceof Float;
export const Expression$Float$location = (value) => value.location;
export const Expression$Float$0 = (value) => value.location;
export const Expression$Float$value = (value) => value.value;
export const Expression$Float$1 = (value) => value.value;

export class String extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Expression$String = (location, value) =>
  new String(location, value);
export const Expression$isString = (value) => value instanceof String;
export const Expression$String$location = (value) => value.location;
export const Expression$String$0 = (value) => value.location;
export const Expression$String$value = (value) => value.value;
export const Expression$String$1 = (value) => value.value;

export class Variable extends $CustomType {
  constructor(location, name) {
    super();
    this.location = location;
    this.name = name;
  }
}
export const Expression$Variable = (location, name) =>
  new Variable(location, name);
export const Expression$isVariable = (value) => value instanceof Variable;
export const Expression$Variable$location = (value) => value.location;
export const Expression$Variable$0 = (value) => value.location;
export const Expression$Variable$name = (value) => value.name;
export const Expression$Variable$1 = (value) => value.name;

export class NegateInt extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Expression$NegateInt = (location, value) =>
  new NegateInt(location, value);
export const Expression$isNegateInt = (value) => value instanceof NegateInt;
export const Expression$NegateInt$location = (value) => value.location;
export const Expression$NegateInt$0 = (value) => value.location;
export const Expression$NegateInt$value = (value) => value.value;
export const Expression$NegateInt$1 = (value) => value.value;

export class NegateBool extends $CustomType {
  constructor(location, value) {
    super();
    this.location = location;
    this.value = value;
  }
}
export const Expression$NegateBool = (location, value) =>
  new NegateBool(location, value);
export const Expression$isNegateBool = (value) => value instanceof NegateBool;
export const Expression$NegateBool$location = (value) => value.location;
export const Expression$NegateBool$0 = (value) => value.location;
export const Expression$NegateBool$value = (value) => value.value;
export const Expression$NegateBool$1 = (value) => value.value;

export class Block extends $CustomType {
  constructor(location, statements) {
    super();
    this.location = location;
    this.statements = statements;
  }
}
export const Expression$Block = (location, statements) =>
  new Block(location, statements);
export const Expression$isBlock = (value) => value instanceof Block;
export const Expression$Block$location = (value) => value.location;
export const Expression$Block$0 = (value) => value.location;
export const Expression$Block$statements = (value) => value.statements;
export const Expression$Block$1 = (value) => value.statements;

export class Panic extends $CustomType {
  constructor(location, message) {
    super();
    this.location = location;
    this.message = message;
  }
}
export const Expression$Panic = (location, message) =>
  new Panic(location, message);
export const Expression$isPanic = (value) => value instanceof Panic;
export const Expression$Panic$location = (value) => value.location;
export const Expression$Panic$0 = (value) => value.location;
export const Expression$Panic$message = (value) => value.message;
export const Expression$Panic$1 = (value) => value.message;

export class Todo extends $CustomType {
  constructor(location, message) {
    super();
    this.location = location;
    this.message = message;
  }
}
export const Expression$Todo = (location, message) =>
  new Todo(location, message);
export const Expression$isTodo = (value) => value instanceof Todo;
export const Expression$Todo$location = (value) => value.location;
export const Expression$Todo$0 = (value) => value.location;
export const Expression$Todo$message = (value) => value.message;
export const Expression$Todo$1 = (value) => value.message;

export class Tuple extends $CustomType {
  constructor(location, elements) {
    super();
    this.location = location;
    this.elements = elements;
  }
}
export const Expression$Tuple = (location, elements) =>
  new Tuple(location, elements);
export const Expression$isTuple = (value) => value instanceof Tuple;
export const Expression$Tuple$location = (value) => value.location;
export const Expression$Tuple$0 = (value) => value.location;
export const Expression$Tuple$elements = (value) => value.elements;
export const Expression$Tuple$1 = (value) => value.elements;

export class List extends $CustomType {
  constructor(location, elements, rest) {
    super();
    this.location = location;
    this.elements = elements;
    this.rest = rest;
  }
}
export const Expression$List = (location, elements, rest) =>
  new List(location, elements, rest);
export const Expression$isList = (value) => value instanceof List;
export const Expression$List$location = (value) => value.location;
export const Expression$List$0 = (value) => value.location;
export const Expression$List$elements = (value) => value.elements;
export const Expression$List$1 = (value) => value.elements;
export const Expression$List$rest = (value) => value.rest;
export const Expression$List$2 = (value) => value.rest;

export class Fn extends $CustomType {
  constructor(location, arguments$, return_annotation, body) {
    super();
    this.location = location;
    this.arguments = arguments$;
    this.return_annotation = return_annotation;
    this.body = body;
  }
}
export const Expression$Fn = (location, arguments$, return_annotation, body) =>
  new Fn(location, arguments$, return_annotation, body);
export const Expression$isFn = (value) => value instanceof Fn;
export const Expression$Fn$location = (value) => value.location;
export const Expression$Fn$0 = (value) => value.location;
export const Expression$Fn$arguments = (value) => value.arguments;
export const Expression$Fn$1 = (value) => value.arguments;
export const Expression$Fn$return_annotation = (value) =>
  value.return_annotation;
export const Expression$Fn$2 = (value) => value.return_annotation;
export const Expression$Fn$body = (value) => value.body;
export const Expression$Fn$3 = (value) => value.body;

export class RecordUpdate extends $CustomType {
  constructor(location, module, constructor, record, fields) {
    super();
    this.location = location;
    this.module = module;
    this.constructor$ = constructor;
    this.record = record;
    this.fields = fields;
  }
}
export const Expression$RecordUpdate = (location, module, constructor, record, fields) =>
  new RecordUpdate(location, module, constructor, record, fields);
export const Expression$isRecordUpdate = (value) =>
  value instanceof RecordUpdate;
export const Expression$RecordUpdate$location = (value) => value.location;
export const Expression$RecordUpdate$0 = (value) => value.location;
export const Expression$RecordUpdate$module = (value) => value.module;
export const Expression$RecordUpdate$1 = (value) => value.module;
export const Expression$RecordUpdate$constructor = (value) =>
  value.constructor$;
export const Expression$RecordUpdate$2 = (value) => value.constructor$;
export const Expression$RecordUpdate$record = (value) => value.record;
export const Expression$RecordUpdate$3 = (value) => value.record;
export const Expression$RecordUpdate$fields = (value) => value.fields;
export const Expression$RecordUpdate$4 = (value) => value.fields;

export class FieldAccess extends $CustomType {
  constructor(location, container, label) {
    super();
    this.location = location;
    this.container = container;
    this.label = label;
  }
}
export const Expression$FieldAccess = (location, container, label) =>
  new FieldAccess(location, container, label);
export const Expression$isFieldAccess = (value) => value instanceof FieldAccess;
export const Expression$FieldAccess$location = (value) => value.location;
export const Expression$FieldAccess$0 = (value) => value.location;
export const Expression$FieldAccess$container = (value) => value.container;
export const Expression$FieldAccess$1 = (value) => value.container;
export const Expression$FieldAccess$label = (value) => value.label;
export const Expression$FieldAccess$2 = (value) => value.label;

export class Call extends $CustomType {
  constructor(location, function$, arguments$) {
    super();
    this.location = location;
    this.function = function$;
    this.arguments = arguments$;
  }
}
export const Expression$Call = (location, function$, arguments$) =>
  new Call(location, function$, arguments$);
export const Expression$isCall = (value) => value instanceof Call;
export const Expression$Call$location = (value) => value.location;
export const Expression$Call$0 = (value) => value.location;
export const Expression$Call$function = (value) => value.function;
export const Expression$Call$1 = (value) => value.function;
export const Expression$Call$arguments = (value) => value.arguments;
export const Expression$Call$2 = (value) => value.arguments;

export class TupleIndex extends $CustomType {
  constructor(location, tuple, index) {
    super();
    this.location = location;
    this.tuple = tuple;
    this.index = index;
  }
}
export const Expression$TupleIndex = (location, tuple, index) =>
  new TupleIndex(location, tuple, index);
export const Expression$isTupleIndex = (value) => value instanceof TupleIndex;
export const Expression$TupleIndex$location = (value) => value.location;
export const Expression$TupleIndex$0 = (value) => value.location;
export const Expression$TupleIndex$tuple = (value) => value.tuple;
export const Expression$TupleIndex$1 = (value) => value.tuple;
export const Expression$TupleIndex$index = (value) => value.index;
export const Expression$TupleIndex$2 = (value) => value.index;

export class FnCapture extends $CustomType {
  constructor(location, label, function$, arguments_before, arguments_after) {
    super();
    this.location = location;
    this.label = label;
    this.function = function$;
    this.arguments_before = arguments_before;
    this.arguments_after = arguments_after;
  }
}
export const Expression$FnCapture = (location, label, function$, arguments_before, arguments_after) =>
  new FnCapture(location, label, function$, arguments_before, arguments_after);
export const Expression$isFnCapture = (value) => value instanceof FnCapture;
export const Expression$FnCapture$location = (value) => value.location;
export const Expression$FnCapture$0 = (value) => value.location;
export const Expression$FnCapture$label = (value) => value.label;
export const Expression$FnCapture$1 = (value) => value.label;
export const Expression$FnCapture$function = (value) => value.function;
export const Expression$FnCapture$2 = (value) => value.function;
export const Expression$FnCapture$arguments_before = (value) =>
  value.arguments_before;
export const Expression$FnCapture$3 = (value) => value.arguments_before;
export const Expression$FnCapture$arguments_after = (value) =>
  value.arguments_after;
export const Expression$FnCapture$4 = (value) => value.arguments_after;

export class BitString extends $CustomType {
  constructor(location, segments) {
    super();
    this.location = location;
    this.segments = segments;
  }
}
export const Expression$BitString = (location, segments) =>
  new BitString(location, segments);
export const Expression$isBitString = (value) => value instanceof BitString;
export const Expression$BitString$location = (value) => value.location;
export const Expression$BitString$0 = (value) => value.location;
export const Expression$BitString$segments = (value) => value.segments;
export const Expression$BitString$1 = (value) => value.segments;

export class Case extends $CustomType {
  constructor(location, subjects, clauses) {
    super();
    this.location = location;
    this.subjects = subjects;
    this.clauses = clauses;
  }
}
export const Expression$Case = (location, subjects, clauses) =>
  new Case(location, subjects, clauses);
export const Expression$isCase = (value) => value instanceof Case;
export const Expression$Case$location = (value) => value.location;
export const Expression$Case$0 = (value) => value.location;
export const Expression$Case$subjects = (value) => value.subjects;
export const Expression$Case$1 = (value) => value.subjects;
export const Expression$Case$clauses = (value) => value.clauses;
export const Expression$Case$2 = (value) => value.clauses;

export class BinaryOperator extends $CustomType {
  constructor(location, name, left, right) {
    super();
    this.location = location;
    this.name = name;
    this.left = left;
    this.right = right;
  }
}
export const Expression$BinaryOperator = (location, name, left, right) =>
  new BinaryOperator(location, name, left, right);
export const Expression$isBinaryOperator = (value) =>
  value instanceof BinaryOperator;
export const Expression$BinaryOperator$location = (value) => value.location;
export const Expression$BinaryOperator$0 = (value) => value.location;
export const Expression$BinaryOperator$name = (value) => value.name;
export const Expression$BinaryOperator$1 = (value) => value.name;
export const Expression$BinaryOperator$left = (value) => value.left;
export const Expression$BinaryOperator$2 = (value) => value.left;
export const Expression$BinaryOperator$right = (value) => value.right;
export const Expression$BinaryOperator$3 = (value) => value.right;

export class Echo extends $CustomType {
  constructor(location, expression, message) {
    super();
    this.location = location;
    this.expression = expression;
    this.message = message;
  }
}
export const Expression$Echo = (location, expression, message) =>
  new Echo(location, expression, message);
export const Expression$isEcho = (value) => value instanceof Echo;
export const Expression$Echo$location = (value) => value.location;
export const Expression$Echo$0 = (value) => value.location;
export const Expression$Echo$expression = (value) => value.expression;
export const Expression$Echo$1 = (value) => value.expression;
export const Expression$Echo$message = (value) => value.message;
export const Expression$Echo$2 = (value) => value.message;

export const Expression$location = (value) => value.location;

export class Clause extends $CustomType {
  constructor(patterns, guard, body) {
    super();
    this.patterns = patterns;
    this.guard = guard;
    this.body = body;
  }
}
export const Clause$Clause = (patterns, guard, body) =>
  new Clause(patterns, guard, body);
export const Clause$isClause = (value) => value instanceof Clause;
export const Clause$Clause$patterns = (value) => value.patterns;
export const Clause$Clause$0 = (value) => value.patterns;
export const Clause$Clause$guard = (value) => value.guard;
export const Clause$Clause$1 = (value) => value.guard;
export const Clause$Clause$body = (value) => value.body;
export const Clause$Clause$2 = (value) => value.body;

export class BytesOption extends $CustomType {}
export const BitStringSegmentOption$BytesOption = () => new BytesOption();
export const BitStringSegmentOption$isBytesOption = (value) =>
  value instanceof BytesOption;

export class IntOption extends $CustomType {}
export const BitStringSegmentOption$IntOption = () => new IntOption();
export const BitStringSegmentOption$isIntOption = (value) =>
  value instanceof IntOption;

export class FloatOption extends $CustomType {}
export const BitStringSegmentOption$FloatOption = () => new FloatOption();
export const BitStringSegmentOption$isFloatOption = (value) =>
  value instanceof FloatOption;

export class BitsOption extends $CustomType {}
export const BitStringSegmentOption$BitsOption = () => new BitsOption();
export const BitStringSegmentOption$isBitsOption = (value) =>
  value instanceof BitsOption;

export class Utf8Option extends $CustomType {}
export const BitStringSegmentOption$Utf8Option = () => new Utf8Option();
export const BitStringSegmentOption$isUtf8Option = (value) =>
  value instanceof Utf8Option;

export class Utf16Option extends $CustomType {}
export const BitStringSegmentOption$Utf16Option = () => new Utf16Option();
export const BitStringSegmentOption$isUtf16Option = (value) =>
  value instanceof Utf16Option;

export class Utf32Option extends $CustomType {}
export const BitStringSegmentOption$Utf32Option = () => new Utf32Option();
export const BitStringSegmentOption$isUtf32Option = (value) =>
  value instanceof Utf32Option;

export class Utf8CodepointOption extends $CustomType {}
export const BitStringSegmentOption$Utf8CodepointOption = () =>
  new Utf8CodepointOption();
export const BitStringSegmentOption$isUtf8CodepointOption = (value) =>
  value instanceof Utf8CodepointOption;

export class Utf16CodepointOption extends $CustomType {}
export const BitStringSegmentOption$Utf16CodepointOption = () =>
  new Utf16CodepointOption();
export const BitStringSegmentOption$isUtf16CodepointOption = (value) =>
  value instanceof Utf16CodepointOption;

export class Utf32CodepointOption extends $CustomType {}
export const BitStringSegmentOption$Utf32CodepointOption = () =>
  new Utf32CodepointOption();
export const BitStringSegmentOption$isUtf32CodepointOption = (value) =>
  value instanceof Utf32CodepointOption;

export class SignedOption extends $CustomType {}
export const BitStringSegmentOption$SignedOption = () => new SignedOption();
export const BitStringSegmentOption$isSignedOption = (value) =>
  value instanceof SignedOption;

export class UnsignedOption extends $CustomType {}
export const BitStringSegmentOption$UnsignedOption = () => new UnsignedOption();
export const BitStringSegmentOption$isUnsignedOption = (value) =>
  value instanceof UnsignedOption;

export class BigOption extends $CustomType {}
export const BitStringSegmentOption$BigOption = () => new BigOption();
export const BitStringSegmentOption$isBigOption = (value) =>
  value instanceof BigOption;

export class LittleOption extends $CustomType {}
export const BitStringSegmentOption$LittleOption = () => new LittleOption();
export const BitStringSegmentOption$isLittleOption = (value) =>
  value instanceof LittleOption;

export class NativeOption extends $CustomType {}
export const BitStringSegmentOption$NativeOption = () => new NativeOption();
export const BitStringSegmentOption$isNativeOption = (value) =>
  value instanceof NativeOption;

export class SizeValueOption extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const BitStringSegmentOption$SizeValueOption = ($0) =>
  new SizeValueOption($0);
export const BitStringSegmentOption$isSizeValueOption = (value) =>
  value instanceof SizeValueOption;
export const BitStringSegmentOption$SizeValueOption$0 = (value) => value[0];

export class SizeOption extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const BitStringSegmentOption$SizeOption = ($0) => new SizeOption($0);
export const BitStringSegmentOption$isSizeOption = (value) =>
  value instanceof SizeOption;
export const BitStringSegmentOption$SizeOption$0 = (value) => value[0];

export class UnitOption extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const BitStringSegmentOption$UnitOption = ($0) => new UnitOption($0);
export const BitStringSegmentOption$isUnitOption = (value) =>
  value instanceof UnitOption;
export const BitStringSegmentOption$UnitOption$0 = (value) => value[0];

export class And extends $CustomType {}
export const BinaryOperator$And = () => new And();
export const BinaryOperator$isAnd = (value) => value instanceof And;

export class Or extends $CustomType {}
export const BinaryOperator$Or = () => new Or();
export const BinaryOperator$isOr = (value) => value instanceof Or;

export class Eq extends $CustomType {}
export const BinaryOperator$Eq = () => new Eq();
export const BinaryOperator$isEq = (value) => value instanceof Eq;

export class NotEq extends $CustomType {}
export const BinaryOperator$NotEq = () => new NotEq();
export const BinaryOperator$isNotEq = (value) => value instanceof NotEq;

export class LtInt extends $CustomType {}
export const BinaryOperator$LtInt = () => new LtInt();
export const BinaryOperator$isLtInt = (value) => value instanceof LtInt;

export class LtEqInt extends $CustomType {}
export const BinaryOperator$LtEqInt = () => new LtEqInt();
export const BinaryOperator$isLtEqInt = (value) => value instanceof LtEqInt;

export class LtFloat extends $CustomType {}
export const BinaryOperator$LtFloat = () => new LtFloat();
export const BinaryOperator$isLtFloat = (value) => value instanceof LtFloat;

export class LtEqFloat extends $CustomType {}
export const BinaryOperator$LtEqFloat = () => new LtEqFloat();
export const BinaryOperator$isLtEqFloat = (value) => value instanceof LtEqFloat;

export class GtEqInt extends $CustomType {}
export const BinaryOperator$GtEqInt = () => new GtEqInt();
export const BinaryOperator$isGtEqInt = (value) => value instanceof GtEqInt;

export class GtInt extends $CustomType {}
export const BinaryOperator$GtInt = () => new GtInt();
export const BinaryOperator$isGtInt = (value) => value instanceof GtInt;

export class GtEqFloat extends $CustomType {}
export const BinaryOperator$GtEqFloat = () => new GtEqFloat();
export const BinaryOperator$isGtEqFloat = (value) => value instanceof GtEqFloat;

export class GtFloat extends $CustomType {}
export const BinaryOperator$GtFloat = () => new GtFloat();
export const BinaryOperator$isGtFloat = (value) => value instanceof GtFloat;

export class Pipe extends $CustomType {}
export const BinaryOperator$Pipe = () => new Pipe();
export const BinaryOperator$isPipe = (value) => value instanceof Pipe;

export class AddInt extends $CustomType {}
export const BinaryOperator$AddInt = () => new AddInt();
export const BinaryOperator$isAddInt = (value) => value instanceof AddInt;

export class AddFloat extends $CustomType {}
export const BinaryOperator$AddFloat = () => new AddFloat();
export const BinaryOperator$isAddFloat = (value) => value instanceof AddFloat;

export class SubInt extends $CustomType {}
export const BinaryOperator$SubInt = () => new SubInt();
export const BinaryOperator$isSubInt = (value) => value instanceof SubInt;

export class SubFloat extends $CustomType {}
export const BinaryOperator$SubFloat = () => new SubFloat();
export const BinaryOperator$isSubFloat = (value) => value instanceof SubFloat;

export class MultInt extends $CustomType {}
export const BinaryOperator$MultInt = () => new MultInt();
export const BinaryOperator$isMultInt = (value) => value instanceof MultInt;

export class MultFloat extends $CustomType {}
export const BinaryOperator$MultFloat = () => new MultFloat();
export const BinaryOperator$isMultFloat = (value) => value instanceof MultFloat;

export class DivInt extends $CustomType {}
export const BinaryOperator$DivInt = () => new DivInt();
export const BinaryOperator$isDivInt = (value) => value instanceof DivInt;

export class DivFloat extends $CustomType {}
export const BinaryOperator$DivFloat = () => new DivFloat();
export const BinaryOperator$isDivFloat = (value) => value instanceof DivFloat;

export class RemainderInt extends $CustomType {}
export const BinaryOperator$RemainderInt = () => new RemainderInt();
export const BinaryOperator$isRemainderInt = (value) =>
  value instanceof RemainderInt;

export class Concatenate extends $CustomType {}
export const BinaryOperator$Concatenate = () => new Concatenate();
export const BinaryOperator$isConcatenate = (value) =>
  value instanceof Concatenate;

export class FnParameter extends $CustomType {
  constructor(name, type_) {
    super();
    this.name = name;
    this.type_ = type_;
  }
}
export const FnParameter$FnParameter = (name, type_) =>
  new FnParameter(name, type_);
export const FnParameter$isFnParameter = (value) =>
  value instanceof FnParameter;
export const FnParameter$FnParameter$name = (value) => value.name;
export const FnParameter$FnParameter$0 = (value) => value.name;
export const FnParameter$FnParameter$type_ = (value) => value.type_;
export const FnParameter$FnParameter$1 = (value) => value.type_;

export class FunctionParameter extends $CustomType {
  constructor(label, name, type_) {
    super();
    this.label = label;
    this.name = name;
    this.type_ = type_;
  }
}
export const FunctionParameter$FunctionParameter = (label, name, type_) =>
  new FunctionParameter(label, name, type_);
export const FunctionParameter$isFunctionParameter = (value) =>
  value instanceof FunctionParameter;
export const FunctionParameter$FunctionParameter$label = (value) => value.label;
export const FunctionParameter$FunctionParameter$0 = (value) => value.label;
export const FunctionParameter$FunctionParameter$name = (value) => value.name;
export const FunctionParameter$FunctionParameter$1 = (value) => value.name;
export const FunctionParameter$FunctionParameter$type_ = (value) => value.type_;
export const FunctionParameter$FunctionParameter$2 = (value) => value.type_;

export class Named extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const AssignmentName$Named = ($0) => new Named($0);
export const AssignmentName$isNamed = (value) => value instanceof Named;
export const AssignmentName$Named$0 = (value) => value[0];

export class Discarded extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const AssignmentName$Discarded = ($0) => new Discarded($0);
export const AssignmentName$isDiscarded = (value) => value instanceof Discarded;
export const AssignmentName$Discarded$0 = (value) => value[0];

export class Import extends $CustomType {
  constructor(location, module, alias, unqualified_types, unqualified_values) {
    super();
    this.location = location;
    this.module = module;
    this.alias = alias;
    this.unqualified_types = unqualified_types;
    this.unqualified_values = unqualified_values;
  }
}
export const Import$Import = (location, module, alias, unqualified_types, unqualified_values) =>
  new Import(location, module, alias, unqualified_types, unqualified_values);
export const Import$isImport = (value) => value instanceof Import;
export const Import$Import$location = (value) => value.location;
export const Import$Import$0 = (value) => value.location;
export const Import$Import$module = (value) => value.module;
export const Import$Import$1 = (value) => value.module;
export const Import$Import$alias = (value) => value.alias;
export const Import$Import$2 = (value) => value.alias;
export const Import$Import$unqualified_types = (value) =>
  value.unqualified_types;
export const Import$Import$3 = (value) => value.unqualified_types;
export const Import$Import$unqualified_values = (value) =>
  value.unqualified_values;
export const Import$Import$4 = (value) => value.unqualified_values;

export class Constant extends $CustomType {
  constructor(location, name, publicity, annotation, value) {
    super();
    this.location = location;
    this.name = name;
    this.publicity = publicity;
    this.annotation = annotation;
    this.value = value;
  }
}
export const Constant$Constant = (location, name, publicity, annotation, value) =>
  new Constant(location, name, publicity, annotation, value);
export const Constant$isConstant = (value) => value instanceof Constant;
export const Constant$Constant$location = (value) => value.location;
export const Constant$Constant$0 = (value) => value.location;
export const Constant$Constant$name = (value) => value.name;
export const Constant$Constant$1 = (value) => value.name;
export const Constant$Constant$publicity = (value) => value.publicity;
export const Constant$Constant$2 = (value) => value.publicity;
export const Constant$Constant$annotation = (value) => value.annotation;
export const Constant$Constant$3 = (value) => value.annotation;
export const Constant$Constant$value = (value) => value.value;
export const Constant$Constant$4 = (value) => value.value;

export class UnqualifiedImport extends $CustomType {
  constructor(name, alias) {
    super();
    this.name = name;
    this.alias = alias;
  }
}
export const UnqualifiedImport$UnqualifiedImport = (name, alias) =>
  new UnqualifiedImport(name, alias);
export const UnqualifiedImport$isUnqualifiedImport = (value) =>
  value instanceof UnqualifiedImport;
export const UnqualifiedImport$UnqualifiedImport$name = (value) => value.name;
export const UnqualifiedImport$UnqualifiedImport$0 = (value) => value.name;
export const UnqualifiedImport$UnqualifiedImport$alias = (value) => value.alias;
export const UnqualifiedImport$UnqualifiedImport$1 = (value) => value.alias;

export class Public extends $CustomType {}
export const Publicity$Public = () => new Public();
export const Publicity$isPublic = (value) => value instanceof Public;

export class Private extends $CustomType {}
export const Publicity$Private = () => new Private();
export const Publicity$isPrivate = (value) => value instanceof Private;

export class TypeAlias extends $CustomType {
  constructor(location, name, publicity, parameters, aliased) {
    super();
    this.location = location;
    this.name = name;
    this.publicity = publicity;
    this.parameters = parameters;
    this.aliased = aliased;
  }
}
export const TypeAlias$TypeAlias = (location, name, publicity, parameters, aliased) =>
  new TypeAlias(location, name, publicity, parameters, aliased);
export const TypeAlias$isTypeAlias = (value) => value instanceof TypeAlias;
export const TypeAlias$TypeAlias$location = (value) => value.location;
export const TypeAlias$TypeAlias$0 = (value) => value.location;
export const TypeAlias$TypeAlias$name = (value) => value.name;
export const TypeAlias$TypeAlias$1 = (value) => value.name;
export const TypeAlias$TypeAlias$publicity = (value) => value.publicity;
export const TypeAlias$TypeAlias$2 = (value) => value.publicity;
export const TypeAlias$TypeAlias$parameters = (value) => value.parameters;
export const TypeAlias$TypeAlias$3 = (value) => value.parameters;
export const TypeAlias$TypeAlias$aliased = (value) => value.aliased;
export const TypeAlias$TypeAlias$4 = (value) => value.aliased;

export class CustomType extends $CustomType {
  constructor(location, name, publicity, opaque_, parameters, variants) {
    super();
    this.location = location;
    this.name = name;
    this.publicity = publicity;
    this.opaque_ = opaque_;
    this.parameters = parameters;
    this.variants = variants;
  }
}
export const CustomType$CustomType = (location, name, publicity, opaque_, parameters, variants) =>
  new CustomType(location, name, publicity, opaque_, parameters, variants);
export const CustomType$isCustomType = (value) => value instanceof CustomType;
export const CustomType$CustomType$location = (value) => value.location;
export const CustomType$CustomType$0 = (value) => value.location;
export const CustomType$CustomType$name = (value) => value.name;
export const CustomType$CustomType$1 = (value) => value.name;
export const CustomType$CustomType$publicity = (value) => value.publicity;
export const CustomType$CustomType$2 = (value) => value.publicity;
export const CustomType$CustomType$opaque_ = (value) => value.opaque_;
export const CustomType$CustomType$3 = (value) => value.opaque_;
export const CustomType$CustomType$parameters = (value) => value.parameters;
export const CustomType$CustomType$4 = (value) => value.parameters;
export const CustomType$CustomType$variants = (value) => value.variants;
export const CustomType$CustomType$5 = (value) => value.variants;

export class Variant extends $CustomType {
  constructor(name, fields, attributes) {
    super();
    this.name = name;
    this.fields = fields;
    this.attributes = attributes;
  }
}
export const Variant$Variant = (name, fields, attributes) =>
  new Variant(name, fields, attributes);
export const Variant$isVariant = (value) => value instanceof Variant;
export const Variant$Variant$name = (value) => value.name;
export const Variant$Variant$0 = (value) => value.name;
export const Variant$Variant$fields = (value) => value.fields;
export const Variant$Variant$1 = (value) => value.fields;
export const Variant$Variant$attributes = (value) => value.attributes;
export const Variant$Variant$2 = (value) => value.attributes;

export class RecordUpdateField extends $CustomType {
  constructor(label, item) {
    super();
    this.label = label;
    this.item = item;
  }
}
export const RecordUpdateField$RecordUpdateField = (label, item) =>
  new RecordUpdateField(label, item);
export const RecordUpdateField$isRecordUpdateField = (value) =>
  value instanceof RecordUpdateField;
export const RecordUpdateField$RecordUpdateField$label = (value) => value.label;
export const RecordUpdateField$RecordUpdateField$0 = (value) => value.label;
export const RecordUpdateField$RecordUpdateField$item = (value) => value.item;
export const RecordUpdateField$RecordUpdateField$1 = (value) => value.item;

export class LabelledVariantField extends $CustomType {
  constructor(item, label) {
    super();
    this.item = item;
    this.label = label;
  }
}
export const VariantField$LabelledVariantField = (item, label) =>
  new LabelledVariantField(item, label);
export const VariantField$isLabelledVariantField = (value) =>
  value instanceof LabelledVariantField;
export const VariantField$LabelledVariantField$item = (value) => value.item;
export const VariantField$LabelledVariantField$0 = (value) => value.item;
export const VariantField$LabelledVariantField$label = (value) => value.label;
export const VariantField$LabelledVariantField$1 = (value) => value.label;

export class UnlabelledVariantField extends $CustomType {
  constructor(item) {
    super();
    this.item = item;
  }
}
export const VariantField$UnlabelledVariantField = (item) =>
  new UnlabelledVariantField(item);
export const VariantField$isUnlabelledVariantField = (value) =>
  value instanceof UnlabelledVariantField;
export const VariantField$UnlabelledVariantField$item = (value) => value.item;
export const VariantField$UnlabelledVariantField$0 = (value) => value.item;

export const VariantField$item = (value) => value.item;

export class LabelledField extends $CustomType {
  constructor(label, label_location, item) {
    super();
    this.label = label;
    this.label_location = label_location;
    this.item = item;
  }
}
export const Field$LabelledField = (label, label_location, item) =>
  new LabelledField(label, label_location, item);
export const Field$isLabelledField = (value) => value instanceof LabelledField;
export const Field$LabelledField$label = (value) => value.label;
export const Field$LabelledField$0 = (value) => value.label;
export const Field$LabelledField$label_location = (value) =>
  value.label_location;
export const Field$LabelledField$1 = (value) => value.label_location;
export const Field$LabelledField$item = (value) => value.item;
export const Field$LabelledField$2 = (value) => value.item;

export class ShorthandField extends $CustomType {
  constructor(label, location) {
    super();
    this.label = label;
    this.location = location;
  }
}
export const Field$ShorthandField = (label, location) =>
  new ShorthandField(label, location);
export const Field$isShorthandField = (value) =>
  value instanceof ShorthandField;
export const Field$ShorthandField$label = (value) => value.label;
export const Field$ShorthandField$0 = (value) => value.label;
export const Field$ShorthandField$location = (value) => value.location;
export const Field$ShorthandField$1 = (value) => value.location;

export class UnlabelledField extends $CustomType {
  constructor(item) {
    super();
    this.item = item;
  }
}
export const Field$UnlabelledField = (item) => new UnlabelledField(item);
export const Field$isUnlabelledField = (value) =>
  value instanceof UnlabelledField;
export const Field$UnlabelledField$item = (value) => value.item;
export const Field$UnlabelledField$0 = (value) => value.item;

export class NamedType extends $CustomType {
  constructor(location, name, module, parameters) {
    super();
    this.location = location;
    this.name = name;
    this.module = module;
    this.parameters = parameters;
  }
}
export const Type$NamedType = (location, name, module, parameters) =>
  new NamedType(location, name, module, parameters);
export const Type$isNamedType = (value) => value instanceof NamedType;
export const Type$NamedType$location = (value) => value.location;
export const Type$NamedType$0 = (value) => value.location;
export const Type$NamedType$name = (value) => value.name;
export const Type$NamedType$1 = (value) => value.name;
export const Type$NamedType$module = (value) => value.module;
export const Type$NamedType$2 = (value) => value.module;
export const Type$NamedType$parameters = (value) => value.parameters;
export const Type$NamedType$3 = (value) => value.parameters;

export class TupleType extends $CustomType {
  constructor(location, elements) {
    super();
    this.location = location;
    this.elements = elements;
  }
}
export const Type$TupleType = (location, elements) =>
  new TupleType(location, elements);
export const Type$isTupleType = (value) => value instanceof TupleType;
export const Type$TupleType$location = (value) => value.location;
export const Type$TupleType$0 = (value) => value.location;
export const Type$TupleType$elements = (value) => value.elements;
export const Type$TupleType$1 = (value) => value.elements;

export class FunctionType extends $CustomType {
  constructor(location, parameters, return$) {
    super();
    this.location = location;
    this.parameters = parameters;
    this.return = return$;
  }
}
export const Type$FunctionType = (location, parameters, return$) =>
  new FunctionType(location, parameters, return$);
export const Type$isFunctionType = (value) => value instanceof FunctionType;
export const Type$FunctionType$location = (value) => value.location;
export const Type$FunctionType$0 = (value) => value.location;
export const Type$FunctionType$parameters = (value) => value.parameters;
export const Type$FunctionType$1 = (value) => value.parameters;
export const Type$FunctionType$return = (value) => value.return;
export const Type$FunctionType$2 = (value) => value.return;

export class VariableType extends $CustomType {
  constructor(location, name) {
    super();
    this.location = location;
    this.name = name;
  }
}
export const Type$VariableType = (location, name) =>
  new VariableType(location, name);
export const Type$isVariableType = (value) => value instanceof VariableType;
export const Type$VariableType$location = (value) => value.location;
export const Type$VariableType$0 = (value) => value.location;
export const Type$VariableType$name = (value) => value.name;
export const Type$VariableType$1 = (value) => value.name;

export class HoleType extends $CustomType {
  constructor(location, name) {
    super();
    this.location = location;
    this.name = name;
  }
}
export const Type$HoleType = (location, name) => new HoleType(location, name);
export const Type$isHoleType = (value) => value instanceof HoleType;
export const Type$HoleType$location = (value) => value.location;
export const Type$HoleType$0 = (value) => value.location;
export const Type$HoleType$name = (value) => value.name;
export const Type$HoleType$1 = (value) => value.name;

export const Type$location = (value) => value.location;

export class UnexpectedEndOfInput extends $CustomType {}
export const Error$UnexpectedEndOfInput = () => new UnexpectedEndOfInput();
export const Error$isUnexpectedEndOfInput = (value) =>
  value instanceof UnexpectedEndOfInput;

export class UnexpectedToken extends $CustomType {
  constructor(token, position) {
    super();
    this.token = token;
    this.position = position;
  }
}
export const Error$UnexpectedToken = (token, position) =>
  new UnexpectedToken(token, position);
export const Error$isUnexpectedToken = (value) =>
  value instanceof UnexpectedToken;
export const Error$UnexpectedToken$token = (value) => value.token;
export const Error$UnexpectedToken$0 = (value) => value.token;
export const Error$UnexpectedToken$position = (value) => value.position;
export const Error$UnexpectedToken$1 = (value) => value.position;

class UnqualifiedImports extends $CustomType {
  constructor(types, values, end, remaining_tokens) {
    super();
    this.types = types;
    this.values = values;
    this.end = end;
    this.remaining_tokens = remaining_tokens;
  }
}

class PatternConstructorArguments extends $CustomType {
  constructor(fields, spread, end, remaining_tokens) {
    super();
    this.fields = fields;
    this.spread = spread;
    this.end = end;
    this.remaining_tokens = remaining_tokens;
  }
}

class RegularExpressionUnit extends $CustomType {}

class ExpressionUnitAfterPipe extends $CustomType {}

class ParsedList extends $CustomType {
  constructor(values, spread, remaining_tokens, end) {
    super();
    this.values = values;
    this.spread = spread;
    this.remaining_tokens = remaining_tokens;
    this.end = end;
  }
}

export function precedence(operator) {
  if (operator instanceof And) {
    return 2;
  } else if (operator instanceof Or) {
    return 1;
  } else if (operator instanceof Eq) {
    return 3;
  } else if (operator instanceof NotEq) {
    return 3;
  } else if (operator instanceof LtInt) {
    return 4;
  } else if (operator instanceof LtEqInt) {
    return 4;
  } else if (operator instanceof LtFloat) {
    return 4;
  } else if (operator instanceof LtEqFloat) {
    return 4;
  } else if (operator instanceof GtEqInt) {
    return 4;
  } else if (operator instanceof GtInt) {
    return 4;
  } else if (operator instanceof GtEqFloat) {
    return 4;
  } else if (operator instanceof GtFloat) {
    return 4;
  } else if (operator instanceof Pipe) {
    return 6;
  } else if (operator instanceof AddInt) {
    return 7;
  } else if (operator instanceof AddFloat) {
    return 7;
  } else if (operator instanceof SubInt) {
    return 7;
  } else if (operator instanceof SubFloat) {
    return 7;
  } else if (operator instanceof MultInt) {
    return 8;
  } else if (operator instanceof MultFloat) {
    return 8;
  } else if (operator instanceof DivInt) {
    return 8;
  } else if (operator instanceof DivFloat) {
    return 8;
  } else if (operator instanceof RemainderInt) {
    return 8;
  } else {
    return 5;
  }
}

function push_variant(custom_type, variant) {
  return new CustomType(
    custom_type.location,
    custom_type.name,
    custom_type.publicity,
    custom_type.opaque_,
    custom_type.parameters,
    listPrepend(variant, custom_type.variants),
  );
}

function expect(expected, tokens, next) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let token = tokens.head[0];
    if (isEqual(token, expected)) {
      let tokens$1 = tokens.tail;
      let position = tokens.head[1];
      return next(position, tokens$1);
    } else {
      let other = tokens.head[0];
      let position = tokens.head[1];
      return new Error(new UnexpectedToken(other, position));
    }
  }
}

function expect_upper_name(tokens, next) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.UpperName) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      let name$1 = $[0];
      return next(name$1, end, tokens$1);
    } else {
      let other = $;
      let position = tokens.head[1];
      return new Error(new UnexpectedToken(other, position));
    }
  }
}

function expect_name(tokens, next) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Name) {
      let tokens$1 = tokens.tail;
      let name$1 = $[0];
      return next(name$1, tokens$1);
    } else {
      let other = $;
      let position = tokens.head[1];
      return new Error(new UnexpectedToken(other, position));
    }
  }
}

function module_name(loop$name, loop$end, loop$tokens) {
  while (true) {
    let name = loop$name;
    let end = loop$end;
    let tokens = loop$tokens;
    if (tokens instanceof $Empty) {
      if (name === "") {
        return new Error(new UnexpectedEndOfInput());
      } else {
        return new Ok([name, end, tokens]);
      }
    } else {
      let $ = tokens.tail;
      if ($ instanceof $Empty) {
        let $1 = tokens.head[0];
        if ($1 instanceof $t.Name && name === "") {
          let tokens$1 = $;
          let i = tokens.head[1].byte_offset;
          let s = $1[0];
          let end$1 = i + $string.byte_size(s);
          loop$name = s;
          loop$end = end$1;
          loop$tokens = tokens$1;
        } else if (name === "") {
          let other = $1;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        } else {
          return new Ok([name, end, tokens]);
        }
      } else {
        let $1 = tokens.head[0];
        if ($1 instanceof $t.Name && name === "") {
          let tokens$1 = $;
          let s = $1[0];
          let i = tokens.head[1].byte_offset;
          let end$1 = i + $string.byte_size(s);
          loop$name = s;
          loop$end = end$1;
          loop$tokens = tokens$1;
        } else if ($1 instanceof $t.Slash) {
          let $2 = $.head[0];
          if ($2 instanceof $t.Name && name !== "") {
            let tokens$1 = $.tail;
            let i = $.head[1].byte_offset;
            let s = $2[0];
            let end$1 = i + $string.byte_size(s);
            loop$name = (name + "/") + s;
            loop$end = end$1;
            loop$tokens = tokens$1;
          } else if (name === "") {
            let other = $1;
            let position = tokens.head[1];
            return new Error(new UnexpectedToken(other, position));
          } else {
            return new Ok([name, end, tokens]);
          }
        } else if (name === "") {
          let other = $1;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        } else {
          return new Ok([name, end, tokens]);
        }
      }
    }
  }
}

function unexpected_error(tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let token = tokens.head[0];
    let position = tokens.head[1];
    return new Error(new UnexpectedToken(token, position));
  }
}

function binary_operator(token) {
  if (token instanceof $t.Plus) {
    return new Ok(new AddInt());
  } else if (token instanceof $t.Minus) {
    return new Ok(new SubInt());
  } else if (token instanceof $t.Star) {
    return new Ok(new MultInt());
  } else if (token instanceof $t.Slash) {
    return new Ok(new DivInt());
  } else if (token instanceof $t.Less) {
    return new Ok(new LtInt());
  } else if (token instanceof $t.Greater) {
    return new Ok(new GtInt());
  } else if (token instanceof $t.LessEqual) {
    return new Ok(new LtEqInt());
  } else if (token instanceof $t.GreaterEqual) {
    return new Ok(new GtEqInt());
  } else if (token instanceof $t.Percent) {
    return new Ok(new RemainderInt());
  } else if (token instanceof $t.PlusDot) {
    return new Ok(new AddFloat());
  } else if (token instanceof $t.MinusDot) {
    return new Ok(new SubFloat());
  } else if (token instanceof $t.StarDot) {
    return new Ok(new MultFloat());
  } else if (token instanceof $t.SlashDot) {
    return new Ok(new DivFloat());
  } else if (token instanceof $t.LessDot) {
    return new Ok(new LtFloat());
  } else if (token instanceof $t.GreaterDot) {
    return new Ok(new GtFloat());
  } else if (token instanceof $t.LessEqualDot) {
    return new Ok(new LtEqFloat());
  } else if (token instanceof $t.GreaterEqualDot) {
    return new Ok(new GtEqFloat());
  } else if (token instanceof $t.LessGreater) {
    return new Ok(new Concatenate());
  } else if (token instanceof $t.EqualEqual) {
    return new Ok(new Eq());
  } else if (token instanceof $t.NotEqual) {
    return new Ok(new NotEq());
  } else if (token instanceof $t.VBarVBar) {
    return new Ok(new Or());
  } else if (token instanceof $t.AmperAmper) {
    return new Ok(new And());
  } else if (token instanceof $t.Pipe) {
    return new Ok(new Pipe());
  } else {
    return new Error(undefined);
  }
}

function pop_binary_operator(tokens) {
  if (tokens instanceof $Empty) {
    return new Error(undefined);
  } else {
    let tokens$1 = tokens.tail;
    let token = tokens.head[0];
    return $result.map(
      binary_operator(token),
      (op) => { return [op, tokens$1]; },
    );
  }
}

/**
 * Simple-Precedence-Parser, handle seeing an operator or end
 * 
 * @ignore
 */
function handle_operator(loop$next, loop$operators, loop$values) {
  while (true) {
    let next = loop$next;
    let operators = loop$operators;
    let values = loop$values;
    if (next instanceof Some) {
      if (operators instanceof $Empty) {
        let operator = next[0];
        return [new None(), toList([operator]), values];
      } else if (values instanceof $Empty) {
        throw makeError(
          "panic",
          FILEPATH,
          "glance",
          1173,
          "handle_operator",
          "parser bug, expression not full reduced",
          {}
        )
      } else {
        let $ = values.tail;
        if ($ instanceof $Empty) {
          throw makeError(
            "panic",
            FILEPATH,
            "glance",
            1173,
            "handle_operator",
            "parser bug, expression not full reduced",
            {}
          )
        } else {
          let next$1 = next[0];
          let previous = operators.head;
          let operators$1 = operators.tail;
          let a = values.head;
          let b = $.head;
          let rest_values = $.tail;
          let $1 = precedence(previous) >= precedence(next$1);
          if ($1) {
            let span = new Span(b.location.start, a.location.end);
            let expression$1 = new BinaryOperator(span, previous, b, a);
            let values$1 = listPrepend(expression$1, rest_values);
            loop$next = new Some(next$1);
            loop$operators = operators$1;
            loop$values = values$1;
          } else {
            return [
              new None(),
              listPrepend(next$1, listPrepend(previous, operators$1)),
              values,
            ];
          }
        }
      }
    } else if (operators instanceof $Empty) {
      if (values instanceof $Empty) {
        return [new None(), operators, values];
      } else {
        let $ = values.tail;
        if ($ instanceof $Empty) {
          let expression$1 = values.head;
          return [new Some(expression$1), operators, values];
        } else {
          throw makeError(
            "panic",
            FILEPATH,
            "glance",
            1173,
            "handle_operator",
            "parser bug, expression not full reduced",
            {}
          )
        }
      }
    } else if (values instanceof $Empty) {
      throw makeError(
        "panic",
        FILEPATH,
        "glance",
        1173,
        "handle_operator",
        "parser bug, expression not full reduced",
        {}
      )
    } else {
      let $ = values.tail;
      if ($ instanceof $Empty) {
        throw makeError(
          "panic",
          FILEPATH,
          "glance",
          1173,
          "handle_operator",
          "parser bug, expression not full reduced",
          {}
        )
      } else {
        let operator = operators.head;
        let operators$1 = operators.tail;
        let a = values.head;
        let b = $.head;
        let values$1 = $.tail;
        let values$2 = listPrepend(
          new BinaryOperator(
            new Span(b.location.start, a.location.end),
            operator,
            b,
            a,
          ),
          values$1,
        );
        loop$next = new None();
        loop$operators = operators$1;
        loop$values = values$2;
      }
    }
  }
}

function span_from_string(start, string) {
  return new Span(start, start + $string.byte_size(string));
}

function string_offset(start, string) {
  return start + $string.byte_size(string);
}

function until(loop$limit, loop$acc, loop$tokens, loop$callback) {
  while (true) {
    let limit = loop$limit;
    let acc = loop$acc;
    let tokens = loop$tokens;
    let callback = loop$callback;
    if (tokens instanceof $Empty) {
      return new Error(new UnexpectedEndOfInput());
    } else {
      let token = tokens.head[0];
      if (isEqual(token, limit)) {
        let tokens$1 = tokens.tail;
        let i = tokens.head[1].byte_offset;
        return new Ok([acc, string_offset(i, $t.to_source(token)), tokens$1]);
      } else {
        let $ = callback(acc, tokens);
        if ($ instanceof Ok) {
          let acc$1 = $[0][0];
          let tokens$1 = $[0][1];
          loop$limit = limit;
          loop$acc = acc$1;
          loop$tokens = tokens$1;
          loop$callback = callback;
        } else {
          return $;
        }
      }
    }
  }
}

function optional_module_alias(tokens, end) {
  if (tokens instanceof $Empty) {
    return [new None(), end, tokens];
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      return [new None(), end, tokens];
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.As) {
        let $2 = $.head[0];
        if ($2 instanceof $t.Name) {
          let tokens$1 = $.tail;
          let alias_start = $.head[1].byte_offset;
          let alias = $2[0];
          return [
            new Some(new Named(alias)),
            string_offset(alias_start, alias),
            tokens$1,
          ];
        } else if ($2 instanceof $t.DiscardName) {
          let tokens$1 = $.tail;
          let alias_start = $.head[1].byte_offset;
          let alias = $2[0];
          return [
            new Some(new Discarded(alias)),
            string_offset(alias_start, alias) + 1,
            tokens$1,
          ];
        } else {
          return [new None(), end, tokens];
        }
      } else {
        return [new None(), end, tokens];
      }
    }
  }
}

function list(parser, discard, acc, tokens) {
  if (tokens instanceof $Empty) {
    return $result.try$(
      parser(tokens),
      (_use0) => {
        let element;
        let tokens$1;
        element = _use0[0];
        tokens$1 = _use0[1];
        let acc$1 = listPrepend(element, acc);
        if (tokens$1 instanceof $Empty) {
          return new Error(new UnexpectedEndOfInput());
        } else {
          let $ = tokens$1.head[0];
          if ($ instanceof $t.RightSquare) {
            let tokens$2 = tokens$1.tail;
            let end = tokens$1.head[1].byte_offset;
            return new Ok(
              new ParsedList(
                $list.reverse(acc$1),
                new None(),
                tokens$2,
                end + 1,
              ),
            );
          } else if ($ instanceof $t.Comma) {
            let $1 = tokens$1.tail;
            if ($1 instanceof $Empty) {
              let tokens$2 = $1;
              return list(parser, discard, acc$1, tokens$2);
            } else {
              let $2 = $1.head[0];
              if ($2 instanceof $t.RightSquare) {
                let tokens$2 = $1.tail;
                let end = $1.head[1].byte_offset;
                return new Ok(
                  new ParsedList(
                    $list.reverse(acc$1),
                    new None(),
                    tokens$2,
                    end + 1,
                  ),
                );
              } else if ($2 instanceof $t.DotDot) {
                let $3 = $1.tail;
                if ($3 instanceof $Empty) {
                  let tokens$2 = $3;
                  return $result.try$(
                    parser(tokens$2),
                    (_use0) => {
                      let rest;
                      let tokens$3;
                      rest = _use0[0];
                      tokens$3 = _use0[1];
                      return expect(
                        new $t.RightSquare(),
                        tokens$3,
                        (_use0, tokens) => {
                          let end;
                          end = _use0.byte_offset;
                          return new Ok(
                            new ParsedList(
                              $list.reverse(acc$1),
                              new Some(rest),
                              tokens,
                              end + 1,
                            ),
                          );
                        },
                      );
                    },
                  );
                } else {
                  let $4 = $3.head[0];
                  if ($4 instanceof $t.RightSquare) {
                    let start = $1.head[1].byte_offset;
                    let close = $3.head;
                    let tokens$2 = $3.tail;
                    let end = $3.head[1].byte_offset;
                    if (discard instanceof Some) {
                      let discard$1 = discard[0];
                      let value = discard$1(new Span(start, start + 1));
                      let parsed_list = new ParsedList(
                        $list.reverse(acc$1),
                        new Some(value),
                        tokens$2,
                        end + 1,
                      );
                      return new Ok(parsed_list);
                    } else {
                      return unexpected_error(listPrepend(close, tokens$2));
                    }
                  } else {
                    let tokens$2 = $3;
                    return $result.try$(
                      parser(tokens$2),
                      (_use0) => {
                        let rest;
                        let tokens$3;
                        rest = _use0[0];
                        tokens$3 = _use0[1];
                        return expect(
                          new $t.RightSquare(),
                          tokens$3,
                          (_use0, tokens) => {
                            let end;
                            end = _use0.byte_offset;
                            return new Ok(
                              new ParsedList(
                                $list.reverse(acc$1),
                                new Some(rest),
                                tokens,
                                end + 1,
                              ),
                            );
                          },
                        );
                      },
                    );
                  }
                }
              } else {
                let tokens$2 = $1;
                return list(parser, discard, acc$1, tokens$2);
              }
            }
          } else {
            let other = $;
            let position = tokens$1.head[1];
            return new Error(new UnexpectedToken(other, position));
          }
        }
      },
    );
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.RightSquare) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      return new Ok(
        new ParsedList($list.reverse(acc), new None(), tokens$1, end + 1),
      );
    } else if ($ instanceof $t.Comma) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        return $result.try$(
          parser(tokens),
          (_use0) => {
            let element;
            let tokens$1;
            element = _use0[0];
            tokens$1 = _use0[1];
            let acc$1 = listPrepend(element, acc);
            if (tokens$1 instanceof $Empty) {
              return new Error(new UnexpectedEndOfInput());
            } else {
              let $2 = tokens$1.head[0];
              if ($2 instanceof $t.RightSquare) {
                let tokens$2 = tokens$1.tail;
                let end = tokens$1.head[1].byte_offset;
                return new Ok(
                  new ParsedList(
                    $list.reverse(acc$1),
                    new None(),
                    tokens$2,
                    end + 1,
                  ),
                );
              } else if ($2 instanceof $t.Comma) {
                let $3 = tokens$1.tail;
                if ($3 instanceof $Empty) {
                  let tokens$2 = $3;
                  return list(parser, discard, acc$1, tokens$2);
                } else {
                  let $4 = $3.head[0];
                  if ($4 instanceof $t.RightSquare) {
                    let tokens$2 = $3.tail;
                    let end = $3.head[1].byte_offset;
                    return new Ok(
                      new ParsedList(
                        $list.reverse(acc$1),
                        new None(),
                        tokens$2,
                        end + 1,
                      ),
                    );
                  } else if ($4 instanceof $t.DotDot) {
                    let $5 = $3.tail;
                    if ($5 instanceof $Empty) {
                      let tokens$2 = $5;
                      return $result.try$(
                        parser(tokens$2),
                        (_use0) => {
                          let rest;
                          let tokens$3;
                          rest = _use0[0];
                          tokens$3 = _use0[1];
                          return expect(
                            new $t.RightSquare(),
                            tokens$3,
                            (_use0, tokens) => {
                              let end;
                              end = _use0.byte_offset;
                              return new Ok(
                                new ParsedList(
                                  $list.reverse(acc$1),
                                  new Some(rest),
                                  tokens,
                                  end + 1,
                                ),
                              );
                            },
                          );
                        },
                      );
                    } else {
                      let $6 = $5.head[0];
                      if ($6 instanceof $t.RightSquare) {
                        let start = $3.head[1].byte_offset;
                        let close = $5.head;
                        let tokens$2 = $5.tail;
                        let end = $5.head[1].byte_offset;
                        if (discard instanceof Some) {
                          let discard$1 = discard[0];
                          let value = discard$1(new Span(start, start + 1));
                          let parsed_list = new ParsedList(
                            $list.reverse(acc$1),
                            new Some(value),
                            tokens$2,
                            end + 1,
                          );
                          return new Ok(parsed_list);
                        } else {
                          return unexpected_error(listPrepend(close, tokens$2));
                        }
                      } else {
                        let tokens$2 = $5;
                        return $result.try$(
                          parser(tokens$2),
                          (_use0) => {
                            let rest;
                            let tokens$3;
                            rest = _use0[0];
                            tokens$3 = _use0[1];
                            return expect(
                              new $t.RightSquare(),
                              tokens$3,
                              (_use0, tokens) => {
                                let end;
                                end = _use0.byte_offset;
                                return new Ok(
                                  new ParsedList(
                                    $list.reverse(acc$1),
                                    new Some(rest),
                                    tokens,
                                    end + 1,
                                  ),
                                );
                              },
                            );
                          },
                        );
                      }
                    }
                  } else {
                    let tokens$2 = $3;
                    return list(parser, discard, acc$1, tokens$2);
                  }
                }
              } else {
                let other = $2;
                let position = tokens$1.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          },
        );
      } else {
        let $2 = $1.head[0];
        if ($2 instanceof $t.RightSquare && !isEqual(acc, toList([]))) {
          let tokens$1 = $1.tail;
          let end = $1.head[1].byte_offset;
          return new Ok(
            new ParsedList($list.reverse(acc), new None(), tokens$1, end + 1),
          );
        } else {
          return $result.try$(
            parser(tokens),
            (_use0) => {
              let element;
              let tokens$1;
              element = _use0[0];
              tokens$1 = _use0[1];
              let acc$1 = listPrepend(element, acc);
              if (tokens$1 instanceof $Empty) {
                return new Error(new UnexpectedEndOfInput());
              } else {
                let $3 = tokens$1.head[0];
                if ($3 instanceof $t.RightSquare) {
                  let tokens$2 = tokens$1.tail;
                  let end = tokens$1.head[1].byte_offset;
                  return new Ok(
                    new ParsedList(
                      $list.reverse(acc$1),
                      new None(),
                      tokens$2,
                      end + 1,
                    ),
                  );
                } else if ($3 instanceof $t.Comma) {
                  let $4 = tokens$1.tail;
                  if ($4 instanceof $Empty) {
                    let tokens$2 = $4;
                    return list(parser, discard, acc$1, tokens$2);
                  } else {
                    let $5 = $4.head[0];
                    if ($5 instanceof $t.RightSquare) {
                      let tokens$2 = $4.tail;
                      let end = $4.head[1].byte_offset;
                      return new Ok(
                        new ParsedList(
                          $list.reverse(acc$1),
                          new None(),
                          tokens$2,
                          end + 1,
                        ),
                      );
                    } else if ($5 instanceof $t.DotDot) {
                      let $6 = $4.tail;
                      if ($6 instanceof $Empty) {
                        let tokens$2 = $6;
                        return $result.try$(
                          parser(tokens$2),
                          (_use0) => {
                            let rest;
                            let tokens$3;
                            rest = _use0[0];
                            tokens$3 = _use0[1];
                            return expect(
                              new $t.RightSquare(),
                              tokens$3,
                              (_use0, tokens) => {
                                let end;
                                end = _use0.byte_offset;
                                return new Ok(
                                  new ParsedList(
                                    $list.reverse(acc$1),
                                    new Some(rest),
                                    tokens,
                                    end + 1,
                                  ),
                                );
                              },
                            );
                          },
                        );
                      } else {
                        let $7 = $6.head[0];
                        if ($7 instanceof $t.RightSquare) {
                          let start = $4.head[1].byte_offset;
                          let close = $6.head;
                          let tokens$2 = $6.tail;
                          let end = $6.head[1].byte_offset;
                          if (discard instanceof Some) {
                            let discard$1 = discard[0];
                            let value = discard$1(new Span(start, start + 1));
                            let parsed_list = new ParsedList(
                              $list.reverse(acc$1),
                              new Some(value),
                              tokens$2,
                              end + 1,
                            );
                            return new Ok(parsed_list);
                          } else {
                            return unexpected_error(
                              listPrepend(close, tokens$2),
                            );
                          }
                        } else {
                          let tokens$2 = $6;
                          return $result.try$(
                            parser(tokens$2),
                            (_use0) => {
                              let rest;
                              let tokens$3;
                              rest = _use0[0];
                              tokens$3 = _use0[1];
                              return expect(
                                new $t.RightSquare(),
                                tokens$3,
                                (_use0, tokens) => {
                                  let end;
                                  end = _use0.byte_offset;
                                  return new Ok(
                                    new ParsedList(
                                      $list.reverse(acc$1),
                                      new Some(rest),
                                      tokens,
                                      end + 1,
                                    ),
                                  );
                                },
                              );
                            },
                          );
                        }
                      }
                    } else {
                      let tokens$2 = $4;
                      return list(parser, discard, acc$1, tokens$2);
                    }
                  }
                } else {
                  let other = $3;
                  let position = tokens$1.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              }
            },
          );
        }
      }
    } else if ($ instanceof $t.DotDot) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        let tokens$1 = $1;
        return $result.try$(
          parser(tokens$1),
          (_use0) => {
            let rest;
            let tokens$2;
            rest = _use0[0];
            tokens$2 = _use0[1];
            return expect(
              new $t.RightSquare(),
              tokens$2,
              (_use0, tokens) => {
                let end;
                end = _use0.byte_offset;
                return new Ok(
                  new ParsedList(
                    $list.reverse(acc),
                    new Some(rest),
                    tokens,
                    end + 1,
                  ),
                );
              },
            );
          },
        );
      } else {
        let $2 = $1.head[0];
        if ($2 instanceof $t.RightSquare) {
          let start = tokens.head[1].byte_offset;
          let close = $1.head;
          let tokens$1 = $1.tail;
          let end = $1.head[1].byte_offset;
          if (discard instanceof Some) {
            let discard$1 = discard[0];
            let value = discard$1(new Span(start, start + 1));
            let parsed_list = new ParsedList(
              $list.reverse(acc),
              new Some(value),
              tokens$1,
              end + 1,
            );
            return new Ok(parsed_list);
          } else {
            return unexpected_error(listPrepend(close, tokens$1));
          }
        } else {
          let tokens$1 = $1;
          return $result.try$(
            parser(tokens$1),
            (_use0) => {
              let rest;
              let tokens$2;
              rest = _use0[0];
              tokens$2 = _use0[1];
              return expect(
                new $t.RightSquare(),
                tokens$2,
                (_use0, tokens) => {
                  let end;
                  end = _use0.byte_offset;
                  return new Ok(
                    new ParsedList(
                      $list.reverse(acc),
                      new Some(rest),
                      tokens,
                      end + 1,
                    ),
                  );
                },
              );
            },
          );
        }
      }
    } else {
      return $result.try$(
        parser(tokens),
        (_use0) => {
          let element;
          let tokens$1;
          element = _use0[0];
          tokens$1 = _use0[1];
          let acc$1 = listPrepend(element, acc);
          if (tokens$1 instanceof $Empty) {
            return new Error(new UnexpectedEndOfInput());
          } else {
            let $1 = tokens$1.head[0];
            if ($1 instanceof $t.RightSquare) {
              let tokens$2 = tokens$1.tail;
              let end = tokens$1.head[1].byte_offset;
              return new Ok(
                new ParsedList(
                  $list.reverse(acc$1),
                  new None(),
                  tokens$2,
                  end + 1,
                ),
              );
            } else if ($1 instanceof $t.Comma) {
              let $2 = tokens$1.tail;
              if ($2 instanceof $Empty) {
                let tokens$2 = $2;
                return list(parser, discard, acc$1, tokens$2);
              } else {
                let $3 = $2.head[0];
                if ($3 instanceof $t.RightSquare) {
                  let tokens$2 = $2.tail;
                  let end = $2.head[1].byte_offset;
                  return new Ok(
                    new ParsedList(
                      $list.reverse(acc$1),
                      new None(),
                      tokens$2,
                      end + 1,
                    ),
                  );
                } else if ($3 instanceof $t.DotDot) {
                  let $4 = $2.tail;
                  if ($4 instanceof $Empty) {
                    let tokens$2 = $4;
                    return $result.try$(
                      parser(tokens$2),
                      (_use0) => {
                        let rest;
                        let tokens$3;
                        rest = _use0[0];
                        tokens$3 = _use0[1];
                        return expect(
                          new $t.RightSquare(),
                          tokens$3,
                          (_use0, tokens) => {
                            let end;
                            end = _use0.byte_offset;
                            return new Ok(
                              new ParsedList(
                                $list.reverse(acc$1),
                                new Some(rest),
                                tokens,
                                end + 1,
                              ),
                            );
                          },
                        );
                      },
                    );
                  } else {
                    let $5 = $4.head[0];
                    if ($5 instanceof $t.RightSquare) {
                      let start = $2.head[1].byte_offset;
                      let close = $4.head;
                      let tokens$2 = $4.tail;
                      let end = $4.head[1].byte_offset;
                      if (discard instanceof Some) {
                        let discard$1 = discard[0];
                        let value = discard$1(new Span(start, start + 1));
                        let parsed_list = new ParsedList(
                          $list.reverse(acc$1),
                          new Some(value),
                          tokens$2,
                          end + 1,
                        );
                        return new Ok(parsed_list);
                      } else {
                        return unexpected_error(listPrepend(close, tokens$2));
                      }
                    } else {
                      let tokens$2 = $4;
                      return $result.try$(
                        parser(tokens$2),
                        (_use0) => {
                          let rest;
                          let tokens$3;
                          rest = _use0[0];
                          tokens$3 = _use0[1];
                          return expect(
                            new $t.RightSquare(),
                            tokens$3,
                            (_use0, tokens) => {
                              let end;
                              end = _use0.byte_offset;
                              return new Ok(
                                new ParsedList(
                                  $list.reverse(acc$1),
                                  new Some(rest),
                                  tokens,
                                  end + 1,
                                ),
                              );
                            },
                          );
                        },
                      );
                    }
                  }
                } else {
                  let tokens$2 = $2;
                  return list(parser, discard, acc$1, tokens$2);
                }
              }
            } else {
              let other = $1;
              let position = tokens$1.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          }
        },
      );
    }
  }
}

function push_constant(module, attributes, constant) {
  return new Module(
    module.imports,
    module.custom_types,
    module.type_aliases,
    listPrepend(
      new Definition($list.reverse(attributes), constant),
      module.constants,
    ),
    module.functions,
  );
}

function push_function(module, attributes, function$) {
  return new Module(
    module.imports,
    module.custom_types,
    module.type_aliases,
    module.constants,
    listPrepend(
      new Definition($list.reverse(attributes), function$),
      module.functions,
    ),
  );
}

function push_custom_type(module, attributes, custom_type) {
  let custom_type$1 = new CustomType(
    custom_type.location,
    custom_type.name,
    custom_type.publicity,
    custom_type.opaque_,
    custom_type.parameters,
    $list.reverse(custom_type.variants),
  );
  return new Module(
    module.imports,
    listPrepend(
      new Definition($list.reverse(attributes), custom_type$1),
      module.custom_types,
    ),
    module.type_aliases,
    module.constants,
    module.functions,
  );
}

function push_type_alias(module, attributes, type_alias) {
  return new Module(
    module.imports,
    module.custom_types,
    listPrepend(
      new Definition($list.reverse(attributes), type_alias),
      module.type_aliases,
    ),
    module.constants,
    module.functions,
  );
}

function unqualified_imports(loop$types, loop$values, loop$tokens) {
  while (true) {
    let types = loop$types;
    let values = loop$values;
    let tokens = loop$tokens;
    if (tokens instanceof $Empty) {
      return new Error(new UnexpectedEndOfInput());
    } else {
      let $ = tokens.head[0];
      if ($ instanceof $t.Name) {
        let $1 = tokens.tail;
        if ($1 instanceof $Empty) {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            let $3 = $1.head[0];
            if ($3 instanceof $t.RightBrace) {
              let name$1 = $[0];
              let tokens$1 = $2;
              let end = $1.head[1].byte_offset;
              let import_ = new UnqualifiedImport(name$1, new None());
              return new Ok(
                new UnqualifiedImports(
                  $list.reverse(types),
                  $list.reverse(listPrepend(import_, values)),
                  end + 1,
                  tokens$1,
                ),
              );
            } else if ($3 instanceof $t.Comma) {
              let name$1 = $[0];
              let tokens$1 = $2;
              let import_ = new UnqualifiedImport(name$1, new None());
              loop$types = types;
              loop$values = listPrepend(import_, values);
              loop$tokens = tokens$1;
            } else {
              let other = $;
              let position = tokens.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          } else {
            let $3 = $2.tail;
            if ($3 instanceof $Empty) {
              let $4 = $1.head[0];
              if ($4 instanceof $t.RightBrace) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let end = $1.head[1].byte_offset;
                let import_ = new UnqualifiedImport(name$1, new None());
                return new Ok(
                  new UnqualifiedImports(
                    $list.reverse(types),
                    $list.reverse(listPrepend(import_, values)),
                    end + 1,
                    tokens$1,
                  ),
                );
              } else if ($4 instanceof $t.Comma) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let import_ = new UnqualifiedImport(name$1, new None());
                loop$types = types;
                loop$values = listPrepend(import_, values);
                loop$tokens = tokens$1;
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            } else {
              let $4 = $1.head[0];
              if ($4 instanceof $t.As) {
                let $5 = $2.head[0];
                if ($5 instanceof $t.Name) {
                  let $6 = $3.head[0];
                  if ($6 instanceof $t.RightBrace) {
                    let name$1 = $[0];
                    let tokens$1 = $3.tail;
                    let alias = $5[0];
                    let end = $3.head[1].byte_offset;
                    let import_ = new UnqualifiedImport(name$1, new Some(alias));
                    return new Ok(
                      new UnqualifiedImports(
                        $list.reverse(types),
                        $list.reverse(listPrepend(import_, values)),
                        end + 1,
                        tokens$1,
                      ),
                    );
                  } else if ($6 instanceof $t.Comma) {
                    let name$1 = $[0];
                    let tokens$1 = $3.tail;
                    let alias = $5[0];
                    let import_ = new UnqualifiedImport(name$1, new Some(alias));
                    loop$types = types;
                    loop$values = listPrepend(import_, values);
                    loop$tokens = tokens$1;
                  } else {
                    let other = $;
                    let position = tokens.head[1];
                    return new Error(new UnexpectedToken(other, position));
                  }
                } else {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              } else if ($4 instanceof $t.RightBrace) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let end = $1.head[1].byte_offset;
                let import_ = new UnqualifiedImport(name$1, new None());
                return new Ok(
                  new UnqualifiedImports(
                    $list.reverse(types),
                    $list.reverse(listPrepend(import_, values)),
                    end + 1,
                    tokens$1,
                  ),
                );
              } else if ($4 instanceof $t.Comma) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let import_ = new UnqualifiedImport(name$1, new None());
                loop$types = types;
                loop$values = listPrepend(import_, values);
                loop$tokens = tokens$1;
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          }
        }
      } else if ($ instanceof $t.UpperName) {
        let $1 = tokens.tail;
        if ($1 instanceof $Empty) {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            let $3 = $1.head[0];
            if ($3 instanceof $t.RightBrace) {
              let name$1 = $[0];
              let tokens$1 = $2;
              let end = $1.head[1].byte_offset;
              let import_ = new UnqualifiedImport(name$1, new None());
              return new Ok(
                new UnqualifiedImports(
                  $list.reverse(types),
                  $list.reverse(listPrepend(import_, values)),
                  end + 1,
                  tokens$1,
                ),
              );
            } else if ($3 instanceof $t.Comma) {
              let name$1 = $[0];
              let tokens$1 = $2;
              let import_ = new UnqualifiedImport(name$1, new None());
              loop$types = types;
              loop$values = listPrepend(import_, values);
              loop$tokens = tokens$1;
            } else {
              let other = $;
              let position = tokens.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          } else {
            let $3 = $2.tail;
            if ($3 instanceof $Empty) {
              let $4 = $1.head[0];
              if ($4 instanceof $t.RightBrace) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let end = $1.head[1].byte_offset;
                let import_ = new UnqualifiedImport(name$1, new None());
                return new Ok(
                  new UnqualifiedImports(
                    $list.reverse(types),
                    $list.reverse(listPrepend(import_, values)),
                    end + 1,
                    tokens$1,
                  ),
                );
              } else if ($4 instanceof $t.Comma) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let import_ = new UnqualifiedImport(name$1, new None());
                loop$types = types;
                loop$values = listPrepend(import_, values);
                loop$tokens = tokens$1;
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            } else {
              let $4 = $1.head[0];
              if ($4 instanceof $t.As) {
                let $5 = $2.head[0];
                if ($5 instanceof $t.UpperName) {
                  let $6 = $3.head[0];
                  if ($6 instanceof $t.RightBrace) {
                    let name$1 = $[0];
                    let tokens$1 = $3.tail;
                    let alias = $5[0];
                    let end = $3.head[1].byte_offset;
                    let import_ = new UnqualifiedImport(name$1, new Some(alias));
                    return new Ok(
                      new UnqualifiedImports(
                        $list.reverse(types),
                        $list.reverse(listPrepend(import_, values)),
                        end + 1,
                        tokens$1,
                      ),
                    );
                  } else if ($6 instanceof $t.Comma) {
                    let name$1 = $[0];
                    let tokens$1 = $3.tail;
                    let alias = $5[0];
                    let import_ = new UnqualifiedImport(name$1, new Some(alias));
                    loop$types = types;
                    loop$values = listPrepend(import_, values);
                    loop$tokens = tokens$1;
                  } else {
                    let other = $;
                    let position = tokens.head[1];
                    return new Error(new UnexpectedToken(other, position));
                  }
                } else {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              } else if ($4 instanceof $t.RightBrace) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let end = $1.head[1].byte_offset;
                let import_ = new UnqualifiedImport(name$1, new None());
                return new Ok(
                  new UnqualifiedImports(
                    $list.reverse(types),
                    $list.reverse(listPrepend(import_, values)),
                    end + 1,
                    tokens$1,
                  ),
                );
              } else if ($4 instanceof $t.Comma) {
                let name$1 = $[0];
                let tokens$1 = $2;
                let import_ = new UnqualifiedImport(name$1, new None());
                loop$types = types;
                loop$values = listPrepend(import_, values);
                loop$tokens = tokens$1;
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          }
        }
      } else if ($ instanceof $t.Type) {
        let $1 = tokens.tail;
        if ($1 instanceof $Empty) {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            let other = $;
            let position = tokens.head[1];
            return new Error(new UnexpectedToken(other, position));
          } else {
            let $3 = $2.tail;
            if ($3 instanceof $Empty) {
              let $4 = $1.head[0];
              if ($4 instanceof $t.UpperName) {
                let $5 = $2.head[0];
                if ($5 instanceof $t.RightBrace) {
                  let tokens$1 = $3;
                  let name$1 = $4[0];
                  let end = $2.head[1].byte_offset;
                  let import_ = new UnqualifiedImport(name$1, new None());
                  return new Ok(
                    new UnqualifiedImports(
                      $list.reverse(listPrepend(import_, types)),
                      $list.reverse(values),
                      end + 1,
                      tokens$1,
                    ),
                  );
                } else if ($5 instanceof $t.Comma) {
                  let tokens$1 = $3;
                  let name$1 = $4[0];
                  let import_ = new UnqualifiedImport(name$1, new None());
                  loop$types = listPrepend(import_, types);
                  loop$values = values;
                  loop$tokens = tokens$1;
                } else {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            } else {
              let $4 = $3.tail;
              if ($4 instanceof $Empty) {
                let $5 = $1.head[0];
                if ($5 instanceof $t.UpperName) {
                  let $6 = $2.head[0];
                  if ($6 instanceof $t.RightBrace) {
                    let tokens$1 = $3;
                    let name$1 = $5[0];
                    let end = $2.head[1].byte_offset;
                    let import_ = new UnqualifiedImport(name$1, new None());
                    return new Ok(
                      new UnqualifiedImports(
                        $list.reverse(listPrepend(import_, types)),
                        $list.reverse(values),
                        end + 1,
                        tokens$1,
                      ),
                    );
                  } else if ($6 instanceof $t.Comma) {
                    let tokens$1 = $3;
                    let name$1 = $5[0];
                    let import_ = new UnqualifiedImport(name$1, new None());
                    loop$types = listPrepend(import_, types);
                    loop$values = values;
                    loop$tokens = tokens$1;
                  } else {
                    let other = $;
                    let position = tokens.head[1];
                    return new Error(new UnexpectedToken(other, position));
                  }
                } else {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              } else {
                let $5 = $1.head[0];
                if ($5 instanceof $t.UpperName) {
                  let $6 = $2.head[0];
                  if ($6 instanceof $t.As) {
                    let $7 = $3.head[0];
                    if ($7 instanceof $t.UpperName) {
                      let $8 = $4.head[0];
                      if ($8 instanceof $t.RightBrace) {
                        let tokens$1 = $4.tail;
                        let name$1 = $5[0];
                        let alias = $7[0];
                        let end = $4.head[1].byte_offset;
                        let import_ = new UnqualifiedImport(
                          name$1,
                          new Some(alias),
                        );
                        return new Ok(
                          new UnqualifiedImports(
                            $list.reverse(listPrepend(import_, types)),
                            $list.reverse(values),
                            end + 1,
                            tokens$1,
                          ),
                        );
                      } else if ($8 instanceof $t.Comma) {
                        let tokens$1 = $4.tail;
                        let name$1 = $5[0];
                        let alias = $7[0];
                        let import_ = new UnqualifiedImport(
                          name$1,
                          new Some(alias),
                        );
                        loop$types = listPrepend(import_, types);
                        loop$values = values;
                        loop$tokens = tokens$1;
                      } else {
                        let other = $;
                        let position = tokens.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    } else {
                      let other = $;
                      let position = tokens.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  } else if ($6 instanceof $t.RightBrace) {
                    let tokens$1 = $3;
                    let name$1 = $5[0];
                    let end = $2.head[1].byte_offset;
                    let import_ = new UnqualifiedImport(name$1, new None());
                    return new Ok(
                      new UnqualifiedImports(
                        $list.reverse(listPrepend(import_, types)),
                        $list.reverse(values),
                        end + 1,
                        tokens$1,
                      ),
                    );
                  } else if ($6 instanceof $t.Comma) {
                    let tokens$1 = $3;
                    let name$1 = $5[0];
                    let import_ = new UnqualifiedImport(name$1, new None());
                    loop$types = listPrepend(import_, types);
                    loop$values = values;
                    loop$tokens = tokens$1;
                  } else {
                    let other = $;
                    let position = tokens.head[1];
                    return new Error(new UnexpectedToken(other, position));
                  }
                } else {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              }
            }
          }
        }
      } else if ($ instanceof $t.RightBrace) {
        let tokens$1 = tokens.tail;
        let end = tokens.head[1].byte_offset;
        return new Ok(
          new UnqualifiedImports(
            $list.reverse(types),
            $list.reverse(values),
            end + 1,
            tokens$1,
          ),
        );
      } else {
        let other = $;
        let position = tokens.head[1];
        return new Error(new UnexpectedToken(other, position));
      }
    }
  }
}

function optional_unqualified_imports(tokens, end) {
  if (tokens instanceof $Empty) {
    return new Ok(new UnqualifiedImports(toList([]), toList([]), end, tokens));
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      return new Ok(new UnqualifiedImports(toList([]), toList([]), end, tokens));
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Dot) {
        let $2 = $.head[0];
        if ($2 instanceof $t.LeftBrace) {
          let tokens$1 = $.tail;
          return unqualified_imports(toList([]), toList([]), tokens$1);
        } else {
          return new Ok(
            new UnqualifiedImports(toList([]), toList([]), end, tokens),
          );
        }
      } else {
        return new Ok(
          new UnqualifiedImports(toList([]), toList([]), end, tokens),
        );
      }
    }
  }
}

function import_statement(module, attributes, tokens, start) {
  return $result.try$(
    module_name("", 0, tokens),
    (_use0) => {
      let module_name$1;
      let end;
      let tokens$1;
      module_name$1 = _use0[0];
      end = _use0[1];
      tokens$1 = _use0[2];
      return $result.try$(
        optional_unqualified_imports(tokens$1, end),
        (_use0) => {
          let ts;
          let vs;
          let end$1;
          let tokens$2;
          ts = _use0.types;
          vs = _use0.values;
          end$1 = _use0.end;
          tokens$2 = _use0.remaining_tokens;
          let $ = optional_module_alias(tokens$2, end$1);
          let alias;
          let end$2;
          let tokens$3;
          alias = $[0];
          end$2 = $[1];
          tokens$3 = $[2];
          let span = new Span(start, end$2);
          let import_ = new Import(span, module_name$1, alias, ts, vs);
          let definition = new Definition($list.reverse(attributes), import_);
          let module$1 = new Module(
            listPrepend(definition, module.imports),
            module.custom_types,
            module.type_aliases,
            module.constants,
            module.functions,
          );
          return new Ok([module$1, tokens$3]);
        },
      );
    },
  );
}

function bit_string_segment_options(parser, options, tokens) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Error(new UnexpectedEndOfInput());
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.Name) {
          let $1 = tokens.tail;
          if ($1 instanceof $Empty) {
            let $2 = $[0];
            if ($2 === "bytes") {
              let tokens$1 = $1;
              return new Ok([new BytesOption(), tokens$1]);
            } else if ($2 === "binary") {
              let tokens$1 = $1;
              return new Ok([new BytesOption(), tokens$1]);
            } else if ($2 === "int") {
              let tokens$1 = $1;
              return new Ok([new IntOption(), tokens$1]);
            } else if ($2 === "float") {
              let tokens$1 = $1;
              return new Ok([new FloatOption(), tokens$1]);
            } else if ($2 === "bits") {
              let tokens$1 = $1;
              return new Ok([new BitsOption(), tokens$1]);
            } else if ($2 === "bit_string") {
              let tokens$1 = $1;
              return new Ok([new BitsOption(), tokens$1]);
            } else if ($2 === "utf8") {
              let tokens$1 = $1;
              return new Ok([new Utf8Option(), tokens$1]);
            } else if ($2 === "utf16") {
              let tokens$1 = $1;
              return new Ok([new Utf16Option(), tokens$1]);
            } else if ($2 === "utf32") {
              let tokens$1 = $1;
              return new Ok([new Utf32Option(), tokens$1]);
            } else if ($2 === "utf8_codepoint") {
              let tokens$1 = $1;
              return new Ok([new Utf8CodepointOption(), tokens$1]);
            } else if ($2 === "utf16_codepoint") {
              let tokens$1 = $1;
              return new Ok([new Utf16CodepointOption(), tokens$1]);
            } else if ($2 === "utf32_codepoint") {
              let tokens$1 = $1;
              return new Ok([new Utf32CodepointOption(), tokens$1]);
            } else if ($2 === "signed") {
              let tokens$1 = $1;
              return new Ok([new SignedOption(), tokens$1]);
            } else if ($2 === "unsigned") {
              let tokens$1 = $1;
              return new Ok([new UnsignedOption(), tokens$1]);
            } else if ($2 === "big") {
              let tokens$1 = $1;
              return new Ok([new BigOption(), tokens$1]);
            } else if ($2 === "little") {
              let tokens$1 = $1;
              return new Ok([new LittleOption(), tokens$1]);
            } else if ($2 === "native") {
              let tokens$1 = $1;
              return new Ok([new NativeOption(), tokens$1]);
            } else {
              let other = $;
              let position = tokens.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          } else {
            let $2 = $1.head[0];
            if ($2 instanceof $t.LeftParen) {
              let $3 = $[0];
              if ($3 === "size") {
                let tokens$1 = $1.tail;
                return $result.try$(
                  parser(tokens$1),
                  (_use0) => {
                    let value;
                    let tokens$2;
                    value = _use0[0];
                    tokens$2 = _use0[1];
                    return expect(
                      new $t.RightParen(),
                      tokens$2,
                      (_, tokens) => {
                        return new Ok([new SizeValueOption(value), tokens]);
                      },
                    );
                  },
                );
              } else if ($3 === "unit") {
                let $4 = $1.tail;
                if ($4 instanceof $Empty) {
                  let other = $;
                  let position = tokens.head[1];
                  return new Error(new UnexpectedToken(other, position));
                } else {
                  let $5 = $4.tail;
                  if ($5 instanceof $Empty) {
                    let other = $;
                    let position = tokens.head[1];
                    return new Error(new UnexpectedToken(other, position));
                  } else {
                    let $6 = $4.head[0];
                    if ($6 instanceof $t.Int) {
                      let $7 = $5.head[0];
                      if ($7 instanceof $t.RightParen) {
                        let position = tokens.head[1];
                        let tokens$1 = $5.tail;
                        let i = $6[0];
                        let $8 = $int.parse(i);
                        if ($8 instanceof Ok) {
                          let i$1 = $8[0];
                          return new Ok([new UnitOption(i$1), tokens$1]);
                        } else {
                          return new Error(
                            new UnexpectedToken(new $t.Int(i), position),
                          );
                        }
                      } else {
                        let other = $;
                        let position = tokens.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    } else {
                      let other = $;
                      let position = tokens.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                }
              } else if ($3 === "bytes") {
                let tokens$1 = $1;
                return new Ok([new BytesOption(), tokens$1]);
              } else if ($3 === "binary") {
                let tokens$1 = $1;
                return new Ok([new BytesOption(), tokens$1]);
              } else if ($3 === "int") {
                let tokens$1 = $1;
                return new Ok([new IntOption(), tokens$1]);
              } else if ($3 === "float") {
                let tokens$1 = $1;
                return new Ok([new FloatOption(), tokens$1]);
              } else if ($3 === "bits") {
                let tokens$1 = $1;
                return new Ok([new BitsOption(), tokens$1]);
              } else if ($3 === "bit_string") {
                let tokens$1 = $1;
                return new Ok([new BitsOption(), tokens$1]);
              } else if ($3 === "utf8") {
                let tokens$1 = $1;
                return new Ok([new Utf8Option(), tokens$1]);
              } else if ($3 === "utf16") {
                let tokens$1 = $1;
                return new Ok([new Utf16Option(), tokens$1]);
              } else if ($3 === "utf32") {
                let tokens$1 = $1;
                return new Ok([new Utf32Option(), tokens$1]);
              } else if ($3 === "utf8_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf8CodepointOption(), tokens$1]);
              } else if ($3 === "utf16_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf16CodepointOption(), tokens$1]);
              } else if ($3 === "utf32_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf32CodepointOption(), tokens$1]);
              } else if ($3 === "signed") {
                let tokens$1 = $1;
                return new Ok([new SignedOption(), tokens$1]);
              } else if ($3 === "unsigned") {
                let tokens$1 = $1;
                return new Ok([new UnsignedOption(), tokens$1]);
              } else if ($3 === "big") {
                let tokens$1 = $1;
                return new Ok([new BigOption(), tokens$1]);
              } else if ($3 === "little") {
                let tokens$1 = $1;
                return new Ok([new LittleOption(), tokens$1]);
              } else if ($3 === "native") {
                let tokens$1 = $1;
                return new Ok([new NativeOption(), tokens$1]);
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            } else {
              let $3 = $[0];
              if ($3 === "bytes") {
                let tokens$1 = $1;
                return new Ok([new BytesOption(), tokens$1]);
              } else if ($3 === "binary") {
                let tokens$1 = $1;
                return new Ok([new BytesOption(), tokens$1]);
              } else if ($3 === "int") {
                let tokens$1 = $1;
                return new Ok([new IntOption(), tokens$1]);
              } else if ($3 === "float") {
                let tokens$1 = $1;
                return new Ok([new FloatOption(), tokens$1]);
              } else if ($3 === "bits") {
                let tokens$1 = $1;
                return new Ok([new BitsOption(), tokens$1]);
              } else if ($3 === "bit_string") {
                let tokens$1 = $1;
                return new Ok([new BitsOption(), tokens$1]);
              } else if ($3 === "utf8") {
                let tokens$1 = $1;
                return new Ok([new Utf8Option(), tokens$1]);
              } else if ($3 === "utf16") {
                let tokens$1 = $1;
                return new Ok([new Utf16Option(), tokens$1]);
              } else if ($3 === "utf32") {
                let tokens$1 = $1;
                return new Ok([new Utf32Option(), tokens$1]);
              } else if ($3 === "utf8_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf8CodepointOption(), tokens$1]);
              } else if ($3 === "utf16_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf16CodepointOption(), tokens$1]);
              } else if ($3 === "utf32_codepoint") {
                let tokens$1 = $1;
                return new Ok([new Utf32CodepointOption(), tokens$1]);
              } else if ($3 === "signed") {
                let tokens$1 = $1;
                return new Ok([new SignedOption(), tokens$1]);
              } else if ($3 === "unsigned") {
                let tokens$1 = $1;
                return new Ok([new UnsignedOption(), tokens$1]);
              } else if ($3 === "big") {
                let tokens$1 = $1;
                return new Ok([new BigOption(), tokens$1]);
              } else if ($3 === "little") {
                let tokens$1 = $1;
                return new Ok([new LittleOption(), tokens$1]);
              } else if ($3 === "native") {
                let tokens$1 = $1;
                return new Ok([new NativeOption(), tokens$1]);
              } else {
                let other = $;
                let position = tokens.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          }
        } else if ($ instanceof $t.Int) {
          let tokens$1 = tokens.tail;
          let position = tokens.head[1];
          let i = $[0];
          let $1 = $int.parse(i);
          if ($1 instanceof Ok) {
            let i$1 = $1[0];
            return new Ok([new SizeOption(i$1), tokens$1]);
          } else {
            return new Error(new UnexpectedToken(new $t.Int(i), position));
          }
        } else {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        }
      }
    })(),
    (_use0) => {
      let option;
      let tokens$1;
      option = _use0[0];
      tokens$1 = _use0[1];
      let options$1 = listPrepend(option, options);
      if (tokens$1 instanceof $Empty) {
        return new Ok([$list.reverse(options$1), tokens$1]);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.Minus) {
          let tokens$2 = tokens$1.tail;
          return bit_string_segment_options(parser, options$1, tokens$2);
        } else {
          return new Ok([$list.reverse(options$1), tokens$1]);
        }
      }
    },
  );
}

function optional_bit_string_segment_options(parser, tokens) {
  if (tokens instanceof $Empty) {
    return new Ok([toList([]), tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Colon) {
      let tokens$1 = tokens.tail;
      return bit_string_segment_options(parser, toList([]), tokens$1);
    } else {
      return new Ok([toList([]), tokens]);
    }
  }
}

function bit_string_segment(parser, tokens) {
  return $result.try$(
    parser(tokens),
    (_use0) => {
      let value;
      let tokens$1;
      value = _use0[0];
      tokens$1 = _use0[1];
      let result = optional_bit_string_segment_options(parser, tokens$1);
      return $result.try$(
        result,
        (_use0) => {
          let options;
          let tokens$2;
          options = _use0[0];
          tokens$2 = _use0[1];
          return new Ok([[value, options], tokens$2]);
        },
      );
    },
  );
}

function delimited(acc, tokens, parser, delimeter) {
  return $result.try$(
    parser(tokens),
    (_use0) => {
      let t;
      let tokens$1;
      t = _use0[0];
      tokens$1 = _use0[1];
      let acc$1 = listPrepend(t, acc);
      if (tokens$1 instanceof $Empty) {
        return new Ok([$list.reverse(acc$1), tokens$1]);
      } else {
        let token = tokens$1.head[0];
        if (isEqual(token, delimeter)) {
          let tokens$2 = tokens$1.tail;
          return delimited(acc$1, tokens$2, parser, delimeter);
        } else {
          return new Ok([$list.reverse(acc$1), tokens$1]);
        }
      }
    },
  );
}

function comma_delimited(items, tokens, parser, final) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let token = tokens.head[0];
    if (isEqual(token, final)) {
      let tokens$1 = tokens.tail;
      let token_start = tokens.head[1].byte_offset;
      return new Ok(
        [
          $list.reverse(items),
          string_offset(token_start, $t.to_source(token)),
          tokens$1,
        ],
      );
    } else {
      return $result.try$(
        parser(tokens),
        (_use0) => {
          let element;
          let tokens$1;
          element = _use0[0];
          tokens$1 = _use0[1];
          if (tokens$1 instanceof $Empty) {
            return new Error(new UnexpectedEndOfInput());
          } else {
            let $ = tokens$1.head[0];
            if ($ instanceof $t.Comma) {
              let tokens$2 = tokens$1.tail;
              return comma_delimited(
                listPrepend(element, items),
                tokens$2,
                parser,
                final,
              );
            } else {
              let token = $;
              if (isEqual(token, final)) {
                let tokens$2 = tokens$1.tail;
                let token_start = tokens$1.head[1].byte_offset;
                let offset = string_offset(token_start, $t.to_source(token));
                return new Ok(
                  [$list.reverse(listPrepend(element, items)), offset, tokens$2],
                );
              } else {
                let other = $;
                let position = tokens$1.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          }
        },
      );
    }
  }
}

function name(tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Name) {
      let tokens$1 = tokens.tail;
      let name$1 = $[0];
      return new Ok([name$1, tokens$1]);
    } else {
      let token = $;
      let position = tokens.head[1];
      return new Error(new UnexpectedToken(token, position));
    }
  }
}

function field(tokens, parser) {
  if (tokens instanceof $Empty) {
    return $result.try$(
      parser(tokens),
      (_use0) => {
        let t;
        let tokens$1;
        t = _use0[0];
        tokens$1 = _use0[1];
        return new Ok([new UnlabelledField(t), tokens$1]);
      },
    );
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      return $result.try$(
        parser(tokens),
        (_use0) => {
          let t;
          let tokens$1;
          t = _use0[0];
          tokens$1 = _use0[1];
          return new Ok([new UnlabelledField(t), tokens$1]);
        },
      );
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Name) {
        let $2 = $.head[0];
        if ($2 instanceof $t.Colon) {
          let start = tokens.head[1];
          let tokens$1 = $.tail;
          let end = $.head[1];
          let name$1 = $1[0];
          if (tokens$1 instanceof $Empty) {
            return $result.try$(
              parser(tokens$1),
              (_use0) => {
                let t;
                let tokens$2;
                t = _use0[0];
                tokens$2 = _use0[1];
                return new Ok(
                  [
                    new LabelledField(
                      name$1,
                      new Span(start.byte_offset, end.byte_offset + 1),
                      t,
                    ),
                    tokens$2,
                  ],
                );
              },
            );
          } else {
            let $3 = tokens$1.head[0];
            if ($3 instanceof $t.RightParen) {
              return new Ok(
                [
                  new ShorthandField(
                    name$1,
                    new Span(start.byte_offset, end.byte_offset + 1),
                  ),
                  tokens$1,
                ],
              );
            } else if ($3 instanceof $t.Comma) {
              return new Ok(
                [
                  new ShorthandField(
                    name$1,
                    new Span(start.byte_offset, end.byte_offset + 1),
                  ),
                  tokens$1,
                ],
              );
            } else {
              return $result.try$(
                parser(tokens$1),
                (_use0) => {
                  let t;
                  let tokens$2;
                  t = _use0[0];
                  tokens$2 = _use0[1];
                  return new Ok(
                    [
                      new LabelledField(
                        name$1,
                        new Span(start.byte_offset, end.byte_offset + 1),
                        t,
                      ),
                      tokens$2,
                    ],
                  );
                },
              );
            }
          }
        } else {
          return $result.try$(
            parser(tokens),
            (_use0) => {
              let t;
              let tokens$1;
              t = _use0[0];
              tokens$1 = _use0[1];
              return new Ok([new UnlabelledField(t), tokens$1]);
            },
          );
        }
      } else {
        return $result.try$(
          parser(tokens),
          (_use0) => {
            let t;
            let tokens$1;
            t = _use0[0];
            tokens$1 = _use0[1];
            return new Ok([new UnlabelledField(t), tokens$1]);
          },
        );
      }
    }
  }
}

function statement(tokens) {
  if (tokens instanceof $Empty) {
    let tokens$1 = tokens;
    return $result.try$(
      expression(tokens$1),
      (_use0) => {
        let expression$1;
        let tokens$2;
        expression$1 = _use0[0];
        tokens$2 = _use0[1];
        return new Ok([new Expression(expression$1), tokens$2]);
      },
    );
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Assert) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        return assert_(tokens$1, start);
      } else if ($1 instanceof $t.Let) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        return assignment(new Let(), tokens$1, start);
      } else if ($1 instanceof $t.Use) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        return use_(tokens$1, start);
      } else {
        let tokens$1 = tokens;
        return $result.try$(
          expression(tokens$1),
          (_use0) => {
            let expression$1;
            let tokens$2;
            expression$1 = _use0[0];
            tokens$2 = _use0[1];
            return new Ok([new Expression(expression$1), tokens$2]);
          },
        );
      }
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Assert) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        return assert_(tokens$1, start);
      } else if ($1 instanceof $t.Let) {
        let $2 = $.head[0];
        if ($2 instanceof $t.Assert) {
          let start = tokens.head[1].byte_offset;
          let tokens$1 = $.tail;
          return assignment(new LetAssert(new None()), tokens$1, start);
        } else {
          let tokens$1 = $;
          let start = tokens.head[1].byte_offset;
          return assignment(new Let(), tokens$1, start);
        }
      } else if ($1 instanceof $t.Use) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        return use_(tokens$1, start);
      } else {
        let tokens$1 = tokens;
        return $result.try$(
          expression(tokens$1),
          (_use0) => {
            let expression$1;
            let tokens$2;
            expression$1 = _use0[0];
            tokens$2 = _use0[1];
            return new Ok([new Expression(expression$1), tokens$2]);
          },
        );
      }
    }
  }
}

function assert_(tokens, start) {
  return $result.try$(
    expression(tokens),
    (_use0) => {
      let subject;
      let tokens$1;
      subject = _use0[0];
      tokens$1 = _use0[1];
      if (tokens$1 instanceof $Empty) {
        let statement$1 = new Assert(
          new Span(start, subject.location.end),
          subject,
          new None(),
        );
        return new Ok([statement$1, tokens$1]);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.As) {
          let tokens$2 = tokens$1.tail;
          let $1 = expression(tokens$2);
          if ($1 instanceof Ok) {
            let message = $1[0][0];
            let tokens$3 = $1[0][1];
            let statement$1 = new Assert(
              new Span(start, message.location.end),
              subject,
              new Some(message),
            );
            return new Ok([statement$1, tokens$3]);
          } else {
            return $1;
          }
        } else {
          let statement$1 = new Assert(
            new Span(start, subject.location.end),
            subject,
            new None(),
          );
          return new Ok([statement$1, tokens$1]);
        }
      }
    },
  );
}

function expression(tokens) {
  return expression_loop(
    tokens,
    toList([]),
    toList([]),
    new RegularExpressionUnit(),
  );
}

function expression_loop(tokens, operators, values, context) {
  return $result.try$(
    expression_unit(tokens, context),
    (_use0) => {
      let expression$1;
      let tokens$1;
      expression$1 = _use0[0];
      tokens$1 = _use0[1];
      if (expression$1 instanceof Some) {
        let e = expression$1[0];
        let values$1 = listPrepend(e, values);
        let $ = pop_binary_operator(tokens$1);
        if ($ instanceof Ok) {
          let operator = $[0][0];
          let tokens$2 = $[0][1];
          let $1 = handle_operator(new Some(operator), operators, values$1);
          let $2 = $1[0];
          if ($2 instanceof Some) {
            let expression$2 = $2[0];
            return new Ok([expression$2, tokens$2]);
          } else {
            let operators$1 = $1[1];
            let values$2 = $1[2];
            return expression_loop(
              tokens$2,
              operators$1,
              values$2,
              (() => {
                if (operator instanceof Pipe) {
                  return new ExpressionUnitAfterPipe();
                } else {
                  return new RegularExpressionUnit();
                }
              })(),
            );
          }
        } else {
          let $1 = handle_operator(new None(), operators, values$1)[0];
          if ($1 instanceof Some) {
            let expression$2 = $1[0];
            return new Ok([expression$2, tokens$1]);
          } else {
            return unexpected_error(tokens$1);
          }
        }
      } else {
        return unexpected_error(tokens$1);
      }
    },
  );
}

function expression_unit(tokens, context) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Ok([new None(), tokens]);
      } else {
        let $ = tokens.tail;
        if ($ instanceof $Empty) {
          let $1 = tokens.head[0];
          if ($1 instanceof $t.Name) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let name$1 = $1[0];
            let span = span_from_string(start, name$1);
            return new Ok([new Some(new Variable(span, name$1)), tokens$1]);
          } else if ($1 instanceof $t.UpperName) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let name$1 = $1[0];
            return new Ok(
              [
                new Some(new Variable(span_from_string(start, name$1), name$1)),
                tokens$1,
              ],
            );
          } else if ($1 instanceof $t.Int) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let value = $1[0];
            let span = span_from_string(start, value);
            return new Ok([new Some(new Int(span, value)), tokens$1]);
          } else if ($1 instanceof $t.Float) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let value = $1[0];
            let span = span_from_string(start, value);
            return new Ok([new Some(new Float(span, value)), tokens$1]);
          } else if ($1 instanceof $t.String) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let value = $1[0];
            let span = new Span(start, string_offset(start, value) + 2);
            return new Ok([new Some(new String(span, value)), tokens$1]);
          } else if ($1 instanceof $t.Case) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            return case_(tokens$1, start);
          } else if ($1 instanceof $t.Echo) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let _block;
            if (context instanceof RegularExpressionUnit) {
              _block = $result.map(
                expression(tokens$1),
                (expression_and_tokens) => {
                  let expression$1;
                  let tokens$2;
                  expression$1 = expression_and_tokens[0];
                  tokens$2 = expression_and_tokens[1];
                  let span = new Span(start, expression$1.location.end);
                  return [span, new Some(expression$1), tokens$2];
                },
              );
            } else {
              let span = span_from_string(start, "echo");
              _block = new Ok([span, new None(), tokens$1]);
            }
            let result = _block;
            return $result.try$(
              result,
              (_use0) => {
                let span;
                let echo_expression;
                let tokens$2;
                span = _use0[0];
                echo_expression = _use0[1];
                tokens$2 = _use0[2];
                if (tokens$2 instanceof $Empty) {
                  return new Ok(
                    [
                      new Some(new Echo(span, echo_expression, new None())),
                      tokens$2,
                    ],
                  );
                } else {
                  let $2 = tokens$2.head[0];
                  if ($2 instanceof $t.As) {
                    let tokens$3 = tokens$2.tail;
                    return $result.map(
                      expression(tokens$3),
                      (_use0) => {
                        let message;
                        let tokens$4;
                        message = _use0[0];
                        tokens$4 = _use0[1];
                        let span$1 = new Span(span.start, message.location.end);
                        return [
                          new Some(
                            new Echo(span$1, echo_expression, new Some(message)),
                          ),
                          tokens$4,
                        ];
                      },
                    );
                  } else {
                    return new Ok(
                      [
                        new Some(new Echo(span, echo_expression, new None())),
                        tokens$2,
                      ],
                    );
                  }
                }
              },
            );
          } else if ($1 instanceof $t.Fn) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            return fn_(tokens$1, start);
          } else if ($1 instanceof $t.Panic) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            return todo_panic(
              tokens$1,
              (var0, var1) => { return new Panic(var0, var1); },
              start,
              "panic",
            );
          } else if ($1 instanceof $t.Todo) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            return todo_panic(
              tokens$1,
              (var0, var1) => { return new Todo(var0, var1); },
              start,
              "todo",
            );
          } else if ($1 instanceof $t.LeftBrace) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            return $result.map(
              statements(toList([]), tokens$1),
              (_use0) => {
                let statements$1;
                let end;
                let tokens$2;
                statements$1 = _use0[0];
                end = _use0[1];
                tokens$2 = _use0[2];
                return [
                  new Some(new Block(new Span(start, end), statements$1)),
                  tokens$2,
                ];
              },
            );
          } else if ($1 instanceof $t.LeftSquare) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let result = list(expression, new None(), toList([]), tokens$1);
            return $result.map(
              result,
              (_use0) => {
                let elements;
                let rest;
                let tokens$2;
                let end;
                elements = _use0.values;
                rest = _use0.spread;
                tokens$2 = _use0.remaining_tokens;
                end = _use0.end;
                return [
                  new Some(new List(new Span(start, end), elements, rest)),
                  tokens$2,
                ];
              },
            );
          } else if ($1 instanceof $t.Minus) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let unit = expression_unit(tokens$1, new RegularExpressionUnit());
            return $result.try$(
              unit,
              (_use0) => {
                let maybe_expression;
                let tokens$2;
                maybe_expression = _use0[0];
                tokens$2 = _use0[1];
                if (maybe_expression instanceof Some) {
                  let expression$1 = maybe_expression[0];
                  let span = new Span(start, expression$1.location.end);
                  return new Ok(
                    [new Some(new NegateInt(span, expression$1)), tokens$2],
                  );
                } else {
                  return unexpected_error(tokens$2);
                }
              },
            );
          } else if ($1 instanceof $t.Bang) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let unit = expression_unit(tokens$1, new RegularExpressionUnit());
            return $result.try$(
              unit,
              (_use0) => {
                let maybe_expression;
                let tokens$2;
                maybe_expression = _use0[0];
                tokens$2 = _use0[1];
                if (maybe_expression instanceof Some) {
                  let expression$1 = maybe_expression[0];
                  let span = new Span(start, expression$1.location.end);
                  return new Ok(
                    [new Some(new NegateBool(span, expression$1)), tokens$2],
                  );
                } else {
                  return unexpected_error(tokens$2);
                }
              },
            );
          } else if ($1 instanceof $t.LessLess) {
            let tokens$1 = $;
            let start = tokens.head[1].byte_offset;
            let parser = (_capture) => {
              return bit_string_segment(expression, _capture);
            };
            let result = comma_delimited(
              toList([]),
              tokens$1,
              parser,
              new $t.GreaterGreater(),
            );
            return $result.map(
              result,
              (_use0) => {
                let segments;
                let end;
                let tokens$2;
                segments = _use0[0];
                end = _use0[1];
                tokens$2 = _use0[2];
                return [
                  new Some(new BitString(new Span(start, end), segments)),
                  tokens$2,
                ];
              },
            );
          } else {
            return new Ok([new None(), tokens]);
          }
        } else {
          let $1 = $.tail;
          if ($1 instanceof $Empty) {
            let $2 = tokens.head[0];
            if ($2 instanceof $t.Name) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let name$1 = $2[0];
              let span = span_from_string(start, name$1);
              return new Ok([new Some(new Variable(span, name$1)), tokens$1]);
            } else if ($2 instanceof $t.UpperName) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let name$1 = $2[0];
              return new Ok(
                [
                  new Some(
                    new Variable(span_from_string(start, name$1), name$1),
                  ),
                  tokens$1,
                ],
              );
            } else if ($2 instanceof $t.Int) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let value = $2[0];
              let span = span_from_string(start, value);
              return new Ok([new Some(new Int(span, value)), tokens$1]);
            } else if ($2 instanceof $t.Float) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let value = $2[0];
              let span = span_from_string(start, value);
              return new Ok([new Some(new Float(span, value)), tokens$1]);
            } else if ($2 instanceof $t.String) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let value = $2[0];
              let span = new Span(start, string_offset(start, value) + 2);
              return new Ok([new Some(new String(span, value)), tokens$1]);
            } else if ($2 instanceof $t.Case) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              return case_(tokens$1, start);
            } else if ($2 instanceof $t.Echo) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let _block;
              if (context instanceof RegularExpressionUnit) {
                _block = $result.map(
                  expression(tokens$1),
                  (expression_and_tokens) => {
                    let expression$1;
                    let tokens$2;
                    expression$1 = expression_and_tokens[0];
                    tokens$2 = expression_and_tokens[1];
                    let span = new Span(start, expression$1.location.end);
                    return [span, new Some(expression$1), tokens$2];
                  },
                );
              } else {
                let span = span_from_string(start, "echo");
                _block = new Ok([span, new None(), tokens$1]);
              }
              let result = _block;
              return $result.try$(
                result,
                (_use0) => {
                  let span;
                  let echo_expression;
                  let tokens$2;
                  span = _use0[0];
                  echo_expression = _use0[1];
                  tokens$2 = _use0[2];
                  if (tokens$2 instanceof $Empty) {
                    return new Ok(
                      [
                        new Some(new Echo(span, echo_expression, new None())),
                        tokens$2,
                      ],
                    );
                  } else {
                    let $3 = tokens$2.head[0];
                    if ($3 instanceof $t.As) {
                      let tokens$3 = tokens$2.tail;
                      return $result.map(
                        expression(tokens$3),
                        (_use0) => {
                          let message;
                          let tokens$4;
                          message = _use0[0];
                          tokens$4 = _use0[1];
                          let span$1 = new Span(
                            span.start,
                            message.location.end,
                          );
                          return [
                            new Some(
                              new Echo(
                                span$1,
                                echo_expression,
                                new Some(message),
                              ),
                            ),
                            tokens$4,
                          ];
                        },
                      );
                    } else {
                      return new Ok(
                        [
                          new Some(new Echo(span, echo_expression, new None())),
                          tokens$2,
                        ],
                      );
                    }
                  }
                },
              );
            } else if ($2 instanceof $t.Fn) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              return fn_(tokens$1, start);
            } else if ($2 instanceof $t.Panic) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              return todo_panic(
                tokens$1,
                (var0, var1) => { return new Panic(var0, var1); },
                start,
                "panic",
              );
            } else if ($2 instanceof $t.Todo) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              return todo_panic(
                tokens$1,
                (var0, var1) => { return new Todo(var0, var1); },
                start,
                "todo",
              );
            } else if ($2 instanceof $t.LeftBrace) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              return $result.map(
                statements(toList([]), tokens$1),
                (_use0) => {
                  let statements$1;
                  let end;
                  let tokens$2;
                  statements$1 = _use0[0];
                  end = _use0[1];
                  tokens$2 = _use0[2];
                  return [
                    new Some(new Block(new Span(start, end), statements$1)),
                    tokens$2,
                  ];
                },
              );
            } else if ($2 instanceof $t.LeftSquare) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let result = list(expression, new None(), toList([]), tokens$1);
              return $result.map(
                result,
                (_use0) => {
                  let elements;
                  let rest;
                  let tokens$2;
                  let end;
                  elements = _use0.values;
                  rest = _use0.spread;
                  tokens$2 = _use0.remaining_tokens;
                  end = _use0.end;
                  return [
                    new Some(new List(new Span(start, end), elements, rest)),
                    tokens$2,
                  ];
                },
              );
            } else if ($2 instanceof $t.Minus) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let unit = expression_unit(tokens$1, new RegularExpressionUnit());
              return $result.try$(
                unit,
                (_use0) => {
                  let maybe_expression;
                  let tokens$2;
                  maybe_expression = _use0[0];
                  tokens$2 = _use0[1];
                  if (maybe_expression instanceof Some) {
                    let expression$1 = maybe_expression[0];
                    let span = new Span(start, expression$1.location.end);
                    return new Ok(
                      [new Some(new NegateInt(span, expression$1)), tokens$2],
                    );
                  } else {
                    return unexpected_error(tokens$2);
                  }
                },
              );
            } else if ($2 instanceof $t.Hash) {
              let $3 = $.head[0];
              if ($3 instanceof $t.LeftParen) {
                let start = tokens.head[1].byte_offset;
                let tokens$1 = $1;
                let result = comma_delimited(
                  toList([]),
                  tokens$1,
                  expression,
                  new $t.RightParen(),
                );
                return $result.map(
                  result,
                  (_use0) => {
                    let expressions;
                    let end;
                    let tokens$2;
                    expressions = _use0[0];
                    end = _use0[1];
                    tokens$2 = _use0[2];
                    return [
                      new Some(new Tuple(new Span(start, end), expressions)),
                      tokens$2,
                    ];
                  },
                );
              } else {
                return new Ok([new None(), tokens]);
              }
            } else if ($2 instanceof $t.Bang) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let unit = expression_unit(tokens$1, new RegularExpressionUnit());
              return $result.try$(
                unit,
                (_use0) => {
                  let maybe_expression;
                  let tokens$2;
                  maybe_expression = _use0[0];
                  tokens$2 = _use0[1];
                  if (maybe_expression instanceof Some) {
                    let expression$1 = maybe_expression[0];
                    let span = new Span(start, expression$1.location.end);
                    return new Ok(
                      [new Some(new NegateBool(span, expression$1)), tokens$2],
                    );
                  } else {
                    return unexpected_error(tokens$2);
                  }
                },
              );
            } else if ($2 instanceof $t.LessLess) {
              let tokens$1 = $;
              let start = tokens.head[1].byte_offset;
              let parser = (_capture) => {
                return bit_string_segment(expression, _capture);
              };
              let result = comma_delimited(
                toList([]),
                tokens$1,
                parser,
                new $t.GreaterGreater(),
              );
              return $result.map(
                result,
                (_use0) => {
                  let segments;
                  let end;
                  let tokens$2;
                  segments = _use0[0];
                  end = _use0[1];
                  tokens$2 = _use0[2];
                  return [
                    new Some(new BitString(new Span(start, end), segments)),
                    tokens$2,
                  ];
                },
              );
            } else {
              return new Ok([new None(), tokens]);
            }
          } else {
            let $2 = $1.tail;
            if ($2 instanceof $Empty) {
              let $3 = tokens.head[0];
              if ($3 instanceof $t.Name) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let name$1 = $3[0];
                let span = span_from_string(start, name$1);
                return new Ok([new Some(new Variable(span, name$1)), tokens$1]);
              } else if ($3 instanceof $t.UpperName) {
                let $4 = $.head[0];
                if ($4 instanceof $t.LeftParen) {
                  let $5 = $1.head[0];
                  if ($5 instanceof $t.DotDot) {
                    let start = tokens.head[1].byte_offset;
                    let tokens$1 = $2;
                    let constructor = $3[0];
                    return record_update(
                      new None(),
                      constructor,
                      tokens$1,
                      start,
                    );
                  } else {
                    let tokens$1 = $;
                    let start = tokens.head[1].byte_offset;
                    let name$1 = $3[0];
                    return new Ok(
                      [
                        new Some(
                          new Variable(span_from_string(start, name$1), name$1),
                        ),
                        tokens$1,
                      ],
                    );
                  }
                } else {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let name$1 = $3[0];
                  return new Ok(
                    [
                      new Some(
                        new Variable(span_from_string(start, name$1), name$1),
                      ),
                      tokens$1,
                    ],
                  );
                }
              } else if ($3 instanceof $t.Int) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let value = $3[0];
                let span = span_from_string(start, value);
                return new Ok([new Some(new Int(span, value)), tokens$1]);
              } else if ($3 instanceof $t.Float) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let value = $3[0];
                let span = span_from_string(start, value);
                return new Ok([new Some(new Float(span, value)), tokens$1]);
              } else if ($3 instanceof $t.String) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let value = $3[0];
                let span = new Span(start, string_offset(start, value) + 2);
                return new Ok([new Some(new String(span, value)), tokens$1]);
              } else if ($3 instanceof $t.Case) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                return case_(tokens$1, start);
              } else if ($3 instanceof $t.Echo) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let _block;
                if (context instanceof RegularExpressionUnit) {
                  _block = $result.map(
                    expression(tokens$1),
                    (expression_and_tokens) => {
                      let expression$1;
                      let tokens$2;
                      expression$1 = expression_and_tokens[0];
                      tokens$2 = expression_and_tokens[1];
                      let span = new Span(start, expression$1.location.end);
                      return [span, new Some(expression$1), tokens$2];
                    },
                  );
                } else {
                  let span = span_from_string(start, "echo");
                  _block = new Ok([span, new None(), tokens$1]);
                }
                let result = _block;
                return $result.try$(
                  result,
                  (_use0) => {
                    let span;
                    let echo_expression;
                    let tokens$2;
                    span = _use0[0];
                    echo_expression = _use0[1];
                    tokens$2 = _use0[2];
                    if (tokens$2 instanceof $Empty) {
                      return new Ok(
                        [
                          new Some(new Echo(span, echo_expression, new None())),
                          tokens$2,
                        ],
                      );
                    } else {
                      let $4 = tokens$2.head[0];
                      if ($4 instanceof $t.As) {
                        let tokens$3 = tokens$2.tail;
                        return $result.map(
                          expression(tokens$3),
                          (_use0) => {
                            let message;
                            let tokens$4;
                            message = _use0[0];
                            tokens$4 = _use0[1];
                            let span$1 = new Span(
                              span.start,
                              message.location.end,
                            );
                            return [
                              new Some(
                                new Echo(
                                  span$1,
                                  echo_expression,
                                  new Some(message),
                                ),
                              ),
                              tokens$4,
                            ];
                          },
                        );
                      } else {
                        return new Ok(
                          [
                            new Some(
                              new Echo(span, echo_expression, new None()),
                            ),
                            tokens$2,
                          ],
                        );
                      }
                    }
                  },
                );
              } else if ($3 instanceof $t.Fn) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                return fn_(tokens$1, start);
              } else if ($3 instanceof $t.Panic) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                return todo_panic(
                  tokens$1,
                  (var0, var1) => { return new Panic(var0, var1); },
                  start,
                  "panic",
                );
              } else if ($3 instanceof $t.Todo) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                return todo_panic(
                  tokens$1,
                  (var0, var1) => { return new Todo(var0, var1); },
                  start,
                  "todo",
                );
              } else if ($3 instanceof $t.LeftBrace) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                return $result.map(
                  statements(toList([]), tokens$1),
                  (_use0) => {
                    let statements$1;
                    let end;
                    let tokens$2;
                    statements$1 = _use0[0];
                    end = _use0[1];
                    tokens$2 = _use0[2];
                    return [
                      new Some(new Block(new Span(start, end), statements$1)),
                      tokens$2,
                    ];
                  },
                );
              } else if ($3 instanceof $t.LeftSquare) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let result = list(expression, new None(), toList([]), tokens$1);
                return $result.map(
                  result,
                  (_use0) => {
                    let elements;
                    let rest;
                    let tokens$2;
                    let end;
                    elements = _use0.values;
                    rest = _use0.spread;
                    tokens$2 = _use0.remaining_tokens;
                    end = _use0.end;
                    return [
                      new Some(new List(new Span(start, end), elements, rest)),
                      tokens$2,
                    ];
                  },
                );
              } else if ($3 instanceof $t.Minus) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let unit = expression_unit(
                  tokens$1,
                  new RegularExpressionUnit(),
                );
                return $result.try$(
                  unit,
                  (_use0) => {
                    let maybe_expression;
                    let tokens$2;
                    maybe_expression = _use0[0];
                    tokens$2 = _use0[1];
                    if (maybe_expression instanceof Some) {
                      let expression$1 = maybe_expression[0];
                      let span = new Span(start, expression$1.location.end);
                      return new Ok(
                        [new Some(new NegateInt(span, expression$1)), tokens$2],
                      );
                    } else {
                      return unexpected_error(tokens$2);
                    }
                  },
                );
              } else if ($3 instanceof $t.Hash) {
                let $4 = $.head[0];
                if ($4 instanceof $t.LeftParen) {
                  let start = tokens.head[1].byte_offset;
                  let tokens$1 = $1;
                  let result = comma_delimited(
                    toList([]),
                    tokens$1,
                    expression,
                    new $t.RightParen(),
                  );
                  return $result.map(
                    result,
                    (_use0) => {
                      let expressions;
                      let end;
                      let tokens$2;
                      expressions = _use0[0];
                      end = _use0[1];
                      tokens$2 = _use0[2];
                      return [
                        new Some(new Tuple(new Span(start, end), expressions)),
                        tokens$2,
                      ];
                    },
                  );
                } else {
                  return new Ok([new None(), tokens]);
                }
              } else if ($3 instanceof $t.Bang) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let unit = expression_unit(
                  tokens$1,
                  new RegularExpressionUnit(),
                );
                return $result.try$(
                  unit,
                  (_use0) => {
                    let maybe_expression;
                    let tokens$2;
                    maybe_expression = _use0[0];
                    tokens$2 = _use0[1];
                    if (maybe_expression instanceof Some) {
                      let expression$1 = maybe_expression[0];
                      let span = new Span(start, expression$1.location.end);
                      return new Ok(
                        [new Some(new NegateBool(span, expression$1)), tokens$2],
                      );
                    } else {
                      return unexpected_error(tokens$2);
                    }
                  },
                );
              } else if ($3 instanceof $t.LessLess) {
                let tokens$1 = $;
                let start = tokens.head[1].byte_offset;
                let parser = (_capture) => {
                  return bit_string_segment(expression, _capture);
                };
                let result = comma_delimited(
                  toList([]),
                  tokens$1,
                  parser,
                  new $t.GreaterGreater(),
                );
                return $result.map(
                  result,
                  (_use0) => {
                    let segments;
                    let end;
                    let tokens$2;
                    segments = _use0[0];
                    end = _use0[1];
                    tokens$2 = _use0[2];
                    return [
                      new Some(new BitString(new Span(start, end), segments)),
                      tokens$2,
                    ];
                  },
                );
              } else {
                return new Ok([new None(), tokens]);
              }
            } else {
              let $3 = $2.tail;
              if ($3 instanceof $Empty) {
                let $4 = tokens.head[0];
                if ($4 instanceof $t.Name) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let name$1 = $4[0];
                  let span = span_from_string(start, name$1);
                  return new Ok(
                    [new Some(new Variable(span, name$1)), tokens$1],
                  );
                } else if ($4 instanceof $t.UpperName) {
                  let $5 = $.head[0];
                  if ($5 instanceof $t.LeftParen) {
                    let $6 = $1.head[0];
                    if ($6 instanceof $t.DotDot) {
                      let start = tokens.head[1].byte_offset;
                      let tokens$1 = $2;
                      let constructor = $4[0];
                      return record_update(
                        new None(),
                        constructor,
                        tokens$1,
                        start,
                      );
                    } else {
                      let tokens$1 = $;
                      let start = tokens.head[1].byte_offset;
                      let name$1 = $4[0];
                      return new Ok(
                        [
                          new Some(
                            new Variable(
                              span_from_string(start, name$1),
                              name$1,
                            ),
                          ),
                          tokens$1,
                        ],
                      );
                    }
                  } else {
                    let tokens$1 = $;
                    let start = tokens.head[1].byte_offset;
                    let name$1 = $4[0];
                    return new Ok(
                      [
                        new Some(
                          new Variable(span_from_string(start, name$1), name$1),
                        ),
                        tokens$1,
                      ],
                    );
                  }
                } else if ($4 instanceof $t.Int) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = span_from_string(start, value);
                  return new Ok([new Some(new Int(span, value)), tokens$1]);
                } else if ($4 instanceof $t.Float) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = span_from_string(start, value);
                  return new Ok([new Some(new Float(span, value)), tokens$1]);
                } else if ($4 instanceof $t.String) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = new Span(start, string_offset(start, value) + 2);
                  return new Ok([new Some(new String(span, value)), tokens$1]);
                } else if ($4 instanceof $t.Case) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return case_(tokens$1, start);
                } else if ($4 instanceof $t.Echo) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let _block;
                  if (context instanceof RegularExpressionUnit) {
                    _block = $result.map(
                      expression(tokens$1),
                      (expression_and_tokens) => {
                        let expression$1;
                        let tokens$2;
                        expression$1 = expression_and_tokens[0];
                        tokens$2 = expression_and_tokens[1];
                        let span = new Span(start, expression$1.location.end);
                        return [span, new Some(expression$1), tokens$2];
                      },
                    );
                  } else {
                    let span = span_from_string(start, "echo");
                    _block = new Ok([span, new None(), tokens$1]);
                  }
                  let result = _block;
                  return $result.try$(
                    result,
                    (_use0) => {
                      let span;
                      let echo_expression;
                      let tokens$2;
                      span = _use0[0];
                      echo_expression = _use0[1];
                      tokens$2 = _use0[2];
                      if (tokens$2 instanceof $Empty) {
                        return new Ok(
                          [
                            new Some(
                              new Echo(span, echo_expression, new None()),
                            ),
                            tokens$2,
                          ],
                        );
                      } else {
                        let $5 = tokens$2.head[0];
                        if ($5 instanceof $t.As) {
                          let tokens$3 = tokens$2.tail;
                          return $result.map(
                            expression(tokens$3),
                            (_use0) => {
                              let message;
                              let tokens$4;
                              message = _use0[0];
                              tokens$4 = _use0[1];
                              let span$1 = new Span(
                                span.start,
                                message.location.end,
                              );
                              return [
                                new Some(
                                  new Echo(
                                    span$1,
                                    echo_expression,
                                    new Some(message),
                                  ),
                                ),
                                tokens$4,
                              ];
                            },
                          );
                        } else {
                          return new Ok(
                            [
                              new Some(
                                new Echo(span, echo_expression, new None()),
                              ),
                              tokens$2,
                            ],
                          );
                        }
                      }
                    },
                  );
                } else if ($4 instanceof $t.Fn) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return fn_(tokens$1, start);
                } else if ($4 instanceof $t.Panic) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return todo_panic(
                    tokens$1,
                    (var0, var1) => { return new Panic(var0, var1); },
                    start,
                    "panic",
                  );
                } else if ($4 instanceof $t.Todo) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return todo_panic(
                    tokens$1,
                    (var0, var1) => { return new Todo(var0, var1); },
                    start,
                    "todo",
                  );
                } else if ($4 instanceof $t.LeftBrace) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return $result.map(
                    statements(toList([]), tokens$1),
                    (_use0) => {
                      let statements$1;
                      let end;
                      let tokens$2;
                      statements$1 = _use0[0];
                      end = _use0[1];
                      tokens$2 = _use0[2];
                      return [
                        new Some(new Block(new Span(start, end), statements$1)),
                        tokens$2,
                      ];
                    },
                  );
                } else if ($4 instanceof $t.LeftSquare) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let result = list(
                    expression,
                    new None(),
                    toList([]),
                    tokens$1,
                  );
                  return $result.map(
                    result,
                    (_use0) => {
                      let elements;
                      let rest;
                      let tokens$2;
                      let end;
                      elements = _use0.values;
                      rest = _use0.spread;
                      tokens$2 = _use0.remaining_tokens;
                      end = _use0.end;
                      return [
                        new Some(new List(new Span(start, end), elements, rest)),
                        tokens$2,
                      ];
                    },
                  );
                } else if ($4 instanceof $t.Minus) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let unit = expression_unit(
                    tokens$1,
                    new RegularExpressionUnit(),
                  );
                  return $result.try$(
                    unit,
                    (_use0) => {
                      let maybe_expression;
                      let tokens$2;
                      maybe_expression = _use0[0];
                      tokens$2 = _use0[1];
                      if (maybe_expression instanceof Some) {
                        let expression$1 = maybe_expression[0];
                        let span = new Span(start, expression$1.location.end);
                        return new Ok(
                          [
                            new Some(new NegateInt(span, expression$1)),
                            tokens$2,
                          ],
                        );
                      } else {
                        return unexpected_error(tokens$2);
                      }
                    },
                  );
                } else if ($4 instanceof $t.Hash) {
                  let $5 = $.head[0];
                  if ($5 instanceof $t.LeftParen) {
                    let start = tokens.head[1].byte_offset;
                    let tokens$1 = $1;
                    let result = comma_delimited(
                      toList([]),
                      tokens$1,
                      expression,
                      new $t.RightParen(),
                    );
                    return $result.map(
                      result,
                      (_use0) => {
                        let expressions;
                        let end;
                        let tokens$2;
                        expressions = _use0[0];
                        end = _use0[1];
                        tokens$2 = _use0[2];
                        return [
                          new Some(new Tuple(new Span(start, end), expressions)),
                          tokens$2,
                        ];
                      },
                    );
                  } else {
                    return new Ok([new None(), tokens]);
                  }
                } else if ($4 instanceof $t.Bang) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let unit = expression_unit(
                    tokens$1,
                    new RegularExpressionUnit(),
                  );
                  return $result.try$(
                    unit,
                    (_use0) => {
                      let maybe_expression;
                      let tokens$2;
                      maybe_expression = _use0[0];
                      tokens$2 = _use0[1];
                      if (maybe_expression instanceof Some) {
                        let expression$1 = maybe_expression[0];
                        let span = new Span(start, expression$1.location.end);
                        return new Ok(
                          [
                            new Some(new NegateBool(span, expression$1)),
                            tokens$2,
                          ],
                        );
                      } else {
                        return unexpected_error(tokens$2);
                      }
                    },
                  );
                } else if ($4 instanceof $t.LessLess) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let parser = (_capture) => {
                    return bit_string_segment(expression, _capture);
                  };
                  let result = comma_delimited(
                    toList([]),
                    tokens$1,
                    parser,
                    new $t.GreaterGreater(),
                  );
                  return $result.map(
                    result,
                    (_use0) => {
                      let segments;
                      let end;
                      let tokens$2;
                      segments = _use0[0];
                      end = _use0[1];
                      tokens$2 = _use0[2];
                      return [
                        new Some(new BitString(new Span(start, end), segments)),
                        tokens$2,
                      ];
                    },
                  );
                } else {
                  return new Ok([new None(), tokens]);
                }
              } else {
                let $4 = tokens.head[0];
                if ($4 instanceof $t.Name) {
                  let $5 = $.head[0];
                  if ($5 instanceof $t.Dot) {
                    let $6 = $1.head[0];
                    if ($6 instanceof $t.UpperName) {
                      let $7 = $2.head[0];
                      if ($7 instanceof $t.LeftParen) {
                        let $8 = $3.head[0];
                        if ($8 instanceof $t.DotDot) {
                          let start = tokens.head[1].byte_offset;
                          let tokens$1 = $3.tail;
                          let module$1 = $4[0];
                          let constructor = $6[0];
                          return record_update(
                            new Some(module$1),
                            constructor,
                            tokens$1,
                            start,
                          );
                        } else {
                          let tokens$1 = $;
                          let start = tokens.head[1].byte_offset;
                          let name$1 = $4[0];
                          let span = span_from_string(start, name$1);
                          return new Ok(
                            [new Some(new Variable(span, name$1)), tokens$1],
                          );
                        }
                      } else {
                        let tokens$1 = $;
                        let start = tokens.head[1].byte_offset;
                        let name$1 = $4[0];
                        let span = span_from_string(start, name$1);
                        return new Ok(
                          [new Some(new Variable(span, name$1)), tokens$1],
                        );
                      }
                    } else {
                      let tokens$1 = $;
                      let start = tokens.head[1].byte_offset;
                      let name$1 = $4[0];
                      let span = span_from_string(start, name$1);
                      return new Ok(
                        [new Some(new Variable(span, name$1)), tokens$1],
                      );
                    }
                  } else {
                    let tokens$1 = $;
                    let start = tokens.head[1].byte_offset;
                    let name$1 = $4[0];
                    let span = span_from_string(start, name$1);
                    return new Ok(
                      [new Some(new Variable(span, name$1)), tokens$1],
                    );
                  }
                } else if ($4 instanceof $t.UpperName) {
                  let $5 = $.head[0];
                  if ($5 instanceof $t.LeftParen) {
                    let $6 = $1.head[0];
                    if ($6 instanceof $t.DotDot) {
                      let start = tokens.head[1].byte_offset;
                      let tokens$1 = $2;
                      let constructor = $4[0];
                      return record_update(
                        new None(),
                        constructor,
                        tokens$1,
                        start,
                      );
                    } else {
                      let tokens$1 = $;
                      let start = tokens.head[1].byte_offset;
                      let name$1 = $4[0];
                      return new Ok(
                        [
                          new Some(
                            new Variable(
                              span_from_string(start, name$1),
                              name$1,
                            ),
                          ),
                          tokens$1,
                        ],
                      );
                    }
                  } else {
                    let tokens$1 = $;
                    let start = tokens.head[1].byte_offset;
                    let name$1 = $4[0];
                    return new Ok(
                      [
                        new Some(
                          new Variable(span_from_string(start, name$1), name$1),
                        ),
                        tokens$1,
                      ],
                    );
                  }
                } else if ($4 instanceof $t.Int) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = span_from_string(start, value);
                  return new Ok([new Some(new Int(span, value)), tokens$1]);
                } else if ($4 instanceof $t.Float) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = span_from_string(start, value);
                  return new Ok([new Some(new Float(span, value)), tokens$1]);
                } else if ($4 instanceof $t.String) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let value = $4[0];
                  let span = new Span(start, string_offset(start, value) + 2);
                  return new Ok([new Some(new String(span, value)), tokens$1]);
                } else if ($4 instanceof $t.Case) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return case_(tokens$1, start);
                } else if ($4 instanceof $t.Echo) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let _block;
                  if (context instanceof RegularExpressionUnit) {
                    _block = $result.map(
                      expression(tokens$1),
                      (expression_and_tokens) => {
                        let expression$1;
                        let tokens$2;
                        expression$1 = expression_and_tokens[0];
                        tokens$2 = expression_and_tokens[1];
                        let span = new Span(start, expression$1.location.end);
                        return [span, new Some(expression$1), tokens$2];
                      },
                    );
                  } else {
                    let span = span_from_string(start, "echo");
                    _block = new Ok([span, new None(), tokens$1]);
                  }
                  let result = _block;
                  return $result.try$(
                    result,
                    (_use0) => {
                      let span;
                      let echo_expression;
                      let tokens$2;
                      span = _use0[0];
                      echo_expression = _use0[1];
                      tokens$2 = _use0[2];
                      if (tokens$2 instanceof $Empty) {
                        return new Ok(
                          [
                            new Some(
                              new Echo(span, echo_expression, new None()),
                            ),
                            tokens$2,
                          ],
                        );
                      } else {
                        let $5 = tokens$2.head[0];
                        if ($5 instanceof $t.As) {
                          let tokens$3 = tokens$2.tail;
                          return $result.map(
                            expression(tokens$3),
                            (_use0) => {
                              let message;
                              let tokens$4;
                              message = _use0[0];
                              tokens$4 = _use0[1];
                              let span$1 = new Span(
                                span.start,
                                message.location.end,
                              );
                              return [
                                new Some(
                                  new Echo(
                                    span$1,
                                    echo_expression,
                                    new Some(message),
                                  ),
                                ),
                                tokens$4,
                              ];
                            },
                          );
                        } else {
                          return new Ok(
                            [
                              new Some(
                                new Echo(span, echo_expression, new None()),
                              ),
                              tokens$2,
                            ],
                          );
                        }
                      }
                    },
                  );
                } else if ($4 instanceof $t.Fn) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return fn_(tokens$1, start);
                } else if ($4 instanceof $t.Panic) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return todo_panic(
                    tokens$1,
                    (var0, var1) => { return new Panic(var0, var1); },
                    start,
                    "panic",
                  );
                } else if ($4 instanceof $t.Todo) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return todo_panic(
                    tokens$1,
                    (var0, var1) => { return new Todo(var0, var1); },
                    start,
                    "todo",
                  );
                } else if ($4 instanceof $t.LeftBrace) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  return $result.map(
                    statements(toList([]), tokens$1),
                    (_use0) => {
                      let statements$1;
                      let end;
                      let tokens$2;
                      statements$1 = _use0[0];
                      end = _use0[1];
                      tokens$2 = _use0[2];
                      return [
                        new Some(new Block(new Span(start, end), statements$1)),
                        tokens$2,
                      ];
                    },
                  );
                } else if ($4 instanceof $t.LeftSquare) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let result = list(
                    expression,
                    new None(),
                    toList([]),
                    tokens$1,
                  );
                  return $result.map(
                    result,
                    (_use0) => {
                      let elements;
                      let rest;
                      let tokens$2;
                      let end;
                      elements = _use0.values;
                      rest = _use0.spread;
                      tokens$2 = _use0.remaining_tokens;
                      end = _use0.end;
                      return [
                        new Some(new List(new Span(start, end), elements, rest)),
                        tokens$2,
                      ];
                    },
                  );
                } else if ($4 instanceof $t.Minus) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let unit = expression_unit(
                    tokens$1,
                    new RegularExpressionUnit(),
                  );
                  return $result.try$(
                    unit,
                    (_use0) => {
                      let maybe_expression;
                      let tokens$2;
                      maybe_expression = _use0[0];
                      tokens$2 = _use0[1];
                      if (maybe_expression instanceof Some) {
                        let expression$1 = maybe_expression[0];
                        let span = new Span(start, expression$1.location.end);
                        return new Ok(
                          [
                            new Some(new NegateInt(span, expression$1)),
                            tokens$2,
                          ],
                        );
                      } else {
                        return unexpected_error(tokens$2);
                      }
                    },
                  );
                } else if ($4 instanceof $t.Hash) {
                  let $5 = $.head[0];
                  if ($5 instanceof $t.LeftParen) {
                    let start = tokens.head[1].byte_offset;
                    let tokens$1 = $1;
                    let result = comma_delimited(
                      toList([]),
                      tokens$1,
                      expression,
                      new $t.RightParen(),
                    );
                    return $result.map(
                      result,
                      (_use0) => {
                        let expressions;
                        let end;
                        let tokens$2;
                        expressions = _use0[0];
                        end = _use0[1];
                        tokens$2 = _use0[2];
                        return [
                          new Some(new Tuple(new Span(start, end), expressions)),
                          tokens$2,
                        ];
                      },
                    );
                  } else {
                    return new Ok([new None(), tokens]);
                  }
                } else if ($4 instanceof $t.Bang) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let unit = expression_unit(
                    tokens$1,
                    new RegularExpressionUnit(),
                  );
                  return $result.try$(
                    unit,
                    (_use0) => {
                      let maybe_expression;
                      let tokens$2;
                      maybe_expression = _use0[0];
                      tokens$2 = _use0[1];
                      if (maybe_expression instanceof Some) {
                        let expression$1 = maybe_expression[0];
                        let span = new Span(start, expression$1.location.end);
                        return new Ok(
                          [
                            new Some(new NegateBool(span, expression$1)),
                            tokens$2,
                          ],
                        );
                      } else {
                        return unexpected_error(tokens$2);
                      }
                    },
                  );
                } else if ($4 instanceof $t.LessLess) {
                  let tokens$1 = $;
                  let start = tokens.head[1].byte_offset;
                  let parser = (_capture) => {
                    return bit_string_segment(expression, _capture);
                  };
                  let result = comma_delimited(
                    toList([]),
                    tokens$1,
                    parser,
                    new $t.GreaterGreater(),
                  );
                  return $result.map(
                    result,
                    (_use0) => {
                      let segments;
                      let end;
                      let tokens$2;
                      segments = _use0[0];
                      end = _use0[1];
                      tokens$2 = _use0[2];
                      return [
                        new Some(new BitString(new Span(start, end), segments)),
                        tokens$2,
                      ];
                    },
                  );
                } else {
                  return new Ok([new None(), tokens]);
                }
              }
            }
          }
        }
      }
    })(),
    (_use0) => {
      let parsed;
      let tokens$1;
      parsed = _use0[0];
      tokens$1 = _use0[1];
      if (parsed instanceof Some) {
        let expression$1 = parsed[0];
        let $ = after_expression(expression$1, tokens$1);
        if ($ instanceof Ok) {
          let expression$2 = $[0][0];
          let tokens$2 = $[0][1];
          return new Ok([new Some(expression$2), tokens$2]);
        } else {
          return $;
        }
      } else {
        return new Ok([new None(), tokens$1]);
      }
    },
  );
}

function fn_(tokens, start) {
  return expect(
    new $t.LeftParen(),
    tokens,
    (_, tokens) => {
      let result = comma_delimited(
        toList([]),
        tokens,
        fn_parameter,
        new $t.RightParen(),
      );
      return $result.try$(
        result,
        (_use0) => {
          let parameters;
          let tokens$1;
          parameters = _use0[0];
          tokens$1 = _use0[2];
          return $result.try$(
            optional_return_annotation(0, tokens$1),
            (_use0) => {
              let return$;
              let tokens$2;
              return$ = _use0[0];
              tokens$2 = _use0[2];
              return expect(
                new $t.LeftBrace(),
                tokens$2,
                (_, tokens) => {
                  return $result.try$(
                    statements(toList([]), tokens),
                    (_use0) => {
                      let body;
                      let end;
                      let tokens$1;
                      body = _use0[0];
                      end = _use0[1];
                      tokens$1 = _use0[2];
                      return new Ok(
                        [
                          new Some(
                            new Fn(
                              new Span(start, end),
                              parameters,
                              return$,
                              body,
                            ),
                          ),
                          tokens$1,
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

function statements(acc, tokens) {
  if (tokens instanceof $Empty) {
    return $result.try$(
      statement(tokens),
      (_use0) => {
        let statement$1;
        let tokens$1;
        statement$1 = _use0[0];
        tokens$1 = _use0[1];
        return statements(listPrepend(statement$1, acc), tokens$1);
      },
    );
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.RightBrace) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      return new Ok([$list.reverse(acc), end + 1, tokens$1]);
    } else {
      return $result.try$(
        statement(tokens),
        (_use0) => {
          let statement$1;
          let tokens$1;
          statement$1 = _use0[0];
          tokens$1 = _use0[1];
          return statements(listPrepend(statement$1, acc), tokens$1);
        },
      );
    }
  }
}

function attribute(tokens) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Error(new UnexpectedEndOfInput());
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.Name) {
          let tokens$1 = tokens.tail;
          let name$1 = $[0];
          return new Ok([name$1, tokens$1]);
        } else {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        }
      }
    })(),
    (_use0) => {
      let name$1;
      let tokens$1;
      name$1 = _use0[0];
      tokens$1 = _use0[1];
      if (tokens$1 instanceof $Empty) {
        return new Ok([new Attribute(name$1, toList([])), tokens$1]);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.LeftParen) {
          let tokens$2 = tokens$1.tail;
          let result = comma_delimited(
            toList([]),
            tokens$2,
            expression,
            new $t.RightParen(),
          );
          return $result.try$(
            result,
            (_use0) => {
              let parameters;
              let tokens$3;
              parameters = _use0[0];
              tokens$3 = _use0[2];
              return new Ok([new Attribute(name$1, parameters), tokens$3]);
            },
          );
        } else {
          return new Ok([new Attribute(name$1, toList([])), tokens$1]);
        }
      }
    },
  );
}

function todo_panic(tokens, constructor, start, keyword_name) {
  if (tokens instanceof $Empty) {
    let span = span_from_string(start, keyword_name);
    let expression$1 = constructor(span, new None());
    return new Ok([new Some(expression$1), tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.As) {
      let tokens$1 = tokens.tail;
      return $result.try$(
        expression(tokens$1),
        (_use0) => {
          let reason;
          let tokens$2;
          reason = _use0[0];
          tokens$2 = _use0[1];
          let span = new Span(start, reason.location.end);
          let expression$1 = constructor(span, new Some(reason));
          return new Ok([new Some(expression$1), tokens$2]);
        },
      );
    } else {
      let span = span_from_string(start, keyword_name);
      let expression$1 = constructor(span, new None());
      return new Ok([new Some(expression$1), tokens]);
    }
  }
}

function record_update_field(tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      let other = tokens.head[0];
      let position = tokens.head[1];
      return new Error(new UnexpectedToken(other, position));
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Name) {
        let $2 = $.head[0];
        if ($2 instanceof $t.Colon) {
          let tokens$1 = $.tail;
          let name$1 = $1[0];
          if (tokens$1 instanceof $Empty) {
            return $result.try$(
              expression(tokens$1),
              (_use0) => {
                let expression$1;
                let tokens$2;
                expression$1 = _use0[0];
                tokens$2 = _use0[1];
                return new Ok(
                  [
                    new RecordUpdateField(name$1, new Some(expression$1)),
                    tokens$2,
                  ],
                );
              },
            );
          } else {
            let $3 = tokens$1.head[0];
            if ($3 instanceof $t.RightParen) {
              return new Ok(
                [new RecordUpdateField(name$1, new None()), tokens$1],
              );
            } else if ($3 instanceof $t.Comma) {
              return new Ok(
                [new RecordUpdateField(name$1, new None()), tokens$1],
              );
            } else {
              return $result.try$(
                expression(tokens$1),
                (_use0) => {
                  let expression$1;
                  let tokens$2;
                  expression$1 = _use0[0];
                  tokens$2 = _use0[1];
                  return new Ok(
                    [
                      new RecordUpdateField(name$1, new Some(expression$1)),
                      tokens$2,
                    ],
                  );
                },
              );
            }
          }
        } else {
          let other = $1;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        }
      } else {
        let other = $1;
        let position = tokens.head[1];
        return new Error(new UnexpectedToken(other, position));
      }
    }
  }
}

function record_update(module, constructor, tokens, start) {
  return $result.try$(
    expression(tokens),
    (_use0) => {
      let record;
      let tokens$1;
      record = _use0[0];
      tokens$1 = _use0[1];
      if (tokens$1 instanceof $Empty) {
        return new Ok([new None(), tokens$1]);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.RightParen) {
          let tokens$2 = tokens$1.tail;
          let end = tokens$1.head[1].byte_offset;
          let span = new Span(start, end + 1);
          let expression$1 = new RecordUpdate(
            span,
            module,
            constructor,
            record,
            toList([]),
          );
          return new Ok([new Some(expression$1), tokens$2]);
        } else if ($ instanceof $t.Comma) {
          let tokens$2 = tokens$1.tail;
          let result = comma_delimited(
            toList([]),
            tokens$2,
            record_update_field,
            new $t.RightParen(),
          );
          return $result.try$(
            result,
            (_use0) => {
              let fields;
              let end;
              let tokens$3;
              fields = _use0[0];
              end = _use0[1];
              tokens$3 = _use0[2];
              let span = new Span(start, end);
              let expression$1 = new RecordUpdate(
                span,
                module,
                constructor,
                record,
                fields,
              );
              return new Ok([new Some(expression$1), tokens$3]);
            },
          );
        } else {
          return new Ok([new None(), tokens$1]);
        }
      }
    },
  );
}

function case_subjects(subjects, tokens) {
  return $result.try$(
    expression(tokens),
    (_use0) => {
      let subject;
      let tokens$1;
      subject = _use0[0];
      tokens$1 = _use0[1];
      let subjects$1 = listPrepend(subject, subjects);
      if (tokens$1 instanceof $Empty) {
        return new Ok([$list.reverse(subjects$1), tokens$1]);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.Comma) {
          let tokens$2 = tokens$1.tail;
          return case_subjects(subjects$1, tokens$2);
        } else {
          return new Ok([$list.reverse(subjects$1), tokens$1]);
        }
      }
    },
  );
}

function optional_clause_guard(tokens) {
  if (tokens instanceof $Empty) {
    return new Ok([new None(), tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.If) {
      let tokens$1 = tokens.tail;
      return $result.try$(
        expression(tokens$1),
        (_use0) => {
          let expression$1;
          let tokens$2;
          expression$1 = _use0[0];
          tokens$2 = _use0[1];
          return new Ok([new Some(expression$1), tokens$2]);
        },
      );
    } else {
      return new Ok([new None(), tokens]);
    }
  }
}

function attributes(loop$accumulated_attributes, loop$tokens) {
  while (true) {
    let accumulated_attributes = loop$accumulated_attributes;
    let tokens = loop$tokens;
    if (tokens instanceof $Empty) {
      return new Ok([$list.reverse(accumulated_attributes), tokens]);
    } else {
      let $ = tokens.head[0];
      if ($ instanceof $t.At) {
        let tokens$1 = tokens.tail;
        let $1 = attribute(tokens$1);
        if ($1 instanceof Ok) {
          let attribute$1 = $1[0][0];
          let tokens$2 = $1[0][1];
          loop$accumulated_attributes = listPrepend(
            attribute$1,
            accumulated_attributes,
          );
          loop$tokens = tokens$2;
        } else {
          return $1;
        }
      } else {
        return new Ok([$list.reverse(accumulated_attributes), tokens]);
      }
    }
  }
}

function call(arguments$, function$, tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Name) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        return $result.try$(
          field(tokens, expression),
          (_use0) => {
            let argument;
            let tokens$1;
            argument = _use0[0];
            tokens$1 = _use0[1];
            let arguments$1 = listPrepend(argument, arguments$);
            if (tokens$1 instanceof $Empty) {
              return new Error(new UnexpectedEndOfInput());
            } else {
              let $2 = tokens$1.head[0];
              if ($2 instanceof $t.RightParen) {
                let tokens$2 = tokens$1.tail;
                let end = tokens$1.head[1].byte_offset;
                let span = new Span(function$.location.start, end + 1);
                let call$1 = new Call(
                  span,
                  function$,
                  $list.reverse(arguments$1),
                );
                return after_expression(call$1, tokens$2);
              } else if ($2 instanceof $t.Comma) {
                let tokens$2 = tokens$1.tail;
                return call(arguments$1, function$, tokens$2);
              } else {
                let other = $2;
                let position = tokens$1.head[1];
                return new Error(new UnexpectedToken(other, position));
              }
            }
          },
        );
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          return $result.try$(
            field(tokens, expression),
            (_use0) => {
              let argument;
              let tokens$1;
              argument = _use0[0];
              tokens$1 = _use0[1];
              let arguments$1 = listPrepend(argument, arguments$);
              if (tokens$1 instanceof $Empty) {
                return new Error(new UnexpectedEndOfInput());
              } else {
                let $3 = tokens$1.head[0];
                if ($3 instanceof $t.RightParen) {
                  let tokens$2 = tokens$1.tail;
                  let end = tokens$1.head[1].byte_offset;
                  let span = new Span(function$.location.start, end + 1);
                  let call$1 = new Call(
                    span,
                    function$,
                    $list.reverse(arguments$1),
                  );
                  return after_expression(call$1, tokens$2);
                } else if ($3 instanceof $t.Comma) {
                  let tokens$2 = tokens$1.tail;
                  return call(arguments$1, function$, tokens$2);
                } else {
                  let other = $3;
                  let position = tokens$1.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              }
            },
          );
        } else {
          let $3 = $2.tail;
          if ($3 instanceof $Empty) {
            let $4 = $1.head[0];
            if ($4 instanceof $t.Colon) {
              let $5 = $2.head[0];
              if ($5 instanceof $t.DiscardName) {
                let $6 = $5[0];
                if ($6 === "") {
                  let label = $[0];
                  let tokens$1 = $3;
                  return fn_capture(
                    new Some(label),
                    function$,
                    $list.reverse(arguments$),
                    toList([]),
                    tokens$1,
                  );
                } else {
                  return $result.try$(
                    field(tokens, expression),
                    (_use0) => {
                      let argument;
                      let tokens$1;
                      argument = _use0[0];
                      tokens$1 = _use0[1];
                      let arguments$1 = listPrepend(argument, arguments$);
                      if (tokens$1 instanceof $Empty) {
                        return new Error(new UnexpectedEndOfInput());
                      } else {
                        let $7 = tokens$1.head[0];
                        if ($7 instanceof $t.RightParen) {
                          let tokens$2 = tokens$1.tail;
                          let end = tokens$1.head[1].byte_offset;
                          let span = new Span(function$.location.start, end + 1);
                          let call$1 = new Call(
                            span,
                            function$,
                            $list.reverse(arguments$1),
                          );
                          return after_expression(call$1, tokens$2);
                        } else if ($7 instanceof $t.Comma) {
                          let tokens$2 = tokens$1.tail;
                          return call(arguments$1, function$, tokens$2);
                        } else {
                          let other = $7;
                          let position = tokens$1.head[1];
                          return new Error(new UnexpectedToken(other, position));
                        }
                      }
                    },
                  );
                }
              } else {
                return $result.try$(
                  field(tokens, expression),
                  (_use0) => {
                    let argument;
                    let tokens$1;
                    argument = _use0[0];
                    tokens$1 = _use0[1];
                    let arguments$1 = listPrepend(argument, arguments$);
                    if (tokens$1 instanceof $Empty) {
                      return new Error(new UnexpectedEndOfInput());
                    } else {
                      let $6 = tokens$1.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$2 = tokens$1.tail;
                        let end = tokens$1.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let call$1 = new Call(
                          span,
                          function$,
                          $list.reverse(arguments$1),
                        );
                        return after_expression(call$1, tokens$2);
                      } else if ($6 instanceof $t.Comma) {
                        let tokens$2 = tokens$1.tail;
                        return call(arguments$1, function$, tokens$2);
                      } else {
                        let other = $6;
                        let position = tokens$1.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    }
                  },
                );
              }
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          } else {
            let $4 = $3.tail;
            if ($4 instanceof $Empty) {
              let $5 = $1.head[0];
              if ($5 instanceof $t.Colon) {
                let $6 = $2.head[0];
                if ($6 instanceof $t.DiscardName) {
                  let $7 = $3.head[0];
                  if ($7 instanceof $t.RightParen) {
                    let $8 = $6[0];
                    if ($8 === "") {
                      let label = $[0];
                      let tokens$1 = $4;
                      let end = $3.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let capture = new FnCapture(
                        span,
                        new Some(label),
                        function$,
                        $list.reverse(arguments$),
                        toList([]),
                      );
                      return after_expression(capture, tokens$1);
                    } else {
                      return $result.try$(
                        field(tokens, expression),
                        (_use0) => {
                          let argument;
                          let tokens$1;
                          argument = _use0[0];
                          tokens$1 = _use0[1];
                          let arguments$1 = listPrepend(argument, arguments$);
                          if (tokens$1 instanceof $Empty) {
                            return new Error(new UnexpectedEndOfInput());
                          } else {
                            let $9 = tokens$1.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$2 = tokens$1.tail;
                              let end = tokens$1.head[1].byte_offset;
                              let span = new Span(
                                function$.location.start,
                                end + 1,
                              );
                              let call$1 = new Call(
                                span,
                                function$,
                                $list.reverse(arguments$1),
                              );
                              return after_expression(call$1, tokens$2);
                            } else if ($9 instanceof $t.Comma) {
                              let tokens$2 = tokens$1.tail;
                              return call(arguments$1, function$, tokens$2);
                            } else {
                              let other = $9;
                              let position = tokens$1.head[1];
                              return new Error(
                                new UnexpectedToken(other, position),
                              );
                            }
                          }
                        },
                      );
                    }
                  } else if ($7 instanceof $t.Comma) {
                    let $8 = $6[0];
                    if ($8 === "") {
                      let label = $[0];
                      let tokens$1 = $4;
                      return fn_capture(
                        new Some(label),
                        function$,
                        $list.reverse(arguments$),
                        toList([]),
                        tokens$1,
                      );
                    } else {
                      return $result.try$(
                        field(tokens, expression),
                        (_use0) => {
                          let argument;
                          let tokens$1;
                          argument = _use0[0];
                          tokens$1 = _use0[1];
                          let arguments$1 = listPrepend(argument, arguments$);
                          if (tokens$1 instanceof $Empty) {
                            return new Error(new UnexpectedEndOfInput());
                          } else {
                            let $9 = tokens$1.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$2 = tokens$1.tail;
                              let end = tokens$1.head[1].byte_offset;
                              let span = new Span(
                                function$.location.start,
                                end + 1,
                              );
                              let call$1 = new Call(
                                span,
                                function$,
                                $list.reverse(arguments$1),
                              );
                              return after_expression(call$1, tokens$2);
                            } else if ($9 instanceof $t.Comma) {
                              let tokens$2 = tokens$1.tail;
                              return call(arguments$1, function$, tokens$2);
                            } else {
                              let other = $9;
                              let position = tokens$1.head[1];
                              return new Error(
                                new UnexpectedToken(other, position),
                              );
                            }
                          }
                        },
                      );
                    }
                  } else {
                    let $8 = $6[0];
                    if ($8 === "") {
                      let label = $[0];
                      let tokens$1 = $3;
                      return fn_capture(
                        new Some(label),
                        function$,
                        $list.reverse(arguments$),
                        toList([]),
                        tokens$1,
                      );
                    } else {
                      return $result.try$(
                        field(tokens, expression),
                        (_use0) => {
                          let argument;
                          let tokens$1;
                          argument = _use0[0];
                          tokens$1 = _use0[1];
                          let arguments$1 = listPrepend(argument, arguments$);
                          if (tokens$1 instanceof $Empty) {
                            return new Error(new UnexpectedEndOfInput());
                          } else {
                            let $9 = tokens$1.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$2 = tokens$1.tail;
                              let end = tokens$1.head[1].byte_offset;
                              let span = new Span(
                                function$.location.start,
                                end + 1,
                              );
                              let call$1 = new Call(
                                span,
                                function$,
                                $list.reverse(arguments$1),
                              );
                              return after_expression(call$1, tokens$2);
                            } else if ($9 instanceof $t.Comma) {
                              let tokens$2 = tokens$1.tail;
                              return call(arguments$1, function$, tokens$2);
                            } else {
                              let other = $9;
                              let position = tokens$1.head[1];
                              return new Error(
                                new UnexpectedToken(other, position),
                              );
                            }
                          }
                        },
                      );
                    }
                  }
                } else {
                  return $result.try$(
                    field(tokens, expression),
                    (_use0) => {
                      let argument;
                      let tokens$1;
                      argument = _use0[0];
                      tokens$1 = _use0[1];
                      let arguments$1 = listPrepend(argument, arguments$);
                      if (tokens$1 instanceof $Empty) {
                        return new Error(new UnexpectedEndOfInput());
                      } else {
                        let $7 = tokens$1.head[0];
                        if ($7 instanceof $t.RightParen) {
                          let tokens$2 = tokens$1.tail;
                          let end = tokens$1.head[1].byte_offset;
                          let span = new Span(function$.location.start, end + 1);
                          let call$1 = new Call(
                            span,
                            function$,
                            $list.reverse(arguments$1),
                          );
                          return after_expression(call$1, tokens$2);
                        } else if ($7 instanceof $t.Comma) {
                          let tokens$2 = tokens$1.tail;
                          return call(arguments$1, function$, tokens$2);
                        } else {
                          let other = $7;
                          let position = tokens$1.head[1];
                          return new Error(new UnexpectedToken(other, position));
                        }
                      }
                    },
                  );
                }
              } else {
                return $result.try$(
                  field(tokens, expression),
                  (_use0) => {
                    let argument;
                    let tokens$1;
                    argument = _use0[0];
                    tokens$1 = _use0[1];
                    let arguments$1 = listPrepend(argument, arguments$);
                    if (tokens$1 instanceof $Empty) {
                      return new Error(new UnexpectedEndOfInput());
                    } else {
                      let $6 = tokens$1.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$2 = tokens$1.tail;
                        let end = tokens$1.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let call$1 = new Call(
                          span,
                          function$,
                          $list.reverse(arguments$1),
                        );
                        return after_expression(call$1, tokens$2);
                      } else if ($6 instanceof $t.Comma) {
                        let tokens$2 = tokens$1.tail;
                        return call(arguments$1, function$, tokens$2);
                      } else {
                        let other = $6;
                        let position = tokens$1.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    }
                  },
                );
              }
            } else {
              let $5 = $1.head[0];
              if ($5 instanceof $t.Colon) {
                let $6 = $2.head[0];
                if ($6 instanceof $t.DiscardName) {
                  let $7 = $3.head[0];
                  if ($7 instanceof $t.RightParen) {
                    let $8 = $6[0];
                    if ($8 === "") {
                      let label = $[0];
                      let tokens$1 = $4;
                      let end = $3.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let capture = new FnCapture(
                        span,
                        new Some(label),
                        function$,
                        $list.reverse(arguments$),
                        toList([]),
                      );
                      return after_expression(capture, tokens$1);
                    } else {
                      return $result.try$(
                        field(tokens, expression),
                        (_use0) => {
                          let argument;
                          let tokens$1;
                          argument = _use0[0];
                          tokens$1 = _use0[1];
                          let arguments$1 = listPrepend(argument, arguments$);
                          if (tokens$1 instanceof $Empty) {
                            return new Error(new UnexpectedEndOfInput());
                          } else {
                            let $9 = tokens$1.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$2 = tokens$1.tail;
                              let end = tokens$1.head[1].byte_offset;
                              let span = new Span(
                                function$.location.start,
                                end + 1,
                              );
                              let call$1 = new Call(
                                span,
                                function$,
                                $list.reverse(arguments$1),
                              );
                              return after_expression(call$1, tokens$2);
                            } else if ($9 instanceof $t.Comma) {
                              let tokens$2 = tokens$1.tail;
                              return call(arguments$1, function$, tokens$2);
                            } else {
                              let other = $9;
                              let position = tokens$1.head[1];
                              return new Error(
                                new UnexpectedToken(other, position),
                              );
                            }
                          }
                        },
                      );
                    }
                  } else if ($7 instanceof $t.Comma) {
                    let $8 = $4.head[0];
                    if ($8 instanceof $t.RightParen) {
                      let $9 = $6[0];
                      if ($9 === "") {
                        let label = $[0];
                        let tokens$1 = $4.tail;
                        let end = $4.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let capture = new FnCapture(
                          span,
                          new Some(label),
                          function$,
                          $list.reverse(arguments$),
                          toList([]),
                        );
                        return after_expression(capture, tokens$1);
                      } else {
                        return $result.try$(
                          field(tokens, expression),
                          (_use0) => {
                            let argument;
                            let tokens$1;
                            argument = _use0[0];
                            tokens$1 = _use0[1];
                            let arguments$1 = listPrepend(argument, arguments$);
                            if (tokens$1 instanceof $Empty) {
                              return new Error(new UnexpectedEndOfInput());
                            } else {
                              let $10 = tokens$1.head[0];
                              if ($10 instanceof $t.RightParen) {
                                let tokens$2 = tokens$1.tail;
                                let end = tokens$1.head[1].byte_offset;
                                let span = new Span(
                                  function$.location.start,
                                  end + 1,
                                );
                                let call$1 = new Call(
                                  span,
                                  function$,
                                  $list.reverse(arguments$1),
                                );
                                return after_expression(call$1, tokens$2);
                              } else if ($10 instanceof $t.Comma) {
                                let tokens$2 = tokens$1.tail;
                                return call(arguments$1, function$, tokens$2);
                              } else {
                                let other = $10;
                                let position = tokens$1.head[1];
                                return new Error(
                                  new UnexpectedToken(other, position),
                                );
                              }
                            }
                          },
                        );
                      }
                    } else {
                      let $9 = $6[0];
                      if ($9 === "") {
                        let label = $[0];
                        let tokens$1 = $4;
                        return fn_capture(
                          new Some(label),
                          function$,
                          $list.reverse(arguments$),
                          toList([]),
                          tokens$1,
                        );
                      } else {
                        return $result.try$(
                          field(tokens, expression),
                          (_use0) => {
                            let argument;
                            let tokens$1;
                            argument = _use0[0];
                            tokens$1 = _use0[1];
                            let arguments$1 = listPrepend(argument, arguments$);
                            if (tokens$1 instanceof $Empty) {
                              return new Error(new UnexpectedEndOfInput());
                            } else {
                              let $10 = tokens$1.head[0];
                              if ($10 instanceof $t.RightParen) {
                                let tokens$2 = tokens$1.tail;
                                let end = tokens$1.head[1].byte_offset;
                                let span = new Span(
                                  function$.location.start,
                                  end + 1,
                                );
                                let call$1 = new Call(
                                  span,
                                  function$,
                                  $list.reverse(arguments$1),
                                );
                                return after_expression(call$1, tokens$2);
                              } else if ($10 instanceof $t.Comma) {
                                let tokens$2 = tokens$1.tail;
                                return call(arguments$1, function$, tokens$2);
                              } else {
                                let other = $10;
                                let position = tokens$1.head[1];
                                return new Error(
                                  new UnexpectedToken(other, position),
                                );
                              }
                            }
                          },
                        );
                      }
                    }
                  } else {
                    let $8 = $6[0];
                    if ($8 === "") {
                      let label = $[0];
                      let tokens$1 = $3;
                      return fn_capture(
                        new Some(label),
                        function$,
                        $list.reverse(arguments$),
                        toList([]),
                        tokens$1,
                      );
                    } else {
                      return $result.try$(
                        field(tokens, expression),
                        (_use0) => {
                          let argument;
                          let tokens$1;
                          argument = _use0[0];
                          tokens$1 = _use0[1];
                          let arguments$1 = listPrepend(argument, arguments$);
                          if (tokens$1 instanceof $Empty) {
                            return new Error(new UnexpectedEndOfInput());
                          } else {
                            let $9 = tokens$1.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$2 = tokens$1.tail;
                              let end = tokens$1.head[1].byte_offset;
                              let span = new Span(
                                function$.location.start,
                                end + 1,
                              );
                              let call$1 = new Call(
                                span,
                                function$,
                                $list.reverse(arguments$1),
                              );
                              return after_expression(call$1, tokens$2);
                            } else if ($9 instanceof $t.Comma) {
                              let tokens$2 = tokens$1.tail;
                              return call(arguments$1, function$, tokens$2);
                            } else {
                              let other = $9;
                              let position = tokens$1.head[1];
                              return new Error(
                                new UnexpectedToken(other, position),
                              );
                            }
                          }
                        },
                      );
                    }
                  }
                } else {
                  return $result.try$(
                    field(tokens, expression),
                    (_use0) => {
                      let argument;
                      let tokens$1;
                      argument = _use0[0];
                      tokens$1 = _use0[1];
                      let arguments$1 = listPrepend(argument, arguments$);
                      if (tokens$1 instanceof $Empty) {
                        return new Error(new UnexpectedEndOfInput());
                      } else {
                        let $7 = tokens$1.head[0];
                        if ($7 instanceof $t.RightParen) {
                          let tokens$2 = tokens$1.tail;
                          let end = tokens$1.head[1].byte_offset;
                          let span = new Span(function$.location.start, end + 1);
                          let call$1 = new Call(
                            span,
                            function$,
                            $list.reverse(arguments$1),
                          );
                          return after_expression(call$1, tokens$2);
                        } else if ($7 instanceof $t.Comma) {
                          let tokens$2 = tokens$1.tail;
                          return call(arguments$1, function$, tokens$2);
                        } else {
                          let other = $7;
                          let position = tokens$1.head[1];
                          return new Error(new UnexpectedToken(other, position));
                        }
                      }
                    },
                  );
                }
              } else {
                return $result.try$(
                  field(tokens, expression),
                  (_use0) => {
                    let argument;
                    let tokens$1;
                    argument = _use0[0];
                    tokens$1 = _use0[1];
                    let arguments$1 = listPrepend(argument, arguments$);
                    if (tokens$1 instanceof $Empty) {
                      return new Error(new UnexpectedEndOfInput());
                    } else {
                      let $6 = tokens$1.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$2 = tokens$1.tail;
                        let end = tokens$1.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let call$1 = new Call(
                          span,
                          function$,
                          $list.reverse(arguments$1),
                        );
                        return after_expression(call$1, tokens$2);
                      } else if ($6 instanceof $t.Comma) {
                        let tokens$2 = tokens$1.tail;
                        return call(arguments$1, function$, tokens$2);
                      } else {
                        let other = $6;
                        let position = tokens$1.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    }
                  },
                );
              }
            }
          }
        }
      }
    } else if ($ instanceof $t.DiscardName) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        let $2 = $[0];
        if ($2 === "") {
          let tokens$1 = $1;
          return fn_capture(
            new None(),
            function$,
            $list.reverse(arguments$),
            toList([]),
            tokens$1,
          );
        } else {
          return $result.try$(
            field(tokens, expression),
            (_use0) => {
              let argument;
              let tokens$1;
              argument = _use0[0];
              tokens$1 = _use0[1];
              let arguments$1 = listPrepend(argument, arguments$);
              if (tokens$1 instanceof $Empty) {
                return new Error(new UnexpectedEndOfInput());
              } else {
                let $3 = tokens$1.head[0];
                if ($3 instanceof $t.RightParen) {
                  let tokens$2 = tokens$1.tail;
                  let end = tokens$1.head[1].byte_offset;
                  let span = new Span(function$.location.start, end + 1);
                  let call$1 = new Call(
                    span,
                    function$,
                    $list.reverse(arguments$1),
                  );
                  return after_expression(call$1, tokens$2);
                } else if ($3 instanceof $t.Comma) {
                  let tokens$2 = tokens$1.tail;
                  return call(arguments$1, function$, tokens$2);
                } else {
                  let other = $3;
                  let position = tokens$1.head[1];
                  return new Error(new UnexpectedToken(other, position));
                }
              }
            },
          );
        }
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let $3 = $1.head[0];
          if ($3 instanceof $t.RightParen) {
            let $4 = $[0];
            if ($4 === "") {
              let tokens$1 = $2;
              let end = $1.head[1].byte_offset;
              let span = new Span(function$.location.start, end + 1);
              let capture = new FnCapture(
                span,
                new None(),
                function$,
                $list.reverse(arguments$),
                toList([]),
              );
              return after_expression(capture, tokens$1);
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          } else if ($3 instanceof $t.Comma) {
            let $4 = $[0];
            if ($4 === "") {
              let tokens$1 = $2;
              return fn_capture(
                new None(),
                function$,
                $list.reverse(arguments$),
                toList([]),
                tokens$1,
              );
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          } else {
            let $4 = $[0];
            if ($4 === "") {
              let tokens$1 = $1;
              return fn_capture(
                new None(),
                function$,
                $list.reverse(arguments$),
                toList([]),
                tokens$1,
              );
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          }
        } else {
          let $3 = $1.head[0];
          if ($3 instanceof $t.RightParen) {
            let $4 = $[0];
            if ($4 === "") {
              let tokens$1 = $2;
              let end = $1.head[1].byte_offset;
              let span = new Span(function$.location.start, end + 1);
              let capture = new FnCapture(
                span,
                new None(),
                function$,
                $list.reverse(arguments$),
                toList([]),
              );
              return after_expression(capture, tokens$1);
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          } else if ($3 instanceof $t.Comma) {
            let $4 = $2.head[0];
            if ($4 instanceof $t.RightParen) {
              let $5 = $[0];
              if ($5 === "") {
                let tokens$1 = $2.tail;
                let end = $2.head[1].byte_offset;
                let span = new Span(function$.location.start, end + 1);
                let capture = new FnCapture(
                  span,
                  new None(),
                  function$,
                  $list.reverse(arguments$),
                  toList([]),
                );
                return after_expression(capture, tokens$1);
              } else {
                return $result.try$(
                  field(tokens, expression),
                  (_use0) => {
                    let argument;
                    let tokens$1;
                    argument = _use0[0];
                    tokens$1 = _use0[1];
                    let arguments$1 = listPrepend(argument, arguments$);
                    if (tokens$1 instanceof $Empty) {
                      return new Error(new UnexpectedEndOfInput());
                    } else {
                      let $6 = tokens$1.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$2 = tokens$1.tail;
                        let end = tokens$1.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let call$1 = new Call(
                          span,
                          function$,
                          $list.reverse(arguments$1),
                        );
                        return after_expression(call$1, tokens$2);
                      } else if ($6 instanceof $t.Comma) {
                        let tokens$2 = tokens$1.tail;
                        return call(arguments$1, function$, tokens$2);
                      } else {
                        let other = $6;
                        let position = tokens$1.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    }
                  },
                );
              }
            } else {
              let $5 = $[0];
              if ($5 === "") {
                let tokens$1 = $2;
                return fn_capture(
                  new None(),
                  function$,
                  $list.reverse(arguments$),
                  toList([]),
                  tokens$1,
                );
              } else {
                return $result.try$(
                  field(tokens, expression),
                  (_use0) => {
                    let argument;
                    let tokens$1;
                    argument = _use0[0];
                    tokens$1 = _use0[1];
                    let arguments$1 = listPrepend(argument, arguments$);
                    if (tokens$1 instanceof $Empty) {
                      return new Error(new UnexpectedEndOfInput());
                    } else {
                      let $6 = tokens$1.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$2 = tokens$1.tail;
                        let end = tokens$1.head[1].byte_offset;
                        let span = new Span(function$.location.start, end + 1);
                        let call$1 = new Call(
                          span,
                          function$,
                          $list.reverse(arguments$1),
                        );
                        return after_expression(call$1, tokens$2);
                      } else if ($6 instanceof $t.Comma) {
                        let tokens$2 = tokens$1.tail;
                        return call(arguments$1, function$, tokens$2);
                      } else {
                        let other = $6;
                        let position = tokens$1.head[1];
                        return new Error(new UnexpectedToken(other, position));
                      }
                    }
                  },
                );
              }
            }
          } else {
            let $4 = $[0];
            if ($4 === "") {
              let tokens$1 = $1;
              return fn_capture(
                new None(),
                function$,
                $list.reverse(arguments$),
                toList([]),
                tokens$1,
              );
            } else {
              return $result.try$(
                field(tokens, expression),
                (_use0) => {
                  let argument;
                  let tokens$1;
                  argument = _use0[0];
                  tokens$1 = _use0[1];
                  let arguments$1 = listPrepend(argument, arguments$);
                  if (tokens$1 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$1.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$2 = tokens$1.tail;
                      let end = tokens$1.head[1].byte_offset;
                      let span = new Span(function$.location.start, end + 1);
                      let call$1 = new Call(
                        span,
                        function$,
                        $list.reverse(arguments$1),
                      );
                      return after_expression(call$1, tokens$2);
                    } else if ($5 instanceof $t.Comma) {
                      let tokens$2 = tokens$1.tail;
                      return call(arguments$1, function$, tokens$2);
                    } else {
                      let other = $5;
                      let position = tokens$1.head[1];
                      return new Error(new UnexpectedToken(other, position));
                    }
                  }
                },
              );
            }
          }
        }
      }
    } else if ($ instanceof $t.RightParen) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      let span = new Span(function$.location.start, end + 1);
      let call$1 = new Call(span, function$, $list.reverse(arguments$));
      return after_expression(call$1, tokens$1);
    } else {
      return $result.try$(
        field(tokens, expression),
        (_use0) => {
          let argument;
          let tokens$1;
          argument = _use0[0];
          tokens$1 = _use0[1];
          let arguments$1 = listPrepend(argument, arguments$);
          if (tokens$1 instanceof $Empty) {
            return new Error(new UnexpectedEndOfInput());
          } else {
            let $1 = tokens$1.head[0];
            if ($1 instanceof $t.RightParen) {
              let tokens$2 = tokens$1.tail;
              let end = tokens$1.head[1].byte_offset;
              let span = new Span(function$.location.start, end + 1);
              let call$1 = new Call(span, function$, $list.reverse(arguments$1));
              return after_expression(call$1, tokens$2);
            } else if ($1 instanceof $t.Comma) {
              let tokens$2 = tokens$1.tail;
              return call(arguments$1, function$, tokens$2);
            } else {
              let other = $1;
              let position = tokens$1.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          }
        },
      );
    }
  }
}

function fn_capture(label, function$, before, after, tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.RightParen) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      let span = new Span(function$.location.start, end + 1);
      let capture = new FnCapture(
        span,
        label,
        function$,
        before,
        $list.reverse(after),
      );
      return after_expression(capture, tokens$1);
    } else {
      return $result.try$(
        field(tokens, expression),
        (_use0) => {
          let argument;
          let tokens$1;
          argument = _use0[0];
          tokens$1 = _use0[1];
          let after$1 = listPrepend(argument, after);
          if (tokens$1 instanceof $Empty) {
            return new Error(new UnexpectedEndOfInput());
          } else {
            let $1 = tokens$1.head[0];
            if ($1 instanceof $t.RightParen) {
              let tokens$2 = tokens$1.tail;
              let end = tokens$1.head[1].byte_offset;
              let span = new Span(function$.location.start, end + 1);
              let call$1 = new FnCapture(
                span,
                label,
                function$,
                before,
                $list.reverse(after$1),
              );
              return after_expression(call$1, tokens$2);
            } else if ($1 instanceof $t.Comma) {
              let tokens$2 = tokens$1.tail;
              return fn_capture(label, function$, before, after$1, tokens$2);
            } else {
              let other = $1;
              let position = tokens$1.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          }
        },
      );
    }
  }
}

function after_expression(loop$parsed, loop$tokens) {
  while (true) {
    let parsed = loop$parsed;
    let tokens = loop$tokens;
    if (tokens instanceof $Empty) {
      return new Ok([parsed, tokens]);
    } else {
      let $ = tokens.tail;
      if ($ instanceof $Empty) {
        let $1 = tokens.head[0];
        if ($1 instanceof $t.LeftParen) {
          let tokens$1 = $;
          return call(toList([]), parsed, tokens$1);
        } else {
          return new Ok([parsed, tokens]);
        }
      } else {
        let $1 = tokens.head[0];
        if ($1 instanceof $t.LeftParen) {
          let tokens$1 = $;
          return call(toList([]), parsed, tokens$1);
        } else if ($1 instanceof $t.Dot) {
          let $2 = $.head[0];
          if ($2 instanceof $t.Name) {
            let tokens$1 = $.tail;
            let label_start = $.head[1].byte_offset;
            let label = $2[0];
            let span = new Span(
              parsed.location.start,
              string_offset(label_start, label),
            );
            let expression$1 = new FieldAccess(span, parsed, label);
            loop$parsed = expression$1;
            loop$tokens = tokens$1;
          } else if ($2 instanceof $t.UpperName) {
            let tokens$1 = $.tail;
            let label_start = $.head[1].byte_offset;
            let label = $2[0];
            let span = new Span(
              parsed.location.start,
              string_offset(label_start, label),
            );
            let expression$1 = new FieldAccess(span, parsed, label);
            loop$parsed = expression$1;
            loop$tokens = tokens$1;
          } else if ($2 instanceof $t.Int) {
            let tokens$1 = $.tail;
            let token = $2;
            let position = $.head[1];
            let value = $2[0];
            let $3 = $int.parse(value);
            if ($3 instanceof Ok) {
              let i = $3[0];
              let end = string_offset(position.byte_offset, value);
              let span = new Span(parsed.location.start, end);
              let expression$1 = new TupleIndex(span, parsed, i);
              loop$parsed = expression$1;
              loop$tokens = tokens$1;
            } else {
              return new Error(new UnexpectedToken(token, position));
            }
          } else {
            return new Ok([parsed, tokens]);
          }
        } else {
          return new Ok([parsed, tokens]);
        }
      }
    }
  }
}

function pattern_constructor_arguments(arguments$, tokens) {
  if (tokens instanceof $Empty) {
    let tokens$1 = tokens;
    return $result.try$(
      field(tokens$1, pattern),
      (_use0) => {
        let pattern$1;
        let tokens$2;
        pattern$1 = _use0[0];
        tokens$2 = _use0[1];
        let arguments$1 = listPrepend(pattern$1, arguments$);
        if (tokens$2 instanceof $Empty) {
          return new Error(new UnexpectedEndOfInput());
        } else {
          let $ = tokens$2.head[0];
          if ($ instanceof $t.RightParen) {
            let tokens$3 = tokens$2.tail;
            let end = tokens$2.head[1].byte_offset;
            return new Ok(
              new PatternConstructorArguments(
                arguments$1,
                false,
                end + 1,
                tokens$3,
              ),
            );
          } else if ($ instanceof $t.Comma) {
            let $1 = tokens$2.tail;
            if ($1 instanceof $Empty) {
              let tokens$3 = $1;
              return pattern_constructor_arguments(arguments$1, tokens$3);
            } else {
              let $2 = $1.tail;
              if ($2 instanceof $Empty) {
                let tokens$3 = $1;
                return pattern_constructor_arguments(arguments$1, tokens$3);
              } else {
                let $3 = $1.head[0];
                if ($3 instanceof $t.DotDot) {
                  let $4 = $2.head[0];
                  if ($4 instanceof $t.RightParen) {
                    let tokens$3 = $2.tail;
                    let end = $2.head[1].byte_offset;
                    return new Ok(
                      new PatternConstructorArguments(
                        arguments$1,
                        true,
                        end + 1,
                        tokens$3,
                      ),
                    );
                  } else {
                    let tokens$3 = $1;
                    return pattern_constructor_arguments(arguments$1, tokens$3);
                  }
                } else {
                  let tokens$3 = $1;
                  return pattern_constructor_arguments(arguments$1, tokens$3);
                }
              }
            }
          } else {
            let token = $;
            let position = tokens$2.head[1];
            return new Error(new UnexpectedToken(token, position));
          }
        }
      },
    );
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.RightParen) {
      let tokens$1 = tokens.tail;
      let end = tokens.head[1].byte_offset;
      return new Ok(
        new PatternConstructorArguments(arguments$, false, end + 1, tokens$1),
      );
    } else if ($ instanceof $t.DotDot) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        let tokens$1 = tokens;
        return $result.try$(
          field(tokens$1, pattern),
          (_use0) => {
            let pattern$1;
            let tokens$2;
            pattern$1 = _use0[0];
            tokens$2 = _use0[1];
            let arguments$1 = listPrepend(pattern$1, arguments$);
            if (tokens$2 instanceof $Empty) {
              return new Error(new UnexpectedEndOfInput());
            } else {
              let $2 = tokens$2.head[0];
              if ($2 instanceof $t.RightParen) {
                let tokens$3 = tokens$2.tail;
                let end = tokens$2.head[1].byte_offset;
                return new Ok(
                  new PatternConstructorArguments(
                    arguments$1,
                    false,
                    end + 1,
                    tokens$3,
                  ),
                );
              } else if ($2 instanceof $t.Comma) {
                let $3 = tokens$2.tail;
                if ($3 instanceof $Empty) {
                  let tokens$3 = $3;
                  return pattern_constructor_arguments(arguments$1, tokens$3);
                } else {
                  let $4 = $3.tail;
                  if ($4 instanceof $Empty) {
                    let tokens$3 = $3;
                    return pattern_constructor_arguments(arguments$1, tokens$3);
                  } else {
                    let $5 = $3.head[0];
                    if ($5 instanceof $t.DotDot) {
                      let $6 = $4.head[0];
                      if ($6 instanceof $t.RightParen) {
                        let tokens$3 = $4.tail;
                        let end = $4.head[1].byte_offset;
                        return new Ok(
                          new PatternConstructorArguments(
                            arguments$1,
                            true,
                            end + 1,
                            tokens$3,
                          ),
                        );
                      } else {
                        let tokens$3 = $3;
                        return pattern_constructor_arguments(
                          arguments$1,
                          tokens$3,
                        );
                      }
                    } else {
                      let tokens$3 = $3;
                      return pattern_constructor_arguments(
                        arguments$1,
                        tokens$3,
                      );
                    }
                  }
                }
              } else {
                let token = $2;
                let position = tokens$2.head[1];
                return new Error(new UnexpectedToken(token, position));
              }
            }
          },
        );
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let $3 = $1.head[0];
          if ($3 instanceof $t.RightParen) {
            let tokens$1 = $2;
            let end = $1.head[1].byte_offset;
            return new Ok(
              new PatternConstructorArguments(
                arguments$,
                true,
                end + 1,
                tokens$1,
              ),
            );
          } else {
            let tokens$1 = tokens;
            return $result.try$(
              field(tokens$1, pattern),
              (_use0) => {
                let pattern$1;
                let tokens$2;
                pattern$1 = _use0[0];
                tokens$2 = _use0[1];
                let arguments$1 = listPrepend(pattern$1, arguments$);
                if (tokens$2 instanceof $Empty) {
                  return new Error(new UnexpectedEndOfInput());
                } else {
                  let $4 = tokens$2.head[0];
                  if ($4 instanceof $t.RightParen) {
                    let tokens$3 = tokens$2.tail;
                    let end = tokens$2.head[1].byte_offset;
                    return new Ok(
                      new PatternConstructorArguments(
                        arguments$1,
                        false,
                        end + 1,
                        tokens$3,
                      ),
                    );
                  } else if ($4 instanceof $t.Comma) {
                    let $5 = tokens$2.tail;
                    if ($5 instanceof $Empty) {
                      let tokens$3 = $5;
                      return pattern_constructor_arguments(
                        arguments$1,
                        tokens$3,
                      );
                    } else {
                      let $6 = $5.tail;
                      if ($6 instanceof $Empty) {
                        let tokens$3 = $5;
                        return pattern_constructor_arguments(
                          arguments$1,
                          tokens$3,
                        );
                      } else {
                        let $7 = $5.head[0];
                        if ($7 instanceof $t.DotDot) {
                          let $8 = $6.head[0];
                          if ($8 instanceof $t.RightParen) {
                            let tokens$3 = $6.tail;
                            let end = $6.head[1].byte_offset;
                            return new Ok(
                              new PatternConstructorArguments(
                                arguments$1,
                                true,
                                end + 1,
                                tokens$3,
                              ),
                            );
                          } else {
                            let tokens$3 = $5;
                            return pattern_constructor_arguments(
                              arguments$1,
                              tokens$3,
                            );
                          }
                        } else {
                          let tokens$3 = $5;
                          return pattern_constructor_arguments(
                            arguments$1,
                            tokens$3,
                          );
                        }
                      }
                    }
                  } else {
                    let token = $4;
                    let position = tokens$2.head[1];
                    return new Error(new UnexpectedToken(token, position));
                  }
                }
              },
            );
          }
        } else {
          let $3 = $1.head[0];
          if ($3 instanceof $t.RightParen) {
            let tokens$1 = $2;
            let end = $1.head[1].byte_offset;
            return new Ok(
              new PatternConstructorArguments(
                arguments$,
                true,
                end + 1,
                tokens$1,
              ),
            );
          } else if ($3 instanceof $t.Comma) {
            let $4 = $2.head[0];
            if ($4 instanceof $t.RightParen) {
              let tokens$1 = $2.tail;
              let end = $2.head[1].byte_offset;
              return new Ok(
                new PatternConstructorArguments(
                  arguments$,
                  true,
                  end + 1,
                  tokens$1,
                ),
              );
            } else {
              let tokens$1 = tokens;
              return $result.try$(
                field(tokens$1, pattern),
                (_use0) => {
                  let pattern$1;
                  let tokens$2;
                  pattern$1 = _use0[0];
                  tokens$2 = _use0[1];
                  let arguments$1 = listPrepend(pattern$1, arguments$);
                  if (tokens$2 instanceof $Empty) {
                    return new Error(new UnexpectedEndOfInput());
                  } else {
                    let $5 = tokens$2.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$3 = tokens$2.tail;
                      let end = tokens$2.head[1].byte_offset;
                      return new Ok(
                        new PatternConstructorArguments(
                          arguments$1,
                          false,
                          end + 1,
                          tokens$3,
                        ),
                      );
                    } else if ($5 instanceof $t.Comma) {
                      let $6 = tokens$2.tail;
                      if ($6 instanceof $Empty) {
                        let tokens$3 = $6;
                        return pattern_constructor_arguments(
                          arguments$1,
                          tokens$3,
                        );
                      } else {
                        let $7 = $6.tail;
                        if ($7 instanceof $Empty) {
                          let tokens$3 = $6;
                          return pattern_constructor_arguments(
                            arguments$1,
                            tokens$3,
                          );
                        } else {
                          let $8 = $6.head[0];
                          if ($8 instanceof $t.DotDot) {
                            let $9 = $7.head[0];
                            if ($9 instanceof $t.RightParen) {
                              let tokens$3 = $7.tail;
                              let end = $7.head[1].byte_offset;
                              return new Ok(
                                new PatternConstructorArguments(
                                  arguments$1,
                                  true,
                                  end + 1,
                                  tokens$3,
                                ),
                              );
                            } else {
                              let tokens$3 = $6;
                              return pattern_constructor_arguments(
                                arguments$1,
                                tokens$3,
                              );
                            }
                          } else {
                            let tokens$3 = $6;
                            return pattern_constructor_arguments(
                              arguments$1,
                              tokens$3,
                            );
                          }
                        }
                      }
                    } else {
                      let token = $5;
                      let position = tokens$2.head[1];
                      return new Error(new UnexpectedToken(token, position));
                    }
                  }
                },
              );
            }
          } else {
            let tokens$1 = tokens;
            return $result.try$(
              field(tokens$1, pattern),
              (_use0) => {
                let pattern$1;
                let tokens$2;
                pattern$1 = _use0[0];
                tokens$2 = _use0[1];
                let arguments$1 = listPrepend(pattern$1, arguments$);
                if (tokens$2 instanceof $Empty) {
                  return new Error(new UnexpectedEndOfInput());
                } else {
                  let $4 = tokens$2.head[0];
                  if ($4 instanceof $t.RightParen) {
                    let tokens$3 = tokens$2.tail;
                    let end = tokens$2.head[1].byte_offset;
                    return new Ok(
                      new PatternConstructorArguments(
                        arguments$1,
                        false,
                        end + 1,
                        tokens$3,
                      ),
                    );
                  } else if ($4 instanceof $t.Comma) {
                    let $5 = tokens$2.tail;
                    if ($5 instanceof $Empty) {
                      let tokens$3 = $5;
                      return pattern_constructor_arguments(
                        arguments$1,
                        tokens$3,
                      );
                    } else {
                      let $6 = $5.tail;
                      if ($6 instanceof $Empty) {
                        let tokens$3 = $5;
                        return pattern_constructor_arguments(
                          arguments$1,
                          tokens$3,
                        );
                      } else {
                        let $7 = $5.head[0];
                        if ($7 instanceof $t.DotDot) {
                          let $8 = $6.head[0];
                          if ($8 instanceof $t.RightParen) {
                            let tokens$3 = $6.tail;
                            let end = $6.head[1].byte_offset;
                            return new Ok(
                              new PatternConstructorArguments(
                                arguments$1,
                                true,
                                end + 1,
                                tokens$3,
                              ),
                            );
                          } else {
                            let tokens$3 = $5;
                            return pattern_constructor_arguments(
                              arguments$1,
                              tokens$3,
                            );
                          }
                        } else {
                          let tokens$3 = $5;
                          return pattern_constructor_arguments(
                            arguments$1,
                            tokens$3,
                          );
                        }
                      }
                    }
                  } else {
                    let token = $4;
                    let position = tokens$2.head[1];
                    return new Error(new UnexpectedToken(token, position));
                  }
                }
              },
            );
          }
        }
      }
    } else {
      let tokens$1 = tokens;
      return $result.try$(
        field(tokens$1, pattern),
        (_use0) => {
          let pattern$1;
          let tokens$2;
          pattern$1 = _use0[0];
          tokens$2 = _use0[1];
          let arguments$1 = listPrepend(pattern$1, arguments$);
          if (tokens$2 instanceof $Empty) {
            return new Error(new UnexpectedEndOfInput());
          } else {
            let $1 = tokens$2.head[0];
            if ($1 instanceof $t.RightParen) {
              let tokens$3 = tokens$2.tail;
              let end = tokens$2.head[1].byte_offset;
              return new Ok(
                new PatternConstructorArguments(
                  arguments$1,
                  false,
                  end + 1,
                  tokens$3,
                ),
              );
            } else if ($1 instanceof $t.Comma) {
              let $2 = tokens$2.tail;
              if ($2 instanceof $Empty) {
                let tokens$3 = $2;
                return pattern_constructor_arguments(arguments$1, tokens$3);
              } else {
                let $3 = $2.tail;
                if ($3 instanceof $Empty) {
                  let tokens$3 = $2;
                  return pattern_constructor_arguments(arguments$1, tokens$3);
                } else {
                  let $4 = $2.head[0];
                  if ($4 instanceof $t.DotDot) {
                    let $5 = $3.head[0];
                    if ($5 instanceof $t.RightParen) {
                      let tokens$3 = $3.tail;
                      let end = $3.head[1].byte_offset;
                      return new Ok(
                        new PatternConstructorArguments(
                          arguments$1,
                          true,
                          end + 1,
                          tokens$3,
                        ),
                      );
                    } else {
                      let tokens$3 = $2;
                      return pattern_constructor_arguments(
                        arguments$1,
                        tokens$3,
                      );
                    }
                  } else {
                    let tokens$3 = $2;
                    return pattern_constructor_arguments(arguments$1, tokens$3);
                  }
                }
              }
            } else {
              let token = $1;
              let position = tokens$2.head[1];
              return new Error(new UnexpectedToken(token, position));
            }
          }
        },
      );
    }
  }
}

function pattern(tokens) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Error(new UnexpectedEndOfInput());
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.Name) {
          let $1 = tokens.tail;
          if ($1 instanceof $Empty) {
            let tokens$1 = $1;
            let start = tokens.head[1].byte_offset;
            let name$1 = $[0];
            return new Ok(
              [
                new PatternVariable(span_from_string(start, name$1), name$1),
                tokens$1,
              ],
            );
          } else {
            let $2 = $1.tail;
            if ($2 instanceof $Empty) {
              let tokens$1 = $1;
              let start = tokens.head[1].byte_offset;
              let name$1 = $[0];
              return new Ok(
                [
                  new PatternVariable(span_from_string(start, name$1), name$1),
                  tokens$1,
                ],
              );
            } else {
              let $3 = $1.head[0];
              if ($3 instanceof $t.Dot) {
                let $4 = $2.head[0];
                if ($4 instanceof $t.UpperName) {
                  let start = tokens.head[1].byte_offset;
                  let module$1 = $[0];
                  let tokens$1 = $2.tail;
                  let name_start = $2.head[1].byte_offset;
                  let name$1 = $4[0];
                  return pattern_constructor(
                    new Some(module$1),
                    name$1,
                    tokens$1,
                    start,
                    name_start,
                  );
                } else {
                  let tokens$1 = $1;
                  let start = tokens.head[1].byte_offset;
                  let name$1 = $[0];
                  return new Ok(
                    [
                      new PatternVariable(
                        span_from_string(start, name$1),
                        name$1,
                      ),
                      tokens$1,
                    ],
                  );
                }
              } else {
                let tokens$1 = $1;
                let start = tokens.head[1].byte_offset;
                let name$1 = $[0];
                return new Ok(
                  [
                    new PatternVariable(span_from_string(start, name$1), name$1),
                    tokens$1,
                  ],
                );
              }
            }
          }
        } else if ($ instanceof $t.UpperName) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let name$1 = $[0];
          return pattern_constructor(new None(), name$1, tokens$1, start, start);
        } else if ($ instanceof $t.DiscardName) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let name$1 = $[0];
          return new Ok(
            [
              new PatternDiscard(
                new Span(start, string_offset(start, name$1) + 1),
                name$1,
              ),
              tokens$1,
            ],
          );
        } else if ($ instanceof $t.Int) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let value = $[0];
          return new Ok(
            [new PatternInt(span_from_string(start, value), value), tokens$1],
          );
        } else if ($ instanceof $t.Float) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let value = $[0];
          return new Ok(
            [new PatternFloat(span_from_string(start, value), value), tokens$1],
          );
        } else if ($ instanceof $t.String) {
          let $1 = tokens.tail;
          if ($1 instanceof $Empty) {
            let tokens$1 = $1;
            let start = tokens.head[1].byte_offset;
            let value = $[0];
            return new Ok(
              [
                new PatternString(
                  new Span(start, string_offset(start, value) + 2),
                  value,
                ),
                tokens$1,
              ],
            );
          } else {
            let $2 = $1.tail;
            if ($2 instanceof $Empty) {
              let tokens$1 = $1;
              let start = tokens.head[1].byte_offset;
              let value = $[0];
              return new Ok(
                [
                  new PatternString(
                    new Span(start, string_offset(start, value) + 2),
                    value,
                  ),
                  tokens$1,
                ],
              );
            } else {
              let $3 = $2.tail;
              if ($3 instanceof $Empty) {
                let $4 = $1.head[0];
                if ($4 instanceof $t.LessGreater) {
                  let $5 = $2.head[0];
                  if ($5 instanceof $t.Name) {
                    let start = tokens.head[1].byte_offset;
                    let v = $[0];
                    let tokens$1 = $3;
                    let name_start = $2.head[1].byte_offset;
                    let n = $5[0];
                    let span = new Span(start, string_offset(name_start, n));
                    let pattern$1 = new PatternConcatenate(
                      span,
                      v,
                      new None(),
                      new Named(n),
                    );
                    return new Ok([pattern$1, tokens$1]);
                  } else if ($5 instanceof $t.DiscardName) {
                    let start = tokens.head[1].byte_offset;
                    let v = $[0];
                    let tokens$1 = $3;
                    let name_start = $2.head[1].byte_offset;
                    let n = $5[0];
                    let span = new Span(start, string_offset(name_start, n) + 1);
                    let pattern$1 = new PatternConcatenate(
                      span,
                      v,
                      new None(),
                      new Discarded(n),
                    );
                    return new Ok([pattern$1, tokens$1]);
                  } else {
                    let tokens$1 = $1;
                    let start = tokens.head[1].byte_offset;
                    let value = $[0];
                    return new Ok(
                      [
                        new PatternString(
                          new Span(start, string_offset(start, value) + 2),
                          value,
                        ),
                        tokens$1,
                      ],
                    );
                  }
                } else {
                  let tokens$1 = $1;
                  let start = tokens.head[1].byte_offset;
                  let value = $[0];
                  return new Ok(
                    [
                      new PatternString(
                        new Span(start, string_offset(start, value) + 2),
                        value,
                      ),
                      tokens$1,
                    ],
                  );
                }
              } else {
                let $4 = $3.tail;
                if ($4 instanceof $Empty) {
                  let $5 = $1.head[0];
                  if ($5 instanceof $t.LessGreater) {
                    let $6 = $2.head[0];
                    if ($6 instanceof $t.Name) {
                      let start = tokens.head[1].byte_offset;
                      let v = $[0];
                      let tokens$1 = $3;
                      let name_start = $2.head[1].byte_offset;
                      let n = $6[0];
                      let span = new Span(start, string_offset(name_start, n));
                      let pattern$1 = new PatternConcatenate(
                        span,
                        v,
                        new None(),
                        new Named(n),
                      );
                      return new Ok([pattern$1, tokens$1]);
                    } else if ($6 instanceof $t.DiscardName) {
                      let start = tokens.head[1].byte_offset;
                      let v = $[0];
                      let tokens$1 = $3;
                      let name_start = $2.head[1].byte_offset;
                      let n = $6[0];
                      let span = new Span(
                        start,
                        string_offset(name_start, n) + 1,
                      );
                      let pattern$1 = new PatternConcatenate(
                        span,
                        v,
                        new None(),
                        new Discarded(n),
                      );
                      return new Ok([pattern$1, tokens$1]);
                    } else {
                      let tokens$1 = $1;
                      let start = tokens.head[1].byte_offset;
                      let value = $[0];
                      return new Ok(
                        [
                          new PatternString(
                            new Span(start, string_offset(start, value) + 2),
                            value,
                          ),
                          tokens$1,
                        ],
                      );
                    }
                  } else {
                    let tokens$1 = $1;
                    let start = tokens.head[1].byte_offset;
                    let value = $[0];
                    return new Ok(
                      [
                        new PatternString(
                          new Span(start, string_offset(start, value) + 2),
                          value,
                        ),
                        tokens$1,
                      ],
                    );
                  }
                } else {
                  let $5 = $1.head[0];
                  if ($5 instanceof $t.As) {
                    let $6 = $2.head[0];
                    if ($6 instanceof $t.Name) {
                      let $7 = $3.head[0];
                      if ($7 instanceof $t.LessGreater) {
                        let $8 = $4.head[0];
                        if ($8 instanceof $t.Name) {
                          let start = tokens.head[1].byte_offset;
                          let v = $[0];
                          let tokens$1 = $4.tail;
                          let name_start = $4.head[1].byte_offset;
                          let l = $6[0];
                          let r = $8[0];
                          let span = new Span(
                            start,
                            string_offset(name_start, r),
                          );
                          let pattern$1 = new PatternConcatenate(
                            span,
                            v,
                            new Some(new Named(l)),
                            new Named(r),
                          );
                          return new Ok([pattern$1, tokens$1]);
                        } else {
                          let tokens$1 = $1;
                          let start = tokens.head[1].byte_offset;
                          let value = $[0];
                          return new Ok(
                            [
                              new PatternString(
                                new Span(start, string_offset(start, value) + 2),
                                value,
                              ),
                              tokens$1,
                            ],
                          );
                        }
                      } else {
                        let tokens$1 = $1;
                        let start = tokens.head[1].byte_offset;
                        let value = $[0];
                        return new Ok(
                          [
                            new PatternString(
                              new Span(start, string_offset(start, value) + 2),
                              value,
                            ),
                            tokens$1,
                          ],
                        );
                      }
                    } else if ($6 instanceof $t.DiscardName) {
                      let $7 = $3.head[0];
                      if ($7 instanceof $t.LessGreater) {
                        let $8 = $4.head[0];
                        if ($8 instanceof $t.Name) {
                          let start = tokens.head[1].byte_offset;
                          let v = $[0];
                          let tokens$1 = $4.tail;
                          let name_start = $4.head[1].byte_offset;
                          let l = $6[0];
                          let r = $8[0];
                          let span = new Span(
                            start,
                            string_offset(name_start, r),
                          );
                          let pattern$1 = new PatternConcatenate(
                            span,
                            v,
                            new Some(new Discarded(l)),
                            new Named(r),
                          );
                          return new Ok([pattern$1, tokens$1]);
                        } else {
                          let tokens$1 = $1;
                          let start = tokens.head[1].byte_offset;
                          let value = $[0];
                          return new Ok(
                            [
                              new PatternString(
                                new Span(start, string_offset(start, value) + 2),
                                value,
                              ),
                              tokens$1,
                            ],
                          );
                        }
                      } else {
                        let tokens$1 = $1;
                        let start = tokens.head[1].byte_offset;
                        let value = $[0];
                        return new Ok(
                          [
                            new PatternString(
                              new Span(start, string_offset(start, value) + 2),
                              value,
                            ),
                            tokens$1,
                          ],
                        );
                      }
                    } else {
                      let tokens$1 = $1;
                      let start = tokens.head[1].byte_offset;
                      let value = $[0];
                      return new Ok(
                        [
                          new PatternString(
                            new Span(start, string_offset(start, value) + 2),
                            value,
                          ),
                          tokens$1,
                        ],
                      );
                    }
                  } else if ($5 instanceof $t.LessGreater) {
                    let $6 = $2.head[0];
                    if ($6 instanceof $t.Name) {
                      let start = tokens.head[1].byte_offset;
                      let v = $[0];
                      let tokens$1 = $3;
                      let name_start = $2.head[1].byte_offset;
                      let n = $6[0];
                      let span = new Span(start, string_offset(name_start, n));
                      let pattern$1 = new PatternConcatenate(
                        span,
                        v,
                        new None(),
                        new Named(n),
                      );
                      return new Ok([pattern$1, tokens$1]);
                    } else if ($6 instanceof $t.DiscardName) {
                      let start = tokens.head[1].byte_offset;
                      let v = $[0];
                      let tokens$1 = $3;
                      let name_start = $2.head[1].byte_offset;
                      let n = $6[0];
                      let span = new Span(
                        start,
                        string_offset(name_start, n) + 1,
                      );
                      let pattern$1 = new PatternConcatenate(
                        span,
                        v,
                        new None(),
                        new Discarded(n),
                      );
                      return new Ok([pattern$1, tokens$1]);
                    } else {
                      let tokens$1 = $1;
                      let start = tokens.head[1].byte_offset;
                      let value = $[0];
                      return new Ok(
                        [
                          new PatternString(
                            new Span(start, string_offset(start, value) + 2),
                            value,
                          ),
                          tokens$1,
                        ],
                      );
                    }
                  } else {
                    let tokens$1 = $1;
                    let start = tokens.head[1].byte_offset;
                    let value = $[0];
                    return new Ok(
                      [
                        new PatternString(
                          new Span(start, string_offset(start, value) + 2),
                          value,
                        ),
                        tokens$1,
                      ],
                    );
                  }
                }
              }
            }
          }
        } else if ($ instanceof $t.LeftSquare) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let result = list(
            pattern,
            new Some((_capture) => { return new PatternDiscard(_capture, ""); }),
            toList([]),
            tokens$1,
          );
          return $result.map(
            result,
            (_use0) => {
              let elements;
              let rest;
              let tokens$2;
              let end;
              elements = _use0.values;
              rest = _use0.spread;
              tokens$2 = _use0.remaining_tokens;
              end = _use0.end;
              return [
                new PatternList(new Span(start, end), elements, rest),
                tokens$2,
              ];
            },
          );
        } else if ($ instanceof $t.Hash) {
          let $1 = tokens.tail;
          if ($1 instanceof $Empty) {
            let other = $;
            let position = tokens.head[1];
            return new Error(new UnexpectedToken(other, position));
          } else {
            let $2 = $1.head[0];
            if ($2 instanceof $t.LeftParen) {
              let start = tokens.head[1].byte_offset;
              let tokens$1 = $1.tail;
              let result = comma_delimited(
                toList([]),
                tokens$1,
                pattern,
                new $t.RightParen(),
              );
              return $result.try$(
                result,
                (_use0) => {
                  let patterns;
                  let end;
                  let tokens$2;
                  patterns = _use0[0];
                  end = _use0[1];
                  tokens$2 = _use0[2];
                  return new Ok(
                    [new PatternTuple(new Span(start, end), patterns), tokens$2],
                  );
                },
              );
            } else {
              let other = $;
              let position = tokens.head[1];
              return new Error(new UnexpectedToken(other, position));
            }
          }
        } else if ($ instanceof $t.LessLess) {
          let tokens$1 = tokens.tail;
          let start = tokens.head[1].byte_offset;
          let parser = (_capture) => {
            return bit_string_segment(pattern, _capture);
          };
          let result = comma_delimited(
            toList([]),
            tokens$1,
            parser,
            new $t.GreaterGreater(),
          );
          return $result.try$(
            result,
            (_use0) => {
              let segments;
              let end;
              let tokens$2;
              segments = _use0[0];
              end = _use0[1];
              tokens$2 = _use0[2];
              return new Ok(
                [new PatternBitString(new Span(start, end), segments), tokens$2],
              );
            },
          );
        } else {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        }
      }
    })(),
    (_use0) => {
      let pattern$1;
      let tokens$1;
      pattern$1 = _use0[0];
      tokens$1 = _use0[1];
      if (tokens$1 instanceof $Empty) {
        return new Ok([pattern$1, tokens$1]);
      } else {
        let $ = tokens$1.tail;
        if ($ instanceof $Empty) {
          return new Ok([pattern$1, tokens$1]);
        } else {
          let $1 = tokens$1.head[0];
          if ($1 instanceof $t.As) {
            let $2 = $.head[0];
            if ($2 instanceof $t.Name) {
              let tokens$2 = $.tail;
              let name_start = $.head[1].byte_offset;
              let name$1 = $2[0];
              let span = new Span(
                pattern$1.location.start,
                string_offset(name_start, name$1),
              );
              let pattern$2 = new PatternAssignment(span, pattern$1, name$1);
              return new Ok([pattern$2, tokens$2]);
            } else {
              return new Ok([pattern$1, tokens$1]);
            }
          } else {
            return new Ok([pattern$1, tokens$1]);
          }
        }
      }
    },
  );
}

function pattern_constructor(module, constructor, tokens, start, name_start) {
  if (tokens instanceof $Empty) {
    let span = new Span(start, string_offset(name_start, constructor));
    let pattern$1 = new PatternVariant(
      span,
      module,
      constructor,
      toList([]),
      false,
    );
    return new Ok([pattern$1, tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.LeftParen) {
      let tokens$1 = tokens.tail;
      let result = pattern_constructor_arguments(toList([]), tokens$1);
      return $result.try$(
        result,
        (_use0) => {
          let patterns;
          let spread;
          let end;
          let tokens$2;
          patterns = _use0.fields;
          spread = _use0.spread;
          end = _use0.end;
          tokens$2 = _use0.remaining_tokens;
          let arguments$ = $list.reverse(patterns);
          let pattern$1 = new PatternVariant(
            new Span(start, end),
            module,
            constructor,
            arguments$,
            spread,
          );
          return new Ok([pattern$1, tokens$2]);
        },
      );
    } else {
      let span = new Span(start, string_offset(name_start, constructor));
      let pattern$1 = new PatternVariant(
        span,
        module,
        constructor,
        toList([]),
        false,
      );
      return new Ok([pattern$1, tokens]);
    }
  }
}

function case_clause(tokens) {
  let multipatterns = (_capture) => {
    return delimited(toList([]), _capture, pattern, new $t.Comma());
  };
  let result = delimited(toList([]), tokens, multipatterns, new $t.VBar());
  return $result.try$(
    result,
    (_use0) => {
      let patterns;
      let tokens$1;
      patterns = _use0[0];
      tokens$1 = _use0[1];
      return $result.try$(
        optional_clause_guard(tokens$1),
        (_use0) => {
          let guard;
          let tokens$2;
          guard = _use0[0];
          tokens$2 = _use0[1];
          return expect(
            new $t.RightArrow(),
            tokens$2,
            (_, tokens) => {
              return $result.map(
                expression(tokens),
                (_use0) => {
                  let expression$1;
                  let tokens$1;
                  expression$1 = _use0[0];
                  tokens$1 = _use0[1];
                  return [new Clause(patterns, guard, expression$1), tokens$1];
                },
              );
            },
          );
        },
      );
    },
  );
}

function case_clauses(clauses, tokens) {
  return $result.try$(
    case_clause(tokens),
    (_use0) => {
      let clause;
      let tokens$1;
      clause = _use0[0];
      tokens$1 = _use0[1];
      let clauses$1 = listPrepend(clause, clauses);
      if (tokens$1 instanceof $Empty) {
        return case_clauses(clauses$1, tokens$1);
      } else {
        let $ = tokens$1.head[0];
        if ($ instanceof $t.RightBrace) {
          let tokens$2 = tokens$1.tail;
          let end = tokens$1.head[1].byte_offset;
          return new Ok([$list.reverse(clauses$1), tokens$2, end + 1]);
        } else {
          return case_clauses(clauses$1, tokens$1);
        }
      }
    },
  );
}

function case_(tokens, start) {
  return $result.try$(
    case_subjects(toList([]), tokens),
    (_use0) => {
      let subjects;
      let tokens$1;
      subjects = _use0[0];
      tokens$1 = _use0[1];
      return expect(
        new $t.LeftBrace(),
        tokens$1,
        (_, tokens) => {
          return $result.try$(
            case_clauses(toList([]), tokens),
            (_use0) => {
              let clauses;
              let tokens$1;
              let end;
              clauses = _use0[0];
              tokens$1 = _use0[1];
              end = _use0[2];
              return new Ok(
                [
                  new Some(new Case(new Span(start, end), subjects, clauses)),
                  tokens$1,
                ],
              );
            },
          );
        },
      );
    },
  );
}

function named_type(name, module, tokens, start, name_start) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        let end = name_start + $string.byte_size(name);
        return new Ok([toList([]), end, tokens]);
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.LeftParen) {
          let tokens$1 = tokens.tail;
          return comma_delimited(
            toList([]),
            tokens$1,
            type_,
            new $t.RightParen(),
          );
        } else {
          let end = name_start + $string.byte_size(name);
          return new Ok([toList([]), end, tokens]);
        }
      }
    })(),
    (_use0) => {
      let parameters;
      let end;
      let tokens$1;
      parameters = _use0[0];
      end = _use0[1];
      tokens$1 = _use0[2];
      let t = new NamedType(new Span(start, end), name, module, parameters);
      return new Ok([t, tokens$1]);
    },
  );
}

function type_(tokens) {
  if (tokens instanceof $Empty) {
    return new Error(new UnexpectedEndOfInput());
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Name) {
        let tokens$1 = $;
        let i = tokens.head[1].byte_offset;
        let name$1 = $1[0];
        let value = new VariableType(span_from_string(i, name$1), name$1);
        return new Ok([value, tokens$1]);
      } else if ($1 instanceof $t.UpperName) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        let name$1 = $1[0];
        return named_type(name$1, new None(), tokens$1, start, start);
      } else if ($1 instanceof $t.DiscardName) {
        let tokens$1 = $;
        let i = tokens.head[1].byte_offset;
        let name$1 = $1[0];
        let value = new HoleType(
          new Span(i, string_offset(i, name$1) + 1),
          name$1,
        );
        return new Ok([value, tokens$1]);
      } else {
        let token = $1;
        let position = tokens.head[1];
        return new Error(new UnexpectedToken(token, position));
      }
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Name) {
        let $2 = $.tail;
        if ($2 instanceof $Empty) {
          let tokens$1 = $;
          let i = tokens.head[1].byte_offset;
          let name$1 = $1[0];
          let value = new VariableType(span_from_string(i, name$1), name$1);
          return new Ok([value, tokens$1]);
        } else {
          let $3 = $.head[0];
          if ($3 instanceof $t.Dot) {
            let $4 = $2.head[0];
            if ($4 instanceof $t.UpperName) {
              let start = tokens.head[1].byte_offset;
              let module$1 = $1[0];
              let tokens$1 = $2.tail;
              let end = $2.head[1].byte_offset;
              let name$1 = $4[0];
              return named_type(
                name$1,
                new Some(module$1),
                tokens$1,
                start,
                end,
              );
            } else {
              let tokens$1 = $;
              let i = tokens.head[1].byte_offset;
              let name$1 = $1[0];
              let value = new VariableType(span_from_string(i, name$1), name$1);
              return new Ok([value, tokens$1]);
            }
          } else {
            let tokens$1 = $;
            let i = tokens.head[1].byte_offset;
            let name$1 = $1[0];
            let value = new VariableType(span_from_string(i, name$1), name$1);
            return new Ok([value, tokens$1]);
          }
        }
      } else if ($1 instanceof $t.UpperName) {
        let tokens$1 = $;
        let start = tokens.head[1].byte_offset;
        let name$1 = $1[0];
        return named_type(name$1, new None(), tokens$1, start, start);
      } else if ($1 instanceof $t.DiscardName) {
        let tokens$1 = $;
        let i = tokens.head[1].byte_offset;
        let name$1 = $1[0];
        let value = new HoleType(
          new Span(i, string_offset(i, name$1) + 1),
          name$1,
        );
        return new Ok([value, tokens$1]);
      } else if ($1 instanceof $t.Fn) {
        let $2 = $.head[0];
        if ($2 instanceof $t.LeftParen) {
          let i = tokens.head[1].byte_offset;
          let tokens$1 = $.tail;
          return fn_type(i, tokens$1);
        } else {
          let token = $1;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(token, position));
        }
      } else if ($1 instanceof $t.Hash) {
        let $2 = $.head[0];
        if ($2 instanceof $t.LeftParen) {
          let i = tokens.head[1].byte_offset;
          let tokens$1 = $.tail;
          return tuple_type(i, tokens$1);
        } else {
          let token = $1;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(token, position));
        }
      } else {
        let token = $1;
        let position = tokens.head[1];
        return new Error(new UnexpectedToken(token, position));
      }
    }
  }
}

function optional_return_annotation(end, tokens) {
  if (tokens instanceof $Empty) {
    return new Ok([new None(), end, tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.RightArrow) {
      let tokens$1 = tokens.tail;
      return $result.try$(
        type_(tokens$1),
        (_use0) => {
          let return_type;
          let tokens$2;
          return_type = _use0[0];
          tokens$2 = _use0[1];
          return new Ok(
            [new Some(return_type), return_type.location.end, tokens$2],
          );
        },
      );
    } else {
      return new Ok([new None(), end, tokens]);
    }
  }
}

function optional_type_annotation(tokens) {
  if (tokens instanceof $Empty) {
    return new Ok([new None(), tokens]);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Colon) {
      let tokens$1 = tokens.tail;
      return $result.map(
        type_(tokens$1),
        (_use0) => {
          let annotation;
          let tokens$2;
          annotation = _use0[0];
          tokens$2 = _use0[1];
          return [new Some(annotation), tokens$2];
        },
      );
    } else {
      return new Ok([new None(), tokens]);
    }
  }
}

function use_pattern(tokens) {
  return $result.try$(
    pattern(tokens),
    (_use0) => {
      let pattern$1;
      let tokens$1;
      pattern$1 = _use0[0];
      tokens$1 = _use0[1];
      return $result.try$(
        optional_type_annotation(tokens$1),
        (_use0) => {
          let annotation;
          let tokens$2;
          annotation = _use0[0];
          tokens$2 = _use0[1];
          return new Ok([new UsePattern(pattern$1, annotation), tokens$2]);
        },
      );
    },
  );
}

function use_(tokens, start) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return delimited(toList([]), tokens, use_pattern, new $t.Comma());
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.LeftArrow) {
          return new Ok([toList([]), tokens]);
        } else {
          return delimited(toList([]), tokens, use_pattern, new $t.Comma());
        }
      }
    })(),
    (_use0) => {
      let patterns;
      let tokens$1;
      patterns = _use0[0];
      tokens$1 = _use0[1];
      return expect(
        new $t.LeftArrow(),
        tokens$1,
        (_, tokens) => {
          return $result.try$(
            expression(tokens),
            (_use0) => {
              let function$;
              let tokens$1;
              function$ = _use0[0];
              tokens$1 = _use0[1];
              return new Ok(
                [
                  new Use(
                    new Span(start, function$.location.end),
                    patterns,
                    function$,
                  ),
                  tokens$1,
                ],
              );
            },
          );
        },
      );
    },
  );
}

function assignment(kind, tokens, start) {
  return $result.try$(
    pattern(tokens),
    (_use0) => {
      let pattern$1;
      let tokens$1;
      pattern$1 = _use0[0];
      tokens$1 = _use0[1];
      return $result.try$(
        optional_type_annotation(tokens$1),
        (_use0) => {
          let annotation;
          let tokens$2;
          annotation = _use0[0];
          tokens$2 = _use0[1];
          return expect(
            new $t.Equal(),
            tokens$2,
            (_, tokens) => {
              return $result.try$(
                expression(tokens),
                (_use0) => {
                  let value;
                  let tokens$1;
                  value = _use0[0];
                  tokens$1 = _use0[1];
                  return $result.try$(
                    (() => {
                      if (kind instanceof Let) {
                        return new Ok([kind, tokens$1, value.location.end]);
                      } else if (tokens$1 instanceof $Empty) {
                        return new Ok([kind, tokens$1, value.location.end]);
                      } else {
                        let $ = kind.message;
                        if ($ instanceof None) {
                          let $1 = tokens$1.head[0];
                          if ($1 instanceof $t.As) {
                            let tokens$2 = tokens$1.tail;
                            return $result.map(
                              expression(tokens$2),
                              (_use0) => {
                                let message;
                                let tokens$3;
                                message = _use0[0];
                                tokens$3 = _use0[1];
                                return [
                                  new LetAssert(new Some(message)),
                                  tokens$3,
                                  message.location.end,
                                ];
                              },
                            );
                          } else {
                            return new Ok([kind, tokens$1, value.location.end]);
                          }
                        } else {
                          return new Ok([kind, tokens$1, value.location.end]);
                        }
                      }
                    })(),
                    (_use0) => {
                      let kind$1;
                      let tokens$2;
                      let end;
                      kind$1 = _use0[0];
                      tokens$2 = _use0[1];
                      end = _use0[2];
                      let statement$1 = new Assignment(
                        new Span(start, end),
                        kind$1,
                        pattern$1,
                        annotation,
                        value,
                      );
                      return new Ok([statement$1, tokens$2]);
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

function fn_parameter(tokens) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Error(new UnexpectedEndOfInput());
      } else {
        let $ = tokens.head[0];
        if ($ instanceof $t.Name) {
          let tokens$1 = tokens.tail;
          let name$1 = $[0];
          return new Ok([new Named(name$1), tokens$1]);
        } else if ($ instanceof $t.DiscardName) {
          let tokens$1 = tokens.tail;
          let name$1 = $[0];
          return new Ok([new Discarded(name$1), tokens$1]);
        } else {
          let other = $;
          let position = tokens.head[1];
          return new Error(new UnexpectedToken(other, position));
        }
      }
    })(),
    (_use0) => {
      let name$1;
      let tokens$1;
      name$1 = _use0[0];
      tokens$1 = _use0[1];
      return $result.try$(
        optional_type_annotation(tokens$1),
        (_use0) => {
          let type_$1;
          let tokens$2;
          type_$1 = _use0[0];
          tokens$2 = _use0[1];
          return new Ok([new FnParameter(name$1, type_$1), tokens$2]);
        },
      );
    },
  );
}

function function_parameter(tokens) {
  return $result.try$(
    (() => {
      if (tokens instanceof $Empty) {
        return new Error(new UnexpectedEndOfInput());
      } else {
        let $ = tokens.tail;
        if ($ instanceof $Empty) {
          let $1 = tokens.head[0];
          if ($1 instanceof $t.Name) {
            let tokens$1 = $;
            let name$1 = $1[0];
            return new Ok([new None(), new Named(name$1), tokens$1]);
          } else if ($1 instanceof $t.DiscardName) {
            let tokens$1 = $;
            let name$1 = $1[0];
            return new Ok([new None(), new Discarded(name$1), tokens$1]);
          } else {
            let token = $1;
            let position = tokens.head[1];
            return new Error(new UnexpectedToken(token, position));
          }
        } else {
          let $1 = tokens.head[0];
          if ($1 instanceof $t.Name) {
            let $2 = $.head[0];
            if ($2 instanceof $t.Name) {
              let tokens$1 = $.tail;
              let label = $1[0];
              let name$1 = $2[0];
              return new Ok([new Some(label), new Named(name$1), tokens$1]);
            } else if ($2 instanceof $t.DiscardName) {
              let tokens$1 = $.tail;
              let label = $1[0];
              let name$1 = $2[0];
              return new Ok([new Some(label), new Discarded(name$1), tokens$1]);
            } else {
              let tokens$1 = $;
              let name$1 = $1[0];
              return new Ok([new None(), new Named(name$1), tokens$1]);
            }
          } else if ($1 instanceof $t.DiscardName) {
            let tokens$1 = $;
            let name$1 = $1[0];
            return new Ok([new None(), new Discarded(name$1), tokens$1]);
          } else {
            let token = $1;
            let position = tokens.head[1];
            return new Error(new UnexpectedToken(token, position));
          }
        }
      }
    })(),
    (_use0) => {
      let label;
      let parameter;
      let tokens$1;
      label = _use0[0];
      parameter = _use0[1];
      tokens$1 = _use0[2];
      return $result.try$(
        optional_type_annotation(tokens$1),
        (_use0) => {
          let type_$1;
          let tokens$2;
          type_$1 = _use0[0];
          tokens$2 = _use0[1];
          return new Ok(
            [new FunctionParameter(label, parameter, type_$1), tokens$2],
          );
        },
      );
    },
  );
}

function function_definition(module, attributes, publicity, name, start, tokens) {
  return expect(
    new $t.LeftParen(),
    tokens,
    (_, tokens) => {
      let result = comma_delimited(
        toList([]),
        tokens,
        function_parameter,
        new $t.RightParen(),
      );
      return $result.try$(
        result,
        (_use0) => {
          let parameters;
          let end;
          let tokens$1;
          parameters = _use0[0];
          end = _use0[1];
          tokens$1 = _use0[2];
          let result$1 = optional_return_annotation(end, tokens$1);
          return $result.try$(
            result$1,
            (_use0) => {
              let return_type;
              let end$1;
              let tokens$2;
              return_type = _use0[0];
              end$1 = _use0[1];
              tokens$2 = _use0[2];
              return $result.try$(
                (() => {
                  if (tokens$2 instanceof $Empty) {
                    return new Ok([toList([]), end$1, tokens$2]);
                  } else {
                    let $ = tokens$2.head[0];
                    if ($ instanceof $t.LeftBrace) {
                      let tokens$3 = tokens$2.tail;
                      return statements(toList([]), tokens$3);
                    } else {
                      return new Ok([toList([]), end$1, tokens$2]);
                    }
                  }
                })(),
                (_use0) => {
                  let body;
                  let end$2;
                  let tokens$3;
                  body = _use0[0];
                  end$2 = _use0[1];
                  tokens$3 = _use0[2];
                  let location = new Span(start, end$2);
                  let function$ = new Function(
                    location,
                    name,
                    publicity,
                    parameters,
                    return_type,
                    body,
                  );
                  let module$1 = push_function(module, attributes, function$);
                  return new Ok([module$1, tokens$3]);
                },
              );
            },
          );
        },
      );
    },
  );
}

function const_definition(module, attributes, publicity, tokens, start) {
  return expect_name(
    tokens,
    (name, tokens) => {
      return $result.try$(
        optional_type_annotation(tokens),
        (_use0) => {
          let annotation;
          let tokens$1;
          annotation = _use0[0];
          tokens$1 = _use0[1];
          return expect(
            new $t.Equal(),
            tokens$1,
            (_, tokens) => {
              return $result.try$(
                expression(tokens),
                (_use0) => {
                  let expression$1;
                  let tokens$1;
                  expression$1 = _use0[0];
                  tokens$1 = _use0[1];
                  let constant = new Constant(
                    new Span(start, expression$1.location.end),
                    name,
                    publicity,
                    annotation,
                    expression$1,
                  );
                  let module$1 = push_constant(module, attributes, constant);
                  return new Ok([module$1, tokens$1]);
                },
              );
            },
          );
        },
      );
    },
  );
}

function type_alias(
  module,
  attributes,
  name,
  parameters,
  publicity,
  start,
  tokens
) {
  return $result.try$(
    type_(tokens),
    (_use0) => {
      let type_$1;
      let tokens$1;
      type_$1 = _use0[0];
      tokens$1 = _use0[1];
      let span = new Span(start, type_$1.location.end);
      let alias = new TypeAlias(span, name, publicity, parameters, type_$1);
      let module$1 = push_type_alias(module, attributes, alias);
      return new Ok([module$1, tokens$1]);
    },
  );
}

function fn_type(start, tokens) {
  let result = comma_delimited(toList([]), tokens, type_, new $t.RightParen());
  return $result.try$(
    result,
    (_use0) => {
      let parameters;
      let tokens$1;
      parameters = _use0[0];
      tokens$1 = _use0[2];
      return expect(
        new $t.RightArrow(),
        tokens$1,
        (_, tokens) => {
          return $result.try$(
            type_(tokens),
            (_use0) => {
              let return$;
              let tokens$1;
              return$ = _use0[0];
              tokens$1 = _use0[1];
              let span = new Span(start, return$.location.end);
              return new Ok(
                [new FunctionType(span, parameters, return$), tokens$1],
              );
            },
          );
        },
      );
    },
  );
}

function tuple_type(start, tokens) {
  let result = comma_delimited(toList([]), tokens, type_, new $t.RightParen());
  return $result.try$(
    result,
    (_use0) => {
      let types;
      let end;
      let tokens$1;
      types = _use0[0];
      end = _use0[1];
      tokens$1 = _use0[2];
      let span = new Span(start, end);
      return new Ok([new TupleType(span, types), tokens$1]);
    },
  );
}

function variant_field(tokens) {
  if (tokens instanceof $Empty) {
    let tokens$1 = tokens;
    return $result.try$(
      type_(tokens$1),
      (_use0) => {
        let type_$1;
        let tokens$2;
        type_$1 = _use0[0];
        tokens$2 = _use0[1];
        return new Ok([new UnlabelledVariantField(type_$1), tokens$2]);
      },
    );
  } else {
    let $ = tokens.tail;
    if ($ instanceof $Empty) {
      let tokens$1 = tokens;
      return $result.try$(
        type_(tokens$1),
        (_use0) => {
          let type_$1;
          let tokens$2;
          type_$1 = _use0[0];
          tokens$2 = _use0[1];
          return new Ok([new UnlabelledVariantField(type_$1), tokens$2]);
        },
      );
    } else {
      let $1 = tokens.head[0];
      if ($1 instanceof $t.Name) {
        let $2 = $.head[0];
        if ($2 instanceof $t.Colon) {
          let tokens$1 = $.tail;
          let name$1 = $1[0];
          return $result.try$(
            type_(tokens$1),
            (_use0) => {
              let type_$1;
              let tokens$2;
              type_$1 = _use0[0];
              tokens$2 = _use0[1];
              return new Ok(
                [new LabelledVariantField(type_$1, name$1), tokens$2],
              );
            },
          );
        } else {
          let tokens$1 = tokens;
          return $result.try$(
            type_(tokens$1),
            (_use0) => {
              let type_$1;
              let tokens$2;
              type_$1 = _use0[0];
              tokens$2 = _use0[1];
              return new Ok([new UnlabelledVariantField(type_$1), tokens$2]);
            },
          );
        }
      } else {
        let tokens$1 = tokens;
        return $result.try$(
          type_(tokens$1),
          (_use0) => {
            let type_$1;
            let tokens$2;
            type_$1 = _use0[0];
            tokens$2 = _use0[1];
            return new Ok([new UnlabelledVariantField(type_$1), tokens$2]);
          },
        );
      }
    }
  }
}

function variants(ct, tokens) {
  return until(
    new $t.RightBrace(),
    ct,
    tokens,
    (ct, tokens) => {
      return $result.try$(
        attributes(toList([]), tokens),
        (_use0) => {
          let attributes$1;
          let tokens$1;
          attributes$1 = _use0[0];
          tokens$1 = _use0[1];
          return expect_upper_name(
            tokens$1,
            (name, _, tokens) => {
              return $result.try$(
                (() => {
                  if (tokens instanceof $Empty) {
                    return new Ok([toList([]), 0, tokens]);
                  } else {
                    let $ = tokens.tail;
                    if ($ instanceof $Empty) {
                      let $1 = tokens.head[0];
                      if ($1 instanceof $t.LeftParen) {
                        let tokens$1 = $;
                        return comma_delimited(
                          toList([]),
                          tokens$1,
                          variant_field,
                          new $t.RightParen(),
                        );
                      } else {
                        return new Ok([toList([]), 0, tokens]);
                      }
                    } else {
                      let $1 = tokens.head[0];
                      if ($1 instanceof $t.LeftParen) {
                        let $2 = $.head[0];
                        if ($2 instanceof $t.RightParen) {
                          let tokens$1 = $.tail;
                          let i = $.head[1].byte_offset;
                          return new Ok([toList([]), i, tokens$1]);
                        } else {
                          let tokens$1 = $;
                          return comma_delimited(
                            toList([]),
                            tokens$1,
                            variant_field,
                            new $t.RightParen(),
                          );
                        }
                      } else {
                        return new Ok([toList([]), 0, tokens]);
                      }
                    }
                  }
                })(),
                (_use0) => {
                  let fields;
                  let tokens$1;
                  fields = _use0[0];
                  tokens$1 = _use0[2];
                  let ct$1 = push_variant(
                    ct,
                    new Variant(name, fields, attributes$1),
                  );
                  return new Ok([ct$1, tokens$1]);
                },
              );
            },
          );
        },
      );
    },
  );
}

function custom_type(
  module,
  attributes,
  name,
  parameters,
  publicity,
  opaque_,
  tokens,
  start
) {
  let ct = new CustomType(
    new Span(0, 0),
    name,
    publicity,
    opaque_,
    parameters,
    toList([]),
  );
  return $result.try$(
    variants(ct, tokens),
    (_use0) => {
      let ct$1;
      let end;
      let tokens$1;
      ct$1 = _use0[0];
      end = _use0[1];
      tokens$1 = _use0[2];
      let ct$2 = new CustomType(
        new Span(start, end),
        ct$1.name,
        ct$1.publicity,
        ct$1.opaque_,
        ct$1.parameters,
        ct$1.variants,
      );
      let module$1 = push_custom_type(module, attributes, ct$2);
      return new Ok([module$1, tokens$1]);
    },
  );
}

function type_definition(module, attributes, publicity, opaque_, tokens, start) {
  return expect_upper_name(
    tokens,
    (name_value, name_start, tokens) => {
      return $result.try$(
        (() => {
          if (tokens instanceof $Empty) {
            return new Ok(
              [toList([]), string_offset(name_start, name_value), tokens],
            );
          } else {
            let $ = tokens.head[0];
            if ($ instanceof $t.LeftParen) {
              let tokens$1 = tokens.tail;
              return comma_delimited(
                toList([]),
                tokens$1,
                name,
                new $t.RightParen(),
              );
            } else {
              return new Ok(
                [toList([]), string_offset(name_start, name_value), tokens],
              );
            }
          }
        })(),
        (_use0) => {
          let parameters;
          let end;
          let tokens$1;
          parameters = _use0[0];
          end = _use0[1];
          tokens$1 = _use0[2];
          if (tokens$1 instanceof $Empty) {
            let span = new Span(start, end);
            let ct = new CustomType(
              span,
              name_value,
              publicity,
              opaque_,
              parameters,
              toList([]),
            );
            let module$1 = push_custom_type(module, attributes, ct);
            return new Ok([module$1, tokens$1]);
          } else {
            let $ = tokens$1.head[0];
            if ($ instanceof $t.LeftBrace) {
              let tokens$2 = tokens$1.tail;
              let _pipe = module;
              return custom_type(
                _pipe,
                attributes,
                name_value,
                parameters,
                publicity,
                opaque_,
                tokens$2,
                start,
              );
            } else if ($ instanceof $t.Equal) {
              let tokens$2 = tokens$1.tail;
              return type_alias(
                module,
                attributes,
                name_value,
                parameters,
                publicity,
                start,
                tokens$2,
              );
            } else {
              let span = new Span(start, end);
              let ct = new CustomType(
                span,
                name_value,
                publicity,
                opaque_,
                parameters,
                toList([]),
              );
              let module$1 = push_custom_type(module, attributes, ct);
              return new Ok([module$1, tokens$1]);
            }
          }
        },
      );
    },
  );
}

function slurp(module, attributes, tokens) {
  if (tokens instanceof $Empty) {
    return new Ok(module);
  } else {
    let $ = tokens.head[0];
    if ($ instanceof $t.Const) {
      let tokens$1 = tokens.tail;
      let start = tokens.head[1].byte_offset;
      let result = const_definition(
        module,
        attributes,
        new Private(),
        tokens$1,
        start,
      );
      return $result.try$(
        result,
        (_use0) => {
          let module$1;
          let tokens$2;
          module$1 = _use0[0];
          tokens$2 = _use0[1];
          return slurp(module$1, toList([]), tokens$2);
        },
      );
    } else if ($ instanceof $t.Fn) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        let tokens$1 = tokens;
        return unexpected_error(tokens$1);
      } else {
        let $2 = $1.head[0];
        if ($2 instanceof $t.Name) {
          let start = tokens.head[1];
          let tokens$1 = $1.tail;
          let name$1 = $2[0];
          let start$1;
          start$1 = start.byte_offset;
          let result = function_definition(
            module,
            attributes,
            new Private(),
            name$1,
            start$1,
            tokens$1,
          );
          return $result.try$(
            result,
            (_use0) => {
              let module$1;
              let tokens$2;
              module$1 = _use0[0];
              tokens$2 = _use0[1];
              return slurp(module$1, toList([]), tokens$2);
            },
          );
        } else {
          let tokens$1 = tokens;
          return unexpected_error(tokens$1);
        }
      }
    } else if ($ instanceof $t.Import) {
      let tokens$1 = tokens.tail;
      let start = tokens.head[1].byte_offset;
      let result = import_statement(module, attributes, tokens$1, start);
      return $result.try$(
        result,
        (_use0) => {
          let module$1;
          let tokens$2;
          module$1 = _use0[0];
          tokens$2 = _use0[1];
          return slurp(module$1, toList([]), tokens$2);
        },
      );
    } else if ($ instanceof $t.Pub) {
      let $1 = tokens.tail;
      if ($1 instanceof $Empty) {
        let tokens$1 = tokens;
        return unexpected_error(tokens$1);
      } else {
        let $2 = $1.head[0];
        if ($2 instanceof $t.Const) {
          let start = tokens.head[1].byte_offset;
          let tokens$1 = $1.tail;
          let result = const_definition(
            module,
            attributes,
            new Public(),
            tokens$1,
            start,
          );
          return $result.try$(
            result,
            (_use0) => {
              let module$1;
              let tokens$2;
              module$1 = _use0[0];
              tokens$2 = _use0[1];
              return slurp(module$1, toList([]), tokens$2);
            },
          );
        } else if ($2 instanceof $t.Fn) {
          let $3 = $1.tail;
          if ($3 instanceof $Empty) {
            let tokens$1 = tokens;
            return unexpected_error(tokens$1);
          } else {
            let $4 = $3.head[0];
            if ($4 instanceof $t.Name) {
              let start = tokens.head[1];
              let tokens$1 = $3.tail;
              let name$1 = $4[0];
              let start$1;
              start$1 = start.byte_offset;
              let result = function_definition(
                module,
                attributes,
                new Public(),
                name$1,
                start$1,
                tokens$1,
              );
              return $result.try$(
                result,
                (_use0) => {
                  let module$1;
                  let tokens$2;
                  module$1 = _use0[0];
                  tokens$2 = _use0[1];
                  return slurp(module$1, toList([]), tokens$2);
                },
              );
            } else {
              let tokens$1 = tokens;
              return unexpected_error(tokens$1);
            }
          }
        } else if ($2 instanceof $t.Opaque) {
          let $3 = $1.tail;
          if ($3 instanceof $Empty) {
            let tokens$1 = tokens;
            return unexpected_error(tokens$1);
          } else {
            let $4 = $3.head[0];
            if ($4 instanceof $t.Type) {
              let start = tokens.head[1].byte_offset;
              let tokens$1 = $3.tail;
              let result = type_definition(
                module,
                attributes,
                new Public(),
                true,
                tokens$1,
                start,
              );
              return $result.try$(
                result,
                (_use0) => {
                  let module$1;
                  let tokens$2;
                  module$1 = _use0[0];
                  tokens$2 = _use0[1];
                  return slurp(module$1, toList([]), tokens$2);
                },
              );
            } else {
              let tokens$1 = tokens;
              return unexpected_error(tokens$1);
            }
          }
        } else if ($2 instanceof $t.Type) {
          let start = tokens.head[1].byte_offset;
          let tokens$1 = $1.tail;
          let result = type_definition(
            module,
            attributes,
            new Public(),
            false,
            tokens$1,
            start,
          );
          return $result.try$(
            result,
            (_use0) => {
              let module$1;
              let tokens$2;
              module$1 = _use0[0];
              tokens$2 = _use0[1];
              return slurp(module$1, toList([]), tokens$2);
            },
          );
        } else {
          let tokens$1 = tokens;
          return unexpected_error(tokens$1);
        }
      }
    } else if ($ instanceof $t.Type) {
      let tokens$1 = tokens.tail;
      let start = tokens.head[1].byte_offset;
      let result = type_definition(
        module,
        attributes,
        new Private(),
        false,
        tokens$1,
        start,
      );
      return $result.try$(
        result,
        (_use0) => {
          let module$1;
          let tokens$2;
          module$1 = _use0[0];
          tokens$2 = _use0[1];
          return slurp(module$1, toList([]), tokens$2);
        },
      );
    } else if ($ instanceof $t.At) {
      let tokens$1 = tokens.tail;
      return $result.try$(
        attribute(tokens$1),
        (_use0) => {
          let attribute$1;
          let tokens$2;
          attribute$1 = _use0[0];
          tokens$2 = _use0[1];
          return slurp(module, listPrepend(attribute$1, attributes), tokens$2);
        },
      );
    } else {
      let tokens$1 = tokens;
      return unexpected_error(tokens$1);
    }
  }
}

export function module(src) {
  let _pipe = $glexer.new$(src);
  let _pipe$1 = $glexer.discard_comments(_pipe);
  let _pipe$2 = $glexer.discard_whitespace(_pipe$1);
  let _pipe$3 = $glexer.lex(_pipe$2);
  return ((_capture) => {
    return slurp(
      new Module(toList([]), toList([]), toList([]), toList([]), toList([])),
      toList([]),
      _capture,
    );
  })(_pipe$3);
}
