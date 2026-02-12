import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class Document extends $CustomType {
  constructor(definitions) {
    super();
    this.definitions = definitions;
  }
}
export const Document$Document = (definitions) => new Document(definitions);
export const Document$isDocument = (value) => value instanceof Document;
export const Document$Document$definitions = (value) => value.definitions;
export const Document$Document$0 = (value) => value.definitions;

export class OperationDefinition extends $CustomType {
  constructor(operation) {
    super();
    this.operation = operation;
  }
}
export const Definition$OperationDefinition = (operation) =>
  new OperationDefinition(operation);
export const Definition$isOperationDefinition = (value) =>
  value instanceof OperationDefinition;
export const Definition$OperationDefinition$operation = (value) =>
  value.operation;
export const Definition$OperationDefinition$0 = (value) => value.operation;

export class FragmentDefinition extends $CustomType {
  constructor(fragment) {
    super();
    this.fragment = fragment;
  }
}
export const Definition$FragmentDefinition = (fragment) =>
  new FragmentDefinition(fragment);
export const Definition$isFragmentDefinition = (value) =>
  value instanceof FragmentDefinition;
export const Definition$FragmentDefinition$fragment = (value) => value.fragment;
export const Definition$FragmentDefinition$0 = (value) => value.fragment;

export class Operation extends $CustomType {
  constructor(operation_type, name, variable_definitions, directives, selection_set) {
    super();
    this.operation_type = operation_type;
    this.name = name;
    this.variable_definitions = variable_definitions;
    this.directives = directives;
    this.selection_set = selection_set;
  }
}
export const Operation$Operation = (operation_type, name, variable_definitions, directives, selection_set) =>
  new Operation(operation_type,
  name,
  variable_definitions,
  directives,
  selection_set);
export const Operation$isOperation = (value) => value instanceof Operation;
export const Operation$Operation$operation_type = (value) =>
  value.operation_type;
export const Operation$Operation$0 = (value) => value.operation_type;
export const Operation$Operation$name = (value) => value.name;
export const Operation$Operation$1 = (value) => value.name;
export const Operation$Operation$variable_definitions = (value) =>
  value.variable_definitions;
export const Operation$Operation$2 = (value) => value.variable_definitions;
export const Operation$Operation$directives = (value) => value.directives;
export const Operation$Operation$3 = (value) => value.directives;
export const Operation$Operation$selection_set = (value) => value.selection_set;
export const Operation$Operation$4 = (value) => value.selection_set;

export class ShorthandQuery extends $CustomType {
  constructor(selection_set) {
    super();
    this.selection_set = selection_set;
  }
}
export const Operation$ShorthandQuery = (selection_set) =>
  new ShorthandQuery(selection_set);
export const Operation$isShorthandQuery = (value) =>
  value instanceof ShorthandQuery;
export const Operation$ShorthandQuery$selection_set = (value) =>
  value.selection_set;
export const Operation$ShorthandQuery$0 = (value) => value.selection_set;

export class Query extends $CustomType {}
export const OperationType$Query = () => new Query();
export const OperationType$isQuery = (value) => value instanceof Query;

export class Mutation extends $CustomType {}
export const OperationType$Mutation = () => new Mutation();
export const OperationType$isMutation = (value) => value instanceof Mutation;

export class Subscription extends $CustomType {}
export const OperationType$Subscription = () => new Subscription();
export const OperationType$isSubscription = (value) =>
  value instanceof Subscription;

export class SelectionSet extends $CustomType {
  constructor(selections) {
    super();
    this.selections = selections;
  }
}
export const SelectionSet$SelectionSet = (selections) =>
  new SelectionSet(selections);
export const SelectionSet$isSelectionSet = (value) =>
  value instanceof SelectionSet;
export const SelectionSet$SelectionSet$selections = (value) => value.selections;
export const SelectionSet$SelectionSet$0 = (value) => value.selections;

export class FieldSelection extends $CustomType {
  constructor(field) {
    super();
    this.field = field;
  }
}
export const Selection$FieldSelection = (field) => new FieldSelection(field);
export const Selection$isFieldSelection = (value) =>
  value instanceof FieldSelection;
export const Selection$FieldSelection$field = (value) => value.field;
export const Selection$FieldSelection$0 = (value) => value.field;

export class FragmentSpread extends $CustomType {
  constructor(fragment_spread) {
    super();
    this.fragment_spread = fragment_spread;
  }
}
export const Selection$FragmentSpread = (fragment_spread) =>
  new FragmentSpread(fragment_spread);
export const Selection$isFragmentSpread = (value) =>
  value instanceof FragmentSpread;
export const Selection$FragmentSpread$fragment_spread = (value) =>
  value.fragment_spread;
export const Selection$FragmentSpread$0 = (value) => value.fragment_spread;

export class InlineFragment extends $CustomType {
  constructor(inline_fragment) {
    super();
    this.inline_fragment = inline_fragment;
  }
}
export const Selection$InlineFragment = (inline_fragment) =>
  new InlineFragment(inline_fragment);
export const Selection$isInlineFragment = (value) =>
  value instanceof InlineFragment;
export const Selection$InlineFragment$inline_fragment = (value) =>
  value.inline_fragment;
export const Selection$InlineFragment$0 = (value) => value.inline_fragment;

export class Field extends $CustomType {
  constructor(alias, name, arguments$, directives, selection_set) {
    super();
    this.alias = alias;
    this.name = name;
    this.arguments = arguments$;
    this.directives = directives;
    this.selection_set = selection_set;
  }
}
export const Field$Field = (alias, name, arguments$, directives, selection_set) =>
  new Field(alias, name, arguments$, directives, selection_set);
export const Field$isField = (value) => value instanceof Field;
export const Field$Field$alias = (value) => value.alias;
export const Field$Field$0 = (value) => value.alias;
export const Field$Field$name = (value) => value.name;
export const Field$Field$1 = (value) => value.name;
export const Field$Field$arguments = (value) => value.arguments;
export const Field$Field$2 = (value) => value.arguments;
export const Field$Field$directives = (value) => value.directives;
export const Field$Field$3 = (value) => value.directives;
export const Field$Field$selection_set = (value) => value.selection_set;
export const Field$Field$4 = (value) => value.selection_set;

export class Fragment extends $CustomType {
  constructor(name, type_condition, directives, selection_set) {
    super();
    this.name = name;
    this.type_condition = type_condition;
    this.directives = directives;
    this.selection_set = selection_set;
  }
}
export const Fragment$Fragment = (name, type_condition, directives, selection_set) =>
  new Fragment(name, type_condition, directives, selection_set);
export const Fragment$isFragment = (value) => value instanceof Fragment;
export const Fragment$Fragment$name = (value) => value.name;
export const Fragment$Fragment$0 = (value) => value.name;
export const Fragment$Fragment$type_condition = (value) => value.type_condition;
export const Fragment$Fragment$1 = (value) => value.type_condition;
export const Fragment$Fragment$directives = (value) => value.directives;
export const Fragment$Fragment$2 = (value) => value.directives;
export const Fragment$Fragment$selection_set = (value) => value.selection_set;
export const Fragment$Fragment$3 = (value) => value.selection_set;

export class FragmentSpreadValue extends $CustomType {
  constructor(name, directives) {
    super();
    this.name = name;
    this.directives = directives;
  }
}
export const FragmentSpreadValue$FragmentSpreadValue = (name, directives) =>
  new FragmentSpreadValue(name, directives);
export const FragmentSpreadValue$isFragmentSpreadValue = (value) =>
  value instanceof FragmentSpreadValue;
export const FragmentSpreadValue$FragmentSpreadValue$name = (value) =>
  value.name;
export const FragmentSpreadValue$FragmentSpreadValue$0 = (value) => value.name;
export const FragmentSpreadValue$FragmentSpreadValue$directives = (value) =>
  value.directives;
export const FragmentSpreadValue$FragmentSpreadValue$1 = (value) =>
  value.directives;

export class InlineFragmentValue extends $CustomType {
  constructor(type_condition, directives, selection_set) {
    super();
    this.type_condition = type_condition;
    this.directives = directives;
    this.selection_set = selection_set;
  }
}
export const InlineFragmentValue$InlineFragmentValue = (type_condition, directives, selection_set) =>
  new InlineFragmentValue(type_condition, directives, selection_set);
export const InlineFragmentValue$isInlineFragmentValue = (value) =>
  value instanceof InlineFragmentValue;
export const InlineFragmentValue$InlineFragmentValue$type_condition = (value) =>
  value.type_condition;
export const InlineFragmentValue$InlineFragmentValue$0 = (value) =>
  value.type_condition;
export const InlineFragmentValue$InlineFragmentValue$directives = (value) =>
  value.directives;
export const InlineFragmentValue$InlineFragmentValue$1 = (value) =>
  value.directives;
export const InlineFragmentValue$InlineFragmentValue$selection_set = (value) =>
  value.selection_set;
export const InlineFragmentValue$InlineFragmentValue$2 = (value) =>
  value.selection_set;

export class VariableDefinition extends $CustomType {
  constructor(variable, type_, default_value, directives) {
    super();
    this.variable = variable;
    this.type_ = type_;
    this.default_value = default_value;
    this.directives = directives;
  }
}
export const VariableDefinition$VariableDefinition = (variable, type_, default_value, directives) =>
  new VariableDefinition(variable, type_, default_value, directives);
export const VariableDefinition$isVariableDefinition = (value) =>
  value instanceof VariableDefinition;
export const VariableDefinition$VariableDefinition$variable = (value) =>
  value.variable;
export const VariableDefinition$VariableDefinition$0 = (value) =>
  value.variable;
export const VariableDefinition$VariableDefinition$type_ = (value) =>
  value.type_;
export const VariableDefinition$VariableDefinition$1 = (value) => value.type_;
export const VariableDefinition$VariableDefinition$default_value = (value) =>
  value.default_value;
export const VariableDefinition$VariableDefinition$2 = (value) =>
  value.default_value;
export const VariableDefinition$VariableDefinition$directives = (value) =>
  value.directives;
export const VariableDefinition$VariableDefinition$3 = (value) =>
  value.directives;

export class Argument extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}
export const Argument$Argument = (name, value) => new Argument(name, value);
export const Argument$isArgument = (value) => value instanceof Argument;
export const Argument$Argument$name = (value) => value.name;
export const Argument$Argument$0 = (value) => value.name;
export const Argument$Argument$value = (value) => value.value;
export const Argument$Argument$1 = (value) => value.value;

export class Directive extends $CustomType {
  constructor(name, arguments$) {
    super();
    this.name = name;
    this.arguments = arguments$;
  }
}
export const Directive$Directive = (name, arguments$) =>
  new Directive(name, arguments$);
export const Directive$isDirective = (value) => value instanceof Directive;
export const Directive$Directive$name = (value) => value.name;
export const Directive$Directive$0 = (value) => value.name;
export const Directive$Directive$arguments = (value) => value.arguments;
export const Directive$Directive$1 = (value) => value.arguments;

export class NamedType extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const Type$NamedType = (name) => new NamedType(name);
export const Type$isNamedType = (value) => value instanceof NamedType;
export const Type$NamedType$name = (value) => value.name;
export const Type$NamedType$0 = (value) => value.name;

export class ListType extends $CustomType {
  constructor(inner_type) {
    super();
    this.inner_type = inner_type;
  }
}
export const Type$ListType = (inner_type) => new ListType(inner_type);
export const Type$isListType = (value) => value instanceof ListType;
export const Type$ListType$inner_type = (value) => value.inner_type;
export const Type$ListType$0 = (value) => value.inner_type;

export class NonNullType extends $CustomType {
  constructor(inner_type) {
    super();
    this.inner_type = inner_type;
  }
}
export const Type$NonNullType = (inner_type) => new NonNullType(inner_type);
export const Type$isNonNullType = (value) => value instanceof NonNullType;
export const Type$NonNullType$inner_type = (value) => value.inner_type;
export const Type$NonNullType$0 = (value) => value.inner_type;

export class IntValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Value$IntValue = (value) => new IntValue(value);
export const Value$isIntValue = (value) => value instanceof IntValue;
export const Value$IntValue$value = (value) => value.value;
export const Value$IntValue$0 = (value) => value.value;

export class FloatValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Value$FloatValue = (value) => new FloatValue(value);
export const Value$isFloatValue = (value) => value instanceof FloatValue;
export const Value$FloatValue$value = (value) => value.value;
export const Value$FloatValue$0 = (value) => value.value;

export class StringValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Value$StringValue = (value) => new StringValue(value);
export const Value$isStringValue = (value) => value instanceof StringValue;
export const Value$StringValue$value = (value) => value.value;
export const Value$StringValue$0 = (value) => value.value;

export class BooleanValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Value$BooleanValue = (value) => new BooleanValue(value);
export const Value$isBooleanValue = (value) => value instanceof BooleanValue;
export const Value$BooleanValue$value = (value) => value.value;
export const Value$BooleanValue$0 = (value) => value.value;

export class NullValue extends $CustomType {}
export const Value$NullValue = () => new NullValue();
export const Value$isNullValue = (value) => value instanceof NullValue;

export class EnumValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const Value$EnumValue = (value) => new EnumValue(value);
export const Value$isEnumValue = (value) => value instanceof EnumValue;
export const Value$EnumValue$value = (value) => value.value;
export const Value$EnumValue$0 = (value) => value.value;

export class ListValue extends $CustomType {
  constructor(values) {
    super();
    this.values = values;
  }
}
export const Value$ListValue = (values) => new ListValue(values);
export const Value$isListValue = (value) => value instanceof ListValue;
export const Value$ListValue$values = (value) => value.values;
export const Value$ListValue$0 = (value) => value.values;

export class ObjectValue extends $CustomType {
  constructor(fields) {
    super();
    this.fields = fields;
  }
}
export const Value$ObjectValue = (fields) => new ObjectValue(fields);
export const Value$isObjectValue = (value) => value instanceof ObjectValue;
export const Value$ObjectValue$fields = (value) => value.fields;
export const Value$ObjectValue$0 = (value) => value.fields;

export class VariableValue extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const Value$VariableValue = (name) => new VariableValue(name);
export const Value$isVariableValue = (value) => value instanceof VariableValue;
export const Value$VariableValue$name = (value) => value.name;
export const Value$VariableValue$0 = (value) => value.name;

export class ObjectField extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}
export const ObjectField$ObjectField = (name, value) =>
  new ObjectField(name, value);
export const ObjectField$isObjectField = (value) =>
  value instanceof ObjectField;
export const ObjectField$ObjectField$name = (value) => value.name;
export const ObjectField$ObjectField$0 = (value) => value.name;
export const ObjectField$ObjectField$value = (value) => value.value;
export const ObjectField$ObjectField$1 = (value) => value.value;
