import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class SDLDocument extends $CustomType {
  constructor(definitions) {
    super();
    this.definitions = definitions;
  }
}
export const SDLDocument$SDLDocument = (definitions) =>
  new SDLDocument(definitions);
export const SDLDocument$isSDLDocument = (value) =>
  value instanceof SDLDocument;
export const SDLDocument$SDLDocument$definitions = (value) => value.definitions;
export const SDLDocument$SDLDocument$0 = (value) => value.definitions;

export class TypeDefinition extends $CustomType {
  constructor(type_def) {
    super();
    this.type_def = type_def;
  }
}
export const TypeSystemDefinition$TypeDefinition = (type_def) =>
  new TypeDefinition(type_def);
export const TypeSystemDefinition$isTypeDefinition = (value) =>
  value instanceof TypeDefinition;
export const TypeSystemDefinition$TypeDefinition$type_def = (value) =>
  value.type_def;
export const TypeSystemDefinition$TypeDefinition$0 = (value) => value.type_def;

export class DirectiveDefinition extends $CustomType {
  constructor(directive_def) {
    super();
    this.directive_def = directive_def;
  }
}
export const TypeSystemDefinition$DirectiveDefinition = (directive_def) =>
  new DirectiveDefinition(directive_def);
export const TypeSystemDefinition$isDirectiveDefinition = (value) =>
  value instanceof DirectiveDefinition;
export const TypeSystemDefinition$DirectiveDefinition$directive_def = (value) =>
  value.directive_def;
export const TypeSystemDefinition$DirectiveDefinition$0 = (value) =>
  value.directive_def;

export class SchemaDefinition extends $CustomType {
  constructor(schema_def) {
    super();
    this.schema_def = schema_def;
  }
}
export const TypeSystemDefinition$SchemaDefinition = (schema_def) =>
  new SchemaDefinition(schema_def);
export const TypeSystemDefinition$isSchemaDefinition = (value) =>
  value instanceof SchemaDefinition;
export const TypeSystemDefinition$SchemaDefinition$schema_def = (value) =>
  value.schema_def;
export const TypeSystemDefinition$SchemaDefinition$0 = (value) =>
  value.schema_def;

export class ObjectTypeDefinition extends $CustomType {
  constructor(object_def) {
    super();
    this.object_def = object_def;
  }
}
export const TypeDef$ObjectTypeDefinition = (object_def) =>
  new ObjectTypeDefinition(object_def);
export const TypeDef$isObjectTypeDefinition = (value) =>
  value instanceof ObjectTypeDefinition;
export const TypeDef$ObjectTypeDefinition$object_def = (value) =>
  value.object_def;
export const TypeDef$ObjectTypeDefinition$0 = (value) => value.object_def;

export class InterfaceTypeDefinition extends $CustomType {
  constructor(interface_def) {
    super();
    this.interface_def = interface_def;
  }
}
export const TypeDef$InterfaceTypeDefinition = (interface_def) =>
  new InterfaceTypeDefinition(interface_def);
export const TypeDef$isInterfaceTypeDefinition = (value) =>
  value instanceof InterfaceTypeDefinition;
export const TypeDef$InterfaceTypeDefinition$interface_def = (value) =>
  value.interface_def;
export const TypeDef$InterfaceTypeDefinition$0 = (value) => value.interface_def;

export class UnionTypeDefinition extends $CustomType {
  constructor(union_def) {
    super();
    this.union_def = union_def;
  }
}
export const TypeDef$UnionTypeDefinition = (union_def) =>
  new UnionTypeDefinition(union_def);
export const TypeDef$isUnionTypeDefinition = (value) =>
  value instanceof UnionTypeDefinition;
export const TypeDef$UnionTypeDefinition$union_def = (value) => value.union_def;
export const TypeDef$UnionTypeDefinition$0 = (value) => value.union_def;

export class ScalarTypeDefinition extends $CustomType {
  constructor(scalar_def) {
    super();
    this.scalar_def = scalar_def;
  }
}
export const TypeDef$ScalarTypeDefinition = (scalar_def) =>
  new ScalarTypeDefinition(scalar_def);
export const TypeDef$isScalarTypeDefinition = (value) =>
  value instanceof ScalarTypeDefinition;
export const TypeDef$ScalarTypeDefinition$scalar_def = (value) =>
  value.scalar_def;
export const TypeDef$ScalarTypeDefinition$0 = (value) => value.scalar_def;

export class EnumTypeDefinition extends $CustomType {
  constructor(enum_def) {
    super();
    this.enum_def = enum_def;
  }
}
export const TypeDef$EnumTypeDefinition = (enum_def) =>
  new EnumTypeDefinition(enum_def);
export const TypeDef$isEnumTypeDefinition = (value) =>
  value instanceof EnumTypeDefinition;
export const TypeDef$EnumTypeDefinition$enum_def = (value) => value.enum_def;
export const TypeDef$EnumTypeDefinition$0 = (value) => value.enum_def;

export class InputObjectTypeDefinition extends $CustomType {
  constructor(input_def) {
    super();
    this.input_def = input_def;
  }
}
export const TypeDef$InputObjectTypeDefinition = (input_def) =>
  new InputObjectTypeDefinition(input_def);
export const TypeDef$isInputObjectTypeDefinition = (value) =>
  value instanceof InputObjectTypeDefinition;
export const TypeDef$InputObjectTypeDefinition$input_def = (value) =>
  value.input_def;
export const TypeDef$InputObjectTypeDefinition$0 = (value) => value.input_def;

export class ObjectTypeDef extends $CustomType {
  constructor(name, description, interfaces, directives, fields) {
    super();
    this.name = name;
    this.description = description;
    this.interfaces = interfaces;
    this.directives = directives;
    this.fields = fields;
  }
}
export const ObjectTypeDef$ObjectTypeDef = (name, description, interfaces, directives, fields) =>
  new ObjectTypeDef(name, description, interfaces, directives, fields);
export const ObjectTypeDef$isObjectTypeDef = (value) =>
  value instanceof ObjectTypeDef;
export const ObjectTypeDef$ObjectTypeDef$name = (value) => value.name;
export const ObjectTypeDef$ObjectTypeDef$0 = (value) => value.name;
export const ObjectTypeDef$ObjectTypeDef$description = (value) =>
  value.description;
export const ObjectTypeDef$ObjectTypeDef$1 = (value) => value.description;
export const ObjectTypeDef$ObjectTypeDef$interfaces = (value) =>
  value.interfaces;
export const ObjectTypeDef$ObjectTypeDef$2 = (value) => value.interfaces;
export const ObjectTypeDef$ObjectTypeDef$directives = (value) =>
  value.directives;
export const ObjectTypeDef$ObjectTypeDef$3 = (value) => value.directives;
export const ObjectTypeDef$ObjectTypeDef$fields = (value) => value.fields;
export const ObjectTypeDef$ObjectTypeDef$4 = (value) => value.fields;

export class FieldDef extends $CustomType {
  constructor(name, description, arguments$, field_type, directives) {
    super();
    this.name = name;
    this.description = description;
    this.arguments = arguments$;
    this.field_type = field_type;
    this.directives = directives;
  }
}
export const FieldDef$FieldDef = (name, description, arguments$, field_type, directives) =>
  new FieldDef(name, description, arguments$, field_type, directives);
export const FieldDef$isFieldDef = (value) => value instanceof FieldDef;
export const FieldDef$FieldDef$name = (value) => value.name;
export const FieldDef$FieldDef$0 = (value) => value.name;
export const FieldDef$FieldDef$description = (value) => value.description;
export const FieldDef$FieldDef$1 = (value) => value.description;
export const FieldDef$FieldDef$arguments = (value) => value.arguments;
export const FieldDef$FieldDef$2 = (value) => value.arguments;
export const FieldDef$FieldDef$field_type = (value) => value.field_type;
export const FieldDef$FieldDef$3 = (value) => value.field_type;
export const FieldDef$FieldDef$directives = (value) => value.directives;
export const FieldDef$FieldDef$4 = (value) => value.directives;

export class ArgumentDef extends $CustomType {
  constructor(name, description, arg_type, default_value, directives) {
    super();
    this.name = name;
    this.description = description;
    this.arg_type = arg_type;
    this.default_value = default_value;
    this.directives = directives;
  }
}
export const ArgumentDef$ArgumentDef = (name, description, arg_type, default_value, directives) =>
  new ArgumentDef(name, description, arg_type, default_value, directives);
export const ArgumentDef$isArgumentDef = (value) =>
  value instanceof ArgumentDef;
export const ArgumentDef$ArgumentDef$name = (value) => value.name;
export const ArgumentDef$ArgumentDef$0 = (value) => value.name;
export const ArgumentDef$ArgumentDef$description = (value) => value.description;
export const ArgumentDef$ArgumentDef$1 = (value) => value.description;
export const ArgumentDef$ArgumentDef$arg_type = (value) => value.arg_type;
export const ArgumentDef$ArgumentDef$2 = (value) => value.arg_type;
export const ArgumentDef$ArgumentDef$default_value = (value) =>
  value.default_value;
export const ArgumentDef$ArgumentDef$3 = (value) => value.default_value;
export const ArgumentDef$ArgumentDef$directives = (value) => value.directives;
export const ArgumentDef$ArgumentDef$4 = (value) => value.directives;

export class NamedType extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const SDLType$NamedType = (name) => new NamedType(name);
export const SDLType$isNamedType = (value) => value instanceof NamedType;
export const SDLType$NamedType$name = (value) => value.name;
export const SDLType$NamedType$0 = (value) => value.name;

export class ListType extends $CustomType {
  constructor(inner_type) {
    super();
    this.inner_type = inner_type;
  }
}
export const SDLType$ListType = (inner_type) => new ListType(inner_type);
export const SDLType$isListType = (value) => value instanceof ListType;
export const SDLType$ListType$inner_type = (value) => value.inner_type;
export const SDLType$ListType$0 = (value) => value.inner_type;

export class NonNullType extends $CustomType {
  constructor(inner_type) {
    super();
    this.inner_type = inner_type;
  }
}
export const SDLType$NonNullType = (inner_type) => new NonNullType(inner_type);
export const SDLType$isNonNullType = (value) => value instanceof NonNullType;
export const SDLType$NonNullType$inner_type = (value) => value.inner_type;
export const SDLType$NonNullType$0 = (value) => value.inner_type;

export class IntValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLValue$IntValue = (value) => new IntValue(value);
export const SDLValue$isIntValue = (value) => value instanceof IntValue;
export const SDLValue$IntValue$value = (value) => value.value;
export const SDLValue$IntValue$0 = (value) => value.value;

export class FloatValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLValue$FloatValue = (value) => new FloatValue(value);
export const SDLValue$isFloatValue = (value) => value instanceof FloatValue;
export const SDLValue$FloatValue$value = (value) => value.value;
export const SDLValue$FloatValue$0 = (value) => value.value;

export class StringValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLValue$StringValue = (value) => new StringValue(value);
export const SDLValue$isStringValue = (value) => value instanceof StringValue;
export const SDLValue$StringValue$value = (value) => value.value;
export const SDLValue$StringValue$0 = (value) => value.value;

export class BooleanValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLValue$BooleanValue = (value) => new BooleanValue(value);
export const SDLValue$isBooleanValue = (value) => value instanceof BooleanValue;
export const SDLValue$BooleanValue$value = (value) => value.value;
export const SDLValue$BooleanValue$0 = (value) => value.value;

export class NullValue extends $CustomType {}
export const SDLValue$NullValue = () => new NullValue();
export const SDLValue$isNullValue = (value) => value instanceof NullValue;

export class EnumValue extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}
export const SDLValue$EnumValue = (value) => new EnumValue(value);
export const SDLValue$isEnumValue = (value) => value instanceof EnumValue;
export const SDLValue$EnumValue$value = (value) => value.value;
export const SDLValue$EnumValue$0 = (value) => value.value;

export class ListValue extends $CustomType {
  constructor(values) {
    super();
    this.values = values;
  }
}
export const SDLValue$ListValue = (values) => new ListValue(values);
export const SDLValue$isListValue = (value) => value instanceof ListValue;
export const SDLValue$ListValue$values = (value) => value.values;
export const SDLValue$ListValue$0 = (value) => value.values;

export class ObjectValue extends $CustomType {
  constructor(fields) {
    super();
    this.fields = fields;
  }
}
export const SDLValue$ObjectValue = (fields) => new ObjectValue(fields);
export const SDLValue$isObjectValue = (value) => value instanceof ObjectValue;
export const SDLValue$ObjectValue$fields = (value) => value.fields;
export const SDLValue$ObjectValue$0 = (value) => value.fields;

export class ObjectFieldValue extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}
export const ObjectFieldValue$ObjectFieldValue = (name, value) =>
  new ObjectFieldValue(name, value);
export const ObjectFieldValue$isObjectFieldValue = (value) =>
  value instanceof ObjectFieldValue;
export const ObjectFieldValue$ObjectFieldValue$name = (value) => value.name;
export const ObjectFieldValue$ObjectFieldValue$0 = (value) => value.name;
export const ObjectFieldValue$ObjectFieldValue$value = (value) => value.value;
export const ObjectFieldValue$ObjectFieldValue$1 = (value) => value.value;

export class InterfaceTypeDef extends $CustomType {
  constructor(name, description, directives, fields) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
    this.fields = fields;
  }
}
export const InterfaceTypeDef$InterfaceTypeDef = (name, description, directives, fields) =>
  new InterfaceTypeDef(name, description, directives, fields);
export const InterfaceTypeDef$isInterfaceTypeDef = (value) =>
  value instanceof InterfaceTypeDef;
export const InterfaceTypeDef$InterfaceTypeDef$name = (value) => value.name;
export const InterfaceTypeDef$InterfaceTypeDef$0 = (value) => value.name;
export const InterfaceTypeDef$InterfaceTypeDef$description = (value) =>
  value.description;
export const InterfaceTypeDef$InterfaceTypeDef$1 = (value) => value.description;
export const InterfaceTypeDef$InterfaceTypeDef$directives = (value) =>
  value.directives;
export const InterfaceTypeDef$InterfaceTypeDef$2 = (value) => value.directives;
export const InterfaceTypeDef$InterfaceTypeDef$fields = (value) => value.fields;
export const InterfaceTypeDef$InterfaceTypeDef$3 = (value) => value.fields;

export class UnionTypeDef extends $CustomType {
  constructor(name, description, directives, member_types) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
    this.member_types = member_types;
  }
}
export const UnionTypeDef$UnionTypeDef = (name, description, directives, member_types) =>
  new UnionTypeDef(name, description, directives, member_types);
export const UnionTypeDef$isUnionTypeDef = (value) =>
  value instanceof UnionTypeDef;
export const UnionTypeDef$UnionTypeDef$name = (value) => value.name;
export const UnionTypeDef$UnionTypeDef$0 = (value) => value.name;
export const UnionTypeDef$UnionTypeDef$description = (value) =>
  value.description;
export const UnionTypeDef$UnionTypeDef$1 = (value) => value.description;
export const UnionTypeDef$UnionTypeDef$directives = (value) => value.directives;
export const UnionTypeDef$UnionTypeDef$2 = (value) => value.directives;
export const UnionTypeDef$UnionTypeDef$member_types = (value) =>
  value.member_types;
export const UnionTypeDef$UnionTypeDef$3 = (value) => value.member_types;

export class ScalarTypeDef extends $CustomType {
  constructor(name, description, directives) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
  }
}
export const ScalarTypeDef$ScalarTypeDef = (name, description, directives) =>
  new ScalarTypeDef(name, description, directives);
export const ScalarTypeDef$isScalarTypeDef = (value) =>
  value instanceof ScalarTypeDef;
export const ScalarTypeDef$ScalarTypeDef$name = (value) => value.name;
export const ScalarTypeDef$ScalarTypeDef$0 = (value) => value.name;
export const ScalarTypeDef$ScalarTypeDef$description = (value) =>
  value.description;
export const ScalarTypeDef$ScalarTypeDef$1 = (value) => value.description;
export const ScalarTypeDef$ScalarTypeDef$directives = (value) =>
  value.directives;
export const ScalarTypeDef$ScalarTypeDef$2 = (value) => value.directives;

export class EnumTypeDef extends $CustomType {
  constructor(name, description, directives, values) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
    this.values = values;
  }
}
export const EnumTypeDef$EnumTypeDef = (name, description, directives, values) =>
  new EnumTypeDef(name, description, directives, values);
export const EnumTypeDef$isEnumTypeDef = (value) =>
  value instanceof EnumTypeDef;
export const EnumTypeDef$EnumTypeDef$name = (value) => value.name;
export const EnumTypeDef$EnumTypeDef$0 = (value) => value.name;
export const EnumTypeDef$EnumTypeDef$description = (value) => value.description;
export const EnumTypeDef$EnumTypeDef$1 = (value) => value.description;
export const EnumTypeDef$EnumTypeDef$directives = (value) => value.directives;
export const EnumTypeDef$EnumTypeDef$2 = (value) => value.directives;
export const EnumTypeDef$EnumTypeDef$values = (value) => value.values;
export const EnumTypeDef$EnumTypeDef$3 = (value) => value.values;

export class EnumValueDef extends $CustomType {
  constructor(name, description, directives) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
  }
}
export const EnumValueDef$EnumValueDef = (name, description, directives) =>
  new EnumValueDef(name, description, directives);
export const EnumValueDef$isEnumValueDef = (value) =>
  value instanceof EnumValueDef;
export const EnumValueDef$EnumValueDef$name = (value) => value.name;
export const EnumValueDef$EnumValueDef$0 = (value) => value.name;
export const EnumValueDef$EnumValueDef$description = (value) =>
  value.description;
export const EnumValueDef$EnumValueDef$1 = (value) => value.description;
export const EnumValueDef$EnumValueDef$directives = (value) => value.directives;
export const EnumValueDef$EnumValueDef$2 = (value) => value.directives;

export class InputObjectTypeDef extends $CustomType {
  constructor(name, description, directives, fields) {
    super();
    this.name = name;
    this.description = description;
    this.directives = directives;
    this.fields = fields;
  }
}
export const InputObjectTypeDef$InputObjectTypeDef = (name, description, directives, fields) =>
  new InputObjectTypeDef(name, description, directives, fields);
export const InputObjectTypeDef$isInputObjectTypeDef = (value) =>
  value instanceof InputObjectTypeDef;
export const InputObjectTypeDef$InputObjectTypeDef$name = (value) => value.name;
export const InputObjectTypeDef$InputObjectTypeDef$0 = (value) => value.name;
export const InputObjectTypeDef$InputObjectTypeDef$description = (value) =>
  value.description;
export const InputObjectTypeDef$InputObjectTypeDef$1 = (value) =>
  value.description;
export const InputObjectTypeDef$InputObjectTypeDef$directives = (value) =>
  value.directives;
export const InputObjectTypeDef$InputObjectTypeDef$2 = (value) =>
  value.directives;
export const InputObjectTypeDef$InputObjectTypeDef$fields = (value) =>
  value.fields;
export const InputObjectTypeDef$InputObjectTypeDef$3 = (value) => value.fields;

export class InputFieldDef extends $CustomType {
  constructor(name, description, field_type, default_value, directives) {
    super();
    this.name = name;
    this.description = description;
    this.field_type = field_type;
    this.default_value = default_value;
    this.directives = directives;
  }
}
export const InputFieldDef$InputFieldDef = (name, description, field_type, default_value, directives) =>
  new InputFieldDef(name, description, field_type, default_value, directives);
export const InputFieldDef$isInputFieldDef = (value) =>
  value instanceof InputFieldDef;
export const InputFieldDef$InputFieldDef$name = (value) => value.name;
export const InputFieldDef$InputFieldDef$0 = (value) => value.name;
export const InputFieldDef$InputFieldDef$description = (value) =>
  value.description;
export const InputFieldDef$InputFieldDef$1 = (value) => value.description;
export const InputFieldDef$InputFieldDef$field_type = (value) =>
  value.field_type;
export const InputFieldDef$InputFieldDef$2 = (value) => value.field_type;
export const InputFieldDef$InputFieldDef$default_value = (value) =>
  value.default_value;
export const InputFieldDef$InputFieldDef$3 = (value) => value.default_value;
export const InputFieldDef$InputFieldDef$directives = (value) =>
  value.directives;
export const InputFieldDef$InputFieldDef$4 = (value) => value.directives;

export class DirectiveDef extends $CustomType {
  constructor(name, description, locations, arguments$) {
    super();
    this.name = name;
    this.description = description;
    this.locations = locations;
    this.arguments = arguments$;
  }
}
export const DirectiveDef$DirectiveDef = (name, description, locations, arguments$) =>
  new DirectiveDef(name, description, locations, arguments$);
export const DirectiveDef$isDirectiveDef = (value) =>
  value instanceof DirectiveDef;
export const DirectiveDef$DirectiveDef$name = (value) => value.name;
export const DirectiveDef$DirectiveDef$0 = (value) => value.name;
export const DirectiveDef$DirectiveDef$description = (value) =>
  value.description;
export const DirectiveDef$DirectiveDef$1 = (value) => value.description;
export const DirectiveDef$DirectiveDef$locations = (value) => value.locations;
export const DirectiveDef$DirectiveDef$2 = (value) => value.locations;
export const DirectiveDef$DirectiveDef$arguments = (value) => value.arguments;
export const DirectiveDef$DirectiveDef$3 = (value) => value.arguments;

export class DirectiveUsage extends $CustomType {
  constructor(name, arguments$) {
    super();
    this.name = name;
    this.arguments = arguments$;
  }
}
export const DirectiveUsage$DirectiveUsage = (name, arguments$) =>
  new DirectiveUsage(name, arguments$);
export const DirectiveUsage$isDirectiveUsage = (value) =>
  value instanceof DirectiveUsage;
export const DirectiveUsage$DirectiveUsage$name = (value) => value.name;
export const DirectiveUsage$DirectiveUsage$0 = (value) => value.name;
export const DirectiveUsage$DirectiveUsage$arguments = (value) =>
  value.arguments;
export const DirectiveUsage$DirectiveUsage$1 = (value) => value.arguments;

export class DirectiveArgument extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}
export const DirectiveArgument$DirectiveArgument = (name, value) =>
  new DirectiveArgument(name, value);
export const DirectiveArgument$isDirectiveArgument = (value) =>
  value instanceof DirectiveArgument;
export const DirectiveArgument$DirectiveArgument$name = (value) => value.name;
export const DirectiveArgument$DirectiveArgument$0 = (value) => value.name;
export const DirectiveArgument$DirectiveArgument$value = (value) => value.value;
export const DirectiveArgument$DirectiveArgument$1 = (value) => value.value;

export class QUERY extends $CustomType {}
export const DirectiveLocation$QUERY = () => new QUERY();
export const DirectiveLocation$isQUERY = (value) => value instanceof QUERY;

export class MUTATION extends $CustomType {}
export const DirectiveLocation$MUTATION = () => new MUTATION();
export const DirectiveLocation$isMUTATION = (value) =>
  value instanceof MUTATION;

export class SUBSCRIPTION extends $CustomType {}
export const DirectiveLocation$SUBSCRIPTION = () => new SUBSCRIPTION();
export const DirectiveLocation$isSUBSCRIPTION = (value) =>
  value instanceof SUBSCRIPTION;

export class FIELD extends $CustomType {}
export const DirectiveLocation$FIELD = () => new FIELD();
export const DirectiveLocation$isFIELD = (value) => value instanceof FIELD;

export class FRAGMENTDEFINITION extends $CustomType {}
export const DirectiveLocation$FRAGMENTDEFINITION = () =>
  new FRAGMENTDEFINITION();
export const DirectiveLocation$isFRAGMENTDEFINITION = (value) =>
  value instanceof FRAGMENTDEFINITION;

export class FRAGMENTSPREAD extends $CustomType {}
export const DirectiveLocation$FRAGMENTSPREAD = () => new FRAGMENTSPREAD();
export const DirectiveLocation$isFRAGMENTSPREAD = (value) =>
  value instanceof FRAGMENTSPREAD;

export class INLINEFRAGMENT extends $CustomType {}
export const DirectiveLocation$INLINEFRAGMENT = () => new INLINEFRAGMENT();
export const DirectiveLocation$isINLINEFRAGMENT = (value) =>
  value instanceof INLINEFRAGMENT;

export class SCHEMA extends $CustomType {}
export const DirectiveLocation$SCHEMA = () => new SCHEMA();
export const DirectiveLocation$isSCHEMA = (value) => value instanceof SCHEMA;

export class SCALAR extends $CustomType {}
export const DirectiveLocation$SCALAR = () => new SCALAR();
export const DirectiveLocation$isSCALAR = (value) => value instanceof SCALAR;

export class OBJECT extends $CustomType {}
export const DirectiveLocation$OBJECT = () => new OBJECT();
export const DirectiveLocation$isOBJECT = (value) => value instanceof OBJECT;

export class FIELDDEFINITION extends $CustomType {}
export const DirectiveLocation$FIELDDEFINITION = () => new FIELDDEFINITION();
export const DirectiveLocation$isFIELDDEFINITION = (value) =>
  value instanceof FIELDDEFINITION;

export class ARGUMENTDEFINITION extends $CustomType {}
export const DirectiveLocation$ARGUMENTDEFINITION = () =>
  new ARGUMENTDEFINITION();
export const DirectiveLocation$isARGUMENTDEFINITION = (value) =>
  value instanceof ARGUMENTDEFINITION;

export class INTERFACE extends $CustomType {}
export const DirectiveLocation$INTERFACE = () => new INTERFACE();
export const DirectiveLocation$isINTERFACE = (value) =>
  value instanceof INTERFACE;

export class UNION extends $CustomType {}
export const DirectiveLocation$UNION = () => new UNION();
export const DirectiveLocation$isUNION = (value) => value instanceof UNION;

export class ENUM extends $CustomType {}
export const DirectiveLocation$ENUM = () => new ENUM();
export const DirectiveLocation$isENUM = (value) => value instanceof ENUM;

export class ENUMVALUE extends $CustomType {}
export const DirectiveLocation$ENUMVALUE = () => new ENUMVALUE();
export const DirectiveLocation$isENUMVALUE = (value) =>
  value instanceof ENUMVALUE;

export class INPUTOBJECT extends $CustomType {}
export const DirectiveLocation$INPUTOBJECT = () => new INPUTOBJECT();
export const DirectiveLocation$isINPUTOBJECT = (value) =>
  value instanceof INPUTOBJECT;

export class INPUTFIELDDEFINITION extends $CustomType {}
export const DirectiveLocation$INPUTFIELDDEFINITION = () =>
  new INPUTFIELDDEFINITION();
export const DirectiveLocation$isINPUTFIELDDEFINITION = (value) =>
  value instanceof INPUTFIELDDEFINITION;

export class SchemaDef extends $CustomType {
  constructor(description, directives, query, mutation, subscription) {
    super();
    this.description = description;
    this.directives = directives;
    this.query = query;
    this.mutation = mutation;
    this.subscription = subscription;
  }
}
export const SchemaDef$SchemaDef = (description, directives, query, mutation, subscription) =>
  new SchemaDef(description, directives, query, mutation, subscription);
export const SchemaDef$isSchemaDef = (value) => value instanceof SchemaDef;
export const SchemaDef$SchemaDef$description = (value) => value.description;
export const SchemaDef$SchemaDef$0 = (value) => value.description;
export const SchemaDef$SchemaDef$directives = (value) => value.directives;
export const SchemaDef$SchemaDef$1 = (value) => value.directives;
export const SchemaDef$SchemaDef$query = (value) => value.query;
export const SchemaDef$SchemaDef$2 = (value) => value.query;
export const SchemaDef$SchemaDef$mutation = (value) => value.mutation;
export const SchemaDef$SchemaDef$3 = (value) => value.mutation;
export const SchemaDef$SchemaDef$subscription = (value) => value.subscription;
export const SchemaDef$SchemaDef$4 = (value) => value.subscription;

/**
 * Helper functions for working with SDL AST
 * Get the name of a type definition
 */
export function get_type_name(type_def) {
  if (type_def instanceof ObjectTypeDefinition) {
    let obj = type_def.object_def;
    return obj.name;
  } else if (type_def instanceof InterfaceTypeDefinition) {
    let iface = type_def.interface_def;
    return iface.name;
  } else if (type_def instanceof UnionTypeDefinition) {
    let union = type_def.union_def;
    return union.name;
  } else if (type_def instanceof ScalarTypeDefinition) {
    let scalar = type_def.scalar_def;
    return scalar.name;
  } else if (type_def instanceof EnumTypeDefinition) {
    let enum$ = type_def.enum_def;
    return enum$.name;
  } else {
    let input = type_def.input_def;
    return input.name;
  }
}

/**
 * Convert SDL type to string representation
 */
export function sdl_type_to_string(sdl_type) {
  if (sdl_type instanceof NamedType) {
    let name = sdl_type.name;
    return name;
  } else if (sdl_type instanceof ListType) {
    let inner = sdl_type.inner_type;
    return ("[" + sdl_type_to_string(inner)) + "]";
  } else {
    let inner = sdl_type.inner_type;
    return sdl_type_to_string(inner) + "!";
  }
}

/**
 * Check if a type is non-null
 */
export function is_non_null_type(sdl_type) {
  if (sdl_type instanceof NonNullType) {
    return true;
  } else {
    return false;
  }
}

/**
 * Check if a type is a list
 */
export function is_list_type(sdl_type) {
  if (sdl_type instanceof ListType) {
    return true;
  } else if (sdl_type instanceof NonNullType) {
    let $ = sdl_type.inner_type;
    if ($ instanceof ListType) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

/**
 * Get the base type (unwrap NonNull and List wrappers)
 */
export function get_base_type(loop$sdl_type) {
  while (true) {
    let sdl_type = loop$sdl_type;
    if (sdl_type instanceof NamedType) {
      let name = sdl_type.name;
      return name;
    } else if (sdl_type instanceof ListType) {
      let inner = sdl_type.inner_type;
      loop$sdl_type = inner;
    } else {
      let inner = sdl_type.inner_type;
      loop$sdl_type = inner;
    }
  }
}
