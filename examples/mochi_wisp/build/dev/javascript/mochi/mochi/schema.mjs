import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";
import * as $dataloader from "../mochi/dataloader.mjs";

export class Schema extends $CustomType {
  constructor(query, mutation, subscription, types, directives) {
    super();
    this.query = query;
    this.mutation = mutation;
    this.subscription = subscription;
    this.types = types;
    this.directives = directives;
  }
}
export const Schema$Schema = (query, mutation, subscription, types, directives) =>
  new Schema(query, mutation, subscription, types, directives);
export const Schema$isSchema = (value) => value instanceof Schema;
export const Schema$Schema$query = (value) => value.query;
export const Schema$Schema$0 = (value) => value.query;
export const Schema$Schema$mutation = (value) => value.mutation;
export const Schema$Schema$1 = (value) => value.mutation;
export const Schema$Schema$subscription = (value) => value.subscription;
export const Schema$Schema$2 = (value) => value.subscription;
export const Schema$Schema$types = (value) => value.types;
export const Schema$Schema$3 = (value) => value.types;
export const Schema$Schema$directives = (value) => value.directives;
export const Schema$Schema$4 = (value) => value.directives;

export class DirectiveDefinition extends $CustomType {
  constructor(name, description, arguments$, locations, is_repeatable, handler) {
    super();
    this.name = name;
    this.description = description;
    this.arguments = arguments$;
    this.locations = locations;
    this.is_repeatable = is_repeatable;
    this.handler = handler;
  }
}
export const DirectiveDefinition$DirectiveDefinition = (name, description, arguments$, locations, is_repeatable, handler) =>
  new DirectiveDefinition(name,
  description,
  arguments$,
  locations,
  is_repeatable,
  handler);
export const DirectiveDefinition$isDirectiveDefinition = (value) =>
  value instanceof DirectiveDefinition;
export const DirectiveDefinition$DirectiveDefinition$name = (value) =>
  value.name;
export const DirectiveDefinition$DirectiveDefinition$0 = (value) => value.name;
export const DirectiveDefinition$DirectiveDefinition$description = (value) =>
  value.description;
export const DirectiveDefinition$DirectiveDefinition$1 = (value) =>
  value.description;
export const DirectiveDefinition$DirectiveDefinition$arguments = (value) =>
  value.arguments;
export const DirectiveDefinition$DirectiveDefinition$2 = (value) =>
  value.arguments;
export const DirectiveDefinition$DirectiveDefinition$locations = (value) =>
  value.locations;
export const DirectiveDefinition$DirectiveDefinition$3 = (value) =>
  value.locations;
export const DirectiveDefinition$DirectiveDefinition$is_repeatable = (value) =>
  value.is_repeatable;
export const DirectiveDefinition$DirectiveDefinition$4 = (value) =>
  value.is_repeatable;
export const DirectiveDefinition$DirectiveDefinition$handler = (value) =>
  value.handler;
export const DirectiveDefinition$DirectiveDefinition$5 = (value) =>
  value.handler;

export class QueryLocation extends $CustomType {}
export const DirectiveLocation$QueryLocation = () => new QueryLocation();
export const DirectiveLocation$isQueryLocation = (value) =>
  value instanceof QueryLocation;

export class MutationLocation extends $CustomType {}
export const DirectiveLocation$MutationLocation = () => new MutationLocation();
export const DirectiveLocation$isMutationLocation = (value) =>
  value instanceof MutationLocation;

export class SubscriptionLocation extends $CustomType {}
export const DirectiveLocation$SubscriptionLocation = () =>
  new SubscriptionLocation();
export const DirectiveLocation$isSubscriptionLocation = (value) =>
  value instanceof SubscriptionLocation;

export class FieldLocation extends $CustomType {}
export const DirectiveLocation$FieldLocation = () => new FieldLocation();
export const DirectiveLocation$isFieldLocation = (value) =>
  value instanceof FieldLocation;

export class FragmentDefinitionLocation extends $CustomType {}
export const DirectiveLocation$FragmentDefinitionLocation = () =>
  new FragmentDefinitionLocation();
export const DirectiveLocation$isFragmentDefinitionLocation = (value) =>
  value instanceof FragmentDefinitionLocation;

export class FragmentSpreadLocation extends $CustomType {}
export const DirectiveLocation$FragmentSpreadLocation = () =>
  new FragmentSpreadLocation();
export const DirectiveLocation$isFragmentSpreadLocation = (value) =>
  value instanceof FragmentSpreadLocation;

export class InlineFragmentLocation extends $CustomType {}
export const DirectiveLocation$InlineFragmentLocation = () =>
  new InlineFragmentLocation();
export const DirectiveLocation$isInlineFragmentLocation = (value) =>
  value instanceof InlineFragmentLocation;

export class VariableDefinitionLocation extends $CustomType {}
export const DirectiveLocation$VariableDefinitionLocation = () =>
  new VariableDefinitionLocation();
export const DirectiveLocation$isVariableDefinitionLocation = (value) =>
  value instanceof VariableDefinitionLocation;

export class SchemaLocation extends $CustomType {}
export const DirectiveLocation$SchemaLocation = () => new SchemaLocation();
export const DirectiveLocation$isSchemaLocation = (value) =>
  value instanceof SchemaLocation;

export class ScalarLocation extends $CustomType {}
export const DirectiveLocation$ScalarLocation = () => new ScalarLocation();
export const DirectiveLocation$isScalarLocation = (value) =>
  value instanceof ScalarLocation;

export class ObjectLocation extends $CustomType {}
export const DirectiveLocation$ObjectLocation = () => new ObjectLocation();
export const DirectiveLocation$isObjectLocation = (value) =>
  value instanceof ObjectLocation;

export class FieldDefinitionLocation extends $CustomType {}
export const DirectiveLocation$FieldDefinitionLocation = () =>
  new FieldDefinitionLocation();
export const DirectiveLocation$isFieldDefinitionLocation = (value) =>
  value instanceof FieldDefinitionLocation;

export class ArgumentDefinitionLocation extends $CustomType {}
export const DirectiveLocation$ArgumentDefinitionLocation = () =>
  new ArgumentDefinitionLocation();
export const DirectiveLocation$isArgumentDefinitionLocation = (value) =>
  value instanceof ArgumentDefinitionLocation;

export class InterfaceLocation extends $CustomType {}
export const DirectiveLocation$InterfaceLocation = () =>
  new InterfaceLocation();
export const DirectiveLocation$isInterfaceLocation = (value) =>
  value instanceof InterfaceLocation;

export class UnionLocation extends $CustomType {}
export const DirectiveLocation$UnionLocation = () => new UnionLocation();
export const DirectiveLocation$isUnionLocation = (value) =>
  value instanceof UnionLocation;

export class EnumLocation extends $CustomType {}
export const DirectiveLocation$EnumLocation = () => new EnumLocation();
export const DirectiveLocation$isEnumLocation = (value) =>
  value instanceof EnumLocation;

export class EnumValueLocation extends $CustomType {}
export const DirectiveLocation$EnumValueLocation = () =>
  new EnumValueLocation();
export const DirectiveLocation$isEnumValueLocation = (value) =>
  value instanceof EnumValueLocation;

export class InputObjectLocation extends $CustomType {}
export const DirectiveLocation$InputObjectLocation = () =>
  new InputObjectLocation();
export const DirectiveLocation$isInputObjectLocation = (value) =>
  value instanceof InputObjectLocation;

export class InputFieldDefinitionLocation extends $CustomType {}
export const DirectiveLocation$InputFieldDefinitionLocation = () =>
  new InputFieldDefinitionLocation();
export const DirectiveLocation$isInputFieldDefinitionLocation = (value) =>
  value instanceof InputFieldDefinitionLocation;

export class ObjectTypeDef extends $CustomType {
  constructor(object_type) {
    super();
    this.object_type = object_type;
  }
}
export const TypeDefinition$ObjectTypeDef = (object_type) =>
  new ObjectTypeDef(object_type);
export const TypeDefinition$isObjectTypeDef = (value) =>
  value instanceof ObjectTypeDef;
export const TypeDefinition$ObjectTypeDef$object_type = (value) =>
  value.object_type;
export const TypeDefinition$ObjectTypeDef$0 = (value) => value.object_type;

export class ScalarTypeDef extends $CustomType {
  constructor(scalar_type) {
    super();
    this.scalar_type = scalar_type;
  }
}
export const TypeDefinition$ScalarTypeDef = (scalar_type) =>
  new ScalarTypeDef(scalar_type);
export const TypeDefinition$isScalarTypeDef = (value) =>
  value instanceof ScalarTypeDef;
export const TypeDefinition$ScalarTypeDef$scalar_type = (value) =>
  value.scalar_type;
export const TypeDefinition$ScalarTypeDef$0 = (value) => value.scalar_type;

export class EnumTypeDef extends $CustomType {
  constructor(enum_type) {
    super();
    this.enum_type = enum_type;
  }
}
export const TypeDefinition$EnumTypeDef = (enum_type) =>
  new EnumTypeDef(enum_type);
export const TypeDefinition$isEnumTypeDef = (value) =>
  value instanceof EnumTypeDef;
export const TypeDefinition$EnumTypeDef$enum_type = (value) => value.enum_type;
export const TypeDefinition$EnumTypeDef$0 = (value) => value.enum_type;

export class InterfaceTypeDef extends $CustomType {
  constructor(interface_type) {
    super();
    this.interface_type = interface_type;
  }
}
export const TypeDefinition$InterfaceTypeDef = (interface_type) =>
  new InterfaceTypeDef(interface_type);
export const TypeDefinition$isInterfaceTypeDef = (value) =>
  value instanceof InterfaceTypeDef;
export const TypeDefinition$InterfaceTypeDef$interface_type = (value) =>
  value.interface_type;
export const TypeDefinition$InterfaceTypeDef$0 = (value) =>
  value.interface_type;

export class UnionTypeDef extends $CustomType {
  constructor(union_type) {
    super();
    this.union_type = union_type;
  }
}
export const TypeDefinition$UnionTypeDef = (union_type) =>
  new UnionTypeDef(union_type);
export const TypeDefinition$isUnionTypeDef = (value) =>
  value instanceof UnionTypeDef;
export const TypeDefinition$UnionTypeDef$union_type = (value) =>
  value.union_type;
export const TypeDefinition$UnionTypeDef$0 = (value) => value.union_type;

export class InputObjectTypeDef extends $CustomType {
  constructor(input_object_type) {
    super();
    this.input_object_type = input_object_type;
  }
}
export const TypeDefinition$InputObjectTypeDef = (input_object_type) =>
  new InputObjectTypeDef(input_object_type);
export const TypeDefinition$isInputObjectTypeDef = (value) =>
  value instanceof InputObjectTypeDef;
export const TypeDefinition$InputObjectTypeDef$input_object_type = (value) =>
  value.input_object_type;
export const TypeDefinition$InputObjectTypeDef$0 = (value) =>
  value.input_object_type;

export class ObjectType extends $CustomType {
  constructor(name, description, fields, interfaces) {
    super();
    this.name = name;
    this.description = description;
    this.fields = fields;
    this.interfaces = interfaces;
  }
}
export const ObjectType$ObjectType = (name, description, fields, interfaces) =>
  new ObjectType(name, description, fields, interfaces);
export const ObjectType$isObjectType = (value) => value instanceof ObjectType;
export const ObjectType$ObjectType$name = (value) => value.name;
export const ObjectType$ObjectType$0 = (value) => value.name;
export const ObjectType$ObjectType$description = (value) => value.description;
export const ObjectType$ObjectType$1 = (value) => value.description;
export const ObjectType$ObjectType$fields = (value) => value.fields;
export const ObjectType$ObjectType$2 = (value) => value.fields;
export const ObjectType$ObjectType$interfaces = (value) => value.interfaces;
export const ObjectType$ObjectType$3 = (value) => value.interfaces;

export class FieldDefinition extends $CustomType {
  constructor(name, description, field_type, arguments$, resolver, is_deprecated, deprecation_reason) {
    super();
    this.name = name;
    this.description = description;
    this.field_type = field_type;
    this.arguments = arguments$;
    this.resolver = resolver;
    this.is_deprecated = is_deprecated;
    this.deprecation_reason = deprecation_reason;
  }
}
export const FieldDefinition$FieldDefinition = (name, description, field_type, arguments$, resolver, is_deprecated, deprecation_reason) =>
  new FieldDefinition(name,
  description,
  field_type,
  arguments$,
  resolver,
  is_deprecated,
  deprecation_reason);
export const FieldDefinition$isFieldDefinition = (value) =>
  value instanceof FieldDefinition;
export const FieldDefinition$FieldDefinition$name = (value) => value.name;
export const FieldDefinition$FieldDefinition$0 = (value) => value.name;
export const FieldDefinition$FieldDefinition$description = (value) =>
  value.description;
export const FieldDefinition$FieldDefinition$1 = (value) => value.description;
export const FieldDefinition$FieldDefinition$field_type = (value) =>
  value.field_type;
export const FieldDefinition$FieldDefinition$2 = (value) => value.field_type;
export const FieldDefinition$FieldDefinition$arguments = (value) =>
  value.arguments;
export const FieldDefinition$FieldDefinition$3 = (value) => value.arguments;
export const FieldDefinition$FieldDefinition$resolver = (value) =>
  value.resolver;
export const FieldDefinition$FieldDefinition$4 = (value) => value.resolver;
export const FieldDefinition$FieldDefinition$is_deprecated = (value) =>
  value.is_deprecated;
export const FieldDefinition$FieldDefinition$5 = (value) => value.is_deprecated;
export const FieldDefinition$FieldDefinition$deprecation_reason = (value) =>
  value.deprecation_reason;
export const FieldDefinition$FieldDefinition$6 = (value) =>
  value.deprecation_reason;

export class NonNull extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}
export const FieldType$NonNull = (inner) => new NonNull(inner);
export const FieldType$isNonNull = (value) => value instanceof NonNull;
export const FieldType$NonNull$inner = (value) => value.inner;
export const FieldType$NonNull$0 = (value) => value.inner;

export class List extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}
export const FieldType$List = (inner) => new List(inner);
export const FieldType$isList = (value) => value instanceof List;
export const FieldType$List$inner = (value) => value.inner;
export const FieldType$List$0 = (value) => value.inner;

export class Named extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const FieldType$Named = (name) => new Named(name);
export const FieldType$isNamed = (value) => value instanceof Named;
export const FieldType$Named$name = (value) => value.name;
export const FieldType$Named$0 = (value) => value.name;

export class ArgumentDefinition extends $CustomType {
  constructor(name, description, arg_type, default_value) {
    super();
    this.name = name;
    this.description = description;
    this.arg_type = arg_type;
    this.default_value = default_value;
  }
}
export const ArgumentDefinition$ArgumentDefinition = (name, description, arg_type, default_value) =>
  new ArgumentDefinition(name, description, arg_type, default_value);
export const ArgumentDefinition$isArgumentDefinition = (value) =>
  value instanceof ArgumentDefinition;
export const ArgumentDefinition$ArgumentDefinition$name = (value) => value.name;
export const ArgumentDefinition$ArgumentDefinition$0 = (value) => value.name;
export const ArgumentDefinition$ArgumentDefinition$description = (value) =>
  value.description;
export const ArgumentDefinition$ArgumentDefinition$1 = (value) =>
  value.description;
export const ArgumentDefinition$ArgumentDefinition$arg_type = (value) =>
  value.arg_type;
export const ArgumentDefinition$ArgumentDefinition$2 = (value) =>
  value.arg_type;
export const ArgumentDefinition$ArgumentDefinition$default_value = (value) =>
  value.default_value;
export const ArgumentDefinition$ArgumentDefinition$3 = (value) =>
  value.default_value;

export class ScalarType extends $CustomType {
  constructor(name, description, serialize, parse_value, parse_literal) {
    super();
    this.name = name;
    this.description = description;
    this.serialize = serialize;
    this.parse_value = parse_value;
    this.parse_literal = parse_literal;
  }
}
export const ScalarType$ScalarType = (name, description, serialize, parse_value, parse_literal) =>
  new ScalarType(name, description, serialize, parse_value, parse_literal);
export const ScalarType$isScalarType = (value) => value instanceof ScalarType;
export const ScalarType$ScalarType$name = (value) => value.name;
export const ScalarType$ScalarType$0 = (value) => value.name;
export const ScalarType$ScalarType$description = (value) => value.description;
export const ScalarType$ScalarType$1 = (value) => value.description;
export const ScalarType$ScalarType$serialize = (value) => value.serialize;
export const ScalarType$ScalarType$2 = (value) => value.serialize;
export const ScalarType$ScalarType$parse_value = (value) => value.parse_value;
export const ScalarType$ScalarType$3 = (value) => value.parse_value;
export const ScalarType$ScalarType$parse_literal = (value) =>
  value.parse_literal;
export const ScalarType$ScalarType$4 = (value) => value.parse_literal;

export class EnumType extends $CustomType {
  constructor(name, description, values) {
    super();
    this.name = name;
    this.description = description;
    this.values = values;
  }
}
export const EnumType$EnumType = (name, description, values) =>
  new EnumType(name, description, values);
export const EnumType$isEnumType = (value) => value instanceof EnumType;
export const EnumType$EnumType$name = (value) => value.name;
export const EnumType$EnumType$0 = (value) => value.name;
export const EnumType$EnumType$description = (value) => value.description;
export const EnumType$EnumType$1 = (value) => value.description;
export const EnumType$EnumType$values = (value) => value.values;
export const EnumType$EnumType$2 = (value) => value.values;

export class EnumValueDefinition extends $CustomType {
  constructor(name, description, value, is_deprecated, deprecation_reason) {
    super();
    this.name = name;
    this.description = description;
    this.value = value;
    this.is_deprecated = is_deprecated;
    this.deprecation_reason = deprecation_reason;
  }
}
export const EnumValueDefinition$EnumValueDefinition = (name, description, value, is_deprecated, deprecation_reason) =>
  new EnumValueDefinition(name,
  description,
  value,
  is_deprecated,
  deprecation_reason);
export const EnumValueDefinition$isEnumValueDefinition = (value) =>
  value instanceof EnumValueDefinition;
export const EnumValueDefinition$EnumValueDefinition$name = (value) =>
  value.name;
export const EnumValueDefinition$EnumValueDefinition$0 = (value) => value.name;
export const EnumValueDefinition$EnumValueDefinition$description = (value) =>
  value.description;
export const EnumValueDefinition$EnumValueDefinition$1 = (value) =>
  value.description;
export const EnumValueDefinition$EnumValueDefinition$value = (value) =>
  value.value;
export const EnumValueDefinition$EnumValueDefinition$2 = (value) => value.value;
export const EnumValueDefinition$EnumValueDefinition$is_deprecated = (value) =>
  value.is_deprecated;
export const EnumValueDefinition$EnumValueDefinition$3 = (value) =>
  value.is_deprecated;
export const EnumValueDefinition$EnumValueDefinition$deprecation_reason = (value) =>
  value.deprecation_reason;
export const EnumValueDefinition$EnumValueDefinition$4 = (value) =>
  value.deprecation_reason;

export class InterfaceType extends $CustomType {
  constructor(name, description, fields, resolve_type) {
    super();
    this.name = name;
    this.description = description;
    this.fields = fields;
    this.resolve_type = resolve_type;
  }
}
export const InterfaceType$InterfaceType = (name, description, fields, resolve_type) =>
  new InterfaceType(name, description, fields, resolve_type);
export const InterfaceType$isInterfaceType = (value) =>
  value instanceof InterfaceType;
export const InterfaceType$InterfaceType$name = (value) => value.name;
export const InterfaceType$InterfaceType$0 = (value) => value.name;
export const InterfaceType$InterfaceType$description = (value) =>
  value.description;
export const InterfaceType$InterfaceType$1 = (value) => value.description;
export const InterfaceType$InterfaceType$fields = (value) => value.fields;
export const InterfaceType$InterfaceType$2 = (value) => value.fields;
export const InterfaceType$InterfaceType$resolve_type = (value) =>
  value.resolve_type;
export const InterfaceType$InterfaceType$3 = (value) => value.resolve_type;

export class UnionType extends $CustomType {
  constructor(name, description, types, resolve_type) {
    super();
    this.name = name;
    this.description = description;
    this.types = types;
    this.resolve_type = resolve_type;
  }
}
export const UnionType$UnionType = (name, description, types, resolve_type) =>
  new UnionType(name, description, types, resolve_type);
export const UnionType$isUnionType = (value) => value instanceof UnionType;
export const UnionType$UnionType$name = (value) => value.name;
export const UnionType$UnionType$0 = (value) => value.name;
export const UnionType$UnionType$description = (value) => value.description;
export const UnionType$UnionType$1 = (value) => value.description;
export const UnionType$UnionType$types = (value) => value.types;
export const UnionType$UnionType$2 = (value) => value.types;
export const UnionType$UnionType$resolve_type = (value) => value.resolve_type;
export const UnionType$UnionType$3 = (value) => value.resolve_type;

export class InputObjectType extends $CustomType {
  constructor(name, description, fields) {
    super();
    this.name = name;
    this.description = description;
    this.fields = fields;
  }
}
export const InputObjectType$InputObjectType = (name, description, fields) =>
  new InputObjectType(name, description, fields);
export const InputObjectType$isInputObjectType = (value) =>
  value instanceof InputObjectType;
export const InputObjectType$InputObjectType$name = (value) => value.name;
export const InputObjectType$InputObjectType$0 = (value) => value.name;
export const InputObjectType$InputObjectType$description = (value) =>
  value.description;
export const InputObjectType$InputObjectType$1 = (value) => value.description;
export const InputObjectType$InputObjectType$fields = (value) => value.fields;
export const InputObjectType$InputObjectType$2 = (value) => value.fields;

export class InputFieldDefinition extends $CustomType {
  constructor(name, description, field_type, default_value) {
    super();
    this.name = name;
    this.description = description;
    this.field_type = field_type;
    this.default_value = default_value;
  }
}
export const InputFieldDefinition$InputFieldDefinition = (name, description, field_type, default_value) =>
  new InputFieldDefinition(name, description, field_type, default_value);
export const InputFieldDefinition$isInputFieldDefinition = (value) =>
  value instanceof InputFieldDefinition;
export const InputFieldDefinition$InputFieldDefinition$name = (value) =>
  value.name;
export const InputFieldDefinition$InputFieldDefinition$0 = (value) =>
  value.name;
export const InputFieldDefinition$InputFieldDefinition$description = (value) =>
  value.description;
export const InputFieldDefinition$InputFieldDefinition$1 = (value) =>
  value.description;
export const InputFieldDefinition$InputFieldDefinition$field_type = (value) =>
  value.field_type;
export const InputFieldDefinition$InputFieldDefinition$2 = (value) =>
  value.field_type;
export const InputFieldDefinition$InputFieldDefinition$default_value = (value) =>
  value.default_value;
export const InputFieldDefinition$InputFieldDefinition$3 = (value) =>
  value.default_value;

export class ExecutionContext extends $CustomType {
  constructor(user_context, data_loaders) {
    super();
    this.user_context = user_context;
    this.data_loaders = data_loaders;
  }
}
export const ExecutionContext$ExecutionContext = (user_context, data_loaders) =>
  new ExecutionContext(user_context, data_loaders);
export const ExecutionContext$isExecutionContext = (value) =>
  value instanceof ExecutionContext;
export const ExecutionContext$ExecutionContext$user_context = (value) =>
  value.user_context;
export const ExecutionContext$ExecutionContext$0 = (value) =>
  value.user_context;
export const ExecutionContext$ExecutionContext$data_loaders = (value) =>
  value.data_loaders;
export const ExecutionContext$ExecutionContext$1 = (value) =>
  value.data_loaders;

export class ResolverInfo extends $CustomType {
  constructor(parent, arguments$, context, info) {
    super();
    this.parent = parent;
    this.arguments = arguments$;
    this.context = context;
    this.info = info;
  }
}
export const ResolverInfo$ResolverInfo = (parent, arguments$, context, info) =>
  new ResolverInfo(parent, arguments$, context, info);
export const ResolverInfo$isResolverInfo = (value) =>
  value instanceof ResolverInfo;
export const ResolverInfo$ResolverInfo$parent = (value) => value.parent;
export const ResolverInfo$ResolverInfo$0 = (value) => value.parent;
export const ResolverInfo$ResolverInfo$arguments = (value) => value.arguments;
export const ResolverInfo$ResolverInfo$1 = (value) => value.arguments;
export const ResolverInfo$ResolverInfo$context = (value) => value.context;
export const ResolverInfo$ResolverInfo$2 = (value) => value.context;
export const ResolverInfo$ResolverInfo$info = (value) => value.info;
export const ResolverInfo$ResolverInfo$3 = (value) => value.info;

/**
 * Create a new execution context
 */
export function execution_context(user_context) {
  return new ExecutionContext(user_context, $dict.new$());
}

/**
 * Add a DataLoader to the execution context
 */
export function add_data_loader(context, name, loader) {
  return new ExecutionContext(
    context.user_context,
    $dict.insert(context.data_loaders, name, loader),
  );
}

/**
 * Get a DataLoader from the execution context
 */
export function get_data_loader(context, name) {
  let $ = $dict.get(context.data_loaders, name);
  if ($ instanceof Ok) {
    return $;
  } else {
    return new Error(
      ("DataLoader '" + name) + "' not found in execution context",
    );
  }
}

/**
 * Update a DataLoader in the execution context
 */
export function update_data_loader(context, name, loader) {
  return new ExecutionContext(
    context.user_context,
    $dict.insert(context.data_loaders, name, loader),
  );
}

export function schema() {
  return new Schema(
    new None(),
    new None(),
    new None(),
    $dict.new$(),
    $dict.new$(),
  );
}

export function query(schema, query_type) {
  return new Schema(
    new Some(query_type),
    schema.mutation,
    schema.subscription,
    schema.types,
    schema.directives,
  );
}

export function mutation(schema, mutation_type) {
  return new Schema(
    schema.query,
    new Some(mutation_type),
    schema.subscription,
    schema.types,
    schema.directives,
  );
}

export function subscription(schema, subscription_type) {
  return new Schema(
    schema.query,
    schema.mutation,
    new Some(subscription_type),
    schema.types,
    schema.directives,
  );
}

export function add_type(schema, type_def) {
  let _block;
  if (type_def instanceof ObjectTypeDef) {
    let obj = type_def.object_type;
    _block = obj.name;
  } else if (type_def instanceof ScalarTypeDef) {
    let scalar$1 = type_def.scalar_type;
    _block = scalar$1.name;
  } else if (type_def instanceof EnumTypeDef) {
    let enum$ = type_def.enum_type;
    _block = enum$.name;
  } else if (type_def instanceof InterfaceTypeDef) {
    let interface$1 = type_def.interface_type;
    _block = interface$1.name;
  } else if (type_def instanceof UnionTypeDef) {
    let union$1 = type_def.union_type;
    _block = union$1.name;
  } else {
    let input = type_def.input_object_type;
    _block = input.name;
  }
  let type_name = _block;
  return new Schema(
    schema.query,
    schema.mutation,
    schema.subscription,
    $dict.insert(schema.types, type_name, type_def),
    schema.directives,
  );
}

export function object(name) {
  return new ObjectType(name, new None(), $dict.new$(), toList([]));
}

export function description(obj, desc) {
  return new ObjectType(obj.name, new Some(desc), obj.fields, obj.interfaces);
}

export function field(obj, field_def) {
  return new ObjectType(
    obj.name,
    obj.description,
    $dict.insert(obj.fields, field_def.name, field_def),
    obj.interfaces,
  );
}

export function implements$(obj, interface$) {
  return new ObjectType(
    obj.name,
    obj.description,
    obj.fields,
    listPrepend(interface$, obj.interfaces),
  );
}

export function field_def(name, field_type) {
  return new FieldDefinition(
    name,
    new None(),
    field_type,
    $dict.new$(),
    new None(),
    false,
    new None(),
  );
}

/**
 * Mark a field as deprecated
 */
export function deprecate(field, reason) {
  return new FieldDefinition(
    field.name,
    field.description,
    field.field_type,
    field.arguments,
    field.resolver,
    true,
    new Some(reason),
  );
}

/**
 * Mark a field as deprecated without a reason
 */
export function deprecate_field(field) {
  return new FieldDefinition(
    field.name,
    field.description,
    field.field_type,
    field.arguments,
    field.resolver,
    true,
    new None(),
  );
}

export function field_description(field, desc) {
  return new FieldDefinition(
    field.name,
    new Some(desc),
    field.field_type,
    field.arguments,
    field.resolver,
    field.is_deprecated,
    field.deprecation_reason,
  );
}

export function argument(field, arg_def) {
  return new FieldDefinition(
    field.name,
    field.description,
    field.field_type,
    $dict.insert(field.arguments, arg_def.name, arg_def),
    field.resolver,
    field.is_deprecated,
    field.deprecation_reason,
  );
}

export function resolver(field, resolve_fn) {
  return new FieldDefinition(
    field.name,
    field.description,
    field.field_type,
    field.arguments,
    new Some(resolve_fn),
    field.is_deprecated,
    field.deprecation_reason,
  );
}

export function arg(name, arg_type) {
  return new ArgumentDefinition(name, new None(), arg_type, new None());
}

export function arg_description(arg, desc) {
  return new ArgumentDefinition(
    arg.name,
    new Some(desc),
    arg.arg_type,
    arg.default_value,
  );
}

export function default_value(arg, value) {
  return new ArgumentDefinition(
    arg.name,
    arg.description,
    arg.arg_type,
    new Some(value),
  );
}

export function string_type() {
  return new Named("String");
}

export function int_type() {
  return new Named("Int");
}

export function float_type() {
  return new Named("Float");
}

export function boolean_type() {
  return new Named("Boolean");
}

export function id_type() {
  return new Named("ID");
}

export function named_type(name) {
  return new Named(name);
}

export function non_null(inner) {
  return new NonNull(inner);
}

export function list_type(inner) {
  return new List(inner);
}

export function scalar(name) {
  return new ScalarType(
    name,
    new None(),
    (value) => { return new Ok(value); },
    (value) => { return new Ok(value); },
    (value) => { return new Ok(value); },
  );
}

export function scalar_description(scalar, desc) {
  return new ScalarType(
    scalar.name,
    new Some(desc),
    scalar.serialize,
    scalar.parse_value,
    scalar.parse_literal,
  );
}

export function serialize(scalar, serialize_fn) {
  return new ScalarType(
    scalar.name,
    scalar.description,
    serialize_fn,
    scalar.parse_value,
    scalar.parse_literal,
  );
}

export function parse_value(scalar, parse_fn) {
  return new ScalarType(
    scalar.name,
    scalar.description,
    scalar.serialize,
    parse_fn,
    scalar.parse_literal,
  );
}

export function parse_literal(scalar, parse_fn) {
  return new ScalarType(
    scalar.name,
    scalar.description,
    scalar.serialize,
    scalar.parse_value,
    parse_fn,
  );
}

export function string_scalar() {
  let _pipe = scalar("String");
  return scalar_description(
    _pipe,
    "The String scalar type represents textual data",
  );
}

export function int_scalar() {
  let _pipe = scalar("Int");
  return scalar_description(
    _pipe,
    "The Int scalar type represents non-fractional signed whole numeric values",
  );
}

export function float_scalar() {
  let _pipe = scalar("Float");
  return scalar_description(
    _pipe,
    "The Float scalar type represents signed double-precision fractional values",
  );
}

export function boolean_scalar() {
  let _pipe = scalar("Boolean");
  return scalar_description(
    _pipe,
    "The Boolean scalar type represents true or false",
  );
}

export function id_scalar() {
  let _pipe = scalar("ID");
  return scalar_description(
    _pipe,
    "The ID scalar type represents a unique identifier",
  );
}

export function interface$(name) {
  return new InterfaceType(name, new None(), $dict.new$(), new None());
}

export function interface_description(iface, desc) {
  return new InterfaceType(
    iface.name,
    new Some(desc),
    iface.fields,
    iface.resolve_type,
  );
}

export function interface_field(iface, field_def) {
  return new InterfaceType(
    iface.name,
    iface.description,
    $dict.insert(iface.fields, field_def.name, field_def),
    iface.resolve_type,
  );
}

export function interface_resolve_type(iface, resolver) {
  return new InterfaceType(
    iface.name,
    iface.description,
    iface.fields,
    new Some(resolver),
  );
}

export function union(name) {
  return new UnionType(name, new None(), toList([]), new None());
}

export function union_description(union_type, desc) {
  return new UnionType(
    union_type.name,
    new Some(desc),
    union_type.types,
    union_type.resolve_type,
  );
}

export function union_member(union_type, member) {
  return new UnionType(
    union_type.name,
    union_type.description,
    listPrepend(member, union_type.types),
    union_type.resolve_type,
  );
}

export function union_resolve_type(union_type, resolver) {
  return new UnionType(
    union_type.name,
    union_type.description,
    union_type.types,
    new Some(resolver),
  );
}

/**
 * Add a directive definition to the schema
 */
export function add_directive(schema, directive) {
  return new Schema(
    schema.query,
    schema.mutation,
    schema.subscription,
    schema.types,
    $dict.insert(schema.directives, directive.name, directive),
  );
}

/**
 * Create a new directive definition
 */
export function directive(name, locations) {
  return new DirectiveDefinition(
    name,
    new None(),
    $dict.new$(),
    locations,
    false,
    new None(),
  );
}

/**
 * Add description to a directive
 */
export function directive_description(dir, desc) {
  return new DirectiveDefinition(
    dir.name,
    new Some(desc),
    dir.arguments,
    dir.locations,
    dir.is_repeatable,
    dir.handler,
  );
}

/**
 * Add an argument to a directive
 */
export function directive_argument(dir, arg_def) {
  return new DirectiveDefinition(
    dir.name,
    dir.description,
    $dict.insert(dir.arguments, arg_def.name, arg_def),
    dir.locations,
    dir.is_repeatable,
    dir.handler,
  );
}

/**
 * Make a directive repeatable
 */
export function directive_repeatable(dir) {
  return new DirectiveDefinition(
    dir.name,
    dir.description,
    dir.arguments,
    dir.locations,
    true,
    dir.handler,
  );
}

/**
 * Set the handler function for a directive (for field-level directives)
 */
export function directive_handler(dir, handler) {
  return new DirectiveDefinition(
    dir.name,
    dir.description,
    dir.arguments,
    dir.locations,
    dir.is_repeatable,
    new Some(handler),
  );
}

/**
 * @skip(if: Boolean!) directive
 */
export function skip_directive() {
  let _pipe = directive(
    "skip",
    toList([
      new FieldLocation(),
      new FragmentSpreadLocation(),
      new InlineFragmentLocation(),
    ]),
  );
  let _pipe$1 = directive_description(
    _pipe,
    "Directs the executor to skip this field or fragment when the `if` argument is true.",
  );
  return directive_argument(
    _pipe$1,
    (() => {
      let _pipe$2 = arg("if", non_null(boolean_type()));
      return arg_description(_pipe$2, "Skipped when true.");
    })(),
  );
}

/**
 * @include(if: Boolean!) directive
 */
export function include_directive() {
  let _pipe = directive(
    "include",
    toList([
      new FieldLocation(),
      new FragmentSpreadLocation(),
      new InlineFragmentLocation(),
    ]),
  );
  let _pipe$1 = directive_description(
    _pipe,
    "Directs the executor to include this field or fragment only when the `if` argument is true.",
  );
  return directive_argument(
    _pipe$1,
    (() => {
      let _pipe$2 = arg("if", non_null(boolean_type()));
      return arg_description(_pipe$2, "Included when true.");
    })(),
  );
}

/**
 * @deprecated(reason: String) directive
 */
export function deprecated_directive() {
  let _pipe = directive(
    "deprecated",
    toList([new FieldDefinitionLocation(), new EnumValueLocation()]),
  );
  let _pipe$1 = directive_description(
    _pipe,
    "Marks an element of a GraphQL schema as no longer supported.",
  );
  return directive_argument(
    _pipe$1,
    (() => {
      let _pipe$2 = arg("reason", string_type());
      return arg_description(
        _pipe$2,
        "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data.",
      );
    })(),
  );
}

/**
 * Get all built-in directives
 */
export function builtin_directives() {
  return toList([skip_directive(), include_directive(), deprecated_directive()]);
}

/**
 * Convert DirectiveLocation to string (for SDL generation)
 */
export function directive_location_to_string(loc) {
  if (loc instanceof QueryLocation) {
    return "QUERY";
  } else if (loc instanceof MutationLocation) {
    return "MUTATION";
  } else if (loc instanceof SubscriptionLocation) {
    return "SUBSCRIPTION";
  } else if (loc instanceof FieldLocation) {
    return "FIELD";
  } else if (loc instanceof FragmentDefinitionLocation) {
    return "FRAGMENT_DEFINITION";
  } else if (loc instanceof FragmentSpreadLocation) {
    return "FRAGMENT_SPREAD";
  } else if (loc instanceof InlineFragmentLocation) {
    return "INLINE_FRAGMENT";
  } else if (loc instanceof VariableDefinitionLocation) {
    return "VARIABLE_DEFINITION";
  } else if (loc instanceof SchemaLocation) {
    return "SCHEMA";
  } else if (loc instanceof ScalarLocation) {
    return "SCALAR";
  } else if (loc instanceof ObjectLocation) {
    return "OBJECT";
  } else if (loc instanceof FieldDefinitionLocation) {
    return "FIELD_DEFINITION";
  } else if (loc instanceof ArgumentDefinitionLocation) {
    return "ARGUMENT_DEFINITION";
  } else if (loc instanceof InterfaceLocation) {
    return "INTERFACE";
  } else if (loc instanceof UnionLocation) {
    return "UNION";
  } else if (loc instanceof EnumLocation) {
    return "ENUM";
  } else if (loc instanceof EnumValueLocation) {
    return "ENUM_VALUE";
  } else if (loc instanceof InputObjectLocation) {
    return "INPUT_OBJECT";
  } else {
    return "INPUT_FIELD_DEFINITION";
  }
}
