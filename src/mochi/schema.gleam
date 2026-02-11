import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import mochi/dataloader.{type DataLoader}

// Core schema types
pub type Schema {
  Schema(
    query: Option(ObjectType),
    mutation: Option(ObjectType),
    subscription: Option(ObjectType),
    types: Dict(String, TypeDefinition),
  )
}

pub type TypeDefinition {
  ObjectTypeDef(object_type: ObjectType)
  ScalarTypeDef(scalar_type: ScalarType)
  EnumTypeDef(enum_type: EnumType)
  InterfaceTypeDef(interface_type: InterfaceType)
  UnionTypeDef(union_type: UnionType)
  InputObjectTypeDef(input_object_type: InputObjectType)
}

pub type ObjectType {
  ObjectType(
    name: String,
    description: Option(String),
    fields: Dict(String, FieldDefinition),
    interfaces: List(InterfaceType),
  )
}

pub type FieldDefinition {
  FieldDefinition(
    name: String,
    description: Option(String),
    field_type: FieldType,
    arguments: Dict(String, ArgumentDefinition),
    resolver: Option(Resolver),
    is_deprecated: Bool,
    deprecation_reason: Option(String),
  )
}

pub type FieldType {
  NonNull(inner: FieldType)
  List(inner: FieldType)
  Named(name: String)
}

pub type ArgumentDefinition {
  ArgumentDefinition(
    name: String,
    description: Option(String),
    arg_type: FieldType,
    default_value: Option(Dynamic),
  )
}

pub type ScalarType {
  ScalarType(
    name: String,
    description: Option(String),
    serialize: fn(Dynamic) -> Result(Dynamic, String),
    parse_value: fn(Dynamic) -> Result(Dynamic, String),
    parse_literal: fn(Dynamic) -> Result(Dynamic, String),
  )
}

pub type EnumType {
  EnumType(
    name: String,
    description: Option(String),
    values: Dict(String, EnumValueDefinition),
  )
}

pub type EnumValueDefinition {
  EnumValueDefinition(
    name: String,
    description: Option(String),
    value: Dynamic,
    is_deprecated: Bool,
    deprecation_reason: Option(String),
  )
}

/// Type resolver function that determines the concrete type at runtime
pub type TypeResolver =
  fn(Dynamic) -> Result(String, String)

pub type InterfaceType {
  InterfaceType(
    name: String,
    description: Option(String),
    fields: Dict(String, FieldDefinition),
    resolve_type: Option(TypeResolver),
  )
}

pub type UnionType {
  UnionType(
    name: String,
    description: Option(String),
    types: List(ObjectType),
    resolve_type: Option(TypeResolver),
  )
}

pub type InputObjectType {
  InputObjectType(
    name: String,
    description: Option(String),
    fields: Dict(String, InputFieldDefinition),
  )
}

pub type InputFieldDefinition {
  InputFieldDefinition(
    name: String,
    description: Option(String),
    field_type: FieldType,
    default_value: Option(Dynamic),
  )
}

// Resolver type
pub type Resolver =
  fn(ResolverInfo) -> Result(Dynamic, String)

/// Context for GraphQL execution, including DataLoader instances
pub type ExecutionContext {
  ExecutionContext(
    /// Custom user context data
    user_context: Dynamic,
    /// DataLoader instances for batching and caching
    data_loaders: Dict(String, DataLoader(Dynamic, Dynamic)),
  )
}

/// Information passed to field resolvers
pub type ResolverInfo {
  ResolverInfo(
    parent: Option(Dynamic),
    arguments: Dict(String, Dynamic),
    context: ExecutionContext,
    info: Dynamic,
  )
}

// Execution Context helpers

/// Create a new execution context
pub fn execution_context(user_context: Dynamic) -> ExecutionContext {
  ExecutionContext(user_context: user_context, data_loaders: dict.new())
}

/// Add a DataLoader to the execution context
pub fn add_data_loader(
  context: ExecutionContext,
  name: String,
  loader: DataLoader(Dynamic, Dynamic),
) -> ExecutionContext {
  ExecutionContext(
    ..context,
    data_loaders: dict.insert(context.data_loaders, name, loader),
  )
}

/// Get a DataLoader from the execution context
pub fn get_data_loader(
  context: ExecutionContext,
  name: String,
) -> Result(DataLoader(Dynamic, Dynamic), String) {
  case dict.get(context.data_loaders, name) {
    Ok(loader) -> Ok(loader)
    Error(_) ->
      Error("DataLoader '" <> name <> "' not found in execution context")
  }
}

/// Update a DataLoader in the execution context
pub fn update_data_loader(
  context: ExecutionContext,
  name: String,
  loader: DataLoader(Dynamic, Dynamic),
) -> ExecutionContext {
  ExecutionContext(
    ..context,
    data_loaders: dict.insert(context.data_loaders, name, loader),
  )
}

// Builder API
pub fn schema() -> Schema {
  Schema(query: None, mutation: None, subscription: None, types: dict.new())
}

pub fn query(schema: Schema, query_type: ObjectType) -> Schema {
  Schema(..schema, query: Some(query_type))
}

pub fn mutation(schema: Schema, mutation_type: ObjectType) -> Schema {
  Schema(..schema, mutation: Some(mutation_type))
}

pub fn subscription(schema: Schema, subscription_type: ObjectType) -> Schema {
  Schema(..schema, subscription: Some(subscription_type))
}

pub fn add_type(schema: Schema, type_def: TypeDefinition) -> Schema {
  let type_name = case type_def {
    ObjectTypeDef(obj) -> obj.name
    ScalarTypeDef(scalar) -> scalar.name
    EnumTypeDef(enum) -> enum.name
    InterfaceTypeDef(interface) -> interface.name
    UnionTypeDef(union) -> union.name
    InputObjectTypeDef(input) -> input.name
  }

  Schema(..schema, types: dict.insert(schema.types, type_name, type_def))
}

// Object type builder
pub fn object(name: String) -> ObjectType {
  ObjectType(name: name, description: None, fields: dict.new(), interfaces: [])
}

pub fn description(obj: ObjectType, desc: String) -> ObjectType {
  ObjectType(..obj, description: Some(desc))
}

pub fn field(obj: ObjectType, field_def: FieldDefinition) -> ObjectType {
  ObjectType(..obj, fields: dict.insert(obj.fields, field_def.name, field_def))
}

pub fn implements(obj: ObjectType, interface: InterfaceType) -> ObjectType {
  ObjectType(..obj, interfaces: [interface, ..obj.interfaces])
}

// Field definition builder
pub fn field_def(name: String, field_type: FieldType) -> FieldDefinition {
  FieldDefinition(
    name: name,
    description: None,
    field_type: field_type,
    arguments: dict.new(),
    resolver: None,
    is_deprecated: False,
    deprecation_reason: None,
  )
}

/// Mark a field as deprecated
pub fn deprecate(field: FieldDefinition, reason: String) -> FieldDefinition {
  FieldDefinition(
    ..field,
    is_deprecated: True,
    deprecation_reason: Some(reason),
  )
}

/// Mark a field as deprecated without a reason
pub fn deprecate_field(field: FieldDefinition) -> FieldDefinition {
  FieldDefinition(..field, is_deprecated: True, deprecation_reason: None)
}

pub fn field_description(
  field: FieldDefinition,
  desc: String,
) -> FieldDefinition {
  FieldDefinition(..field, description: Some(desc))
}

pub fn argument(
  field: FieldDefinition,
  arg_def: ArgumentDefinition,
) -> FieldDefinition {
  FieldDefinition(
    ..field,
    arguments: dict.insert(field.arguments, arg_def.name, arg_def),
  )
}

pub fn resolver(field: FieldDefinition, resolve_fn: Resolver) -> FieldDefinition {
  FieldDefinition(..field, resolver: Some(resolve_fn))
}

// Argument definition builder
pub fn arg(name: String, arg_type: FieldType) -> ArgumentDefinition {
  ArgumentDefinition(
    name: name,
    description: None,
    arg_type: arg_type,
    default_value: None,
  )
}

pub fn arg_description(
  arg: ArgumentDefinition,
  desc: String,
) -> ArgumentDefinition {
  ArgumentDefinition(..arg, description: Some(desc))
}

pub fn default_value(
  arg: ArgumentDefinition,
  value: Dynamic,
) -> ArgumentDefinition {
  ArgumentDefinition(..arg, default_value: Some(value))
}

// Field type builders
pub fn string_type() -> FieldType {
  Named("String")
}

pub fn int_type() -> FieldType {
  Named("Int")
}

pub fn float_type() -> FieldType {
  Named("Float")
}

pub fn boolean_type() -> FieldType {
  Named("Boolean")
}

pub fn id_type() -> FieldType {
  Named("ID")
}

pub fn named_type(name: String) -> FieldType {
  Named(name)
}

pub fn non_null(inner: FieldType) -> FieldType {
  NonNull(inner)
}

pub fn list_type(inner: FieldType) -> FieldType {
  List(inner)
}

// Scalar type builder
pub fn scalar(name: String) -> ScalarType {
  ScalarType(
    name: name,
    description: None,
    serialize: fn(value) { Ok(value) },
    parse_value: fn(value) { Ok(value) },
    parse_literal: fn(value) { Ok(value) },
  )
}

pub fn scalar_description(scalar: ScalarType, desc: String) -> ScalarType {
  ScalarType(..scalar, description: Some(desc))
}

pub fn serialize(
  scalar: ScalarType,
  serialize_fn: fn(Dynamic) -> Result(Dynamic, String),
) -> ScalarType {
  ScalarType(..scalar, serialize: serialize_fn)
}

pub fn parse_value(
  scalar: ScalarType,
  parse_fn: fn(Dynamic) -> Result(Dynamic, String),
) -> ScalarType {
  ScalarType(..scalar, parse_value: parse_fn)
}

pub fn parse_literal(
  scalar: ScalarType,
  parse_fn: fn(Dynamic) -> Result(Dynamic, String),
) -> ScalarType {
  ScalarType(..scalar, parse_literal: parse_fn)
}

// Built-in scalars
pub fn string_scalar() -> ScalarType {
  scalar("String")
  |> scalar_description("The String scalar type represents textual data")
}

pub fn int_scalar() -> ScalarType {
  scalar("Int")
  |> scalar_description(
    "The Int scalar type represents non-fractional signed whole numeric values",
  )
}

pub fn float_scalar() -> ScalarType {
  scalar("Float")
  |> scalar_description(
    "The Float scalar type represents signed double-precision fractional values",
  )
}

pub fn boolean_scalar() -> ScalarType {
  scalar("Boolean")
  |> scalar_description("The Boolean scalar type represents true or false")
}

pub fn id_scalar() -> ScalarType {
  scalar("ID")
  |> scalar_description("The ID scalar type represents a unique identifier")
}

// Interface type builder
pub fn interface(name: String) -> InterfaceType {
  InterfaceType(
    name: name,
    description: None,
    fields: dict.new(),
    resolve_type: None,
  )
}

pub fn interface_description(
  iface: InterfaceType,
  desc: String,
) -> InterfaceType {
  InterfaceType(..iface, description: Some(desc))
}

pub fn interface_field(
  iface: InterfaceType,
  field_def: FieldDefinition,
) -> InterfaceType {
  InterfaceType(
    ..iface,
    fields: dict.insert(iface.fields, field_def.name, field_def),
  )
}

pub fn interface_resolve_type(
  iface: InterfaceType,
  resolver: TypeResolver,
) -> InterfaceType {
  InterfaceType(..iface, resolve_type: Some(resolver))
}

// Union type builder
pub fn union(name: String) -> UnionType {
  UnionType(name: name, description: None, types: [], resolve_type: None)
}

pub fn union_description(union_type: UnionType, desc: String) -> UnionType {
  UnionType(..union_type, description: Some(desc))
}

pub fn union_member(union_type: UnionType, member: ObjectType) -> UnionType {
  UnionType(..union_type, types: [member, ..union_type.types])
}

pub fn union_resolve_type(
  union_type: UnionType,
  resolver: TypeResolver,
) -> UnionType {
  UnionType(..union_type, resolve_type: Some(resolver))
}
