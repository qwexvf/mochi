//// Core schema types and execution context for GraphQL.
////
//// This module provides:
//// - Schema type definitions (ObjectType, FieldType, ScalarType, etc.)
//// - ExecutionContext for managing DataLoaders and middleware
//// - Builder functions for constructing schemas
////
//// ## ExecutionContext with DataLoaders
////
//// ```gleam
//// // Create context with multiple loaders
//// let ctx = schema.execution_context(types.to_dynamic(dict.new()))
////   |> schema.with_loaders([
////     #("pokemon", pokemon_loader),
////     #("trainer", trainer_loader),
////   ])
////
//// // Load by ID (handles context threading automatically)
//// let #(ctx, pokemon) = schema.load_by_id(ctx, "pokemon", 25)
//// let #(ctx, trainers) = schema.load_many_by_id(ctx, "trainer", [1, 2, 3])
//// ```
////
//// ## Schema Building
////
//// ```gleam
//// let my_schema = schema.schema()
////   |> schema.query(query_type)
////   |> schema.mutation(mutation_type)
////   |> schema.add_type(schema.ObjectTypeDef(user_type))
////   |> schema.add_directive(auth_directive)
//// ```

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/dataloader.{type DataLoader}

// Forward declarations for types defined in other modules
// These are opaque references to avoid circular dependencies

/// Opaque type for middleware pipeline (defined in mochi/middleware.gleam)
pub type MiddlewarePipeline

/// Opaque type for telemetry context (defined in mochi/telemetry.gleam)
pub type TelemetryContext

// Core schema types
pub type Schema {
  Schema(
    query: Option(ObjectType),
    mutation: Option(ObjectType),
    subscription: Option(ObjectType),
    types: Dict(String, TypeDefinition),
    directives: Dict(String, DirectiveDefinition),
  )
}

/// Custom directive definition
pub type DirectiveDefinition {
  DirectiveDefinition(
    name: String,
    description: Option(String),
    arguments: Dict(String, ArgumentDefinition),
    locations: List(DirectiveLocation),
    is_repeatable: Bool,
    handler: Option(DirectiveHandler),
  )
}

/// Handler function for custom directives
/// Takes the directive arguments and field value, returns modified value or error
pub type DirectiveHandler =
  fn(Dict(String, Dynamic), Dynamic) -> Result(Dynamic, String)

/// Locations where a directive can be used
pub type DirectiveLocation {
  // Executable locations (query/mutation/subscription)
  QueryLocation
  MutationLocation
  SubscriptionLocation
  FieldLocation
  FragmentDefinitionLocation
  FragmentSpreadLocation
  InlineFragmentLocation
  VariableDefinitionLocation
  // Type system locations (schema definition)
  SchemaLocation
  ScalarLocation
  ObjectLocation
  FieldDefinitionLocation
  ArgumentDefinitionLocation
  InterfaceLocation
  UnionLocation
  EnumLocation
  EnumValueLocation
  InputObjectLocation
  InputFieldDefinitionLocation
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
    /// Optional middleware pipeline for field resolution
    middleware_pipeline: Option(MiddlewarePipeline),
    /// Optional telemetry context for instrumentation
    telemetry: Option(TelemetryContext),
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
  ExecutionContext(
    user_context: user_context,
    data_loaders: dict.new(),
    middleware_pipeline: None,
    telemetry: None,
  )
}

/// Create an execution context with middleware
pub fn execution_context_with_middleware(
  user_context: Dynamic,
  middleware: MiddlewarePipeline,
) -> ExecutionContext {
  ExecutionContext(
    user_context: user_context,
    data_loaders: dict.new(),
    middleware_pipeline: Some(middleware),
    telemetry: None,
  )
}

/// Create an execution context with telemetry
pub fn execution_context_with_telemetry(
  user_context: Dynamic,
  telemetry: TelemetryContext,
) -> ExecutionContext {
  ExecutionContext(
    user_context: user_context,
    data_loaders: dict.new(),
    middleware_pipeline: None,
    telemetry: Some(telemetry),
  )
}

/// Create a full execution context with all options
pub fn full_execution_context(
  user_context: Dynamic,
  middleware: Option(MiddlewarePipeline),
  telemetry: Option(TelemetryContext),
) -> ExecutionContext {
  ExecutionContext(
    user_context: user_context,
    data_loaders: dict.new(),
    middleware_pipeline: middleware,
    telemetry: telemetry,
  )
}

/// Set middleware pipeline on an execution context
pub fn with_middleware(
  context: ExecutionContext,
  middleware: MiddlewarePipeline,
) -> ExecutionContext {
  ExecutionContext(..context, middleware_pipeline: Some(middleware))
}

/// Set telemetry on an execution context
pub fn with_telemetry(
  context: ExecutionContext,
  telemetry: TelemetryContext,
) -> ExecutionContext {
  ExecutionContext(..context, telemetry: Some(telemetry))
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

/// Get the middleware pipeline from context
pub fn get_middleware(context: ExecutionContext) -> Option(MiddlewarePipeline) {
  context.middleware_pipeline
}

/// Get the telemetry context
pub fn get_telemetry(context: ExecutionContext) -> Option(TelemetryContext) {
  context.telemetry
}

/// Update the telemetry context
pub fn update_telemetry(
  context: ExecutionContext,
  telemetry: TelemetryContext,
) -> ExecutionContext {
  ExecutionContext(..context, telemetry: Some(telemetry))
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

/// Add multiple DataLoaders to the execution context at once
///
/// ## Example
///
/// ```gleam
/// let ctx = schema.execution_context(user_ctx)
///   |> schema.with_loaders([
///     #("pokemon", pokemon_loader),
///     #("move", move_loader),
///     #("trainer", trainer_loader),
///   ])
/// ```
pub fn with_loaders(
  context: ExecutionContext,
  loaders: List(#(String, DataLoader(Dynamic, Dynamic))),
) -> ExecutionContext {
  list.fold(loaders, context, fn(ctx, loader_pair) {
    let #(name, loader) = loader_pair
    add_data_loader(ctx, name, loader)
  })
}

/// Load a value using a named DataLoader, returning updated context and result
///
/// This handles the context threading automatically.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, result) = schema.load(ctx, "pokemon", dataloader.int_key(25))
/// ```
pub fn load(
  context: ExecutionContext,
  loader_name: String,
  key: Dynamic,
) -> #(ExecutionContext, Result(Dynamic, String)) {
  case get_data_loader(context, loader_name) {
    Ok(loader) -> {
      let #(new_loader, result) = dataloader.load(loader, key)
      let new_ctx = update_data_loader(context, loader_name, new_loader)
      #(new_ctx, result)
    }
    Error(e) -> #(context, Error(e))
  }
}

/// Load multiple values using a named DataLoader
///
/// ## Example
///
/// ```gleam
/// let keys = list.map([1, 2, 3], dataloader.int_key)
/// let #(ctx, results) = schema.load_many(ctx, "pokemon", keys)
/// ```
pub fn load_many(
  context: ExecutionContext,
  loader_name: String,
  keys: List(Dynamic),
) -> #(ExecutionContext, List(Result(Dynamic, String))) {
  case get_data_loader(context, loader_name) {
    Ok(loader) -> {
      let #(new_loader, results) = dataloader.load_many(loader, keys)
      let new_ctx = update_data_loader(context, loader_name, new_loader)
      #(new_ctx, results)
    }
    Error(e) -> #(context, list.map(keys, fn(_) { Error(e) }))
  }
}

/// Load an entity by Int ID using a named DataLoader
///
/// Convenience wrapper for the common case of loading by integer ID.
///
/// ## Example
///
/// ```gleam
/// let #(ctx, pokemon) = schema.load_by_id(ctx, "pokemon", 25)
/// ```
pub fn load_by_id(
  context: ExecutionContext,
  loader_name: String,
  id: Int,
) -> #(ExecutionContext, Result(Dynamic, String)) {
  load(context, loader_name, dataloader.int_key(id))
}

/// Load multiple entities by Int IDs using a named DataLoader
///
/// ## Example
///
/// ```gleam
/// let #(ctx, pokemon_list) = schema.load_many_by_id(ctx, "pokemon", [1, 4, 7, 25])
/// ```
pub fn load_many_by_id(
  context: ExecutionContext,
  loader_name: String,
  ids: List(Int),
) -> #(ExecutionContext, List(Result(Dynamic, String))) {
  let keys = list.map(ids, dataloader.int_key)
  load_many(context, loader_name, keys)
}

// Builder API
pub fn schema() -> Schema {
  Schema(
    query: None,
    mutation: None,
    subscription: None,
    types: dict.new(),
    directives: dict.new(),
  )
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

// ============================================================================
// Simple Field Helpers
// ============================================================================
// These helpers reduce boilerplate for Dynamic-based schemas where fields
// are extracted from a Dict parent.

import gleam/dynamic/decode

/// Create a resolver that auto-extracts a field from parent by name
pub fn auto_resolver(field_name: String) -> Resolver {
  fn(info: ResolverInfo) {
    case info.parent {
      Some(parent) -> {
        case decode.run(parent, decode.dict(decode.string, decode.dynamic)) {
          Ok(d) -> {
            case dict.get(d, field_name) {
              Ok(value) -> Ok(value)
              Error(_) -> Error("Field not found: " <> field_name)
            }
          }
          Error(_) -> Error("Invalid parent type")
        }
      }
      None -> Error("No parent")
    }
  }
}

/// Add a field with auto-resolver (extracts field by name from parent)
pub fn auto_field(
  obj: ObjectType,
  name: String,
  field_type: FieldType,
) -> ObjectType {
  let f =
    FieldDefinition(
      name: name,
      description: None,
      field_type: field_type,
      arguments: dict.new(),
      resolver: Some(auto_resolver(name)),
      is_deprecated: False,
      deprecation_reason: None,
    )
  ObjectType(..obj, fields: dict.insert(obj.fields, name, f))
}

/// Add a non-null ID field with auto-resolver
pub fn id_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, NonNull(Named("ID")))
}

/// Add a nullable String field with auto-resolver
pub fn string_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, Named("String"))
}

/// Add a non-null String field with auto-resolver
pub fn required_string_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, NonNull(Named("String")))
}

/// Add a nullable Int field with auto-resolver
pub fn int_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, Named("Int"))
}

/// Add a non-null Int field with auto-resolver
pub fn required_int_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, NonNull(Named("Int")))
}

/// Add a nullable Boolean field with auto-resolver
pub fn bool_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, Named("Boolean"))
}

/// Add a non-null Boolean field with auto-resolver
pub fn required_bool_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, NonNull(Named("Boolean")))
}

/// Add a nullable Float field with auto-resolver
pub fn float_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, Named("Float"))
}

/// Add a non-null Float field with auto-resolver
pub fn required_float_field(obj: ObjectType, name: String) -> ObjectType {
  auto_field(obj, name, NonNull(Named("Float")))
}

/// Add a list field with auto-resolver
pub fn list_field(
  obj: ObjectType,
  name: String,
  item_type: String,
) -> ObjectType {
  auto_field(obj, name, List(Named(item_type)))
}

/// Add a non-null list field with auto-resolver
pub fn required_list_field(
  obj: ObjectType,
  name: String,
  item_type: String,
) -> ObjectType {
  auto_field(obj, name, NonNull(List(Named(item_type))))
}

/// Add a reference field to another type with auto-resolver
pub fn ref_field(obj: ObjectType, name: String, type_name: String) -> ObjectType {
  auto_field(obj, name, Named(type_name))
}

/// Add a non-null reference field with auto-resolver
pub fn required_ref_field(
  obj: ObjectType,
  name: String,
  type_name: String,
) -> ObjectType {
  auto_field(obj, name, NonNull(Named(type_name)))
}

// ============================================================================
// Query/Mutation Field Helpers
// ============================================================================
// These helpers simplify defining query/mutation fields with custom resolvers.

/// Add a field with custom resolver and optional description
pub fn resolver_field(
  obj: ObjectType,
  name: String,
  field_type: FieldType,
  desc: String,
  resolve_fn: Resolver,
) -> ObjectType {
  let f =
    FieldDefinition(
      name: name,
      description: Some(desc),
      field_type: field_type,
      arguments: dict.new(),
      resolver: Some(resolve_fn),
      is_deprecated: False,
      deprecation_reason: None,
    )
  ObjectType(..obj, fields: dict.insert(obj.fields, name, f))
}

/// Add a list query field: `name: [ItemType]`
pub fn list_query(
  obj: ObjectType,
  name: String,
  item_type: String,
  desc: String,
  resolve_fn: Resolver,
) -> ObjectType {
  resolver_field(obj, name, List(Named(item_type)), desc, resolve_fn)
}

/// Add a reference query field: `name: TypeName`
pub fn ref_query(
  obj: ObjectType,
  name: String,
  type_name: String,
  desc: String,
  resolve_fn: Resolver,
) -> ObjectType {
  resolver_field(obj, name, Named(type_name), desc, resolve_fn)
}

/// Add a query field with arguments
pub fn query_with_args(
  obj: ObjectType,
  name: String,
  field_type: FieldType,
  args: List(ArgumentDefinition),
  desc: String,
  resolve_fn: Resolver,
) -> ObjectType {
  let f =
    FieldDefinition(
      name: name,
      description: Some(desc),
      field_type: field_type,
      arguments: list.fold(args, dict.new(), fn(acc, a) {
        dict.insert(acc, a.name, a)
      }),
      resolver: Some(resolve_fn),
      is_deprecated: False,
      deprecation_reason: None,
    )
  ObjectType(..obj, fields: dict.insert(obj.fields, name, f))
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

// ============================================================================
// Directive Builder API
// ============================================================================

/// Add a directive definition to the schema
pub fn add_directive(schema: Schema, directive: DirectiveDefinition) -> Schema {
  Schema(
    ..schema,
    directives: dict.insert(schema.directives, directive.name, directive),
  )
}

/// Create a new directive definition
pub fn directive(
  name: String,
  locations: List(DirectiveLocation),
) -> DirectiveDefinition {
  DirectiveDefinition(
    name: name,
    description: None,
    arguments: dict.new(),
    locations: locations,
    is_repeatable: False,
    handler: None,
  )
}

/// Add description to a directive
pub fn directive_description(
  dir: DirectiveDefinition,
  desc: String,
) -> DirectiveDefinition {
  DirectiveDefinition(..dir, description: Some(desc))
}

/// Add an argument to a directive
pub fn directive_argument(
  dir: DirectiveDefinition,
  arg_def: ArgumentDefinition,
) -> DirectiveDefinition {
  DirectiveDefinition(
    ..dir,
    arguments: dict.insert(dir.arguments, arg_def.name, arg_def),
  )
}

/// Make a directive repeatable
pub fn directive_repeatable(dir: DirectiveDefinition) -> DirectiveDefinition {
  DirectiveDefinition(..dir, is_repeatable: True)
}

/// Set the handler function for a directive (for field-level directives)
pub fn directive_handler(
  dir: DirectiveDefinition,
  handler: DirectiveHandler,
) -> DirectiveDefinition {
  DirectiveDefinition(..dir, handler: Some(handler))
}

// ============================================================================
// Built-in Directive Definitions
// ============================================================================

/// @skip(if: Boolean!) directive
pub fn skip_directive() -> DirectiveDefinition {
  directive("skip", [
    FieldLocation,
    FragmentSpreadLocation,
    InlineFragmentLocation,
  ])
  |> directive_description(
    "Directs the executor to skip this field or fragment when the `if` argument is true.",
  )
  |> directive_argument(
    arg("if", non_null(boolean_type()))
    |> arg_description("Skipped when true."),
  )
}

/// @include(if: Boolean!) directive
pub fn include_directive() -> DirectiveDefinition {
  directive("include", [
    FieldLocation,
    FragmentSpreadLocation,
    InlineFragmentLocation,
  ])
  |> directive_description(
    "Directs the executor to include this field or fragment only when the `if` argument is true.",
  )
  |> directive_argument(
    arg("if", non_null(boolean_type()))
    |> arg_description("Included when true."),
  )
}

/// @deprecated(reason: String) directive
pub fn deprecated_directive() -> DirectiveDefinition {
  directive("deprecated", [FieldDefinitionLocation, EnumValueLocation])
  |> directive_description(
    "Marks an element of a GraphQL schema as no longer supported.",
  )
  |> directive_argument(
    arg("reason", string_type())
    |> arg_description(
      "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data.",
    ),
  )
}

/// Get all built-in directives
pub fn builtin_directives() -> List(DirectiveDefinition) {
  [skip_directive(), include_directive(), deprecated_directive()]
}

/// Convert DirectiveLocation to string (for SDL generation)
pub fn directive_location_to_string(loc: DirectiveLocation) -> String {
  case loc {
    QueryLocation -> "QUERY"
    MutationLocation -> "MUTATION"
    SubscriptionLocation -> "SUBSCRIPTION"
    FieldLocation -> "FIELD"
    FragmentDefinitionLocation -> "FRAGMENT_DEFINITION"
    FragmentSpreadLocation -> "FRAGMENT_SPREAD"
    InlineFragmentLocation -> "INLINE_FRAGMENT"
    VariableDefinitionLocation -> "VARIABLE_DEFINITION"
    SchemaLocation -> "SCHEMA"
    ScalarLocation -> "SCALAR"
    ObjectLocation -> "OBJECT"
    FieldDefinitionLocation -> "FIELD_DEFINITION"
    ArgumentDefinitionLocation -> "ARGUMENT_DEFINITION"
    InterfaceLocation -> "INTERFACE"
    UnionLocation -> "UNION"
    EnumLocation -> "ENUM"
    EnumValueLocation -> "ENUM_VALUE"
    InputObjectLocation -> "INPUT_OBJECT"
    InputFieldDefinitionLocation -> "INPUT_FIELD_DEFINITION"
  }
}
