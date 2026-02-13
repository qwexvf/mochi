// mochi/query.gleam
// Code First API for defining GraphQL queries and mutations
// Inspired by gqlkit's simple, type-safe approach

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema.{
  type ExecutionContext, type FieldDefinition, type FieldType, type ObjectType,
  type Resolver, type ResolverInfo, type Schema,
}

// ============================================================================
// Core Types
// ============================================================================

/// A query definition with typed arguments and return value
pub type QueryDef(args, result) {
  QueryDef(
    name: String,
    description: Option(String),
    args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
    resolver: fn(args, ExecutionContext) -> Result(result, String),
    result_encoder: fn(result) -> Dynamic,
    arg_definitions: List(ArgDef),
    return_type: FieldType,
  )
}

/// A mutation definition
pub type MutationDef(args, result) {
  MutationDef(
    name: String,
    description: Option(String),
    args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
    resolver: fn(args, ExecutionContext) -> Result(result, String),
    result_encoder: fn(result) -> Dynamic,
    arg_definitions: List(ArgDef),
    return_type: FieldType,
  )
}

/// A subscription definition
pub type SubscriptionDef(args, event) {
  SubscriptionDef(
    name: String,
    description: Option(String),
    args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
    /// Returns the topic to subscribe to
    topic_resolver: fn(args, ExecutionContext) -> Result(String, String),
    /// Transforms event data for GraphQL response
    event_encoder: fn(event) -> Dynamic,
    arg_definitions: List(ArgDef),
    return_type: FieldType,
  )
}

/// A field definition for extending types
pub type FieldDef(parent, args, result) {
  FieldDef(
    name: String,
    description: Option(String),
    parent_decoder: fn(Dynamic) -> Result(parent, String),
    args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
    resolver: fn(parent, args, ExecutionContext) -> Result(result, String),
    result_encoder: fn(result) -> Dynamic,
    arg_definitions: List(ArgDef),
    return_type: FieldType,
  )
}

/// Argument definition for schema generation
pub type ArgDef {
  ArgDef(name: String, arg_type: FieldType, description: Option(String))
}

/// No arguments marker type
pub type NoArgs {
  NoArgs
}

// ============================================================================
// Query Builders
// ============================================================================

/// Define a query with no arguments
pub fn query(
  name: String,
  return_type: FieldType,
  resolver: fn(ExecutionContext) -> Result(result, String),
  encoder: fn(result) -> Dynamic,
) -> QueryDef(NoArgs, result) {
  QueryDef(
    name: name,
    description: None,
    args_decoder: fn(_) { Ok(NoArgs) },
    resolver: fn(_, ctx) { resolver(ctx) },
    result_encoder: encoder,
    arg_definitions: [],
    return_type: return_type,
  )
}

/// Define a query with arguments
pub fn query_with_args(
  name: String,
  arg_defs: List(ArgDef),
  return_type: FieldType,
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) -> Result(result, String),
  encoder: fn(result) -> Dynamic,
) -> QueryDef(args, result) {
  QueryDef(
    name: name,
    description: None,
    args_decoder: args_decoder,
    resolver: resolver,
    result_encoder: encoder,
    arg_definitions: arg_defs,
    return_type: return_type,
  )
}

/// Add description to a query
pub fn query_description(
  q: QueryDef(args, result),
  desc: String,
) -> QueryDef(args, result) {
  QueryDef(..q, description: Some(desc))
}

// ============================================================================
// Mutation Builders
// ============================================================================

/// Define a mutation with arguments
pub fn mutation(
  name: String,
  arg_defs: List(ArgDef),
  return_type: FieldType,
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) -> Result(result, String),
  encoder: fn(result) -> Dynamic,
) -> MutationDef(args, result) {
  MutationDef(
    name: name,
    description: None,
    args_decoder: args_decoder,
    resolver: resolver,
    result_encoder: encoder,
    arg_definitions: arg_defs,
    return_type: return_type,
  )
}

/// Add description to a mutation
pub fn mutation_description(
  m: MutationDef(args, result),
  desc: String,
) -> MutationDef(args, result) {
  MutationDef(..m, description: Some(desc))
}

// ============================================================================
// Subscription Builders
// ============================================================================

/// Define a subscription with no arguments
pub fn subscription(
  name: String,
  return_type: FieldType,
  topic: String,
  encoder: fn(event) -> Dynamic,
) -> SubscriptionDef(NoArgs, event) {
  SubscriptionDef(
    name: name,
    description: None,
    args_decoder: fn(_) { Ok(NoArgs) },
    topic_resolver: fn(_, _) { Ok(topic) },
    event_encoder: encoder,
    arg_definitions: [],
    return_type: return_type,
  )
}

/// Define a subscription with arguments
pub fn subscription_with_args(
  name: String,
  arg_defs: List(ArgDef),
  return_type: FieldType,
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  topic_resolver: fn(args, ExecutionContext) -> Result(String, String),
  encoder: fn(event) -> Dynamic,
) -> SubscriptionDef(args, event) {
  SubscriptionDef(
    name: name,
    description: None,
    args_decoder: args_decoder,
    topic_resolver: topic_resolver,
    event_encoder: encoder,
    arg_definitions: arg_defs,
    return_type: return_type,
  )
}

/// Add description to a subscription
pub fn subscription_description(
  s: SubscriptionDef(args, event),
  desc: String,
) -> SubscriptionDef(args, event) {
  SubscriptionDef(..s, description: Some(desc))
}

// ============================================================================
// Field Builders (for extending types)
// ============================================================================

/// Define a field on a type
pub fn field(
  name: String,
  return_type: FieldType,
  parent_decoder: fn(Dynamic) -> Result(parent, String),
  resolver: fn(parent, ExecutionContext) -> Result(result, String),
  encoder: fn(result) -> Dynamic,
) -> FieldDef(parent, NoArgs, result) {
  FieldDef(
    name: name,
    description: None,
    parent_decoder: parent_decoder,
    args_decoder: fn(_) { Ok(NoArgs) },
    resolver: fn(parent, _, ctx) { resolver(parent, ctx) },
    result_encoder: encoder,
    arg_definitions: [],
    return_type: return_type,
  )
}

/// Define a field with arguments
pub fn field_with_args(
  name: String,
  arg_defs: List(ArgDef),
  return_type: FieldType,
  parent_decoder: fn(Dynamic) -> Result(parent, String),
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(parent, args, ExecutionContext) -> Result(result, String),
  encoder: fn(result) -> Dynamic,
) -> FieldDef(parent, args, result) {
  FieldDef(
    name: name,
    description: None,
    parent_decoder: parent_decoder,
    args_decoder: args_decoder,
    resolver: resolver,
    result_encoder: encoder,
    arg_definitions: arg_defs,
    return_type: return_type,
  )
}

/// Add description to a field
pub fn field_description(
  f: FieldDef(parent, args, result),
  desc: String,
) -> FieldDef(parent, args, result) {
  FieldDef(..f, description: Some(desc))
}

// ============================================================================
// Argument Helpers
// ============================================================================

/// Create an argument definition
pub fn arg(name: String, arg_type: FieldType) -> ArgDef {
  ArgDef(name: name, arg_type: arg_type, description: None)
}

/// Create an argument with description
pub fn arg_with_desc(
  name: String,
  arg_type: FieldType,
  description: String,
) -> ArgDef {
  ArgDef(name: name, arg_type: arg_type, description: Some(description))
}

// ============================================================================
// Schema Building
// ============================================================================

/// Convert a QueryDef to a schema FieldDefinition
pub fn query_to_field_def(q: QueryDef(args, result)) -> FieldDefinition {
  let resolver: Resolver = fn(info: ResolverInfo) {
    case q.args_decoder(info.arguments) {
      Ok(decoded_args) ->
        case q.resolver(decoded_args, info.context) {
          Ok(result) -> Ok(q.result_encoder(result))
          Error(e) -> Error(e)
        }
      Error(e) -> Error("Failed to decode arguments: " <> e)
    }
  }

  let base =
    schema.field_def(q.name, q.return_type)
    |> schema.resolver(resolver)

  let with_args = add_args_to_field(base, q.arg_definitions)

  case q.description {
    Some(desc) -> schema.field_description(with_args, desc)
    None -> with_args
  }
}

/// Convert a MutationDef to a schema FieldDefinition
pub fn mutation_to_field_def(m: MutationDef(args, result)) -> FieldDefinition {
  let resolver: Resolver = fn(info: ResolverInfo) {
    case m.args_decoder(info.arguments) {
      Ok(decoded_args) ->
        case m.resolver(decoded_args, info.context) {
          Ok(result) -> Ok(m.result_encoder(result))
          Error(e) -> Error(e)
        }
      Error(e) -> Error("Failed to decode arguments: " <> e)
    }
  }

  let base =
    schema.field_def(m.name, m.return_type)
    |> schema.resolver(resolver)

  let with_args = add_args_to_field(base, m.arg_definitions)

  case m.description {
    Some(desc) -> schema.field_description(with_args, desc)
    None -> with_args
  }
}

/// Convert a SubscriptionDef to a schema FieldDefinition
/// Note: The actual subscription logic is handled by the subscription executor,
/// this just creates the schema definition for introspection and SDL generation
pub fn subscription_to_field_def(
  s: SubscriptionDef(args, event),
) -> FieldDefinition {
  // Subscriptions don't have a regular resolver - they're handled specially
  // We create a placeholder resolver that returns an error if called directly
  let resolver: Resolver = fn(_info: ResolverInfo) {
    Error("Subscriptions must be executed through the subscription executor")
  }

  let base =
    schema.field_def(s.name, s.return_type)
    |> schema.resolver(resolver)

  let with_args = add_args_to_field(base, s.arg_definitions)

  case s.description {
    Some(desc) -> schema.field_description(with_args, desc)
    None -> with_args
  }
}

/// Convert a FieldDef to a schema FieldDefinition
pub fn field_def_to_schema(f: FieldDef(parent, args, result)) -> FieldDefinition {
  let resolver: Resolver = fn(info: ResolverInfo) {
    case info.parent {
      Some(parent_dyn) ->
        case f.parent_decoder(parent_dyn) {
          Ok(parent) ->
            case f.args_decoder(info.arguments) {
              Ok(decoded_args) ->
                case f.resolver(parent, decoded_args, info.context) {
                  Ok(result) -> Ok(f.result_encoder(result))
                  Error(e) -> Error(e)
                }
              Error(e) -> Error("Failed to decode arguments: " <> e)
            }
          Error(e) -> Error("Failed to decode parent: " <> e)
        }
      None -> Error("No parent value provided")
    }
  }

  let base =
    schema.field_def(f.name, f.return_type)
    |> schema.resolver(resolver)

  let with_args = add_args_to_field(base, f.arg_definitions)

  case f.description {
    Some(desc) -> schema.field_description(with_args, desc)
    None -> with_args
  }
}

fn add_args_to_field(
  field: FieldDefinition,
  args: List(ArgDef),
) -> FieldDefinition {
  list.fold(args, field, fn(f, a) {
    let arg_def = case a.description {
      Some(desc) ->
        schema.arg(a.name, a.arg_type)
        |> schema.arg_description(desc)
      None -> schema.arg(a.name, a.arg_type)
    }
    schema.argument(f, arg_def)
  })
}

// ============================================================================
// Schema Builder - Fluent API
// ============================================================================

/// Schema builder for collecting queries, mutations, and subscriptions
pub type SchemaBuilder {
  SchemaBuilder(
    queries: List(FieldDefinition),
    mutations: List(FieldDefinition),
    subscriptions: List(FieldDefinition),
    types: List(ObjectType),
    enums: List(schema.EnumType),
    interfaces: List(schema.InterfaceType),
    unions: List(schema.UnionType),
    scalars: List(schema.ScalarType),
  )
}

/// Create a new schema builder
pub fn new() -> SchemaBuilder {
  SchemaBuilder(
    queries: [],
    mutations: [],
    subscriptions: [],
    types: [],
    enums: [],
    interfaces: [],
    unions: [],
    scalars: [],
  )
}

/// Add a query to the schema
pub fn add_query(
  builder: SchemaBuilder,
  q: QueryDef(args, result),
) -> SchemaBuilder {
  SchemaBuilder(..builder, queries: [query_to_field_def(q), ..builder.queries])
}

/// Add a mutation to the schema
pub fn add_mutation(
  builder: SchemaBuilder,
  m: MutationDef(args, result),
) -> SchemaBuilder {
  SchemaBuilder(..builder, mutations: [
    mutation_to_field_def(m),
    ..builder.mutations
  ])
}

/// Add a subscription to the schema
pub fn add_subscription(
  builder: SchemaBuilder,
  s: SubscriptionDef(args, event),
) -> SchemaBuilder {
  SchemaBuilder(..builder, subscriptions: [
    subscription_to_field_def(s),
    ..builder.subscriptions
  ])
}

/// Add a type definition
pub fn add_type(builder: SchemaBuilder, t: ObjectType) -> SchemaBuilder {
  SchemaBuilder(..builder, types: [t, ..builder.types])
}

/// Add an enum type
pub fn add_enum(builder: SchemaBuilder, e: schema.EnumType) -> SchemaBuilder {
  SchemaBuilder(..builder, enums: [e, ..builder.enums])
}

/// Add an interface type
pub fn add_interface(
  builder: SchemaBuilder,
  i: schema.InterfaceType,
) -> SchemaBuilder {
  SchemaBuilder(..builder, interfaces: [i, ..builder.interfaces])
}

/// Add a union type
pub fn add_union(builder: SchemaBuilder, u: schema.UnionType) -> SchemaBuilder {
  SchemaBuilder(..builder, unions: [u, ..builder.unions])
}

/// Add a custom scalar type (e.g., Upload, DateTime, JSON)
pub fn add_scalar(builder: SchemaBuilder, s: schema.ScalarType) -> SchemaBuilder {
  SchemaBuilder(..builder, scalars: [s, ..builder.scalars])
}

/// Build the final schema
pub fn build(builder: SchemaBuilder) -> Schema {
  let query_type =
    list.fold(builder.queries, schema.object("Query"), fn(obj, field) {
      schema.field(obj, field)
    })

  let base_schema =
    schema.schema()
    |> schema.query(query_type)

  let with_mutation = case builder.mutations {
    [] -> base_schema
    _ -> {
      let mutation_type =
        list.fold(builder.mutations, schema.object("Mutation"), fn(obj, field) {
          schema.field(obj, field)
        })
      schema.mutation(base_schema, mutation_type)
    }
  }

  let with_subscription = case builder.subscriptions {
    [] -> with_mutation
    _ -> {
      let subscription_type =
        list.fold(
          builder.subscriptions,
          schema.object("Subscription"),
          fn(obj, field) { schema.field(obj, field) },
        )
      schema.subscription(with_mutation, subscription_type)
    }
  }

  let with_types =
    list.fold(builder.types, with_subscription, fn(s, t) {
      schema.add_type(s, schema.ObjectTypeDef(t))
    })

  let with_enums =
    list.fold(builder.enums, with_types, fn(s, e) {
      schema.add_type(s, schema.EnumTypeDef(e))
    })

  let with_interfaces =
    list.fold(builder.interfaces, with_enums, fn(s, i) {
      schema.add_type(s, schema.InterfaceTypeDef(i))
    })

  let with_unions =
    list.fold(builder.unions, with_interfaces, fn(s, u) {
      schema.add_type(s, schema.UnionTypeDef(u))
    })

  list.fold(builder.scalars, with_unions, fn(s, scalar) {
    schema.add_type(s, schema.ScalarTypeDef(scalar))
  })
}
