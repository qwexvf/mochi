import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/document_cache
import mochi/error.{type GqlError}
import mochi/scalars
import mochi/schema.{
  type ExecutionContext, type FieldDefinition, type FieldType, type ObjectType,
  type ResolverInfo, type Schema,
}
import mochi/types as t

// ============================================================================
// Phantom Kind Types
// ============================================================================

pub type QueryKind {
  QueryKind
}

pub type MutationKind {
  MutationKind
}

pub type SubscriptionKind {
  SubscriptionKind
}

pub opaque type FieldKind(parent) {
  FieldKind(fn(parent) -> parent)
}

// ============================================================================
// Type Aliases
// ============================================================================

pub type QueryOp(result) =
  Op(QueryKind, result)

pub type MutationOp(result) =
  Op(MutationKind, result)

pub type SubscriptionOp(event) =
  Op(SubscriptionKind, event)

pub type FieldOp(parent, result) =
  Op(FieldKind(parent), result)

// ============================================================================
// Internal Resolver Variants
// ============================================================================

type InternalResolver(result) {
  SimpleResolver(
    fn(Dict(String, Dynamic), ExecutionContext) -> Result(result, GqlError),
  )
  TopicResolver(
    fn(Dict(String, Dynamic), ExecutionContext) -> Result(String, GqlError),
  )
  ParentResolver(
    fn(Option(Dynamic), Dict(String, Dynamic), ExecutionContext) ->
      Result(result, GqlError),
  )
}

// ============================================================================
// Unified Op Type
// ============================================================================

/// Unified operation type for queries, mutations, subscriptions, and fields.
/// The `kind` parameter encodes what kind of operation this is at the type level.
/// Use the type aliases `QueryOp`, `MutationOp`, `SubscriptionOp`, `FieldOp` in annotations.
pub opaque type Op(kind, result) {
  Op(
    name: String,
    description: Option(String),
    op_resolver: InternalResolver(result),
    result_encoder: fn(result) -> Dynamic,
    arg_definitions: List(ArgDef),
    return_type: FieldType,
    guards: List(Guard),
    kind: kind,
  )
}

/// Argument definition for schema generation
pub type ArgDef {
  ArgDef(
    name: String,
    arg_type: FieldType,
    description: Option(String),
    default_value: Option(Dynamic),
  )
}

// ============================================================================
// Generic Modifiers — work on any Op(k, r)
// ============================================================================

/// Override the result encoder. Works on any operation type.
pub fn with_encoder(op: Op(k, r), encoder: fn(r) -> Dynamic) -> Op(k, r) {
  Op(..op, result_encoder: encoder)
}

/// Add a guard. Works on any operation type. Guards run in the order added.
pub fn with_guard(op: Op(k, r), guard_fn: Guard) -> Op(k, r) {
  Op(..op, guards: list.append(op.guards, [guard_fn]))
}

/// Set the description. Works on any operation type.
pub fn with_description(op: Op(k, r), desc: String) -> Op(k, r) {
  Op(..op, description: Some(desc))
}

// ============================================================================
// Generic Accessors
// ============================================================================

pub fn get_name(op: Op(k, r)) -> String {
  op.name
}

pub fn get_description(op: Op(k, r)) -> Option(String) {
  op.description
}

pub fn get_args(op: Op(k, r)) -> List(ArgDef) {
  op.arg_definitions
}

// ============================================================================
// Query Builders
// ============================================================================

/// Define a query with no arguments.
pub fn query(
  name name: String,
  returns return_type: FieldType,
  resolve resolver: fn(ExecutionContext) -> Result(result, GqlError),
) -> QueryOp(result) {
  Op(
    name: name,
    description: None,
    op_resolver: SimpleResolver(fn(_, ctx) { resolver(ctx) }),
    result_encoder: t.to_dynamic,
    arg_definitions: [],
    return_type: return_type,
    guards: [],
    kind: QueryKind,
  )
}

/// Define a query with arguments.
pub fn query_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  resolve resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
) -> QueryOp(result) {
  Op(
    name: name,
    description: None,
    op_resolver: SimpleResolver(resolver),
    result_encoder: t.to_dynamic,
    arg_definitions: arg_defs,
    return_type: return_type,
    guards: [],
    kind: QueryKind,
  )
}

// ============================================================================
// Mutation Builders
// ============================================================================

/// Define a mutation with arguments.
pub fn mutation(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  resolve resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
) -> MutationOp(result) {
  Op(
    name: name,
    description: None,
    op_resolver: SimpleResolver(resolver),
    result_encoder: t.to_dynamic,
    arg_definitions: arg_defs,
    return_type: return_type,
    guards: [],
    kind: MutationKind,
  )
}

// ============================================================================
// Subscription Builders
// ============================================================================

/// Define a subscription with a static topic string.
pub fn subscription(
  name name: String,
  returns return_type: FieldType,
  topic topic: String,
) -> SubscriptionOp(event) {
  Op(
    name: name,
    description: None,
    op_resolver: TopicResolver(fn(_, _) { Ok(topic) }),
    result_encoder: t.to_dynamic,
    arg_definitions: [],
    return_type: return_type,
    guards: [],
    kind: SubscriptionKind,
  )
}

/// Define a subscription with arguments and a dynamic topic resolver.
pub fn subscription_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  topic topic_resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(String, GqlError),
) -> SubscriptionOp(event) {
  Op(
    name: name,
    description: None,
    op_resolver: TopicResolver(topic_resolver),
    result_encoder: t.to_dynamic,
    arg_definitions: arg_defs,
    return_type: return_type,
    guards: [],
    kind: SubscriptionKind,
  )
}

// ============================================================================
// Field Builders (for extending types)
// ============================================================================

/// Define a field on a type with no extra arguments.
pub fn field(
  name name: String,
  returns return_type: FieldType,
  decode parent_decoder: fn(Dynamic) -> Result(parent, String),
  resolve resolver: fn(parent, ExecutionContext) -> Result(result, GqlError),
) -> FieldOp(parent, result) {
  Op(
    name: name,
    description: None,
    op_resolver: ParentResolver(fn(parent_dyn, _, ctx) {
      use parent_dyn_val <- result.try(option.to_result(
        parent_dyn,
        error.new("No parent value provided"),
      ))
      use p <- result.try(
        parent_decoder(parent_dyn_val)
        |> result.map_error(fn(e) {
          error.new("Failed to decode parent: " <> e)
        }),
      )
      resolver(p, ctx)
    }),
    result_encoder: t.to_dynamic,
    arg_definitions: [],
    return_type: return_type,
    guards: [],
    kind: FieldKind(fn(x) { x }),
  )
}

/// Define a field on a type with arguments.
pub fn field_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  decode parent_decoder: fn(Dynamic) -> Result(parent, String),
  resolve resolver: fn(parent, Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
) -> FieldOp(parent, result) {
  Op(
    name: name,
    description: None,
    op_resolver: ParentResolver(fn(parent_dyn, args, ctx) {
      use parent_dyn_val <- result.try(option.to_result(
        parent_dyn,
        error.new("No parent value provided"),
      ))
      use p <- result.try(
        parent_decoder(parent_dyn_val)
        |> result.map_error(fn(e) {
          error.new("Failed to decode parent: " <> e)
        }),
      )
      resolver(p, args, ctx)
    }),
    result_encoder: t.to_dynamic,
    arg_definitions: arg_defs,
    return_type: return_type,
    guards: [],
    kind: FieldKind(fn(x) { x }),
  )
}

// ============================================================================
// Guard Combinators
// ============================================================================

pub type Guard =
  fn(ExecutionContext) -> Result(Nil, String)

/// Combine guards with AND logic — all must pass (checked in list order).
pub fn all_of(guard_fns: List(Guard)) -> Guard {
  fn(ctx) { list.try_each(guard_fns, fn(g) { g(ctx) }) }
}

/// Combine guards with OR logic — at least one must pass.
/// Fails with the last error if all fail.
pub fn any_of(guard_fns: List(Guard)) -> Guard {
  fn(ctx) {
    case guard_fns {
      [] -> Error("No guards provided")
      _ -> try_any(guard_fns, ctx, "No guards passed")
    }
  }
}

fn try_any(
  guards: List(Guard),
  ctx: ExecutionContext,
  last_error: String,
) -> Result(Nil, String) {
  case guards {
    [] -> Error(last_error)
    [g, ..rest] ->
      case g(ctx) {
        Ok(Nil) -> Ok(Nil)
        Error(e) -> try_any(rest, ctx, e)
      }
  }
}

fn apply_gql_guards(
  guards: List(Guard),
  resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
) -> fn(Dict(String, Dynamic), ExecutionContext) -> Result(result, GqlError) {
  list.fold(list.reverse(guards), resolver, fn(inner, g) {
    fn(args, ctx) {
      case g(ctx) {
        Ok(Nil) -> inner(args, ctx)
        Error(msg) -> Error(error.new(msg))
      }
    }
  })
}

fn apply_topic_guards(
  guards: List(Guard),
  resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(String, GqlError),
) -> fn(Dict(String, Dynamic), ExecutionContext) -> Result(String, GqlError) {
  list.fold(list.reverse(guards), resolver, fn(inner, g) {
    fn(args, ctx) {
      case g(ctx) {
        Ok(Nil) -> inner(args, ctx)
        Error(msg) -> Error(error.new(msg))
      }
    }
  })
}

fn apply_parent_gql_guards(
  guards: List(Guard),
  resolver: fn(Option(Dynamic), Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
) -> fn(Option(Dynamic), Dict(String, Dynamic), ExecutionContext) ->
  Result(result, GqlError) {
  list.fold(list.reverse(guards), resolver, fn(inner, g) {
    fn(parent, args, ctx) {
      case g(ctx) {
        Ok(Nil) -> inner(parent, args, ctx)
        Error(msg) -> Error(error.new(msg))
      }
    }
  })
}

// ============================================================================
// Argument Helpers
// ============================================================================

/// Create an argument definition
pub fn arg(name: String, arg_type: FieldType) -> ArgDef {
  ArgDef(name: name, arg_type: arg_type, description: None, default_value: None)
}

/// Create an argument with description
pub fn arg_with_desc(
  name: String,
  arg_type: FieldType,
  description: String,
) -> ArgDef {
  ArgDef(
    name: name,
    arg_type: arg_type,
    description: Some(description),
    default_value: None,
  )
}

/// Create an argument with a default value
pub fn arg_with_default(
  name: String,
  arg_type: FieldType,
  default: Dynamic,
) -> ArgDef {
  ArgDef(
    name: name,
    arg_type: arg_type,
    description: None,
    default_value: Some(default),
  )
}

/// Create an argument with default value and description
pub fn arg_with_default_desc(
  name: String,
  arg_type: FieldType,
  default: Dynamic,
  description: String,
) -> ArgDef {
  ArgDef(
    name: name,
    arg_type: arg_type,
    description: Some(description),
    default_value: Some(default),
  )
}

// ============================================================================
// Argument Parsing Helpers
// ============================================================================

fn fetch_required(
  args: Dict(String, Dynamic),
  key: String,
  decoder: decode.Decoder(a),
  type_label: String,
) -> Result(a, GqlError) {
  case dict.get(args, key) {
    Error(_) -> Error(error.new("Missing required argument: " <> key))
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) {
        error.new(
          "Invalid type for argument '" <> key <> "': expected " <> type_label,
        )
      })
  }
}

fn fetch_optional(
  args: Dict(String, Dynamic),
  key: String,
  decoder: decode.Decoder(a),
) -> Option(a) {
  case dict.get(args, key) {
    Error(_) -> None
    Ok(value) ->
      case decode.run(value, decoder) {
        Ok(v) -> Some(v)
        Error(_) -> None
      }
  }
}

fn fetch_or(
  args: Dict(String, Dynamic),
  key: String,
  decoder: decode.Decoder(a),
  default: a,
) -> a {
  case dict.get(args, key) {
    Error(_) -> default
    Ok(value) -> result.unwrap(decode.run(value, decoder), default)
  }
}

pub fn get_string(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(String, GqlError) {
  fetch_required(args, key, decode.string, "String")
}

pub fn get_id(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(String, GqlError) {
  fetch_required(args, key, decode.string, "ID")
}

pub fn get_int(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Int, GqlError) {
  fetch_required(args, key, decode.int, "Int")
}

pub fn get_float(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Float, GqlError) {
  fetch_required(args, key, decode.float, "Float")
}

pub fn get_bool(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Bool, GqlError) {
  fetch_required(args, key, decode.bool, "Bool")
}

pub fn get_optional_string(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(String) {
  fetch_optional(args, key, decode.string)
}

pub fn get_optional_id(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(String) {
  get_optional_string(args, key)
}

pub fn get_optional_int(args: Dict(String, Dynamic), key: String) -> Option(Int) {
  fetch_optional(args, key, decode.int)
}

pub fn get_optional_float(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Float) {
  fetch_optional(args, key, decode.float)
}

pub fn get_optional_bool(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Bool) {
  fetch_optional(args, key, decode.bool)
}

pub fn get_string_list(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(List(String), GqlError) {
  fetch_required(args, key, decode.list(decode.string), "[String]")
}

pub fn get_int_list(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(List(Int), GqlError) {
  fetch_required(args, key, decode.list(decode.int), "[Int]")
}

pub fn decode_input(
  args: Dict(String, Dynamic),
  key: String,
  decoder: decode.Decoder(a),
) -> Result(a, GqlError) {
  case dict.get(args, key) {
    Error(_) -> Error(error.new("Missing required argument: " <> key))
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) {
        error.new("Invalid input for '" <> key <> "'")
      })
  }
}

pub fn get_dynamic(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Dynamic, GqlError) {
  dict.get(args, key)
  |> result.map_error(fn(_) { error.new("Missing required argument: " <> key) })
}

pub fn get_optional_dynamic(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Dynamic) {
  dict.get(args, key)
  |> option.from_result
}

pub fn get_string_or(
  args: Dict(String, Dynamic),
  key: String,
  default: String,
) -> String {
  fetch_or(args, key, decode.string, default)
}

pub fn get_id_or(
  args: Dict(String, Dynamic),
  key: String,
  default: String,
) -> String {
  get_string_or(args, key, default)
}

pub fn get_int_or(args: Dict(String, Dynamic), key: String, default: Int) -> Int {
  fetch_or(args, key, decode.int, default)
}

pub fn get_float_or(
  args: Dict(String, Dynamic),
  key: String,
  default: Float,
) -> Float {
  fetch_or(args, key, decode.float, default)
}

pub fn get_bool_or(
  args: Dict(String, Dynamic),
  key: String,
  default: Bool,
) -> Bool {
  fetch_or(args, key, decode.bool, default)
}

// ============================================================================
// Schema Building
// ============================================================================

fn build_gql_resolver(
  resolver: fn(Dict(String, Dynamic), ExecutionContext) ->
    Result(result, GqlError),
  result_encoder: fn(result) -> Dynamic,
) -> schema.RichResolver {
  fn(info: ResolverInfo) {
    case resolver(info.arguments, info.context) {
      Error(err) -> Error(error.to_payload(err))
      Ok(res) -> Ok(result_encoder(res))
    }
  }
}

fn apply_desc(field: FieldDefinition, desc: Option(String)) -> FieldDefinition {
  case desc {
    Some(d) -> schema.field_description(field, d)
    None -> field
  }
}

/// Convert any Op to a schema FieldDefinition.
pub fn to_field_def(op: Op(k, r)) -> FieldDefinition {
  let base =
    schema.field_def(op.name, op.return_type)
    |> add_args_to_field(op.arg_definitions)
    |> apply_desc(op.description)
  case op.op_resolver {
    SimpleResolver(resolver) ->
      schema.rich_resolver_fn(
        base,
        build_gql_resolver(
          apply_gql_guards(op.guards, resolver),
          op.result_encoder,
        ),
      )
    TopicResolver(resolver) -> {
      let guarded_topic = apply_topic_guards(op.guards, resolver)
      base
      |> schema.resolver(fn(_: ResolverInfo) {
        Error(
          "Subscriptions must be executed through the subscription executor",
        )
      })
      |> fn(fd) {
        schema.FieldDefinition(
          ..fd,
          topic_fn: Some(fn(args, ctx) {
            guarded_topic(args, ctx)
            |> result.map_error(fn(e) { e.message })
          }),
        )
      }
    }
    ParentResolver(resolver) -> {
      let guarded = apply_parent_gql_guards(op.guards, resolver)
      schema.rich_resolver_fn(base, fn(info: ResolverInfo) {
        case guarded(info.parent, info.arguments, info.context) {
          Error(err) -> Error(error.to_payload(err))
          Ok(res) -> Ok(op.result_encoder(res))
        }
      })
    }
  }
}

fn add_args_to_field(
  field: FieldDefinition,
  args: List(ArgDef),
) -> FieldDefinition {
  list.fold(args, field, fn(f, a) {
    let base_arg = schema.arg(a.name, a.arg_type)

    let with_desc = case a.description {
      Some(desc) -> schema.arg_description(base_arg, desc)
      None -> base_arg
    }

    let arg_def = case a.default_value {
      Some(default) -> schema.default_value(with_desc, default)
      None -> with_desc
    }

    schema.argument(f, arg_def)
  })
}

// ============================================================================
// Schema Builder
// ============================================================================

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
    inputs: List(schema.InputObjectType),
    cache: Bool,
  )
}

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
    inputs: [],
    cache: False,
  )
}

pub fn add_query(builder: SchemaBuilder, q: Op(QueryKind, r)) -> SchemaBuilder {
  SchemaBuilder(..builder, queries: [to_field_def(q), ..builder.queries])
}

pub fn add_mutation(
  builder: SchemaBuilder,
  m: Op(MutationKind, r),
) -> SchemaBuilder {
  SchemaBuilder(..builder, mutations: [to_field_def(m), ..builder.mutations])
}

pub fn add_subscription(
  builder: SchemaBuilder,
  s: Op(SubscriptionKind, e),
) -> SchemaBuilder {
  SchemaBuilder(..builder, subscriptions: [
    to_field_def(s),
    ..builder.subscriptions
  ])
}

pub fn add_type(builder: SchemaBuilder, t: ObjectType) -> SchemaBuilder {
  SchemaBuilder(..builder, types: [t, ..builder.types])
}

pub fn add_enum(builder: SchemaBuilder, e: schema.EnumType) -> SchemaBuilder {
  SchemaBuilder(..builder, enums: [e, ..builder.enums])
}

pub fn add_interface(
  builder: SchemaBuilder,
  i: schema.InterfaceType,
) -> SchemaBuilder {
  SchemaBuilder(..builder, interfaces: [i, ..builder.interfaces])
}

pub fn add_union(builder: SchemaBuilder, u: schema.UnionType) -> SchemaBuilder {
  SchemaBuilder(..builder, unions: [u, ..builder.unions])
}

pub fn add_scalar(builder: SchemaBuilder, s: schema.ScalarType) -> SchemaBuilder {
  SchemaBuilder(..builder, scalars: [s, ..builder.scalars])
}

pub fn add_input(
  builder: SchemaBuilder,
  i: schema.InputObjectType,
) -> SchemaBuilder {
  SchemaBuilder(..builder, inputs: [i, ..builder.inputs])
}

pub fn add_queries(
  builder: SchemaBuilder,
  queries: List(Op(QueryKind, r)),
) -> SchemaBuilder {
  list.fold(queries, builder, fn(b, q) { add_query(b, q) })
}

pub fn add_mutations(
  builder: SchemaBuilder,
  mutations: List(Op(MutationKind, r)),
) -> SchemaBuilder {
  list.fold(mutations, builder, fn(b, m) { add_mutation(b, m) })
}

pub fn add_subscriptions(
  builder: SchemaBuilder,
  subscriptions: List(Op(SubscriptionKind, e)),
) -> SchemaBuilder {
  list.fold(subscriptions, builder, fn(b, s) { add_subscription(b, s) })
}

pub fn add_types(
  builder: SchemaBuilder,
  types: List(ObjectType),
) -> SchemaBuilder {
  list.fold(types, builder, fn(b, t) { add_type(b, t) })
}

pub fn add_enums(
  builder: SchemaBuilder,
  enums: List(schema.EnumType),
) -> SchemaBuilder {
  list.fold(enums, builder, fn(b, e) { add_enum(b, e) })
}

pub fn add_interfaces(
  builder: SchemaBuilder,
  interfaces: List(schema.InterfaceType),
) -> SchemaBuilder {
  list.fold(interfaces, builder, fn(b, i) { add_interface(b, i) })
}

pub fn add_unions(
  builder: SchemaBuilder,
  unions: List(schema.UnionType),
) -> SchemaBuilder {
  list.fold(unions, builder, fn(b, u) { add_union(b, u) })
}

pub fn add_scalars(
  builder: SchemaBuilder,
  scalars: List(schema.ScalarType),
) -> SchemaBuilder {
  list.fold(scalars, builder, fn(b, s) { add_scalar(b, s) })
}

pub fn add_inputs(
  builder: SchemaBuilder,
  inputs: List(schema.InputObjectType),
) -> SchemaBuilder {
  list.fold(inputs, builder, fn(b, i) { add_input(b, i) })
}

/// Register all built-in custom scalars (DateTime, Date, UUID, Email, URL, JSON).
pub fn with_common_scalars(builder: SchemaBuilder) -> SchemaBuilder {
  add_scalars(builder, [
    scalars.date_time(),
    scalars.date(),
    scalars.uuid(),
    scalars.email(),
    scalars.url(),
    scalars.json(),
  ])
}

fn check_duplicates(
  label: String,
  a_items: List(a),
  b_items: List(a),
  name_fn: fn(a) -> String,
) -> Nil {
  let a_names = list.map(a_items, name_fn)
  let conflicts =
    list.filter(b_items, fn(item) { list.contains(a_names, name_fn(item)) })
  case conflicts {
    [] -> Nil
    _ ->
      panic as {
        "Schema merge conflict: duplicate "
        <> label
        <> " name(s): "
        <> string.join(list.map(conflicts, name_fn), ", ")
      }
  }
}

pub fn merge(a: SchemaBuilder, b: SchemaBuilder) -> SchemaBuilder {
  check_duplicates("query", a.queries, b.queries, fn(f) { f.name })
  check_duplicates("mutation", a.mutations, b.mutations, fn(f) { f.name })
  check_duplicates("subscription", a.subscriptions, b.subscriptions, fn(f) {
    f.name
  })
  check_duplicates("type", a.types, b.types, fn(t) { t.name })
  check_duplicates("enum", a.enums, b.enums, fn(e) { e.name })
  check_duplicates("interface", a.interfaces, b.interfaces, fn(i) { i.name })
  check_duplicates("union", a.unions, b.unions, fn(u) { u.name })
  check_duplicates("scalar", a.scalars, b.scalars, fn(s) { s.name })
  check_duplicates("input", a.inputs, b.inputs, fn(i) { i.name })
  SchemaBuilder(
    queries: list.append(a.queries, b.queries),
    mutations: list.append(a.mutations, b.mutations),
    subscriptions: list.append(a.subscriptions, b.subscriptions),
    types: list.append(a.types, b.types),
    enums: list.append(a.enums, b.enums),
    interfaces: list.append(a.interfaces, b.interfaces),
    unions: list.append(a.unions, b.unions),
    scalars: list.append(a.scalars, b.scalars),
    inputs: list.append(a.inputs, b.inputs),
    cache: a.cache || b.cache,
  )
}

pub fn with_cache(builder: SchemaBuilder) -> SchemaBuilder {
  SchemaBuilder(..builder, cache: True)
}


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

  let with_scalars =
    list.fold(builder.scalars, with_unions, fn(s, scalar) {
      schema.add_type(s, schema.ScalarTypeDef(scalar))
    })

  let with_inputs =
    list.fold(builder.inputs, with_scalars, fn(s, input) {
      schema.add_type(s, schema.InputObjectTypeDef(input))
    })

  let with_directives =
    list.fold(schema.builtin_directives(), with_inputs, fn(s, directive) {
      schema.add_directive(s, directive)
    })

  schema.Schema(
    ..with_directives,
    document_cache: option.Some(document_cache.new()),
  )
}
