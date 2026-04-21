// mochi/query.gleam
// Code First API for defining GraphQL queries and mutations
// Inspired by gqlkit's simple, type-safe approach

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/document_cache
import mochi/scalars
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
  ArgDef(
    name: String,
    arg_type: FieldType,
    description: Option(String),
    default_value: Option(Dynamic),
  )
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
///
/// Uses labeled arguments for clarity:
/// ```gleam
/// query.query_with_args(
///   name: "user",
///   args: [query.arg("id", schema.non_null(schema.id_type()))],
///   returns: schema.named_type("User"),
///   decode: fn(args) { get_id(args, "id") },
///   resolve: fn(id, ctx) { get_user_by_id(id) },
///   encode: fn(user) { types.to_dynamic(user) },
/// )
/// ```
pub fn query_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  decode args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolve resolver: fn(args, ExecutionContext) -> Result(result, String),
  encode encoder: fn(result) -> Dynamic,
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

/// Add a guard to a query. The guard runs before the resolver — if it returns
/// Error, the resolver is skipped and the error is returned.
/// Multiple guards can be stacked by piping; the last guard added is checked
/// first (outermost wrapper), so place the broadest checks last:
///
/// ```gleam
/// let my_posts = query.query_with_args(
///   name: "myPosts", ...
/// )
/// |> query.with_guard(require_admin)  // checked second
/// |> query.with_guard(require_auth)   // checked first (outermost)
/// ```
pub fn with_guard(
  q: QueryDef(args, result),
  guard_fn: fn(ExecutionContext) -> Result(Nil, String),
) -> QueryDef(args, result) {
  let original = q.resolver
  QueryDef(..q, resolver: fn(args, ctx) {
    case guard_fn(ctx) {
      Ok(Nil) -> original(args, ctx)
      Error(msg) -> Error(msg)
    }
  })
}

// ============================================================================
// Mutation Builders
// ============================================================================

/// Define a mutation with arguments
///
/// Uses labeled arguments for clarity:
/// ```gleam
/// query.mutation(
///   name: "createUser",
///   args: [query.arg("input", schema.non_null(schema.named_type("CreateUserInput")))],
///   returns: schema.named_type("User"),
///   decode: fn(args) { decode_create_user_input(args) },
///   resolve: fn(input, ctx) { create_user(input) },
///   encode: fn(user) { types.to_dynamic(user) },
/// )
/// ```
pub fn mutation(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  decode args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolve resolver: fn(args, ExecutionContext) -> Result(result, String),
  encode encoder: fn(result) -> Dynamic,
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

/// Add a guard to a mutation. The guard runs before the resolver — if it returns
/// Error, the resolver is skipped and the error is returned.
/// Multiple guards can be stacked by piping; the last guard added is checked first.
pub fn mutation_with_guard(
  m: MutationDef(args, result),
  guard_fn: fn(ExecutionContext) -> Result(Nil, String),
) -> MutationDef(args, result) {
  let original = m.resolver
  MutationDef(..m, resolver: fn(args, ctx) {
    case guard_fn(ctx) {
      Ok(Nil) -> original(args, ctx)
      Error(msg) -> Error(msg)
    }
  })
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
///
/// Uses labeled arguments for clarity:
/// ```gleam
/// query.subscription_with_args(
///   name: "onMessage",
///   args: [query.arg("channelId", schema.non_null(schema.id_type()))],
///   returns: schema.named_type("Message"),
///   decode: fn(args) { get_id(args, "channelId") },
///   topic: fn(channel_id, ctx) { Ok("channel:" <> channel_id) },
///   encode: fn(msg) { types.to_dynamic(msg) },
/// )
/// ```
pub fn subscription_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  decode args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  topic topic_resolver: fn(args, ExecutionContext) -> Result(String, String),
  encode encoder: fn(event) -> Dynamic,
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

/// Add a guard to a subscription. The guard runs before the topic resolver —
/// if it returns Error, the subscription is rejected.
/// Multiple guards can be stacked by piping; the last guard added is checked first.
pub fn subscription_with_guard(
  s: SubscriptionDef(args, event),
  guard_fn: fn(ExecutionContext) -> Result(Nil, String),
) -> SubscriptionDef(args, event) {
  let original = s.topic_resolver
  SubscriptionDef(..s, topic_resolver: fn(args, ctx) {
    case guard_fn(ctx) {
      Ok(Nil) -> original(args, ctx)
      Error(msg) -> Error(msg)
    }
  })
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
///
/// Uses labeled arguments for clarity:
/// ```gleam
/// query.field_with_args(
///   name: "posts",
///   args: [query.arg("limit", schema.int_type())],
///   returns: schema.list_type(schema.named_type("Post")),
///   parent: decode_user,
///   decode: fn(args) { get_optional_int(args, "limit") },
///   resolve: fn(user, limit, ctx) { get_user_posts(user.id, limit) },
///   encode: fn(posts) { types.to_dynamic(posts) },
/// )
/// ```
pub fn field_with_args(
  name name: String,
  args arg_defs: List(ArgDef),
  returns return_type: FieldType,
  parent parent_decoder: fn(Dynamic) -> Result(parent, String),
  decode args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolve resolver: fn(parent, args, ExecutionContext) -> Result(result, String),
  encode encoder: fn(result) -> Dynamic,
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

/// Add a guard to a field definition. The guard runs before the resolver — if it
/// returns Error, the resolver is skipped and the error is returned.
/// Multiple guards can be stacked by piping; the last guard added is checked first.
pub fn field_with_guard(
  f: FieldDef(parent, args, result),
  guard_fn: fn(ExecutionContext) -> Result(Nil, String),
) -> FieldDef(parent, args, result) {
  let original = f.resolver
  FieldDef(..f, resolver: fn(parent, args, ctx) {
    case guard_fn(ctx) {
      Ok(Nil) -> original(parent, args, ctx)
      Error(msg) -> Error(msg)
    }
  })
}

// ============================================================================
// Guard Combinators
// ============================================================================

/// High-level guard type for the Code First API.
pub type HighLevelGuard =
  fn(ExecutionContext) -> Result(Nil, String)

/// Combine guards with AND logic — all must pass (checked in list order).
pub fn all_of(guard_fns: List(HighLevelGuard)) -> HighLevelGuard {
  fn(ctx) { list.try_each(guard_fns, fn(g) { g(ctx) }) }
}

/// Combine guards with OR logic — at least one must pass.
/// Fails with the last error if all fail.
pub fn any_of(guard_fns: List(HighLevelGuard)) -> HighLevelGuard {
  fn(ctx) {
    case guard_fns {
      [] -> Error("No guards provided")
      _ -> try_any(guard_fns, ctx, "No guards passed")
    }
  }
}

fn try_any(
  guards: List(HighLevelGuard),
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
///
/// ```gleam
/// query.arg_with_default("limit", schema.int_type(), types.to_dynamic(10))
/// ```
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
///
/// ```gleam
/// query.arg_with_default_desc("limit", schema.int_type(), types.to_dynamic(10), "Maximum items to return")
/// ```
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
) -> Result(a, String) {
  case dict.get(args, key) {
    Error(_) -> Error("Missing required argument: " <> key)
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) {
        "Invalid type for argument '" <> key <> "': expected " <> type_label
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

/// Get a required string argument from the arguments dict
///
/// ```gleam
/// fn decode_args(args) {
///   use name <- result.try(query.get_string(args, "name"))
///   Ok(name)
/// }
/// ```
pub fn get_string(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(String, String) {
  fetch_required(args, key, decode.string, "String")
}

/// Get a required ID argument (as String) from the arguments dict
///
/// ```gleam
/// fn decode_args(args) {
///   use id <- result.try(query.get_id(args, "id"))
///   Ok(id)
/// }
/// ```
pub fn get_id(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(String, String) {
  fetch_required(args, key, decode.string, "ID")
}

/// Get a required int argument from the arguments dict
///
/// ```gleam
/// fn decode_args(args) {
///   use age <- result.try(query.get_int(args, "age"))
///   Ok(age)
/// }
/// ```
pub fn get_int(args: Dict(String, Dynamic), key: String) -> Result(Int, String) {
  fetch_required(args, key, decode.int, "Int")
}

/// Get a required float argument from the arguments dict
pub fn get_float(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Float, String) {
  fetch_required(args, key, decode.float, "Float")
}

/// Get a required bool argument from the arguments dict
pub fn get_bool(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Bool, String) {
  fetch_required(args, key, decode.bool, "Bool")
}

/// Get an optional string argument from the arguments dict
///
/// ```gleam
/// fn decode_args(args) {
///   let name = query.get_optional_string(args, "name")
///   Ok(SearchArgs(name: name))
/// }
/// ```
pub fn get_optional_string(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(String) {
  fetch_optional(args, key, decode.string)
}

/// Get an optional ID argument from the arguments dict
pub fn get_optional_id(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(String) {
  get_optional_string(args, key)
}

/// Get an optional int argument from the arguments dict
pub fn get_optional_int(args: Dict(String, Dynamic), key: String) -> Option(Int) {
  fetch_optional(args, key, decode.int)
}

/// Get an optional float argument from the arguments dict
pub fn get_optional_float(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Float) {
  fetch_optional(args, key, decode.float)
}

/// Get an optional bool argument from the arguments dict
pub fn get_optional_bool(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Bool) {
  fetch_optional(args, key, decode.bool)
}

/// Get a required list of strings from the arguments dict
pub fn get_string_list(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(List(String), String) {
  fetch_required(args, key, decode.list(decode.string), "[String]")
}

/// Get a required list of ints from the arguments dict
pub fn get_int_list(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(List(Int), String) {
  fetch_required(args, key, decode.list(decode.int), "[Int]")
}

pub fn decode_input(
  args: Dict(String, Dynamic),
  key: String,
  decoder: decode.Decoder(a),
) -> Result(a, String) {
  case dict.get(args, key) {
    Error(_) -> Error("Missing required argument: " <> key)
    Ok(value) ->
      decode.run(value, decoder)
      |> result.map_error(fn(_) { "Invalid input for '" <> key <> "'" })
  }
}

pub fn get_dynamic(
  args: Dict(String, Dynamic),
  key: String,
) -> Result(Dynamic, String) {
  dict.get(args, key)
  |> result.map_error(fn(_) { "Missing required argument: " <> key })
}

pub fn get_optional_dynamic(
  args: Dict(String, Dynamic),
  key: String,
) -> Option(Dynamic) {
  dict.get(args, key)
  |> option.from_result
}

// ============================================================================
// Argument Parsing Helpers with Defaults
// ============================================================================

/// Get a string argument or return default if not present
///
/// ```gleam
/// let name = query.get_string_or(args, "name", "Anonymous")
/// ```
pub fn get_string_or(
  args: Dict(String, Dynamic),
  key: String,
  default: String,
) -> String {
  fetch_or(args, key, decode.string, default)
}

/// Get an ID argument or return default if not present
pub fn get_id_or(
  args: Dict(String, Dynamic),
  key: String,
  default: String,
) -> String {
  get_string_or(args, key, default)
}

/// Get an int argument or return default if not present
///
/// ```gleam
/// let limit = query.get_int_or(args, "limit", 10)
/// ```
pub fn get_int_or(args: Dict(String, Dynamic), key: String, default: Int) -> Int {
  fetch_or(args, key, decode.int, default)
}

/// Get a float argument or return default if not present
///
/// ```gleam
/// let price = query.get_float_or(args, "price", 0.0)
/// ```
pub fn get_float_or(
  args: Dict(String, Dynamic),
  key: String,
  default: Float,
) -> Float {
  fetch_or(args, key, decode.float, default)
}

/// Get a bool argument or return default if not present
///
/// ```gleam
/// let active = query.get_bool_or(args, "active", True)
/// ```
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

fn build_op_resolver(
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) -> Result(result, String),
  result_encoder: fn(result) -> Dynamic,
) -> Resolver {
  fn(info: ResolverInfo) {
    use decoded_args <- result.try(
      args_decoder(info.arguments)
      |> result.map_error(fn(e) { "Failed to decode arguments: " <> e }),
    )
    use res <- result.map(resolver(decoded_args, info.context))
    result_encoder(res)
  }
}

fn build_rich_op_resolver(
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) ->
    Result(result, schema.RichResolverPayload),
  result_encoder: fn(result) -> Dynamic,
) -> schema.RichResolver {
  fn(info: ResolverInfo) {
    case args_decoder(info.arguments) {
      Error(e) -> Error(#("Failed to decode arguments: " <> e, option.None))
      Ok(decoded_args) ->
        case resolver(decoded_args, info.context) {
          Error(payload) -> Error(payload)
          Ok(res) -> Ok(result_encoder(res))
        }
    }
  }
}

fn apply_desc(field: FieldDefinition, desc: Option(String)) -> FieldDefinition {
  case desc {
    Some(d) -> schema.field_description(field, d)
    None -> field
  }
}

/// Convert a QueryDef to a schema FieldDefinition
pub fn query_to_field_def(q: QueryDef(args, result)) -> FieldDefinition {
  schema.field_def(q.name, q.return_type)
  |> schema.resolver(build_op_resolver(
    q.args_decoder,
    q.resolver,
    q.result_encoder,
  ))
  |> add_args_to_field(q.arg_definitions)
  |> apply_desc(q.description)
}

/// Convert a MutationDef to a schema FieldDefinition
pub fn mutation_to_field_def(m: MutationDef(args, result)) -> FieldDefinition {
  schema.field_def(m.name, m.return_type)
  |> schema.resolver(build_op_resolver(
    m.args_decoder,
    m.resolver,
    m.result_encoder,
  ))
  |> add_args_to_field(m.arg_definitions)
  |> apply_desc(m.description)
}

/// Convert a SubscriptionDef to a schema FieldDefinition.
/// Actual subscription logic is handled by the subscription executor;
/// this creates the schema definition for introspection and SDL generation.
pub fn subscription_to_field_def(
  s: SubscriptionDef(args, event),
) -> FieldDefinition {
  // placeholder — subscriptions are routed through the subscription executor, not resolved here
  let resolver: Resolver = fn(_info: ResolverInfo) {
    Error("Subscriptions must be executed through the subscription executor")
  }

  let topic_fn =
    Some(fn(raw_args: Dict(String, Dynamic), ctx: ExecutionContext) {
      case s.args_decoder(raw_args) {
        Ok(decoded_args) -> s.topic_resolver(decoded_args, ctx)
        Error(e) -> Error("Failed to decode subscription args: " <> e)
      }
    })

  schema.field_def(s.name, s.return_type)
  |> schema.resolver(resolver)
  |> fn(fd) { schema.FieldDefinition(..fd, topic_fn: topic_fn) }
  |> add_args_to_field(s.arg_definitions)
  |> apply_desc(s.description)
}

/// Convert a FieldDef to a schema FieldDefinition
pub fn field_def_to_schema(f: FieldDef(parent, args, result)) -> FieldDefinition {
  let resolver: Resolver = fn(info: ResolverInfo) {
    use parent_dyn <- result.try(option.to_result(
      info.parent,
      "No parent value provided",
    ))
    use parent <- result.try(
      f.parent_decoder(parent_dyn)
      |> result.map_error(fn(e) { "Failed to decode parent: " <> e }),
    )
    use decoded_args <- result.try(
      f.args_decoder(info.arguments)
      |> result.map_error(fn(e) { "Failed to decode arguments: " <> e }),
    )
    use res <- result.map(f.resolver(parent, decoded_args, info.context))
    f.result_encoder(res)
  }

  schema.field_def(f.name, f.return_type)
  |> schema.resolver(resolver)
  |> add_args_to_field(f.arg_definitions)
  |> apply_desc(f.description)
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
    inputs: List(schema.InputObjectType),
    cache: Bool,
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
    inputs: [],
    cache: False,
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

pub fn add_rich_query(
  builder: SchemaBuilder,
  name: String,
  arg_defs: List(ArgDef),
  return_type: schema.FieldType,
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) ->
    Result(result, schema.RichResolverPayload),
  encoder: fn(result) -> Dynamic,
) -> SchemaBuilder {
  let field =
    schema.field_def(name, return_type)
    |> schema.rich_resolver_fn(build_rich_op_resolver(
      args_decoder,
      resolver,
      encoder,
    ))
    |> add_args_to_field(arg_defs)
  SchemaBuilder(..builder, queries: [field, ..builder.queries])
}

pub fn add_rich_mutation(
  builder: SchemaBuilder,
  name: String,
  arg_defs: List(ArgDef),
  return_type: schema.FieldType,
  args_decoder: fn(Dict(String, Dynamic)) -> Result(args, String),
  resolver: fn(args, ExecutionContext) ->
    Result(result, schema.RichResolverPayload),
  encoder: fn(result) -> Dynamic,
) -> SchemaBuilder {
  let field =
    schema.field_def(name, return_type)
    |> schema.rich_resolver_fn(build_rich_op_resolver(
      args_decoder,
      resolver,
      encoder,
    ))
    |> add_args_to_field(arg_defs)
  SchemaBuilder(..builder, mutations: [field, ..builder.mutations])
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

/// Add an input object type
pub fn add_input(
  builder: SchemaBuilder,
  i: schema.InputObjectType,
) -> SchemaBuilder {
  SchemaBuilder(..builder, inputs: [i, ..builder.inputs])
}

/// Add multiple queries to the schema
pub fn add_queries(
  builder: SchemaBuilder,
  queries: List(QueryDef(args, result)),
) -> SchemaBuilder {
  list.fold(queries, builder, fn(b, q) { add_query(b, q) })
}

/// Add multiple mutations to the schema
pub fn add_mutations(
  builder: SchemaBuilder,
  mutations: List(MutationDef(args, result)),
) -> SchemaBuilder {
  list.fold(mutations, builder, fn(b, m) { add_mutation(b, m) })
}

/// Add multiple subscriptions to the schema
pub fn add_subscriptions(
  builder: SchemaBuilder,
  subscriptions: List(SubscriptionDef(args, event)),
) -> SchemaBuilder {
  list.fold(subscriptions, builder, fn(b, s) { add_subscription(b, s) })
}

/// Add multiple type definitions
pub fn add_types(
  builder: SchemaBuilder,
  types: List(ObjectType),
) -> SchemaBuilder {
  list.fold(types, builder, fn(b, t) { add_type(b, t) })
}

/// Add multiple enum types
pub fn add_enums(
  builder: SchemaBuilder,
  enums: List(schema.EnumType),
) -> SchemaBuilder {
  list.fold(enums, builder, fn(b, e) { add_enum(b, e) })
}

/// Add multiple interface types
pub fn add_interfaces(
  builder: SchemaBuilder,
  interfaces: List(schema.InterfaceType),
) -> SchemaBuilder {
  list.fold(interfaces, builder, fn(b, i) { add_interface(b, i) })
}

/// Add multiple union types
pub fn add_unions(
  builder: SchemaBuilder,
  unions: List(schema.UnionType),
) -> SchemaBuilder {
  list.fold(unions, builder, fn(b, u) { add_union(b, u) })
}

/// Add multiple custom scalar types
pub fn add_scalars(
  builder: SchemaBuilder,
  scalars: List(schema.ScalarType),
) -> SchemaBuilder {
  list.fold(scalars, builder, fn(b, s) { add_scalar(b, s) })
}

/// Add multiple input object types
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

/// Build the final schema
pub fn with_cache(builder: SchemaBuilder) -> SchemaBuilder {
  SchemaBuilder(..builder, cache: True)
}

pub fn build_without_cache(builder: SchemaBuilder) -> Schema {
  build(SchemaBuilder(..builder, cache: False))
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
