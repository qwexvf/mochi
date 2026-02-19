// mochi/middleware.gleam
// Middleware system for GraphQL field resolution

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema.{type FieldDefinition, type MiddlewareFn, type ResolverInfo}
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// Information about the current field being resolved
pub type ResolverContext {
  ResolverContext(
    /// The field name being resolved
    field_name: String,
    /// The parent type name
    parent_type: String,
    /// Current path in the query
    path: List(String),
    /// The full resolver info
    info: ResolverInfo,
  )
}

/// Resolution state passed through the middleware chain
pub type Resolution {
  Resolution(
    /// The resolved value (None if not yet resolved or if error occurred)
    value: Option(Dynamic),
    /// Error message if resolution failed
    error: Option(String),
    /// Field name being resolved
    field_name: String,
    /// Parent type name
    parent_type: String,
    /// Resolver context
    context: ResolverContext,
    /// Private data that middleware can use to communicate
    private: Dict(String, Dynamic),
  )
}

/// Middleware function type
/// Takes the current resolution state and a "next" function to continue the chain
pub type Middleware =
  fn(Resolution, fn(Resolution) -> Resolution) -> Resolution

/// A middleware definition with metadata
pub type MiddlewareDef {
  MiddlewareDef(
    /// Name for debugging/logging
    name: String,
    /// Priority - lower values run first (default: 100)
    priority: Int,
    /// Optional filter to only apply to certain fields
    field_filter: Option(FieldFilter),
    /// The middleware function
    middleware: Middleware,
  )
}

/// Filter to determine which fields a middleware applies to
pub type FieldFilter {
  /// Apply to specific (type, field) pairs
  SpecificFields(List(#(String, String)))
  /// Apply to all fields on specific types
  TypeFields(List(String))
  /// Apply to fields with specific names (on any type)
  NamedFields(List(String))
  /// Apply to all fields
  AllFields
}

/// The middleware pipeline containing all registered middleware
pub opaque type MiddlewarePipeline {
  MiddlewarePipeline(middleware: List(MiddlewareDef))
}

// ============================================================================
// Resolution Helpers
// ============================================================================

/// Create a new resolution state for a field
pub fn new_resolution(
  field_name: String,
  parent_type: String,
  context: ResolverContext,
) -> Resolution {
  Resolution(
    value: None,
    error: None,
    field_name: field_name,
    parent_type: parent_type,
    context: context,
    private: dict.new(),
  )
}

/// Set the resolved value
pub fn set_value(resolution: Resolution, value: Dynamic) -> Resolution {
  Resolution(..resolution, value: Some(value), error: None)
}

/// Set an error on the resolution
pub fn set_error(resolution: Resolution, error: String) -> Resolution {
  Resolution(..resolution, value: None, error: Some(error))
}

/// Add private data to the resolution
pub fn put_private(
  resolution: Resolution,
  key: String,
  value: Dynamic,
) -> Resolution {
  Resolution(..resolution, private: dict.insert(resolution.private, key, value))
}

/// Get private data from the resolution
pub fn get_private(resolution: Resolution, key: String) -> Option(Dynamic) {
  case dict.get(resolution.private, key) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

/// Check if resolution has a value
pub fn has_value(resolution: Resolution) -> Bool {
  option.is_some(resolution.value)
}

/// Check if resolution has an error
pub fn has_error(resolution: Resolution) -> Bool {
  option.is_some(resolution.error)
}

// ============================================================================
// Middleware Pipeline
// ============================================================================

/// Create a new empty middleware pipeline
pub fn new_pipeline() -> MiddlewarePipeline {
  MiddlewarePipeline(middleware: [])
}

/// Add middleware to the pipeline
pub fn add_middleware(
  pipeline: MiddlewarePipeline,
  mw: MiddlewareDef,
) -> MiddlewarePipeline {
  let new_list =
    [mw, ..pipeline.middleware]
    |> list.sort(fn(a, b) { int.compare(a.priority, b.priority) })
  MiddlewarePipeline(middleware: new_list)
}

/// Create a basic middleware definition
pub fn middleware(name: String, mw_fn: Middleware) -> MiddlewareDef {
  MiddlewareDef(
    name: name,
    priority: 100,
    field_filter: None,
    middleware: mw_fn,
  )
}

/// Set the priority of a middleware (lower runs first)
pub fn with_priority(mw: MiddlewareDef, priority: Int) -> MiddlewareDef {
  MiddlewareDef(..mw, priority: priority)
}

/// Set a field filter on middleware
pub fn with_filter(mw: MiddlewareDef, filter: FieldFilter) -> MiddlewareDef {
  MiddlewareDef(..mw, field_filter: Some(filter))
}

/// Check if a middleware applies to a given field
fn middleware_applies(
  mw: MiddlewareDef,
  parent_type: String,
  field_name: String,
) -> Bool {
  case mw.field_filter {
    None -> True
    Some(AllFields) -> True
    Some(SpecificFields(pairs)) ->
      list.any(pairs, fn(pair) { pair.0 == parent_type && pair.1 == field_name })
    Some(TypeFields(types)) -> list.contains(types, parent_type)
    Some(NamedFields(names)) -> list.contains(names, field_name)
  }
}

// ============================================================================
// Middleware Execution
// ============================================================================

/// Convert a MiddlewarePipeline to a schema.MiddlewareFn
///
/// This is used to wire a middleware pipeline into the execution context
/// without creating a circular dependency between schema and middleware.
///
/// ## Example
///
/// ```gleam
/// let pipeline = middleware.new_pipeline()
///   |> middleware.add_middleware(middleware.logging_middleware(io.println))
///
/// let ctx = schema.execution_context(user_ctx)
///   |> schema.with_middleware(middleware.to_executor_fn(pipeline))
/// ```
pub fn to_executor_fn(pipeline: MiddlewarePipeline) -> schema.MiddlewareFn {
  fn(parent_type, field_def, info, resolver) {
    execute_with_middleware(pipeline, parent_type, field_def, info, resolver)
  }
}

/// Execute a resolver with the middleware pipeline
pub fn execute_with_middleware(
  pipeline: MiddlewarePipeline,
  parent_type: String,
  field_def: FieldDefinition,
  info: ResolverInfo,
  resolver: fn(ResolverInfo) -> Result(Dynamic, String),
) -> Result(Dynamic, String) {
  let context =
    ResolverContext(
      field_name: field_def.name,
      parent_type: parent_type,
      path: [],
      info: info,
    )

  let resolution = new_resolution(field_def.name, parent_type, context)

  // Filter middleware that applies to this field
  let applicable_middleware =
    list.filter(pipeline.middleware, fn(mw) {
      middleware_applies(mw, parent_type, field_def.name)
    })

  // Build the middleware chain with the resolver at the end
  let final_resolution =
    execute_chain(applicable_middleware, resolution, fn(res) {
      // This is the "innermost" function - actually call the resolver
      case resolver(info) {
        Ok(value) -> set_value(res, value)
        Error(msg) -> set_error(res, msg)
      }
    })

  // Convert resolution back to Result
  case final_resolution.error {
    Some(err) -> Error(err)
    None ->
      case final_resolution.value {
        Some(value) -> Ok(value)
        None -> Ok(types.to_dynamic(Nil))
      }
  }
}

/// Execute the middleware chain recursively
fn execute_chain(
  middleware: List(MiddlewareDef),
  resolution: Resolution,
  next: fn(Resolution) -> Resolution,
) -> Resolution {
  case middleware {
    [] -> next(resolution)
    [mw, ..rest] -> {
      // Create a "next" function that continues to the rest of the chain
      let continue = fn(res: Resolution) { execute_chain(rest, res, next) }
      mw.middleware(resolution, continue)
    }
  }
}

// ============================================================================
// Common Middleware
// ============================================================================

/// Logging middleware that logs field resolution
pub fn logging_middleware(log_fn: fn(String) -> Nil) -> MiddlewareDef {
  middleware("logging", fn(resolution, next) {
    log_fn(
      "Resolving " <> resolution.parent_type <> "." <> resolution.field_name,
    )
    let result = next(resolution)
    case result.error {
      Some(err) ->
        log_fn(
          "Error resolving "
          <> resolution.parent_type
          <> "."
          <> resolution.field_name
          <> ": "
          <> err,
        )
      None ->
        log_fn(
          "Resolved " <> resolution.parent_type <> "." <> resolution.field_name,
        )
    }
    result
  })
}

/// Authorization middleware that checks for a specific role in context
pub fn auth_middleware(
  role_extractor: fn(Dynamic) -> Option(String),
  required_role: String,
) -> MiddlewareDef {
  middleware("auth", fn(resolution, next) {
    case role_extractor(resolution.context.info.context.user_context) {
      Some(role) if role == required_role -> next(resolution)
      Some(_) ->
        set_error(
          resolution,
          "Forbidden: requires role '" <> required_role <> "'",
        )
      None -> set_error(resolution, "Unauthorized: authentication required")
    }
  })
  |> with_priority(10)
}

/// Rate limiting middleware (simple in-memory version)
/// Note: For production, use external state (Redis, etc.)
pub fn rate_limit_middleware(
  key_extractor: fn(Resolution) -> String,
  max_requests: Int,
  checker: fn(String, Int) -> Bool,
) -> MiddlewareDef {
  middleware("rate_limit", fn(resolution, next) {
    let key = key_extractor(resolution)
    case checker(key, max_requests) {
      True -> next(resolution)
      False -> set_error(resolution, "Rate limit exceeded")
    }
  })
  |> with_priority(5)
}

/// Caching middleware that caches field results
pub fn caching_middleware(
  cache_key: fn(Resolution) -> String,
  get_cached: fn(String) -> Option(Dynamic),
  set_cached: fn(String, Dynamic) -> Nil,
) -> MiddlewareDef {
  middleware("cache", fn(resolution, next) {
    let key = cache_key(resolution)
    case get_cached(key) {
      Some(cached) -> set_value(resolution, cached)
      None -> {
        let result = next(resolution)
        case result.value {
          Some(value) -> {
            set_cached(key, value)
            result
          }
          None -> result
        }
      }
    }
  })
  |> with_priority(20)
}

/// Timing middleware that records field resolution time
pub fn timing_middleware(
  get_time: fn() -> Int,
  record_timing: fn(String, String, Int) -> Nil,
) -> MiddlewareDef {
  middleware("timing", fn(resolution, next) {
    let start = get_time()
    let result = next(resolution)
    let duration = get_time() - start
    record_timing(resolution.parent_type, resolution.field_name, duration)
    result
  })
  |> with_priority(1)
}

/// Transform middleware that transforms the resolved value
pub fn transform_middleware(transform: fn(Dynamic) -> Dynamic) -> MiddlewareDef {
  middleware("transform", fn(resolution, next) {
    let result = next(resolution)
    case result.value {
      Some(value) -> set_value(result, transform(value))
      None -> result
    }
  })
  |> with_priority(200)
}

/// Validation middleware that validates arguments
pub fn validation_middleware(
  validator: fn(ResolverInfo) -> Result(Nil, String),
) -> MiddlewareDef {
  middleware("validation", fn(resolution, next) {
    case validator(resolution.context.info) {
      Ok(_) -> next(resolution)
      Error(msg) -> set_error(resolution, "Validation error: " <> msg)
    }
  })
  |> with_priority(15)
}

/// Error wrapping middleware that transforms errors
pub fn error_wrapper_middleware(wrapper: fn(String) -> String) -> MiddlewareDef {
  middleware("error_wrapper", fn(resolution, next) {
    let result = next(resolution)
    case result.error {
      Some(err) -> set_error(result, wrapper(err))
      None -> result
    }
  })
  |> with_priority(250)
}
