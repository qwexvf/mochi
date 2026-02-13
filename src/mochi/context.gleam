// mochi/context.gleam
// Context builder system for constructing execution context from HTTP requests

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/result
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// Information extracted from the incoming HTTP request
pub type RequestInfo {
  RequestInfo(
    /// HTTP headers (lowercase keys)
    headers: Dict(String, String),
    /// HTTP method (GET, POST, etc.)
    method: String,
    /// Request path
    path: String,
  )
}

/// A function that builds/transforms context from request info
/// Takes the request info and current context, returns modified context or error
pub type ContextBuilder =
  fn(RequestInfo, Dict(String, Dynamic)) ->
    Result(Dict(String, Dynamic), String)

/// Pipeline of context builders that execute in order
pub opaque type ContextPipeline {
  ContextPipeline(builders: List(ContextBuilder))
}

// ============================================================================
// Request Info Helpers
// ============================================================================

/// Create a new RequestInfo
pub fn request_info(
  headers: Dict(String, String),
  method: String,
  path: String,
) -> RequestInfo {
  RequestInfo(headers: headers, method: method, path: path)
}

/// Get a header value from request info (case-insensitive)
pub fn get_header(info: RequestInfo, name: String) -> Result(String, Nil) {
  dict.get(info.headers, name)
}

/// Get the Authorization header value
pub fn get_authorization(info: RequestInfo) -> Result(String, Nil) {
  get_header(info, "authorization")
}

/// Get bearer token from Authorization header
pub fn get_bearer_token(info: RequestInfo) -> Result(String, Nil) {
  case get_authorization(info) {
    Ok(auth) -> {
      case auth {
        "Bearer " <> token -> Ok(token)
        "bearer " <> token -> Ok(token)
        _ -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

// ============================================================================
// Context Pipeline
// ============================================================================

/// Create a new empty context pipeline
pub fn new_pipeline() -> ContextPipeline {
  ContextPipeline(builders: [])
}

/// Add a context builder to the pipeline
/// Builders execute in the order they are added
pub fn add_builder(
  pipeline: ContextPipeline,
  builder: ContextBuilder,
) -> ContextPipeline {
  ContextPipeline(builders: list.append(pipeline.builders, [builder]))
}

/// Build the execution context by running all builders in sequence
/// Each builder receives the output of the previous builder
pub fn build_context(
  pipeline: ContextPipeline,
  request: RequestInfo,
  initial: Dict(String, Dynamic),
) -> Result(Dict(String, Dynamic), String) {
  list.fold(pipeline.builders, Ok(initial), fn(acc, builder) {
    case acc {
      Ok(ctx) -> builder(request, ctx)
      Error(e) -> Error(e)
    }
  })
}

/// Try to build context, returning initial context on any error
pub fn try_build_context(
  pipeline: ContextPipeline,
  request: RequestInfo,
  initial: Dict(String, Dynamic),
) -> Dict(String, Dynamic) {
  build_context(pipeline, request, initial)
  |> result.unwrap(initial)
}

/// Convert context dict to Dynamic for use with ExecutionContext
pub fn to_dynamic(ctx: Dict(String, Dynamic)) -> Dynamic {
  types.to_dynamic(ctx)
}

// ============================================================================
// Common Context Builders
// ============================================================================

/// Create a builder that adds a value to the context dict
pub fn add_to_context(
  key: String,
  extractor: fn(RequestInfo) -> Result(Dynamic, String),
) -> ContextBuilder {
  fn(request: RequestInfo, ctx: Dict(String, Dynamic)) {
    case extractor(request) {
      Ok(value) -> Ok(dict.insert(ctx, key, value))
      Error(e) -> Error(e)
    }
  }
}

/// Create a builder that adds a value to context, with a fallback on error
pub fn add_to_context_or(
  key: String,
  extractor: fn(RequestInfo) -> Result(Dynamic, String),
  default: Dynamic,
) -> ContextBuilder {
  fn(request: RequestInfo, ctx: Dict(String, Dynamic)) {
    let value = case extractor(request) {
      Ok(v) -> v
      Error(_) -> default
    }
    Ok(dict.insert(ctx, key, value))
  }
}

/// Create a builder that validates a condition or fails
pub fn require(
  validator: fn(RequestInfo, Dict(String, Dynamic)) -> Result(Nil, String),
) -> ContextBuilder {
  fn(request: RequestInfo, ctx: Dict(String, Dynamic)) {
    case validator(request, ctx) {
      Ok(_) -> Ok(ctx)
      Error(e) -> Error(e)
    }
  }
}

/// Create a builder that transforms the entire context
pub fn transform(
  transformer: fn(RequestInfo, Dict(String, Dynamic)) ->
    Result(Dict(String, Dynamic), String),
) -> ContextBuilder {
  transformer
}

// ============================================================================
// Pre-built Context Builders
// ============================================================================

/// Builder that extracts the bearer token and adds it to context as "token"
pub fn bearer_token_builder() -> ContextBuilder {
  add_to_context_or(
    "token",
    fn(request) {
      get_bearer_token(request)
      |> result.map(types.to_dynamic)
      |> result.map_error(fn(_) { "No bearer token found" })
    },
    types.to_dynamic(Nil),
  )
}

/// Builder that requires a bearer token to be present
pub fn require_bearer_token() -> ContextBuilder {
  require(fn(request, _ctx) {
    case get_bearer_token(request) {
      Ok(_) -> Ok(Nil)
      Error(_) -> Error("Authorization required: Bearer token missing")
    }
  })
}

/// Builder that adds request metadata to context
pub fn request_metadata_builder() -> ContextBuilder {
  fn(request: RequestInfo, ctx: Dict(String, Dynamic)) {
    let metadata =
      dict.from_list([
        #("method", types.to_dynamic(request.method)),
        #("path", types.to_dynamic(request.path)),
      ])
    Ok(dict.insert(ctx, "request", types.to_dynamic(metadata)))
  }
}

/// Builder that adds all headers to context under "headers" key
pub fn headers_builder() -> ContextBuilder {
  fn(request: RequestInfo, ctx: Dict(String, Dynamic)) {
    let headers_dynamic =
      dict.map_values(request.headers, fn(_, v) { types.to_dynamic(v) })
    Ok(dict.insert(ctx, "headers", types.to_dynamic(headers_dynamic)))
  }
}
