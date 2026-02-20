// Tests for mochi/context.gleam - context builder pipeline
import gleam/dict
import gleam/option.{None}
import gleeunit/should
import mochi/context
import mochi/types

// ============================================================================
// RequestInfo Tests
// ============================================================================

pub fn request_info_basic_test() {
  let headers = dict.from_list([#("content-type", "application/json")])
  let info = context.request_info(headers, "POST", "/graphql")
  should.equal(info.method, "POST")
  should.equal(info.path, "/graphql")
}

pub fn get_header_found_test() {
  let headers = dict.from_list([#("authorization", "Bearer token123")])
  let info = context.request_info(headers, "GET", "/")
  let result = context.get_header(info, "authorization")
  should.equal(result, Ok("Bearer token123"))
}

pub fn get_header_case_insensitive_test() {
  let headers = dict.from_list([#("Content-Type", "application/json")])
  let info = context.request_info(headers, "GET", "/")
  // Should find header regardless of case
  let result = context.get_header(info, "content-type")
  should.equal(result, Ok("application/json"))
}

pub fn get_header_not_found_test() {
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.get_header(info, "missing-header")
  should.equal(result, Error(Nil))
}

pub fn get_authorization_test() {
  let headers = dict.from_list([#("authorization", "Bearer mytoken")])
  let info = context.request_info(headers, "POST", "/")
  should.equal(context.get_authorization(info), Ok("Bearer mytoken"))
}

pub fn get_bearer_token_test() {
  let headers = dict.from_list([#("authorization", "Bearer secret-token")])
  let info = context.request_info(headers, "POST", "/")
  should.equal(context.get_bearer_token(info), Ok("secret-token"))
}

pub fn get_bearer_token_lowercase_test() {
  let headers = dict.from_list([#("authorization", "bearer token-lowercase")])
  let info = context.request_info(headers, "POST", "/")
  should.equal(context.get_bearer_token(info), Ok("token-lowercase"))
}

pub fn get_bearer_token_no_auth_test() {
  let info = context.request_info(dict.new(), "GET", "/")
  should.equal(context.get_bearer_token(info), Error(Nil))
}

pub fn get_bearer_token_wrong_scheme_test() {
  let headers = dict.from_list([#("authorization", "Basic abc123")])
  let info = context.request_info(headers, "POST", "/")
  should.equal(context.get_bearer_token(info), Error(Nil))
}

// ============================================================================
// ContextPipeline Tests
// ============================================================================

pub fn new_pipeline_test() {
  let pipeline = context.new_pipeline()
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())
  should.equal(result, Ok(dict.new()))
}

pub fn add_builder_test() {
  let builder = fn(_req: context.RequestInfo, ctx: dict.Dict(String, _)) {
    Ok(dict.insert(ctx, "key", types.to_dynamic("value")))
  }
  let pipeline = context.new_pipeline() |> context.add_builder(builder)
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())

  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "key"))
    Error(_) -> should.fail()
  }
}

pub fn pipeline_runs_in_order_test() {
  let builder1 = fn(_req: context.RequestInfo, ctx: dict.Dict(String, _)) {
    Ok(dict.insert(ctx, "step", types.to_dynamic(1)))
  }
  let builder2 = fn(_req: context.RequestInfo, ctx: dict.Dict(String, _)) {
    Ok(dict.insert(ctx, "step", types.to_dynamic(2)))
  }
  let pipeline =
    context.new_pipeline()
    |> context.add_builder(builder1)
    |> context.add_builder(builder2)
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())

  case result {
    Ok(_ctx) -> Nil
    // Builder 2 overwrites step=1 with step=2
    Error(_) -> should.fail()
  }
}

pub fn pipeline_stops_on_error_test() {
  let failing_builder = fn(
    _req: context.RequestInfo,
    _ctx: dict.Dict(String, _),
  ) {
    Error("Auth failed")
  }
  let should_not_run = fn(_req: context.RequestInfo, ctx: dict.Dict(String, _)) {
    Ok(dict.insert(ctx, "ran", types.to_dynamic(True)))
  }
  let pipeline =
    context.new_pipeline()
    |> context.add_builder(failing_builder)
    |> context.add_builder(should_not_run)
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())

  should.equal(result, Error("Auth failed"))
}

pub fn try_build_context_returns_initial_on_error_test() {
  let failing_builder = fn(
    _req: context.RequestInfo,
    _ctx: dict.Dict(String, _),
  ) {
    Error("Fail")
  }
  let initial = dict.from_list([#("default", types.to_dynamic("value"))])
  let pipeline = context.new_pipeline() |> context.add_builder(failing_builder)
  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.try_build_context(pipeline, info, initial)

  should.equal(result, initial)
}

pub fn to_dynamic_test() {
  let ctx = dict.from_list([#("user_id", types.to_dynamic("123"))])
  let dyn = context.to_dynamic(ctx)
  // Should return a valid Dynamic (just check it doesn't panic)
  should.not_equal(dyn, types.to_dynamic(None))
}

// ============================================================================
// Built-in Context Builders Tests
// ============================================================================

pub fn bearer_token_builder_test() {
  let builder = context.bearer_token_builder()
  let headers = dict.from_list([#("authorization", "Bearer mytoken")])
  let info = context.request_info(headers, "GET", "/")
  let result = builder(info, dict.new())

  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "token"))
    Error(_) -> should.fail()
  }
}

pub fn headers_builder_test() {
  let builder = context.headers_builder()
  let headers =
    dict.from_list([
      #("content-type", "application/json"),
      #("x-request-id", "abc123"),
    ])
  let info = context.request_info(headers, "POST", "/graphql")
  let result = builder(info, dict.new())

  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "headers"))
    Error(_) -> should.fail()
  }
}

// ============================================================================
// require_bearer_token Tests
// ============================================================================

pub fn require_bearer_token_present_test() {
  let builder = context.require_bearer_token()
  let headers = dict.from_list([#("authorization", "Bearer secret123")])
  let info = context.request_info(headers, "POST", "/graphql")
  let result = builder(info, dict.new())
  should.be_ok(result)
}

pub fn require_bearer_token_missing_test() {
  let builder = context.require_bearer_token()
  let info = context.request_info(dict.new(), "POST", "/graphql")
  let result = builder(info, dict.new())
  should.be_error(result)
}

pub fn require_bearer_token_wrong_scheme_test() {
  let builder = context.require_bearer_token()
  let headers = dict.from_list([#("authorization", "Basic dXNlcjpwYXNz")])
  let info = context.request_info(headers, "POST", "/graphql")
  let result = builder(info, dict.new())
  should.be_error(result)
}

// ============================================================================
// add_to_context_or Tests
// ============================================================================

pub fn add_to_context_or_success_test() {
  // Extractor succeeds — uses the extracted value
  let builder =
    context.add_to_context_or(
      "custom",
      fn(info) {
        case context.get_header(info, "x-custom") {
          Ok(v) -> Ok(types.to_dynamic(v))
          Error(_) -> Error("no header")
        }
      },
      types.to_dynamic("default"),
    )
  let headers = dict.from_list([#("x-custom", "my-value")])
  let info = context.request_info(headers, "GET", "/")
  let result = builder(info, dict.new())
  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "custom"))
    Error(_) -> should.fail()
  }
}

pub fn add_to_context_or_fallback_test() {
  // Extractor fails — uses the default value, no error
  let builder =
    context.add_to_context_or(
      "custom",
      fn(_info) { Error("not found") },
      types.to_dynamic("default"),
    )
  let info = context.request_info(dict.new(), "GET", "/")
  let result = builder(info, dict.new())
  // Should succeed with default, not error
  should.be_ok(result)
  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "custom"))
    Error(_) -> should.fail()
  }
}

// ============================================================================
// request_metadata_builder Tests
// ============================================================================

pub fn request_metadata_builder_test() {
  let builder = context.request_metadata_builder()
  let info = context.request_info(dict.new(), "DELETE", "/api/users/1")
  let result = builder(info, dict.new())
  case result {
    Ok(ctx) -> should.be_true(dict.has_key(ctx, "request"))
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Pipeline composition Tests
// ============================================================================

pub fn pipeline_with_multiple_builders_test() {
  let pipeline =
    context.new_pipeline()
    |> context.add_builder(context.headers_builder())
    |> context.add_builder(context.bearer_token_builder())
    |> context.add_builder(context.request_metadata_builder())

  let headers = dict.from_list([#("authorization", "Bearer tok")])
  let info = context.request_info(headers, "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())
  case result {
    Ok(ctx) -> {
      should.be_true(dict.has_key(ctx, "headers"))
      should.be_true(dict.has_key(ctx, "token"))
      should.be_true(dict.has_key(ctx, "request"))
    }
    Error(_) -> should.fail()
  }
}

pub fn pipeline_require_bearer_blocks_without_token_test() {
  let pipeline =
    context.new_pipeline()
    |> context.add_builder(context.require_bearer_token())
    |> context.add_builder(context.headers_builder())

  let info = context.request_info(dict.new(), "GET", "/")
  let result = context.build_context(pipeline, info, dict.new())
  // require_bearer_token should cause the pipeline to fail
  should.be_error(result)
}
