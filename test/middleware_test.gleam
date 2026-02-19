// Tests for middleware wiring, directive parsing, directive validation,
// and variable pre-execution type checking.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/ast
import mochi/executor
import mochi/middleware
import mochi/parser
import mochi/query
import mochi/schema
import mochi/types
import mochi/validation

// ============================================================================
// Test Schema Helpers
// ============================================================================

pub type User {
  User(id: String, name: String, age: Int)
}

fn decode_user(_dyn: Dynamic) -> Result(User, String) {
  Ok(User("1", "Alice", 30))
}

fn build_test_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.int("age", fn(u: User) { u.age })
    |> types.build(decode_user)

  let user_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_info) { Ok(types.to_dynamic(User("1", "Alice", 30))) },
      fn(u) { types.to_dynamic(u) },
    )

  let user_by_id_query =
    query.query_with_args(
      "userById",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.Named("User"),
      fn(_) { Ok("1") },
      fn(_, _ctx) { Ok(User("1", "Alice", 30)) },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_query(user_by_id_query)
  |> query.add_type(user_type)
  |> query.build
}

// ============================================================================
// Directive Parsing Tests
// ============================================================================

pub fn parse_directive_on_operation_test() {
  // Verify that a directive on an operation is parsed into the AST
  let query_str = "query GetUser @deprecated { user { id } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      case doc.definitions {
        [ast.OperationDefinition(op)] -> {
          let directives = case op {
            ast.Operation(directives: d, ..) -> d
            ast.ShorthandQuery(_) -> []
          }
          should.not_equal(directives, [])
        }
        _ -> panic as "Expected one operation"
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn parse_directive_on_field_test() {
  let query_str = "{ user { id name @skip(if: true) } }"
  case parser.parse(query_str) {
    Ok(doc) -> should.equal(list.length(doc.definitions), 1)
    Error(_) -> panic as "Parse failed"
  }
}

pub fn parse_directive_on_fragment_definition_test() {
  let query_str =
    "fragment UserFields on User @deprecated { id name } query { user { ...UserFields } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let frag =
        list.find(doc.definitions, fn(d) {
          case d {
            ast.FragmentDefinition(_) -> True
            _ -> False
          }
        })
      case frag {
        Ok(ast.FragmentDefinition(f)) -> should.not_equal(f.directives, [])
        _ -> panic as "Expected fragment with directive"
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn parse_directive_on_fragment_spread_test() {
  let query_str =
    "fragment UserFields on User { id } query { user { ...UserFields @skip(if: false) } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let op =
        list.find(doc.definitions, fn(d) {
          case d {
            ast.OperationDefinition(_) -> True
            _ -> False
          }
        })
      case op {
        Ok(ast.OperationDefinition(ast.Operation(selection_set: ss, ..))) -> {
          case ss.selections {
            [ast.FieldSelection(user_field)] -> {
              case user_field.selection_set {
                Some(inner_ss) -> {
                  case inner_ss.selections {
                    [ast.FragmentSpread(spread)] ->
                      should.not_equal(spread.directives, [])
                    _ -> panic as "Expected fragment spread"
                  }
                }
                None -> panic as "Expected selection set on user field"
              }
            }
            _ -> panic as "Expected user field"
          }
        }
        _ -> panic as "Expected named query operation"
      }
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn parse_directive_on_inline_fragment_test() {
  let query_str = "{ user { ... on User @skip(if: false) { id } } }"
  case parser.parse(query_str) {
    Ok(doc) -> should.equal(list.length(doc.definitions), 1)
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Directive Validation Tests
// ============================================================================

pub fn unknown_directive_is_rejected_test() {
  let test_schema = build_test_schema()
  let query_str = "{ user { id name @nonexistent } }"
  case parser.parse(query_str) {
    Ok(doc) ->
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Should have failed with UnknownDirective"
        Error(errors) -> {
          let has_unknown =
            list.any(errors, fn(e) {
              case e {
                validation.UnknownDirective("nonexistent") -> True
                _ -> False
              }
            })
          should.be_true(has_unknown)
        }
      }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn deprecated_directive_not_allowed_on_field_test() {
  // @deprecated is a type-system directive (FIELD_DEFINITION, ENUM_VALUE),
  // not an executable directive (FIELD). It should be rejected at the FIELD location.
  let test_schema = build_test_schema()
  let query_str = "{ user { id name @deprecated } }"
  case parser.parse(query_str) {
    Ok(doc) ->
      case validation.validate(doc, test_schema) {
        Ok(_) -> panic as "Should have failed with DirectiveNotAllowed"
        Error(errors) -> {
          let has_not_allowed =
            list.any(errors, fn(e) {
              case e {
                validation.DirectiveNotAllowed("deprecated", _) -> True
                _ -> False
              }
            })
          should.be_true(has_not_allowed)
        }
      }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn skip_directive_on_field_is_valid_test() {
  let test_schema = build_test_schema()
  let query_str = "{ user { id name @skip(if: true) } }"
  case parser.parse(query_str) {
    Ok(doc) ->
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Should be valid but got errors: " <> msg }
        }
      }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn include_directive_on_fragment_spread_is_valid_test() {
  let test_schema = build_test_schema()
  let query_str =
    "fragment UserFields on User { id } query { user { ...UserFields @include(if: true) } }"
  case parser.parse(query_str) {
    Ok(doc) ->
      case validation.validate(doc, test_schema) {
        Ok(_) -> Nil
        Error(errors) -> {
          let msg = validation.format_errors(errors)
          panic as { "Should be valid but got errors: " <> msg }
        }
      }
    Error(_) -> panic as "Parse failed"
  }
}

// ============================================================================
// Variable Pre-Execution Type Checking Tests
// ============================================================================

pub fn missing_required_variable_causes_error_test() {
  let test_schema = build_test_schema()
  // $id is NonNull ID; pass no variables → should get an error
  let query_str = "query GetUser($id: ID!) { userById(id: $id) { name } }"
  let result =
    executor.execute_query_with_variables(test_schema, query_str, dict.new())
  should.not_equal(result.errors, [])
}

pub fn missing_optional_variable_is_ok_test() {
  let test_schema = build_test_schema()
  // $name is nullable String, not passed → should be fine
  let query_str = "query GetUser($name: String) { user { id } }"
  let result =
    executor.execute_query_with_variables(test_schema, query_str, dict.new())
  should.equal(result.errors, [])
}

pub fn provided_variable_of_correct_type_is_ok_test() {
  let test_schema = build_test_schema()
  let query_str = "query GetUser($id: ID!) { userById(id: $id) { name } }"
  let vars = dict.from_list([#("id", types.to_dynamic("1"))])
  let result =
    executor.execute_query_with_variables(test_schema, query_str, vars)
  should.equal(result.errors, [])
}

pub fn provided_bool_for_string_variable_causes_error_test() {
  let test_schema = build_test_schema()
  // $name is String; passing a Bool should cause a type mismatch error
  let query_str = "query GetUser($name: String!) { user { id } }"
  let vars = dict.from_list([#("name", types.to_dynamic(True))])
  let result =
    executor.execute_query_with_variables(test_schema, query_str, vars)
  // Should produce a validation error about type mismatch
  should.not_equal(result.errors, [])
}

// ============================================================================
// Middleware Wiring Tests
// ============================================================================

pub fn middleware_pipeline_runs_test() {
  // Use a transform middleware as proof that middleware runs
  let test_schema = build_test_schema()

  let pipeline =
    middleware.new_pipeline()
    |> middleware.add_middleware(
      middleware.transform_middleware(fn(value) {
        // Pass value through unchanged to keep execution correct
        value
      }),
    )

  let ctx =
    schema.execution_context(types.to_dynamic(dict.new()))
    |> schema.with_middleware(middleware.to_executor_fn(pipeline))

  let query_str = "{ user { name } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let result = executor.execute(test_schema, doc, None, ctx, dict.new())
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn middleware_logging_runs_without_error_test() {
  // Verify logging middleware does not break execution
  let test_schema = build_test_schema()

  let pipeline =
    middleware.new_pipeline()
    |> middleware.add_middleware(
      middleware.logging_middleware(fn(_msg) { Nil }),
    )

  let ctx =
    schema.execution_context(types.to_dynamic(dict.new()))
    |> schema.with_middleware(middleware.to_executor_fn(pipeline))

  let query_str = "{ user { id name } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let result = executor.execute(test_schema, doc, None, ctx, dict.new())
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn middleware_to_executor_fn_wraps_pipeline_test() {
  // Verify to_executor_fn produces a callable function
  let pipeline = middleware.new_pipeline()
  let mw_fn = middleware.to_executor_fn(pipeline)

  let field_def = schema.field_def("name", schema.Named("String"))

  let info =
    schema.ResolverInfo(
      parent: None,
      arguments: dict.new(),
      context: schema.execution_context(types.to_dynamic(Nil)),
      info: types.to_dynamic(Nil),
    )

  let result =
    mw_fn("Query", field_def, info, fn(_) { Ok(types.to_dynamic("hello")) })

  case result {
    Ok(_) -> Nil
    Error(e) -> panic as { "Middleware fn failed: " <> e }
  }
}

pub fn middleware_with_field_filter_test() {
  // Verify that NamedFields filter doesn't break execution
  let test_schema = build_test_schema()

  let mw =
    middleware.middleware("passthrough", fn(resolution, next) {
      next(resolution)
    })
    |> middleware.with_filter(middleware.NamedFields(["name"]))

  let pipeline =
    middleware.new_pipeline()
    |> middleware.add_middleware(mw)

  let ctx =
    schema.execution_context(types.to_dynamic(dict.new()))
    |> schema.with_middleware(middleware.to_executor_fn(pipeline))

  let query_str = "{ user { id name age } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let result = executor.execute(test_schema, doc, None, ctx, dict.new())
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}

pub fn middleware_no_pipeline_executes_normally_test() {
  // When no middleware is configured, execution proceeds normally
  let test_schema = build_test_schema()

  let ctx = schema.execution_context(types.to_dynamic(dict.new()))

  let query_str = "{ user { id name age } }"
  case parser.parse(query_str) {
    Ok(doc) -> {
      let result = executor.execute(test_schema, doc, None, ctx, dict.new())
      should.be_true(option.is_some(result.data))
      should.equal(result.errors, [])
    }
    Error(_) -> panic as "Parse failed"
  }
}
