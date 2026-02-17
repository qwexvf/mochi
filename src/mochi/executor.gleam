// mochi/executor.gleam
// GraphQL query execution engine with functional patterns

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/ast
import mochi/input_coercion
import mochi/parser
import mochi/schema
import mochi/types

// ============================================================================
// Types
// ============================================================================

pub type ExecutionResult {
  ExecutionResult(data: Option(Dynamic), errors: List(ExecutionError))
}

pub type ExecutionError {
  ValidationError(message: String, path: List(String))
  ResolverError(message: String, path: List(String))
  TypeError(message: String, path: List(String))
  NullValueError(message: String, path: List(String))
}

pub type QueryExecutionContext {
  QueryExecutionContext(
    schema: schema.Schema,
    root_value: Option(Dynamic),
    execution_context: schema.ExecutionContext,
    variable_values: Dict(String, Dynamic),
    fragments: Dict(String, ast.Fragment),
  )
}

pub type FieldContext {
  FieldContext(
    parent_value: Option(Dynamic),
    field_name: String,
    field_args: Dict(String, Dynamic),
    path: List(String),
  )
}

// ============================================================================
// Result Helpers
// ============================================================================

fn ok_result(data: Dynamic) -> ExecutionResult {
  ExecutionResult(data: Some(data), errors: [])
}

fn error_result(error: ExecutionError) -> ExecutionResult {
  ExecutionResult(data: None, errors: [error])
}

fn validation_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(ValidationError(msg, path))
}

fn resolver_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(ResolverError(msg, path))
}

fn type_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(TypeError(msg, path))
}

fn null_value_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(NullValueError(msg, path))
}

// ============================================================================
// Main Execution
// ============================================================================

pub fn execute(
  schema_def: schema.Schema,
  document: ast.Document,
  root_value: Option(Dynamic),
  execution_context: schema.ExecutionContext,
  variable_values: Dict(String, Dynamic),
) -> ExecutionResult {
  execute_with_operation_name(
    schema_def,
    document,
    root_value,
    execution_context,
    variable_values,
    None,
  )
}

/// Execute with a specific operation name selected
/// This is useful for documents containing multiple operations
pub fn execute_with_operation_name(
  schema_def: schema.Schema,
  document: ast.Document,
  root_value: Option(Dynamic),
  execution_context: schema.ExecutionContext,
  variable_values: Dict(String, Dynamic),
  operation_name: Option(String),
) -> ExecutionResult {
  // Extract fragment definitions from the document
  let fragments = extract_fragments(document)

  let context =
    QueryExecutionContext(
      schema: schema_def,
      root_value: root_value,
      execution_context: execution_context,
      variable_values: variable_values,
      fragments: fragments,
    )

  // Find the operation to execute
  case find_operation_by_name(document, operation_name) {
    Ok(operation_def) -> execute_definition(context, operation_def)
    Error(msg) -> validation_error(msg, [])
  }
}

/// Find an operation in the document by name
fn find_operation_by_name(
  document: ast.Document,
  operation_name: Option(String),
) -> Result(ast.Definition, String) {
  let operations =
    document.definitions
    |> list.filter(fn(def) {
      case def {
        ast.OperationDefinition(_) -> True
        ast.FragmentDefinition(_) -> False
      }
    })

  case operation_name, operations {
    // No name specified, must have exactly one operation
    None, [single] -> Ok(single)
    None, [] -> Error("Document contains no operations")
    None, _ ->
      Error("Document contains multiple operations; operation name is required")

    // Name specified, find matching operation
    Some(name), ops -> {
      ops
      |> list.find(fn(op) { get_operation_name_from_def(op) == Some(name) })
      |> result.map_error(fn(_) {
        "Operation '" <> name <> "' not found in document"
      })
    }
  }
}

/// Get the operation name from a definition
fn get_operation_name_from_def(def: ast.Definition) -> Option(String) {
  case def {
    ast.OperationDefinition(ast.Operation(name: name, ..)) -> name
    ast.OperationDefinition(ast.ShorthandQuery(_)) -> None
    ast.FragmentDefinition(_) -> None
  }
}

fn extract_fragments(document: ast.Document) -> Dict(String, ast.Fragment) {
  document.definitions
  |> list.filter_map(fn(def) {
    case def {
      ast.FragmentDefinition(fragment) -> Ok(#(fragment.name, fragment))
      ast.OperationDefinition(_) -> Error(Nil)
    }
  })
  |> dict.from_list
}

fn execute_definition(
  context: QueryExecutionContext,
  definition: ast.Definition,
) -> ExecutionResult {
  case definition {
    ast.OperationDefinition(operation) -> execute_operation(context, operation)
    ast.FragmentDefinition(_) ->
      validation_error("Fragment definitions not yet supported", [])
  }
}

fn execute_operation(
  context: QueryExecutionContext,
  operation: ast.Operation,
) -> ExecutionResult {
  let root_type = get_root_type(context.schema, operation)
  let selection_set = get_selection_set(operation)

  root_type
  |> option.map(fn(obj_type) {
    let field_ctx =
      FieldContext(
        parent_value: context.root_value,
        field_name: "root",
        field_args: dict.new(),
        path: [],
      )
    execute_selection_set(context, selection_set, obj_type, field_ctx)
  })
  |> option.unwrap(
    validation_error(
      "Schema does not define a root type for this operation",
      [],
    ),
  )
}

fn get_root_type(
  schema_def: schema.Schema,
  operation: ast.Operation,
) -> Option(schema.ObjectType) {
  case operation {
    ast.Operation(operation_type: ast.Query, ..) -> schema_def.query
    ast.Operation(operation_type: ast.Mutation, ..) -> schema_def.mutation
    ast.Operation(operation_type: ast.Subscription, ..) ->
      schema_def.subscription
    ast.ShorthandQuery(..) -> schema_def.query
  }
}

fn get_selection_set(operation: ast.Operation) -> ast.SelectionSet {
  case operation {
    ast.Operation(selection_set: ss, ..) -> ss
    ast.ShorthandQuery(selection_set: ss) -> ss
  }
}

// ============================================================================
// Selection Set Execution
// ============================================================================

fn execute_selection_set(
  context: QueryExecutionContext,
  selection_set: ast.SelectionSet,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  // Single-pass collection: gather data, errors, and null-check simultaneously
  let #(data_acc, errors_acc, has_none) =
    list.fold(selection_set.selections, #([], [], False), fn(acc, selection) {
      let #(data_list, errors_list, none_found) = acc
      let result =
        execute_selection(context, selection, object_type, field_context)

      let new_data = case result.data {
        Some(d) -> [d, ..data_list]
        None -> data_list
      }
      let new_none = none_found || option.is_none(result.data)

      #(new_data, list.append(result.errors, errors_list), new_none)
    })

  // Reverse accumulated lists to preserve order
  let data_list = list.reverse(data_acc)
  let errors = list.reverse(errors_acc)

  // If any child propagated null (returned None data), this selection set is null
  case has_none, data_list, errors {
    True, _, _ -> ExecutionResult(data: None, errors: errors)
    False, [], [] -> ok_result(types.to_dynamic(dict.new()))
    False, [], _ -> ExecutionResult(data: None, errors: errors)
    False, _, _ ->
      ExecutionResult(data: Some(merge_results(data_list)), errors: errors)
  }
}

fn execute_selection(
  context: QueryExecutionContext,
  selection: ast.Selection,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  case selection {
    ast.FieldSelection(field) ->
      execute_field(context, field, object_type, field_context)
    ast.FragmentSpread(spread) ->
      execute_fragment_spread(context, spread, object_type, field_context)
    ast.InlineFragment(inline) ->
      execute_inline_fragment(context, inline, object_type, field_context)
  }
}

fn execute_inline_fragment(
  context: QueryExecutionContext,
  inline: ast.InlineFragmentValue,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  case inline.type_condition {
    None ->
      // No type condition - always include
      execute_selection_set(
        context,
        inline.selection_set,
        object_type,
        field_context,
      )
    Some(type_name) ->
      // Check if type applies
      case does_type_apply(context.schema, object_type.name, type_name) {
        True ->
          execute_selection_set(
            context,
            inline.selection_set,
            object_type,
            field_context,
          )
        False ->
          // Type doesn't match - return empty result
          ok_result(types.to_dynamic(dict.new()))
      }
  }
}

fn execute_fragment_spread(
  context: QueryExecutionContext,
  spread: ast.FragmentSpreadValue,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  case dict.get(context.fragments, spread.name) {
    Ok(fragment) -> {
      // Check if the type condition matches
      case
        does_type_apply(
          context.schema,
          object_type.name,
          fragment.type_condition,
        )
      {
        True ->
          execute_selection_set(
            context,
            fragment.selection_set,
            object_type,
            field_context,
          )
        False ->
          // Type doesn't match, return empty result (not an error)
          ok_result(types.to_dynamic(dict.new()))
      }
    }
    Error(_) ->
      validation_error(
        "Fragment '" <> spread.name <> "' is not defined",
        field_context.path,
      )
  }
}

fn does_type_apply(
  schema_def: schema.Schema,
  object_type_name: String,
  type_condition: String,
) -> Bool {
  case object_type_name == type_condition {
    True -> True
    False -> {
      // Check if object_type implements the interface
      case dict.get(schema_def.types, object_type_name) {
        Ok(schema.ObjectTypeDef(obj)) ->
          list.any(obj.interfaces, fn(iface) { iface.name == type_condition })
        _ -> False
      }
    }
  }
}

// ============================================================================
// Field Execution
// ============================================================================

fn execute_field(
  context: QueryExecutionContext,
  field: ast.Field,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  // Check directives first - @skip and @include
  case should_include_field(field.directives, context.variable_values) {
    False -> ok_result(types.to_dynamic(dict.new()))
    // Skip this field
    True -> {
      let response_name = option.unwrap(field.alias, field.name)
      let field_path = list.append(field_context.path, [response_name])

      case field.name {
        "__typename" ->
          ok_result(make_field(
            response_name,
            types.to_dynamic(object_type.name),
          ))
        "__schema" ->
          execute_introspection_schema(context, field, response_name)
        "__type" ->
          execute_introspection_type(context, field, field_path, response_name)
        _ ->
          execute_regular_field(
            context,
            field,
            object_type,
            field_context,
            response_name,
            field_path,
          )
      }
    }
  }
}

// ============================================================================
// Directive Evaluation
// ============================================================================

/// Check if a field should be included based on @skip and @include directives
fn should_include_field(
  directives: List(ast.Directive),
  variables: Dict(String, Dynamic),
) -> Bool {
  let skip_value = get_directive_bool_arg(directives, "skip", "if", variables)
  let include_value =
    get_directive_bool_arg(directives, "include", "if", variables)

  case skip_value, include_value {
    Some(True), _ -> False
    _, Some(False) -> False
    _, _ -> True
  }
}

/// Get a boolean argument value from a specific directive
fn get_directive_bool_arg(
  directives: List(ast.Directive),
  directive_name: String,
  arg_name: String,
  variables: Dict(String, Dynamic),
) -> Option(Bool) {
  directives
  |> list.find(fn(d) { d.name == directive_name })
  |> result.map(fn(directive) {
    directive.arguments
    |> list.find(fn(arg) { arg.name == arg_name })
    |> result.map(fn(arg) { eval_bool_value(arg.value, variables) })
    |> result.unwrap(None)
  })
  |> result.unwrap(None)
}

/// Evaluate a Value to a boolean, handling variables
fn eval_bool_value(
  value: ast.Value,
  variables: Dict(String, Dynamic),
) -> Option(Bool) {
  case value {
    ast.BooleanValue(b) -> Some(b)
    ast.VariableValue(name) -> {
      dict.get(variables, name)
      |> result.map(decode_bool_from_dynamic)
      |> result.unwrap(None)
    }
    _ -> None
  }
}

fn decode_bool_from_dynamic(value: Dynamic) -> Option(Bool) {
  case decode.run(value, decode.bool) {
    Ok(b) -> Some(b)
    Error(_) -> None
  }
}

// ============================================================================
// Custom Directive Execution
// ============================================================================

/// Apply custom directive handlers to a resolved field value.
/// Directives are applied in order (left to right as they appear in the query).
/// Built-in directives (@skip, @include, @deprecated) are skipped as they
/// are handled specially elsewhere.
fn apply_custom_directives(
  schema_def: schema.Schema,
  directives: List(ast.Directive),
  value: Dynamic,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> Result(Dynamic, String) {
  list.fold(directives, Ok(value), fn(acc, directive) {
    case acc {
      Error(msg) -> Error(msg)
      Ok(current_value) ->
        apply_single_directive(
          schema_def,
          directive,
          current_value,
          variables,
          path,
        )
    }
  })
}

/// Apply a single directive handler to a value.
/// Returns the transformed value or an error.
fn apply_single_directive(
  schema_def: schema.Schema,
  directive: ast.Directive,
  value: Dynamic,
  variables: Dict(String, Dynamic),
  _path: List(String),
) -> Result(Dynamic, String) {
  use <- skip_builtin_directive(directive.name, value)
  use directive_def <- with_directive_def(schema_def, directive.name, value)
  use handler <- with_directive_handler(directive_def, value)

  let args = coerce_directive_arguments(directive.arguments, variables)
  handler(args, value)
}

/// Skip built-in directives and call continuation for custom ones
fn skip_builtin_directive(
  name: String,
  value: Dynamic,
  next: fn() -> Result(Dynamic, String),
) -> Result(Dynamic, String) {
  case name {
    "skip" | "include" | "deprecated" -> Ok(value)
    _ -> next()
  }
}

/// Get directive definition or pass through value if not found
fn with_directive_def(
  schema_def: schema.Schema,
  name: String,
  value: Dynamic,
  next: fn(schema.DirectiveDefinition) -> Result(Dynamic, String),
) -> Result(Dynamic, String) {
  case dict.get(schema_def.directives, name) {
    Ok(directive_def) -> next(directive_def)
    Error(_) -> Ok(value)
  }
}

/// Get directive handler or pass through value if none defined
fn with_directive_handler(
  directive_def: schema.DirectiveDefinition,
  value: Dynamic,
  next: fn(schema.DirectiveHandler) -> Result(Dynamic, String),
) -> Result(Dynamic, String) {
  case directive_def.handler {
    Some(handler) -> next(handler)
    None -> Ok(value)
  }
}

/// Coerce directive arguments from AST to Dynamic values
fn coerce_directive_arguments(
  args: List(ast.Argument),
  variables: Dict(String, Dynamic),
) -> Dict(String, Dynamic) {
  list.fold(args, dict.new(), fn(acc, arg) {
    dict.insert(acc, arg.name, coerce_value_simple(arg.value, variables))
  })
}

fn get_list_elements(value: Dynamic) -> Option(List(Dynamic)) {
  case decode.run(value, decode.list(decode.dynamic)) {
    Ok(items) -> Some(items)
    Error(_) -> None
  }
}

fn is_null(value: Dynamic) -> Bool {
  case decode.run(value, decode.optional(decode.dynamic)) {
    Ok(None) -> True
    _ -> False
  }
}

fn execute_regular_field(
  context: QueryExecutionContext,
  field: ast.Field,
  object_type: schema.ObjectType,
  field_context: FieldContext,
  response_name: String,
  field_path: List(String),
) -> ExecutionResult {
  use field_def <- require_field(object_type, field.name, field_path)

  // Coerce and validate arguments with the new input coercion module
  case
    input_coercion.coerce_arguments(
      field.arguments,
      field_def.arguments,
      context.schema,
      context.variable_values,
      field_path,
    )
  {
    Ok(field_args) -> {
      case field_def.resolver {
        Some(resolver) ->
          resolve_field(
            context,
            field,
            field_def,
            field_args,
            response_name,
            field_path,
            resolver,
            field_context.parent_value,
            field.directives,
          )
        None ->
          resolve_from_parent(
            field_context.parent_value,
            field.name,
            response_name,
            field_path,
          )
      }
    }
    Error(coercion_error) ->
      validation_error(input_coercion.format_error(coercion_error), field_path)
  }
}

fn require_field(
  object_type: schema.ObjectType,
  field_name: String,
  path: List(String),
  next: fn(schema.FieldDefinition) -> ExecutionResult,
) -> ExecutionResult {
  case dict.get(object_type.fields, field_name) {
    Ok(field_def) -> next(field_def)
    Error(_) ->
      validation_error(
        "Field '"
          <> field_name
          <> "' not found on type '"
          <> object_type.name
          <> "'",
        path,
      )
  }
}

fn resolve_field(
  context: QueryExecutionContext,
  field: ast.Field,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  response_name: String,
  field_path: List(String),
  resolver: schema.Resolver,
  parent_value: Option(Dynamic),
  directives: List(ast.Directive),
) -> ExecutionResult {
  let resolver_info =
    schema.ResolverInfo(
      parent: parent_value,
      arguments: field_args,
      context: context.execution_context,
      info: types.to_dynamic(dict.new()),
    )

  // Execute resolver, optionally through middleware pipeline
  let resolve_result =
    execute_resolver_with_middleware(
      context.execution_context,
      field_def,
      resolver_info,
      resolver,
      response_name,
    )

  case resolve_result {
    Ok(resolved) ->
      // Apply custom directive handlers after resolving the value
      case
        apply_custom_directives(
          context.schema,
          directives,
          resolved,
          context.variable_values,
          field_path,
        )
      {
        Ok(transformed) ->
          handle_resolved_value(
            context,
            field,
            field_def,
            field_args,
            response_name,
            field_path,
            transformed,
          )
        Error(msg) -> resolver_error(msg, field_path)
      }
    Error(msg) -> resolver_error(msg, field_path)
  }
}

/// Execute a resolver, optionally through the middleware pipeline
/// When middleware is configured, it wraps the resolver call
/// Note: Middleware integration happens at the schema level via ExecutionContext
fn execute_resolver_with_middleware(
  exec_context: schema.ExecutionContext,
  _field_def: schema.FieldDefinition,
  resolver_info: schema.ResolverInfo,
  resolver: schema.Resolver,
  _parent_type_name: String,
) -> Result(Dynamic, String) {
  // The middleware_pipeline field is available in ExecutionContext
  // Actual middleware execution is handled by mochi/middleware.execute_with_middleware
  // which should be called at a higher level when constructing handlers
  //
  // Here we just execute the resolver directly - middleware wrapping happens
  // at the application level using middleware.execute_with_middleware()
  //
  // This design avoids circular dependencies between executor and middleware
  case exec_context.middleware_pipeline {
    Some(_pipeline) ->
      // Middleware is present but we can't call it directly due to module deps
      // The application should wrap using middleware.execute_with_middleware
      resolver(resolver_info)
    None ->
      // No middleware, call resolver directly
      resolver(resolver_info)
  }
}

fn handle_resolved_value(
  context: QueryExecutionContext,
  field: ast.Field,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  response_name: String,
  field_path: List(String),
  resolved: Dynamic,
) -> ExecutionResult {
  use resolved_value <- with_non_null_check(
    resolved,
    field_def.field_type,
    response_name,
    field_path,
  )

  case field.selection_set {
    None -> ok_result(make_field(response_name, resolved_value))
    Some(sub_ss) ->
      handle_selection_set(
        context,
        sub_ss,
        field_def,
        field_args,
        response_name,
        field_path,
        resolved_value,
      )
  }
}

/// Check for null values and handle non-null field constraints
fn with_non_null_check(
  resolved: Dynamic,
  field_type: schema.FieldType,
  response_name: String,
  field_path: List(String),
  next: fn(Dynamic) -> ExecutionResult,
) -> ExecutionResult {
  case is_null(resolved), is_non_null_type(field_type) {
    True, True ->
      null_value_error(
        "Cannot return null for non-null field '" <> response_name <> "'",
        field_path,
      )
    True, False -> ok_result(make_field(response_name, types.to_dynamic(Nil)))
    False, _ -> next(resolved)
  }
}

/// Handle selection set execution for a field with a resolved value
fn handle_selection_set(
  context: QueryExecutionContext,
  sub_ss: ast.SelectionSet,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  response_name: String,
  field_path: List(String),
  resolved: Dynamic,
) -> ExecutionResult {
  case is_list_field_type(field_def.field_type) {
    True ->
      execute_list_sub_selection(
        context,
        sub_ss,
        field_def,
        field_args,
        response_name,
        field_path,
        resolved,
      )
    False -> {
      let sub_result =
        execute_sub_selection(
          context,
          sub_ss,
          field_def,
          field_args,
          field_path,
          resolved,
        )
      handle_sub_selection_result(
        sub_result,
        field_def.field_type,
        response_name,
      )
    }
  }
}

/// Handle null propagation from sub-selection results
fn handle_sub_selection_result(
  sub_result: ExecutionResult,
  field_type: schema.FieldType,
  response_name: String,
) -> ExecutionResult {
  case sub_result.data, is_non_null_type(field_type) {
    Some(_), _ -> wrap_result_in_field(sub_result, response_name)
    None, True -> sub_result
    None, False ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(Nil))),
        errors: sub_result.errors,
      )
  }
}

/// Check if a field type is a list type (unwrapping NonNull)
fn is_list_field_type(field_type: schema.FieldType) -> Bool {
  case field_type {
    schema.List(_) -> True
    schema.NonNull(inner) -> is_list_field_type(inner)
    schema.Named(_) -> False
  }
}

/// Check if a field type is non-null at the outermost level
fn is_non_null_type(field_type: schema.FieldType) -> Bool {
  case field_type {
    schema.NonNull(_) -> True
    _ -> False
  }
}

/// Get the inner type of a list field type
fn get_list_inner_type(field_type: schema.FieldType) -> schema.FieldType {
  case field_type {
    schema.List(inner) -> inner
    schema.NonNull(inner) -> get_list_inner_type(inner)
    _ -> field_type
  }
}

/// Execute selection set on a list of items
fn execute_list_sub_selection(
  context: QueryExecutionContext,
  sub_selection_set: ast.SelectionSet,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  response_name: String,
  field_path: List(String),
  resolved_value: Dynamic,
) -> ExecutionResult {
  case get_list_elements(resolved_value) {
    Some(elements) -> {
      let inner_type = get_list_inner_type(field_def.field_type)
      let inner_field_def =
        schema.FieldDefinition(..field_def, field_type: inner_type)
      let items_are_non_null = is_non_null_type(inner_type)

      let results =
        list.index_map(elements, fn(element, index) {
          execute_list_item(
            context,
            sub_selection_set,
            inner_field_def,
            field_args,
            element,
            index,
            field_path,
            items_are_non_null,
          )
        })

      aggregate_list_results(results, field_def.field_type, response_name)
    }
    None -> {
      let sub_result =
        execute_sub_selection(
          context,
          sub_selection_set,
          field_def,
          field_args,
          field_path,
          resolved_value,
        )
      handle_sub_selection_result(
        sub_result,
        field_def.field_type,
        response_name,
      )
    }
  }
}

/// Execute selection set on a single list item
fn execute_list_item(
  context: QueryExecutionContext,
  selection_set: ast.SelectionSet,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  element: Dynamic,
  index: Int,
  field_path: List(String),
  items_non_null: Bool,
) -> ExecutionResult {
  let item_path = list.append(field_path, [int.to_string(index)])

  case is_null(element), items_non_null {
    True, True ->
      null_value_error(
        "Cannot return null for non-null list item at index "
          <> int.to_string(index),
        item_path,
      )
    True, False -> ok_result(types.to_dynamic(Nil))
    False, _ ->
      execute_sub_selection(
        context,
        selection_set,
        field_def,
        field_args,
        item_path,
        element,
      )
  }
}

/// Aggregate results from list item executions
fn aggregate_list_results(
  results: List(ExecutionResult),
  field_type: schema.FieldType,
  response_name: String,
) -> ExecutionResult {
  // Single-pass: collect data, errors, and check for null errors simultaneously
  let #(data_acc, errors_acc, has_null_error) =
    list.fold(results, #([], [], False), fn(acc, result) {
      let #(data_list, error_list, null_found) = acc
      let new_data = case result.data {
        Some(d) -> [d, ..data_list]
        None -> data_list
      }
      let new_null =
        null_found
        || list.any(result.errors, fn(e) {
          case e {
            NullValueError(_, _) -> True
            _ -> False
          }
        })
      #(new_data, list.append(result.errors, error_list), new_null)
    })

  let errors = list.reverse(errors_acc)
  let data_list = list.reverse(data_acc)

  case has_null_error, errors {
    True, _ -> handle_list_null_error(field_type, response_name, errors)
    False, [] ->
      ok_result(make_field(response_name, types.to_dynamic(data_list)))
    False, _ ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(data_list))),
        errors: errors,
      )
  }
}

/// Handle null errors in list results based on nullability
fn handle_list_null_error(
  field_type: schema.FieldType,
  response_name: String,
  errors: List(ExecutionError),
) -> ExecutionResult {
  case is_non_null_type(field_type) {
    True -> ExecutionResult(data: None, errors: errors)
    False ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(Nil))),
        errors: errors,
      )
  }
}

/// Wrap an execution result's data in a field
fn wrap_result_in_field(
  result: ExecutionResult,
  name: String,
) -> ExecutionResult {
  case result.data {
    Some(data) -> ExecutionResult(..result, data: Some(make_field(name, data)))
    None -> result
  }
}

fn execute_sub_selection(
  context: QueryExecutionContext,
  sub_selection_set: ast.SelectionSet,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  field_path: List(String),
  resolved_value: Dynamic,
) -> ExecutionResult {
  case get_field_type_definition(context.schema, field_def.field_type) {
    Ok(schema.ObjectTypeDef(sub_type)) -> {
      let sub_ctx =
        FieldContext(
          parent_value: Some(resolved_value),
          field_name: field_def.name,
          field_args: field_args,
          path: field_path,
        )
      execute_selection_set(context, sub_selection_set, sub_type, sub_ctx)
    }
    Ok(schema.InterfaceTypeDef(iface)) ->
      execute_abstract_type(
        context,
        sub_selection_set,
        field_args,
        field_path,
        resolved_value,
        iface.resolve_type,
      )
    Ok(schema.UnionTypeDef(union)) ->
      execute_abstract_type(
        context,
        sub_selection_set,
        field_args,
        field_path,
        resolved_value,
        union.resolve_type,
      )
    Ok(_) ->
      type_error("Cannot execute selection set on non-object type", field_path)
    Error(msg) -> type_error(msg, field_path)
  }
}

fn execute_abstract_type(
  context: QueryExecutionContext,
  sub_selection_set: ast.SelectionSet,
  field_args: Dict(String, Dynamic),
  field_path: List(String),
  resolved_value: Dynamic,
  resolve_type: Option(schema.TypeResolver),
) -> ExecutionResult {
  use resolver <- require_type_resolver(resolve_type, field_path)
  use type_name <- require_resolved_type(resolver, resolved_value, field_path)
  use concrete_type <- require_object_type(
    context.schema,
    type_name,
    field_path,
  )

  let sub_ctx =
    FieldContext(
      parent_value: Some(resolved_value),
      field_name: "",
      field_args: field_args,
      path: field_path,
    )
  execute_selection_set(context, sub_selection_set, concrete_type, sub_ctx)
}

/// Require a type resolver function exists
fn require_type_resolver(
  resolve_type: Option(schema.TypeResolver),
  path: List(String),
  next: fn(schema.TypeResolver) -> ExecutionResult,
) -> ExecutionResult {
  case resolve_type {
    Some(resolver) -> next(resolver)
    None -> type_error("Abstract type requires a resolve_type function", path)
  }
}

/// Require the type resolver successfully returns a type name
fn require_resolved_type(
  resolver: schema.TypeResolver,
  value: Dynamic,
  path: List(String),
  next: fn(String) -> ExecutionResult,
) -> ExecutionResult {
  case resolver(value) {
    Ok(type_name) -> next(type_name)
    Error(msg) -> resolver_error("resolve_type failed: " <> msg, path)
  }
}

/// Require the type name resolves to an object type in the schema
fn require_object_type(
  schema_def: schema.Schema,
  type_name: String,
  path: List(String),
  next: fn(schema.ObjectType) -> ExecutionResult,
) -> ExecutionResult {
  case dict.get(schema_def.types, type_name) {
    Ok(schema.ObjectTypeDef(concrete_type)) -> next(concrete_type)
    Ok(_) ->
      type_error("resolve_type returned non-object type: " <> type_name, path)
    Error(_) ->
      type_error("resolve_type returned unknown type: " <> type_name, path)
  }
}

fn resolve_from_parent(
  parent: Option(Dynamic),
  _field_name: String,
  response_name: String,
  field_path: List(String),
) -> ExecutionResult {
  parent
  |> option.map(fn(_p) {
    ok_result(make_field(response_name, types.to_dynamic(Nil)))
  })
  |> option.unwrap(resolver_error("No resolver and no parent value", field_path))
}

fn get_field_type_definition(
  schema_def: schema.Schema,
  field_type: schema.FieldType,
) -> Result(schema.TypeDefinition, String) {
  case field_type {
    schema.Named(name) ->
      dict.get(schema_def.types, name)
      |> result.map_error(fn(_) { "Type '" <> name <> "' not found in schema" })
    schema.NonNull(inner) -> get_field_type_definition(schema_def, inner)
    schema.List(inner) -> get_field_type_definition(schema_def, inner)
  }
}

// ============================================================================
// Value Coercion (unvalidated - used for directives)
// ============================================================================

/// Coerce an AST value to Dynamic without type validation.
/// Used for directive arguments where type checking is less critical.
fn coerce_value_simple(
  value: ast.Value,
  variables: Dict(String, Dynamic),
) -> Dynamic {
  case value {
    ast.IntValue(i) -> types.to_dynamic(i)
    ast.FloatValue(f) -> types.to_dynamic(f)
    ast.StringValue(s) -> types.to_dynamic(s)
    ast.BooleanValue(b) -> types.to_dynamic(b)
    ast.NullValue -> types.to_dynamic(Nil)
    ast.EnumValue(e) -> types.to_dynamic(e)
    ast.VariableValue(name) ->
      dict.get(variables, name) |> result.unwrap(types.to_dynamic(Nil))
    ast.ListValue(values) ->
      types.to_dynamic(list.map(values, coerce_value_simple(_, variables)))
    ast.ObjectValue(fields) ->
      types.to_dynamic(
        list.fold(fields, dict.new(), fn(acc, f) {
          dict.insert(acc, f.name, coerce_value_simple(f.value, variables))
        }),
      )
  }
}

// ============================================================================
// Introspection
// ============================================================================

fn execute_introspection_schema(
  context: QueryExecutionContext,
  field: ast.Field,
  response_name: String,
) -> ExecutionResult {
  ok_result(make_field(
    response_name,
    build_schema_introspection(context.schema, field),
  ))
}

fn execute_introspection_type(
  context: QueryExecutionContext,
  field: ast.Field,
  field_path: List(String),
  response_name: String,
) -> ExecutionResult {
  get_string_argument(field.arguments, "name", context.variable_values)
  |> option.map(fn(name) {
    ok_result(make_field(
      response_name,
      build_type_introspection(context.schema, name),
    ))
  })
  |> option.unwrap(validation_error(
    "Missing required argument 'name' for __type",
    field_path,
  ))
}

fn get_string_argument(
  args: List(ast.Argument),
  name: String,
  variables: Dict(String, Dynamic),
) -> Option(String) {
  list.find(args, fn(arg) { arg.name == name })
  |> result.map(fn(arg) { extract_string_value(arg.value, variables) })
  |> result.unwrap(None)
}

fn extract_string_value(
  value: ast.Value,
  variables: Dict(String, Dynamic),
) -> Option(String) {
  case value {
    ast.StringValue(s) -> Some(s)
    ast.VariableValue(var_name) ->
      dict.get(variables, var_name)
      |> result.map(decode_string_from_dynamic)
      |> result.unwrap(None)
    _ -> None
  }
}

fn decode_string_from_dynamic(value: Dynamic) -> Option(String) {
  case decode.run(value, decode.string) {
    Ok(s) -> Some(s)
    Error(_) -> None
  }
}

// ============================================================================
// Introspection Builders
// ============================================================================

fn build_schema_introspection(
  schema_def: schema.Schema,
  _field: ast.Field,
) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("queryType", build_type_ref(schema_def.query)),
      #("mutationType", build_type_ref(schema_def.mutation)),
      #("subscriptionType", build_type_ref(schema_def.subscription)),
      #(
        "types",
        types.to_dynamic(
          list.map(get_all_type_names(schema_def), build_type_introspection(
            schema_def,
            _,
          )),
        ),
      ),
      #("directives", build_directives_introspection(schema_def)),
    ]),
  )
}

/// Build introspection data for all directives (built-in + custom)
fn build_directives_introspection(schema_def: schema.Schema) -> Dynamic {
  // Built-in directives per GraphQL spec
  let builtin_directives = [
    build_skip_directive_introspection(),
    build_include_directive_introspection(),
    build_deprecated_directive_introspection(),
    build_specified_by_directive_introspection(),
  ]

  // Custom directives from schema
  let custom_directives =
    schema_def.directives
    |> dict.to_list
    |> list.map(fn(kv) {
      let #(_name, directive_def) = kv
      build_directive_introspection(directive_def)
    })

  types.to_dynamic(list.append(builtin_directives, custom_directives))
}

/// Build introspection for a single directive
fn build_directive_introspection(
  directive: schema.DirectiveDefinition,
) -> Dynamic {
  let args =
    directive.arguments
    |> dict.to_list
    |> list.map(fn(kv) {
      let #(arg_name, arg_def) = kv
      types.to_dynamic(
        dict.from_list([
          #("name", types.to_dynamic(arg_name)),
          #(
            "description",
            types.to_dynamic(option.unwrap(arg_def.description, "")),
          ),
          #("type", build_field_type_introspection(arg_def.arg_type)),
          #("defaultValue", types.to_dynamic(Nil)),
        ]),
      )
    })

  let locations =
    directive.locations
    |> list.map(fn(loc) {
      types.to_dynamic(schema.directive_location_to_string(loc))
    })

  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic(directive.name)),
      #(
        "description",
        types.to_dynamic(option.unwrap(directive.description, "")),
      ),
      #("locations", types.to_dynamic(locations)),
      #("args", types.to_dynamic(args)),
      #("isRepeatable", types.to_dynamic(directive.is_repeatable)),
    ]),
  )
}

/// @skip directive - conditionally skip a field
fn build_skip_directive_introspection() -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic("skip")),
      #(
        "description",
        types.to_dynamic(
          "Directs the executor to skip this field or fragment when the `if` argument is true.",
        ),
      ),
      #(
        "locations",
        types.to_dynamic([
          types.to_dynamic("FIELD"),
          types.to_dynamic("FRAGMENT_SPREAD"),
          types.to_dynamic("INLINE_FRAGMENT"),
        ]),
      ),
      #(
        "args",
        types.to_dynamic([
          types.to_dynamic(
            dict.from_list([
              #("name", types.to_dynamic("if")),
              #("description", types.to_dynamic("Skipped when true.")),
              #(
                "type",
                build_field_type_introspection(
                  schema.NonNull(schema.Named("Boolean")),
                ),
              ),
              #("defaultValue", types.to_dynamic(Nil)),
            ]),
          ),
        ]),
      ),
      #("isRepeatable", types.to_dynamic(False)),
    ]),
  )
}

/// @include directive - conditionally include a field
fn build_include_directive_introspection() -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic("include")),
      #(
        "description",
        types.to_dynamic(
          "Directs the executor to include this field or fragment only when the `if` argument is true.",
        ),
      ),
      #(
        "locations",
        types.to_dynamic([
          types.to_dynamic("FIELD"),
          types.to_dynamic("FRAGMENT_SPREAD"),
          types.to_dynamic("INLINE_FRAGMENT"),
        ]),
      ),
      #(
        "args",
        types.to_dynamic([
          types.to_dynamic(
            dict.from_list([
              #("name", types.to_dynamic("if")),
              #("description", types.to_dynamic("Included when true.")),
              #(
                "type",
                build_field_type_introspection(
                  schema.NonNull(schema.Named("Boolean")),
                ),
              ),
              #("defaultValue", types.to_dynamic(Nil)),
            ]),
          ),
        ]),
      ),
      #("isRepeatable", types.to_dynamic(False)),
    ]),
  )
}

/// @deprecated directive - marks a field or enum value as deprecated
fn build_deprecated_directive_introspection() -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic("deprecated")),
      #(
        "description",
        types.to_dynamic(
          "Marks an element of a GraphQL schema as no longer supported.",
        ),
      ),
      #(
        "locations",
        types.to_dynamic([
          types.to_dynamic("FIELD_DEFINITION"),
          types.to_dynamic("ARGUMENT_DEFINITION"),
          types.to_dynamic("INPUT_FIELD_DEFINITION"),
          types.to_dynamic("ENUM_VALUE"),
        ]),
      ),
      #(
        "args",
        types.to_dynamic([
          types.to_dynamic(
            dict.from_list([
              #("name", types.to_dynamic("reason")),
              #(
                "description",
                types.to_dynamic(
                  "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).",
                ),
              ),
              #("type", build_field_type_introspection(schema.Named("String"))),
              #("defaultValue", types.to_dynamic("\"No longer supported\"")),
            ]),
          ),
        ]),
      ),
      #("isRepeatable", types.to_dynamic(False)),
    ]),
  )
}

/// @specifiedBy directive - provides a URL for a custom scalar specification
fn build_specified_by_directive_introspection() -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic("specifiedBy")),
      #(
        "description",
        types.to_dynamic(
          "Exposes a URL that specifies the behavior of this scalar.",
        ),
      ),
      #("locations", types.to_dynamic([types.to_dynamic("SCALAR")])),
      #(
        "args",
        types.to_dynamic([
          types.to_dynamic(
            dict.from_list([
              #("name", types.to_dynamic("url")),
              #(
                "description",
                types.to_dynamic(
                  "The URL that specifies the behavior of this scalar.",
                ),
              ),
              #(
                "type",
                build_field_type_introspection(
                  schema.NonNull(schema.Named("String")),
                ),
              ),
              #("defaultValue", types.to_dynamic(Nil)),
            ]),
          ),
        ]),
      ),
      #("isRepeatable", types.to_dynamic(False)),
    ]),
  )
}

fn build_type_ref(obj: Option(schema.ObjectType)) -> Dynamic {
  obj
  |> option.map(fn(o) {
    types.to_dynamic(dict.from_list([#("name", types.to_dynamic(o.name))]))
  })
  |> option.unwrap(types.to_dynamic(Nil))
}

fn get_all_type_names(schema_def: schema.Schema) -> List(String) {
  let builtin = ["String", "Int", "Float", "Boolean", "ID"]
  let introspection = [
    "__Schema",
    "__Type",
    "__Field",
    "__InputValue",
    "__EnumValue",
    "__Directive",
    "__DirectiveLocation",
    "__TypeKind",
  ]
  let user_types = dict.keys(schema_def.types)
  let root_types =
    [schema_def.query, schema_def.mutation, schema_def.subscription]
    |> list.filter_map(fn(opt) {
      option.map(opt, fn(o) { o.name }) |> option.to_result(Nil)
    })

  list.flatten([builtin, user_types, root_types, introspection]) |> list.unique
}

fn build_type_introspection(schema_def: schema.Schema, name: String) -> Dynamic {
  case name {
    "String" | "Int" | "Float" | "Boolean" | "ID" ->
      build_scalar_introspection(name)
    _ -> lookup_type_introspection(schema_def, name)
  }
}

fn build_scalar_introspection(name: String) -> Dynamic {
  make_type_object(
    "SCALAR",
    name,
    get_scalar_description(name),
    None,
    None,
    None,
    None,
  )
}

fn get_scalar_description(name: String) -> String {
  case name {
    "String" -> "The String scalar type represents textual data"
    "Int" ->
      "The Int scalar type represents non-fractional signed whole numeric values"
    "Float" ->
      "The Float scalar type represents signed double-precision fractional values"
    "Boolean" -> "The Boolean scalar type represents true or false"
    "ID" -> "The ID scalar type represents a unique identifier"
    _ -> ""
  }
}

fn lookup_type_introspection(schema_def: schema.Schema, name: String) -> Dynamic {
  case dict.get(schema_def.types, name) {
    Ok(type_def) -> build_type_def_introspection(type_def)
    Error(_) -> lookup_root_type(schema_def, name)
  }
}

fn lookup_root_type(schema_def: schema.Schema, name: String) -> Dynamic {
  [schema_def.query, schema_def.mutation, schema_def.subscription]
  |> list.find(fn(opt) {
    option.map(opt, fn(o) { o.name == name }) |> option.unwrap(False)
  })
  |> result.map(fn(opt) {
    option.map(opt, build_object_introspection)
    |> option.unwrap(types.to_dynamic(Nil))
  })
  |> result.unwrap(build_meta_type_introspection(name))
}

fn build_type_def_introspection(type_def: schema.TypeDefinition) -> Dynamic {
  case type_def {
    schema.ObjectTypeDef(obj) -> build_object_introspection(obj)
    schema.ScalarTypeDef(scalar) ->
      make_type_object(
        "SCALAR",
        scalar.name,
        option.unwrap(scalar.description, ""),
        None,
        None,
        None,
        None,
      )
    schema.EnumTypeDef(enum) -> build_enum_introspection(enum)
    schema.InterfaceTypeDef(iface) -> build_interface_introspection(iface)
    schema.UnionTypeDef(union) -> build_union_introspection(union)
    schema.InputObjectTypeDef(input) -> build_input_introspection(input)
  }
}

fn build_object_introspection(obj: schema.ObjectType) -> Dynamic {
  let fields = build_fields_introspection(obj.fields)
  let interfaces =
    list.map(obj.interfaces, fn(i) {
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("INTERFACE")),
          #("name", types.to_dynamic(i.name)),
        ]),
      )
    })
  make_type_object(
    "OBJECT",
    obj.name,
    option.unwrap(obj.description, ""),
    Some(fields),
    Some(interfaces),
    None,
    None,
  )
}

fn build_enum_introspection(enum: schema.EnumType) -> Dynamic {
  let values =
    dict.to_list(enum.values)
    |> list.map(fn(kv) {
      let #(name, def) = kv
      types.to_dynamic(
        dict.from_list([
          #("name", types.to_dynamic(name)),
          #("description", types.to_dynamic(option.unwrap(def.description, ""))),
          #("isDeprecated", types.to_dynamic(def.is_deprecated)),
          #("deprecationReason", case def.deprecation_reason {
            option.Some(reason) -> types.to_dynamic(reason)
            option.None -> types.to_dynamic(Nil)
          }),
        ]),
      )
    })
  make_type_object(
    "ENUM",
    enum.name,
    option.unwrap(enum.description, ""),
    None,
    None,
    Some(values),
    None,
  )
}

fn build_interface_introspection(iface: schema.InterfaceType) -> Dynamic {
  let fields = build_fields_introspection(iface.fields)
  make_type_object(
    "INTERFACE",
    iface.name,
    option.unwrap(iface.description, ""),
    Some(fields),
    None,
    None,
    None,
  )
}

fn build_union_introspection(union: schema.UnionType) -> Dynamic {
  let possible =
    list.map(union.types, fn(t) {
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("OBJECT")),
          #("name", types.to_dynamic(t.name)),
        ]),
      )
    })
  types.to_dynamic(
    dict.from_list([
      #("kind", types.to_dynamic("UNION")),
      #("name", types.to_dynamic(union.name)),
      #("description", types.to_dynamic(option.unwrap(union.description, ""))),
      #("possibleTypes", types.to_dynamic(possible)),
      #("fields", types.to_dynamic(Nil)),
      #("interfaces", types.to_dynamic(Nil)),
      #("enumValues", types.to_dynamic(Nil)),
      #("inputFields", types.to_dynamic(Nil)),
      #("ofType", types.to_dynamic(Nil)),
    ]),
  )
}

fn build_input_introspection(input: schema.InputObjectType) -> Dynamic {
  let fields =
    dict.to_list(input.fields)
    |> list.map(fn(kv) {
      let #(name, def) = kv
      types.to_dynamic(
        dict.from_list([
          #("name", types.to_dynamic(name)),
          #("description", types.to_dynamic(option.unwrap(def.description, ""))),
          #("type", build_field_type_introspection(def.field_type)),
          #("defaultValue", types.to_dynamic(Nil)),
        ]),
      )
    })
  make_type_object(
    "INPUT_OBJECT",
    input.name,
    option.unwrap(input.description, ""),
    None,
    None,
    None,
    Some(fields),
  )
}

fn build_meta_type_introspection(name: String) -> Dynamic {
  case name {
    "__Schema"
    | "__Type"
    | "__Field"
    | "__InputValue"
    | "__EnumValue"
    | "__Directive" ->
      make_type_object(
        "OBJECT",
        name,
        "Introspection type",
        Some([]),
        Some([]),
        None,
        None,
      )
    "__TypeKind" | "__DirectiveLocation" ->
      make_type_object(
        "ENUM",
        name,
        "Introspection enum",
        None,
        None,
        Some([]),
        None,
      )
    _ -> types.to_dynamic(Nil)
  }
}

fn make_type_object(
  kind: String,
  name: String,
  description: String,
  fields: Option(List(Dynamic)),
  interfaces: Option(List(Dynamic)),
  enum_values: Option(List(Dynamic)),
  input_fields: Option(List(Dynamic)),
) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("kind", types.to_dynamic(kind)),
      #("name", types.to_dynamic(name)),
      #("description", types.to_dynamic(description)),
      #(
        "fields",
        option.map(fields, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "interfaces",
        option.map(interfaces, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #("possibleTypes", types.to_dynamic(Nil)),
      #(
        "enumValues",
        option.map(enum_values, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "inputFields",
        option.map(input_fields, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #("ofType", types.to_dynamic(Nil)),
    ]),
  )
}

fn build_fields_introspection(
  fields: Dict(String, schema.FieldDefinition),
) -> List(Dynamic) {
  dict.to_list(fields)
  |> list.map(fn(kv) {
    let #(name, def) = kv
    let args =
      dict.to_list(def.arguments)
      |> list.map(fn(arg_kv) {
        let #(arg_name, arg_def) = arg_kv
        types.to_dynamic(
          dict.from_list([
            #("name", types.to_dynamic(arg_name)),
            #(
              "description",
              types.to_dynamic(option.unwrap(arg_def.description, "")),
            ),
            #("type", build_field_type_introspection(arg_def.arg_type)),
            #("defaultValue", types.to_dynamic(Nil)),
          ]),
        )
      })
    types.to_dynamic(
      dict.from_list([
        #("name", types.to_dynamic(name)),
        #("description", types.to_dynamic(option.unwrap(def.description, ""))),
        #("args", types.to_dynamic(args)),
        #("type", build_field_type_introspection(def.field_type)),
        #("isDeprecated", types.to_dynamic(def.is_deprecated)),
        #("deprecationReason", case def.deprecation_reason {
          option.Some(reason) -> types.to_dynamic(reason)
          option.None -> types.to_dynamic(Nil)
        }),
      ]),
    )
  })
}

fn build_field_type_introspection(field_type: schema.FieldType) -> Dynamic {
  case field_type {
    schema.NonNull(inner) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("NON_NULL")),
          #("name", types.to_dynamic(Nil)),
          #("ofType", build_field_type_introspection(inner)),
        ]),
      )
    schema.List(inner) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("LIST")),
          #("name", types.to_dynamic(Nil)),
          #("ofType", build_field_type_introspection(inner)),
        ]),
      )
    schema.Named(name) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic(get_type_kind(name))),
          #("name", types.to_dynamic(name)),
          #("ofType", types.to_dynamic(Nil)),
        ]),
      )
  }
}

fn get_type_kind(name: String) -> String {
  case name {
    "String" | "Int" | "Float" | "Boolean" | "ID" -> "SCALAR"
    _ -> "OBJECT"
  }
}

// ============================================================================
// Helpers
// ============================================================================

fn make_field(name: String, value: Dynamic) -> Dynamic {
  types.to_dynamic(dict.from_list([#(name, value)]))
}

fn merge_results(results: List(Dynamic)) -> Dynamic {
  case results {
    [] -> types.to_dynamic(dict.new())
    _ -> {
      // Each result is a dict with a single field - merge them all
      let merged =
        list.fold(results, dict.new(), fn(acc, result) {
          case decode.run(result, decode.dict(decode.string, decode.dynamic)) {
            Ok(d) -> dict.merge(acc, d)
            Error(_) -> acc
          }
        })
      types.to_dynamic(merged)
    }
  }
}

// ============================================================================
// Public API
// ============================================================================

pub fn execute_query(
  schema_def: schema.Schema,
  query: String,
) -> ExecutionResult {
  execute_query_with_variables(schema_def, query, dict.new())
}

pub fn execute_query_with_variables(
  schema_def: schema.Schema,
  query: String,
  variables: Dict(String, Dynamic),
) -> ExecutionResult {
  parser.parse(query)
  |> result.map(fn(document) {
    let ctx = schema.execution_context(types.to_dynamic(dict.new()))
    execute(schema_def, document, None, ctx, variables)
  })
  |> result.unwrap(validation_error("Failed to parse query", []))
}

// ============================================================================
// Debug Execution
// ============================================================================

pub type DebugContext {
  DebugContext(enabled: Bool, indent_level: Int, step_counter: Int)
}

pub fn execute_query_debug(
  schema_def: schema.Schema,
  query: String,
) -> ExecutionResult {
  execute_query_debug_with_variables(schema_def, query, dict.new())
}

pub fn execute_query_debug_with_variables(
  schema_def: schema.Schema,
  query: String,
  variables: Dict(String, Dynamic),
) -> ExecutionResult {
  let debug = DebugContext(enabled: True, indent_level: 0, step_counter: 1)

  log_step(debug, "Starting GraphQL Query Execution")
  log_info(debug, "Query: " <> query)
  log_step(debug, "Parsing Query")

  case parser.parse(query) {
    Ok(document) -> {
      log_success(debug, "Parse successful")
      log_info(
        debug,
        "Definitions: " <> int.to_string(list.length(document.definitions)),
      )
      log_step(debug, "Executing Query")

      let result = execute_query_with_variables(schema_def, query, variables)

      case result.data {
        Some(_) -> log_success(debug, "Query executed successfully!")
        None ->
          log_error(
            debug,
            "Query execution failed: "
              <> int.to_string(list.length(result.errors))
              <> " errors",
          )
      }
      result
    }
    Error(error) -> {
      log_error(debug, "Parse failed: " <> format_parse_error(error))
      validation_error("Parse error: " <> format_parse_error(error), [])
    }
  }
}

fn log_step(debug: DebugContext, msg: String) -> Nil {
  case debug.enabled {
    True -> io.println(string.repeat("  ", debug.indent_level) <> "📋 " <> msg)
    False -> Nil
  }
}

fn log_info(debug: DebugContext, msg: String) -> Nil {
  case debug.enabled {
    True -> io.println(string.repeat("  ", debug.indent_level) <> "🔍 " <> msg)
    False -> Nil
  }
}

fn log_success(debug: DebugContext, msg: String) -> Nil {
  case debug.enabled {
    True -> io.println(string.repeat("  ", debug.indent_level) <> "✅ " <> msg)
    False -> Nil
  }
}

fn log_error(debug: DebugContext, msg: String) -> Nil {
  case debug.enabled {
    True -> io.println(string.repeat("  ", debug.indent_level) <> "❌ " <> msg)
    False -> Nil
  }
}

fn format_parse_error(error: parser.ParseError) -> String {
  case error {
    parser.LexError(_) -> "Lexer error"
    parser.UnexpectedToken(expected, _, _) -> "Expected " <> expected
    parser.UnexpectedEOF(expected) -> "Unexpected EOF, expected " <> expected
  }
}
