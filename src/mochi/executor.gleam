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
import mochi/document_cache
import mochi/error as graphql_error
import mochi/input_coercion
import mochi/introspection
import mochi/lexer
import mochi/parser
import mochi/schema
import mochi/telemetry
import mochi/types
import mochi/validation

// ============================================================================
// Types
// ============================================================================

pub type DeferredPatch {
  DeferredPatch(
    path: List(String),
    data: Option(Dynamic),
    errors: List(ExecutionError),
    label: Option(String),
  )
}

pub type ExecutionResult {
  ExecutionResult(
    data: Option(Dynamic),
    errors: List(ExecutionError),
    deferred: List(DeferredPatch),
  )
}

pub type ExecutionError {
  ValidationError(
    message: String,
    path: List(String),
    /// Source location of the field in the query document (line, column)
    location: Option(#(Int, Int)),
  )
  ResolverError(
    message: String,
    path: List(String),
    /// Source location of the field in the query document (line, column)
    location: Option(#(Int, Int)),
  )
  TypeError(
    message: String,
    path: List(String),
    /// Source location of the field in the query document (line, column)
    location: Option(#(Int, Int)),
  )
  NullValueError(
    message: String,
    path: List(String),
    /// Source location of the field in the query document (line, column)
    location: Option(#(Int, Int)),
  )
  RichResolverError(
    graphql_error: graphql_error.GraphQLError,
    path: List(String),
    location: Option(#(Int, Int)),
  )
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
  ExecutionResult(data: Some(data), errors: [], deferred: [])
}

fn error_result(error: ExecutionError) -> ExecutionResult {
  ExecutionResult(data: None, errors: [error], deferred: [])
}

fn validation_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(ValidationError(msg, path, location: None))
}

fn validation_error_at(
  msg: String,
  path: List(String),
  loc: Option(#(Int, Int)),
) -> ExecutionResult {
  error_result(ValidationError(msg, path, location: loc))
}

fn resolver_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(ResolverError(msg, path, location: None))
}

fn resolver_error_at(
  msg: String,
  path: List(String),
  loc: Option(#(Int, Int)),
) -> ExecutionResult {
  error_result(ResolverError(msg, path, location: loc))
}

fn type_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(TypeError(msg, path, location: None))
}

fn null_value_error(msg: String, path: List(String)) -> ExecutionResult {
  error_result(NullValueError(msg, path, location: None))
}

fn null_value_error_at(
  msg: String,
  path: List(String),
  loc: Option(#(Int, Int)),
) -> ExecutionResult {
  error_result(NullValueError(msg, path, location: loc))
}

fn rich_resolver_error_at(
  err: graphql_error.GraphQLError,
  path: List(String),
  loc: Option(#(Int, Int)),
) -> ExecutionResult {
  error_result(RichResolverError(graphql_error: err, path: path, location: loc))
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
  let fragments = extract_fragments(document)

  let context =
    QueryExecutionContext(
      schema: schema_def,
      root_value: root_value,
      execution_context: execution_context,
      variable_values: variable_values,
      fragments: fragments,
    )

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
      panic as "unreachable: fragment definition reached execute_definition"
  }
}

fn execute_operation(
  context: QueryExecutionContext,
  operation: ast.Operation,
) -> ExecutionResult {
  let root_type = get_root_type(context.schema, operation)
  let selection_set = get_selection_set(operation)
  let var_defs = get_variable_definitions(operation)
  let operation_name = case operation {
    ast.Operation(name: name, ..) -> name
    ast.ShorthandQuery(_) -> None
  }

  // Emit operation start telemetry
  emit_telemetry(
    context.execution_context,
    schema.SchemaOperationStart(operation_name),
  )

  // Coerce and validate variable values against their declared types
  let exec_result = case coerce_variable_values(context, var_defs) {
    Error(errors) -> ExecutionResult(data: None, errors: errors, deferred: [])
    Ok(coerced_variables) -> {
      let context =
        QueryExecutionContext(..context, variable_values: coerced_variables)
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
  }

  // Emit operation end telemetry
  emit_telemetry(
    context.execution_context,
    schema.SchemaOperationEnd(
      operation_name,
      list.is_empty(exec_result.errors),
      list.length(exec_result.errors),
    ),
  )

  exec_result
}

fn get_variable_definitions(
  operation: ast.Operation,
) -> List(ast.VariableDefinition) {
  case operation {
    ast.Operation(variable_definitions: defs, ..) -> defs
    ast.ShorthandQuery(_) -> []
  }
}

/// Coerce variable values against their declared types.
/// Returns Error with a list of ExecutionErrors if any variable is invalid.
fn coerce_variable_values(
  context: QueryExecutionContext,
  var_defs: List(ast.VariableDefinition),
) -> Result(Dict(String, Dynamic), List(ExecutionError)) {
  let #(coerced, errors) =
    list.fold(var_defs, #(context.variable_values, []), fn(acc, var_def) {
      let #(values, errs) = acc
      let var_name = var_def.variable
      let declared_type = var_def.type_
      let provided = dict.get(values, var_name)

      case provided, var_def.default_value {
        // Variable provided — type check it. Per GraphQL spec §6.4.1,
        // an explicit null is a valid input for any nullable variable,
        // so skip the named-type check for null values against nullable
        // types (e.g. `$x: String` = null).
        Ok(value), _ ->
          case is_null_value(value), is_non_null_ast_type(declared_type) {
            True, False -> #(dict.insert(values, var_name, value), errs)
            _, _ ->
              case check_variable_type(value, declared_type, context.schema) {
                Ok(coerced_value) -> #(
                  dict.insert(values, var_name, coerced_value),
                  errs,
                )
                Error(msg) -> #(values, [
                  ValidationError(
                    "Variable \"$" <> var_name <> "\": " <> msg,
                    [],
                    None,
                  ),
                  ..errs
                ])
              }
          }

        // Variable missing but has AST default — coerce and use default
        Error(_), Some(default_ast) -> {
          let default_value =
            input_coercion.coerce_argument_value(
              default_ast,
              ast_type_to_schema_type(declared_type),
              context.schema,
              dict.new(),
              ["$" <> var_name],
            )
          case default_value {
            Ok(v) -> #(dict.insert(values, var_name, v), errs)
            Error(_) -> #(values, errs)
          }
        }

        // Variable missing, no default, non-null — error
        // Variable missing, no default, nullable — coerce to null (per spec §6.4.1)
        Error(_), None ->
          case is_non_null_ast_type(declared_type) {
            True -> #(values, [
              ValidationError(
                "Variable \"$"
                  <> var_name
                  <> "\" of required type is not provided",
                [],
                None,
              ),
              ..errs
            ])
            False -> #(
              dict.insert(values, var_name, types.to_dynamic(Nil)),
              errs,
            )
          }
      }
    })

  case errors {
    [] -> Ok(coerced)
    _ -> Error(list.reverse(errors))
  }
}

fn is_non_null_ast_type(t: ast.Type) -> Bool {
  case t {
    ast.NonNullType(_) -> True
    _ -> False
  }
}

/// Convert an AST type to a schema FieldType for input coercion
fn ast_type_to_schema_type(t: ast.Type) -> schema.FieldType {
  case t {
    ast.NamedType(name) -> schema.Named(name)
    ast.ListType(inner) -> schema.List(ast_type_to_schema_type(inner))
    ast.NonNullType(inner) -> schema.NonNull(ast_type_to_schema_type(inner))
  }
}

/// Check that a Dynamic value is compatible with the declared variable type.
/// Returns Ok with the (possibly coerced) value, or Error with a message.
fn check_variable_type(
  value: Dynamic,
  declared_type: ast.Type,
  schema_def: schema.Schema,
) -> Result(Dynamic, String) {
  let schema_type = ast_type_to_schema_type(declared_type)
  check_value_against_schema_type(value, schema_type, schema_def)
}

fn check_value_against_schema_type(
  value: Dynamic,
  field_type: schema.FieldType,
  schema_def: schema.Schema,
) -> Result(Dynamic, String) {
  case field_type {
    schema.NonNull(inner) ->
      case is_null_value(value) {
        True -> Error("Expected non-null value but got null")
        False -> check_value_against_schema_type(value, inner, schema_def)
      }
    schema.List(inner) ->
      case decode.run(value, decode.list(decode.dynamic)) {
        Ok(items) -> {
          let results =
            list.index_map(items, fn(item, idx) {
              check_value_against_schema_type(item, inner, schema_def)
              |> result.map_error(fn(msg) {
                "At index " <> int.to_string(idx) <> ": " <> msg
              })
            })
          let errors =
            list.filter_map(results, fn(r) {
              case r {
                Ok(_) -> Error(Nil)
                Error(msg) -> Ok(msg)
              }
            })
          case errors {
            [] -> Ok(value)
            [first, ..] -> Error(first)
          }
        }
        Error(_) ->
          // Single item coercion to list — allowed per spec
          check_value_against_schema_type(value, inner, schema_def)
      }
    schema.Named(type_name) ->
      check_named_type_value(value, type_name, schema_def)
  }
}

fn check_named_type_value(
  value: Dynamic,
  type_name: String,
  schema_def: schema.Schema,
) -> Result(Dynamic, String) {
  case type_name {
    "String" ->
      case decode.run(value, decode.string) {
        Ok(_) -> Ok(value)
        Error(_) -> Error("Expected String, got incompatible type")
      }
    "Int" ->
      case decode.run(value, decode.int) {
        Ok(_) -> Ok(value)
        Error(_) -> Error("Expected Int, got incompatible type")
      }
    "Float" ->
      case decode.run(value, decode.float) {
        Ok(_) -> Ok(value)
        Error(_) ->
          // Int is coercible to Float
          case decode.run(value, decode.int) {
            Ok(i) -> Ok(types.to_dynamic(int.to_float(i)))
            Error(_) -> Error("Expected Float, got incompatible type")
          }
      }
    "Boolean" ->
      case decode.run(value, decode.bool) {
        Ok(_) -> Ok(value)
        Error(_) -> Error("Expected Boolean, got incompatible type")
      }
    "ID" ->
      case decode.run(value, decode.string) {
        Ok(_) -> Ok(value)
        Error(_) ->
          case decode.run(value, decode.int) {
            Ok(i) -> Ok(types.to_dynamic(int.to_string(i)))
            Error(_) ->
              Error("Expected ID (String or Int), got incompatible type")
          }
      }
    _ ->
      case dict.get(schema_def.types, type_name) {
        Ok(schema.EnumTypeDef(enum_def)) ->
          case decode.run(value, decode.string) {
            Ok(s) ->
              case dict.has_key(enum_def.values, s) {
                True -> Ok(value)
                False ->
                  Error(
                    "Value \""
                    <> s
                    <> "\" is not a valid value for enum "
                    <> type_name,
                  )
              }
            Error(_) -> Error("Expected enum value for " <> type_name)
          }
        Ok(schema.InputObjectTypeDef(_)) ->
          // For input objects, trust the Dynamic — deep validation is handled by input_coercion
          Ok(value)
        Ok(schema.ScalarTypeDef(_)) ->
          // Custom scalars — trust the value
          Ok(value)
        _ -> Ok(value)
      }
  }
}

fn is_null_value(value: Dynamic) -> Bool {
  // A JSON `null` decoded through gleam_json does not `==` Nil atom-wise
  // on Erlang; `decode.optional` is the robust cross-target way.
  case value == types.to_dynamic(Nil) {
    True -> True
    False ->
      case decode.run(value, decode.optional(decode.dynamic)) {
        Ok(None) -> True
        _ -> False
      }
  }
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
  let #(data_dict, errors_acc, has_none, deferred_acc) =
    list.fold(
      selection_set.selections,
      #(dict.new(), [], False, []),
      fn(acc, selection) {
        let #(data_map, errors_list, none_found, deferred_list) = acc

        case get_defer_info(selection, context.variable_values) {
          Some(#(label, True)) -> {
            let result =
              execute_selection(context, selection, object_type, field_context)
            let patch =
              DeferredPatch(
                path: field_context.path,
                data: result.data,
                errors: result.errors,
                label: label,
              )
            let new_deferred =
              list.append(deferred_list, [patch, ..result.deferred])
            #(data_map, errors_list, none_found, new_deferred)
          }
          _ -> {
            let result =
              execute_selection(context, selection, object_type, field_context)

            let new_map = case result.data {
              Some(d) ->
                case decode.run(d, decode.dict(decode.string, decode.dynamic)) {
                  Ok(field_dict) -> dict.merge(data_map, field_dict)
                  Error(_) -> data_map
                }
              None -> data_map
            }
            let new_none = none_found || option.is_none(result.data)
            let new_deferred = list.append(deferred_list, result.deferred)

            #(new_map, [result.errors, ..errors_list], new_none, new_deferred)
          }
        }
      },
    )

  let errors = list.reverse(errors_acc) |> list.flatten

  case has_none, dict.is_empty(data_dict), errors {
    True, _, _ ->
      ExecutionResult(data: None, errors: errors, deferred: deferred_acc)
    False, True, [] ->
      ExecutionResult(
        data: Some(types.to_dynamic(dict.new())),
        errors: [],
        deferred: deferred_acc,
      )
    False, True, _ ->
      ExecutionResult(data: None, errors: errors, deferred: deferred_acc)
    False, False, _ ->
      ExecutionResult(
        data: Some(types.to_dynamic(data_dict)),
        errors: errors,
        deferred: deferred_acc,
      )
  }
}

fn extract_defer_label(
  directive: ast.Directive,
  variables: Dict(String, Dynamic),
) -> Option(String) {
  directive.arguments
  |> list.find(fn(a) { a.name == "label" })
  |> result.try(fn(a) {
    case a.value {
      ast.StringValue(s) -> Ok(s)
      ast.VariableValue(name) ->
        dict.get(variables, name)
        |> result.try(fn(v) {
          decode.run(v, decode.string) |> result.map_error(fn(_) { Nil })
        })
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

fn get_defer_info(
  selection: ast.Selection,
  variables: Dict(String, Dynamic),
) -> Option(#(Option(String), Bool)) {
  let directives = case selection {
    ast.InlineFragment(inline) -> inline.directives
    ast.FragmentSpread(spread) -> spread.directives
    ast.FieldSelection(_) -> []
  }
  case list.find(directives, fn(d) { d.name == "defer" }) {
    Error(_) -> None
    Ok(directive) -> {
      let should_defer =
        get_directive_bool_arg([directive], "defer", "if", variables)
        |> option.unwrap(True)
      Some(#(extract_defer_label(directive, variables), should_defer))
    }
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
      // Check if object_type implements the interface named type_condition
      let implements_interface = case
        dict.get(schema_def.types, object_type_name)
      {
        Ok(schema.ObjectTypeDef(obj)) ->
          list.any(obj.interfaces, fn(iface) { iface.name == type_condition })
        _ -> False
      }
      case implements_interface {
        True -> True
        False ->
          // Check if type_condition is a union and object_type_name is a member
          case dict.get(schema_def.types, type_condition) {
            Ok(schema.UnionTypeDef(union_type)) ->
              list.any(union_type.types, fn(t) { t.name == object_type_name })
            _ -> False
          }
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
    "skip" | "include" | "deprecated" | "defer" -> Ok(value)
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
  // Extract source location from the AST field for error reporting
  let field_location =
    option.map(field.location, fn(pos) { #(pos.line, pos.column) })

  use field_def <- require_field(
    object_type,
    field.name,
    field_path,
    field_location,
  )

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
      case field_def.rich_resolver, field_def.resolver {
        Some(rich_res), _ ->
          resolve_rich_field(
            context,
            field,
            field_def,
            field_args,
            response_name,
            field_path,
            rich_res,
            field_context.parent_value,
            field.directives,
          )
        None, Some(resolver) ->
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
        None, None ->
          resolve_from_parent(
            field_context.parent_value,
            field.name,
            response_name,
            field_path,
          )
      }
    }
    Error(coercion_error) ->
      validation_error_at(
        input_coercion.format_error(coercion_error),
        field_path,
        field_location,
      )
  }
}

fn require_field(
  object_type: schema.ObjectType,
  field_name: String,
  path: List(String),
  location: Option(#(Int, Int)),
  next: fn(schema.FieldDefinition) -> ExecutionResult,
) -> ExecutionResult {
  case dict.get(object_type.fields, field_name) {
    Ok(field_def) -> next(field_def)
    Error(_) ->
      validation_error_at(
        "Field '"
          <> field_name
          <> "' not found on type '"
          <> object_type.name
          <> "'",
        path,
        location,
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
  // Extract source location from the AST field for error reporting
  let field_location =
    option.map(field.location, fn(pos) { #(pos.line, pos.column) })

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
        Error(msg) -> resolver_error_at(msg, field_path, field_location)
      }
    Error(msg) -> resolver_error_at(msg, field_path, field_location)
  }
}

fn resolve_rich_field(
  context: QueryExecutionContext,
  field: ast.Field,
  field_def: schema.FieldDefinition,
  field_args: Dict(String, Dynamic),
  response_name: String,
  field_path: List(String),
  rich_res: schema.RichResolver,
  parent_value: Option(Dynamic),
  directives: List(ast.Directive),
) -> ExecutionResult {
  let field_location =
    option.map(field.location, fn(pos) { #(pos.line, pos.column) })

  let resolver_info =
    schema.ResolverInfo(
      parent: parent_value,
      arguments: field_args,
      context: context.execution_context,
      info: types.to_dynamic(dict.new()),
    )

  emit_telemetry(
    context.execution_context,
    schema.SchemaFieldStart(field_def.name, response_name, field_path),
  )
  let start_ns = telemetry.get_timestamp_ns()
  let rich_result = rich_res(resolver_info)
  let duration_ns = telemetry.get_timestamp_ns() - start_ns
  emit_telemetry(
    context.execution_context,
    schema.SchemaFieldEnd(
      field_def.name,
      response_name,
      field_path,
      result.is_ok(rich_result),
      duration_ns,
    ),
  )

  case rich_result {
    Ok(resolved) ->
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
        Error(msg) -> resolver_error_at(msg, field_path, field_location)
      }
    Error(#(msg, extensions)) ->
      rich_resolver_error_at(
        graphql_error.GraphQLError(
          message: msg,
          locations: None,
          path: None,
          extensions: extensions,
        ),
        field_path,
        field_location,
      )
  }
}

/// Emit a SchemaEvent to the telemetry callback if one is configured.
fn emit_telemetry(
  exec_context: schema.ExecutionContext,
  event: schema.SchemaEvent,
) -> Nil {
  case exec_context.telemetry_fn {
    Some(fn_) -> fn_(event)
    None -> Nil
  }
}

/// Execute a resolver through the middleware function if one is configured,
/// otherwise call the resolver directly. Emits telemetry field events.
fn execute_resolver_with_middleware(
  exec_context: schema.ExecutionContext,
  field_def: schema.FieldDefinition,
  resolver_info: schema.ResolverInfo,
  resolver: schema.Resolver,
  parent_type_name: String,
) -> Result(Dynamic, String) {
  // Determine current field path from resolver_info parent (best effort)
  let path = []

  emit_telemetry(
    exec_context,
    schema.SchemaFieldStart(field_def.name, parent_type_name, path),
  )

  let start_ns = telemetry.get_timestamp_ns()

  let result = case exec_context.middleware_fn {
    Some(mw_fn) -> mw_fn(parent_type_name, field_def, resolver_info, resolver)
    None -> resolver(resolver_info)
  }

  let duration_ns = telemetry.get_timestamp_ns() - start_ns

  emit_telemetry(
    exec_context,
    schema.SchemaFieldEnd(
      field_def.name,
      parent_type_name,
      path,
      result |> result.is_ok,
      duration_ns,
    ),
  )

  result
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
  let field_location =
    option.map(field.location, fn(pos) { #(pos.line, pos.column) })
  use resolved_value <- with_non_null_check(
    resolved,
    field_def.field_type,
    response_name,
    field_path,
    field_location,
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
  field_location: Option(#(Int, Int)),
  next: fn(Dynamic) -> ExecutionResult,
) -> ExecutionResult {
  case is_null(resolved), is_non_null_type(field_type) {
    True, True ->
      null_value_error_at(
        "Cannot return null for non-null field '" <> response_name <> "'",
        field_path,
        field_location,
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
        deferred: sub_result.deferred,
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
  let #(data_acc, errors_acc, deferred_acc, has_null_error) =
    list.fold(results, #([], [], [], False), fn(acc, result) {
      let #(data_list, error_list, deferred_list, null_found) = acc
      let new_data = case result.data {
        Some(d) -> [d, ..data_list]
        None -> data_list
      }
      let new_null =
        null_found
        || list.any(result.errors, fn(e) {
          case e {
            NullValueError(_, _, _) -> True
            _ -> False
          }
        })
      #(
        new_data,
        [result.errors, ..error_list],
        [result.deferred, ..deferred_list],
        new_null,
      )
    })

  let errors = list.reverse(errors_acc) |> list.flatten
  let deferred = list.reverse(deferred_acc) |> list.flatten
  let data_list = list.reverse(data_acc)

  case has_null_error, errors {
    True, _ ->
      handle_list_null_error(field_type, response_name, errors, deferred)
    False, [] ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(data_list))),
        errors: [],
        deferred: deferred,
      )
    False, _ ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(data_list))),
        errors: errors,
        deferred: deferred,
      )
  }
}

/// Handle null errors in list results based on nullability
fn handle_list_null_error(
  field_type: schema.FieldType,
  response_name: String,
  errors: List(ExecutionError),
  deferred: List(DeferredPatch),
) -> ExecutionResult {
  case is_non_null_type(field_type) {
    True -> ExecutionResult(data: None, errors: errors, deferred: deferred)
    False ->
      ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(Nil))),
        errors: errors,
        deferred: deferred,
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
  field_name: String,
  response_name: String,
  field_path: List(String),
) -> ExecutionResult {
  parent
  |> option.map(fn(p) {
    case decode.run(p, decode.dict(decode.string, decode.dynamic)) {
      Ok(d) ->
        case dict.get(d, field_name) {
          Ok(value) -> ok_result(make_field(response_name, value))
          Error(_) ->
            ok_result(make_field(response_name, types.to_dynamic(Nil)))
        }
      Error(_) -> ok_result(make_field(response_name, types.to_dynamic(Nil)))
    }
  })
  |> option.unwrap(resolver_error("No resolver and no parent value", field_path))
}

fn get_field_type_definition(
  schema_def: schema.Schema,
  field_type: schema.FieldType,
) -> Result(schema.TypeDefinition, String) {
  case field_type {
    schema.Named(name) ->
      // First check for introspection types
      case introspection.get_introspection_object_type(name) {
        Some(obj) -> Ok(schema.ObjectTypeDef(obj))
        None ->
          dict.get(schema_def.types, name)
          |> result.map_error(fn(_) {
            "Type '" <> name <> "' not found in schema"
          })
      }
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
  let introspection_data =
    introspection.build_schema_introspection(context.schema)

  // Process selection set if present
  case field.selection_set {
    None -> ok_result(make_field(response_name, introspection_data))
    Some(selection_set) -> {
      let schema_type = introspection.get_introspection_schema_type()
      let field_ctx =
        FieldContext(
          parent_value: Some(introspection_data),
          field_name: "__schema",
          field_args: dict.new(),
          path: [response_name],
        )
      let result =
        execute_selection_set(context, selection_set, schema_type, field_ctx)
      wrap_result_in_field(result, response_name)
    }
  }
}

fn execute_introspection_type(
  context: QueryExecutionContext,
  field: ast.Field,
  field_path: List(String),
  response_name: String,
) -> ExecutionResult {
  get_string_argument(field.arguments, "name", context.variable_values)
  |> option.map(fn(name) {
    let introspection_data =
      introspection.build_type_introspection(context.schema, name)

    // Check if the type exists (null check)
    case is_null(introspection_data) {
      True -> ok_result(make_field(response_name, types.to_dynamic(Nil)))
      False ->
        // Process selection set if present
        case field.selection_set {
          None -> ok_result(make_field(response_name, introspection_data))
          Some(selection_set) -> {
            let type_type = introspection.get_introspection_type_type()
            let field_ctx =
              FieldContext(
                parent_value: Some(introspection_data),
                field_name: "__type",
                field_args: dict.new(),
                path: field_path,
              )
            let result =
              execute_selection_set(
                context,
                selection_set,
                type_type,
                field_ctx,
              )
            wrap_result_in_field(result, response_name)
          }
        }
    }
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

// Helpers
// ============================================================================

fn make_field(name: String, value: Dynamic) -> Dynamic {
  types.to_dynamic(dict.from_list([#(name, value)]))
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
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))
  case get_or_parse(schema_def, query) {
    Ok(document) -> execute(schema_def, document, None, ctx, variables)
    Error(error) -> {
      let loc = case error {
        parser.UnexpectedToken(_, _, pos) -> Some(#(pos.line, pos.column))
        _ -> None
      }
      validation_error_at("Parse error: " <> format_parse_error(error), [], loc)
    }
  }
}

pub fn get_or_parse(
  schema_def: schema.Schema,
  query: String,
) -> Result(ast.Document, parser.ParseError) {
  case get_cached(schema_def, query) {
    Ok(doc) -> Ok(doc)
    Error(_) -> {
      use doc <- result.map(parser.parse(query))
      cache_put(schema_def, query, doc)
      doc
    }
  }
}

fn get_cached(
  schema_def: schema.Schema,
  query: String,
) -> Result(ast.Document, Nil) {
  case schema_def.document_cache {
    None -> Error(Nil)
    Some(cache) -> document_cache.get(cache, query)
  }
}

fn cache_put(schema_def: schema.Schema, query: String, doc: ast.Document) -> Nil {
  case schema_def.document_cache {
    None -> Nil
    Some(cache) -> document_cache.put(cache, query, doc)
  }
}

/// Execute a query with a custom execution context (enables full telemetry).
///
/// Use this function when you need telemetry for parsing and validation phases.
///
/// ## Example
///
/// ```gleam
/// let config = telemetry.with_handler(fn(event) { echo event })
/// let ctx = schema.execution_context(user_data)
///   |> schema.with_telemetry_fn(telemetry.to_schema_fn(config))
///
/// let result = executor.execute_query_with_context(
///   my_schema,
///   "{ users { name } }",
///   dict.new(),
///   ctx,
/// )
/// ```
pub fn execute_query_with_context(
  schema_def: schema.Schema,
  query: String,
  variables: Dict(String, Dynamic),
  ctx: schema.ExecutionContext,
) -> ExecutionResult {
  emit_telemetry(ctx, schema.SchemaParseStart)
  let parse_start = telemetry.get_timestamp_ns()

  let parse_result = case get_cached(schema_def, query) {
    Ok(doc) -> Ok(#(doc, True))
    Error(_) -> result.map(parser.parse(query), fn(doc) { #(doc, False) })
  }

  case parse_result {
    Ok(#(document, was_cached)) -> {
      let parse_duration = telemetry.get_timestamp_ns() - parse_start
      emit_telemetry(ctx, schema.SchemaParseEnd(True, parse_duration))

      case was_cached {
        True -> execute(schema_def, document, None, ctx, variables)
        False -> {
          emit_telemetry(ctx, schema.SchemaValidationStart)
          let validation_start = telemetry.get_timestamp_ns()

          case validation.validate_located(document, schema_def) {
            Ok(_validated_doc) -> {
              let validation_duration =
                telemetry.get_timestamp_ns() - validation_start
              emit_telemetry(
                ctx,
                schema.SchemaValidationEnd(True, 0, validation_duration),
              )
              cache_put(schema_def, query, document)
              execute(schema_def, document, None, ctx, variables)
            }
            Error(validation_errors) -> {
              let validation_duration =
                telemetry.get_timestamp_ns() - validation_start
              let error_count = list.length(validation_errors)
              emit_telemetry(
                ctx,
                schema.SchemaValidationEnd(
                  False,
                  error_count,
                  validation_duration,
                ),
              )
              let errors =
                list.map(validation_errors, fn(le) {
                  let #(err, loc) = le
                  ValidationError(
                    message: validation.format_error(err),
                    path: [],
                    location: loc,
                  )
                })
              ExecutionResult(data: None, errors: errors, deferred: [])
            }
          }
        }
      }
    }
    Error(error) -> {
      let parse_duration = telemetry.get_timestamp_ns() - parse_start
      emit_telemetry(ctx, schema.SchemaParseEnd(False, parse_duration))
      let loc = case error {
        parser.UnexpectedToken(_, _, pos) -> Some(#(pos.line, pos.column))
        _ -> None
      }
      validation_error_at("Parse error: " <> format_parse_error(error), [], loc)
    }
  }
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
      let loc = case error {
        parser.UnexpectedToken(_, _, pos) -> Some(#(pos.line, pos.column))
        _ -> None
      }
      validation_error_at("Parse error: " <> format_parse_error(error), [], loc)
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

fn format_token(token: lexer.Token) -> String {
  case token {
    lexer.Name(v) -> "\"" <> v <> "\""
    lexer.IntValue(v) -> int.to_string(v)
    lexer.FloatValue(_) -> "Float"
    lexer.StringValue(_) -> "String"
    lexer.EOF -> "end of document"
    lexer.Bang -> "!"
    lexer.Dollar -> "$"
    lexer.LeftParen -> "("
    lexer.RightParen -> ")"
    lexer.LeftBrace -> "{"
    lexer.RightBrace -> "}"
    lexer.LeftBracket -> "["
    lexer.RightBracket -> "]"
    lexer.Colon -> ":"
    lexer.Equals -> "="
    lexer.Spread -> "..."
    lexer.Pipe -> "|"
    lexer.At -> "@"
    lexer.Amp -> "&"
    lexer.Query -> "query"
    lexer.Mutation -> "mutation"
    lexer.Subscription -> "subscription"
    lexer.Fragment -> "fragment"
    lexer.On -> "on"
    lexer.TrueKeyword -> "true"
    lexer.FalseKeyword -> "false"
    lexer.NullKeyword -> "null"
  }
}

fn format_parse_error(error: parser.ParseError) -> String {
  case error {
    parser.LexError(_) -> "Lexer error"
    parser.UnexpectedToken(expected, got, _) ->
      "Expected " <> expected <> ", got " <> format_token(got)
    parser.UnexpectedEOF(expected) ->
      "Unexpected end of document, expected " <> expected
  }
}
