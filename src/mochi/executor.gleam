// mochi/executor.gleam
// GraphQL query execution engine with functional patterns

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/ast
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
}

pub type QueryExecutionContext {
  QueryExecutionContext(
    schema: schema.Schema,
    root_value: Option(Dynamic),
    execution_context: schema.ExecutionContext,
    variable_values: Dict(String, Dynamic),
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
  let context =
    QueryExecutionContext(
      schema: schema_def,
      root_value: root_value,
      execution_context: execution_context,
      variable_values: variable_values,
    )

  document.definitions
  |> list.first
  |> result.map(execute_definition(context, _))
  |> result.unwrap(
    validation_error("Document must contain at least one operation", []),
  )
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
  let results =
    list.map(selection_set.selections, fn(selection) {
      execute_selection(context, selection, object_type, field_context)
    })

  let errors = list.flat_map(results, fn(r) { r.errors })
  let data_list =
    list.filter_map(results, fn(r) { option.to_result(r.data, Nil) })

  case data_list, errors {
    [], [] -> ok_result(types.to_dynamic(dict.new()))
    [], _ -> ExecutionResult(data: None, errors: errors)
    _, _ ->
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
    ast.FragmentSpread(_) ->
      validation_error("Fragment spreads not yet supported", field_context.path)
    ast.InlineFragment(_) ->
      validation_error("Inline fragments not yet supported", field_context.path)
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
  // Check @skip first - if @skip(if: true), exclude the field
  let skip_value = get_directive_bool_arg(directives, "skip", "if", variables)
  case skip_value {
    Some(True) -> False
    // Skip this field
    _ -> {
      // Check @include - if @include(if: false), exclude the field
      let include_value =
        get_directive_bool_arg(directives, "include", "if", variables)
      case include_value {
        Some(False) -> False
        // Exclude this field
        _ -> True
        // Include by default
      }
    }
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

@external(erlang, "mochi_ffi", "dynamic_to_bool")
@external(javascript, "../mochi_ffi.mjs", "dynamic_to_bool")
fn decode_bool_from_dynamic(value: Dynamic) -> Option(Bool)

fn execute_regular_field(
  context: QueryExecutionContext,
  field: ast.Field,
  object_type: schema.ObjectType,
  field_context: FieldContext,
  response_name: String,
  field_path: List(String),
) -> ExecutionResult {
  use field_def <- require_field(object_type, field.name, field_path)

  let field_args =
    coerce_arguments(
      field.arguments,
      field_def.arguments,
      context.variable_values,
    )

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
) -> ExecutionResult {
  let resolver_info =
    schema.ResolverInfo(
      parent: parent_value,
      arguments: field_args,
      context: context.execution_context,
      info: types.to_dynamic(dict.new()),
    )

  case resolver(resolver_info) {
    Ok(resolved) ->
      handle_resolved_value(
        context,
        field,
        field_def,
        field_args,
        response_name,
        field_path,
        resolved,
      )
    Error(msg) -> resolver_error(msg, field_path)
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
  case field.selection_set {
    None -> ok_result(make_field(response_name, resolved))
    Some(sub_ss) ->
      execute_sub_selection(
        context,
        sub_ss,
        field_def,
        field_args,
        field_path,
        resolved,
      )
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
    Ok(_) ->
      type_error("Cannot execute selection set on non-object type", field_path)
    Error(msg) -> type_error(msg, field_path)
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
// Argument Processing
// ============================================================================

fn coerce_arguments(
  ast_args: List(ast.Argument),
  arg_defs: Dict(String, schema.ArgumentDefinition),
  variables: Dict(String, Dynamic),
) -> Dict(String, Dynamic) {
  ast_args
  |> list.fold(dict.new(), fn(acc, arg) {
    dict.insert(acc, arg.name, coerce_value(arg.value, variables))
  })
  |> add_default_values(arg_defs, ast_args)
}

fn add_default_values(
  args: Dict(String, Dynamic),
  arg_defs: Dict(String, schema.ArgumentDefinition),
  provided_args: List(ast.Argument),
) -> Dict(String, Dynamic) {
  let provided_names = list.map(provided_args, fn(a) { a.name })

  dict.fold(arg_defs, args, fn(acc, name, def) {
    case list.contains(provided_names, name), def.default_value {
      False, Some(default) -> dict.insert(acc, name, default)
      _, _ -> acc
    }
  })
}

fn coerce_value(value: ast.Value, variables: Dict(String, Dynamic)) -> Dynamic {
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
      types.to_dynamic(list.map(values, coerce_value(_, variables)))
    ast.ObjectValue(fields) ->
      types.to_dynamic(
        list.fold(fields, dict.new(), fn(acc, f) {
          dict.insert(acc, f.name, coerce_value(f.value, variables))
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

@external(erlang, "mochi_ffi", "dynamic_to_string")
@external(javascript, "../mochi_ffi.mjs", "dynamic_to_string")
fn decode_string_from_dynamic(value: Dynamic) -> Option(String)

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
      #("directives", types.to_dynamic([])),
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
          #("isDeprecated", types.to_dynamic(False)),
          #("deprecationReason", types.to_dynamic(Nil)),
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
        #("isDeprecated", types.to_dynamic(False)),
        #("deprecationReason", types.to_dynamic(Nil)),
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
    _ -> types.to_dynamic(results)
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
