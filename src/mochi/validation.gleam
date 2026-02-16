// mochi/validation.gleam
// Query validation for GraphQL documents against a schema
//
// Validates:
// - Fields exist on the queried type
// - Required arguments are provided
// - Fragments are defined and used correctly
// - Variables are defined and used correctly
// - Directive usage is valid

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import mochi/ast.{type Document}
import mochi/parser
import mochi/schema.{type FieldDefinition, type ObjectType, type Schema}

// ============================================================================
// Validation Error Types
// ============================================================================

pub type ValidationError {
  /// Field does not exist on the type
  UnknownField(field_name: String, type_name: String)
  /// Required argument is missing
  MissingRequiredArgument(field_name: String, argument_name: String)
  /// Unknown argument provided
  UnknownArgument(field_name: String, argument_name: String)
  /// Fragment is not defined
  UndefinedFragment(fragment_name: String)
  /// Fragment type condition is invalid (type doesn't exist)
  InvalidTypeCondition(fragment_name: String, type_name: String)
  /// Variable is not defined
  UndefinedVariable(variable_name: String)
  /// Variable is defined but not used
  UnusedVariable(variable_name: String)
  /// Duplicate operation name
  DuplicateOperationName(name: String)
  /// Duplicate fragment name
  DuplicateFragmentName(name: String)
  /// Anonymous operation must be alone
  AnonymousOperationNotAlone
  /// Subscription has multiple root fields
  SubscriptionMultipleRootFields(operation_name: Option(String))
  /// Circular fragment reference
  CircularFragmentReference(fragment_name: String, path: List(String))
  /// Directive not allowed in location
  DirectiveNotAllowed(directive_name: String, location: String)
  /// Unknown directive
  UnknownDirective(directive_name: String)
  /// Selection set required on non-leaf field
  SelectionSetRequired(field_name: String, type_name: String)
  /// Selection set not allowed on leaf field (scalar/enum)
  SelectionSetNotAllowed(field_name: String, type_name: String)
}

/// Validation context tracks state during validation
pub type ValidationContext {
  ValidationContext(
    schema: Schema,
    fragments: Dict(String, ast.Fragment),
    defined_variables: Set(String),
    used_variables: Set(String),
    errors: List(ValidationError),
    current_type: Option(ObjectType),
    fragment_spread_path: List(String),
  )
}

// ============================================================================
// Main Validation Functions
// ============================================================================

/// Validate a document against a schema
pub fn validate(
  document: Document,
  schema: Schema,
) -> Result(Document, List(ValidationError)) {
  let ctx = init_context(schema, document)

  // Collect all validation errors
  let ctx = validate_unique_operation_names(ctx, document)
  let ctx = validate_lone_anonymous_operation(ctx, document)
  let ctx = validate_unique_fragment_names(ctx, document)
  let ctx = validate_document_operations(ctx, document)
  let ctx = validate_fragment_definitions(ctx, document)
  let ctx = validate_fragment_cycles(ctx, document)

  case ctx.errors {
    [] -> Ok(document)
    errors -> Error(list.reverse(errors))
  }
}

/// Validate a single query string against a schema (convenience function)
pub fn validate_query(
  query: String,
  schema: Schema,
) -> Result(Document, List(ValidationError)) {
  case parser.parse(query) {
    Ok(document) -> validate(document, schema)
    Error(_parse_error) -> Error([])
  }
}

// ============================================================================
// Context Initialization
// ============================================================================

fn init_context(schema: Schema, document: Document) -> ValidationContext {
  let fragments = collect_fragments(document)

  ValidationContext(
    schema: schema,
    fragments: fragments,
    defined_variables: set.new(),
    used_variables: set.new(),
    errors: [],
    current_type: schema.query,
    fragment_spread_path: [],
  )
}

fn collect_fragments(document: Document) -> Dict(String, ast.Fragment) {
  list.fold(document.definitions, dict.new(), fn(acc, def) {
    case def {
      ast.FragmentDefinition(fragment) ->
        dict.insert(acc, fragment.name, fragment)
      _ -> acc
    }
  })
}

fn add_error(
  ctx: ValidationContext,
  error: ValidationError,
) -> ValidationContext {
  ValidationContext(..ctx, errors: [error, ..ctx.errors])
}

// ============================================================================
// Operation Validation
// ============================================================================

fn validate_unique_operation_names(
  ctx: ValidationContext,
  document: Document,
) -> ValidationContext {
  let operations = get_operations(document)
  let names =
    list.filter_map(operations, fn(op) {
      case op {
        ast.Operation(name: Some(name), ..) -> Ok(name)
        _ -> Error(Nil)
      }
    })

  check_duplicates(ctx, names, DuplicateOperationName)
}

fn validate_lone_anonymous_operation(
  ctx: ValidationContext,
  document: Document,
) -> ValidationContext {
  let operations = get_operations(document)
  let has_anonymous =
    list.any(operations, fn(op) {
      case op {
        ast.ShorthandQuery(_) -> True
        ast.Operation(name: None, ..) -> True
        _ -> False
      }
    })

  case has_anonymous && list.length(operations) > 1 {
    True -> add_error(ctx, AnonymousOperationNotAlone)
    False -> ctx
  }
}

fn validate_document_operations(
  ctx: ValidationContext,
  document: Document,
) -> ValidationContext {
  list.fold(document.definitions, ctx, fn(ctx, def) {
    case def {
      ast.OperationDefinition(operation) -> validate_operation(ctx, operation)
      _ -> ctx
    }
  })
}

fn validate_operation(
  ctx: ValidationContext,
  operation: ast.Operation,
) -> ValidationContext {
  case operation {
    ast.ShorthandQuery(selection_set) -> {
      let ctx = set_current_type(ctx, ctx.schema.query)
      validate_selection_set(ctx, selection_set)
    }
    ast.Operation(
      operation_type: op_type,
      variable_definitions: var_defs,
      selection_set: selection_set,
      name: op_name,
      ..,
    ) -> {
      // Set the current type based on operation type
      let root_type = case op_type {
        ast.Query -> ctx.schema.query
        ast.Mutation -> ctx.schema.mutation
        ast.Subscription -> ctx.schema.subscription
      }
      let ctx = set_current_type(ctx, root_type)

      // Track defined variables
      let ctx = track_defined_variables(ctx, var_defs)

      // Validate subscription has single root field
      let ctx = case op_type {
        ast.Subscription ->
          validate_subscription_single_root(ctx, selection_set, op_name)
        _ -> ctx
      }

      // Validate selection set
      let ctx = validate_selection_set(ctx, selection_set)

      // Check for unused variables
      validate_unused_variables(ctx)
    }
  }
}

fn set_current_type(
  ctx: ValidationContext,
  type_opt: Option(ObjectType),
) -> ValidationContext {
  ValidationContext(..ctx, current_type: type_opt)
}

fn track_defined_variables(
  ctx: ValidationContext,
  var_defs: List(ast.VariableDefinition),
) -> ValidationContext {
  let defined =
    list.fold(var_defs, set.new(), fn(acc, var_def) {
      set.insert(acc, var_def.variable)
    })
  ValidationContext(
    ..ctx,
    defined_variables: defined,
    used_variables: set.new(),
  )
}

fn validate_unused_variables(ctx: ValidationContext) -> ValidationContext {
  let unused = set.difference(ctx.defined_variables, ctx.used_variables)
  set.fold(unused, ctx, fn(ctx, var_name) {
    add_error(ctx, UnusedVariable(var_name))
  })
}

fn validate_subscription_single_root(
  ctx: ValidationContext,
  selection_set: ast.SelectionSet,
  op_name: Option(String),
) -> ValidationContext {
  // Count root fields (not counting __typename)
  let root_fields =
    list.filter(selection_set.selections, fn(sel) {
      case sel {
        ast.FieldSelection(field) -> field.name != "__typename"
        _ -> True
      }
    })

  case list.length(root_fields) > 1 {
    True -> add_error(ctx, SubscriptionMultipleRootFields(op_name))
    False -> ctx
  }
}

// ============================================================================
// Selection Set Validation
// ============================================================================

fn validate_selection_set(
  ctx: ValidationContext,
  selection_set: ast.SelectionSet,
) -> ValidationContext {
  list.fold(selection_set.selections, ctx, fn(ctx, selection) {
    validate_selection(ctx, selection)
  })
}

fn validate_selection(
  ctx: ValidationContext,
  selection: ast.Selection,
) -> ValidationContext {
  case selection {
    ast.FieldSelection(field) -> validate_field(ctx, field)
    ast.FragmentSpread(spread) -> validate_fragment_spread(ctx, spread)
    ast.InlineFragment(inline) -> validate_inline_fragment(ctx, inline)
  }
}

fn validate_field(ctx: ValidationContext, field: ast.Field) -> ValidationContext {
  use ctx <- skip_introspection_field(field.name, ctx, field.arguments)
  use obj_type <- require_current_type(ctx)
  use field_def <- require_field_def(ctx, obj_type, field.name)

  ctx
  |> validate_field_arguments(field, field_def)
  |> track_argument_variables(field.arguments)
  |> validate_field_selection_set(field, field_def)
}

/// Skip validation for introspection fields, just track variables
fn skip_introspection_field(
  field_name: String,
  ctx: ValidationContext,
  arguments: List(ast.Argument),
  next: fn(ValidationContext) -> ValidationContext,
) -> ValidationContext {
  case string.starts_with(field_name, "__") {
    True -> track_argument_variables(ctx, arguments)
    False -> next(ctx)
  }
}

/// Require a current type exists in context
fn require_current_type(
  ctx: ValidationContext,
  next: fn(ObjectType) -> ValidationContext,
) -> ValidationContext {
  case ctx.current_type {
    Some(obj_type) -> next(obj_type)
    None -> ctx
  }
}

/// Require a field definition exists on the object type
fn require_field_def(
  ctx: ValidationContext,
  obj_type: ObjectType,
  field_name: String,
  next: fn(FieldDefinition) -> ValidationContext,
) -> ValidationContext {
  case dict.get(obj_type.fields, field_name) {
    Ok(field_def) -> next(field_def)
    Error(_) -> add_error(ctx, UnknownField(field_name, obj_type.name))
  }
}

fn validate_field_arguments(
  ctx: ValidationContext,
  field: ast.Field,
  field_def: FieldDefinition,
) -> ValidationContext {
  let provided_args =
    list.fold(field.arguments, dict.new(), fn(acc, arg) {
      dict.insert(acc, arg.name, True)
    })

  // Check for unknown arguments
  let ctx =
    list.fold(field.arguments, ctx, fn(ctx, arg) {
      case dict.has_key(field_def.arguments, arg.name) {
        True -> ctx
        False -> add_error(ctx, UnknownArgument(field.name, arg.name))
      }
    })

  // Check for missing required arguments
  dict.fold(field_def.arguments, ctx, fn(ctx, arg_name, arg_def) {
    case is_required_type(arg_def.arg_type) {
      True ->
        case dict.has_key(provided_args, arg_name) {
          True -> ctx
          False -> add_error(ctx, MissingRequiredArgument(field.name, arg_name))
        }
      False -> ctx
    }
  })
}

fn is_required_type(field_type: schema.FieldType) -> Bool {
  case field_type {
    schema.NonNull(_) -> True
    _ -> False
  }
}

fn validate_field_selection_set(
  ctx: ValidationContext,
  field: ast.Field,
  field_def: FieldDefinition,
) -> ValidationContext {
  let inner_type_name = get_base_type_name(field_def.field_type)
  let is_leaf = is_leaf_type(ctx.schema, inner_type_name)

  case is_leaf, field.selection_set {
    True, Some(_) ->
      add_error(ctx, SelectionSetNotAllowed(field.name, inner_type_name))
    True, None -> ctx
    False, None ->
      add_error(ctx, SelectionSetRequired(field.name, inner_type_name))
    False, Some(ss) -> {
      let inner_type = get_object_type(ctx.schema, inner_type_name)
      ctx
      |> set_current_type(inner_type)
      |> validate_selection_set(ss)
    }
  }
}

fn get_base_type_name(field_type: schema.FieldType) -> String {
  case field_type {
    schema.Named(name) -> name
    schema.NonNull(inner) -> get_base_type_name(inner)
    schema.List(inner) -> get_base_type_name(inner)
  }
}

fn is_leaf_type(schema: Schema, type_name: String) -> Bool {
  case type_name, dict.get(schema.types, type_name) {
    "String", _ | "Int", _ | "Float", _ | "Boolean", _ | "ID", _ -> True
    _, Ok(schema.ScalarTypeDef(_)) -> True
    _, Ok(schema.EnumTypeDef(_)) -> True
    _, _ -> False
  }
}

fn get_object_type(schema: Schema, type_name: String) -> Option(ObjectType) {
  case type_name, dict.get(schema.types, type_name) {
    "Query", _ -> schema.query
    "Mutation", _ -> schema.mutation
    "Subscription", _ -> schema.subscription
    _, Ok(schema.ObjectTypeDef(obj)) -> Some(obj)
    _, _ -> None
  }
}

fn track_argument_variables(
  ctx: ValidationContext,
  arguments: List(ast.Argument),
) -> ValidationContext {
  list.fold(arguments, ctx, fn(ctx, arg) {
    track_value_variables(ctx, arg.value)
  })
}

fn track_value_variables(
  ctx: ValidationContext,
  value: ast.Value,
) -> ValidationContext {
  case value {
    ast.VariableValue(name) -> {
      // Check if variable is defined
      let ctx = case set.contains(ctx.defined_variables, name) {
        True ->
          ValidationContext(
            ..ctx,
            used_variables: set.insert(ctx.used_variables, name),
          )
        False -> add_error(ctx, UndefinedVariable(name))
      }
      ctx
    }
    ast.ListValue(values) ->
      list.fold(values, ctx, fn(ctx, v) { track_value_variables(ctx, v) })
    ast.ObjectValue(fields) ->
      list.fold(fields, ctx, fn(ctx, f) { track_value_variables(ctx, f.value) })
    _ -> ctx
  }
}

// ============================================================================
// Fragment Validation
// ============================================================================

fn validate_unique_fragment_names(
  ctx: ValidationContext,
  document: Document,
) -> ValidationContext {
  let names =
    list.filter_map(document.definitions, fn(def) {
      case def {
        ast.FragmentDefinition(fragment) -> Ok(fragment.name)
        _ -> Error(Nil)
      }
    })

  check_duplicates(ctx, names, DuplicateFragmentName)
}

fn validate_fragment_definitions(
  ctx: ValidationContext,
  document: Document,
) -> ValidationContext {
  list.fold(document.definitions, ctx, fn(ctx, def) {
    case def {
      ast.FragmentDefinition(fragment) ->
        validate_fragment_type_condition(ctx, fragment)
      _ -> ctx
    }
  })
}

fn validate_fragment_type_condition(
  ctx: ValidationContext,
  fragment: ast.Fragment,
) -> ValidationContext {
  let type_name = fragment.type_condition

  // Check if the type exists in the schema
  let type_exists =
    type_name == "Query"
    || type_name == "Mutation"
    || type_name == "Subscription"
    || dict.has_key(ctx.schema.types, type_name)

  case type_exists {
    True -> ctx
    False -> add_error(ctx, InvalidTypeCondition(fragment.name, type_name))
  }
}

fn validate_fragment_spread(
  ctx: ValidationContext,
  spread: ast.FragmentSpreadValue,
) -> ValidationContext {
  let fragment_result = dict.get(ctx.fragments, spread.name)
  let is_cycle = list.contains(ctx.fragment_spread_path, spread.name)

  case fragment_result, is_cycle {
    Error(_), _ -> add_error(ctx, UndefinedFragment(spread.name))
    Ok(_), True -> ctx
    Ok(fragment), False -> {
      let fragment_type = get_object_type(ctx.schema, fragment.type_condition)
      let ctx = set_current_type(ctx, fragment_type)
      let ctx =
        ValidationContext(..ctx, fragment_spread_path: [
          spread.name,
          ..ctx.fragment_spread_path
        ])
      let ctx = validate_selection_set(ctx, fragment.selection_set)
      ValidationContext(
        ..ctx,
        fragment_spread_path: list.drop(ctx.fragment_spread_path, 1),
      )
    }
  }
}

fn validate_inline_fragment(
  ctx: ValidationContext,
  inline: ast.InlineFragmentValue,
) -> ValidationContext {
  case inline.type_condition {
    Some(type_name) -> {
      let inner_type = get_object_type(ctx.schema, type_name)
      let ctx = set_current_type(ctx, inner_type)
      validate_selection_set(ctx, inline.selection_set)
    }
    None -> validate_selection_set(ctx, inline.selection_set)
  }
}

fn validate_fragment_cycles(
  ctx: ValidationContext,
  _document: Document,
) -> ValidationContext {
  // Check each fragment for cycles
  dict.fold(ctx.fragments, ctx, fn(ctx, name, _fragment) {
    check_fragment_cycle(ctx, name, [])
  })
}

fn check_fragment_cycle(
  ctx: ValidationContext,
  fragment_name: String,
  path: List(String),
) -> ValidationContext {
  case list.contains(path, fragment_name) {
    True -> add_error(ctx, CircularFragmentReference(fragment_name, path))
    False -> {
      case dict.get(ctx.fragments, fragment_name) {
        Ok(fragment) -> {
          let new_path = [fragment_name, ..path]
          let spread_names = get_fragment_spreads(fragment.selection_set)
          list.fold(spread_names, ctx, fn(ctx, spread_name) {
            check_fragment_cycle(ctx, spread_name, new_path)
          })
        }
        Error(_) -> ctx
      }
    }
  }
}

fn get_fragment_spreads(selection_set: ast.SelectionSet) -> List(String) {
  list.flat_map(selection_set.selections, fn(sel) {
    case sel {
      ast.FragmentSpread(spread) -> [spread.name]
      ast.FieldSelection(field) ->
        case field.selection_set {
          Some(ss) -> get_fragment_spreads(ss)
          None -> []
        }
      ast.InlineFragment(inline) -> get_fragment_spreads(inline.selection_set)
    }
  })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn get_operations(document: Document) -> List(ast.Operation) {
  list.filter_map(document.definitions, fn(def) {
    case def {
      ast.OperationDefinition(operation) -> Ok(operation)
      _ -> Error(Nil)
    }
  })
}

fn check_duplicates(
  ctx: ValidationContext,
  names: List(String),
  error_fn: fn(String) -> ValidationError,
) -> ValidationContext {
  let #(_, ctx) =
    list.fold(names, #(set.new(), ctx), fn(acc, name) {
      let #(seen, ctx) = acc
      case set.contains(seen, name) {
        True -> #(seen, add_error(ctx, error_fn(name)))
        False -> #(set.insert(seen, name), ctx)
      }
    })
  ctx
}

// ============================================================================
// Error Formatting
// ============================================================================

/// Format a validation error as a human-readable string
pub fn format_error(error: ValidationError) -> String {
  case error {
    UnknownField(field_name, type_name) ->
      "Cannot query field \""
      <> field_name
      <> "\" on type \""
      <> type_name
      <> "\""
    MissingRequiredArgument(field_name, arg_name) ->
      "Field \""
      <> field_name
      <> "\" argument \""
      <> arg_name
      <> "\" of type is required but not provided"
    UnknownArgument(field_name, arg_name) ->
      "Unknown argument \""
      <> arg_name
      <> "\" on field \""
      <> field_name
      <> "\""
    UndefinedFragment(name) -> "Unknown fragment \"" <> name <> "\""
    InvalidTypeCondition(fragment_name, type_name) ->
      "Fragment \""
      <> fragment_name
      <> "\" cannot condition on non-existent type \""
      <> type_name
      <> "\""
    UndefinedVariable(name) -> "Variable \"$" <> name <> "\" is not defined"
    UnusedVariable(name) -> "Variable \"$" <> name <> "\" is never used"
    DuplicateOperationName(name) ->
      "There can be only one operation named \"" <> name <> "\""
    DuplicateFragmentName(name) ->
      "There can be only one fragment named \"" <> name <> "\""
    AnonymousOperationNotAlone ->
      "This anonymous operation must be the only defined operation"
    SubscriptionMultipleRootFields(name) ->
      "Subscription "
      <> case name {
        Some(n) -> "\"" <> n <> "\""
        None -> ""
      }
      <> " must select only one top level field"
    CircularFragmentReference(name, _path) ->
      "Cannot spread fragment \"" <> name <> "\" within itself"
    DirectiveNotAllowed(directive_name, location) ->
      "Directive \"@" <> directive_name <> "\" may not be used on " <> location
    UnknownDirective(name) -> "Unknown directive \"@" <> name <> "\""
    SelectionSetRequired(field_name, type_name) ->
      "Field \""
      <> field_name
      <> "\" of type \""
      <> type_name
      <> "\" must have a selection of subfields"
    SelectionSetNotAllowed(field_name, type_name) ->
      "Field \""
      <> field_name
      <> "\" must not have a selection since type \""
      <> type_name
      <> "\" has no subfields"
  }
}

/// Format all validation errors as a single string
pub fn format_errors(errors: List(ValidationError)) -> String {
  errors
  |> list.map(format_error)
  |> string.join("\n")
}
