//// Input value coercion and validation for GraphQL arguments.
////
//// This module validates and coerces input values (arguments, variables)
//// against their declared types in the schema. It ensures:
//// - Type compatibility (Int vs String, etc.)
//// - Enum value validity
//// - Input object field validation
//// - Non-null constraint enforcement
//// - List item type validation
////
//// ## Example
////
//// ```gleam
//// let result = input_coercion.coerce_argument_value(
////   ast.StringValue("hello"),
////   schema.Named("String"),
////   schema,
////   variables,
////   ["createUser", "input", "name"],
//// )
//// // Ok(dynamic("hello"))
//// ```

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string
import mochi/ast
import mochi/schema.{type FieldType, type Schema}
import mochi/types

/// Errors that can occur during input coercion
pub type CoercionError {
  /// Value type doesn't match expected type
  TypeMismatch(path: List(String), expected: String, got: String)
  /// Enum value is not valid for the enum type
  InvalidEnumValue(path: List(String), enum_name: String, value: String)
  /// Required field is missing from input object
  MissingRequiredField(path: List(String), field: String)
  /// Unknown field provided in input object
  UnknownField(path: List(String), field: String, type_name: String)
  /// Null value provided for non-null type
  NullNotAllowed(path: List(String))
  /// Referenced type not found in schema
  UnknownType(path: List(String), type_name: String)
  /// Custom scalar rejected the value
  ScalarCoercionFailed(path: List(String), scalar_name: String, reason: String)
}

/// Result of coercion - either a Dynamic value or an error
pub type CoercionResult =
  Result(Dynamic, CoercionError)

/// Coerce and validate an argument value against its declared type.
///
/// This is the main entry point for input validation. It recursively
/// validates the value structure against the schema type definition.
pub fn coerce_argument_value(
  value: ast.Value,
  expected_type: FieldType,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> CoercionResult {
  case expected_type, value {
    // Non-null wrapper: value must not be null
    schema.NonNull(_), ast.NullValue -> Error(NullNotAllowed(path))

    schema.NonNull(inner), other ->
      coerce_argument_value(other, inner, schema, variables, path)

    // Any nullable type accepts null
    _, ast.NullValue -> Ok(types.to_dynamic(Nil))

    // Variable substitution - variables should already be coerced
    _, ast.VariableValue(var_name) ->
      case dict.get(variables, var_name) {
        Ok(val) -> Ok(val)
        Error(_) -> Ok(types.to_dynamic(Nil))
      }

    // List type: validate each item
    schema.List(inner_type), ast.ListValue(items) ->
      coerce_list(items, inner_type, schema, variables, path)

    // Single value can be coerced to list of one
    schema.List(inner_type), single_value -> {
      use coerced <- result.try(coerce_argument_value(
        single_value,
        inner_type,
        schema,
        variables,
        list.append(path, ["0"]),
      ))
      Ok(types.to_dynamic([coerced]))
    }

    // Named types: scalars, enums, input objects
    schema.Named(type_name), _ ->
      coerce_named_type(value, type_name, schema, variables, path)
  }
}

/// Coerce a value against a named type (scalar, enum, or input object)
fn coerce_named_type(
  value: ast.Value,
  type_name: String,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> CoercionResult {
  // Check built-in scalars first
  case type_name {
    "String" -> coerce_string(value, path)
    "Int" -> coerce_int(value, path)
    "Float" -> coerce_float(value, path)
    "Boolean" -> coerce_boolean(value, path)
    "ID" -> coerce_id(value, path)
    _ -> coerce_custom_type(value, type_name, schema, variables, path)
  }
}

/// Coerce to String scalar
fn coerce_string(value: ast.Value, path: List(String)) -> CoercionResult {
  case value {
    ast.StringValue(s) -> Ok(types.to_dynamic(s))
    other -> Error(TypeMismatch(path, "String", value_type_name(other)))
  }
}

/// Coerce to Int scalar
fn coerce_int(value: ast.Value, path: List(String)) -> CoercionResult {
  case value {
    ast.IntValue(i) -> Ok(types.to_dynamic(i))
    other -> Error(TypeMismatch(path, "Int", value_type_name(other)))
  }
}

/// Coerce to Float scalar (Int coerces to Float per spec)
fn coerce_float(value: ast.Value, path: List(String)) -> CoercionResult {
  case value {
    ast.FloatValue(f) -> Ok(types.to_dynamic(f))
    ast.IntValue(i) -> Ok(types.to_dynamic(int.to_float(i)))
    other -> Error(TypeMismatch(path, "Float", value_type_name(other)))
  }
}

/// Coerce to Boolean scalar
fn coerce_boolean(value: ast.Value, path: List(String)) -> CoercionResult {
  case value {
    ast.BooleanValue(b) -> Ok(types.to_dynamic(b))
    other -> Error(TypeMismatch(path, "Boolean", value_type_name(other)))
  }
}

/// Coerce to ID scalar (accepts String or Int per spec)
fn coerce_id(value: ast.Value, path: List(String)) -> CoercionResult {
  case value {
    ast.StringValue(s) -> Ok(types.to_dynamic(s))
    ast.IntValue(i) -> Ok(types.to_dynamic(int.to_string(i)))
    other -> Error(TypeMismatch(path, "ID", value_type_name(other)))
  }
}

/// Coerce to a custom type (enum, input object, or custom scalar)
fn coerce_custom_type(
  value: ast.Value,
  type_name: String,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> CoercionResult {
  // Look up type in schema
  case dict.get(schema.types, type_name) {
    // Enum type
    Ok(schema.EnumTypeDef(enum_type)) -> coerce_enum(value, enum_type, path)

    // Input object type
    Ok(schema.InputObjectTypeDef(input_type)) ->
      coerce_input_object(value, input_type, schema, variables, path)

    // Custom scalar type
    Ok(schema.ScalarTypeDef(scalar_type)) ->
      coerce_custom_scalar(value, scalar_type, path)

    // Object/Interface/Union types are not valid for input
    Ok(_) ->
      Error(TypeMismatch(path, type_name <> " (input type)", "output type"))

    // Type not found
    Error(_) -> Error(UnknownType(path, type_name))
  }
}

/// Coerce to an enum type
fn coerce_enum(
  value: ast.Value,
  enum_type: schema.EnumType,
  path: List(String),
) -> CoercionResult {
  case value {
    ast.EnumValue(enum_value) -> {
      case dict.has_key(enum_type.values, enum_value) {
        True -> Ok(types.to_dynamic(enum_value))
        False -> Error(InvalidEnumValue(path, enum_type.name, enum_value))
      }
    }
    // String values are NOT valid for enums in GraphQL
    ast.StringValue(_) ->
      Error(TypeMismatch(
        path,
        enum_type.name <> " (use enum value without quotes)",
        "String",
      ))
    other -> Error(TypeMismatch(path, enum_type.name, value_type_name(other)))
  }
}

/// Coerce to an input object type
fn coerce_input_object(
  value: ast.Value,
  input_type: schema.InputObjectType,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> CoercionResult {
  case value {
    ast.ObjectValue(fields) -> {
      // Check for unknown fields
      use _ <- result.try(validate_no_unknown_fields(fields, input_type, path))

      // Coerce each expected field
      use coerced_fields <- result.try(
        coerce_input_fields(
          input_type.fields |> dict.to_list,
          fields,
          schema,
          variables,
          path,
          [],
        ),
      )

      Ok(types.to_dynamic(dict.from_list(coerced_fields)))
    }
    other -> Error(TypeMismatch(path, input_type.name, value_type_name(other)))
  }
}

/// Validate that no unknown fields are provided
fn validate_no_unknown_fields(
  provided_fields: List(ast.ObjectField),
  input_type: schema.InputObjectType,
  path: List(String),
) -> Result(Nil, CoercionError) {
  list.try_each(provided_fields, fn(field) {
    case dict.has_key(input_type.fields, field.name) {
      True -> Ok(Nil)
      False -> Error(UnknownField(path, field.name, input_type.name))
    }
  })
}

/// Coerce all fields of an input object
fn coerce_input_fields(
  expected_fields: List(#(String, schema.InputFieldDefinition)),
  provided_fields: List(ast.ObjectField),
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
  acc: List(#(String, Dynamic)),
) -> Result(List(#(String, Dynamic)), CoercionError) {
  case expected_fields {
    [] -> Ok(list.reverse(acc))
    [#(field_name, field_def), ..rest] -> {
      let field_path = list.append(path, [field_name])

      // Find the provided value for this field
      let provided_value =
        list.find(provided_fields, fn(f) { f.name == field_name })
        |> result.map(fn(f) { f.value })

      use coerced <- result.try(case provided_value {
        // Field was provided: coerce it
        Ok(value) ->
          coerce_argument_value(
            value,
            field_def.field_type,
            schema,
            variables,
            field_path,
          )

        // Field not provided
        Error(_) -> {
          case field_def.default_value {
            // Has default value: use it
            Some(default) -> Ok(default)

            // No default: check if required
            None -> {
              case field_def.field_type {
                schema.NonNull(_) ->
                  Error(MissingRequiredField(path, field_name))
                _ -> Ok(types.to_dynamic(Nil))
              }
            }
          }
        }
      })

      coerce_input_fields(rest, provided_fields, schema, variables, path, [
        #(field_name, coerced),
        ..acc
      ])
    }
  }
}

/// Coerce a list value
fn coerce_list(
  items: List(ast.Value),
  inner_type: FieldType,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
) -> CoercionResult {
  use coerced_items <- result.try(
    coerce_list_items(items, inner_type, schema, variables, path, 0, []),
  )
  Ok(types.to_dynamic(list.reverse(coerced_items)))
}

fn coerce_list_items(
  items: List(ast.Value),
  inner_type: FieldType,
  schema: Schema,
  variables: Dict(String, Dynamic),
  path: List(String),
  index: Int,
  acc: List(Dynamic),
) -> Result(List(Dynamic), CoercionError) {
  case items {
    [] -> Ok(acc)
    [item, ..rest] -> {
      let item_path = list.append(path, [int.to_string(index)])
      use coerced <- result.try(coerce_argument_value(
        item,
        inner_type,
        schema,
        variables,
        item_path,
      ))
      coerce_list_items(rest, inner_type, schema, variables, path, index + 1, [
        coerced,
        ..acc
      ])
    }
  }
}

/// Coerce to a custom scalar type
fn coerce_custom_scalar(
  value: ast.Value,
  scalar_type: schema.ScalarType,
  path: List(String),
) -> CoercionResult {
  // Convert AST value to Dynamic first
  let raw_value = ast_value_to_dynamic(value)

  // Use the scalar's parse_literal function
  case scalar_type.parse_literal(raw_value) {
    Ok(parsed) -> Ok(parsed)
    Error(reason) -> Error(ScalarCoercionFailed(path, scalar_type.name, reason))
  }
}

/// Convert an AST value to Dynamic (without validation)
fn ast_value_to_dynamic(value: ast.Value) -> Dynamic {
  case value {
    ast.IntValue(i) -> types.to_dynamic(i)
    ast.FloatValue(f) -> types.to_dynamic(f)
    ast.StringValue(s) -> types.to_dynamic(s)
    ast.BooleanValue(b) -> types.to_dynamic(b)
    ast.NullValue -> types.to_dynamic(Nil)
    ast.EnumValue(e) -> types.to_dynamic(e)
    ast.VariableValue(name) -> types.to_dynamic(name)
    ast.ListValue(values) ->
      types.to_dynamic(list.map(values, ast_value_to_dynamic))
    ast.ObjectValue(fields) ->
      types.to_dynamic(
        list.fold(fields, dict.new(), fn(acc, f) {
          dict.insert(acc, f.name, ast_value_to_dynamic(f.value))
        }),
      )
  }
}

/// Get a human-readable name for an AST value type
fn value_type_name(value: ast.Value) -> String {
  case value {
    ast.IntValue(_) -> "Int"
    ast.FloatValue(_) -> "Float"
    ast.StringValue(_) -> "String"
    ast.BooleanValue(_) -> "Boolean"
    ast.NullValue -> "Null"
    ast.EnumValue(_) -> "Enum"
    ast.ListValue(_) -> "List"
    ast.ObjectValue(_) -> "Object"
    ast.VariableValue(_) -> "Variable"
  }
}

// ============================================================================
// Error Formatting
// ============================================================================

/// Format a coercion error as a human-readable message
pub fn format_error(error: CoercionError) -> String {
  case error {
    TypeMismatch(path, expected, got) ->
      "Type mismatch at "
      <> format_path(path)
      <> ": expected "
      <> expected
      <> ", got "
      <> got

    InvalidEnumValue(path, enum_name, value) ->
      "Invalid enum value at "
      <> format_path(path)
      <> ": '"
      <> value
      <> "' is not a valid "
      <> enum_name

    MissingRequiredField(path, field) ->
      "Missing required field '" <> field <> "' at " <> format_path(path)

    UnknownField(path, field, type_name) ->
      "Unknown field '"
      <> field
      <> "' on input type "
      <> type_name
      <> " at "
      <> format_path(path)

    NullNotAllowed(path) -> "Null value not allowed at " <> format_path(path)

    UnknownType(path, type_name) ->
      "Unknown type '" <> type_name <> "' at " <> format_path(path)

    ScalarCoercionFailed(path, scalar_name, reason) ->
      "Invalid "
      <> scalar_name
      <> " value at "
      <> format_path(path)
      <> ": "
      <> reason
  }
}

/// Format a path as a dot-separated string
fn format_path(path: List(String)) -> String {
  case path {
    [] -> "(root)"
    _ -> string.join(path, ".")
  }
}

// ============================================================================
// Batch Coercion for Arguments
// ============================================================================

/// Coerce all arguments for a field, returning either all coerced values or the first error
pub fn coerce_arguments(
  ast_args: List(ast.Argument),
  arg_defs: Dict(String, schema.ArgumentDefinition),
  schema: Schema,
  variables: Dict(String, Dynamic),
  field_path: List(String),
) -> Result(Dict(String, Dynamic), CoercionError) {
  // Build set of provided argument names
  let provided_names =
    ast_args
    |> list.map(fn(a) { a.name })
    |> set.from_list

  // Coerce provided arguments
  use coerced_provided <- result.try(
    coerce_provided_arguments(
      ast_args,
      arg_defs,
      schema,
      variables,
      field_path,
      [],
    ),
  )

  // Add default values for missing arguments
  use with_defaults <- result.try(add_missing_defaults(
    arg_defs |> dict.to_list,
    provided_names,
    field_path,
    coerced_provided,
  ))

  Ok(dict.from_list(with_defaults))
}

fn coerce_provided_arguments(
  args: List(ast.Argument),
  arg_defs: Dict(String, schema.ArgumentDefinition),
  schema: Schema,
  variables: Dict(String, Dynamic),
  field_path: List(String),
  acc: List(#(String, Dynamic)),
) -> Result(List(#(String, Dynamic)), CoercionError) {
  case args {
    [] -> Ok(acc)
    [arg, ..rest] -> {
      let arg_path = list.append(field_path, [arg.name])

      // Get argument definition
      use arg_def <- result.try(case dict.get(arg_defs, arg.name) {
        Ok(def) -> Ok(def)
        // Unknown arguments are caught by validation, allow them through here
        Error(_) ->
          Ok(schema.ArgumentDefinition(
            name: arg.name,
            description: None,
            arg_type: schema.Named("String"),
            default_value: None,
          ))
      })

      use coerced <- result.try(coerce_argument_value(
        arg.value,
        arg_def.arg_type,
        schema,
        variables,
        arg_path,
      ))

      coerce_provided_arguments(rest, arg_defs, schema, variables, field_path, [
        #(arg.name, coerced),
        ..acc
      ])
    }
  }
}

fn add_missing_defaults(
  arg_defs: List(#(String, schema.ArgumentDefinition)),
  provided_names: set.Set(String),
  field_path: List(String),
  acc: List(#(String, Dynamic)),
) -> Result(List(#(String, Dynamic)), CoercionError) {
  case arg_defs {
    [] -> Ok(acc)
    [#(name, def), ..rest] -> {
      case set.contains(provided_names, name) {
        // Already provided
        True -> add_missing_defaults(rest, provided_names, field_path, acc)
        // Not provided
        False -> {
          case def.default_value {
            // Has default
            Some(default) ->
              add_missing_defaults(rest, provided_names, field_path, [
                #(name, default),
                ..acc
              ])
            // No default, check if required
            None -> {
              case def.arg_type {
                schema.NonNull(_) ->
                  Error(MissingRequiredField(field_path, name))
                _ -> add_missing_defaults(rest, provided_names, field_path, acc)
              }
            }
          }
        }
      }
    }
  }
}
