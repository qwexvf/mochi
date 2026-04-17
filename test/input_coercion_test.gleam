import gleam/dict
import gleeunit/should
import mochi/ast
import mochi/input_coercion
import mochi/schema
import mochi/types

// ============================================================================
// Test Helpers
// ============================================================================

fn test_schema() -> schema.Schema {
  // Create a schema with enum and input object types
  let role_enum =
    schema.EnumType(
      name: "Role",
      description: None,
      values: dict.from_list([
        #(
          "ADMIN",
          schema.EnumValueDefinition(
            name: "ADMIN",
            description: None,
            value: types.to_dynamic("ADMIN"),
            is_deprecated: False,
            deprecation_reason: None,
          ),
        ),
        #(
          "USER",
          schema.EnumValueDefinition(
            name: "USER",
            description: None,
            value: types.to_dynamic("USER"),
            is_deprecated: False,
            deprecation_reason: None,
          ),
        ),
      ]),
    )

  let create_user_input =
    schema.InputObjectType(
      name: "CreateUserInput",
      description: None,
      fields: dict.from_list([
        #(
          "name",
          schema.InputFieldDefinition(
            name: "name",
            description: None,
            field_type: schema.NonNull(schema.Named("String")),
            default_value: None,
          ),
        ),
        #(
          "age",
          schema.InputFieldDefinition(
            name: "age",
            description: None,
            field_type: schema.Named("Int"),
            default_value: None,
          ),
        ),
        #(
          "role",
          schema.InputFieldDefinition(
            name: "role",
            description: None,
            field_type: schema.NonNull(schema.Named("Role")),
            default_value: None,
          ),
        ),
      ]),
    )

  schema.Schema(
    query: None,
    mutation: None,
    subscription: None,
    types: dict.from_list([
      #("Role", schema.EnumTypeDef(role_enum)),
      #("CreateUserInput", schema.InputObjectTypeDef(create_user_input)),
    ]),
    directives: dict.new(),
  )
}

// ============================================================================
// Scalar Coercion Tests
// ============================================================================

pub fn coerce_string_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.StringValue("hello"),
      schema.Named("String"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_string_invalid_type_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.IntValue(42),
      schema.Named("String"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.TypeMismatch(_, "String", "Int")) -> Nil
    _ -> panic as "Expected TypeMismatch error"
  }
}

pub fn coerce_int_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.IntValue(42),
      schema.Named("Int"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_int_invalid_type_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.StringValue("42"),
      schema.Named("Int"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.TypeMismatch(_, "Int", "String")) -> Nil
    _ -> panic as "Expected TypeMismatch error"
  }
}

pub fn coerce_float_from_int_test() {
  // Int coerces to Float per GraphQL spec
  let result =
    input_coercion.coerce_argument_value(
      ast.IntValue(42),
      schema.Named("Float"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_float_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.FloatValue(3.14),
      schema.Named("Float"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_boolean_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.BooleanValue(True),
      schema.Named("Boolean"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_id_from_string_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.StringValue("abc123"),
      schema.Named("ID"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_id_from_int_test() {
  // ID accepts Int per spec
  let result =
    input_coercion.coerce_argument_value(
      ast.IntValue(123),
      schema.Named("ID"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

// ============================================================================
// Non-Null Tests
// ============================================================================

pub fn coerce_non_null_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.StringValue("hello"),
      schema.NonNull(schema.Named("String")),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_non_null_with_null_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.NullValue,
      schema.NonNull(schema.Named("String")),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.NullNotAllowed(_)) -> Nil
    _ -> panic as "Expected NullNotAllowed error"
  }
}

pub fn coerce_nullable_with_null_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.NullValue,
      schema.Named("String"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

// ============================================================================
// Enum Tests
// ============================================================================

pub fn coerce_enum_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.EnumValue("ADMIN"),
      schema.Named("Role"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_enum_invalid_value_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.EnumValue("SUPERUSER"),
      schema.Named("Role"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.InvalidEnumValue(_, "Role", "SUPERUSER")) -> Nil
    _ -> panic as "Expected InvalidEnumValue error"
  }
}

pub fn coerce_enum_string_not_allowed_test() {
  // String values are not valid for enums
  let result =
    input_coercion.coerce_argument_value(
      ast.StringValue("ADMIN"),
      schema.Named("Role"),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.TypeMismatch(_, _, "String")) -> Nil
    _ -> panic as "Expected TypeMismatch error"
  }
}

// ============================================================================
// List Tests
// ============================================================================

pub fn coerce_list_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.ListValue([ast.IntValue(1), ast.IntValue(2), ast.IntValue(3)]),
      schema.List(schema.Named("Int")),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

pub fn coerce_list_invalid_item_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.ListValue([ast.IntValue(1), ast.StringValue("two"), ast.IntValue(3)]),
      schema.List(schema.Named("Int")),
      test_schema(),
      dict.new(),
      ["field"],
    )

  case result {
    Error(input_coercion.TypeMismatch(path, "Int", "String")) -> {
      // Path should include the list index
      case path {
        ["field", "1"] -> Nil
        _ -> panic as "Expected path to include index"
      }
    }
    _ -> panic as "Expected TypeMismatch error"
  }
}

pub fn coerce_single_value_to_list_test() {
  // Single value can coerce to list of one
  let result =
    input_coercion.coerce_argument_value(
      ast.IntValue(42),
      schema.List(schema.Named("Int")),
      test_schema(),
      dict.new(),
      ["field"],
    )

  should.be_ok(result)
}

// ============================================================================
// Input Object Tests
// ============================================================================

pub fn coerce_input_object_valid_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.ObjectValue([
        ast.ObjectField("name", ast.StringValue("Alice")),
        ast.ObjectField("age", ast.IntValue(30)),
        ast.ObjectField("role", ast.EnumValue("USER")),
      ]),
      schema.Named("CreateUserInput"),
      test_schema(),
      dict.new(),
      ["input"],
    )

  should.be_ok(result)
}

pub fn coerce_input_object_missing_required_field_test() {
  // Missing required "role" field
  let result =
    input_coercion.coerce_argument_value(
      ast.ObjectValue([
        ast.ObjectField("name", ast.StringValue("Alice")),
        ast.ObjectField("age", ast.IntValue(30)),
      ]),
      schema.Named("CreateUserInput"),
      test_schema(),
      dict.new(),
      ["input"],
    )

  case result {
    Error(input_coercion.MissingRequiredField(_, "role")) -> Nil
    _ -> panic as "Expected MissingRequiredField error"
  }
}

pub fn coerce_input_object_unknown_field_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.ObjectValue([
        ast.ObjectField("name", ast.StringValue("Alice")),
        ast.ObjectField("email", ast.StringValue("alice@example.com")),
        ast.ObjectField("role", ast.EnumValue("USER")),
      ]),
      schema.Named("CreateUserInput"),
      test_schema(),
      dict.new(),
      ["input"],
    )

  case result {
    Error(input_coercion.UnknownField(_, "email", "CreateUserInput")) -> Nil
    _ -> panic as "Expected UnknownField error"
  }
}

pub fn coerce_input_object_wrong_field_type_test() {
  let result =
    input_coercion.coerce_argument_value(
      ast.ObjectValue([
        ast.ObjectField("name", ast.StringValue("Alice")),
        ast.ObjectField("age", ast.StringValue("thirty")),
        ast.ObjectField("role", ast.EnumValue("USER")),
      ]),
      schema.Named("CreateUserInput"),
      test_schema(),
      dict.new(),
      ["input"],
    )

  case result {
    Error(input_coercion.TypeMismatch(path, "Int", "String")) -> {
      case path {
        ["input", "age"] -> Nil
        _ -> panic as "Expected path to include field name"
      }
    }
    _ -> panic as "Expected TypeMismatch error"
  }
}

pub fn coerce_input_object_optional_field_missing_test() {
  // "age" is optional, should work without it
  let result =
    input_coercion.coerce_argument_value(
      ast.ObjectValue([
        ast.ObjectField("name", ast.StringValue("Alice")),
        ast.ObjectField("role", ast.EnumValue("USER")),
      ]),
      schema.Named("CreateUserInput"),
      test_schema(),
      dict.new(),
      ["input"],
    )

  should.be_ok(result)
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn format_type_mismatch_error_test() {
  let error = input_coercion.TypeMismatch(["field", "nested"], "String", "Int")
  let message = input_coercion.format_error(error)

  should.equal(
    message,
    "Type mismatch at field.nested: expected String, got Int",
  )
}

pub fn format_invalid_enum_error_test() {
  let error =
    input_coercion.InvalidEnumValue(["input", "role"], "Role", "SUPERUSER")
  let message = input_coercion.format_error(error)

  should.equal(
    message,
    "Invalid enum value at input.role: 'SUPERUSER' is not a valid Role",
  )
}

pub fn format_missing_field_error_test() {
  let error = input_coercion.MissingRequiredField(["input"], "name")
  let message = input_coercion.format_error(error)

  should.equal(message, "Missing required field 'name' at input")
}

pub fn format_unknown_field_error_test() {
  let error = input_coercion.UnknownField(["input"], "email", "CreateUserInput")
  let message = input_coercion.format_error(error)

  should.equal(
    message,
    "Unknown field 'email' on input type CreateUserInput at input",
  )
}

pub fn format_null_not_allowed_error_test() {
  let error = input_coercion.NullNotAllowed(["field"])
  let message = input_coercion.format_error(error)

  should.equal(message, "Null value not allowed at field")
}

// ============================================================================
// coerce_arguments Tests
// ============================================================================

pub fn coerce_arguments_valid_test() {
  let arg_defs =
    dict.from_list([
      #(
        "name",
        schema.ArgumentDefinition(
          name: "name",
          description: None,
          arg_type: schema.NonNull(schema.Named("String")),
          default_value: None,
        ),
      ),
      #(
        "age",
        schema.ArgumentDefinition(
          name: "age",
          description: None,
          arg_type: schema.Named("Int"),
          default_value: None,
        ),
      ),
    ])

  let ast_args = [
    ast.Argument("name", ast.StringValue("Alice")),
    ast.Argument("age", ast.IntValue(30)),
  ]

  let result =
    input_coercion.coerce_arguments(
      ast_args,
      arg_defs,
      test_schema(),
      dict.new(),
      ["createUser"],
    )

  should.be_ok(result)
}

pub fn coerce_arguments_with_default_test() {
  let arg_defs =
    dict.from_list([
      #(
        "limit",
        schema.ArgumentDefinition(
          name: "limit",
          description: None,
          arg_type: schema.Named("Int"),
          default_value: Some(types.to_dynamic(10)),
        ),
      ),
    ])

  let ast_args = []

  let result =
    input_coercion.coerce_arguments(
      ast_args,
      arg_defs,
      test_schema(),
      dict.new(),
      ["users"],
    )

  should.be_ok(result)
}

pub fn coerce_arguments_missing_required_test() {
  let arg_defs =
    dict.from_list([
      #(
        "id",
        schema.ArgumentDefinition(
          name: "id",
          description: None,
          arg_type: schema.NonNull(schema.Named("ID")),
          default_value: None,
        ),
      ),
    ])

  let ast_args = []

  let result =
    input_coercion.coerce_arguments(
      ast_args,
      arg_defs,
      test_schema(),
      dict.new(),
      ["user"],
    )

  case result {
    Error(input_coercion.MissingRequiredField(_, "id")) -> Nil
    _ -> panic as "Expected MissingRequiredField error"
  }
}

import gleam/option.{None, Some}
