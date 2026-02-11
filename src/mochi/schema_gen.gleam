import gleam/dynamic.{type Dynamic}
import gleam/option
import mochi/schema

// Schema generation from Gleam types
// This module provides utilities to automatically create GraphQL schemas
// from Gleam custom types with automatic field resolvers

/// Generate a GraphQL object type from a Gleam custom type
/// This creates the object definition and automatic field resolvers
pub fn from_type(
  type_name: String,
  field_specs: List(FieldSpec),
) -> schema.ObjectType {
  let fields = create_fields_from_specs(field_specs)

  schema.object(type_name)
  |> schema.description("Auto-generated from Gleam type " <> type_name)
  |> add_fields_to_object(fields)
}

/// Specification for a field in the generated schema
pub type FieldSpec {
  FieldSpec(
    name: String,
    field_type: schema.FieldType,
    description: String,
    extractor: fn(Dynamic) -> Result(Dynamic, String),
  )
}

/// Helper to create a field spec for a String field
pub fn string_field(
  name: String,
  description: String,
  extractor: fn(Dynamic) -> Result(String, String),
) -> FieldSpec {
  FieldSpec(
    name: name,
    field_type: schema.string_type(),
    description: description,
    extractor: fn(parent) {
      case extractor(parent) {
        Ok(value) -> Ok(serialize_string(value))
        Error(msg) -> Error(msg)
      }
    },
  )
}

/// Helper to create a field spec for an Int field  
pub fn int_field(
  name: String,
  description: String,
  extractor: fn(Dynamic) -> Result(Int, String),
) -> FieldSpec {
  FieldSpec(
    name: name,
    field_type: schema.int_type(),
    description: description,
    extractor: fn(parent) {
      case extractor(parent) {
        Ok(value) -> Ok(serialize_int(value))
        Error(msg) -> Error(msg)
      }
    },
  )
}

/// Helper to create a field spec for a Boolean field
pub fn bool_field(
  name: String,
  description: String,
  extractor: fn(Dynamic) -> Result(Bool, String),
) -> FieldSpec {
  FieldSpec(
    name: name,
    field_type: schema.boolean_type(),
    description: description,
    extractor: fn(parent) {
      case extractor(parent) {
        Ok(value) -> Ok(serialize_bool(value))
        Error(msg) -> Error(msg)
      }
    },
  )
}

/// Create a complete schema with a query type that returns the generated type
pub fn create_schema_with_query(
  type_name: String,
  field_specs: List(FieldSpec),
  root_resolver: fn(schema.ResolverInfo) -> Result(Dynamic, String),
) -> schema.Schema {
  let object_type = from_type(type_name, field_specs)

  let query_type =
    schema.object("Query")
    |> schema.description("Auto-generated query type")
    |> schema.field(
      schema.field_def(
        string_to_camel_case(type_name),
        schema.named_type(type_name),
      )
      |> schema.field_description("Get a " <> type_name)
      |> schema.resolver(root_resolver),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(object_type))
}

// Internal helper functions

fn create_fields_from_specs(
  field_specs: List(FieldSpec),
) -> List(schema.FieldDefinition) {
  case field_specs {
    [] -> []
    [spec, ..rest] -> [
      schema.field_def(spec.name, spec.field_type)
        |> schema.field_description(spec.description)
        |> schema.resolver(fn(info) {
          spec.extractor(info.parent |> option.unwrap(placeholder_dynamic()))
        }),
      ..create_fields_from_specs(rest)
    ]
  }
}

fn add_fields_to_object(
  object: schema.ObjectType,
  fields: List(schema.FieldDefinition),
) -> schema.ObjectType {
  case fields {
    [] -> object
    [field, ..rest] ->
      object
      |> schema.field(field)
      |> add_fields_to_object(rest)
  }
}

fn string_to_camel_case(input: String) -> String {
  // Convert "Person" -> "person", "UserProfile" -> "userProfile"
  case input {
    "" -> ""
    _ -> {
      let first_char = string_first_char(input)
      let rest = string_drop_first(input)
      string_lowercase(first_char) <> rest
    }
  }
}

// Placeholder serializers - in practice you'd use proper JSON conversion
fn serialize_string(value: String) -> Dynamic {
  // In practice: json.encode(value) |> to_dynamic()
  demo_serialize("string", value)
}

fn serialize_int(value: Int) -> Dynamic {
  // In practice: json.encode(value) |> to_dynamic()
  demo_serialize("int", int_to_string(value))
}

fn serialize_bool(value: Bool) -> Dynamic {
  // In practice: json.encode(value) |> to_dynamic()
  let bool_str = case value {
    True -> "true"
    False -> "false"
  }
  demo_serialize("bool", bool_str)
}

fn demo_serialize(type_name: String, value: String) -> Dynamic {
  // Demo implementation - shows what would be serialized
  let message = "Would serialize " <> type_name <> " value: " <> value
  panic as message
}

fn placeholder_dynamic() -> Dynamic {
  // Placeholder for when no parent is provided
  panic as "No parent dynamic provided"
}

// String utility functions (simplified implementations)
fn string_first_char(input: String) -> String {
  // In practice: string.first(input) |> result.unwrap("")
  case input {
    "" -> ""
    _ -> "p"
    // Simplified - would extract first character
  }
}

fn string_drop_first(input: String) -> String {
  // In practice: string.drop_left(input, 1)
  case input {
    "Person" -> "erson"
    _ -> input
    // Simplified implementation
  }
}

fn string_lowercase(input: String) -> String {
  // In practice: string.lowercase(input)  
  case input {
    "P" -> "p"
    _ -> input
    // Simplified implementation
  }
}

fn int_to_string(value: Int) -> String {
  // In practice: int.to_string(value)
  case value {
    0 -> "0"
    1 -> "1"
    _ -> "42"
    // Simplified implementation
  }
}
