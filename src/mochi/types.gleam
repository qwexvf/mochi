//// Type definition helpers for Code First GraphQL.
////
//// This module provides builders for creating GraphQL types from Gleam types
//// with minimal boilerplate, plus helpers for Dynamic conversion.
////
//// ## Object Type Builder
////
//// ```gleam
//// let user_type = types.object("User")
////   |> types.description("A user in the system")
////   |> types.id("id", fn(u: User) { u.id })
////   |> types.string("name", fn(u: User) { u.name })
////   |> types.int("age", fn(u: User) { u.age })
////   |> types.build(decode_user)
//// ```
////
//// ## Enum Builder
////
//// ```gleam
//// let role_enum = types.enum_type("Role")
////   |> types.value("ADMIN")
////   |> types.value("USER")
////   |> types.deprecated_value_with_reason("GUEST", "Use USER instead")
////   |> types.build_enum
//// ```
////
//// ## Dynamic Conversion Helpers
////
//// Use these when building DataLoader encoders or custom resolvers:
////
//// ```gleam
//// fn user_to_dynamic(u: User) -> Dynamic {
////   types.record([
////     types.field("id", u.id),
////     types.field("name", u.name),
////     #("profile", profile_to_dynamic(u.profile)),
////     #("age", types.option(u.age)),  // Option(Int) -> null if None
////   ])
//// }
//// ```

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema.{
  type FieldDefinition, type FieldType, type ObjectType, type ResolverInfo,
}

// ============================================================================
// Helper to convert any value to Dynamic
// ============================================================================

/// Convert any Gleam value to Dynamic
/// This uses unsafe_coerce under the hood
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "./mochi_coerce_ffi.mjs", "identity")
pub fn to_dynamic(value: a) -> Dynamic

/// Build a Dynamic dict from a list of field tuples
///
/// This is a convenience helper for creating DataLoader encoders.
///
/// ## Example
///
/// ```gleam
/// fn user_to_dynamic(u: User) -> Dynamic {
///   types.record([
///     #("id", types.to_dynamic(u.id)),
///     #("name", types.to_dynamic(u.name)),
///     #("email", types.to_dynamic(u.email)),
///   ])
/// }
/// ```
pub fn record(fields: List(#(String, Dynamic))) -> Dynamic {
  to_dynamic(dict.from_list(fields))
}

/// Convert an Option to Dynamic (None becomes Nil/null)
///
/// ## Example
///
/// ```gleam
/// #("age", types.option(user.age))
/// ```
pub fn option(opt: Option(a)) -> Dynamic {
  case opt {
    Some(v) -> to_dynamic(v)
    None -> to_dynamic(Nil)
  }
}

/// Shorthand: wrap a value in to_dynamic with a field name
///
/// ## Example
///
/// ```gleam
/// types.record([
///   types.field("id", user.id),
///   types.field("name", user.name),
/// ])
/// ```
pub fn field(name: String, value: a) -> #(String, Dynamic) {
  #(name, to_dynamic(value))
}

// ============================================================================
// Type Builder
// ============================================================================

/// Builder for creating GraphQL object types
pub type TypeBuilder(a) {
  TypeBuilder(
    name: String,
    description: Option(String),
    fields: List(TypeField(a)),
  )
}

/// A field in the type builder
pub type TypeField(a) {
  TypeField(
    name: String,
    description: Option(String),
    field_type: FieldType,
    extractor: fn(a) -> Dynamic,
  )
}

/// Create a new type builder
pub fn object(name: String) -> TypeBuilder(a) {
  TypeBuilder(name: name, description: None, fields: [])
}

/// Add description to type
pub fn description(builder: TypeBuilder(a), desc: String) -> TypeBuilder(a) {
  TypeBuilder(..builder, description: Some(desc))
}

/// Add a string field
pub fn string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.string_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a string field with description
pub fn string_with_desc(
  builder: TypeBuilder(a),
  name: String,
  desc: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: Some(desc),
      field_type: schema.string_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add an ID field
pub fn id(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.non_null(schema.id_type()),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add an int field
pub fn int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Int,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.int_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add an int field with description
pub fn int_with_desc(
  builder: TypeBuilder(a),
  name: String,
  desc: String,
  extractor: fn(a) -> Int,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: Some(desc),
      field_type: schema.int_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a float field
pub fn float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Float,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.float_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a boolean field
pub fn bool(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Bool,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.boolean_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add an optional string field
pub fn optional_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.string_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add an optional int field
pub fn optional_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(Int),
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.int_type(),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a list of strings field
pub fn list_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(String),
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.list_type(schema.string_type()),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a list of ints field
pub fn list_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Int),
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.list_type(schema.int_type()),
      extractor: fn(a) { to_dynamic(extractor(a)) },
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a related object field
pub fn object_field(
  builder: TypeBuilder(a),
  name: String,
  type_name: String,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.named_type(type_name),
      extractor: extractor,
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a list of related objects field
pub fn list_object(
  builder: TypeBuilder(a),
  name: String,
  type_name: String,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.list_type(schema.named_type(type_name)),
      extractor: extractor,
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a non-null field
pub fn non_null_field(
  builder: TypeBuilder(a),
  name: String,
  field_type: FieldType,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  let field =
    TypeField(
      name: name,
      description: None,
      field_type: schema.non_null(field_type),
      extractor: extractor,
    )
  TypeBuilder(..builder, fields: [field, ..builder.fields])
}

// ============================================================================
// Build to Schema Types
// ============================================================================

/// Build the TypeBuilder into an ObjectType with a decoder
pub fn build(
  builder: TypeBuilder(a),
  decoder: fn(Dynamic) -> Result(a, String),
) -> ObjectType {
  let schema_fields =
    list.map(list.reverse(builder.fields), fn(f) { to_field_def(f, decoder) })

  let base_obj = schema.object(builder.name)

  let with_desc = case builder.description {
    Some(d) -> schema.description(base_obj, d)
    None -> base_obj
  }

  list.fold(schema_fields, with_desc, fn(obj, field) {
    schema.field(obj, field)
  })
}

fn to_field_def(
  f: TypeField(a),
  decoder: fn(Dynamic) -> Result(a, String),
) -> FieldDefinition {
  let resolver = fn(info: ResolverInfo) {
    case info.parent {
      Some(parent_dyn) ->
        case decoder(parent_dyn) {
          Ok(parent) -> Ok(f.extractor(parent))
          Error(e) -> Error(e)
        }
      None -> Error("No parent value")
    }
  }

  let base =
    schema.field_def(f.name, f.field_type)
    |> schema.resolver(resolver)

  case f.description {
    Some(desc) -> schema.field_description(base, desc)
    None -> base
  }
}

// ============================================================================
// Enum Builder
// ============================================================================

/// Builder for GraphQL enum types
pub type EnumBuilder {
  EnumBuilder(
    name: String,
    description: Option(String),
    values: List(EnumValue),
  )
}

/// An enum value
pub type EnumValue {
  EnumValue(
    name: String,
    description: Option(String),
    is_deprecated: Bool,
    deprecation_reason: Option(String),
  )
}

/// Create a new enum builder
pub fn enum_type(name: String) -> EnumBuilder {
  EnumBuilder(name: name, description: None, values: [])
}

/// Add description to enum
pub fn enum_description(builder: EnumBuilder, desc: String) -> EnumBuilder {
  EnumBuilder(..builder, description: Some(desc))
}

/// Add an enum value
pub fn value(builder: EnumBuilder, name: String) -> EnumBuilder {
  EnumBuilder(..builder, values: [
    EnumValue(name, None, False, None),
    ..builder.values
  ])
}

/// Add an enum value with description
pub fn value_with_desc(
  builder: EnumBuilder,
  name: String,
  desc: String,
) -> EnumBuilder {
  EnumBuilder(..builder, values: [
    EnumValue(name, Some(desc), False, None),
    ..builder.values
  ])
}

/// Add a deprecated enum value
pub fn deprecated_value(builder: EnumBuilder, name: String) -> EnumBuilder {
  EnumBuilder(..builder, values: [
    EnumValue(name, None, True, None),
    ..builder.values
  ])
}

/// Add a deprecated enum value with reason
pub fn deprecated_value_with_reason(
  builder: EnumBuilder,
  name: String,
  reason: String,
) -> EnumBuilder {
  EnumBuilder(..builder, values: [
    EnumValue(name, None, True, Some(reason)),
    ..builder.values
  ])
}

/// Build the enum type
pub fn build_enum(builder: EnumBuilder) -> schema.EnumType {
  let enum_values =
    list.fold(list.reverse(builder.values), dict.new(), fn(acc, v) {
      let value_def =
        schema.EnumValueDefinition(
          name: v.name,
          description: v.description,
          value: to_dynamic(v.name),
          is_deprecated: v.is_deprecated,
          deprecation_reason: v.deprecation_reason,
        )
      dict.insert(acc, v.name, value_def)
    })

  schema.EnumType(
    name: builder.name,
    description: builder.description,
    values: enum_values,
  )
}
