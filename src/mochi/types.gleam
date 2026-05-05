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
import gleam/dynamic/decode as dynamic_decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/args as args_mod
import mochi/schema.{
  type ArgumentDefinition, type ExecutionContext, type FieldDefinition,
  type FieldType, type ObjectType, type ResolverInfo,
}

// ============================================================================
// Helper to convert any value to Dynamic
// ============================================================================

/// Convert any Gleam value to Dynamic
/// This uses unsafe_coerce under the hood
@external(erlang, "gleam_stdlib", "identity")
pub fn to_dynamic(value: a) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_coerce(value: Dynamic) -> a

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
  /// Simple field with extractor
  TypeField(
    name: String,
    description: Option(String),
    field_type: FieldType,
    extractor: fn(a) -> Dynamic,
    is_deprecated: Bool,
    deprecation_reason: Option(String),
  )
  /// Field with arguments and resolver
  TypeFieldWithArgs(
    name: String,
    description: Option(String),
    field_type: FieldType,
    args: List(ArgumentDefinition),
    resolver: fn(a, args_mod.Args, ExecutionContext) ->
      Result(Dynamic, String),
    is_deprecated: Bool,
    deprecation_reason: Option(String),
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

/// Mark the most recently added field as deprecated with a reason.
pub fn deprecated(builder: TypeBuilder(a), reason: String) -> TypeBuilder(a) {
  case builder.fields {
    [] -> builder
    [field, ..rest] ->
      TypeBuilder(..builder, fields: [
        mark_deprecated(field, True, Some(reason)),
        ..rest
      ])
  }
}

/// Mark the most recently added field as deprecated without a reason.
pub fn deprecated_no_reason(builder: TypeBuilder(a)) -> TypeBuilder(a) {
  case builder.fields {
    [] -> builder
    [field, ..rest] ->
      TypeBuilder(..builder, fields: [
        mark_deprecated(field, True, None),
        ..rest
      ])
  }
}

pub fn field_description(
  builder: TypeBuilder(a),
  desc: String,
) -> TypeBuilder(a) {
  case builder.fields {
    [] -> builder
    [f, ..rest] ->
      TypeBuilder(..builder, fields: [set_field_description(f, desc), ..rest])
  }
}

fn set_field_description(field: TypeField(a), desc: String) -> TypeField(a) {
  case field {
    TypeField(..) -> TypeField(..field, description: Some(desc))
    TypeFieldWithArgs(..) -> TypeFieldWithArgs(..field, description: Some(desc))
  }
}

fn mark_deprecated(
  field: TypeField(a),
  is_deprecated: Bool,
  reason: Option(String),
) -> TypeField(a) {
  case field {
    TypeField(..) ->
      TypeField(
        ..field,
        is_deprecated: is_deprecated,
        deprecation_reason: reason,
      )
    TypeFieldWithArgs(..) ->
      TypeFieldWithArgs(
        ..field,
        is_deprecated: is_deprecated,
        deprecation_reason: reason,
      )
  }
}

fn add_field(
  builder: TypeBuilder(a),
  name: String,
  description: Option(String),
  field_type: FieldType,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  TypeBuilder(..builder, fields: [
    TypeField(
      name: name,
      description: description,
      field_type: field_type,
      extractor: extractor,
      is_deprecated: False,
      deprecation_reason: None,
    ),
    ..builder.fields
  ])
}

/// Add a string field
pub fn string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.string_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add an ID field
pub fn id(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(schema.id_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a nullable ID field
pub fn optional_id(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> option.Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.id_type(), fn(a) {
    option(extractor(a))
  })
}

/// Add an int field
pub fn int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Int,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.int_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a float field
pub fn float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Float,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.float_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a boolean field
pub fn bool(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Bool,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.boolean_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

// ============================================================================
// Custom Scalar Field Helpers
// ============================================================================

pub fn datetime(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.datetime_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn optional_datetime(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.datetime_type(), fn(a) {
    option(extractor(a))
  })
}

pub fn date(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.date_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn optional_date(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.date_type(), fn(a) {
    option(extractor(a))
  })
}

pub fn uuid(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.uuid_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn optional_uuid(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.uuid_type(), fn(a) {
    option(extractor(a))
  })
}

pub fn email(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.email_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn optional_email(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.email_type(), fn(a) {
    option(extractor(a))
  })
}

pub fn url(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.url_type(), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn optional_url(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.url_type(), fn(a) {
    option(extractor(a))
  })
}

pub fn json(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.json_type(), extractor)
}

pub fn optional_json(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(Dynamic),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.json_type(), fn(a) {
    option(extractor(a))
  })
}

/// Add an optional string field. `None` becomes JSON `null`; `Some(v)` becomes
/// the string value. Use this for nullable GraphQL fields.
pub fn optional_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.string_type(), fn(a) {
    option(extractor(a))
  })
}

/// Add an optional int field. `None` becomes JSON `null`; `Some(v)` becomes
/// the int value.
pub fn optional_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(Int),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.int_type(), fn(a) {
    option(extractor(a))
  })
}

/// Add an optional float field. `None` becomes JSON `null`; `Some(v)` becomes
/// the float value.
pub fn optional_float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(Float),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.float_type(), fn(a) {
    option(extractor(a))
  })
}

/// Add an optional bool field. `None` becomes JSON `null`; `Some(v)` becomes
/// the bool value.
pub fn optional_bool(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Option(Bool),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.boolean_type(), fn(a) {
    option(extractor(a))
  })
}

// ============================================================================
// Decoder helpers for round-trip safety
// ============================================================================
//
// `types.option(None)` encodes as Erlang `nil`, which is what mochi's executor
// re-parses when calling field extractors via `build(decoder)`. The standard
// library's `decode.string` rejects nil, so a typed Gleam record with
// `Option(String)` fields can't be decoded back without help.
//
// `nullable` wraps an inner decoder and returns `None` when the value is nil,
// otherwise runs the inner decoder. Use it inside your `build` decoder for
// every field that was emitted via `option()` or `optional_*`.

/// A decoder that returns `None` when the value is nil/null and otherwise runs
/// the inner decoder.
///
/// ## Example
///
/// ```gleam
/// use email <- decode.optional_field("email", None, types.nullable(decode.string))
/// ```
pub fn nullable(
  inner: dynamic_decode.Decoder(a),
) -> dynamic_decode.Decoder(Option(a)) {
  dynamic_decode.optional(inner)
}

/// Add a list of strings field
pub fn list_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.list_type(schema.string_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a list of ints field
pub fn list_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Int),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.list_type(schema.int_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn list_float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Float),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.list_type(schema.float_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn list_bool(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Bool),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.list_type(schema.boolean_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn list_id(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(String),
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.list_type(schema.id_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

pub fn non_null_list_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(String),
) -> TypeBuilder(a) {
  add_field(
    builder,
    name,
    None,
    schema.non_null(schema.list_type(schema.string_type())),
    fn(a) { to_dynamic(extractor(a)) },
  )
}

pub fn non_null_list_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Int),
) -> TypeBuilder(a) {
  add_field(
    builder,
    name,
    None,
    schema.non_null(schema.list_type(schema.int_type())),
    fn(a) { to_dynamic(extractor(a)) },
  )
}

pub fn non_null_list_float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> List(Float),
) -> TypeBuilder(a) {
  add_field(
    builder,
    name,
    None,
    schema.non_null(schema.list_type(schema.float_type())),
    fn(a) { to_dynamic(extractor(a)) },
  )
}

/// Add a related object field
pub fn object_field(
  builder: TypeBuilder(a),
  name: String,
  type_name: String,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.named_type(type_name), extractor)
}

/// Add a list of related objects field
pub fn list_object(
  builder: TypeBuilder(a),
  name: String,
  type_name: String,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  add_field(
    builder,
    name,
    None,
    schema.list_type(schema.named_type(type_name)),
    extractor,
  )
}

/// Add a non-null field
pub fn non_null_field(
  builder: TypeBuilder(a),
  name: String,
  field_type: FieldType,
  extractor: fn(a) -> Dynamic,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(field_type), extractor)
}

// ============================================================================
// Non-null Field Convenience Helpers
// ============================================================================

/// Add a non-null string field
pub fn non_null_string(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> String,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(schema.string_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a non-null int field
pub fn non_null_int(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Int,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(schema.int_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a non-null float field
pub fn non_null_float(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Float,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(schema.float_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

/// Add a non-null bool field
pub fn non_null_bool(
  builder: TypeBuilder(a),
  name: String,
  extractor: fn(a) -> Bool,
) -> TypeBuilder(a) {
  add_field(builder, name, None, schema.non_null(schema.boolean_type()), fn(a) {
    to_dynamic(extractor(a))
  })
}

// ============================================================================
// Fields with Arguments
// ============================================================================

/// Add a field with arguments and custom resolver
///
/// ```gleam
/// types.object("User")
///   |> types.field_with_args(
///     name: "posts",
///     returns: schema.list_type(schema.named_type("Post")),
///     args: [schema.arg("limit", schema.int_type())],
///     desc: "User's posts with optional limit",
///     resolve: fn(user, args, ctx) {
///       let limit = query.get_optional_int(args, "limit")
///       get_user_posts(user.id, limit)
///     },
///   )
/// ```
pub fn field_with_args(
  builder: TypeBuilder(a),
  name name: String,
  returns field_type: FieldType,
  args args: List(ArgumentDefinition),
  desc description: String,
  resolve resolver: fn(a, args_mod.Args, ExecutionContext) ->
    Result(Dynamic, String),
) -> TypeBuilder(a) {
  let field =
    TypeFieldWithArgs(
      name: name,
      description: Some(description),
      field_type: field_type,
      args: args,
      resolver: resolver,
      is_deprecated: False,
      deprecation_reason: None,
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

/// Build the TypeBuilder into an ObjectType and auto-generated encoder
///
/// This is the recommended way to build types - it generates both the
/// schema type and an encoder function from the same field definitions,
/// eliminating redundant code.
///
/// ## Example
///
/// ```gleam
/// pub type User {
///   User(id: String, name: String, age: Int)
/// }
///
/// let #(user_type, user_encoder) = types.object("User")
///   |> types.id("id", fn(u: User) { u.id })
///   |> types.string("name", fn(u: User) { u.name })
///   |> types.int("age", fn(u: User) { u.age })
///   |> types.build_with_encoder(decode_user)
///
/// // Use user_type in schema, user_encoder in resolvers
/// query.query(
///   name: "user",
///   returns: schema.named_type("User"),
///   resolve: fn(_ctx) { Ok(User("1", "Alice", 30)) },
///   encode: user_encoder,  // Auto-generated!
/// )
/// ```
pub fn build_with_encoder(
  builder: TypeBuilder(a),
  decoder: fn(Dynamic) -> Result(a, String),
) -> #(ObjectType, fn(a) -> Dynamic) {
  let object_type = build(builder, decoder)
  let encoder_fn = encoder(builder)
  #(object_type, encoder_fn)
}

/// Generate an encoder function from a TypeBuilder
///
/// The encoder uses the same extractors defined for each field,
/// so you don't need to write the same field mappings twice.
///
/// ## Example
///
/// ```gleam
/// let user_builder = types.object("User")
///   |> types.id("id", fn(u: User) { u.id })
///   |> types.string("name", fn(u: User) { u.name })
///
/// let user_type = types.build(user_builder, decode_user)
/// let user_encoder = types.encoder(user_builder)
///
/// // user_encoder(User("1", "Alice")) produces:
/// // {"id": "1", "name": "Alice"}
/// ```
/// Build the TypeBuilder into an ObjectType using identity coerce instead of a
/// decoder. This eliminates the encode/decode roundtrip — no Dict is built per
/// object and no Dict lookups are done per field. The encoder is just
/// `to_dynamic` (a BEAM no-op).
///
/// Safe as long as the resolver returns values of type `a` and the extractors
/// are defined for the same type `a`. Do not use when resolvers return
/// pre-encoded Dicts.
pub fn build_direct(builder: TypeBuilder(a)) -> #(ObjectType, fn(a) -> Dynamic) {
  let schema_fields =
    list.map(list.reverse(builder.fields), fn(f) { to_field_def_direct(f) })

  let base_obj = schema.object(builder.name)

  let with_desc = case builder.description {
    Some(d) -> schema.description(base_obj, d)
    None -> base_obj
  }

  let object_type =
    list.fold(schema_fields, with_desc, fn(obj, field) {
      schema.field(obj, field)
    })

  #(object_type, to_dynamic)
}

fn apply_deprecation(
  field: FieldDefinition,
  is_deprecated: Bool,
  deprecation_reason: Option(String),
) -> FieldDefinition {
  case is_deprecated {
    False -> field
    True ->
      case deprecation_reason {
        Some(reason) -> schema.deprecated(field, reason)
        None -> schema.deprecated_no_reason(field)
      }
  }
}

fn to_field_def_direct(f: TypeField(a)) -> FieldDefinition {
  case f {
    TypeField(
      name,
      description,
      field_type,
      extractor,
      is_deprecated,
      deprecation_reason,
    ) -> {
      let resolver = fn(info: ResolverInfo) {
        case info.parent {
          Some(parent_dyn) -> Ok(extractor(unsafe_coerce(parent_dyn)))
          None -> Error("No parent value")
        }
      }
      let base =
        schema.field_def(name, field_type)
        |> schema.resolver(resolver)
      let with_desc = case description {
        Some(desc) -> schema.field_description(base, desc)
        None -> base
      }
      apply_deprecation(with_desc, is_deprecated, deprecation_reason)
    }

    TypeFieldWithArgs(
      name,
      description,
      field_type,
      args,
      field_resolver,
      is_deprecated,
      deprecation_reason,
    ) -> {
      let resolver = fn(info: ResolverInfo) {
        case info.parent {
          Some(parent_dyn) ->
            field_resolver(
              unsafe_coerce(parent_dyn),
              info.args,
              info.context,
            )
          None -> Error("No parent value")
        }
      }
      let base =
        schema.field_def(name, field_type)
        |> schema.resolver(resolver)
      let with_args =
        list.fold(args, base, fn(field, arg) { schema.argument(field, arg) })
      let with_desc = case description {
        Some(desc) -> schema.field_description(with_args, desc)
        None -> with_args
      }
      apply_deprecation(with_desc, is_deprecated, deprecation_reason)
    }
  }
}

pub fn encoder(builder: TypeBuilder(a)) -> fn(a) -> Dynamic {
  fn(value: a) -> Dynamic {
    let field_pairs =
      list.filter_map(builder.fields, fn(f) {
        case f {
          TypeField(name, _, _, extractor, _, _) ->
            Ok(#(name, extractor(value)))
          TypeFieldWithArgs(_, _, _, _, _, _, _) -> Error(Nil)
        }
      })
    to_dynamic(dict.from_list(field_pairs))
  }
}

fn to_field_def(
  f: TypeField(a),
  decoder: fn(Dynamic) -> Result(a, String),
) -> FieldDefinition {
  case f {
    TypeField(
      name,
      description,
      field_type,
      extractor,
      is_deprecated,
      deprecation_reason,
    ) -> {
      let resolver = fn(info: ResolverInfo) {
        case info.parent {
          Some(parent_dyn) -> result.map(decoder(parent_dyn), extractor)
          None -> Error("No parent value")
        }
      }

      let base =
        schema.field_def(name, field_type)
        |> schema.resolver(resolver)

      let with_desc = case description {
        Some(desc) -> schema.field_description(base, desc)
        None -> base
      }
      apply_deprecation(with_desc, is_deprecated, deprecation_reason)
    }

    TypeFieldWithArgs(
      name,
      description,
      field_type,
      args,
      field_resolver,
      is_deprecated,
      deprecation_reason,
    ) -> {
      let resolver = fn(info: ResolverInfo) {
        case info.parent {
          Some(parent_dyn) ->
            result.try(decoder(parent_dyn), fn(parent) {
              field_resolver(parent, info.args, info.context)
            })
          None -> Error("No parent value")
        }
      }

      let base =
        schema.field_def(name, field_type)
        |> schema.resolver(resolver)

      let with_args =
        list.fold(args, base, fn(field, arg) { schema.argument(field, arg) })

      let with_desc = case description {
        Some(desc) -> schema.field_description(with_args, desc)
        None -> with_args
      }
      apply_deprecation(with_desc, is_deprecated, deprecation_reason)
    }
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

// ============================================================================
// Enum Coercion Helpers
// ============================================================================

/// A mapping between a Gleam value and its GraphQL enum representation
pub type EnumMapping(a) {
  EnumMapping(gleam_value: a, graphql_name: String, description: Option(String))
}

/// Create an enum mapping
pub fn enum_mapping(gleam_value: a, graphql_name: String) -> EnumMapping(a) {
  EnumMapping(
    gleam_value: gleam_value,
    graphql_name: graphql_name,
    description: None,
  )
}

/// Create an enum mapping with description
pub fn enum_mapping_with_desc(
  gleam_value: a,
  graphql_name: String,
  desc: String,
) -> EnumMapping(a) {
  EnumMapping(
    gleam_value: gleam_value,
    graphql_name: graphql_name,
    description: Some(desc),
  )
}

/// Build an enum type from mappings with bidirectional coercion functions
///
/// Returns a tuple of:
/// - The EnumType for schema registration
/// - A function to convert Gleam value to GraphQL string
/// - A function to convert GraphQL string to Gleam value
///
/// ```gleam
/// pub type Status {
///   Active
///   Inactive
///   Pending
/// }
///
/// let #(enum_type, to_graphql, from_graphql) = types.enum_from_mappings(
///   "Status",
///   [
///     types.enum_mapping(Active, "ACTIVE"),
///     types.enum_mapping(Inactive, "INACTIVE"),
///     types.enum_mapping_with_desc(Pending, "PENDING", "Awaiting approval"),
///   ],
/// )
///
/// // Use in schema
/// query.new()
///   |> query.add_enum(enum_type)
///
/// // Convert values
/// to_graphql(Active) // -> "ACTIVE"
/// from_graphql("ACTIVE") // -> Ok(Active)
/// ```
pub fn enum_from_mappings(
  name: String,
  mappings: List(EnumMapping(a)),
) -> #(schema.EnumType, fn(a) -> String, fn(String) -> Result(a, String)) {
  let enum_values =
    list.fold(mappings, dict.new(), fn(acc, m) {
      let value_def =
        schema.EnumValueDefinition(
          name: m.graphql_name,
          description: m.description,
          value: to_dynamic(m.graphql_name),
          is_deprecated: False,
          deprecation_reason: None,
        )
      dict.insert(acc, m.graphql_name, value_def)
    })

  let enum_type =
    schema.EnumType(name: name, description: None, values: enum_values)

  let to_graphql = fn(value: a) -> String {
    case
      list.find(mappings, fn(m) {
        // dynamic comparison — no Eq constraint on type param
        to_dynamic(m.gleam_value) == to_dynamic(value)
      })
    {
      Ok(m) -> m.graphql_name
      Error(_) -> ""
    }
  }

  let from_graphql = fn(gql_name: String) -> Result(a, String) {
    case list.find(mappings, fn(m) { m.graphql_name == gql_name }) {
      Ok(m) -> Ok(m.gleam_value)
      Error(_) -> Error("Unknown enum value: " <> gql_name)
    }
  }

  #(enum_type, to_graphql, from_graphql)
}

/// Build an enum type from mappings with description
pub fn enum_from_mappings_with_desc(
  name: String,
  description: String,
  mappings: List(EnumMapping(a)),
) -> #(schema.EnumType, fn(a) -> String, fn(String) -> Result(a, String)) {
  let #(enum_type, to_graphql, from_graphql) =
    enum_from_mappings(name, mappings)
  let enum_type_with_desc =
    schema.EnumType(..enum_type, description: Some(description))
  #(enum_type_with_desc, to_graphql, from_graphql)
}

// ============================================================================
// Input Type Builder
// ============================================================================

/// Builder for GraphQL input object types
pub type InputBuilder {
  InputBuilder(
    name: String,
    description: Option(String),
    fields: List(InputField),
  )
}

/// A field in the input builder
pub type InputField {
  InputField(
    name: String,
    description: Option(String),
    field_type: FieldType,
    default_value: Option(Dynamic),
  )
}

/// Create a new input type builder
///
/// ```gleam
/// let create_user_input = types.input("CreateUserInput")
///   |> types.input_description("Input for creating a new user")
///   |> types.input_string("name", "User's full name")
///   |> types.input_string("email", "User's email address")
///   |> types.input_optional_int("age", "User's age")
///   |> types.build_input
/// ```
pub fn input(name: String) -> InputBuilder {
  InputBuilder(name: name, description: None, fields: [])
}

/// Add description to input type
pub fn input_description(builder: InputBuilder, desc: String) -> InputBuilder {
  InputBuilder(..builder, description: Some(desc))
}

/// Add a required string field to input
pub fn input_string(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.non_null(schema.string_type()), desc)
}

/// Add a required int field to input
pub fn input_int(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.non_null(schema.int_type()), desc)
}

/// Add a required float field to input
pub fn input_float(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.non_null(schema.float_type()), desc)
}

/// Add a required bool field to input
pub fn input_bool(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.non_null(schema.boolean_type()), desc)
}

/// Add a required ID field to input
pub fn input_id(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.non_null(schema.id_type()), desc)
}

/// Add an optional string field to input
pub fn input_optional_string(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.string_type(), desc)
}

/// Add an optional int field to input
pub fn input_optional_int(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.int_type(), desc)
}

/// Add an optional float field to input
pub fn input_optional_float(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.float_type(), desc)
}

/// Add an optional bool field to input
pub fn input_optional_bool(
  builder: InputBuilder,
  name: String,
  desc: String,
) -> InputBuilder {
  input_field(builder, name, schema.boolean_type(), desc)
}

/// Add a field with custom type to input
pub fn input_field(
  builder: InputBuilder,
  name: String,
  field_type: FieldType,
  desc: String,
) -> InputBuilder {
  let field =
    InputField(
      name: name,
      description: Some(desc),
      field_type: field_type,
      default_value: None,
    )
  InputBuilder(..builder, fields: [field, ..builder.fields])
}

/// Add a field with custom type and default value to input
pub fn input_field_with_default(
  builder: InputBuilder,
  name: String,
  field_type: FieldType,
  default: Dynamic,
  desc: String,
) -> InputBuilder {
  let field =
    InputField(
      name: name,
      description: Some(desc),
      field_type: field_type,
      default_value: Some(default),
    )
  InputBuilder(..builder, fields: [field, ..builder.fields])
}

/// Build the input type into a schema InputObjectType
pub fn build_input(builder: InputBuilder) -> schema.InputObjectType {
  let schema_fields =
    list.fold(list.reverse(builder.fields), dict.new(), fn(acc, f) {
      let field_def =
        schema.InputFieldDefinition(
          name: f.name,
          description: f.description,
          field_type: f.field_type,
          default_value: f.default_value,
        )
      dict.insert(acc, f.name, field_def)
    })

  schema.InputObjectType(
    name: builder.name,
    description: builder.description,
    fields: schema_fields,
  )
}
