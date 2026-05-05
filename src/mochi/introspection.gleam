import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/output
import mochi/schema
import mochi/types

pub fn get_introspection_object_type(name: String) -> Option(schema.ObjectType) {
  case name {
    "__Schema" -> Some(get_introspection_schema_type())
    "__Type" -> Some(get_introspection_type_type())
    "__Field" -> Some(get_introspection_field_type())
    "__InputValue" -> Some(get_introspection_input_value_type())
    "__EnumValue" -> Some(get_introspection_enum_value_type())
    "__Directive" -> Some(get_introspection_directive_type())
    _ -> None
  }
}

pub fn get_introspection_schema_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__Schema",
    description: Some(
      "A GraphQL Schema defines the capabilities of a GraphQL server.",
    ),
    fields: dict.from_list([
      #("description", field_def("description", schema.Named("String"))),
      #(
        "types",
        field_def(
          "types",
          schema.NonNull(schema.List(schema.NonNull(schema.Named("__Type")))),
        ),
      ),
      #(
        "queryType",
        field_def("queryType", schema.NonNull(schema.Named("__Type"))),
      ),
      #("mutationType", field_def("mutationType", schema.Named("__Type"))),
      #(
        "subscriptionType",
        field_def("subscriptionType", schema.Named("__Type")),
      ),
      #(
        "directives",
        field_def(
          "directives",
          schema.NonNull(
            schema.List(schema.NonNull(schema.Named("__Directive"))),
          ),
        ),
      ),
    ]),
    interfaces: [],
  )
}

pub fn get_introspection_type_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__Type",
    description: Some("The fundamental unit of any GraphQL Schema is the type."),
    fields: dict.from_list([
      #("kind", field_def("kind", schema.NonNull(schema.Named("String")))),
      #("name", field_def("name", schema.Named("String"))),
      #("description", field_def("description", schema.Named("String"))),
      #("specifiedByURL", field_def("specifiedByURL", schema.Named("String"))),
      #(
        "fields",
        field_def_with_include_deprecated(
          "fields",
          "fields",
          "__fields_all",
          schema.List(schema.NonNull(schema.Named("__Field"))),
        ),
      ),
      #(
        "interfaces",
        field_def(
          "interfaces",
          schema.List(schema.NonNull(schema.Named("__Type"))),
        ),
      ),
      #(
        "possibleTypes",
        field_def(
          "possibleTypes",
          schema.List(schema.NonNull(schema.Named("__Type"))),
        ),
      ),
      #(
        "enumValues",
        field_def_with_include_deprecated(
          "enumValues",
          "enumValues",
          "__enumValues_all",
          schema.List(schema.NonNull(schema.Named("__EnumValue"))),
        ),
      ),
      #(
        "inputFields",
        field_def(
          "inputFields",
          schema.List(schema.NonNull(schema.Named("__InputValue"))),
        ),
      ),
      #("ofType", field_def("ofType", schema.Named("__Type"))),
    ]),
    interfaces: [],
  )
}

fn get_introspection_field_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__Field",
    description: Some(
      "Object and Interface types are described by a list of Fields.",
    ),
    fields: dict.from_list([
      #("name", field_def("name", schema.NonNull(schema.Named("String")))),
      #("description", field_def("description", schema.Named("String"))),
      #(
        "args",
        field_def(
          "args",
          schema.NonNull(
            schema.List(schema.NonNull(schema.Named("__InputValue"))),
          ),
        ),
      ),
      #("type", field_def("type", schema.NonNull(schema.Named("__Type")))),
      #(
        "isDeprecated",
        field_def("isDeprecated", schema.NonNull(schema.Named("Boolean"))),
      ),
      #(
        "deprecationReason",
        field_def("deprecationReason", schema.Named("String")),
      ),
    ]),
    interfaces: [],
  )
}

fn get_introspection_input_value_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__InputValue",
    description: Some("Arguments provided to Fields or Directives."),
    fields: dict.from_list([
      #("name", field_def("name", schema.NonNull(schema.Named("String")))),
      #("description", field_def("description", schema.Named("String"))),
      #("type", field_def("type", schema.NonNull(schema.Named("__Type")))),
      #("defaultValue", field_def("defaultValue", schema.Named("String"))),
      #("isDeprecated", field_def("isDeprecated", schema.Named("Boolean"))),
      #(
        "deprecationReason",
        field_def("deprecationReason", schema.Named("String")),
      ),
    ]),
    interfaces: [],
  )
}

fn get_introspection_enum_value_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__EnumValue",
    description: Some("One possible value for a given Enum."),
    fields: dict.from_list([
      #("name", field_def("name", schema.NonNull(schema.Named("String")))),
      #("description", field_def("description", schema.Named("String"))),
      #(
        "isDeprecated",
        field_def("isDeprecated", schema.NonNull(schema.Named("Boolean"))),
      ),
      #(
        "deprecationReason",
        field_def("deprecationReason", schema.Named("String")),
      ),
    ]),
    interfaces: [],
  )
}

fn get_introspection_directive_type() -> schema.ObjectType {
  schema.ObjectType(
    name: "__Directive",
    description: Some(
      "A Directive provides a way to describe alternate runtime execution.",
    ),
    fields: dict.from_list([
      #("name", field_def("name", schema.NonNull(schema.Named("String")))),
      #("description", field_def("description", schema.Named("String"))),
      #(
        "isRepeatable",
        field_def("isRepeatable", schema.NonNull(schema.Named("Boolean"))),
      ),
      #(
        "locations",
        field_def(
          "locations",
          schema.NonNull(schema.List(schema.NonNull(schema.Named("String")))),
        ),
      ),
      #(
        "args",
        field_def(
          "args",
          schema.NonNull(
            schema.List(schema.NonNull(schema.Named("__InputValue"))),
          ),
        ),
      ),
    ]),
    interfaces: [],
  )
}

fn field_def(
  name: String,
  field_type: schema.FieldType,
) -> schema.FieldDefinition {
  schema.FieldDefinition(
    name: name,
    description: None,
    field_type: field_type,
    arguments: dict.new(),
    resolver: None,
    is_deprecated: False,
    deprecation_reason: None,
    topic_fn: None,
    rich_resolver: None,
  )
}

fn field_def_with_include_deprecated(
  field_name: String,
  data_key: String,
  all_data_key: String,
  field_type: schema.FieldType,
) -> schema.FieldDefinition {
  schema.FieldDefinition(
    name: field_name,
    description: None,
    field_type: field_type,
    arguments: dict.from_list([
      #(
        "includeDeprecated",
        schema.ArgumentDefinition(
          name: "includeDeprecated",
          description: None,
          arg_type: schema.boolean_type(),
          default_value: Some(types.to_dynamic(False)),
        ),
      ),
    ]),
    resolver: Some(fn(info: schema.ResolverInfo) {
      let include_deprecated =
        dict.get(info.arguments, "includeDeprecated")
        |> result.try(fn(v) {
          decode.run(v, decode.bool) |> result.map_error(fn(_) { Nil })
        })
        |> result.unwrap(False)
      case info.parent {
        None -> Ok(types.to_dynamic(Nil))
        Some(parent) ->
          case include_deprecated {
            True ->
              decode.run(parent, decode.at([all_data_key], decode.dynamic))
              |> result.map_error(fn(_) { "Cannot read " <> all_data_key })
            False ->
              decode.run(parent, decode.at([data_key], decode.dynamic))
              |> result.map_error(fn(_) { "Cannot read " <> data_key })
          }
      }
    }),
    is_deprecated: False,
    deprecation_reason: None,
    topic_fn: None,
    rich_resolver: None,
  )
}

pub fn build_schema_introspection(schema_def: schema.Schema) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("queryType", build_type_ref(schema_def, schema_def.query)),
      #("mutationType", build_type_ref(schema_def, schema_def.mutation)),
      #("subscriptionType", build_type_ref(schema_def, schema_def.subscription)),
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

fn build_directives_introspection(schema_def: schema.Schema) -> Dynamic {
  let builtin_directives = [
    build_skip_directive_introspection(schema_def),
    build_include_directive_introspection(schema_def),
    build_deprecated_directive_introspection(schema_def),
    build_specified_by_directive_introspection(schema_def),
  ]

  let custom_directives =
    schema_def.directives
    |> dict.to_list
    |> list.map(fn(kv) {
      let #(_name, directive_def) = kv
      build_directive_introspection(schema_def, directive_def)
    })

  types.to_dynamic(list.append(builtin_directives, custom_directives))
}

fn build_directive_introspection(
  schema_def: schema.Schema,
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
          #(
            "type",
            build_field_type_introspection(schema_def, arg_def.arg_type),
          ),
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

fn build_builtin_directive(
  name name: String,
  description description: String,
  locations locations: List(String),
  args args: List(Dynamic),
) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic(name)),
      #("description", types.to_dynamic(description)),
      #("locations", types.to_dynamic(list.map(locations, types.to_dynamic))),
      #("args", types.to_dynamic(args)),
      #("isRepeatable", types.to_dynamic(False)),
    ]),
  )
}

fn make_bool_arg(
  schema_def: schema.Schema,
  name: String,
  description: String,
  default_value: Dynamic,
) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic(name)),
      #("description", types.to_dynamic(description)),
      #(
        "type",
        build_field_type_introspection(
          schema_def,
          schema.NonNull(schema.Named("Boolean")),
        ),
      ),
      #("defaultValue", default_value),
    ]),
  )
}

fn make_string_arg(
  schema_def: schema.Schema,
  name: String,
  description: String,
  required: Bool,
  default_value: Dynamic,
) -> Dynamic {
  let arg_type = case required {
    True -> schema.NonNull(schema.Named("String"))
    False -> schema.Named("String")
  }
  types.to_dynamic(
    dict.from_list([
      #("name", types.to_dynamic(name)),
      #("description", types.to_dynamic(description)),
      #("type", build_field_type_introspection(schema_def, arg_type)),
      #("defaultValue", default_value),
    ]),
  )
}

fn build_skip_directive_introspection(schema_def: schema.Schema) -> Dynamic {
  build_builtin_directive(
    name: "skip",
    description: "Directs the executor to skip this field or fragment when the `if` argument is true.",
    locations: ["FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"],
    args: [
      make_bool_arg(
        schema_def,
        "if",
        "Skipped when true.",
        types.to_dynamic(Nil),
      ),
    ],
  )
}

fn build_include_directive_introspection(schema_def: schema.Schema) -> Dynamic {
  build_builtin_directive(
    name: "include",
    description: "Directs the executor to include this field or fragment only when the `if` argument is true.",
    locations: ["FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"],
    args: [
      make_bool_arg(
        schema_def,
        "if",
        "Included when true.",
        types.to_dynamic(Nil),
      ),
    ],
  )
}

fn build_deprecated_directive_introspection(
  schema_def: schema.Schema,
) -> Dynamic {
  build_builtin_directive(
    name: "deprecated",
    description: "Marks an element of a GraphQL schema as no longer supported.",
    locations: [
      "FIELD_DEFINITION",
      "ARGUMENT_DEFINITION",
      "INPUT_FIELD_DEFINITION",
      "ENUM_VALUE",
    ],
    args: [
      make_string_arg(
        schema_def,
        "reason",
        "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).",
        False,
        types.to_dynamic("\"No longer supported\""),
      ),
    ],
  )
}

fn build_specified_by_directive_introspection(
  schema_def: schema.Schema,
) -> Dynamic {
  build_builtin_directive(
    name: "specifiedBy",
    description: "Exposes a URL that specifies the behavior of this scalar.",
    locations: ["SCALAR"],
    args: [
      make_string_arg(
        schema_def,
        "url",
        "The URL that specifies the behavior of this scalar.",
        True,
        types.to_dynamic(Nil),
      ),
    ],
  )
}

fn build_type_ref(
  schema_def: schema.Schema,
  obj: Option(schema.ObjectType),
) -> Dynamic {
  obj
  |> option.map(fn(o) { build_object_introspection(schema_def, o) })
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

pub fn build_type_introspection(
  schema_def: schema.Schema,
  name: String,
) -> Dynamic {
  case name {
    "String" | "Int" | "Float" | "Boolean" | "ID" ->
      build_scalar_introspection(name)
    _ -> lookup_type_introspection(schema_def, name)
  }
}

fn build_scalar_introspection(name: String) -> Dynamic {
  make_type_object(
    kind: "SCALAR",
    name: name,
    description: get_scalar_description(name),
    fields: None,
    fields_all: None,
    interfaces: None,
    enum_values: None,
    enum_values_all: None,
    input_fields: None,
    possible_types: None,
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
    Ok(type_def) -> build_type_def_introspection(schema_def, type_def)
    Error(_) -> lookup_root_type(schema_def, name)
  }
}

fn lookup_root_type(schema_def: schema.Schema, name: String) -> Dynamic {
  [schema_def.query, schema_def.mutation, schema_def.subscription]
  |> list.find(fn(opt) {
    option.map(opt, fn(o) { o.name == name }) |> option.unwrap(False)
  })
  |> result.map(fn(opt) {
    option.map(opt, build_object_introspection(schema_def, _))
    |> option.unwrap(types.to_dynamic(Nil))
  })
  |> result.unwrap(build_meta_type_introspection(name))
}

fn build_type_def_introspection(
  schema_def: schema.Schema,
  type_def: schema.TypeDefinition,
) -> Dynamic {
  case type_def {
    schema.ObjectTypeDef(obj) -> build_object_introspection(schema_def, obj)
    schema.ScalarTypeDef(scalar) ->
      make_type_object(
        kind: "SCALAR",
        name: scalar.name,
        description: option.unwrap(scalar.description, ""),
        fields: None,
        fields_all: None,
        interfaces: None,
        enum_values: None,
        enum_values_all: None,
        input_fields: None,
        possible_types: None,
      )
    schema.EnumTypeDef(enum) -> build_enum_introspection(enum)
    schema.InterfaceTypeDef(iface) ->
      build_interface_introspection(schema_def, iface)
    schema.UnionTypeDef(union) -> build_union_introspection(schema_def, union)
    schema.InputObjectTypeDef(input) ->
      build_input_introspection(schema_def, input)
  }
}

fn build_object_introspection(
  schema_def: schema.Schema,
  obj: schema.ObjectType,
) -> Dynamic {
  let fields = build_fields_introspection(schema_def, obj.fields, False)
  let fields_all = build_fields_introspection(schema_def, obj.fields, True)
  let interfaces =
    list.map(obj.interfaces, fn(i) {
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("INTERFACE")),
          #("name", types.to_dynamic(i.name)),
          #("ofType", types.to_dynamic(Nil)),
        ]),
      )
    })
  make_type_object(
    kind: "OBJECT",
    name: obj.name,
    description: option.unwrap(obj.description, ""),
    fields: Some(fields),
    fields_all: Some(fields_all),
    interfaces: Some(interfaces),
    enum_values: None,
    enum_values_all: None,
    input_fields: None,
    possible_types: None,
  )
}

fn build_enum_values(
  enum: schema.EnumType,
  include_deprecated: Bool,
) -> List(Dynamic) {
  dict.to_list(enum.values)
  |> list.filter(fn(kv) {
    let #(_, def) = kv
    include_deprecated || !def.is_deprecated
  })
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
}

fn build_enum_introspection(enum: schema.EnumType) -> Dynamic {
  make_type_object(
    kind: "ENUM",
    name: enum.name,
    description: option.unwrap(enum.description, ""),
    fields: None,
    fields_all: None,
    interfaces: None,
    enum_values: Some(build_enum_values(enum, False)),
    enum_values_all: Some(build_enum_values(enum, True)),
    input_fields: None,
    possible_types: None,
  )
}

fn build_interface_introspection(
  schema_def: schema.Schema,
  iface: schema.InterfaceType,
) -> Dynamic {
  let fields = build_fields_introspection(schema_def, iface.fields, False)
  let fields_all = build_fields_introspection(schema_def, iface.fields, True)
  let possible_types =
    dict.values(schema_def.types)
    |> list.filter_map(fn(type_def) {
      case type_def {
        schema.ObjectTypeDef(obj) ->
          case list.any(obj.interfaces, fn(i) { i.name == iface.name }) {
            True ->
              Ok(
                types.to_dynamic(
                  dict.from_list([
                    #("kind", types.to_dynamic("OBJECT")),
                    #("name", types.to_dynamic(obj.name)),
                    #("ofType", types.to_dynamic(Nil)),
                  ]),
                ),
              )
            False -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    })
  make_type_object(
    kind: "INTERFACE",
    name: iface.name,
    description: option.unwrap(iface.description, ""),
    fields: Some(fields),
    fields_all: Some(fields_all),
    interfaces: None,
    enum_values: None,
    enum_values_all: None,
    input_fields: None,
    possible_types: Some(possible_types),
  )
}

fn build_union_introspection(
  schema_def: schema.Schema,
  union: schema.UnionType,
) -> Dynamic {
  let possible =
    list.map(union.types, fn(t) {
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("OBJECT")),
          #("name", types.to_dynamic(t.name)),
          #("ofType", types.to_dynamic(Nil)),
        ]),
      )
    })
  let _ = schema_def
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

fn build_input_introspection(
  schema_def: schema.Schema,
  input: schema.InputObjectType,
) -> Dynamic {
  let fields =
    dict.to_list(input.fields)
    |> list.map(fn(kv) {
      let #(name, def) = kv
      types.to_dynamic(
        dict.from_list([
          #("name", types.to_dynamic(name)),
          #("description", types.to_dynamic(option.unwrap(def.description, ""))),
          #("type", build_field_type_introspection(schema_def, def.field_type)),
          #("defaultValue", serialize_default_value(def.default_value)),
        ]),
      )
    })
  make_type_object(
    kind: "INPUT_OBJECT",
    name: input.name,
    description: option.unwrap(input.description, ""),
    fields: None,
    fields_all: None,
    interfaces: None,
    enum_values: None,
    enum_values_all: None,
    input_fields: Some(fields),
    possible_types: None,
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
        kind: "OBJECT",
        name: name,
        description: "Introspection type",
        fields: Some([]),
        fields_all: Some([]),
        interfaces: Some([]),
        enum_values: None,
        enum_values_all: None,
        input_fields: None,
        possible_types: None,
      )
    "__TypeKind" | "__DirectiveLocation" ->
      make_type_object(
        kind: "ENUM",
        name: name,
        description: "Introspection enum",
        fields: None,
        fields_all: None,
        interfaces: None,
        enum_values: Some([]),
        enum_values_all: Some([]),
        input_fields: None,
        possible_types: None,
      )
    _ -> types.to_dynamic(Nil)
  }
}

fn make_type_object(
  kind kind: String,
  name name: String,
  description description: String,
  fields fields: Option(List(Dynamic)),
  fields_all fields_all: Option(List(Dynamic)),
  interfaces interfaces: Option(List(Dynamic)),
  enum_values enum_values: Option(List(Dynamic)),
  enum_values_all enum_values_all: Option(List(Dynamic)),
  input_fields input_fields: Option(List(Dynamic)),
  possible_types possible_types: Option(List(Dynamic)),
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
        "__fields_all",
        option.map(fields_all, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "interfaces",
        option.map(interfaces, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "possibleTypes",
        option.map(possible_types, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "enumValues",
        option.map(enum_values, types.to_dynamic)
          |> option.unwrap(types.to_dynamic(Nil)),
      ),
      #(
        "__enumValues_all",
        option.map(enum_values_all, types.to_dynamic)
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
  schema_def: schema.Schema,
  fields: Dict(String, schema.FieldDefinition),
  include_deprecated: Bool,
) -> List(Dynamic) {
  dict.to_list(fields)
  |> list.filter(fn(kv) {
    let #(_, def) = kv
    include_deprecated || !def.is_deprecated
  })
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
            #(
              "type",
              build_field_type_introspection(schema_def, arg_def.arg_type),
            ),
            #("defaultValue", serialize_default_value(arg_def.default_value)),
          ]),
        )
      })
    types.to_dynamic(
      dict.from_list([
        #("name", types.to_dynamic(name)),
        #("description", types.to_dynamic(option.unwrap(def.description, ""))),
        #("args", types.to_dynamic(args)),
        #("type", build_field_type_introspection(schema_def, def.field_type)),
        #("isDeprecated", types.to_dynamic(def.is_deprecated)),
        #("deprecationReason", case def.deprecation_reason {
          option.Some(reason) -> types.to_dynamic(reason)
          option.None -> types.to_dynamic(Nil)
        }),
      ]),
    )
  })
}

fn serialize_default_value(default_value: Option(Dynamic)) -> Dynamic {
  // GraphQL introspection serializes default values as their printed
  // representation: strings stay strings, ints/bools become their
  // textual form, anything else becomes null.
  case default_value {
    None -> types.to_dynamic(Nil)
    Some(v) ->
      case output.from_dynamic(v) {
        Ok(output.VString(s)) -> types.to_dynamic(s)
        Ok(output.VInt(i)) -> types.to_dynamic(int.to_string(i))
        Ok(output.VFloat(f)) -> types.to_dynamic(float.to_string(f))
        Ok(output.VBool(True)) -> types.to_dynamic("true")
        Ok(output.VBool(False)) -> types.to_dynamic("false")
        Ok(output.VNull) -> types.to_dynamic(Nil)
        // Lists/objects don't have a stable printed-form here — fall back
        // to null. Schema-level typing of defaults will fix this.
        Ok(_) -> types.to_dynamic(Nil)
        Error(_) -> types.to_dynamic(Nil)
      }
  }
}


pub fn build_field_type_introspection(
  schema_def: schema.Schema,
  field_type: schema.FieldType,
) -> Dynamic {
  case field_type {
    schema.NonNull(inner) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("NON_NULL")),
          #("name", types.to_dynamic(Nil)),
          #("ofType", build_field_type_introspection(schema_def, inner)),
        ]),
      )
    schema.List(inner) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic("LIST")),
          #("name", types.to_dynamic(Nil)),
          #("ofType", build_field_type_introspection(schema_def, inner)),
        ]),
      )
    schema.Named(name) ->
      types.to_dynamic(
        dict.from_list([
          #("kind", types.to_dynamic(get_type_kind(schema_def, name))),
          #("name", types.to_dynamic(name)),
          #("ofType", types.to_dynamic(Nil)),
        ]),
      )
  }
}

fn get_type_kind(schema_def: schema.Schema, name: String) -> String {
  case name {
    "String" | "Int" | "Float" | "Boolean" | "ID" -> "SCALAR"
    _ ->
      case dict.get(schema_def.types, name) {
        Ok(schema.ObjectTypeDef(_)) -> "OBJECT"
        Ok(schema.ScalarTypeDef(_)) -> "SCALAR"
        Ok(schema.EnumTypeDef(_)) -> "ENUM"
        Ok(schema.InterfaceTypeDef(_)) -> "INTERFACE"
        Ok(schema.UnionTypeDef(_)) -> "UNION"
        Ok(schema.InputObjectTypeDef(_)) -> "INPUT_OBJECT"
        Error(_) ->
          case
            [schema_def.query, schema_def.mutation, schema_def.subscription]
            |> list.find(fn(opt) {
              option.map(opt, fn(o) { o.name == name }) |> option.unwrap(False)
            })
          {
            Ok(_) -> "OBJECT"
            Error(_) -> "OBJECT"
          }
      }
  }
}
