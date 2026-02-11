// Schema Printer - Converts GraphQL schemas back to SDL format
// This allows you to serialize schemas, inspect them, and generate documentation

import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import mochi/schema

/// Print a schema to SDL format
pub fn print_schema(schema_def: schema.Schema) -> String {
  let parts = []

  // Print schema definition if needed
  let parts = case has_custom_root_types(schema_def) {
    True -> [print_schema_definition(schema_def), ..parts]
    False -> parts
  }

  // Print all type definitions
  let type_parts =
    dict.to_list(schema_def.types)
    |> list.map(fn(entry) { print_type_definition(entry.1) })
    |> list.filter(fn(s) { s != "" })

  let all_parts = list.append(parts, type_parts)

  string.join(all_parts, "\n\n")
}

/// Print individual type definition
pub fn print_type_definition(type_def: schema.TypeDefinition) -> String {
  case type_def {
    schema.ObjectTypeDef(obj) -> print_object_type(obj)
    schema.ScalarTypeDef(scalar) -> print_scalar_type(scalar)
    schema.EnumTypeDef(enum) -> print_enum_type(enum)
    schema.InterfaceTypeDef(interface) -> print_interface_type(interface)
    schema.UnionTypeDef(union) -> print_union_type(union)
    schema.InputObjectTypeDef(input) -> print_input_object_type(input)
  }
}

/// Print object type: type User { ... }
fn print_object_type(obj: schema.ObjectType) -> String {
  let header = "type " <> obj.name

  let header = case obj.description {
    Some(desc) -> print_description(desc) <> "\n" <> header
    None -> header
  }

  let header = case list.length(obj.interfaces) {
    0 -> header
    _ ->
      header
      <> " implements "
      <> string.join(list.map(obj.interfaces, fn(i) { i.name }), " & ")
  }

  let fields =
    dict.to_list(obj.fields)
    |> list.map(fn(entry) { print_field_definition(entry.1, "  ") })
    |> string.join("\n")

  case fields {
    "" -> header
    _ -> header <> " {\n" <> fields <> "\n}"
  }
}

/// Print scalar type: scalar DateTime
fn print_scalar_type(scalar: schema.ScalarType) -> String {
  let result = "scalar " <> scalar.name
  case scalar.description {
    Some(desc) -> print_description(desc) <> "\n" <> result
    None -> result
  }
}

/// Print enum type: enum Status { ACTIVE INACTIVE }
fn print_enum_type(enum: schema.EnumType) -> String {
  let header = "enum " <> enum.name

  let header = case enum.description {
    Some(desc) -> print_description(desc) <> "\n" <> header
    None -> header
  }

  let values =
    dict.to_list(enum.values)
    |> list.map(fn(entry) { print_enum_value(entry.1, "  ") })
    |> string.join("\n")

  case values {
    "" -> header
    _ -> header <> " {\n" <> values <> "\n}"
  }
}

/// Print interface type: interface Node { ... }
fn print_interface_type(interface: schema.InterfaceType) -> String {
  let header = "interface " <> interface.name

  let header = case interface.description {
    Some(desc) -> print_description(desc) <> "\n" <> header
    None -> header
  }

  let fields =
    dict.to_list(interface.fields)
    |> list.map(fn(entry) { print_field_definition(entry.1, "  ") })
    |> string.join("\n")

  case fields {
    "" -> header
    _ -> header <> " {\n" <> fields <> "\n}"
  }
}

/// Print union type: union SearchResult = User | Post
fn print_union_type(union: schema.UnionType) -> String {
  let header = "union " <> union.name

  let header = case union.description {
    Some(desc) -> print_description(desc) <> "\n" <> header
    None -> header
  }

  let members =
    list.map(union.types, fn(t) { t.name })
    |> string.join(" | ")

  case members {
    "" -> header
    _ -> header <> " = " <> members
  }
}

/// Print input object type: input CreateUserInput { ... }
fn print_input_object_type(input: schema.InputObjectType) -> String {
  let header = "input " <> input.name

  let header = case input.description {
    Some(desc) -> print_description(desc) <> "\n" <> header
    None -> header
  }

  let fields =
    dict.to_list(input.fields)
    |> list.map(fn(entry) { print_input_field_definition(entry.1, "  ") })
    |> string.join("\n")

  case fields {
    "" -> header
    _ -> header <> " {\n" <> fields <> "\n}"
  }
}

/// Print field definition within a type
fn print_field_definition(
  field: schema.FieldDefinition,
  indent: String,
) -> String {
  let base = indent <> field.name

  // Add arguments if any
  let base = case dict.size(field.arguments) {
    0 -> base
    _ -> {
      let args =
        dict.to_list(field.arguments)
        |> list.map(fn(entry) { print_argument_definition(entry.1) })
        |> string.join(", ")
      base <> "(" <> args <> ")"
    }
  }

  // Add type
  let base = base <> ": " <> print_field_type(field.field_type)

  // Add description if any
  case field.description {
    Some(desc) -> print_description_inline(desc) <> "\n" <> base
    None -> base
  }
}

/// Print input field definition
fn print_input_field_definition(
  field: schema.InputFieldDefinition,
  indent: String,
) -> String {
  let base = indent <> field.name <> ": " <> print_field_type(field.field_type)

  // Add default value if any
  let base = case field.default_value {
    Some(_) -> base <> " = " <> "defaultValue"
    // TODO: Proper value printing
    None -> base
  }

  case field.description {
    Some(desc) -> print_description_inline(desc) <> "\n" <> base
    None -> base
  }
}

/// Print argument definition
fn print_argument_definition(arg: schema.ArgumentDefinition) -> String {
  let base = arg.name <> ": " <> print_field_type(arg.arg_type)

  case arg.default_value {
    Some(_) -> base <> " = " <> "defaultValue"
    // TODO: Proper value printing
    None -> base
  }
}

/// Print enum value
fn print_enum_value(
  enum_value: schema.EnumValueDefinition,
  indent: String,
) -> String {
  let base = indent <> enum_value.name

  case enum_value.description {
    Some(desc) -> print_description_inline(desc) <> "\n" <> base
    None -> base
  }
}

/// Print field type (handles NonNull, List, Named)
fn print_field_type(field_type: schema.FieldType) -> String {
  case field_type {
    schema.Named(name) -> name
    schema.NonNull(inner) -> print_field_type(inner) <> "!"
    schema.List(inner) -> "[" <> print_field_type(inner) <> "]"
  }
}

/// Print schema definition if it has custom root types
fn print_schema_definition(schema_def: schema.Schema) -> String {
  let parts = []

  let parts = case schema_def.query {
    Some(query) if query.name != "Query" -> ["query: " <> query.name, ..parts]
    _ -> parts
  }

  let parts = case schema_def.mutation {
    Some(mutation) if mutation.name != "Mutation" -> [
      "mutation: " <> mutation.name,
      ..parts
    ]
    _ -> parts
  }

  let parts = case schema_def.subscription {
    Some(subscription) if subscription.name != "Subscription" -> [
      "subscription: " <> subscription.name,
      ..parts
    ]
    _ -> parts
  }

  case parts {
    [] -> ""
    _ -> "schema {\n  " <> string.join(list.reverse(parts), "\n  ") <> "\n}"
  }
}

/// Check if schema has custom root type names
fn has_custom_root_types(schema_def: schema.Schema) -> Bool {
  let query_custom = case schema_def.query {
    Some(query) -> query.name != "Query"
    None -> False
  }

  let mutation_custom = case schema_def.mutation {
    Some(mutation) -> mutation.name != "Mutation"
    None -> False
  }

  let subscription_custom = case schema_def.subscription {
    Some(subscription) -> subscription.name != "Subscription"
    None -> False
  }

  query_custom || mutation_custom || subscription_custom
}

/// Print description as a comment
fn print_description(desc: String) -> String {
  "\"\"\"" <> desc <> "\"\"\""
}

/// Print description inline
fn print_description_inline(desc: String) -> String {
  "# " <> desc
}

/// Demo function to show schema printing capabilities
pub fn demo_schema_printing() -> Nil {
  io.println("=== GeQL Schema Printer Demo ===")
  io.println("")

  // Create a sample schema
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("Unique identifier"),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("User's full name"),
    )
    |> schema.field(
      schema.field_def("email", schema.non_null(schema.string_type()))
      |> schema.field_description("User's email address"),
    )

  let post_type =
    schema.object("Post")
    |> schema.description("A blog post")
    |> schema.field(schema.field_def("id", schema.non_null(schema.id_type())))
    |> schema.field(schema.field_def(
      "title",
      schema.non_null(schema.string_type()),
    ))
    |> schema.field(schema.field_def("content", schema.string_type()))

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type()))),
    )
    |> schema.field(schema.field_def(
      "posts",
      schema.list_type(schema.non_null(schema.named_type("Post"))),
    ))

  let complete_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))
    |> schema.add_type(schema.ObjectTypeDef(post_type))

  io.println("📄 Generated SDL:")
  io.println("=================")
  io.println(print_schema(complete_schema))

  io.println("")
  io.println("✨ Schema printing complete!")
}
