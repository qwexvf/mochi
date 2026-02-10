// Schema Definition Language AST
// This defines the structure for parsing GraphQL SDL (Schema Definition Language)

import gleam/option.{type Option}

/// SDL Document containing type definitions
pub type SDLDocument {
  SDLDocument(definitions: List(TypeSystemDefinition))
}

/// Top-level type system definitions
pub type TypeSystemDefinition {
  TypeDefinition(type_def: TypeDef)
  DirectiveDefinition(directive_def: DirectiveDef)
  SchemaDefinition(schema_def: SchemaDef)
}

/// Type definitions (the main SDL constructs)
pub type TypeDef {
  ObjectTypeDefinition(object_def: ObjectTypeDef)
  InterfaceTypeDefinition(interface_def: InterfaceTypeDef)
  UnionTypeDefinition(union_def: UnionTypeDef)
  ScalarTypeDefinition(scalar_def: ScalarTypeDef)
  EnumTypeDefinition(enum_def: EnumTypeDef)
  InputObjectTypeDefinition(input_def: InputObjectTypeDef)
}

/// Object type definition: type User { ... }
pub type ObjectTypeDef {
  ObjectTypeDef(
    name: String,
    description: Option(String),
    interfaces: List(String),
    directives: List(DirectiveUsage),
    fields: List(FieldDef),
  )
}

/// Field definition within a type
pub type FieldDef {
  FieldDef(
    name: String,
    description: Option(String),
    arguments: List(ArgumentDef),
    field_type: SDLType,
    directives: List(DirectiveUsage),
  )
}

/// Argument definition for fields
pub type ArgumentDef {
  ArgumentDef(
    name: String,
    description: Option(String),
    arg_type: SDLType,
    default_value: Option(SDLValue),
    directives: List(DirectiveUsage),
  )
}

/// SDL Type system (with modifiers)
pub type SDLType {
  NamedType(name: String)
  ListType(inner_type: SDLType)
  NonNullType(inner_type: SDLType)
}

/// SDL Values (for default values, etc.)
pub type SDLValue {
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)
  BooleanValue(value: Bool)
  NullValue
  EnumValue(value: String)
  ListValue(values: List(SDLValue))
  ObjectValue(fields: List(ObjectFieldValue))
}

pub type ObjectFieldValue {
  ObjectFieldValue(name: String, value: SDLValue)
}

/// Interface type definition
pub type InterfaceTypeDef {
  InterfaceTypeDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
    fields: List(FieldDef),
  )
}

/// Union type definition
pub type UnionTypeDef {
  UnionTypeDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
    member_types: List(String),
  )
}

/// Scalar type definition
pub type ScalarTypeDef {
  ScalarTypeDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
  )
}

/// Enum type definition
pub type EnumTypeDef {
  EnumTypeDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
    values: List(EnumValueDef),
  )
}

pub type EnumValueDef {
  EnumValueDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
  )
}

/// Input object type definition
pub type InputObjectTypeDef {
  InputObjectTypeDef(
    name: String,
    description: Option(String),
    directives: List(DirectiveUsage),
    fields: List(InputFieldDef),
  )
}

pub type InputFieldDef {
  InputFieldDef(
    name: String,
    description: Option(String),
    field_type: SDLType,
    default_value: Option(SDLValue),
    directives: List(DirectiveUsage),
  )
}

/// Directive definition
pub type DirectiveDef {
  DirectiveDef(
    name: String,
    description: Option(String),
    locations: List(DirectiveLocation),
    arguments: List(ArgumentDef),
  )
}

/// Directive usage in SDL
pub type DirectiveUsage {
  DirectiveUsage(name: String, arguments: List(DirectiveArgument))
}

pub type DirectiveArgument {
  DirectiveArgument(name: String, value: SDLValue)
}

/// Where directives can be used
pub type DirectiveLocation {
  // Executable locations
  QUERY
  MUTATION
  SUBSCRIPTION
  FIELD
  FRAGMENTDEFINITION
  FRAGMENTSPREAD
  INLINEFRAGMENT
  // Type system locations
  SCHEMA
  SCALAR
  OBJECT
  FIELDDEFINITION
  ARGUMENTDEFINITION
  INTERFACE
  UNION
  ENUM
  ENUMVALUE
  INPUTOBJECT
  INPUTFIELDDEFINITION
}

/// Schema definition (optional root types)
pub type SchemaDef {
  SchemaDef(
    description: Option(String),
    directives: List(DirectiveUsage),
    query: Option(String),
    mutation: Option(String),
    subscription: Option(String),
  )
}

/// Helper functions for working with SDL AST
/// Get the name of a type definition
pub fn get_type_name(type_def: TypeDef) -> String {
  case type_def {
    ObjectTypeDefinition(obj) -> obj.name
    InterfaceTypeDefinition(iface) -> iface.name
    UnionTypeDefinition(union) -> union.name
    ScalarTypeDefinition(scalar) -> scalar.name
    EnumTypeDefinition(enum) -> enum.name
    InputObjectTypeDefinition(input) -> input.name
  }
}

/// Convert SDL type to string representation
pub fn sdl_type_to_string(sdl_type: SDLType) -> String {
  case sdl_type {
    NamedType(name) -> name
    ListType(inner) -> "[" <> sdl_type_to_string(inner) <> "]"
    NonNullType(inner) -> sdl_type_to_string(inner) <> "!"
  }
}

/// Check if a type is non-null
pub fn is_non_null_type(sdl_type: SDLType) -> Bool {
  case sdl_type {
    NonNullType(_) -> True
    _ -> False
  }
}

/// Check if a type is a list
pub fn is_list_type(sdl_type: SDLType) -> Bool {
  case sdl_type {
    ListType(_) -> True
    NonNullType(ListType(_)) -> True
    _ -> False
  }
}

/// Get the base type (unwrap NonNull and List wrappers)
pub fn get_base_type(sdl_type: SDLType) -> String {
  case sdl_type {
    NamedType(name) -> name
    ListType(inner) -> get_base_type(inner)
    NonNullType(inner) -> get_base_type(inner)
  }
}
