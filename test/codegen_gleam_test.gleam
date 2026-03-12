// Tests for mochi/codegen/gleam.gleam - Gleam code generation from GraphQL SDL
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import mochi/codegen/gleam as codegen
import mochi/sdl_ast

// ============================================================================
// Test Helpers
// ============================================================================

fn make_field(name: String, field_type: sdl_ast.SDLType) -> sdl_ast.FieldDef {
  sdl_ast.FieldDef(
    name: name,
    description: None,
    arguments: [],
    field_type: field_type,
    directives: [],
  )
}

fn make_field_with_desc(
  name: String,
  field_type: sdl_ast.SDLType,
  desc: String,
) -> sdl_ast.FieldDef {
  sdl_ast.FieldDef(
    name: name,
    description: Some(desc),
    arguments: [],
    field_type: field_type,
    directives: [],
  )
}

fn make_object(name: String, fields: List(sdl_ast.FieldDef)) -> sdl_ast.ObjectTypeDef {
  sdl_ast.ObjectTypeDef(
    name: name,
    description: None,
    interfaces: [],
    directives: [],
    fields: fields,
  )
}

fn make_document(definitions: List(sdl_ast.TypeSystemDefinition)) -> sdl_ast.SDLDocument {
  sdl_ast.SDLDocument(definitions: definitions)
}

fn contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}

// ============================================================================
// Configuration Tests
// ============================================================================

pub fn default_config_test() {
  let config = codegen.default_config()
  should.equal(config.types_module, "schema_types")
  should.equal(config.resolvers_module, "resolvers")
  should.equal(config.generate_resolvers, True)
  should.equal(config.generate_docs, True)
}

pub fn custom_config_test() {
  let config = codegen.GleamGenConfig(
    types_module: "my_types",
    resolvers_module: "my_resolvers",
    generate_resolvers: False,
    generate_docs: False,
  )
  should.equal(config.types_module, "my_types")
  should.equal(config.generate_resolvers, False)
}

// ============================================================================
// Generate Types Tests - Object Types
// ============================================================================

pub fn generate_simple_object_type_test() {
  let user_type = make_object("User", [
    make_field("id", sdl_ast.NonNullType(sdl_ast.NamedType("ID"))),
    make_field("name", sdl_ast.NonNullType(sdl_ast.NamedType("String"))),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type User"))
  should.be_true(contains(output, "id: String"))
  should.be_true(contains(output, "name: String"))
}

pub fn generate_object_with_list_field_test() {
  let user_type = make_object("User", [
    make_field("tags", sdl_ast.ListType(sdl_ast.NamedType("String"))),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "tags: List(String)"))
}

pub fn generate_object_with_description_test() {
  let user_type = sdl_ast.ObjectTypeDef(
    name: "User",
    description: Some("A user in the system"),
    interfaces: [],
    directives: [],
    fields: [make_field("id", sdl_ast.NamedType("ID"))],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "/// A user in the system"))
}

// ============================================================================
// Generate Types Tests - Enum Types
// ============================================================================

pub fn generate_enum_type_test() {
  let status_enum = sdl_ast.EnumTypeDef(
    name: "Status",
    description: None,
    directives: [],
    values: [
      sdl_ast.EnumValueDef(name: "ACTIVE", description: None, directives: []),
      sdl_ast.EnumValueDef(name: "INACTIVE", description: None, directives: []),
      sdl_ast.EnumValueDef(name: "PENDING", description: None, directives: []),
    ],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.EnumTypeDefinition(status_enum)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type Status"))
  should.be_true(contains(output, "Active"))
  should.be_true(contains(output, "Inactive"))
  should.be_true(contains(output, "Pending"))
}

pub fn generate_enum_with_description_test() {
  let status_enum = sdl_ast.EnumTypeDef(
    name: "Status",
    description: Some("User account status"),
    directives: [],
    values: [
      sdl_ast.EnumValueDef(
        name: "ACTIVE",
        description: Some("Account is active"),
        directives: [],
      ),
    ],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.EnumTypeDefinition(status_enum)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "/// User account status"))
  should.be_true(contains(output, "/// Account is active"))
}

// ============================================================================
// Generate Types Tests - Input Types
// ============================================================================

pub fn generate_input_type_test() {
  let input_type = sdl_ast.InputObjectTypeDef(
    name: "CreateUserInput",
    description: None,
    directives: [],
    fields: [
      sdl_ast.InputFieldDef(
        name: "name",
        description: None,
        field_type: sdl_ast.NonNullType(sdl_ast.NamedType("String")),
        default_value: None,
        directives: [],
      ),
      sdl_ast.InputFieldDef(
        name: "email",
        description: None,
        field_type: sdl_ast.NonNullType(sdl_ast.NamedType("String")),
        default_value: None,
        directives: [],
      ),
    ],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.InputObjectTypeDefinition(input_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type CreateUserInput"))
  should.be_true(contains(output, "name: String"))
  should.be_true(contains(output, "email: String"))
}

// ============================================================================
// Generate Types Tests - Union Types
// ============================================================================

pub fn generate_union_type_test() {
  let search_result = sdl_ast.UnionTypeDef(
    name: "SearchResult",
    description: None,
    directives: [],
    member_types: ["User", "Post", "Comment"],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.UnionTypeDefinition(search_result)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type SearchResult"))
  should.be_true(contains(output, "SearchResultUser"))
  should.be_true(contains(output, "SearchResultPost"))
  should.be_true(contains(output, "SearchResultComment"))
}

// ============================================================================
// Generate Types Tests - Scalar Types
// ============================================================================

pub fn generate_scalar_type_test() {
  let datetime_scalar = sdl_ast.ScalarTypeDef(
    name: "DateTime",
    description: Some("ISO 8601 date time"),
    directives: [],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ScalarTypeDefinition(datetime_scalar)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type DateTime"))
  should.be_true(contains(output, "/// ISO 8601 date time"))
}

// ============================================================================
// Generate Types Tests - Interface Types
// ============================================================================

pub fn generate_interface_type_test() {
  let node_interface = sdl_ast.InterfaceTypeDef(
    name: "Node",
    description: Some("An object with an ID"),
    directives: [],
    fields: [make_field("id", sdl_ast.NonNullType(sdl_ast.NamedType("ID")))],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.InterfaceTypeDefinition(node_interface)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "pub type Node"))
  should.be_true(contains(output, "/// An object with an ID"))
}

// ============================================================================
// Generate Resolvers Tests
// ============================================================================

pub fn generate_query_resolvers_test() {
  let query_type = sdl_ast.ObjectTypeDef(
    name: "Query",
    description: None,
    interfaces: [],
    directives: [],
    fields: [
      make_field("users", sdl_ast.ListType(sdl_ast.NamedType("User"))),
      make_field("user", sdl_ast.NamedType("User")),
    ],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(query_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_resolvers(doc, config)

  should.be_true(contains(output, "// Query resolvers"))
  should.be_true(contains(output, "pub fn resolve_users"))
  should.be_true(contains(output, "pub fn resolve_user"))
  should.be_true(contains(output, "ctx: ExecutionContext"))
}

pub fn generate_mutation_resolvers_test() {
  let mutation_type = sdl_ast.ObjectTypeDef(
    name: "Mutation",
    description: None,
    interfaces: [],
    directives: [],
    fields: [
      sdl_ast.FieldDef(
        name: "createUser",
        description: Some("Create a new user"),
        arguments: [
          sdl_ast.ArgumentDef(
            name: "name",
            description: None,
            arg_type: sdl_ast.NonNullType(sdl_ast.NamedType("String")),
            default_value: None,
            directives: [],
          ),
        ],
        field_type: sdl_ast.NamedType("User"),
        directives: [],
      ),
    ],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(mutation_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_resolvers(doc, config)

  should.be_true(contains(output, "// Mutation resolvers"))
  should.be_true(contains(output, "pub fn resolve_create_user"))
  should.be_true(contains(output, "name: String"))
  should.be_true(contains(output, "/// Create a new user"))
}

pub fn generate_resolvers_with_todo_test() {
  let query_type = make_object("Query", [
    make_field("hello", sdl_ast.NamedType("String")),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(query_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_resolvers(doc, config)

  should.be_true(contains(output, "// TODO: Implement resolver"))
  should.be_true(contains(output, "Error(\"Not implemented"))
}

pub fn generate_resolvers_imports_types_module_test() {
  let query_type = make_object("Query", [
    make_field("hello", sdl_ast.NamedType("String")),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(query_type)),
  ])

  let config = codegen.GleamGenConfig(
    types_module: "my_schema_types",
    resolvers_module: "resolvers",
    generate_resolvers: True,
    generate_docs: True,
  )
  let output = codegen.generate_resolvers(doc, config)

  should.be_true(contains(output, "import my_schema_types"))
}

// ============================================================================
// Type Conversion Tests
// ============================================================================

pub fn generate_scalar_types_conversion_test() {
  let types_obj = make_object("TestTypes", [
    make_field("str", sdl_ast.NamedType("String")),
    make_field("num", sdl_ast.NamedType("Int")),
    make_field("dec", sdl_ast.NamedType("Float")),
    make_field("flag", sdl_ast.NamedType("Boolean")),
    make_field("identifier", sdl_ast.NamedType("ID")),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(types_obj)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "str: String"))
  should.be_true(contains(output, "num: Int"))
  should.be_true(contains(output, "dec: Float"))
  should.be_true(contains(output, "flag: Bool"))
  should.be_true(contains(output, "identifier: String"))
}

pub fn generate_nested_list_type_test() {
  let matrix_obj = make_object("Matrix", [
    make_field("rows", sdl_ast.ListType(sdl_ast.ListType(sdl_ast.NamedType("Int")))),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(matrix_obj)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "rows: List(List(Int))"))
}

// ============================================================================
// Header and Imports Tests
// ============================================================================

pub fn generate_types_header_test() {
  let doc = make_document([])
  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "// Generated by mochi - DO NOT EDIT"))
  should.be_true(contains(output, "import gleam/option"))
}

pub fn generate_resolvers_header_test() {
  let doc = make_document([])
  let config = codegen.default_config()
  let output = codegen.generate_resolvers(doc, config)

  should.be_true(contains(output, "// Generated by mochi - DO NOT EDIT"))
  should.be_true(contains(output, "import gleam/dynamic"))
  should.be_true(contains(output, "import mochi/schema"))
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn generate_empty_document_test() {
  let doc = make_document([])
  let config = codegen.default_config()

  let types_output = codegen.generate_types(doc, config)
  let resolvers_output = codegen.generate_resolvers(doc, config)

  // Should still have headers
  should.be_true(contains(types_output, "// Generated by mochi"))
  should.be_true(contains(resolvers_output, "// Generated by mochi"))
}

pub fn generate_without_docs_test() {
  let user_type = sdl_ast.ObjectTypeDef(
    name: "User",
    description: Some("This description should be hidden"),
    interfaces: [],
    directives: [],
    fields: [make_field("id", sdl_ast.NamedType("ID"))],
  )

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.GleamGenConfig(
    types_module: "types",
    resolvers_module: "resolvers",
    generate_resolvers: True,
    generate_docs: False,
  )
  let output = codegen.generate_types(doc, config)

  // Should NOT contain the description as a doc comment
  should.be_false(contains(output, "/// This description should be hidden"))
}

pub fn generate_camel_case_to_snake_case_test() {
  let user_type = make_object("User", [
    make_field("firstName", sdl_ast.NamedType("String")),
    make_field("lastName", sdl_ast.NamedType("String")),
    make_field("emailAddress", sdl_ast.NamedType("String")),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_types(doc, config)

  should.be_true(contains(output, "first_name: String"))
  should.be_true(contains(output, "last_name: String"))
  should.be_true(contains(output, "email_address: String"))
}

pub fn non_root_objects_skip_resolvers_test() {
  // Regular object types (not Query/Mutation/Subscription) should not generate resolvers
  let user_type = make_object("User", [
    make_field("id", sdl_ast.NamedType("ID")),
    make_field("name", sdl_ast.NamedType("String")),
  ])

  let doc = make_document([
    sdl_ast.TypeDefinition(sdl_ast.ObjectTypeDefinition(user_type)),
  ])

  let config = codegen.default_config()
  let output = codegen.generate_resolvers(doc, config)

  // Should NOT have User resolvers
  should.be_false(contains(output, "resolve_id"))
  should.be_false(contains(output, "resolve_name"))
  should.be_false(contains(output, "// User resolvers"))
}
