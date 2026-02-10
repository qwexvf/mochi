// GeQL Snapshot Tests - Using Birdie for comprehensive testing
// Tests SDL parsing, schema building, and query structures

import birdie
import mochi/schema
import mochi/sdl_parser
import gleam/string
import gleeunit

pub fn main() {
  // Run a simple test to verify snapshots are working
  basic_sdl_parsing_test()
  enum_type_parsing_test()
  union_type_parsing_test()
  input_type_parsing_test()
  complex_schema_parsing_test()
  user_schema_structure_test()
  field_type_variations_test()
  sdl_parsing_error_test()
  malformed_union_error_test()

  // If we get here, all snapshots matched
  gleeunit.main()
}

// Test functions need to end with _test for gleeunit to find them

// SDL Parsing Snapshot Tests
pub fn basic_sdl_parsing_test() {
  let sdl =
    "
    scalar DateTime
    
    type User {
      id: ID!
      name: String
      email: String!
    }"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Basic SDL parsing with scalar and object types")
}

pub fn enum_type_parsing_test() {
  let sdl =
    "
    enum UserRole {
      ADMIN
      USER
      MODERATOR
    }"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Enum type SDL parsing")
}

pub fn union_type_parsing_test() {
  let sdl =
    "
    union SearchResult = User | Post | Comment"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Union type SDL parsing")
}

pub fn input_type_parsing_test() {
  let sdl =
    "
    input CreateUserInput {
      name: String!
      email: String!
      age: Int
    }"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Input type SDL parsing")
}

pub fn complex_schema_parsing_test() {
  let sdl =
    "
    type Query {
      user: User
      posts: [Post!]!
    }
    
    type User {
      id: ID!
      name: String!
      posts: [Post!]!
    }
    
    type Post {
      id: ID!
      title: String!
      author: User!
    }"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Complex schema with relationships")
}

// Schema Building Snapshot Tests
pub fn user_schema_structure_test() {
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("Unique identifier"),
    )
    |> schema.field(
      schema.field_def("name", schema.non_null(schema.string_type()))
      |> schema.field_description("User's full name"),
    )
    |> schema.field(
      schema.field_def("email", schema.non_null(schema.string_type()))
      |> schema.field_description("User's email address"),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.argument(
        schema.arg("id", schema.non_null(schema.id_type()))
        |> schema.arg_description("User ID"),
      ),
    )

  let complete_schema =
    schema.schema()
    |> schema.query(query_type)
    |> schema.add_type(schema.ObjectTypeDef(user_type))

  complete_schema
  |> string.inspect
  |> birdie.snap(title: "Complete User schema structure")
}

pub fn field_type_variations_test() {
  let test_type =
    schema.object("TestType")
    |> schema.field(schema.field_def("nullable_string", schema.string_type()))
    |> schema.field(schema.field_def(
      "non_null_string",
      schema.non_null(schema.string_type()),
    ))
    |> schema.field(schema.field_def(
      "list_of_strings",
      schema.list_type(schema.string_type()),
    ))
    |> schema.field(schema.field_def(
      "non_null_list_of_non_null_strings",
      schema.non_null(schema.list_type(schema.non_null(schema.string_type()))),
    ))

  test_type
  |> string.inspect
  |> birdie.snap(title: "Field type variations (nullable, non-null, lists)")
}

// Error Handling Snapshot Tests
pub fn sdl_parsing_error_test() {
  let invalid_sdl =
    "
    type User {
      id: ID!  
      name: String
      # Missing closing brace"

  sdl_parser.parse_sdl(invalid_sdl)
  |> string.inspect
  |> birdie.snap(title: "SDL parsing error handling")
}

pub fn malformed_union_error_test() {
  let invalid_sdl = "union SearchResult = | Post"

  sdl_parser.parse_sdl(invalid_sdl)
  |> string.inspect
  |> birdie.snap(title: "Malformed union type error")
}
