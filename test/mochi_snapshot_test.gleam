// GeQL Snapshot Tests - Using Birdie for comprehensive testing
// Tests SDL parsing, schema building, query structures, and new features
//
// Note: All birdie snapshot tests are Erlang-only because:
// - string.inspect output format differs between targets
// - JSON key ordering differs between targets (dict iteration order)

@target(erlang)
import birdie
import gleam/dict
import gleam/string
import gleeunit
@target(erlang)
import mochi/error
@target(erlang)
import mochi/json
@target(erlang)
import mochi/response
@target(erlang)
import mochi/schema
@target(erlang)
import mochi/sdl_parser
@target(erlang)
import mochi/types

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SDL Parsing Snapshot Tests
// ============================================================================

@target(erlang)
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

@target(erlang)
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

@target(erlang)
pub fn union_type_parsing_test() {
  let sdl =
    "
    union SearchResult = User | Post | Comment"

  sdl_parser.parse_sdl(sdl)
  |> string.inspect
  |> birdie.snap(title: "Union type SDL parsing")
}

@target(erlang)
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

@target(erlang)
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

// ============================================================================
// Schema Building Snapshot Tests
// ============================================================================

@target(erlang)
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

@target(erlang)
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

// ============================================================================
// Error Handling Snapshot Tests
// ============================================================================

@target(erlang)
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

@target(erlang)
pub fn malformed_union_error_test() {
  let invalid_sdl = "union SearchResult = | Post"

  sdl_parser.parse_sdl(invalid_sdl)
  |> string.inspect
  |> birdie.snap(title: "Malformed union type error")
}

// ============================================================================
// JSON Serialization Snapshot Tests
// ============================================================================

@target(erlang)
pub fn json_object_encoding_test() {
  let data =
    dict.from_list([
      #("name", types.to_dynamic("Alice")),
      #("age", types.to_dynamic(30)),
      #("active", types.to_dynamic(True)),
    ])

  data
  |> types.to_dynamic
  |> json.encode_pretty(2)
  |> birdie.snap(title: "JSON object encoding with mixed types")
}

@target(erlang)
pub fn json_nested_encoding_test() {
  let inner =
    dict.from_list([
      #("city", types.to_dynamic("Tokyo")),
      #("country", types.to_dynamic("Japan")),
    ])

  let data =
    dict.from_list([
      #("user", types.to_dynamic("Bob")),
      #("address", types.to_dynamic(inner)),
      #("tags", types.to_dynamic(["dev", "gleam", "graphql"])),
    ])

  data
  |> types.to_dynamic
  |> json.encode_pretty(2)
  |> birdie.snap(title: "JSON nested object and array encoding")
}

// ============================================================================
// Error Extensions Snapshot Tests
// ============================================================================

@target(erlang)
pub fn error_with_extensions_test() {
  let err =
    error.error("Authentication required")
    |> error.with_code("UNAUTHENTICATED")
    |> error.with_category(error.AuthenticationErrorCategory)
    |> error.with_extension("retryAfter", types.to_dynamic(60))

  err
  |> error.to_dynamic
  |> json.encode_pretty(2)
  |> birdie.snap(title: "GraphQL error with extensions")
}

@target(erlang)
pub fn error_with_path_and_location_test() {
  let err =
    error.error("Field 'email' is not valid")
    |> error.at_location(10, 15)
    |> error.with_path([
      error.FieldSegment("query"),
      error.FieldSegment("users"),
      error.IndexSegment(0),
      error.FieldSegment("email"),
    ])
    |> error.with_code("VALIDATION_ERROR")

  err
  |> error.to_dynamic
  |> json.encode_pretty(2)
  |> birdie.snap(title: "GraphQL error with path and location")
}

// ============================================================================
// Response Serialization Snapshot Tests
// ============================================================================

@target(erlang)
pub fn response_success_test() {
  let data =
    dict.from_list([
      #(
        "user",
        types.to_dynamic(
          dict.from_list([
            #("id", types.to_dynamic("123")),
            #("name", types.to_dynamic("Alice")),
          ]),
        ),
      ),
    ])

  let resp =
    response.success(types.to_dynamic(data))
    |> response.with_extension("requestId", types.to_dynamic("req-abc-123"))

  resp
  |> response.to_json_pretty
  |> birdie.snap(title: "GraphQL success response with extensions")
}

@target(erlang)
pub fn response_with_errors_test() {
  let data = dict.from_list([#("user", types.to_dynamic(Nil))])

  let errors = [
    error.error("User not found")
    |> error.with_path([error.FieldSegment("query"), error.FieldSegment("user")])
    |> error.with_code("NOT_FOUND"),
  ]

  let resp = response.partial(types.to_dynamic(data), errors)

  resp
  |> response.to_json_pretty
  |> birdie.snap(title: "GraphQL partial response with errors")
}
