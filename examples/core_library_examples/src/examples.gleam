import mochi/schema

pub fn user_schema() -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("The unique identifier for the user"),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("The user's display name"),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.field_description("The user's email address"),
    )
    |> schema.field(
      schema.field_def("entries", schema.list_type(schema.named_type("Entry")))
      |> schema.field_description("User's blog entries"),
    )

  let entry_type =
    schema.object("Entry")
    |> schema.description("A blog entry")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("The unique identifier for the entry"),
    )
    |> schema.field(
      schema.field_def("title", schema.string_type())
      |> schema.field_description("The entry title"),
    )
    |> schema.field(
      schema.field_def("content", schema.string_type())
      |> schema.field_description("The entry content"),
    )
    |> schema.field(
      schema.field_def("author", schema.named_type("User"))
      |> schema.field_description("The entry author"),
    )

  let query_type =
    schema.object("Query")
    |> schema.description("The root query type")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user")
      |> schema.argument(
        schema.arg("id", schema.non_null(schema.id_type()))
        |> schema.arg_description("The ID of the user to fetch"),
      ),
    )
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users"),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
  |> schema.add_type(schema.ObjectTypeDef(entry_type))
  |> schema.add_type(schema.ScalarTypeDef(schema.string_scalar()))
  |> schema.add_type(schema.ScalarTypeDef(schema.int_scalar()))
  |> schema.add_type(schema.ScalarTypeDef(schema.id_scalar()))
}

// Example demonstrating more features
pub fn blog_schema() -> schema.Schema {
  let post_type =
    schema.object("Post")
    |> schema.description("A blog post")
    |> schema.field(schema.field_def("id", schema.non_null(schema.id_type())))
    |> schema.field(schema.field_def(
      "title",
      schema.non_null(schema.string_type()),
    ))
    |> schema.field(schema.field_def("content", schema.string_type()))
    |> schema.field(schema.field_def("published", schema.boolean_type()))
    |> schema.field(schema.field_def(
      "tags",
      schema.list_type(schema.string_type()),
    ))

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("post", schema.named_type("Post"))
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type()))),
    )
    |> schema.field(
      schema.field_def(
        "posts",
        schema.non_null(schema.list_type(schema.named_type("Post"))),
      )
      |> schema.argument(schema.arg("first", schema.int_type()))
      |> schema.argument(
        schema.arg("after", schema.string_type())
        |> schema.arg_description("Cursor for pagination"),
      ),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(post_type))
}
