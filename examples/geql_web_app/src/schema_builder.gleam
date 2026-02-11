import database
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import gleam/result
import mochi/schema
import mochi/schema_gen

/// Application data types
pub type User {
  User(id: String, name: String, email: String, active: Bool)
}

pub type Post {
  Post(
    id: String,
    title: String,
    content: String,
    author_id: String,
    published: Bool,
  )
}

/// Create the complete GraphQL schema for the web application
pub fn create_schema() -> schema.Schema {
  // Define User type with database-backed resolvers
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("The user's unique identifier")
      |> schema.resolver(user_id_resolver),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("The user's display name")
      |> schema.resolver(user_name_resolver),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.field_description("The user's email address")
      |> schema.resolver(user_email_resolver),
    )
    |> schema.field(
      schema.field_def("active", schema.boolean_type())
      |> schema.field_description("Whether the user account is active")
      |> schema.resolver(user_active_resolver),
    )
    |> schema.field(
      schema.field_def("posts", schema.list_type(schema.named_type("Post")))
      |> schema.field_description("Posts authored by this user")
      |> schema.resolver(user_posts_resolver),
    )

  // Define Post type
  let post_type =
    schema.object("Post")
    |> schema.description("A blog post")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("The post's unique identifier")
      |> schema.resolver(post_id_resolver),
    )
    |> schema.field(
      schema.field_def("title", schema.string_type())
      |> schema.field_description("The post's title")
      |> schema.resolver(post_title_resolver),
    )
    |> schema.field(
      schema.field_def("content", schema.string_type())
      |> schema.field_description("The post's content")
      |> schema.resolver(post_content_resolver),
    )
    |> schema.field(
      schema.field_def("published", schema.boolean_type())
      |> schema.field_description("Whether the post is published")
      |> schema.resolver(post_published_resolver),
    )

  // Define Query type with database-backed resolvers
  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a specific user by ID")
      |> schema.argument(
        schema.arg("id", schema.non_null(schema.id_type()))
        |> schema.arg_description("The user's ID"),
      )
      |> schema.resolver(get_user_resolver),
    )
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users")
      |> schema.resolver(get_all_users_resolver),
    )

  // Build complete schema
  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
  |> schema.add_type(schema.ObjectTypeDef(post_type))
}

// Root resolvers (query entry points)

fn get_user_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case schema.get_argument(info.arguments, "id") {
    Ok(user_id_dynamic) -> {
      let user_id = extract_string_from_dynamic(user_id_dynamic)
      let #(sql, params) = database.get_user_query(user_id)

      case database.execute_query(sql, params) {
        Ok([user_data, ..]) -> Ok(user_data)
        Ok([]) -> Error("User not found")
        Error(err) -> Error("Database error: " <> err)
      }
    }
    Error(_) -> Error("Missing required argument: id")
  }
}

fn get_all_users_resolver(_info: schema.ResolverInfo) -> Result(Dynamic, String) {
  let #(sql, params) = database.get_all_users_query()

  case database.execute_query(sql, params) {
    Ok(users) -> Ok(dynamic.from(users))
    Error(err) -> Error("Database error: " <> err)
  }
}

// User field resolvers

fn user_id_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = extract_user_from_dynamic(parent_user)
      Ok(dynamic.from(user.id))
    }
    None -> Error("No parent user provided")
  }
}

fn user_name_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = extract_user_from_dynamic(parent_user)
      Ok(dynamic.from(user.name))
    }
    None -> Error("No parent user provided")
  }
}

fn user_email_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = extract_user_from_dynamic(parent_user)
      Ok(dynamic.from(user.email))
    }
    None -> Error("No parent user provided")
  }
}

fn user_active_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = extract_user_from_dynamic(parent_user)
      Ok(dynamic.from(user.active))
    }
    None -> Error("No parent user provided")
  }
}

fn user_posts_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = extract_user_from_dynamic(parent_user)
      let #(sql, params) = database.get_user_posts_query(user.id)

      case database.execute_query(sql, params) {
        Ok(posts) -> Ok(dynamic.from(posts))
        Error(err) -> Error("Database error: " <> err)
      }
    }
    None -> Error("No parent user provided")
  }
}

// Post field resolvers

fn post_id_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = extract_post_from_dynamic(parent_post)
      Ok(dynamic.from(post.id))
    }
    None -> Error("No parent post provided")
  }
}

fn post_title_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = extract_post_from_dynamic(parent_post)
      Ok(dynamic.from(post.title))
    }
    None -> Error("No parent post provided")
  }
}

fn post_content_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = extract_post_from_dynamic(parent_post)
      Ok(dynamic.from(post.content))
    }
    None -> Error("No parent post provided")
  }
}

fn post_published_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = extract_post_from_dynamic(parent_post)
      Ok(dynamic.from(post.published))
    }
    None -> Error("No parent post provided")
  }
}

// Helper functions for Dynamic conversion
// In a real app, you'd use gleam_json or a proper serialization library

fn extract_string_from_dynamic(value: Dynamic) -> String {
  // In practice: dynamic.string(value) |> result.unwrap("default")
  "extracted_string"
  // Simplified for demo
}

fn extract_user_from_dynamic(_user_dynamic: Dynamic) -> User {
  // In practice: decode using dynamic.decode4(User, ...)
  User(id: "1", name: "John Doe", email: "john@example.com", active: True)
}

fn extract_post_from_dynamic(_post_dynamic: Dynamic) -> Post {
  // In practice: decode using dynamic.decode5(Post, ...)
  Post(
    id: "1",
    title: "Example Post",
    content: "This is an example post.",
    author_id: "1",
    published: True,
  )
}
