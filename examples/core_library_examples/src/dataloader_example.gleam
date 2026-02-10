/// DataLoader example demonstrating batching and caching for efficient data fetching
/// This solves the N+1 query problem common in GraphQL implementations
import mochi/dataloader
import mochi/executor
import mochi/schema
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}

// Example data types
pub type User {
  User(id: String, name: String, email: String)
}

pub type Post {
  Post(id: String, title: String, author_id: String, content: String)
}

// Mock database
const users = [
  User("1", "Alice Johnson", "alice@example.com"),
  User("2", "Bob Smith", "bob@example.com"),
  User("3", "Carol Davis", "carol@example.com"),
]

const posts = [
  Post(
    "1",
    "Getting Started with GraphQL",
    "1",
    "GraphQL is a query language...",
  ),
  Post("2", "Advanced Gleam Patterns", "1", "Gleam offers powerful features..."),
  Post("3", "Database Design Tips", "2", "When designing schemas..."),
  Post("4", "Web Development Best Practices", "3", "Modern web development..."),
  Post(
    "5",
    "Functional Programming Benefits",
    "2",
    "Functional programming offers...",
  ),
]

/// Batch load function for users - simulates database query
fn batch_load_users(
  user_ids: List(Dynamic),
) -> Result(List(Result(Dynamic, String)), String) {
  io.println(
    "🔄 BATCH LOADING USERS: "
    <> int.to_string(list.length(user_ids))
    <> " users",
  )

  // Convert Dynamic user IDs to strings (in practice, use dynamic.string)
  let string_ids = list.map(user_ids, fn(_) { "mock_id" })

  // Find users by ID (simulating database lookup)
  let results =
    list.map(string_ids, fn(id) {
      case list.find(users, fn(user) { user.id == id }) {
        Ok(user) -> Ok(user_to_dynamic(user))
        Error(_) -> Error("User not found: " <> id)
      }
    })

  Ok(results)
}

/// Batch load function for posts by author ID - simulates database query
fn batch_load_posts_by_author(
  author_ids: List(Dynamic),
) -> Result(List(Result(Dynamic, String)), String) {
  io.println(
    "🔄 BATCH LOADING POSTS: "
    <> int.to_string(list.length(author_ids))
    <> " authors",
  )

  // Convert Dynamic author IDs to strings
  let string_ids = list.map(author_ids, fn(_) { "mock_author_id" })

  // Find posts by author ID (simulating database lookup)
  let results =
    list.map(string_ids, fn(author_id) {
      let author_posts =
        list.filter(posts, fn(post) { post.author_id == author_id })
      Ok(posts_to_dynamic(author_posts))
    })

  Ok(results)
}

/// Create a schema with DataLoader-backed resolvers
pub fn create_dataloader_schema() -> schema.Schema {
  // User type with posts field that uses DataLoader
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("User ID")
      |> schema.resolver(user_id_resolver),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("User name")
      |> schema.resolver(user_name_resolver),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.field_description("User email")
      |> schema.resolver(user_email_resolver),
    )
    |> schema.field(
      schema.field_def("posts", schema.list_type(schema.named_type("Post")))
      |> schema.field_description("Posts by this user")
      |> schema.resolver(user_posts_resolver),
      // Uses DataLoader!
    )

  // Post type
  let post_type =
    schema.object("Post")
    |> schema.description("A blog post")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("Post ID")
      |> schema.resolver(post_id_resolver),
    )
    |> schema.field(
      schema.field_def("title", schema.string_type())
      |> schema.field_description("Post title")
      |> schema.resolver(post_title_resolver),
    )
    |> schema.field(
      schema.field_def("author", schema.named_type("User"))
      |> schema.field_description("Post author")
      |> schema.resolver(post_author_resolver),
      // Uses DataLoader!
    )

  // Query type
  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("users", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users")
      |> schema.resolver(get_users_resolver),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
  |> schema.add_type(schema.ObjectTypeDef(post_type))
}

/// Create execution context with DataLoaders
pub fn create_execution_context_with_dataloaders() -> schema.ExecutionContext {
  let user_context = create_demo_dynamic("demo_context")
  let base_context = schema.execution_context(user_context)

  // Create DataLoaders
  let user_loader = dataloader.new(batch_load_users)
  let posts_loader = dataloader.new(batch_load_posts_by_author)

  // Add DataLoaders to context
  base_context
  |> schema.add_data_loader("users", user_loader)
  |> schema.add_data_loader("posts_by_author", posts_loader)
}

// Resolvers

fn get_users_resolver(_info: schema.ResolverInfo) -> Result(Dynamic, String) {
  // Return all users as mock data
  Ok(users_to_dynamic(users))
}

fn user_id_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = dynamic_to_user(parent_user)
      Ok(create_demo_dynamic(user.id))
    }
    None -> Error("No parent user")
  }
}

fn user_name_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = dynamic_to_user(parent_user)
      Ok(create_demo_dynamic(user.name))
    }
    None -> Error("No parent user")
  }
}

fn user_email_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = dynamic_to_user(parent_user)
      Ok(create_demo_dynamic(user.email))
    }
    None -> Error("No parent user")
  }
}

/// This resolver uses DataLoader to efficiently batch load posts
fn user_posts_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = dynamic_to_user(parent_user)

      // Get the posts DataLoader from context
      case schema.get_data_loader(info.context, "posts_by_author") {
        Ok(posts_loader) -> {
          // Use DataLoader to load posts - this will be batched!
          let #(_updated_loader, result) =
            dataloader.load(posts_loader, create_demo_dynamic(user.id))

          // In a real implementation, you'd update the loader in the context
          // and handle async operations properly
          result
        }
        Error(err) -> Error("DataLoader error: " <> err)
      }
    }
    None -> Error("No parent user")
  }
}

fn post_id_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = dynamic_to_post(parent_post)
      Ok(create_demo_dynamic(post.id))
    }
    None -> Error("No parent post")
  }
}

fn post_title_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = dynamic_to_post(parent_post)
      Ok(create_demo_dynamic(post.title))
    }
    None -> Error("No parent post")
  }
}

/// This resolver uses DataLoader to efficiently batch load users
fn post_author_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_post) -> {
      let post = dynamic_to_post(parent_post)

      // Get the users DataLoader from context
      case schema.get_data_loader(info.context, "users") {
        Ok(users_loader) -> {
          // Use DataLoader to load user - this will be batched!
          let #(_updated_loader, result) =
            dataloader.load(users_loader, create_demo_dynamic(post.author_id))
          result
        }
        Error(err) -> Error("DataLoader error: " <> err)
      }
    }
    None -> Error("No parent post")
  }
}

// Helper functions for Dynamic conversion (simplified for demo)

/// Demo placeholder for Dynamic values - shows where proper serialization would happen
fn create_demo_dynamic(_value: a) -> Dynamic {
  // Since we can't create Dynamic values without external functions in pure Gleam,
  // this demonstrates where proper Dynamic creation would happen.
  // 
  // In practice, you'd use:
  // - gleam_json for JSON serialization  
  // - External FFI functions for Dynamic conversion
  // - Database libraries that return Dynamic values
  panic as "Demo limitation: DataLoader requires Dynamic serialization (use gleam_json in real implementations)"
}

fn user_to_dynamic(user: User) -> Dynamic {
  // In practice: use gleam_json or proper serialization
  create_demo_dynamic("user:" <> user.id)
}

fn users_to_dynamic(users_list: List(User)) -> Dynamic {
  let user_dynamics = list.map(users_list, user_to_dynamic)
  create_demo_dynamic(user_dynamics)
}

fn posts_to_dynamic(posts_list: List(Post)) -> Dynamic {
  let post_dynamics = list.map(posts_list, post_to_dynamic)
  create_demo_dynamic(post_dynamics)
}

fn post_to_dynamic(post: Post) -> Dynamic {
  // In practice: use gleam_json or proper serialization
  create_demo_dynamic("post:" <> post.id)
}

fn dynamic_to_user(_user_dynamic: Dynamic) -> User {
  // In practice: use dynamic.decode4(User, ...)
  User("1", "Mock User", "mock@example.com")
}

fn dynamic_to_post(_post_dynamic: Dynamic) -> Post {
  // In practice: use dynamic.decode4(Post, ...)
  Post("1", "Mock Post", "1", "Mock content")
}

/// Run the DataLoader example
pub fn run_dataloader_example() -> Nil {
  io.println("=== DataLoader Example ===")
  io.println("Demonstrates batching and caching to solve the N+1 query problem")
  io.println("")

  let schema = create_dataloader_schema()
  let execution_context = create_execution_context_with_dataloaders()

  // This query would normally cause the N+1 problem:
  // - 1 query to get all users
  // - N queries to get posts for each user
  // With DataLoader, it becomes:
  // - 1 query to get all users
  // - 1 batched query to get posts for all users
  let query =
    "
    {
      users {
        id
        name
        posts {
          title
        }
      }
    }
  "

  io.println("🔍 Executing query that would normally cause N+1 problem:")
  io.println(query)
  io.println("")
  io.println("With DataLoader, this becomes efficient batched operations:")
  io.println("")

  // Execute the query
  case executor.execute_query(schema, query) {
    result -> {
      case result.data {
        Some(_data) -> {
          io.println("✅ Query executed successfully with DataLoader batching!")
          io.println("")
          io.println("💡 Key Benefits:")
          io.println(
            "- Multiple individual loads are batched into single operations",
          )
          io.println("- Results are cached to avoid duplicate requests")
          io.println("- Dramatic reduction in database queries")
          io.println("- Better performance and reduced load on data sources")
        }
        None -> {
          io.println("❌ Query failed")
          case result.errors {
            [first_error, ..] -> {
              case first_error {
                executor.ValidationError(msg, _) ->
                  io.println("Validation: " <> msg)
                executor.ResolverError(msg, _) ->
                  io.println("Resolver: " <> msg)
                executor.TypeError(msg, _) -> io.println("Type: " <> msg)
              }
            }
            [] -> io.println("Unknown error")
          }
        }
      }
    }
  }
}
