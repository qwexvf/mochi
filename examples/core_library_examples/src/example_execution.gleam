import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import mochi/executor
import mochi/schema

// Sample user data
pub type User {
  User(id: String, name: String, email: String)
}

pub fn sample_user() -> User {
  User(id: "user123", name: "John Doe", email: "john@example.com")
}

// For now, let's use a simple approach - just create the user data directly
// In a real implementation, we'd have proper decoders
fn get_user_from_parent(_dyn: Dynamic) -> User {
  // This is a simplified approach - in reality we'd decode the dynamic value
  // For this demo, we'll just return the sample user
  sample_user()
}

// Demonstration approach: show resolver structure without full Dynamic conversion
// In a real implementation, you'd have proper JSON serialization to Dynamic

// Create a schema with resolvers that return actual data
pub fn create_user_schema() -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("The unique identifier for the user")
      |> schema.resolver(fn(info) {
        // Extract the User struct from parent and return its id
        case info.parent {
          Some(parent_dynamic) -> {
            let user = get_user_from_parent(parent_dynamic)
            // For this demo without FFI, we'll just show the structure works
            // In practice: Ok(json_serialize(user.id))
            Error("Demo: would return user.id = " <> user.id)
          }
          None -> Error("No parent user provided")
        }
      }),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("The user's display name")
      |> schema.resolver(fn(info) {
        // Extract the User struct from parent and return its name
        case info.parent {
          Some(parent_dynamic) -> {
            let user = get_user_from_parent(parent_dynamic)
            // For this demo without FFI, we'll just show the structure works
            // In practice: Ok(json_serialize(user.name))
            Error("Demo: would return user.name = " <> user.name)
          }
          None -> Error("No parent user provided")
        }
      }),
    )
    |> schema.field(
      schema.field_def("email", schema.string_type())
      |> schema.field_description("The user's email address")
      |> schema.resolver(fn(info) {
        // Extract the User struct from parent and return its email
        case info.parent {
          Some(parent_dynamic) -> {
            let user = get_user_from_parent(parent_dynamic)
            // For this demo without FFI, we'll just show the structure works
            // In practice: Ok(json_serialize(user.email))
            Error("Demo: would return user.email = " <> user.email)
          }
          None -> Error("No parent user provided")
        }
      }),
    )

  let query_type =
    schema.object("Query")
    |> schema.description("The root query type")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user")
      |> schema.resolver(fn(_info) {
        // Return the User struct - the child resolvers will extract from it
        let user = sample_user()
        // For this demo without FFI, we'll show the data we would return
        // In practice: Ok(json_serialize(user))
        Error(
          "Demo: would return User(id="
          <> user.id
          <> ", name="
          <> user.name
          <> ", email="
          <> user.email
          <> ")",
        )
      }),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
  |> schema.add_type(schema.ScalarTypeDef(schema.string_scalar()))
  |> schema.add_type(schema.ScalarTypeDef(schema.id_scalar()))
}

pub fn execute_user_query() -> executor.ExecutionResult {
  let user_schema = create_user_schema()
  let query = "{ user { id email name } }"

  executor.execute_query(user_schema, query)
}

pub fn run_example() -> Nil {
  io.println("Executing GraphQL query: { user { id email name } }")

  let result = execute_user_query()

  case result.data {
    Some(_data) -> {
      io.println("✅ Query executed successfully!")
      // In a real implementation, we'd format the JSON output here
    }
    None -> {
      io.println("❌ Query execution failed")
      case result.errors {
        [first_error, ..] -> {
          case first_error {
            executor.ValidationError(msg, path) ->
              io.println("Validation error: " <> msg)
            executor.ResolverError(msg, path) ->
              io.println("Resolver error: " <> msg)
            executor.TypeError(msg, path) -> io.println("Type error: " <> msg)
          }
        }
        [] -> io.println("Unknown error occurred")
      }
    }
  }
}
