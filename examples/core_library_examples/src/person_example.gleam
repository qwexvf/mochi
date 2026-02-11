import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/option.{None, Some}
import mochi/executor
import mochi/schema
import mochi/schema_gen

// Your Gleam type
pub type Person {
  Person(name: String, age: Int, needs_glasses: Bool)
}

// Sample data
pub fn sample_person() -> Person {
  Person(name: "Alice", age: 30, needs_glasses: True)
}

// Extractors for each field - these decode the Person from Dynamic
fn extract_name(parent: Dynamic) -> Result(String, String) {
  // In practice: decode_person(parent) |> result.map(fn(p) { p.name })
  // For demo: just return the sample data
  Ok("Alice")
}

fn extract_age(parent: Dynamic) -> Result(Int, String) {
  // In practice: decode_person(parent) |> result.map(fn(p) { p.age })
  // For demo: just return the sample data
  Ok(30)
}

fn extract_needs_glasses(parent: Dynamic) -> Result(Bool, String) {
  // In practice: decode_person(parent) |> result.map(fn(p) { p.needs_glasses })
  // For demo: just return the sample data
  Ok(True)
}

// Generate schema automatically from the Person type
pub fn create_person_schema() -> schema.Schema {
  let field_specs = [
    schema_gen.string_field("name", "The person's name", extract_name),
    schema_gen.int_field("age", "The person's age in years", extract_age),
    schema_gen.bool_field(
      "needsGlasses",
      "Whether the person needs glasses",
      extract_needs_glasses,
    ),
  ]

  schema_gen.create_schema_with_query("Person", field_specs, fn(_info) {
    // Root resolver - return the Person struct
    let person = sample_person()
    // In practice: Ok(serialize_to_dynamic(person))
    Error(
      "Demo: would return Person(name="
      <> person.name
      <> ", age="
      <> int_to_string(person.age)
      <> ", needs_glasses="
      <> bool_to_string(person.needs_glasses)
      <> ")",
    )
  })
}

// Execute a query against the generated schema
pub fn execute_person_query() -> executor.ExecutionResult {
  let person_schema = create_person_schema()
  let query = "{ person { name age needsGlasses } }"

  executor.execute_query(person_schema, query)
}

pub fn run_person_example() -> Nil {
  io.println("=== Auto-Generated Schema Example ===")
  io.println("Generated GraphQL schema from Gleam type:")
  io.println("")
  io.println("pub type Person {")
  io.println("  Person(name: String, age: Int, needs_glasses: Bool)")
  io.println("}")
  io.println("")
  io.println("Executing query: { person { name age needsGlasses } }")
  io.println("")

  let result = execute_person_query()

  case result.data {
    Some(_data) -> {
      io.println("✅ Auto-generated schema executed successfully!")
    }
    None -> {
      io.println("❌ Query execution failed with auto-generated schema")
      case result.errors {
        [first_error, ..] -> {
          case first_error {
            executor.ValidationError(msg, _path) ->
              io.println("Validation error: " <> msg)
            executor.ResolverError(msg, _path) ->
              io.println("Resolver error: " <> msg)
            executor.TypeError(msg, _path) -> io.println("Type error: " <> msg)
          }
        }
        [] -> io.println("Unknown error occurred")
      }
    }
  }
}

// Helper functions (simplified for demo)
fn int_to_string(value: Int) -> String {
  case value {
    30 -> "30"
    _ -> "unknown"
  }
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
