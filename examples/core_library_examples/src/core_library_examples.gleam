import dataloader_example
import example_execution
import examples
import mochi
import gleam/io
import person_example

/// Demonstrates core GeQL GraphQL library functionality
pub fn main() -> Nil {
  io.println("=== GeQL Core Library Examples ===")
  io.println("")
  io.println(
    "This demonstrates the pure GraphQL functionality without any web server or database dependencies.",
  )
  io.println("")

  // Test basic parsing
  let query = "{ user { entries { id } } }"
  case geql.parse(query) {
    Ok(_document) -> {
      io.println("✅ Parser: Successfully parsed GraphQL query!")
    }
    Error(_error) -> {
      io.println("❌ Parser: Parse error occurred")
    }
  }

  io.println("")

  // Test core GraphQL functionality
  io.println("🎯 Core GraphQL Library Features:")
  io.println("")

  // Test auto-generated schema from types
  io.println("📋 Auto-Generated Schema Example:")
  person_example.run_person_example()

  io.println("")

  // Test manual schema execution  
  io.println("🔧 Manual Schema Definition Example:")
  example_execution.run_example()

  io.println("")

  // Test DataLoader functionality
  dataloader_example.run_dataloader_example()

  io.println("")
  io.println("🧚‍♀️ Pure GraphQL library demo complete!")
  io.println("")
  io.println("For web server integration, see: ../geql_web_app/")
}
