// mochi/cli.gleam
// CLI helpers for Mochi codegen operations
//
// For a full CLI with file writing, use these functions with simplifile:
//
//   import mochi/cli
//   import simplifile
//
//   let schema = build_schema()
//   let _ = simplifile.write("types.ts", cli.typescript(schema))
//   let _ = simplifile.write("schema.graphql", cli.sdl(schema))

import gleam/io
import mochi/codegen/sdl
import mochi/codegen/typescript
import mochi/schema.{type Schema}

/// Generate TypeScript types as a string
pub fn typescript(schema: Schema) -> String {
  typescript.generate(schema)
}

/// Generate GraphQL SDL as a string
pub fn sdl(schema: Schema) -> String {
  sdl.generate(schema)
}

/// Print TypeScript types to stdout
pub fn print_typescript(schema: Schema) -> Nil {
  io.println(typescript.generate(schema))
}

/// Print SDL to stdout
pub fn print_sdl(schema: Schema) -> Nil {
  io.println(sdl.generate(schema))
}

/// Print both TypeScript and SDL to stdout
pub fn print_all(schema: Schema) -> Nil {
  io.println("// TypeScript Types")
  io.println("// =================")
  io.println("")
  print_typescript(schema)
  io.println("")
  io.println("// GraphQL SDL")
  io.println("// ===========")
  io.println("")
  print_sdl(schema)
}
