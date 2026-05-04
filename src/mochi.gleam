//// mochi — Code First GraphQL for Gleam.
////
//// This module re-exports the most common entry points. See
//// `mochi/query`, `mochi/types`, and `mochi/schema` for the full API.

import mochi/ast
import mochi/executor
import mochi/parser
import mochi/query as mochi_query
import mochi/schema

/// Parse a GraphQL query string into an AST document.
pub fn parse(query_string: String) -> Result(ast.Document, parser.ParseError) {
  parser.parse(query_string)
}

/// Execute a GraphQL query against a schema.
pub fn execute(
  schema: schema.Schema,
  query_string: String,
) -> executor.ExecutionResult {
  executor.execute_query(schema, query_string)
}

/// Create a new schema builder.
pub fn new_schema() -> mochi_query.SchemaBuilder {
  mochi_query.new()
}
