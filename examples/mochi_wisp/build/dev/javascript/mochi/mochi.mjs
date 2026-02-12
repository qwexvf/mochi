import * as $io from "../gleam_stdlib/gleam/io.mjs";
import { Ok, Error } from "./gleam.mjs";
import * as $ast from "./mochi/ast.mjs";
import * as $executor from "./mochi/executor.mjs";
import * as $parser from "./mochi/parser.mjs";
import * as $mochi_query from "./mochi/query.mjs";
import * as $schema from "./mochi/schema.mjs";

/**
 * Parse a GraphQL query string
 */
export function parse(query_string) {
  return $parser.parse(query_string);
}

/**
 * Execute a GraphQL query against a schema
 */
export function execute(schema, query_string) {
  return $executor.execute_query(schema, query_string);
}

/**
 * Create a new schema builder
 */
export function new_schema() {
  return $mochi_query.new$();
}

function create_demo_schema() {
  let _block;
  let _pipe = $schema.object("User");
  let _pipe$1 = $schema.description(_pipe, "A user in the system");
  let _pipe$2 = $schema.field(
    _pipe$1,
    (() => {
      let _pipe$2 = $schema.field_def("id", $schema.non_null($schema.id_type()));
      return $schema.resolver(_pipe$2, (_) => { return new Error("Demo"); });
    })(),
  );
  _block = $schema.field(
    _pipe$2,
    (() => {
      let _pipe$3 = $schema.field_def("name", $schema.string_type());
      return $schema.resolver(_pipe$3, (_) => { return new Error("Demo"); });
    })(),
  );
  let user_type = _block;
  let _block$1;
  let _pipe$3 = $schema.object("Query");
  _block$1 = $schema.field(
    _pipe$3,
    (() => {
      let _pipe$4 = $schema.field_def("user", $schema.named_type("User"));
      return $schema.resolver(_pipe$4, (_) => { return new Error("Demo"); });
    })(),
  );
  let query_type = _block$1;
  let _pipe$4 = $schema.schema();
  let _pipe$5 = $schema.query(_pipe$4, query_type);
  return $schema.add_type(_pipe$5, new $schema.ObjectTypeDef(user_type));
}

export function main() {
  $io.println("🍡 mochi - Code First GraphQL for Gleam");
  $io.println("========================================");
  $io.println("");
  $io.println("📦 Code First API Example:");
  $io.println("");
  $io.println("  // Define your Gleam type");
  $io.println("  pub type User {");
  $io.println("    User(id: String, name: String, age: Int)");
  $io.println("  }");
  $io.println("");
  $io.println("  // Build GraphQL type with type-safe extractors");
  $io.println("  let user_type = types.object(\"User\")");
  $io.println("    |> types.id(\"id\", fn(u: User) { u.id })");
  $io.println("    |> types.string(\"name\", fn(u: User) { u.name })");
  $io.println("    |> types.int(\"age\", fn(u: User) { u.age })");
  $io.println("    |> types.build(decode_user)");
  $io.println("");
  let query = "{ user { id name } }";
  let $ = $parser.parse(query);
  if ($ instanceof Ok) {
    $io.println("✅ GraphQL Parser: Working!")
  } else {
    $io.println("❌ Parser error")
  }
  let $1 = create_demo_schema();
  
  $io.println("✅ Schema Builder: Working!");
  $io.println("");
  $io.println("📚 Available Modules:");
  $io.println("  - mochi/types   : Type builders (object, string, int, etc.)");
  $io.println("  - mochi/query   : Query/Mutation builders");
  $io.println("  - mochi/schema  : Low-level schema types");
  $io.println("  - mochi/parser  : GraphQL query parser");
  $io.println("  - mochi/executor: Query execution engine");
  $io.println("");
  return $io.println("🎉 Ready to build type-safe GraphQL APIs!");
}
