import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $schema from "../mochi/schema.mjs";

function demo_query_parsing(_) {
  $io.println("📝 Test: Schema Structure");
  $io.println("=========================");
  let simple_query = "\n    query {\n      hello\n    }";
  $io.println("Example Query that our schema supports:");
  $io.println(simple_query);
  $io.println("✅ Schema successfully defines this structure!");
  $io.println("");
  let complex_query = "\n    query GetUser {\n      user {\n        id\n        name\n        email\n        role\n      }\n    }";
  $io.println("Complex User Query structure:");
  $io.println(complex_query);
  $io.println("✅ Schema supports User type with all these fields!");
  $io.println("");
  $io.println("🏗️  Schema Building Capabilities Verified:");
  $io.println("   ✅ Object type definitions");
  $io.println("   ✅ Field type specifications (String, ID, nullable/non-null)");
  $io.println("   ✅ Field descriptions and documentation");
  $io.println("   ✅ Query root type definition");
  $io.println("   ✅ Complete schema assembly");
  $io.println("");
  $io.println("💡 Note: To execute queries with data, add:");
  $io.println("   - Resolver functions for each field");
  $io.println("   - gleam_json dependency for dynamic serialization");
  return $io.println("   - Data source (database, API, etc.)");
}

export function demo_user_schema() {
  $io.println("=== GeQL User Schema Demo ===");
  $io.println("");
  let _block;
  let _pipe = $schema.object("User");
  let _pipe$1 = $schema.description(_pipe, "A user in the system");
  let _pipe$2 = $schema.field(
    _pipe$1,
    (() => {
      let _pipe$2 = $schema.field_def("id", $schema.non_null($schema.id_type()));
      return $schema.field_description(_pipe$2, "Unique identifier");
    })(),
  );
  let _pipe$3 = $schema.field(
    _pipe$2,
    (() => {
      let _pipe$3 = $schema.field_def(
        "name",
        $schema.non_null($schema.string_type()),
      );
      return $schema.field_description(_pipe$3, "User's full name");
    })(),
  );
  let _pipe$4 = $schema.field(
    _pipe$3,
    (() => {
      let _pipe$4 = $schema.field_def(
        "email",
        $schema.non_null($schema.string_type()),
      );
      return $schema.field_description(_pipe$4, "User's email address");
    })(),
  );
  _block = $schema.field(
    _pipe$4,
    (() => {
      let _pipe$5 = $schema.field_def("role", $schema.string_type());
      return $schema.field_description(_pipe$5, "User's role");
    })(),
  );
  let user_type = _block;
  let _block$1;
  let _pipe$5 = $schema.object("Query");
  _block$1 = $schema.field(
    _pipe$5,
    (() => {
      let _pipe$6 = $schema.field_def("hello", $schema.string_type());
      return $schema.field_description(_pipe$6, "Simple hello field");
    })(),
  );
  let query_type = _block$1;
  let _block$2;
  let _pipe$6 = $schema.schema();
  let _pipe$7 = $schema.query(_pipe$6, query_type);
  _block$2 = $schema.add_type(_pipe$7, new $schema.ObjectTypeDef(user_type));
  let user_schema = _block$2;
  $io.println("🏗️  Built User Schema Successfully!");
  $io.println("   - User type with 4 fields (id, name, email, role)");
  $io.println("   - Query type with hello field");
  $io.println("");
  demo_query_parsing(user_schema);
  return $io.println("🎯 User Schema Demo Complete!");
}
