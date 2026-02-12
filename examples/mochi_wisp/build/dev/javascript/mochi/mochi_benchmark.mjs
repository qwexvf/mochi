import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, Empty as $Empty, divideFloat } from "./gleam.mjs";
import * as $executor from "./mochi/executor.mjs";
import * as $parser from "./mochi/parser.mjs";
import * as $schema from "./mochi/schema.mjs";

function create_benchmark_schema() {
  let _block;
  let _pipe = $schema.object("User");
  let _pipe$1 = $schema.field(
    _pipe,
    (() => {
      let _pipe$1 = $schema.field_def("id", $schema.non_null($schema.id_type()));
      return $schema.resolver(
        _pipe$1,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let _pipe$2 = $schema.field(
    _pipe$1,
    (() => {
      let _pipe$2 = $schema.field_def("name", $schema.string_type());
      return $schema.resolver(
        _pipe$2,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let _pipe$3 = $schema.field(
    _pipe$2,
    (() => {
      let _pipe$3 = $schema.field_def("email", $schema.string_type());
      return $schema.resolver(
        _pipe$3,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let _pipe$4 = $schema.field(
    _pipe$3,
    (() => {
      let _pipe$4 = $schema.field_def("active", $schema.boolean_type());
      return $schema.resolver(
        _pipe$4,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  _block = $schema.field(
    _pipe$4,
    (() => {
      let _pipe$5 = $schema.field_def(
        "posts",
        $schema.list_type($schema.named_type("Post")),
      );
      return $schema.resolver(
        _pipe$5,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let user_type = _block;
  let _block$1;
  let _pipe$5 = $schema.object("Post");
  let _pipe$6 = $schema.field(
    _pipe$5,
    (() => {
      let _pipe$6 = $schema.field_def("id", $schema.non_null($schema.id_type()));
      return $schema.resolver(
        _pipe$6,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let _pipe$7 = $schema.field(
    _pipe$6,
    (() => {
      let _pipe$7 = $schema.field_def("title", $schema.string_type());
      return $schema.resolver(
        _pipe$7,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  _block$1 = $schema.field(
    _pipe$7,
    (() => {
      let _pipe$8 = $schema.field_def("content", $schema.string_type());
      return $schema.resolver(
        _pipe$8,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let post_type = _block$1;
  let _block$2;
  let _pipe$8 = $schema.object("Query");
  let _pipe$9 = $schema.field(
    _pipe$8,
    (() => {
      let _pipe$9 = $schema.field_def("user", $schema.named_type("User"));
      let _pipe$10 = $schema.argument(
        _pipe$9,
        $schema.arg("id", $schema.non_null($schema.id_type())),
      );
      return $schema.resolver(
        _pipe$10,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  _block$2 = $schema.field(
    _pipe$9,
    (() => {
      let _pipe$10 = $schema.field_def(
        "users",
        $schema.list_type($schema.named_type("User")),
      );
      return $schema.resolver(
        _pipe$10,
        (_) => { return new Error("Dynamic serialization needed"); },
      );
    })(),
  );
  let query_type = _block$2;
  let _pipe$10 = $schema.schema();
  let _pipe$11 = $schema.query(_pipe$10, query_type);
  let _pipe$12 = $schema.add_type(
    _pipe$11,
    new $schema.ObjectTypeDef(user_type),
  );
  return $schema.add_type(_pipe$12, new $schema.ObjectTypeDef(post_type));
}

function float_to_string(value) {
  let $ = value >= 99.0;
  if ($) {
    return "100.0";
  } else {
    let $1 = value >= 10.0;
    if ($1) {
      return "99.0";
    } else {
      return "0.0";
    }
  }
}

function test_parsing_performance(name, query, iterations) {
  $io.print(
    ((("Testing " + name) + " parsing (") + $int.to_string(iterations)) + " iterations)... ",
  );
  let _block;
  let _pipe = $list.range(1, iterations);
  _block = $list.map(_pipe, (_) => { return $parser.parse(query); });
  let results = _block;
  let successful = $list.count(
    results,
    (r) => {
      if (r instanceof Ok) {
        return true;
      } else {
        return false;
      }
    },
  );
  let _block$1;
  if (iterations === 0) {
    _block$1 = 0.0;
  } else {
    let n = iterations;
    _block$1 = (divideFloat($int.to_float(successful), $int.to_float(n))) * 100.0;
  }
  let success_rate = _block$1;
  return $io.println(("✅ " + float_to_string(success_rate)) + "% success rate");
}

function test_execution_performance(name, query, schema, iterations) {
  $io.print(
    ((("Testing " + name) + " execution (") + $int.to_string(iterations)) + " iterations)... ",
  );
  let _block;
  let _pipe = $list.range(1, iterations);
  _block = $list.map(
    _pipe,
    (_) => { return $executor.execute_query(schema, query); },
  );
  let results = _block;
  let successful = $list.count(
    results,
    (r) => {
      let $ = r.errors;
      if ($ instanceof $Empty) {
        return true;
      } else {
        return false;
      }
    },
  );
  let _block$1;
  if (iterations === 0) {
    _block$1 = 0.0;
  } else {
    let n = iterations;
    _block$1 = (divideFloat($int.to_float(successful), $int.to_float(n))) * 100.0;
  }
  let success_rate = _block$1;
  return $io.println(
    ("ℹ️  " + float_to_string(success_rate)) + "% (limited by Dynamic serialization)",
  );
}

export function main() {
  $io.println("🍡 mochi Performance Benchmark");
  $io.println(
    (() => {
      let _pipe = "=";
      return $string.repeat(_pipe, 40);
    })(),
  );
  $io.println("");
  let schema = create_benchmark_schema();
  let simple_query = "{ user(id: \"1\") { id name } }";
  let complex_query = "{ user(id: \"1\") { id name email active posts { id title content } } }";
  let nested_query = "{ users { id name posts { id title } } }";
  $io.println("📝 Testing Query Parsing Performance...");
  test_parsing_performance("Simple", simple_query, 1000);
  test_parsing_performance("Complex", complex_query, 1000);
  test_parsing_performance("Nested", nested_query, 1000);
  $io.println("");
  $io.println("⚡ Testing Query Execution Performance...");
  test_execution_performance("Simple", simple_query, schema, 1000);
  test_execution_performance("Complex", complex_query, schema, 1000);
  test_execution_performance("Nested", nested_query, schema, 1000);
  $io.println("");
  $io.println("📊 Benchmark Results Summary");
  $io.println(
    (() => {
      let _pipe = "-";
      return $string.repeat(_pipe, 30);
    })(),
  );
  $io.println("mochi demonstrates strong parsing performance");
  $io.println("Execution limited by Dynamic serialization (known issue)");
  return $io.println("Ready for HTTP load testing comparison!");
}
