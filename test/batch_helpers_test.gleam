// Tests for batch helpers: add_queries, add_types, add_enums, etc.

import gleam/dict
import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test types
// ============================================================================

pub type Item {
  Item(id: String, name: String)
}

fn item_type(name: String) {
  types.object(name)
  |> types.id("id", fn(i: Item) { i.id })
  |> types.string("name", fn(i: Item) { i.name })
  |> types.build(fn(_) { Ok(Item("1", "test")) })
}

// ============================================================================
// add_types
// ============================================================================

pub fn add_types_test() {
  let schema =
    query.new()
    |> query.add_query(
      query.query(
        name: "item",
        returns: schema.named_type("Foo"),
        resolve: fn(_ctx) { Ok(Item("1", "foo")) },
      ),
    )
    |> query.add_types([item_type("Foo"), item_type("Bar")])
    |> query.build

  let result = executor.execute_query(schema, "{ item { id name } }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_types should register types correctly"
  }
}

// ============================================================================
// add_enums
// ============================================================================

pub fn add_enums_test() {
  let e1 =
    types.enum_type("Color")
    |> types.value("RED")
    |> types.value("BLUE")
    |> types.build_enum

  let e2 =
    types.enum_type("Size")
    |> types.value("SMALL")
    |> types.value("LARGE")
    |> types.build_enum

  let schema =
    query.new()
    |> query.add_query(
      query.query(
        name: "ping",
        returns: schema.string_type(),
        resolve: fn(_ctx) { Ok("pong") },
      ),
    )
    |> query.add_enums([e1, e2])
    |> query.build

  let result = executor.execute_query(schema, "{ ping }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_enums should not break schema"
  }
}

// ============================================================================
// add_inputs
// ============================================================================

pub fn add_inputs_test() {
  let input1 =
    schema.InputObjectType(
      name: "CreateInput",
      description: None,
      fields: dict.from_list([
        #(
          "name",
          schema.InputFieldDefinition(
            name: "name",
            description: None,
            field_type: schema.NonNull(schema.Named("String")),
            default_value: None,
          ),
        ),
      ]),
    )

  let input2 =
    schema.InputObjectType(
      name: "UpdateInput",
      description: None,
      fields: dict.from_list([
        #(
          "id",
          schema.InputFieldDefinition(
            name: "id",
            description: None,
            field_type: schema.NonNull(schema.Named("ID")),
            default_value: None,
          ),
        ),
      ]),
    )

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        name: "ping",
        returns: schema.string_type(),
        resolve: fn(_ctx) { Ok("pong") },
      ),
    )
    |> query.add_inputs([input1, input2])
    |> query.build

  let result = executor.execute_query(schema_def, "{ ping }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_inputs should not break schema"
  }
}

// ============================================================================
// add_mutations (batch)
// ============================================================================

pub fn add_mutations_batch_test() {
  let m1 =
    query.mutation(
      name: "doA",
      args: [query.arg("x", schema.non_null(schema.string_type()))],
      returns: schema.string_type(),
      resolve: fn(args, _ctx) { query.get_string(args, "x") },
    )

  let m2 =
    query.mutation(
      name: "doB",
      args: [query.arg("y", schema.non_null(schema.string_type()))],
      returns: schema.string_type(),
      resolve: fn(args, _ctx) { query.get_string(args, "y") },
    )

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        name: "ping",
        returns: schema.string_type(),
        resolve: fn(_ctx) { Ok("pong") },
      ),
    )
    |> query.add_mutations([m1, m2])
    |> query.build

  let r1 = executor.execute_query(schema_def, "mutation { doA(x: \"hello\") }")
  let r2 = executor.execute_query(schema_def, "mutation { doB(y: \"world\") }")

  case r1.errors, r2.errors {
    [], [] -> Nil
    _, _ -> panic as "Both batch-added mutations should work"
  }
}

// ============================================================================
// add_scalars
// ============================================================================

pub fn add_scalars_test() {
  let s1 =
    schema.ScalarType(
      name: "DateTime",
      description: Some("ISO-8601 date-time"),
      serialize: fn(v) { Ok(v) },
      parse_value: fn(v) { Ok(v) },
      parse_literal: fn(v) { Ok(v) },
    )

  let s2 =
    schema.ScalarType(
      name: "JSON",
      description: Some("Arbitrary JSON"),
      serialize: fn(v) { Ok(v) },
      parse_value: fn(v) { Ok(v) },
      parse_literal: fn(v) { Ok(v) },
    )

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(name: "now", returns: schema.string_type(), resolve: fn(_ctx) {
        Ok("2024-01-01")
      }),
    )
    |> query.add_scalars([s1, s2])
    |> query.build

  let result = executor.execute_query(schema_def, "{ now }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_scalars should not break schema"
  }
}

// ============================================================================
// add_interfaces
// ============================================================================

pub fn add_interfaces_test() {
  let iface =
    schema.InterfaceType(
      name: "Node",
      description: None,
      fields: dict.new(),
      resolve_type: None,
    )

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        name: "ping",
        returns: schema.string_type(),
        resolve: fn(_ctx) { Ok("pong") },
      ),
    )
    |> query.add_interfaces([iface])
    |> query.build

  let result = executor.execute_query(schema_def, "{ ping }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_interfaces should not break schema"
  }
}

// ============================================================================
// add_unions
// ============================================================================

pub fn add_unions_test() {
  let union =
    schema.UnionType(
      name: "SearchResult",
      description: None,
      types: [],
      resolve_type: None,
    )

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        name: "ping",
        returns: schema.string_type(),
        resolve: fn(_ctx) { Ok("pong") },
      ),
    )
    |> query.add_unions([union])
    |> query.build

  let result = executor.execute_query(schema_def, "{ ping }")
  case result.errors {
    [] -> Nil
    _ -> panic as "add_unions should not break schema"
  }
}
