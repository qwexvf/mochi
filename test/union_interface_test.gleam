import gleam/dict
import gleam/dynamic
import gleam/option
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type Cat {
  Cat(id: String, name: String, meows: Bool)
}

pub type Dog {
  Dog(id: String, name: String, barks: Bool)
}

// ============================================================================
// Interface Tests
// ============================================================================

fn resolve_animal_type(value: dynamic.Dynamic) -> Result(String, String) {
  // Check if it has a "meows" field - if so it's a Cat
  case decode_dict_has_key(value, "meows") {
    True -> Ok("Cat")
    False -> Ok("Dog")
  }
}

@external(erlang, "mochi_ffi", "dict_has_key")
@external(javascript, "./mochi_ffi.mjs", "dict_has_key")
fn decode_dict_has_key(value: dynamic.Dynamic, key: String) -> Bool

fn build_interface_schema() -> schema.Schema {
  // Define the Animal interface
  let animal_interface =
    schema.interface("Animal")
    |> schema.interface_description("An animal in the system")
    |> schema.interface_field(schema.field_def("id", schema.id_type()))
    |> schema.interface_field(schema.field_def("name", schema.string_type()))
    |> schema.interface_resolve_type(resolve_animal_type)

  // Define Cat type implementing Animal
  let cat_type =
    types.object("Cat")
    |> types.id("id", fn(c: Cat) { c.id })
    |> types.string("name", fn(c: Cat) { c.name })
    |> types.bool("meows", fn(c: Cat) { c.meows })
    |> types.build(fn(_) { Ok(Cat("1", "Whiskers", True)) })
    |> fn(obj) { schema.ObjectType(..obj, interfaces: [animal_interface]) }

  // Define Dog type implementing Animal
  let dog_type =
    types.object("Dog")
    |> types.id("id", fn(d: Dog) { d.id })
    |> types.string("name", fn(d: Dog) { d.name })
    |> types.bool("barks", fn(d: Dog) { d.barks })
    |> types.build(fn(_) { Ok(Dog("2", "Rex", True)) })
    |> fn(obj) { schema.ObjectType(..obj, interfaces: [animal_interface]) }

  // Query that returns the interface type
  let animal_query =
    query.query(
      "animal",
      schema.Named("Animal"),
      fn(_info) {
        // Return a Cat
        Ok(
          types.to_dynamic(
            dict.from_list([
              #("id", types.to_dynamic("1")),
              #("name", types.to_dynamic("Whiskers")),
              #("meows", types.to_dynamic(True)),
            ]),
          ),
        )
      },
      fn(a) { types.to_dynamic(a) },
    )

  query.new()
  |> query.add_query(animal_query)
  |> query.add_type(cat_type)
  |> query.add_type(dog_type)
  |> query.add_interface(animal_interface)
  |> query.build
}

pub fn inline_fragment_on_interface_test() {
  let schema_def = build_interface_schema()
  let query_str =
    "
    query {
      animal {
        id
        name
        ... on Cat {
          meows
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Union Tests
// ============================================================================

fn build_union_schema() -> schema.Schema {
  // Define Cat type
  let cat_type =
    types.object("Cat")
    |> types.id("id", fn(c: Cat) { c.id })
    |> types.string("name", fn(c: Cat) { c.name })
    |> types.bool("meows", fn(c: Cat) { c.meows })
    |> types.build(fn(_) { Ok(Cat("1", "Whiskers", True)) })

  // Define Dog type
  let dog_type =
    types.object("Dog")
    |> types.id("id", fn(d: Dog) { d.id })
    |> types.string("name", fn(d: Dog) { d.name })
    |> types.bool("barks", fn(d: Dog) { d.barks })
    |> types.build(fn(_) { Ok(Dog("2", "Rex", True)) })

  // Define the Pet union
  let pet_union =
    schema.union("Pet")
    |> schema.union_description("A pet in the system")
    |> schema.union_member(cat_type)
    |> schema.union_member(dog_type)
    |> schema.union_resolve_type(resolve_animal_type)

  // Query that returns the union type
  let pet_query =
    query.query(
      "pet",
      schema.Named("Pet"),
      fn(_info) {
        // Return a Dog
        Ok(
          types.to_dynamic(
            dict.from_list([
              #("id", types.to_dynamic("2")),
              #("name", types.to_dynamic("Rex")),
              #("barks", types.to_dynamic(True)),
            ]),
          ),
        )
      },
      fn(p) { types.to_dynamic(p) },
    )

  query.new()
  |> query.add_query(pet_query)
  |> query.add_type(cat_type)
  |> query.add_type(dog_type)
  |> query.add_union(pet_union)
  |> query.build
}

pub fn inline_fragment_on_union_test() {
  let schema_def = build_union_schema()
  let query_str =
    "
    query {
      pet {
        ... on Dog {
          id
          name
          barks
        }
        ... on Cat {
          id
          name
          meows
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn typename_on_union_test() {
  let schema_def = build_union_schema()
  let query_str =
    "
    query {
      pet {
        __typename
        ... on Dog {
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

// ============================================================================
// Simple Inline Fragment Tests (no abstract types)
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

fn build_simple_schema() -> schema.Schema {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.string("email", fn(u: User) { u.email })
    |> types.build(fn(_) { Ok(User("1", "Alice", "alice@example.com")) })

  let user_query =
    query.query(
      "user",
      schema.Named("User"),
      fn(_info) {
        Ok(types.to_dynamic(User("1", "Alice", "alice@example.com")))
      },
      fn(u) { types.to_dynamic(u) },
    )

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type)
  |> query.build
}

pub fn inline_fragment_same_type_test() {
  let schema_def = build_simple_schema()
  let query_str =
    "
    query {
      user {
        id
        ... on User {
          name
          email
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn inline_fragment_no_type_condition_test() {
  let schema_def = build_simple_schema()
  let query_str =
    "
    query {
      user {
        id
        ... {
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  should.be_true(option.is_some(result.data))
  should.equal(result.errors, [])
}

pub fn inline_fragment_type_mismatch_test() {
  let schema_def = build_simple_schema()
  // This should succeed but the inline fragment should be skipped
  let query_str =
    "
    query {
      user {
        id
        ... on OtherType {
          name
        }
      }
    }
    "

  let result = executor.execute_query(schema_def, query_str)
  // Type mismatch should not produce an error, just skip the fragment
  should.be_true(option.is_some(result.data))
}
