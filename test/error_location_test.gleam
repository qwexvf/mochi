import gleam/list
import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

pub type U {
  U(id: String)
}

fn build_schema() {
  let ut =
    types.object("U")
    |> types.id("id", fn(u: U) { u.id })
    |> types.build(fn(_) { Ok(U("1")) })
  let q =
    query.query(name: "u", returns: schema.named_type("U"), resolve: fn(_) {
      Ok(U("1"))
    })
  query.new()
  |> query.add_query(q)
  |> query.add_type(ut)
  |> query.build
}

pub fn unknown_field_error_has_location_test() {
  let s = build_schema()
  let r = executor.execute_query(s, "{ u { id unknownField } }")
  let has_location =
    list.any(r.errors, fn(e) {
      case e {
        executor.ValidationError(_, _, Some(_)) -> True
        _ -> False
      }
    })
  case has_location {
    True -> Nil
    False -> panic as "Expected validation error to have location"
  }
}

pub fn multiple_unknown_fields_all_have_locations_test() {
  let s = build_schema()
  let r = executor.execute_query(s, "{ u { fieldA fieldB fieldC } }")
  case list.length(r.errors) >= 3 {
    False -> panic as "Expected 3 validation errors"
    True -> {
      let all_located =
        list.all(r.errors, fn(e) {
          case e {
            executor.ValidationError(_, _, Some(_)) -> True
            _ -> False
          }
        })
      case all_located {
        True -> Nil
        False -> panic as "Expected all errors to have locations"
      }
    }
  }
}

pub fn parse_error_has_location_test() {
  let s = build_schema()
  let r = executor.execute_query(s, "{ 123 }")
  case r.errors {
    [] -> panic as "Expected parse error"
    [executor.ValidationError(_, _, loc), ..] -> {
      case loc {
        Some(_) -> Nil
        None -> panic as "Expected parse error to carry source location"
      }
    }
    _ -> panic as "Expected ValidationError"
  }
}
