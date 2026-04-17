import gleeunit
import mochi

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn basic_query_test() {
  let query = "{ user { entries { id } } }"
  let result = mochi.parse(query)

  case result {
    Ok(_document) -> Nil
    Error(_error) -> panic as "Failed to parse valid GraphQL query"
  }
}

pub fn named_query_test() {
  let query = "query GetUser { user { name id } }"
  let result = mochi.parse(query)

  case result {
    Ok(_document) -> Nil
    Error(_error) -> panic as "Failed to parse named GraphQL query"
  }
}

pub fn nested_fields_test() {
  let query =
    "{ user { profile { name avatar { url size } } posts { title content } } }"
  let result = mochi.parse(query)

  case result {
    Ok(_document) -> Nil
    Error(_error) -> panic as "Failed to parse nested GraphQL query"
  }
}

pub fn basic_mutation_test() {
  let mutation = "mutation { createUser { id name } }"
  let result = mochi.parse(mutation)

  case result {
    Ok(_document) -> Nil
    Error(_error) -> panic as "Failed to parse basic GraphQL mutation"
  }
}

pub fn named_mutation_test() {
  let mutation = "mutation CreateUser { createUser { id name email } }"
  let result = mochi.parse(mutation)

  case result {
    Ok(_document) -> Nil
    Error(_error) -> panic as "Failed to parse named GraphQL mutation"
  }
}
