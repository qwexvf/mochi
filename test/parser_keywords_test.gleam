import gleam/string
import mochi/parser

fn assert_parses(q: String) {
  case parser.parse(q) {
    Ok(_) -> Nil
    Error(e) ->
      panic as { "Expected parse success but got error: " <> string.inspect(e) }
  }
}

pub fn keyword_query_as_field_name_test() {
  assert_parses("{ query { id } }")
}

pub fn keyword_mutation_as_field_name_test() {
  assert_parses("{ mutation { id } }")
}

pub fn keyword_subscription_as_field_name_test() {
  assert_parses("{ subscription { id } }")
}

pub fn keyword_fragment_as_field_name_test() {
  assert_parses("{ fragment { id } }")
}

pub fn keyword_on_as_field_name_test() {
  assert_parses("{ on { id } }")
}

pub fn keyword_true_as_field_name_test() {
  assert_parses("{ true { id } }")
}

pub fn keyword_false_as_field_name_test() {
  assert_parses("{ false { id } }")
}

pub fn keyword_null_as_field_name_test() {
  assert_parses("{ null { id } }")
}

pub fn keyword_as_alias_test() {
  assert_parses("{ query: myField { id } }")
}

pub fn keyword_as_alias_target_test() {
  assert_parses("{ myAlias: query { id } }")
}

pub fn keyword_on_as_nested_field_test() {
  assert_parses("{ user { on } }")
}

pub fn keyword_fragment_as_nested_field_test() {
  assert_parses("{ post { fragment body } }")
}
