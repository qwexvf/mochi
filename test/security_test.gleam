import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import mochi/parser
import mochi/security

pub fn depth_calculation_test() {
  // Simple query - depth 1
  let assert Ok(doc) = parser.parse("{ users { id } }")
  let analysis = security.analyze(doc)
  should.equal(analysis.depth, 2)

  // Nested query - depth 3
  let assert Ok(doc2) = parser.parse("{ users { posts { comments { id } } } }")
  let analysis2 = security.analyze(doc2)
  should.equal(analysis2.depth, 4)
}

pub fn complexity_calculation_test() {
  // Simple query
  let assert Ok(doc) = parser.parse("{ user { id name } }")
  let analysis = security.analyze(doc)
  // 1 (user) + 2 (id, name) = 3
  should.equal(analysis.complexity, 3)
}

pub fn alias_counting_test() {
  let assert Ok(doc) = parser.parse("{ u1: user { id } u2: user { name } }")
  let analysis = security.analyze(doc)
  should.equal(analysis.alias_count, 2)
}

pub fn root_field_counting_test() {
  let assert Ok(doc) = parser.parse("{ users { id } posts { id } comments { id } }")
  let analysis = security.analyze(doc)
  should.equal(analysis.root_field_count, 3)
}

pub fn depth_limit_validation_test() {
  // Query with depth 11
  let assert Ok(doc) = parser.parse("{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }")

  // With default config (max_depth: 10), should fail (depth is 11)
  let config = security.default_config()
  let result = security.validate(doc, config)
  should.be_error(result)

  // With permissive config (max_depth: 15), should pass
  let permissive_config = security.SecurityConfig(
    max_depth: Some(15),
    max_complexity: None,
    max_aliases: None,
    max_root_fields: None,
  )
  let result2 = security.validate(doc, permissive_config)
  should.be_ok(result2)

  // With strict config (max_depth: 5), should fail
  let strict_config = security.SecurityConfig(
    max_depth: Some(5),
    max_complexity: None,
    max_aliases: None,
    max_root_fields: None,
  )
  let result3 = security.validate(doc, strict_config)
  should.be_error(result3)
}

pub fn complexity_limit_validation_test() {
  // Query with many fields
  let assert Ok(doc) = parser.parse("{ users { id name email role status createdAt updatedAt } }")

  let strict_config = security.SecurityConfig(
    max_depth: None,
    max_complexity: Some(5),
    max_aliases: None,
    max_root_fields: None,
  )

  let result = security.validate(doc, strict_config)
  should.be_error(result)
}

pub fn no_limits_validation_test() {
  let assert Ok(doc) = parser.parse("{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }")

  let config = security.no_limits()
  let result = security.validate(doc, config)
  should.be_ok(result)
}

pub fn error_message_test() {
  let error = security.DepthLimitExceeded(15, 10)
  let msg = security.error_message(error)
  should.be_true(msg |> string.contains("15"))
  should.be_true(msg |> string.contains("10"))
}
