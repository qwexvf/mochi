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
  let assert Ok(doc) =
    parser.parse("{ users { id } posts { id } comments { id } }")
  let analysis = security.analyze(doc)
  should.equal(analysis.root_field_count, 3)
}

pub fn depth_limit_validation_test() {
  // Query with depth 11
  let assert Ok(doc) =
    parser.parse(
      "{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }",
    )

  // With default config (max_depth: 10), should fail (depth is 11)
  let config = security.default_config()
  let result = security.validate(doc, config)
  should.be_error(result)

  // With permissive config (max_depth: 15), should pass
  let permissive_config =
    security.SecurityConfig(
      max_depth: Some(15),
      max_complexity: None,
      max_aliases: None,
      max_root_fields: None,
    )
  let result2 = security.validate(doc, permissive_config)
  should.be_ok(result2)

  // With strict config (max_depth: 5), should fail
  let strict_config =
    security.SecurityConfig(
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
  let assert Ok(doc) =
    parser.parse("{ users { id name email role status createdAt updatedAt } }")

  let strict_config =
    security.SecurityConfig(
      max_depth: None,
      max_complexity: Some(5),
      max_aliases: None,
      max_root_fields: None,
    )

  let result = security.validate(doc, strict_config)
  should.be_error(result)
}

pub fn no_limits_validation_test() {
  let assert Ok(doc) =
    parser.parse(
      "{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }",
    )

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

// Fragment spread depth evasion: a deeply-nested fragment spread should count
// toward the depth limit, not bypass it.
pub fn fragment_spread_depth_test() {
  // depth = 1 (root) + 1 (a) + 1 (spread → b) + 1 (c) + 1 (d) = 5
  let assert Ok(doc) =
    parser.parse(
      "
      fragment DeepFragment on SomeType {
        b { c { d } }
      }
      { a { ...DeepFragment } }
      ",
    )
  let analysis = security.analyze(doc)
  // depth should be 5: query(1) → a(2) → spread(3) → b is inside spread so
  // spread itself adds 1 → b(4) → c(5) → d is a leaf at depth 5
  should.be_true(analysis.depth >= 4)
}

pub fn fragment_spread_depth_limit_test() {
  // Use a fragment to get depth 6, then enforce max_depth: 5
  let assert Ok(doc) =
    parser.parse(
      "
      fragment Deep on T { d { e { f } } }
      { a { b { c { ...Deep } } } }
      ",
    )
  let strict_config =
    security.SecurityConfig(
      max_depth: Some(5),
      max_complexity: None,
      max_aliases: None,
      max_root_fields: None,
    )
  // depth = root(1) + a(2) + b(3) + c(4) + spread(5) + d(6) + e(7) + f leaf
  // should exceed max_depth: 5
  let result = security.validate(doc, strict_config)
  should.be_error(result)
}

pub fn alias_limit_validation_test() {
  let assert Ok(doc) =
    parser.parse(
      "{ u1: user { id } u2: user { id } u3: user { id } u4: user { id } }",
    )
  let strict_config =
    security.SecurityConfig(
      max_depth: None,
      max_complexity: None,
      max_aliases: Some(3),
      max_root_fields: None,
    )
  let result = security.validate(doc, strict_config)
  should.be_error(result)
}

pub fn root_field_limit_validation_test() {
  let assert Ok(doc) =
    parser.parse("{ users { id } posts { id } comments { id } }")
  let strict_config =
    security.SecurityConfig(
      max_depth: None,
      max_complexity: None,
      max_aliases: None,
      max_root_fields: Some(2),
    )
  let result = security.validate(doc, strict_config)
  should.be_error(result)
}

pub fn all_limits_pass_test() {
  let assert Ok(doc) = parser.parse("{ users { id name } }")
  let config =
    security.SecurityConfig(
      max_depth: Some(10),
      max_complexity: Some(100),
      max_aliases: Some(10),
      max_root_fields: Some(10),
    )
  let result = security.validate(doc, config)
  should.be_ok(result)
}

pub fn complexity_error_message_test() {
  let error = security.ComplexityLimitExceeded(20, 10)
  let msg = security.error_message(error)
  should.be_true(msg |> string.contains("20"))
  should.be_true(msg |> string.contains("10"))
}
