import gleam/list
import mochi/lexer

// ── Helpers ───────────────────────────────────────────────────────────────────

fn tokens(input: String) -> Result(List(lexer.Token), lexer.LexerError) {
  case lexer.tokenize(input) {
    Ok(ts) -> Ok(list.map(ts, fn(t) { t.token }))
    Error(e) -> Error(e)
  }
}

fn inspect_error(e: lexer.LexerError) -> String {
  case e {
    lexer.UnexpectedCharacter(c, _) -> "UnexpectedCharacter(" <> c <> ")"
    lexer.InvalidNumber(n, _) -> "InvalidNumber(" <> n <> ")"
    lexer.UnterminatedString(_) -> "UnterminatedString"
  }
}

// ── Punctuation ───────────────────────────────────────────────────────────────

pub fn punctuation_tokens_test() {
  case tokens("! $ & ( ) : = @ [ ] { } |") {
    Ok([
      lexer.Bang,
      lexer.Dollar,
      lexer.Amp,
      lexer.LeftParen,
      lexer.RightParen,
      lexer.Colon,
      lexer.Equals,
      lexer.At,
      lexer.LeftBracket,
      lexer.RightBracket,
      lexer.LeftBrace,
      lexer.RightBrace,
      lexer.Pipe,
      lexer.EOF,
    ]) -> Nil
    other -> panic as { "Unexpected: " <> inspect_error_or_ok(other) }
  }
}

fn inspect_error_or_ok(r: Result(List(lexer.Token), lexer.LexerError)) -> String {
  case r {
    Ok(_) -> "ok-but-wrong-tokens"
    Error(e) -> inspect_error(e)
  }
}

pub fn spread_token_test() {
  case tokens("...") {
    Ok([lexer.Spread, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Keywords ──────────────────────────────────────────────────────────────────

pub fn keyword_query_test() {
  case tokens("query") {
    Ok([lexer.Query, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_mutation_test() {
  case tokens("mutation") {
    Ok([lexer.Mutation, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_subscription_test() {
  case tokens("subscription") {
    Ok([lexer.Subscription, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_fragment_test() {
  case tokens("fragment") {
    Ok([lexer.Fragment, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_on_test() {
  case tokens("on") {
    Ok([lexer.On, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_true_test() {
  case tokens("true") {
    Ok([lexer.TrueKeyword, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_false_test() {
  case tokens("false") {
    Ok([lexer.FalseKeyword, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn keyword_null_test() {
  case tokens("null") {
    Ok([lexer.NullKeyword, lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Names ─────────────────────────────────────────────────────────────────────

pub fn simple_name_test() {
  case tokens("hello") {
    Ok([lexer.Name("hello"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn name_with_digits_and_underscore_test() {
  case tokens("field_name_42") {
    Ok([lexer.Name("field_name_42"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn underscore_leading_name_test() {
  case tokens("_privateField") {
    Ok([lexer.Name("_privateField"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Integer literals ──────────────────────────────────────────────────────────

pub fn positive_integer_test() {
  case tokens("42") {
    Ok([lexer.IntValue(42), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn zero_integer_test() {
  case tokens("0") {
    Ok([lexer.IntValue(0), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn large_integer_test() {
  case tokens("1000000") {
    Ok([lexer.IntValue(1_000_000), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// Negative integers — required by the GraphQL spec
pub fn negative_integer_test() {
  case tokens("-5") {
    Ok([lexer.IntValue(-5), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn negative_zero_test() {
  case tokens("-0") {
    Ok([lexer.IntValue(0), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn negative_large_integer_test() {
  case tokens("-1000") {
    Ok([lexer.IntValue(-1000), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Float literals ────────────────────────────────────────────────────────────

pub fn simple_float_test() {
  case tokens("3.14") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 3.13 && v <. 3.15 {
        True -> Nil
        False -> panic as "float value out of expected range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn negative_float_test() {
  case tokens("-1.5") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. -1.6 && v <. -1.4 {
        True -> Nil
        False -> panic as "negative float value out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn float_with_lowercase_exponent_test() {
  case tokens("1.5e2") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 149.0 && v <. 151.0 {
        True -> Nil
        False -> panic as "exponent float value out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn float_with_uppercase_exponent_test() {
  case tokens("2.0E3") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 1999.0 && v <. 2001.0 {
        True -> Nil
        False -> panic as "uppercase exponent float out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn float_with_positive_exponent_sign_test() {
  case tokens("1.0e+2") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 99.0 && v <. 101.0 {
        True -> Nil
        False -> panic as "positive exponent sign float out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn float_with_negative_exponent_test() {
  case tokens("1.0e-2") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 0.009 && v <. 0.011 {
        True -> Nil
        False -> panic as "negative exponent float out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn integer_with_exponent_test() {
  case tokens("1e3") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. 999.0 && v <. 1001.0 {
        True -> Nil
        False -> panic as "integer exponent float out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn negative_float_with_exponent_test() {
  case tokens("-1.5e2") {
    Ok([lexer.FloatValue(v), lexer.EOF]) ->
      case v >. -151.0 && v <. -149.0 {
        True -> Nil
        False -> panic as "negative exponent float out of range"
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── String literals ───────────────────────────────────────────────────────────

pub fn simple_string_test() {
  case tokens("\"hello\"") {
    Ok([lexer.StringValue("hello"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn empty_string_test() {
  case tokens("\"\"") {
    Ok([lexer.StringValue(""), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn string_with_escape_quote_test() {
  case tokens("\"say \\\"hi\\\"\"") {
    Ok([lexer.StringValue("say \"hi\""), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn string_with_escape_backslash_test() {
  case tokens("\"a\\\\b\"") {
    Ok([lexer.StringValue("a\\b"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn string_with_escape_newline_test() {
  case tokens("\"line1\\nline2\"") {
    Ok([lexer.StringValue("line1\nline2"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn string_with_escape_tab_test() {
  case tokens("\"col1\\tcol2\"") {
    Ok([lexer.StringValue("col1\tcol2"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unterminated_string_is_error_test() {
  case tokens("\"unterminated") {
    Error(lexer.UnterminatedString(_)) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Comments ──────────────────────────────────────────────────────────────────

pub fn comment_is_ignored_test() {
  case tokens("# this is a comment\n42") {
    Ok([lexer.IntValue(42), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn inline_comment_is_ignored_test() {
  case tokens("{ field # comment here\n}") {
    Ok([lexer.LeftBrace, lexer.Name("field"), lexer.RightBrace, lexer.EOF]) ->
      Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn comment_at_end_of_file_test() {
  case tokens("hello # trailing comment") {
    Ok([lexer.Name("hello"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn multiple_comment_lines_test() {
  case tokens("# line 1\n# line 2\nfield") {
    Ok([lexer.Name("field"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Ignored tokens ────────────────────────────────────────────────────────────

pub fn commas_are_ignored_test() {
  case tokens("a, b, c") {
    Ok([lexer.Name("a"), lexer.Name("b"), lexer.Name("c"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn mixed_whitespace_test() {
  case tokens("a  \t  b\n\rc") {
    Ok([lexer.Name("a"), lexer.Name("b"), lexer.Name("c"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Position tracking ─────────────────────────────────────────────────────────

pub fn position_tracks_line_and_column_test() {
  case lexer.tokenize("ab\ncd") {
    Ok([
      lexer.TokenWithPosition(lexer.Name("ab"), lexer.Position(1, 1)),
      lexer.TokenWithPosition(lexer.Name("cd"), lexer.Position(2, 1)),
      lexer.TokenWithPosition(lexer.EOF, _),
    ]) -> Nil
    other ->
      panic as {
        "Unexpected position result: "
        <> case other {
          Ok(_) -> "ok-wrong-tokens"
          Error(e) -> inspect_error(e)
        }
      }
  }
}

// ── Real-world GraphQL query tokenization ─────────────────────────────────────

pub fn full_query_tokenizes_test() {
  case
    tokens(
      "# Get a user
      query GetUser($id: ID!) {
        user(id: $id) {
          id
          name
          email
        }
      }",
    )
  {
    Ok(ts) -> {
      case list.contains(ts, lexer.Query) {
        True -> Nil
        False -> panic as "Expected Query token in result"
      }
    }
    Error(e) ->
      panic as { "Failed to tokenize real query: " <> inspect_error(e) }
  }
}

pub fn mutation_with_negative_default_tokenizes_test() {
  case tokens("mutation { adjust(delta: -10) { result } }") {
    Ok(ts) -> {
      case list.contains(ts, lexer.IntValue(-10)) {
        True -> Nil
        False -> panic as "Expected IntValue(-10) in token list"
      }
    }
    Error(e) ->
      panic as { "Failed to tokenize negative default: " <> inspect_error(e) }
  }
}

// ── Unicode escape sequences ──────────────────────────────────────────────────

pub fn unicode_escape_basic_latin_test() {
  case tokens("\"\\u0041\"") {
    Ok([lexer.StringValue("A"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_latin_supplement_test() {
  case tokens("\"caf\\u00E9\"") {
    Ok([lexer.StringValue("café"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_zero_test() {
  case tokens("\"\\u0000\"") {
    Ok([lexer.StringValue("\u{0000}"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_lowercase_hex_test() {
  case tokens("\"\\u006e\"") {
    Ok([lexer.StringValue("n"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_mixed_case_hex_test() {
  case tokens("\"\\u004F\"") {
    Ok([lexer.StringValue("O"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_in_middle_of_string_test() {
  case tokens("\"hel\\u006Co\"") {
    Ok([lexer.StringValue("hello"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn unicode_escape_multiple_in_string_test() {
  case tokens("\"\\u0048\\u0069\"") {
    Ok([lexer.StringValue("Hi"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

// ── Block strings ─────────────────────────────────────────────────────────────

pub fn block_string_simple_test() {
  case tokens("\"\"\"hello\"\"\"") {
    Ok([lexer.StringValue("hello"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_empty_test() {
  case tokens("\"\"\"\"\"\"") {
    Ok([lexer.StringValue(""), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_preserves_newlines_test() {
  case tokens("\"\"\"line1\nline2\"\"\"") {
    Ok([lexer.StringValue("line1\nline2"), lexer.EOF]) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_strips_common_indent_test() {
  case tokens("\"\"\"
    hello
    world
    \"\"\"") {
    Ok([lexer.StringValue(v), lexer.EOF]) ->
      case v == "hello\nworld" {
        True -> Nil
        False -> panic as { "Unexpected value: '" <> v <> "'" }
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_escaped_triple_quote_test() {
  case tokens("\"\"\"say \\\"\"\"hello\\\"\"\"\"\"\"") {
    Ok([lexer.StringValue(v), lexer.EOF]) ->
      case v == "say \"\"\"hello\"\"\"" {
        True -> Nil
        False -> panic as { "Unexpected value: '" <> v <> "'" }
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_leading_trailing_blank_lines_stripped_test() {
  case tokens("\"\"\"\n\n  hello\n\n\"\"\"") {
    Ok([lexer.StringValue(v), lexer.EOF]) ->
      case v == "hello" {
        True -> Nil
        False -> panic as { "Unexpected value: '" <> v <> "'" }
      }
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_unterminated_is_error_test() {
  case tokens("\"\"\"unterminated") {
    Error(_) -> Nil
    other -> panic as inspect_error_or_ok(other)
  }
}

pub fn block_string_in_query_test() {
  case tokens("{ field(arg: \"\"\"block value\"\"\") { id } }") {
    Ok(ts) ->
      case list.any(ts, fn(t) { t == lexer.StringValue("block value") }) {
        True -> Nil
        False -> panic as "Expected StringValue(\"block value\") in token list"
      }
    Error(e) -> panic as { "Lex error: " <> inspect_error(e) }
  }
}
