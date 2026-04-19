import mochi/scalars
import mochi/types

pub fn date_time_valid_iso8601_test() {
  let scalar = scalars.date_time()
  case scalar.parse_value(types.to_dynamic("2024-01-15T10:30:00Z")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid datetime: " <> e }
  }
}

pub fn date_time_valid_with_offset_test() {
  let scalar = scalars.date_time()
  case scalar.parse_value(types.to_dynamic("2024-01-15T10:30:00+05:30")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid datetime with offset: " <> e }
  }
}

pub fn date_time_invalid_test() {
  let scalar = scalars.date_time()
  case scalar.parse_value(types.to_dynamic("not-a-date")) {
    Ok(_) -> panic as "Should reject invalid datetime"
    Error(_) -> Nil
  }
}

pub fn date_time_empty_string_test() {
  let scalar = scalars.date_time()
  case scalar.parse_value(types.to_dynamic("")) {
    Ok(_) -> panic as "Should reject empty string"
    Error(_) -> Nil
  }
}

pub fn date_valid_test() {
  let scalar = scalars.date()
  case scalar.parse_value(types.to_dynamic("2024-01-15")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid date: " <> e }
  }
}

pub fn date_invalid_format_test() {
  let scalar = scalars.date()
  case scalar.parse_value(types.to_dynamic("2024/01/15")) {
    Ok(_) -> panic as "Should reject slash-delimited date"
    Error(_) -> Nil
  }
}

pub fn date_invalid_short_test() {
  let scalar = scalars.date()
  case scalar.parse_value(types.to_dynamic("2024-1-5")) {
    Ok(_) -> panic as "Should reject short date"
    Error(_) -> Nil
  }
}

pub fn json_any_value_test() {
  let scalar = scalars.json()
  case scalar.parse_value(types.to_dynamic("any string")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "JSON should accept any value: " <> e }
  }
}

pub fn json_int_value_test() {
  let scalar = scalars.json()
  case scalar.parse_value(types.to_dynamic(42)) {
    Ok(_) -> Nil
    Error(e) -> panic as { "JSON should accept int: " <> e }
  }
}

pub fn email_valid_test() {
  let scalar = scalars.email()
  case scalar.parse_value(types.to_dynamic("user@example.com")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid email: " <> e }
  }
}

pub fn email_missing_at_test() {
  let scalar = scalars.email()
  case scalar.parse_value(types.to_dynamic("userexample.com")) {
    Ok(_) -> panic as "Should reject email without @"
    Error(_) -> Nil
  }
}

pub fn email_missing_dot_test() {
  let scalar = scalars.email()
  case scalar.parse_value(types.to_dynamic("user@example")) {
    Ok(_) -> panic as "Should reject email without dot in domain"
    Error(_) -> Nil
  }
}

pub fn email_multiple_at_test() {
  let scalar = scalars.email()
  case scalar.parse_value(types.to_dynamic("user@@example.com")) {
    Ok(_) -> panic as "Should reject email with multiple @"
    Error(_) -> Nil
  }
}

pub fn url_valid_https_test() {
  let scalar = scalars.url()
  case scalar.parse_value(types.to_dynamic("https://example.com")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid https URL: " <> e }
  }
}

pub fn url_valid_http_test() {
  let scalar = scalars.url()
  case scalar.parse_value(types.to_dynamic("http://example.com/path?q=1")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid http URL: " <> e }
  }
}

pub fn url_invalid_ftp_test() {
  let scalar = scalars.url()
  case scalar.parse_value(types.to_dynamic("ftp://example.com")) {
    Ok(_) -> panic as "Should reject ftp URL"
    Error(_) -> Nil
  }
}

pub fn url_invalid_no_scheme_test() {
  let scalar = scalars.url()
  case scalar.parse_value(types.to_dynamic("example.com")) {
    Ok(_) -> panic as "Should reject URL without scheme"
    Error(_) -> Nil
  }
}

pub fn uuid_valid_test() {
  let scalar = scalars.uuid()
  case scalar.parse_value(types.to_dynamic("550e8400-e29b-41d4-a716-446655440000")) {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should parse valid UUID: " <> e }
  }
}

pub fn uuid_invalid_length_test() {
  let scalar = scalars.uuid()
  case scalar.parse_value(types.to_dynamic("550e8400-e29b-41d4-a716")) {
    Ok(_) -> panic as "Should reject short UUID"
    Error(_) -> Nil
  }
}

pub fn uuid_invalid_no_dashes_test() {
  let scalar = scalars.uuid()
  case scalar.parse_value(types.to_dynamic("550e8400e29b41d4a716446655440000")) {
    Ok(_) -> panic as "Should reject UUID without dashes"
    Error(_) -> Nil
  }
}
