import gleam/dict
import gleam/dynamic/decode
import gleam/option.{None, Some}
import gleam/string
import mochi/args
import mochi/query
import mochi/types

pub fn decode_input_valid_test() {
  let args =
    args.from_dict(
      dict.from_list([
        #(
          "input",
          types.record([
            #("name", types.to_dynamic("Alice")),
            #("age", types.to_dynamic(30)),
          ]),
        ),
      ]),
    )

  let decoder = {
    use name <- decode.field("name", decode.string)
    use age <- decode.field("age", decode.int)
    decode.success(#(name, age))
  }

  case query.decode_input(args, "input", decoder) {
    Ok(#("Alice", 30)) -> Nil
    Ok(_) -> panic as "Wrong decoded values"
    Error(e) -> panic as { "Should succeed: " <> e.message }
  }
}

pub fn decode_input_missing_test() {
  let args = args.from_dict(dict.new())
  let decoder = decode.string
  case query.decode_input(args, "input", decoder) {
    Error(msg) ->
      case string.contains(msg.message, "input") {
        True -> Nil
        False -> panic as { "Error should mention field name: " <> msg.message }
      }
    Ok(_) -> panic as "Should fail on missing key"
  }
}

pub fn get_dynamic_present_test() {
  let args =
    args.from_dict(
      dict.from_list([#("file", types.to_dynamic("path/to/file"))]),
    )
  case query.get_dynamic(args, "file") {
    Ok(_) -> Nil
    Error(e) -> panic as { "Should find dynamic value: " <> e.message }
  }
}

pub fn get_optional_dynamic_absent_test() {
  let args = args.from_dict(dict.new())
  case query.get_optional_dynamic(args, "file") {
    None -> Nil
    Some(_) -> panic as "Should return None for missing key"
  }
}
