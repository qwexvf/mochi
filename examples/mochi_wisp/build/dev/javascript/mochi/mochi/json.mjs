import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Empty as $Empty } from "../gleam.mjs";
import {
  is_null,
  is_bool,
  is_int,
  is_float,
  is_string,
  is_list as is_list_value,
  is_dict,
  is_option,
  get_bool,
  get_int,
  get_float,
  get_string,
  get_list,
  get_dict_entries,
  unwrap_option,
} from "../mochi_json_ffi.mjs";

function escape_char(c) {
  if (c === "\"") {
    return "\\\"";
  } else if (c === "\\") {
    return "\\\\";
  } else if (c === "\n") {
    return "\\n";
  } else if (c === "\r") {
    return "\\r";
  } else if (c === "\t") {
    return "\\t";
  } else {
    return c;
  }
}

function escape_string(s) {
  let _pipe = s;
  let _pipe$1 = $string.to_graphemes(_pipe);
  let _pipe$2 = $list.map(_pipe$1, escape_char);
  return $string.concat(_pipe$2);
}

function encode_string(s) {
  return ("\"" + escape_string(s)) + "\"";
}

function make_indent(depth, indent) {
  return $string.repeat(" ", depth * indent);
}

/**
 * Encode a string value
 */
export function encode_string_value(s) {
  return encode_string(s);
}

/**
 * Encode an int value
 */
export function encode_int(n) {
  return $int.to_string(n);
}

/**
 * Encode a float value
 */
export function encode_float_value(f) {
  return $float.to_string(f);
}

/**
 * Encode a bool value
 */
export function encode_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

/**
 * Encode null
 */
export function encode_null() {
  return "null";
}

function encode_array_pretty(items, depth, indent) {
  if (items instanceof $Empty) {
    return "[]";
  } else {
    let inner_indent = make_indent(depth + 1, indent);
    let outer_indent = make_indent(depth, indent);
    let _block;
    let _pipe = items;
    let _pipe$1 = $list.map(
      _pipe,
      (item) => {
        return inner_indent + encode_value_pretty(item, depth + 1, indent);
      },
    );
    _block = $string.join(_pipe$1, ",\n");
    let encoded = _block;
    return ((("[\n" + encoded) + "\n") + outer_indent) + "]";
  }
}

function encode_value_pretty(loop$value, loop$depth, loop$indent) {
  while (true) {
    let value = loop$value;
    let depth = loop$depth;
    let indent = loop$indent;
    let $ = is_null(value);
    if ($) {
      return "null";
    } else {
      let $1 = is_option(value);
      if ($1) {
        loop$value = unwrap_option(value);
        loop$depth = depth;
        loop$indent = indent;
      } else {
        let $2 = is_bool(value);
        if ($2) {
          let $3 = get_bool(value);
          if ($3) {
            return "true";
          } else {
            return "false";
          }
        } else {
          let $3 = is_int(value);
          if ($3) {
            return $int.to_string(get_int(value));
          } else {
            let $4 = is_float(value);
            if ($4) {
              return $float.to_string(get_float(value));
            } else {
              let $5 = is_string(value);
              if ($5) {
                return encode_string(get_string(value));
              } else {
                let $6 = is_list_value(value);
                if ($6) {
                  return encode_array_pretty(get_list(value), depth, indent);
                } else {
                  let $7 = is_dict(value);
                  if ($7) {
                    return encode_object_pretty(
                      get_dict_entries(value),
                      depth,
                      indent,
                    );
                  } else {
                    return "null";
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/**
 * Encode a Dynamic value to a pretty-printed JSON string
 */
export function encode_pretty(value, indent) {
  return encode_value_pretty(value, 0, indent);
}

function encode_object_pretty(entries, depth, indent) {
  if (entries instanceof $Empty) {
    return "{}";
  } else {
    let inner_indent = make_indent(depth + 1, indent);
    let outer_indent = make_indent(depth, indent);
    let _block;
    let _pipe = entries;
    let _pipe$1 = $list.map(
      _pipe,
      (entry) => {
        let key;
        let val;
        key = entry[0];
        val = entry[1];
        return ((inner_indent + encode_string(key)) + ": ") + encode_value_pretty(
          val,
          depth + 1,
          indent,
        );
      },
    );
    _block = $string.join(_pipe$1, ",\n");
    let encoded = _block;
    return ((("{\n" + encoded) + "\n") + outer_indent) + "}";
  }
}

function encode_array(items) {
  let _block;
  let _pipe = items;
  let _pipe$1 = $list.map(_pipe, encode_value);
  _block = $string.join(_pipe$1, ",");
  let encoded = _block;
  return ("[" + encoded) + "]";
}

function encode_value(loop$value) {
  while (true) {
    let value = loop$value;
    let $ = is_null(value);
    if ($) {
      return "null";
    } else {
      let $1 = is_option(value);
      if ($1) {
        loop$value = unwrap_option(value);
      } else {
        let $2 = is_bool(value);
        if ($2) {
          let $3 = get_bool(value);
          if ($3) {
            return "true";
          } else {
            return "false";
          }
        } else {
          let $3 = is_int(value);
          if ($3) {
            return $int.to_string(get_int(value));
          } else {
            let $4 = is_float(value);
            if ($4) {
              return $float.to_string(get_float(value));
            } else {
              let $5 = is_string(value);
              if ($5) {
                return encode_string(get_string(value));
              } else {
                let $6 = is_list_value(value);
                if ($6) {
                  return encode_array(get_list(value));
                } else {
                  let $7 = is_dict(value);
                  if ($7) {
                    return encode_object(get_dict_entries(value));
                  } else {
                    return "null";
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/**
 * Encode a Dynamic value to a JSON string
 */
export function encode(value) {
  return encode_value(value);
}

function encode_object(entries) {
  let _block;
  let _pipe = entries;
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => {
      let key;
      let val;
      key = entry[0];
      val = entry[1];
      return (encode_string(key) + ":") + encode_value(val);
    },
  );
  _block = $string.join(_pipe$1, ",");
  let encoded = _block;
  return ("{" + encoded) + "}";
}

/**
 * Encode a Dict directly to JSON
 */
export function encode_dict(d) {
  return encode_object($dict.to_list(d));
}

/**
 * Encode a List directly to JSON
 */
export function encode_list(items) {
  return encode_array(items);
}
