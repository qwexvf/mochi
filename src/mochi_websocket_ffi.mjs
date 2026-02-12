// WebSocket FFI for JavaScript target
import { Ok, Error } from "./gleam.mjs";
import { toList } from "./gleam.mjs";

export function parse_json(jsonStr) {
  try {
    const parsed = JSON.parse(jsonStr);
    return new Ok(parsed);
  } catch (e) {
    return new Error("Invalid JSON");
  }
}

export function get_field(obj, field) {
  if (obj && typeof obj === "object" && field in obj) {
    return new Ok(obj[field]);
  }
  return new Error(undefined);
}

export function get_string(value) {
  if (typeof value === "string") {
    return new Ok(value);
  }
  return new Error(undefined);
}

export function get_dict(value) {
  if (value && typeof value === "object" && !Array.isArray(value)) {
    return new Ok(value);
  }
  return new Error(undefined);
}
