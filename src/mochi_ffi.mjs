// FFI helpers for mochi
import { Some, None } from "./gleam.mjs";

export function identity(x) {
  return x;
}

export function dynamic_to_string(value) {
  if (typeof value === "string") {
    return new Some(value);
  }
  return new None();
}

export function dynamic_to_bool(value) {
  if (typeof value === "boolean") {
    return new Some(value);
  }
  return new None();
}

export function dict_has_key(value, key) {
  // Handle both Map and plain objects
  if (value instanceof Map) {
    return value.has(key);
  }
  if (value && typeof value === "object") {
    return key in value;
  }
  return false;
}
