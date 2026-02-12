// FFI helpers for mochi
import { Some, None, Ok, Error } from "./gleam.mjs";

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

// Result-returning string extraction for directive handlers
export function try_extract_string(value) {
  if (typeof value === "string") {
    return new Ok(value);
  }
  return new Error("Expected string");
}

// Result-returning int extraction for directive handlers
export function try_extract_int(value) {
  if (typeof value === "number" && Number.isInteger(value)) {
    return new Ok(value);
  }
  return new Error("Expected int");
}

export function get_list_elements(value) {
  if (Array.isArray(value)) {
    return new Some(value);
  }
  return new None();
}

// Extract a field from a dynamic value (object or map)
export function extract_field(data, field) {
  if (data instanceof Map) {
    return data.has(field) ? data.get(field) : null;
  }
  if (data && typeof data === "object") {
    return field in data ? data[field] : null;
  }
  return null;
}

// Check if a value is null or undefined (for null propagation)
export function is_null(value) {
  return value === null || value === undefined;
}
