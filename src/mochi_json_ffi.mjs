// FFI helpers for JSON encoding in mochi
import { Some, None } from "./gleam.mjs";
import { toList } from "./gleam.mjs";

// Check if value is null/undefined/Nil
export function is_null(value) {
  return value === null || value === undefined || value === null;
}

// Check if value is a boolean
export function is_bool(value) {
  return typeof value === "boolean";
}

// Check if value is an integer
export function is_int(value) {
  return typeof value === "number" && Number.isInteger(value);
}

// Check if value is a float (non-integer number)
export function is_float(value) {
  return typeof value === "number";
}

// Check if value is a string
export function is_string(value) {
  return typeof value === "string";
}

// Check if value is a list/array
export function is_list(value) {
  // Gleam lists are linked lists with head/tail structure
  if (Array.isArray(value)) {
    return true;
  }
  // Check for Gleam's linked list structure
  if (value && typeof value === "object") {
    // Empty list
    if (value.constructor && value.constructor.name === "Empty") {
      return true;
    }
    // Non-empty list (has head and tail)
    if ("head" in value && "tail" in value) {
      return true;
    }
  }
  return false;
}

// Check if value is a dict/map/object
export function is_dict(value) {
  // Gleam dicts are Map objects
  if (value instanceof Map) {
    return true;
  }
  // Check for plain objects (but not arrays, nulls, or Gleam types)
  if (
    value &&
    typeof value === "object" &&
    !Array.isArray(value) &&
    !is_list(value) &&
    !is_option(value) &&
    value.constructor === Object
  ) {
    return true;
  }
  return false;
}

// Check if value is an Option (Some/None)
export function is_option(value) {
  if (!value || typeof value !== "object") {
    return false;
  }
  return value instanceof Some || value instanceof None;
}

// Get boolean value
export function get_bool(value) {
  return value;
}

// Get integer value
export function get_int(value) {
  return value;
}

// Get float value
export function get_float(value) {
  return value;
}

// Get string value
export function get_string(value) {
  return value;
}

// Get list elements as Gleam list
export function get_list(value) {
  if (Array.isArray(value)) {
    return toList(value);
  }
  // Convert Gleam linked list to array then back to Gleam list
  const elements = [];
  let current = value;
  while (current && current.head !== undefined) {
    elements.push(current.head);
    current = current.tail;
  }
  return toList(elements);
}

// Get dict entries as list of tuples
export function get_dict_entries(value) {
  const entries = [];
  if (value instanceof Map) {
    for (const [k, v] of value.entries()) {
      entries.push([k, v]);
    }
  } else if (value && typeof value === "object") {
    for (const [k, v] of Object.entries(value)) {
      entries.push([k, v]);
    }
  }
  return toList(entries);
}

// Unwrap Option - returns inner value or null
export function unwrap_option(value) {
  if (value instanceof Some) {
    return value[0];
  }
  return null;
}
