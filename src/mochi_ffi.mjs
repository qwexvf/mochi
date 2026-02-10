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
