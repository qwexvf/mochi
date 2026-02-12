import { Ok } from "./gleam.mjs";
import { terminal_size as get } from "./term_size_ffi.mjs";

export { get };

/**
 * Get the number of rows (lines) visible in the terminal.
 */
export function rows() {
  let $ = get();
  if ($ instanceof Ok) {
    let rows$1 = $[0][0];
    return new Ok(rows$1);
  } else {
    return $;
  }
}

/**
 * Get the character width of the terminal.
 */
export function columns() {
  let $ = get();
  if ($ instanceof Ok) {
    let columns$1 = $[0][1];
    return new Ok(columns$1);
  } else {
    return $;
  }
}
