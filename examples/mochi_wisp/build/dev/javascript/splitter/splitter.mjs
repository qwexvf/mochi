import * as $list from "../gleam_stdlib/gleam/list.mjs";
import { split, split_before, split_after, would_split, make } from "./splitter_ffi.mjs";

export { split, split_after, split_before, would_split };

/**
 * Create a new splitter for a given list of substrings.
 *
 * Substrings are matched for in the order the appear in the list, if one
 * substring is the substring of another place it later in the list than the
 * superstring.
 *
 * Empty strings are discarded, and an empty list will not split off any
 * prefix.
 *
 * There is a small cost to creating a splitter, so if you are going to split
 * a string multiple times, and you want as much performance as possible, then
 * it is better to reuse the same splitter than to create a new one each time.
 */
export function new$(substrings) {
  let _pipe = substrings;
  let _pipe$1 = $list.filter(_pipe, (x) => { return x !== ""; });
  return make(_pipe$1);
}
