// mochi_parallel_ffi.mjs
// JavaScript implementation of parallel_map
// JS is single-threaded so this runs sequentially
import { toList } from "./gleam.mjs";

// Convert a Gleam linked list to a JS array
function gleamListToArray(list) {
  const arr = [];
  let current = list;
  while (current.head !== undefined) {
    arr.push(current.head);
    current = current.tail;
  }
  return arr;
}

export function parallel_map(fns) {
  const arr = gleamListToArray(fns);
  const results = arr.map(fn => fn());
  return toList(results);
}
