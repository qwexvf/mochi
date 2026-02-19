// mochi_parallel_ffi.mjs
// JavaScript implementation of parallel_map using Promise.all
// Falls back to sequential execution (JS is single-threaded)

export async function parallel_map(fns) {
  const results = await Promise.all(fns.map(fn => Promise.resolve(fn(undefined))));
  return results;
}
