export function new_(maxSize) {
  return { table: new Map(), maxSize }
}

export function get(cache, key) {
  const val = cache.table.get(key)
  if (val !== undefined) {
    return new Ok(val)
  }
  return new Error(undefined)
}

export function put(cache, key, value) {
  if (cache.table.size < cache.maxSize) {
    cache.table.set(key, value)
  }
  return undefined
}

export function size(cache) {
  return cache.table.size
}
