import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "./gleam.mjs";

/**
 * The trie constructor, its implementation is based on the one described by Okasaki in
 * Purely Functional Data Structures.
 * 
 * @ignore
 */
class Trie extends $CustomType {
  constructor(entry, children_map) {
    super();
    this.entry = entry;
    this.children_map = children_map;
  }
}

/**
 * Exactly same behaviour as delete but returns `None` if the tree is empty as a
 * result of the deletion.
 * 
 * @ignore
 */
function do_delete(trie, path) {
  if (path instanceof $Empty) {
    let children_map = trie.children_map;
    let $ = $dict.size(children_map);
    if ($ === 0) {
      return new None();
    } else {
      return new Some(new Trie(new None(), children_map));
    }
  } else {
    let entry = trie.entry;
    let children_map = trie.children_map;
    let first = path.head;
    let rest = path.tail;
    let _block;
    let $ = $dict.get(children_map, first);
    if ($ instanceof Ok) {
      let child = $[0];
      let $1 = do_delete(child, rest);
      if ($1 instanceof Some) {
        let trie$1 = $1[0];
        _block = $dict.insert(children_map, first, trie$1);
      } else {
        _block = $dict.delete$(children_map, first);
      }
    } else {
      _block = children_map;
    }
    let new_children = _block;
    let $1 = $dict.size(new_children);
    if (entry instanceof None && $1 === 0) {
      return entry;
    } else {
      return new Some(new Trie(entry, new_children));
    }
  }
}

/**
 * Combines all the trie's values into a single one by calling a given function on each one.
 *
 * The function takes as input the accumulator, the path of a value and the corresponding value.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], 10), #([1], 1)]
 * > |> from_list
 * > |> fold(from: 0, with: fn(sum, _, value) { sum + value })
 * 11
 * ```
 */
export function fold(trie, initial, fun) {
  return $dict.fold(
    trie.children_map,
    (() => {
      let _pipe = trie.entry;
      let _pipe$1 = $option.map(
        _pipe,
        (_capture) => { return fun(initial, toList([]), _capture); },
      );
      return $option.unwrap(_pipe$1, initial);
    })(),
    (acc, first, trie) => {
      return fold(
        trie,
        acc,
        (acc, rest, value) => {
          return fun(acc, listPrepend(first, rest), value);
        },
      );
    },
  );
}

/**
 * Fetches a value from a trie for a given path.
 * If a value is present at the given path it returns it wrapped in an `Ok`,
 * otherwise it returns `Error(Nil)`.
 *
 * ## Examples
 *
 * ```gleam
 * > new()
 * > |> get(at: [1, 2])
 * Result(Nil)
 * ```
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> get(at: [1, 2])
 * Ok("a")
 * ```
 */
export function get(from, path) {
  if (path instanceof $Empty) {
    let $ = from.entry;
    if ($ instanceof Some) {
      let value = $[0];
      return new Ok(value);
    } else {
      return new Error(undefined);
    }
  } else {
    let children_map = from.children_map;
    let first = path.head;
    let rest = path.tail;
    let _pipe = children_map;
    let _pipe$1 = $dict.get(_pipe, first);
    return $result.try$(_pipe$1, (_capture) => { return get(_capture, rest); });
  }
}

/**
 * Determines wether a trie contains a value associated with the given path.
 *
 * ## Examples
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> has_path([1, 2])
 * True
 * ```
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> has_path([1])
 * False
 * ```
 */
export function has_path(trie, path) {
  let $ = get(trie, path);
  if ($ instanceof Ok) {
    return true;
  } else {
    return false;
  }
}

/**
 * Updates all the values in a given trie by calling a function on each value.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> map(fn(s) { s <> "!" })
 * > |> to_list
 * [#([1, 2], "a!"), #([1], "b!")]
 * ```
 */
export function map(trie, fun) {
  return new Trie(
    $option.map(trie.entry, fun),
    $dict.map_values(trie.children_map, (_, t) => { return map(t, fun); }),
  );
}

/**
 * Creates a new empty trie.
 *
 * ## Examples
 *
 * ```gleam
 * > new()
 * > |> to_list
 * []
 * ```
 */
export function new$() {
  return new Trie(new None(), $dict.new$());
}

/**
 * Deletes from a trie the value associated with a given path.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> delete(at: [1, 2])
 * > |> to_list
 * [#([1], "b")]
 * ```
 *
 * ```gleam
 * > new()
 * > |> delete(at: [1, 2])
 * > |> to_list
 * []
 * ```
 */
export function delete$(trie, path) {
  let _pipe = do_delete(trie, path);
  return $option.unwrap(_pipe, new$());
}

/**
 * Inserts a value in a trie at a given path. If there already is a value
 * at the given path it is replaced by the new one.
 *
 * ## Examples
 *
 * ```gleam
 * > new()
 * > |> insert(at: [1, 2], value: "a")
 * > |> insert(at: [1], value: "b")
 * > |> to_list
 * [#([1, 2], "a"), #([1], "b")]
 * ```
 *
 * ```gleam
 * > new()
 * > |> insert(at: [1, 2], value: "a")
 * > |> insert(at: [1, 2], value: "b")
 * > |> to_list
 * [#([1, 2], "b")]
 * ```
 */
export function insert(trie, path, value) {
  if (path instanceof $Empty) {
    let children_map = trie.children_map;
    return new Trie(new Some(value), children_map);
  } else {
    let entry = trie.entry;
    let children_map = trie.children_map;
    let first = path.head;
    let rest = path.tail;
    let _pipe = $dict.get(children_map, first);
    let _pipe$1 = $result.unwrap(_pipe, new$());
    let _pipe$2 = insert(_pipe$1, rest, value);
    let _pipe$3 = ((_capture) => {
      return $dict.insert(children_map, first, _capture);
    })(_pipe$2);
    return ((_capture) => { return new Trie(entry, _capture); })(_pipe$3);
  }
}

/**
 * Creates a new trie from a list of path-value pairs.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> to_list
 * [#([1, 2], "a"), #([1], "b")]
 * ```
 */
export function from_list(list) {
  return $list.fold(
    list,
    new$(),
    (trie, pair) => { return insert(trie, pair[0], pair[1]); },
  );
}

/**
 * Gets a list of all the valid paths in the trie. That is all the paths associated with a value.
 *
 * Tries are not ordered so the paths are not returned in any specific order.
 * Do not write code that relies on the order paths are returned by this function
 * as it may change in later versions of the library.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> paths
 * [[1, 2], [1]]
 * ```
 *
 * ```gleam
 * > new()
 * > |> paths
 * []
 * ```
 */
export function paths(trie) {
  return fold(
    trie,
    toList([]),
    (rest, path, _) => { return listPrepend(path, rest); },
  );
}

/**
 * Creates a new trie with a single value associated to the given path.
 *
 * ## Examples
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> to_list
 * [#([1, 2], "a")]
 * ```
 */
export function singleton(path, value) {
  return insert(new$(), path, value);
}

/**
 * Gets the number of elements in the trie.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> size
 * 2
 * ```
 */
export function size(trie) {
  return fold(trie, 0, (acc, _, _1) => { return acc + 1; });
}

/**
 * Determines wether or not the trie is empty.
 *
 * ## Examples
 *
 * ```gleam
 * > new()
 * > |> is_empty
 * True
 * ```
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> is_empty
 * False
 * ```
 */
export function is_empty(trie) {
  return size(trie) === 0;
}

/**
 * Gets the subtrie whose elements all share a common given prefix.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2, 3], "a"), #([1, 2, 4, 5], "b"), #([3, 4], "c")]
 * > |> from_list
 * > |> subtrie(at: [1, 2])
 * > |> to_list
 * [#([1, 2, 3], "a"), #([1, 2, 4, 5], "b")]
 * ```
 */
export function subtrie(trie, prefix) {
  if (prefix instanceof $Empty) {
    return new Ok(trie);
  } else {
    let first = prefix.head;
    let rest = prefix.tail;
    let children_map = trie.children_map;
    let _pipe = children_map;
    let _pipe$1 = $dict.get(_pipe, first);
    let _pipe$2 = $result.try$(
      _pipe$1,
      (_capture) => { return subtrie(_capture, rest); },
    );
    return $result.map(
      _pipe$2,
      (subtrie) => {
        let _pipe$3 = $dict.new$();
        let _pipe$4 = $dict.insert(_pipe$3, first, subtrie);
        return ((_capture) => { return new Trie(new None(), _capture); })(
          _pipe$4,
        );
      },
    );
  }
}

/**
 * Turns a trie into a list of path-value pairs.
 *
 * ## Examples
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> to_list
 * [#([1, 2], "a")]
 * ```
 *
 * ```gleam
 * > new()
 * > |> to_list
 * []
 * ```
 */
export function to_list(trie) {
  return fold(
    trie,
    toList([]),
    (rest, path, value) => { return listPrepend([path, value], rest); },
  );
}

/**
 * Exactly same behaviour as update but returns `None` if the tree is empty as a
 * result of the (possible) deletion.
 * 
 * @ignore
 */
function do_update(trie, path, fun) {
  if (path instanceof $Empty) {
    let entry = trie.entry;
    let children_map = trie.children_map;
    let $ = fun(entry);
    let $1 = $dict.size(children_map);
    if ($ instanceof None && $1 === 0) {
      return $;
    } else {
      let new_entry = $;
      return new Some(new Trie(new_entry, children_map));
    }
  } else {
    let entry = trie.entry;
    let children_map = trie.children_map;
    let first = path.head;
    let rest = path.tail;
    let _block;
    let $ = $dict.get(children_map, first);
    if ($ instanceof Ok) {
      let child = $[0];
      let $1 = do_update(child, rest, fun);
      if ($1 instanceof Some) {
        let new_child = $1[0];
        _block = $dict.insert(children_map, first, new_child);
      } else {
        _block = $dict.delete$(children_map, first);
      }
    } else {
      let $1 = fun(new None());
      if ($1 instanceof Some) {
        let value = $1[0];
        _block = $dict.insert(children_map, first, singleton(rest, value));
      } else {
        _block = children_map;
      }
    }
    let new_children = _block;
    let $1 = $dict.size(new_children);
    if (entry instanceof None && $1 === 0) {
      return entry;
    } else {
      return new Some(new Trie(entry, new_children));
    }
  }
}

/**
 * Updates the value associated with a path applying it the given function.
 * If there is no value associated with the given path the function is passed `None`.
 *
 * If the function returns `None` any value associated with the path is deleted from the trie.
 * If the function returns `Some(value)` then the new value is associated to the given path.
 *
 * ## Examples
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> update(at: [1, 2], with: fn(n) { n |> option.map(fn(_) { "b" }) })
 * > |> to_list
 * [#([1, 2], "b")]
 * ```
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> update(at: [1, 2], with: fn(_) { None })
 * > |> to_list
 * []
 * ```
 *
 * ```gleam
 * > singleton([1, 2], "a")
 * > |> update(at: [1], with: fn(_) { Some("b") })
 * > |> to_list
 * [#([1, 2], "a"), #([1], "b")]
 * ```
 */
export function update(trie, path, fun) {
  let _pipe = do_update(trie, path, fun);
  return $option.unwrap(_pipe, new$());
}

/**
 * Gets a list of all the values in a given trie.
 *
 * Tries are not ordered so the values are not returned in any specific order.
 * Do not write code that relies on the order values are returned by this function
 * as it may change in later versions of the library.
 *
 * ## Examples
 *
 * ```gleam
 * > [#([1, 2], "a"), #([1], "b")]
 * > |> from_list
 * > |> values
 * ["a", "b"]
 * ```
 *
 * ```gleam
 * > new()
 * > |> values
 * []
 * ```
 */
export function values(trie) {
  return fold(
    trie,
    toList([]),
    (values, _, value) => { return listPrepend(value, values); },
  );
}
