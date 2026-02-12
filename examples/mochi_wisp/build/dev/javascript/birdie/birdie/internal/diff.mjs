import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
} from "../../gleam.mjs";

const FILEPATH = "src/birdie/internal/diff.gleam";

export class DiffLine extends $CustomType {
  constructor(number, line, kind) {
    super();
    this.number = number;
    this.line = line;
    this.kind = kind;
  }
}
export const DiffLine$DiffLine = (number, line, kind) =>
  new DiffLine(number, line, kind);
export const DiffLine$isDiffLine = (value) => value instanceof DiffLine;
export const DiffLine$DiffLine$number = (value) => value.number;
export const DiffLine$DiffLine$0 = (value) => value.number;
export const DiffLine$DiffLine$line = (value) => value.line;
export const DiffLine$DiffLine$1 = (value) => value.line;
export const DiffLine$DiffLine$kind = (value) => value.kind;
export const DiffLine$DiffLine$2 = (value) => value.kind;

export class Old extends $CustomType {}
export const DiffLineKind$Old = () => new Old();
export const DiffLineKind$isOld = (value) => value instanceof Old;

export class New extends $CustomType {}
export const DiffLineKind$New = () => new New();
export const DiffLineKind$isNew = (value) => value instanceof New;

export class Shared extends $CustomType {}
export const DiffLineKind$Shared = () => new Shared();
export const DiffLineKind$isShared = (value) => value instanceof Shared;

class One extends $CustomType {
  constructor(times, before, after) {
    super();
    this.times = times;
    this.before = before;
    this.after = after;
  }
}

class Other extends $CustomType {
  constructor(times, before, after) {
    super();
    this.times = times;
    this.before = before;
    this.after = after;
  }
}

class Both extends $CustomType {
  constructor(times, before_one, after_one, before_other, after_other) {
    super();
    this.times = times;
    this.before_one = before_one;
    this.after_one = after_one;
    this.before_other = before_other;
    this.after_other = after_other;
  }
}

function match_diff_lines(
  loop$lines,
  loop$lcs,
  loop$line_one,
  loop$one,
  loop$line_other,
  loop$other
) {
  while (true) {
    let lines = loop$lines;
    let lcs = loop$lcs;
    let line_one = loop$line_one;
    let one = loop$one;
    let line_other = loop$line_other;
    let other = loop$other;
    if (lcs instanceof $Empty) {
      if (one instanceof $Empty) {
        if (other instanceof $Empty) {
          return $list.reverse(lines);
        } else {
          let first = other.head;
          let other$1 = other.tail;
          let _pipe = listPrepend(
            new DiffLine(line_other, first, new New()),
            lines,
          );
          loop$lines = _pipe;
          loop$lcs = lcs;
          loop$line_one = line_one;
          loop$one = one;
          loop$line_other = line_other + 1;
          loop$other = other$1;
        }
      } else {
        let other$1 = other;
        let first = one.head;
        let one$1 = one.tail;
        let _pipe = listPrepend(new DiffLine(line_one, first, new Old()), lines);
        loop$lines = _pipe;
        loop$lcs = lcs;
        loop$line_one = line_one + 1;
        loop$one = one$1;
        loop$line_other = line_other;
        loop$other = other$1;
      }
    } else if (one instanceof $Empty) {
      if (other instanceof $Empty) {
        throw makeError(
          "panic",
          FILEPATH,
          "birdie/internal/diff",
          66,
          "match_diff_lines",
          "unreachable",
          {}
        )
      } else {
        let first_common = lcs.head;
        let first_other = other.head;
        if (first_common !== first_other) {
          let one$1 = one;
          let other$1 = other.tail;
          let _pipe = listPrepend(
            new DiffLine(line_other, first_other, new New()),
            lines,
          );
          loop$lines = _pipe;
          loop$lcs = lcs;
          loop$line_one = line_one;
          loop$one = one$1;
          loop$line_other = line_other + 1;
          loop$other = other$1;
        } else {
          throw makeError(
            "panic",
            FILEPATH,
            "birdie/internal/diff",
            66,
            "match_diff_lines",
            "unreachable",
            {}
          )
        }
      }
    } else {
      let first_common = lcs.head;
      let first_one = one.head;
      if (first_common !== first_one) {
        let other$1 = other;
        let one$1 = one.tail;
        let _pipe = listPrepend(
          new DiffLine(line_one, first_one, new Old()),
          lines,
        );
        loop$lines = _pipe;
        loop$lcs = lcs;
        loop$line_one = line_one + 1;
        loop$one = one$1;
        loop$line_other = line_other;
        loop$other = other$1;
      } else if (other instanceof $Empty) {
        throw makeError(
          "panic",
          FILEPATH,
          "birdie/internal/diff",
          66,
          "match_diff_lines",
          "unreachable",
          {}
        )
      } else {
        let first_common = lcs.head;
        let first_other = other.head;
        if (first_common !== first_other) {
          let one$1 = one;
          let other$1 = other.tail;
          let _pipe = listPrepend(
            new DiffLine(line_other, first_other, new New()),
            lines,
          );
          loop$lines = _pipe;
          loop$lcs = lcs;
          loop$line_one = line_one;
          loop$one = one$1;
          loop$line_other = line_other + 1;
          loop$other = other$1;
        } else {
          let first_common = lcs.head;
          let lcs$1 = lcs.tail;
          let one$1 = one.tail;
          let other$1 = other.tail;
          let _pipe = listPrepend(
            new DiffLine(line_other, first_common, new Shared()),
            lines,
          );
          loop$lines = _pipe;
          loop$lcs = lcs$1;
          loop$line_one = line_one + 1;
          loop$one = one$1;
          loop$line_other = line_other + 1;
          loop$other = other$1;
        }
      }
    }
  }
}

function sum_occurrences(one, other) {
  if (one instanceof One) {
    if (other instanceof One) {
      let n = one.times;
      let m = other.times;
      let before = other.before;
      let after = other.after;
      return new One(n + m, before, after);
    } else if (other instanceof Other) {
      let n = one.times;
      let before_one = one.before;
      let after_one = one.after;
      let m = other.times;
      let before_other = other.before;
      let after_other = other.after;
      return new Both(n + m, before_one, after_one, before_other, after_other);
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "birdie/internal/diff",
        183,
        "sum_occurrences",
        "unreachable: sum_occurrences",
        {}
      )
    }
  } else if (one instanceof Other) {
    if (other instanceof Other) {
      let n = one.times;
      let m = other.times;
      let before = other.before;
      let after = other.after;
      return new Other(n + m, before, after);
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "birdie/internal/diff",
        183,
        "sum_occurrences",
        "unreachable: sum_occurrences",
        {}
      )
    }
  } else if (other instanceof Other) {
    let n = one.times;
    let before_one = one.before_one;
    let after_one = one.after_one;
    let m = other.times;
    let before_other = other.before;
    let after_other = other.after;
    return new Both(n + m, before_one, after_one, before_other, after_other);
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "birdie/internal/diff",
      183,
      "sum_occurrences",
      "unreachable: sum_occurrences",
      {}
    )
  }
}

function histogram_add(
  loop$histogram,
  loop$list,
  loop$to_occurrence,
  loop$reverse_prefix
) {
  while (true) {
    let histogram = loop$histogram;
    let list = loop$list;
    let to_occurrence = loop$to_occurrence;
    let reverse_prefix = loop$reverse_prefix;
    if (list instanceof $Empty) {
      return histogram;
    } else {
      let first = list.head;
      let rest = list.tail;
      let _pipe = $dict.upsert(
        histogram,
        first,
        (previous) => {
          let new_occurrence = to_occurrence(1, reverse_prefix, rest);
          if (previous instanceof Some) {
            let occurrence = previous[0];
            return sum_occurrences(occurrence, new_occurrence);
          } else {
            return new_occurrence;
          }
        },
      );
      loop$histogram = _pipe;
      loop$list = rest;
      loop$to_occurrence = to_occurrence;
      loop$reverse_prefix = listPrepend(first, reverse_prefix);
    }
  }
}

function lowest_occurrence_common_item(one, other) {
  let _block;
  let _pipe = histogram_add(
    $dict.new$(),
    one,
    (var0, var1, var2) => { return new One(var0, var1, var2); },
    toList([]),
  );
  _block = histogram_add(
    _pipe,
    other,
    (var0, var1, var2) => { return new Other(var0, var1, var2); },
    toList([]),
  );
  let histogram$1 = _block;
  return $dict.fold(
    histogram$1,
    new None(),
    (lowest, a, occurs) => {
      if (occurs instanceof One) {
        return lowest;
      } else if (occurs instanceof Other) {
        return lowest;
      } else {
        let n = occurs.times;
        let before_one = occurs.before_one;
        let after_one = occurs.after_one;
        let before_other = occurs.before_other;
        let after_other = occurs.after_other;
        if (lowest instanceof Some) {
          let m = lowest[0][1];
          let $ = m <= n;
          if ($) {
            return lowest;
          } else {
            let _pipe$1 = [
              a,
              n,
              before_one,
              after_one,
              before_other,
              after_other,
            ];
            return new Some(_pipe$1);
          }
        } else {
          return new Some(
            [a, n, before_one, after_one, before_other, after_other],
          );
        }
      }
    },
  );
}

function do_pop_common_prefix(loop$reverse_prefix, loop$one, loop$other) {
  while (true) {
    let reverse_prefix = loop$reverse_prefix;
    let one = loop$one;
    let other = loop$other;
    if (one instanceof $Empty) {
      return [reverse_prefix, one, other];
    } else if (other instanceof $Empty) {
      return [reverse_prefix, one, other];
    } else {
      let first_one = one.head;
      let first_other = other.head;
      if (isEqual(first_one, first_other)) {
        let one$1 = one.tail;
        let other$1 = other.tail;
        loop$reverse_prefix = listPrepend(first_one, reverse_prefix);
        loop$one = one$1;
        loop$other = other$1;
      } else {
        return [reverse_prefix, one, other];
      }
    }
  }
}

/**
 * Returns the common prefix between two lists, and the remaining lists after
 * removing the common prefix from each one.
 * 
 * @ignore
 */
function pop_common_prefix(one, other) {
  let $ = do_pop_common_prefix(toList([]), one, other);
  let reverse_prefix;
  let one$1;
  let other$1;
  reverse_prefix = $[0];
  one$1 = $[1];
  other$1 = $[2];
  return [$list.reverse(reverse_prefix), one$1, other$1];
}

/**
 * Returns the common suffix between two lists, and the remaining lists after
 * removing the common suffix from each one.
 * 
 * @ignore
 */
function pop_common_suffix(one, other) {
  let $ = do_pop_common_prefix(
    toList([]),
    $list.reverse(one),
    $list.reverse(other),
  );
  let suffix;
  let reverse_one;
  let reverse_other;
  suffix = $[0];
  reverse_one = $[1];
  reverse_other = $[2];
  return [suffix, $list.reverse(reverse_one), $list.reverse(reverse_other)];
}

/**
 * Find the least common subsequences of shared items between two lists.
 *
 * Reference: https://tiarkrompf.github.io/notes/?/diff-algorithm/
 * 
 * @ignore
 */
function lcs(one, other) {
  let $ = pop_common_prefix(one, other);
  let prefix;
  let one$1;
  let other$1;
  prefix = $[0];
  one$1 = $[1];
  other$1 = $[2];
  let $1 = pop_common_suffix(one$1, other$1);
  let suffix;
  let one$2;
  let other$2;
  suffix = $1[0];
  one$2 = $1[1];
  other$2 = $1[2];
  let $2 = lowest_occurrence_common_item(one$2, other$2);
  if ($2 instanceof Some) {
    let item = $2[0][0];
    let before_a = $2[0][2];
    let after_a = $2[0][3];
    let before_b = $2[0][4];
    let after_b = $2[0][5];
    return $list.flatten(
      toList([
        prefix,
        lcs($list.reverse(before_a), $list.reverse(before_b)),
        toList([item]),
        lcs(after_a, after_b),
        suffix,
      ]),
    );
  } else {
    return $list.flatten(toList([prefix, suffix]));
  }
}

export function histogram(one, other) {
  let one_lines = $string.split(one, "\n");
  let other_lines = $string.split(other, "\n");
  let lcs$1 = lcs(one_lines, other_lines);
  return match_diff_lines(toList([]), lcs$1, 1, one_lines, 1, other_lines);
}
