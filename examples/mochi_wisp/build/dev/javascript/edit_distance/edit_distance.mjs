import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { Ok, toList, Empty as $Empty, prepend as listPrepend, makeError } from "./gleam.mjs";

const FILEPATH = "src/edit_distance.gleam";

function update_distances(
  loop$grapheme,
  loop$other,
  loop$previous_distances,
  loop$last_distance,
  loop$new_distances
) {
  while (true) {
    let grapheme = loop$grapheme;
    let other = loop$other;
    let previous_distances = loop$previous_distances;
    let last_distance = loop$last_distance;
    let new_distances = loop$new_distances;
    if (other instanceof $Empty) {
      return $list.reverse(new_distances);
    } else if (previous_distances instanceof $Empty) {
      return $list.reverse(new_distances);
    } else {
      let $ = previous_distances.tail;
      if ($ instanceof $Empty) {
        return $list.reverse(new_distances);
      } else {
        let first = other.head;
        let other$1 = other.tail;
        let previous_distance = previous_distances.head;
        let rest = $;
        let second_distance;
        if (rest instanceof $Empty) {
          throw makeError(
            "let_assert",
            FILEPATH,
            "edit_distance",
            101,
            "update_distances",
            "Pattern match failed, no pattern matched the value.",
            {
              value: rest,
              start: 3799,
              end: 3838,
              pattern_start: 3810,
              pattern_end: 3831
            }
          )
        } else {
          second_distance = rest.head;
        }
        let insertion_distance = last_distance + 1;
        let deletion_distance = second_distance + 1;
        let _block;
        let $1 = grapheme === first;
        if ($1) {
          _block = previous_distance;
        } else {
          _block = previous_distance + 1;
        }
        let substitution_distance = _block;
        let _block$1;
        let _pipe = substitution_distance;
        let _pipe$1 = $int.min(_pipe, insertion_distance);
        _block$1 = $int.min(_pipe$1, deletion_distance);
        let new_distance = _block$1;
        let new_distances$1 = listPrepend(new_distance, new_distances);
        loop$grapheme = grapheme;
        loop$other = other$1;
        loop$previous_distances = rest;
        loop$last_distance = new_distance;
        loop$new_distances = new_distances$1;
      }
    }
  }
}

function levenshtein_loop(
  loop$one,
  loop$other,
  loop$distances,
  loop$prefix_size
) {
  while (true) {
    let one = loop$one;
    let other = loop$other;
    let distances = loop$distances;
    let prefix_size = loop$prefix_size;
    if (one instanceof $Empty) {
      let $ = $list.last(distances);
      let distance;
      if ($ instanceof Ok) {
        distance = $[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "edit_distance",
          52,
          "levenshtein_loop",
          "distance list will always have at least one item",
          {
            value: $,
            start: 1910,
            end: 1956,
            pattern_start: 1921,
            pattern_end: 1933
          }
        )
      }
      return distance;
    } else {
      let first = one.head;
      let rest = one.tail;
      let prefix_size$1 = prefix_size + 1;
      let new_distances = toList([prefix_size$1]);
      let distance_list = update_distances(
        first,
        other,
        distances,
        prefix_size$1,
        new_distances,
      );
      loop$one = rest;
      loop$other = other;
      loop$distances = distance_list;
      loop$prefix_size = prefix_size$1;
    }
  }
}

/**
 * Compute the edit distance between two strings using the
 * [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance).
 * The Levenshtein distance between two strings is the number of edits that
 * will get you from one string to the other; the allowed edits are:
 * - insertion: adding a new character to one of the two strings
 * - deletion: removing a character from one of the two strings
 * - replacemente: replace a character with a new one in one of the two strings
 *
 * ## Examples
 *
 * ```gleam
 * assert 2 == levenshtein("gleam", "beam")
 * assert 1 == levenshtein("cat", "cap")
 * ```
 */
export function levenshtein(one, other) {
  if (one === other) {
    return 0;
  } else if (one === "") {
    let string = other;
    return $string.length(string);
  } else if (other === "") {
    let string = one;
    return $string.length(string);
  } else {
    let one$1 = one;
    let other$1 = other;
    let one$2 = $string.to_graphemes(one$1);
    let other$2 = $string.to_graphemes(other$1);
    let distance_list = $list.range(0, $list.length(other$2));
    return levenshtein_loop(one$2, other$2, distance_list, 0);
  }
}
