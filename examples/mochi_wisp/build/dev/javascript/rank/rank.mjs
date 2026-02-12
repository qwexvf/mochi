import * as $int from "../gleam_stdlib/gleam/int.mjs";

/**
 * Returns the ordinal suffix for a number.
 *
 * # Examples
 *
 * ```gleam
 * suffix(1) // -> "st"
 * suffix(2) // -> "nd"
 * suffix(3) // -> "rd"
 * suffix(4) // -> "th"
 * ```
 */
export function suffix(number) {
  let number$1 = $int.absolute_value(number);
  let hundred = number$1 % 100;
  let $ = number$1 % 10;
  if (((hundred === 11) || (hundred === 12)) || (hundred === 13)) {
    return "th";
  } else if ($ === 1) {
    return "st";
  } else if ($ === 2) {
    return "nd";
  } else if ($ === 3) {
    return "rd";
  } else {
    return "th";
  }
}

/**
 * Convert a number to its ordinal form.
 *
 * # Examples
 *
 * ```gleam
 * ordinalise(1) // -> "1st"
 * ordinalise(2) // -> "2nd"
 * ordinalise(3) // -> "3rd"
 * ordinalise(4) // -> "4th"
 * ```
 */
export function ordinalise(number) {
  return $int.to_string(number) + suffix(number);
}
