// Unique positive integer for JavaScript
let counter = 0;

export function unique_positive_int() {
  counter += 1;
  return Math.floor(Math.random() * 1_000_000_000) + counter;
}
