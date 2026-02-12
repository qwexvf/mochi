import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";

export class Version extends $CustomType {
  constructor(major, minor, patch) {
    super();
    this.major = major;
    this.minor = minor;
    this.patch = patch;
  }
}
export const Version$Version = (major, minor, patch) =>
  new Version(major, minor, patch);
export const Version$isVersion = (value) => value instanceof Version;
export const Version$Version$major = (value) => value.major;
export const Version$Version$0 = (value) => value.major;
export const Version$Version$minor = (value) => value.minor;
export const Version$Version$1 = (value) => value.minor;
export const Version$Version$patch = (value) => value.patch;
export const Version$Version$2 = (value) => value.patch;

export function new$(major, minor, patch) {
  return new Version(major, minor, patch);
}

export function parse(version) {
  let $ = (() => {
    let _pipe = version;
    let _pipe$1 = $string.trim(_pipe);
    return $string.split(_pipe$1, ".");
  })();
  if ($ instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      return new Error(undefined);
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        return new Error(undefined);
      } else {
        let $3 = $2.tail;
        if ($3 instanceof $Empty) {
          let major = $.head;
          let minor = $1.head;
          let patch = $2.head;
          return $result.try$(
            $int.parse(major),
            (major) => {
              return $result.try$(
                $int.parse(minor),
                (minor) => {
                  return $result.try$(
                    $int.parse(patch),
                    (patch) => {
                      return new Ok(new Version(major, minor, patch));
                    },
                  );
                },
              );
            },
          );
        } else {
          return new Error(undefined);
        }
      }
    }
  }
}

export function compare(one, other) {
  return $order.lazy_break_tie(
    $int.compare(one.major, other.major),
    () => {
      return $order.lazy_break_tie(
        $int.compare(one.minor, other.minor),
        () => { return $int.compare(one.patch, other.patch); },
      );
    },
  );
}
