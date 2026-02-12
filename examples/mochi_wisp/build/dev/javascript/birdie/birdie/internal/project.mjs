import * as $filepath from "../../../filepath/filepath.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Ok, toList, makeError } from "../../gleam.mjs";

const FILEPATH = "src/birdie/internal/project.gleam";

function do_find_root(loop$path) {
  while (true) {
    let path = loop$path;
    let manifest = $filepath.join(path, "gleam.toml");
    let $ = $simplifile.is_file(manifest);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1) {
        return new Ok(path);
      } else {
        loop$path = $filepath.join(path, "..");
      }
    } else {
      return $;
    }
  }
}

/**
 * Returns the path to the project's root.
 *
 * > ⚠️ This assumes that this is only ever run inside a Gleam's project and
 * > sooner or later it will reach a `gleam.toml` file.
 * > Otherwise this will end up in an infinite loop, I think.
 */
export function find_root() {
  return do_find_root(".");
}

/**
 * Returns the project's name as specified in its `gleam.toml`.
 */
export function name() {
  return $result.try$(
    find_root(),
    (root) => {
      return $result.try$(
        $simplifile.read($filepath.join(root, "gleam.toml")),
        (file) => {
          let $ = $tom.parse(file);
          let toml;
          if ($ instanceof Ok) {
            toml = $[0];
          } else {
            throw makeError(
              "let_assert",
              FILEPATH,
              "birdie/internal/project",
              30,
              "name",
              "running birdie in a gleam project with an invalid `gleam.toml` should be impossible",
              {
                value: $,
                start: 915,
                end: 952,
                pattern_start: 926,
                pattern_end: 934
              }
            )
          }
          let $1 = $tom.get_string(toml, toList(["name"]));
          let name$1;
          if ($1 instanceof Ok) {
            name$1 = $1[0];
          } else {
            throw makeError(
              "let_assert",
              FILEPATH,
              "birdie/internal/project",
              32,
              "name",
              "`name` is a required field in `gleam.toml`, it should be impossible to run birdie on a project that doesn't have one",
              {
                value: $1,
                start: 1048,
                end: 1100,
                pattern_start: 1059,
                pattern_end: 1067
              }
            )
          }
          return new Ok(name$1);
        },
      );
    },
  );
}
