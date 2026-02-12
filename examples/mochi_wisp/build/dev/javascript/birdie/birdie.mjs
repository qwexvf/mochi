import * as $argv from "../argv/argv.mjs";
import * as $envoy from "../envoy/envoy.mjs";
import * as $filepath from "../filepath/filepath.mjs";
import * as $ansi from "../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $set from "../gleam_stdlib/gleam/set.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $global_value from "../global_value/global_value.mjs";
import * as $justin from "../justin/justin.mjs";
import * as $rank from "../rank/rank.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { Eexist, Enoent } from "../simplifile/simplifile.mjs";
import * as $term_size from "../term_size/term_size.mjs";
import * as $cli from "./birdie/internal/cli.mjs";
import {
  Accept,
  CheckStale,
  DeleteStale,
  FullCommand,
  Help,
  MissingSubcommand,
  Reject,
  Review,
  Stale,
  UnexpectedArgument,
  UnknownCommand,
  UnknownOption,
  UnknownSubcommand,
  WithHelpOption,
} from "./birdie/internal/cli.mjs";
import * as $diff from "./birdie/internal/diff.mjs";
import { DiffLine } from "./birdie/internal/diff.mjs";
import * as $project from "./birdie/internal/project.mjs";
import * as $titles from "./birdie/internal/titles.mjs";
import * as $version from "./birdie/internal/version.mjs";
import { is_windows, halt as exit, get_line } from "./birdie_ffi.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
} from "./gleam.mjs";

const FILEPATH = "src/birdie.gleam";

class SnapshotWithEmptyTitle extends $CustomType {}

class CannotCreateSnapshotsFolder extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class CannotReadAcceptedSnapshot extends $CustomType {
  constructor(reason, source) {
    super();
    this.reason = reason;
    this.source = source;
  }
}

class CannotReadNewSnapshot extends $CustomType {
  constructor(reason, source) {
    super();
    this.reason = reason;
    this.source = source;
  }
}

class CannotSaveNewSnapshot extends $CustomType {
  constructor(reason, title, destination) {
    super();
    this.reason = reason;
    this.title = title;
    this.destination = destination;
  }
}

class CannotReadSnapshots extends $CustomType {
  constructor(reason, folder) {
    super();
    this.reason = reason;
    this.folder = folder;
  }
}

class CannotRejectSnapshot extends $CustomType {
  constructor(reason, snapshot) {
    super();
    this.reason = reason;
    this.snapshot = snapshot;
  }
}

class CannotAcceptSnapshot extends $CustomType {
  constructor(reason, snapshot) {
    super();
    this.reason = reason;
    this.snapshot = snapshot;
  }
}

class CannotReadUserInput extends $CustomType {}

class CorruptedSnapshot extends $CustomType {
  constructor(source) {
    super();
    this.source = source;
  }
}

class CannotFindProjectRoot extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class CannotCreateReferencedFile extends $CustomType {
  constructor(file, reason) {
    super();
    this.file = file;
    this.reason = reason;
  }
}

class CannotReadReferencedFile extends $CustomType {
  constructor(file, reason) {
    super();
    this.file = file;
    this.reason = reason;
  }
}

class CannotMarkSnapshotAsReferenced extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class StaleSnapshotsFound extends $CustomType {
  constructor(stale_snapshots) {
    super();
    this.stale_snapshots = stale_snapshots;
  }
}

class CannotDeleteStaleSnapshot extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class MissingReferencedFile extends $CustomType {}

class CannotGetTitles extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class CannotFigureOutProjectName extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class Snapshot extends $CustomType {
  constructor(title, content, info) {
    super();
    this.title = title;
    this.content = content;
    this.info = info;
  }
}

class NewSnapshotCreated extends $CustomType {
  constructor(snapshot, destination) {
    super();
    this.snapshot = snapshot;
    this.destination = destination;
  }
}

class Different extends $CustomType {
  constructor(accepted, new$) {
    super();
    this.accepted = accepted;
    this.new = new$;
  }
}

class Same extends $CustomType {}

class InfoLineWithTitle extends $CustomType {
  constructor(content, split, title) {
    super();
    this.content = content;
    this.split = split;
    this.title = title;
  }
}

class InfoLineWithNoTitle extends $CustomType {
  constructor(content, split) {
    super();
    this.content = content;
    this.split = split;
  }
}

class DoNotSplit extends $CustomType {}

class SplitWords extends $CustomType {}

class Truncate extends $CustomType {}

class Yes extends $CustomType {}

class No extends $CustomType {}

class ShowDiff extends $CustomType {}

class HideDiff extends $CustomType {}

class AcceptSnapshot extends $CustomType {}

class RejectSnapshot extends $CustomType {}

class SkipSnapshot extends $CustomType {}

class ToggleDiffView extends $CustomType {}

const birdie_version = "1.5.4";

const hint_review_message = "run `gleam run -m birdie` to review the snapshots";

const accepted_extension = "accepted";

const new_extension = "new";

function get_temp_directory() {
  let temp = $result.lazy_or(
    $envoy.get("TMPDIR"),
    () => {
      return $result.lazy_or(
        $envoy.get("TEMP"),
        () => { return $envoy.get("TMP"); },
      );
    },
  );
  if (temp instanceof Ok) {
    let temp$1 = temp[0];
    return temp$1;
  } else {
    let $ = is_windows();
    if ($) {
      return "C:\\TMP";
    } else {
      return "/tmp";
    }
  }
}

function referenced_file_path() {
  return $result.try$(
    (() => {
      let _pipe = $project.name();
      return $result.map_error(
        _pipe,
        (var0) => { return new CannotFigureOutProjectName(var0); },
      );
    })(),
    (name) => {
      return new Ok(
        $filepath.join(get_temp_directory(), name + "_referenced.txt"),
      );
    },
  );
}

/**
 * Returns the path to the referenced file, initialising it to be empty only
 * the first time this function is called.
 * 
 * @ignore
 */
function global_referenced_file() {
  return $global_value.create_with_unique_name(
    "birdie.referenced_file",
    () => {
      return $result.try$(
        referenced_file_path(),
        (referenced_file) => {
          let $ = $simplifile.create_file(referenced_file);
          if ($ instanceof Ok) {
            return new Ok(referenced_file);
          } else {
            let $1 = $[0];
            if ($1 instanceof Eexist) {
              let _pipe = $simplifile.write(referenced_file, "");
              let _pipe$1 = $result.replace(_pipe, referenced_file);
              return $result.map_error(
                _pipe$1,
                (_capture) => {
                  return new CannotCreateReferencedFile(
                    referenced_file,
                    _capture,
                  );
                },
              );
            } else {
              let reason = $1;
              return new Error(
                new CannotCreateReferencedFile(referenced_file, reason),
              );
            }
          }
        },
      );
    },
  );
}

/**
 * Finds the snapshots folder at the root of the project the command is run
 * into. If it's not present the folder is created automatically.
 * 
 * @ignore
 */
function snapshot_folder() {
  return $global_value.create_with_unique_name(
    "birdie.snapshot_folder",
    () => {
      let result = $result.map_error(
        $project.find_root(),
        (var0) => { return new CannotFindProjectRoot(var0); },
      );
      return $result.try$(
        result,
        (project_root) => {
          let snapshots_folder = $filepath.join(
            project_root,
            "birdie_snapshots",
          );
          let $ = $simplifile.create_directory(snapshots_folder);
          if ($ instanceof Ok) {
            return new Ok(snapshots_folder);
          } else {
            let $1 = $[0];
            if ($1 instanceof Eexist) {
              return new Ok(snapshots_folder);
            } else {
              let error = $1;
              return new Error(new CannotCreateSnapshotsFolder(error));
            }
          }
        },
      );
    },
  );
}

function validate_snapshot_title(title) {
  let $ = $string.trim(title);
  if ($ === "") {
    return new Error(new SnapshotWithEmptyTitle());
  } else {
    return new Ok(undefined);
  }
}

function to_diff_lines(accepted, new$) {
  let accepted_content;
  accepted_content = accepted.content;
  let new_content;
  new_content = new$.content;
  return $diff.histogram(accepted_content, new_content);
}

function split_n(string, n, separator) {
  let $ = n <= 0;
  if ($) {
    return new Ok([toList([]), string]);
  } else {
    return $result.try$(
      $string.split_once(string, separator),
      (_use0) => {
        let line;
        let rest;
        line = _use0[0];
        rest = _use0[1];
        return $result.try$(
          split_n(rest, n - 1, separator),
          (_use0) => {
            let lines;
            let rest$1;
            lines = _use0[0];
            rest$1 = _use0[1];
            return new Ok([listPrepend(line, lines), rest$1]);
          },
        );
      },
    );
  }
}

function trim_end_once(string, substring) {
  let $ = $string.ends_with(string, substring);
  if ($) {
    return $string.drop_end(string, $string.length(substring));
  } else {
    return string;
  }
}

/**
 * Birdie started adding newlines to the end of files starting from `1.4.0`,
 * so if we're reading a snapshot created from `1.4.0` onwards then we want to
 * make sure to remove that newline!
 * 
 * @ignore
 */
function trim_content(content, version) {
  let $ = $version.parse(version);
  let version$1;
  if ($ instanceof Ok) {
    version$1 = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "birdie",
      367,
      "trim_content",
      "corrupt birdie version",
      {
        value: $,
        start: 11561,
        end: 11608,
        pattern_start: 11572,
        pattern_end: 11583
      }
    )
  }
  let $1 = $version.compare(version$1, $version.new$(1, 4, 0));
  if ($1 instanceof $order.Lt) {
    return content;
  } else if ($1 instanceof $order.Eq) {
    return trim_end_once(content, "\n");
  } else {
    return trim_end_once(content, "\n");
  }
}

function deserialise(raw) {
  let $ = split_n(raw, 4, "\n");
  if ($ instanceof Ok) {
    let $1 = $[0][0];
    if ($1 instanceof $Empty) {
      let $2 = split_n(raw, 6, "\n");
      if ($2 instanceof Ok) {
        let $3 = $2[0][0];
        if ($3 instanceof $Empty) {
          return new Error(undefined);
        } else {
          let $4 = $3.tail;
          if ($4 instanceof $Empty) {
            return new Error(undefined);
          } else {
            let $5 = $4.tail;
            if ($5 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $6 = $5.tail;
              if ($6 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $7 = $6.tail;
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      let $10 = $3.head;
                      if ($10 === "---") {
                        let $11 = $4.head;
                        if ($11.startsWith("version: ")) {
                          let $12 = $5.head;
                          if ($12.startsWith("title: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("file: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("test_name: ")) {
                                let $15 = $8.head;
                                if ($15 === "---") {
                                  let content = $2[0][1];
                                  let version = $11.slice(9);
                                  let title = $12.slice(7);
                                  let file = $13.slice(6);
                                  let test_name = $14.slice(11);
                                  return new Ok(
                                    new Snapshot(
                                      $string.trim(title),
                                      trim_content(content, version),
                                      new Some(
                                        new $titles.TestInfo(
                                          $string.trim(file),
                                          $string.trim(test_name),
                                        ),
                                      ),
                                    ),
                                  );
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      } else if ($10 === "---\r") {
                        let $11 = $4.head;
                        if ($11.startsWith("version: ")) {
                          let $12 = $5.head;
                          if ($12.startsWith("title: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("file: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("test_name: ")) {
                                let $15 = $8.head;
                                if ($15 === "---\r") {
                                  let content = $2[0][1];
                                  let version = $11.slice(9);
                                  let title = $12.slice(7);
                                  let file = $13.slice(6);
                                  let test_name = $14.slice(11);
                                  return new Ok(
                                    new Snapshot(
                                      $string.trim(title),
                                      trim_content(content, version),
                                      new Some(
                                        new $titles.TestInfo(
                                          $string.trim(file),
                                          $string.trim(test_name),
                                        ),
                                      ),
                                    ),
                                  );
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      } else {
                        return new Error(undefined);
                      }
                    } else {
                      return new Error(undefined);
                    }
                  }
                }
              }
            }
          }
        }
      } else {
        return new Error(undefined);
      }
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        let $3 = split_n(raw, 6, "\n");
        if ($3 instanceof Ok) {
          let $4 = $3[0][0];
          if ($4 instanceof $Empty) {
            return new Error(undefined);
          } else {
            let $5 = $4.tail;
            if ($5 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $6 = $5.tail;
              if ($6 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $7 = $6.tail;
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        let $11 = $4.head;
                        if ($11 === "---") {
                          let $12 = $5.head;
                          if ($12.startsWith("version: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("title: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("file: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("test_name: ")) {
                                  let $16 = $9.head;
                                  if ($16 === "---") {
                                    let content = $3[0][1];
                                    let version = $12.slice(9);
                                    let title = $13.slice(7);
                                    let file = $14.slice(6);
                                    let test_name = $15.slice(11);
                                    return new Ok(
                                      new Snapshot(
                                        $string.trim(title),
                                        trim_content(content, version),
                                        new Some(
                                          new $titles.TestInfo(
                                            $string.trim(file),
                                            $string.trim(test_name),
                                          ),
                                        ),
                                      ),
                                    );
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else if ($11 === "---\r") {
                          let $12 = $5.head;
                          if ($12.startsWith("version: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("title: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("file: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("test_name: ")) {
                                  let $16 = $9.head;
                                  if ($16 === "---\r") {
                                    let content = $3[0][1];
                                    let version = $12.slice(9);
                                    let title = $13.slice(7);
                                    let file = $14.slice(6);
                                    let test_name = $15.slice(11);
                                    return new Ok(
                                      new Snapshot(
                                        $string.trim(title),
                                        trim_content(content, version),
                                        new Some(
                                          new $titles.TestInfo(
                                            $string.trim(file),
                                            $string.trim(test_name),
                                          ),
                                        ),
                                      ),
                                    );
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      } else {
                        return new Error(undefined);
                      }
                    }
                  }
                }
              }
            }
          }
        } else {
          return new Error(undefined);
        }
      } else {
        let $3 = $2.tail;
        if ($3 instanceof $Empty) {
          let $4 = split_n(raw, 6, "\n");
          if ($4 instanceof Ok) {
            let $5 = $4[0][0];
            if ($5 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $6 = $5.tail;
              if ($6 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $7 = $6.tail;
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          let $12 = $5.head;
                          if ($12 === "---") {
                            let $13 = $6.head;
                            if ($13.startsWith("version: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("title: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("file: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("test_name: ")) {
                                    let $17 = $10.head;
                                    if ($17 === "---") {
                                      let content = $4[0][1];
                                      let version = $13.slice(9);
                                      let title = $14.slice(7);
                                      let file = $15.slice(6);
                                      let test_name = $16.slice(11);
                                      return new Ok(
                                        new Snapshot(
                                          $string.trim(title),
                                          trim_content(content, version),
                                          new Some(
                                            new $titles.TestInfo(
                                              $string.trim(file),
                                              $string.trim(test_name),
                                            ),
                                          ),
                                        ),
                                      );
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else if ($12 === "---\r") {
                            let $13 = $6.head;
                            if ($13.startsWith("version: ")) {
                              let $14 = $7.head;
                              if ($14.startsWith("title: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("file: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("test_name: ")) {
                                    let $17 = $10.head;
                                    if ($17 === "---\r") {
                                      let content = $4[0][1];
                                      let version = $13.slice(9);
                                      let title = $14.slice(7);
                                      let file = $15.slice(6);
                                      let test_name = $16.slice(11);
                                      return new Ok(
                                        new Snapshot(
                                          $string.trim(title),
                                          trim_content(content, version),
                                          new Some(
                                            new $titles.TestInfo(
                                              $string.trim(file),
                                              $string.trim(test_name),
                                            ),
                                          ),
                                        ),
                                      );
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      }
                    }
                  }
                }
              }
            }
          } else {
            return new Error(undefined);
          }
        } else {
          let $4 = $3.tail;
          if ($4 instanceof $Empty) {
            let $5 = split_n(raw, 6, "\n");
            if ($5 instanceof Ok) {
              let $6 = $5[0][0];
              if ($6 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $7 = $6.tail;
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            let $13 = $6.head;
                            if ($13 === "---") {
                              let $14 = $7.head;
                              if ($14.startsWith("version: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("title: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("file: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("test_name: ")) {
                                      let $18 = $11.head;
                                      if ($18 === "---") {
                                        let content = $5[0][1];
                                        let version = $14.slice(9);
                                        let title = $15.slice(7);
                                        let file = $16.slice(6);
                                        let test_name = $17.slice(11);
                                        return new Ok(
                                          new Snapshot(
                                            $string.trim(title),
                                            trim_content(content, version),
                                            new Some(
                                              new $titles.TestInfo(
                                                $string.trim(file),
                                                $string.trim(test_name),
                                              ),
                                            ),
                                          ),
                                        );
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else if ($13 === "---\r") {
                              let $14 = $7.head;
                              if ($14.startsWith("version: ")) {
                                let $15 = $8.head;
                                if ($15.startsWith("title: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("file: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("test_name: ")) {
                                      let $18 = $11.head;
                                      if ($18 === "---\r") {
                                        let content = $5[0][1];
                                        let version = $14.slice(9);
                                        let title = $15.slice(7);
                                        let file = $16.slice(6);
                                        let test_name = $17.slice(11);
                                        return new Ok(
                                          new Snapshot(
                                            $string.trim(title),
                                            trim_content(content, version),
                                            new Some(
                                              new $titles.TestInfo(
                                                $string.trim(file),
                                                $string.trim(test_name),
                                              ),
                                            ),
                                          ),
                                        );
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        }
                      }
                    }
                  }
                }
              }
            } else {
              return new Error(undefined);
            }
          } else {
            let $5 = $4.tail;
            if ($5 instanceof $Empty) {
              let $6 = $1.head;
              if ($6 === "---") {
                let $7 = $2.head;
                if ($7.startsWith("version: ")) {
                  let $8 = $3.head;
                  if ($8.startsWith("title: ")) {
                    let $9 = $4.head;
                    if ($9 === "---") {
                      let content = $[0][1];
                      let version = $7.slice(9);
                      let title = $8.slice(7);
                      return new Ok(
                        new Snapshot(
                          $string.trim(title),
                          trim_content(content, version),
                          new None(),
                        ),
                      );
                    } else {
                      let $10 = split_n(raw, 6, "\n");
                      if ($10 instanceof Ok) {
                        let $11 = $10[0][0];
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  return new Error(undefined);
                                } else {
                                  let $16 = $15.tail;
                                  if ($16 instanceof $Empty) {
                                    return new Error(undefined);
                                  } else {
                                    let $17 = $16.tail;
                                    if ($17 instanceof $Empty) {
                                      let $18 = $11.head;
                                      if ($18 === "---") {
                                        let $19 = $12.head;
                                        if ($19.startsWith("version: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("title: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("file: ")) {
                                              let $22 = $15.head;
                                              if ($22.startsWith("test_name: ")) {
                                                let $23 = $16.head;
                                                if ($23 === "---") {
                                                  let content = $10[0][1];
                                                  let version = $19.slice(9);
                                                  let title = $20.slice(7);
                                                  let file = $21.slice(6);
                                                  let test_name = $22.slice(11);
                                                  return new Ok(
                                                    new Snapshot(
                                                      $string.trim(title),
                                                      trim_content(
                                                        content,
                                                        version,
                                                      ),
                                                      new Some(
                                                        new $titles.TestInfo(
                                                          $string.trim(file),
                                                          $string.trim(
                                                            test_name,
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  );
                                                } else {
                                                  return new Error(undefined);
                                                }
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else if ($18 === "---\r") {
                                        let $19 = $12.head;
                                        if ($19.startsWith("version: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("title: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("file: ")) {
                                              let $22 = $15.head;
                                              if ($22.startsWith("test_name: ")) {
                                                let $23 = $16.head;
                                                if ($23 === "---\r") {
                                                  let content = $10[0][1];
                                                  let version = $19.slice(9);
                                                  let title = $20.slice(7);
                                                  let file = $21.slice(6);
                                                  let test_name = $22.slice(11);
                                                  return new Ok(
                                                    new Snapshot(
                                                      $string.trim(title),
                                                      trim_content(
                                                        content,
                                                        version,
                                                      ),
                                                      new Some(
                                                        new $titles.TestInfo(
                                                          $string.trim(file),
                                                          $string.trim(
                                                            test_name,
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  );
                                                } else {
                                                  return new Error(undefined);
                                                }
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      } else {
                        return new Error(undefined);
                      }
                    }
                  } else {
                    let $9 = split_n(raw, 6, "\n");
                    if ($9 instanceof Ok) {
                      let $10 = $9[0][0];
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  return new Error(undefined);
                                } else {
                                  let $16 = $15.tail;
                                  if ($16 instanceof $Empty) {
                                    let $17 = $10.head;
                                    if ($17 === "---") {
                                      let $18 = $11.head;
                                      if ($18.startsWith("version: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("title: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("file: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("test_name: ")) {
                                              let $22 = $15.head;
                                              if ($22 === "---") {
                                                let content = $9[0][1];
                                                let version = $18.slice(9);
                                                let title = $19.slice(7);
                                                let file = $20.slice(6);
                                                let test_name = $21.slice(11);
                                                return new Ok(
                                                  new Snapshot(
                                                    $string.trim(title),
                                                    trim_content(
                                                      content,
                                                      version,
                                                    ),
                                                    new Some(
                                                      new $titles.TestInfo(
                                                        $string.trim(file),
                                                        $string.trim(test_name),
                                                      ),
                                                    ),
                                                  ),
                                                );
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else if ($17 === "---\r") {
                                      let $18 = $11.head;
                                      if ($18.startsWith("version: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("title: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("file: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("test_name: ")) {
                                              let $22 = $15.head;
                                              if ($22 === "---\r") {
                                                let content = $9[0][1];
                                                let version = $18.slice(9);
                                                let title = $19.slice(7);
                                                let file = $20.slice(6);
                                                let test_name = $21.slice(11);
                                                return new Ok(
                                                  new Snapshot(
                                                    $string.trim(title),
                                                    trim_content(
                                                      content,
                                                      version,
                                                    ),
                                                    new Some(
                                                      new $titles.TestInfo(
                                                        $string.trim(file),
                                                        $string.trim(test_name),
                                                      ),
                                                    ),
                                                  ),
                                                );
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    } else {
                      return new Error(undefined);
                    }
                  }
                } else {
                  let $8 = split_n(raw, 6, "\n");
                  if ($8 instanceof Ok) {
                    let $9 = $8[0][0];
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  let $16 = $9.head;
                                  if ($16 === "---") {
                                    let $17 = $10.head;
                                    if ($17.startsWith("version: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("title: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("file: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("test_name: ")) {
                                            let $21 = $14.head;
                                            if ($21 === "---") {
                                              let content = $8[0][1];
                                              let version = $17.slice(9);
                                              let title = $18.slice(7);
                                              let file = $19.slice(6);
                                              let test_name = $20.slice(11);
                                              return new Ok(
                                                new Snapshot(
                                                  $string.trim(title),
                                                  trim_content(content, version),
                                                  new Some(
                                                    new $titles.TestInfo(
                                                      $string.trim(file),
                                                      $string.trim(test_name),
                                                    ),
                                                  ),
                                                ),
                                              );
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else if ($16 === "---\r") {
                                    let $17 = $10.head;
                                    if ($17.startsWith("version: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("title: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("file: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("test_name: ")) {
                                            let $21 = $14.head;
                                            if ($21 === "---\r") {
                                              let content = $8[0][1];
                                              let version = $17.slice(9);
                                              let title = $18.slice(7);
                                              let file = $19.slice(6);
                                              let test_name = $20.slice(11);
                                              return new Ok(
                                                new Snapshot(
                                                  $string.trim(title),
                                                  trim_content(content, version),
                                                  new Some(
                                                    new $titles.TestInfo(
                                                      $string.trim(file),
                                                      $string.trim(test_name),
                                                    ),
                                                  ),
                                                ),
                                              );
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  } else {
                    return new Error(undefined);
                  }
                }
              } else if ($6 === "---\r") {
                let $7 = $2.head;
                if ($7.startsWith("version: ")) {
                  let $8 = $3.head;
                  if ($8.startsWith("title: ")) {
                    let $9 = $4.head;
                    if ($9 === "---\r") {
                      let content = $[0][1];
                      let version = $7.slice(9);
                      let title = $8.slice(7);
                      return new Ok(
                        new Snapshot(
                          $string.trim(title),
                          trim_content(content, version),
                          new None(),
                        ),
                      );
                    } else {
                      let $10 = split_n(raw, 6, "\n");
                      if ($10 instanceof Ok) {
                        let $11 = $10[0][0];
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  return new Error(undefined);
                                } else {
                                  let $16 = $15.tail;
                                  if ($16 instanceof $Empty) {
                                    return new Error(undefined);
                                  } else {
                                    let $17 = $16.tail;
                                    if ($17 instanceof $Empty) {
                                      let $18 = $11.head;
                                      if ($18 === "---") {
                                        let $19 = $12.head;
                                        if ($19.startsWith("version: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("title: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("file: ")) {
                                              let $22 = $15.head;
                                              if ($22.startsWith("test_name: ")) {
                                                let $23 = $16.head;
                                                if ($23 === "---") {
                                                  let content = $10[0][1];
                                                  let version = $19.slice(9);
                                                  let title = $20.slice(7);
                                                  let file = $21.slice(6);
                                                  let test_name = $22.slice(11);
                                                  return new Ok(
                                                    new Snapshot(
                                                      $string.trim(title),
                                                      trim_content(
                                                        content,
                                                        version,
                                                      ),
                                                      new Some(
                                                        new $titles.TestInfo(
                                                          $string.trim(file),
                                                          $string.trim(
                                                            test_name,
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  );
                                                } else {
                                                  return new Error(undefined);
                                                }
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else if ($18 === "---\r") {
                                        let $19 = $12.head;
                                        if ($19.startsWith("version: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("title: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("file: ")) {
                                              let $22 = $15.head;
                                              if ($22.startsWith("test_name: ")) {
                                                let $23 = $16.head;
                                                if ($23 === "---\r") {
                                                  let content = $10[0][1];
                                                  let version = $19.slice(9);
                                                  let title = $20.slice(7);
                                                  let file = $21.slice(6);
                                                  let test_name = $22.slice(11);
                                                  return new Ok(
                                                    new Snapshot(
                                                      $string.trim(title),
                                                      trim_content(
                                                        content,
                                                        version,
                                                      ),
                                                      new Some(
                                                        new $titles.TestInfo(
                                                          $string.trim(file),
                                                          $string.trim(
                                                            test_name,
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  );
                                                } else {
                                                  return new Error(undefined);
                                                }
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      } else {
                        return new Error(undefined);
                      }
                    }
                  } else {
                    let $9 = split_n(raw, 6, "\n");
                    if ($9 instanceof Ok) {
                      let $10 = $9[0][0];
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  return new Error(undefined);
                                } else {
                                  let $16 = $15.tail;
                                  if ($16 instanceof $Empty) {
                                    let $17 = $10.head;
                                    if ($17 === "---") {
                                      let $18 = $11.head;
                                      if ($18.startsWith("version: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("title: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("file: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("test_name: ")) {
                                              let $22 = $15.head;
                                              if ($22 === "---") {
                                                let content = $9[0][1];
                                                let version = $18.slice(9);
                                                let title = $19.slice(7);
                                                let file = $20.slice(6);
                                                let test_name = $21.slice(11);
                                                return new Ok(
                                                  new Snapshot(
                                                    $string.trim(title),
                                                    trim_content(
                                                      content,
                                                      version,
                                                    ),
                                                    new Some(
                                                      new $titles.TestInfo(
                                                        $string.trim(file),
                                                        $string.trim(test_name),
                                                      ),
                                                    ),
                                                  ),
                                                );
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else if ($17 === "---\r") {
                                      let $18 = $11.head;
                                      if ($18.startsWith("version: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("title: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("file: ")) {
                                            let $21 = $14.head;
                                            if ($21.startsWith("test_name: ")) {
                                              let $22 = $15.head;
                                              if ($22 === "---\r") {
                                                let content = $9[0][1];
                                                let version = $18.slice(9);
                                                let title = $19.slice(7);
                                                let file = $20.slice(6);
                                                let test_name = $21.slice(11);
                                                return new Ok(
                                                  new Snapshot(
                                                    $string.trim(title),
                                                    trim_content(
                                                      content,
                                                      version,
                                                    ),
                                                    new Some(
                                                      new $titles.TestInfo(
                                                        $string.trim(file),
                                                        $string.trim(test_name),
                                                      ),
                                                    ),
                                                  ),
                                                );
                                              } else {
                                                return new Error(undefined);
                                              }
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    } else {
                      return new Error(undefined);
                    }
                  }
                } else {
                  let $8 = split_n(raw, 6, "\n");
                  if ($8 instanceof Ok) {
                    let $9 = $8[0][0];
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                return new Error(undefined);
                              } else {
                                let $15 = $14.tail;
                                if ($15 instanceof $Empty) {
                                  let $16 = $9.head;
                                  if ($16 === "---") {
                                    let $17 = $10.head;
                                    if ($17.startsWith("version: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("title: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("file: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("test_name: ")) {
                                            let $21 = $14.head;
                                            if ($21 === "---") {
                                              let content = $8[0][1];
                                              let version = $17.slice(9);
                                              let title = $18.slice(7);
                                              let file = $19.slice(6);
                                              let test_name = $20.slice(11);
                                              return new Ok(
                                                new Snapshot(
                                                  $string.trim(title),
                                                  trim_content(content, version),
                                                  new Some(
                                                    new $titles.TestInfo(
                                                      $string.trim(file),
                                                      $string.trim(test_name),
                                                    ),
                                                  ),
                                                ),
                                              );
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else if ($16 === "---\r") {
                                    let $17 = $10.head;
                                    if ($17.startsWith("version: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("title: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("file: ")) {
                                          let $20 = $13.head;
                                          if ($20.startsWith("test_name: ")) {
                                            let $21 = $14.head;
                                            if ($21 === "---\r") {
                                              let content = $8[0][1];
                                              let version = $17.slice(9);
                                              let title = $18.slice(7);
                                              let file = $19.slice(6);
                                              let test_name = $20.slice(11);
                                              return new Ok(
                                                new Snapshot(
                                                  $string.trim(title),
                                                  trim_content(content, version),
                                                  new Some(
                                                    new $titles.TestInfo(
                                                      $string.trim(file),
                                                      $string.trim(test_name),
                                                    ),
                                                  ),
                                                ),
                                              );
                                            } else {
                                              return new Error(undefined);
                                            }
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  } else {
                    return new Error(undefined);
                  }
                }
              } else {
                let $7 = split_n(raw, 6, "\n");
                if ($7 instanceof Ok) {
                  let $8 = $7[0][0];
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              return new Error(undefined);
                            } else {
                              let $14 = $13.tail;
                              if ($14 instanceof $Empty) {
                                let $15 = $8.head;
                                if ($15 === "---") {
                                  let $16 = $9.head;
                                  if ($16.startsWith("version: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("title: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("file: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("test_name: ")) {
                                          let $20 = $13.head;
                                          if ($20 === "---") {
                                            let content = $7[0][1];
                                            let version = $16.slice(9);
                                            let title = $17.slice(7);
                                            let file = $18.slice(6);
                                            let test_name = $19.slice(11);
                                            return new Ok(
                                              new Snapshot(
                                                $string.trim(title),
                                                trim_content(content, version),
                                                new Some(
                                                  new $titles.TestInfo(
                                                    $string.trim(file),
                                                    $string.trim(test_name),
                                                  ),
                                                ),
                                              ),
                                            );
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if ($15 === "---\r") {
                                  let $16 = $9.head;
                                  if ($16.startsWith("version: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("title: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("file: ")) {
                                        let $19 = $12.head;
                                        if ($19.startsWith("test_name: ")) {
                                          let $20 = $13.head;
                                          if ($20 === "---\r") {
                                            let content = $7[0][1];
                                            let version = $16.slice(9);
                                            let title = $17.slice(7);
                                            let file = $18.slice(6);
                                            let test_name = $19.slice(11);
                                            return new Ok(
                                              new Snapshot(
                                                $string.trim(title),
                                                trim_content(content, version),
                                                new Some(
                                                  new $titles.TestInfo(
                                                    $string.trim(file),
                                                    $string.trim(test_name),
                                                  ),
                                                ),
                                              ),
                                            );
                                          } else {
                                            return new Error(undefined);
                                          }
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                } else {
                  return new Error(undefined);
                }
              }
            } else {
              let $6 = split_n(raw, 6, "\n");
              if ($6 instanceof Ok) {
                let $7 = $6[0][0];
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    return new Error(undefined);
                  } else {
                    let $9 = $8.tail;
                    if ($9 instanceof $Empty) {
                      return new Error(undefined);
                    } else {
                      let $10 = $9.tail;
                      if ($10 instanceof $Empty) {
                        return new Error(undefined);
                      } else {
                        let $11 = $10.tail;
                        if ($11 instanceof $Empty) {
                          return new Error(undefined);
                        } else {
                          let $12 = $11.tail;
                          if ($12 instanceof $Empty) {
                            return new Error(undefined);
                          } else {
                            let $13 = $12.tail;
                            if ($13 instanceof $Empty) {
                              let $14 = $7.head;
                              if ($14 === "---") {
                                let $15 = $8.head;
                                if ($15.startsWith("version: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("title: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("file: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("test_name: ")) {
                                        let $19 = $12.head;
                                        if ($19 === "---") {
                                          let content = $6[0][1];
                                          let version = $15.slice(9);
                                          let title = $16.slice(7);
                                          let file = $17.slice(6);
                                          let test_name = $18.slice(11);
                                          return new Ok(
                                            new Snapshot(
                                              $string.trim(title),
                                              trim_content(content, version),
                                              new Some(
                                                new $titles.TestInfo(
                                                  $string.trim(file),
                                                  $string.trim(test_name),
                                                ),
                                              ),
                                            ),
                                          );
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else if ($14 === "---\r") {
                                let $15 = $8.head;
                                if ($15.startsWith("version: ")) {
                                  let $16 = $9.head;
                                  if ($16.startsWith("title: ")) {
                                    let $17 = $10.head;
                                    if ($17.startsWith("file: ")) {
                                      let $18 = $11.head;
                                      if ($18.startsWith("test_name: ")) {
                                        let $19 = $12.head;
                                        if ($19 === "---\r") {
                                          let content = $6[0][1];
                                          let version = $15.slice(9);
                                          let title = $16.slice(7);
                                          let file = $17.slice(6);
                                          let test_name = $18.slice(11);
                                          return new Ok(
                                            new Snapshot(
                                              $string.trim(title),
                                              trim_content(content, version),
                                              new Some(
                                                new $titles.TestInfo(
                                                  $string.trim(file),
                                                  $string.trim(test_name),
                                                ),
                                              ),
                                            ),
                                          );
                                        } else {
                                          return new Error(undefined);
                                        }
                                      } else {
                                        return new Error(undefined);
                                      }
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else {
                                  return new Error(undefined);
                                }
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          }
                        }
                      }
                    }
                  }
                }
              } else {
                return new Error(undefined);
              }
            }
          }
        }
      }
    }
  } else {
    let $1 = split_n(raw, 6, "\n");
    if ($1 instanceof Ok) {
      let $2 = $1[0][0];
      if ($2 instanceof $Empty) {
        return new Error(undefined);
      } else {
        let $3 = $2.tail;
        if ($3 instanceof $Empty) {
          return new Error(undefined);
        } else {
          let $4 = $3.tail;
          if ($4 instanceof $Empty) {
            return new Error(undefined);
          } else {
            let $5 = $4.tail;
            if ($5 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $6 = $5.tail;
              if ($6 instanceof $Empty) {
                return new Error(undefined);
              } else {
                let $7 = $6.tail;
                if ($7 instanceof $Empty) {
                  return new Error(undefined);
                } else {
                  let $8 = $7.tail;
                  if ($8 instanceof $Empty) {
                    let $9 = $2.head;
                    if ($9 === "---") {
                      let $10 = $3.head;
                      if ($10.startsWith("version: ")) {
                        let $11 = $4.head;
                        if ($11.startsWith("title: ")) {
                          let $12 = $5.head;
                          if ($12.startsWith("file: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("test_name: ")) {
                              let $14 = $7.head;
                              if ($14 === "---") {
                                let content = $1[0][1];
                                let version = $10.slice(9);
                                let title = $11.slice(7);
                                let file = $12.slice(6);
                                let test_name = $13.slice(11);
                                return new Ok(
                                  new Snapshot(
                                    $string.trim(title),
                                    trim_content(content, version),
                                    new Some(
                                      new $titles.TestInfo(
                                        $string.trim(file),
                                        $string.trim(test_name),
                                      ),
                                    ),
                                  ),
                                );
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      } else {
                        return new Error(undefined);
                      }
                    } else if ($9 === "---\r") {
                      let $10 = $3.head;
                      if ($10.startsWith("version: ")) {
                        let $11 = $4.head;
                        if ($11.startsWith("title: ")) {
                          let $12 = $5.head;
                          if ($12.startsWith("file: ")) {
                            let $13 = $6.head;
                            if ($13.startsWith("test_name: ")) {
                              let $14 = $7.head;
                              if ($14 === "---\r") {
                                let content = $1[0][1];
                                let version = $10.slice(9);
                                let title = $11.slice(7);
                                let file = $12.slice(6);
                                let test_name = $13.slice(11);
                                return new Ok(
                                  new Snapshot(
                                    $string.trim(title),
                                    trim_content(content, version),
                                    new Some(
                                      new $titles.TestInfo(
                                        $string.trim(file),
                                        $string.trim(test_name),
                                      ),
                                    ),
                                  ),
                                );
                              } else {
                                return new Error(undefined);
                              }
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else {
                          return new Error(undefined);
                        }
                      } else {
                        return new Error(undefined);
                      }
                    } else {
                      return new Error(undefined);
                    }
                  } else {
                    return new Error(undefined);
                  }
                }
              }
            }
          }
        }
      }
    } else {
      return new Error(undefined);
    }
  }
}

/**
 * Read an accepted snapshot which might be missing.
 * 
 * @ignore
 */
function read_accepted(source) {
  let $ = $simplifile.read(source);
  if ($ instanceof Ok) {
    let content = $[0];
    let $1 = deserialise(content);
    if ($1 instanceof Ok) {
      let snapshot = $1[0];
      return new Ok(new Some(snapshot));
    } else {
      return new Error(new CorruptedSnapshot(source));
    }
  } else {
    let $1 = $[0];
    if ($1 instanceof Enoent) {
      return new Ok(new None());
    } else {
      let reason = $1;
      return new Error(new CannotReadAcceptedSnapshot(reason, source));
    }
  }
}

/**
 * Read a new snapshot.
 *
 * > ℹ️ Notice the different return type compared to `read_accepted`: when we
 * > try to read a new snapshot we are sure it's there (because we've listed
 * > the directory or something else) so if it's not present that's an error
 * > and we don't return an `Ok(None)`.
 * 
 * @ignore
 */
function read_new(source) {
  let $ = $simplifile.read(source);
  if ($ instanceof Ok) {
    let content = $[0];
    return $result.replace_error(
      deserialise(content),
      new CorruptedSnapshot(source),
    );
  } else {
    let reason = $[0];
    return new Error(new CannotReadNewSnapshot(reason, source));
  }
}

function reject_snapshot(new_snapshot_path) {
  let _pipe = $simplifile.delete$(new_snapshot_path);
  return $result.map_error(
    _pipe,
    (_capture) => {
      return new CannotRejectSnapshot(_capture, new_snapshot_path);
    },
  );
}

/**
 * Turns a snapshot's title into a file name stripping it of all dangerous
 * characters (or at least those I could think ok 😁).
 * 
 * @ignore
 */
function file_name(title) {
  let _pipe = $string.replace(title, "/", " ");
  let _pipe$1 = $string.replace(_pipe, "\\", " ");
  let _pipe$2 = $string.replace(_pipe$1, "\n", " ");
  let _pipe$3 = $string.replace(_pipe$2, "\t", " ");
  let _pipe$4 = $string.replace(_pipe$3, "\r", " ");
  let _pipe$5 = $string.replace(_pipe$4, ".", " ");
  let _pipe$6 = $string.replace(_pipe$5, ":", " ");
  return $justin.snake_case(_pipe$6);
}

function to_function_name(file, function_name) {
  let _block;
  if (file.startsWith("./test/")) {
    let rest = file.slice(7);
    _block = $filepath.strip_extension(rest);
  } else {
    _block = $filepath.strip_extension(file);
  }
  let module_name = _block;
  return ((module_name + ".{") + function_name) + "}";
}

function explain(error) {
  let heading = (reason) => {
    return ("[" + $ansi.bold($string.inspect(reason))) + "] ";
  };
  let _block;
  if (error instanceof SnapshotWithEmptyTitle) {
    _block = "a snapshot cannot have the empty string as a title.";
  } else if (error instanceof CannotCreateSnapshotsFolder) {
    let reason = error.reason;
    _block = heading(reason) + "I couldn't create the snapshots folder.";
  } else if (error instanceof CannotReadAcceptedSnapshot) {
    let reason = error.reason;
    let source = error.source;
    _block = (heading(reason) + "I couldn't read the accepted snapshot from ") + $ansi.italic(
      ("\"" + source) + "\".",
    );
  } else if (error instanceof CannotReadNewSnapshot) {
    let reason = error.reason;
    let source = error.source;
    _block = (heading(reason) + "I couldn't read the new snapshot from ") + $ansi.italic(
      ("\"" + source) + "\".",
    );
  } else if (error instanceof CannotSaveNewSnapshot) {
    let reason = error.reason;
    let title = error.title;
    let destination = error.destination;
    _block = (((heading(reason) + "I couldn't save the snapshot ") + $ansi.italic(
      ("\"" + title) + "\" ",
    )) + "to ") + $ansi.italic(("\"" + destination) + "\".");
  } else if (error instanceof CannotReadSnapshots) {
    let reason = error.reason;
    _block = heading(reason) + "I couldn't read the snapshots folder's contents.";
  } else if (error instanceof CannotRejectSnapshot) {
    let reason = error.reason;
    let snapshot = error.snapshot;
    _block = (heading(reason) + "I couldn't reject the snapshot ") + $ansi.italic(
      ("\"" + snapshot) + "\".",
    );
  } else if (error instanceof CannotAcceptSnapshot) {
    let reason = error.reason;
    let snapshot = error.snapshot;
    _block = (heading(reason) + "I couldn't accept the snapshot ") + $ansi.italic(
      ("\"" + snapshot) + "\".",
    );
  } else if (error instanceof CannotReadUserInput) {
    _block = "I couldn't read the user input.";
  } else if (error instanceof CorruptedSnapshot) {
    let source = error.source;
    _block = ((("It looks like " + $ansi.italic(("\"" + source) + "\"\n")) + "is not a valid snapshot.\n") + "This might happen when someone modifies its content.\n") + "Try deleting the snapshot and recreating it.";
  } else if (error instanceof CannotFindProjectRoot) {
    let reason = error.reason;
    _block = (heading(reason) + "I couldn't locate the project's root where the snapshot's") + " folder should be.";
  } else if (error instanceof CannotCreateReferencedFile) {
    let $ = error.reason;
    if ($ instanceof $simplifile.Eacces) {
      let file = error.file;
      _block = (((("I don't have the required permission to create the file used to track\n" + (("stale snapshots at: `" + file) + "`.\n")) + "This usually happens when the current user doesn't have a write\n") + "permission for the system's temporary directory.\n\n") + "Hint: you can set the $TEMP environment variable to make me use a\n") + "different directory to write the reference file in.";
    } else {
      let reason = $;
      _block = heading(reason) + "I couldn't create the file used to track stale snapshots.";
    }
  } else if (error instanceof CannotReadReferencedFile) {
    let $ = error.reason;
    if ($ instanceof $simplifile.Eacces) {
      let file = error.file;
      _block = (((("I don't have the required permission to read the file used to track\n" + (("stale snapshots at: `" + file) + "`.\n")) + "This usually happens when the current user doesn't have a write\n") + "permission for the system's temporary directory.\n\n") + "Hint: you can set the $TEMP environment variable to make me use a\n") + "different directory to write the reference file in.";
    } else {
      let reason = $;
      _block = heading(reason) + "I couldn't read the file used to track stale snapshots.";
    }
  } else if (error instanceof CannotMarkSnapshotAsReferenced) {
    let reason = error.reason;
    _block = heading(reason) + "I couldn't write to the file used to track stale snapshots.";
  } else if (error instanceof StaleSnapshotsFound) {
    let stale_snapshots = error.stale_snapshots;
    let _block$1;
    let _pipe = $list.map(
      stale_snapshots,
      (snapshot) => { return "  - " + $filepath.strip_extension(snapshot); },
    );
    _block$1 = $string.join(_pipe, "\n");
    let titles = _block$1;
    _block = (((("I found the following stale snapshots:\n\n" + titles) + "\n\n") + "These snapshots were not referenced by any snapshot test during the ") + "last `gleam test`\n") + "Hint: run `gleam run -m birdie stale delete` to delete them";
  } else if (error instanceof CannotDeleteStaleSnapshot) {
    let reason = error.reason;
    _block = heading(reason) + "I couldn't delete one of the stale snapshots.";
  } else if (error instanceof MissingReferencedFile) {
    _block = ("I couldn't find any information about stale snapshots.\n" + "Remember you have to run `gleam test` first, so I can find any stale ") + "snapshot.";
  } else if (error instanceof CannotGetTitles) {
    let $ = error.reason;
    if ($ instanceof $titles.CannotFindProjectRoot) {
      let reason = $.reason;
      _block = (heading(reason) + "I couldn't locate the project's root where the snapshot's") + " folder should be.";
    } else if ($ instanceof $titles.CannotReadTestDirectory) {
      let reason = $.reason;
      _block = heading(reason) + "I couldn't list the contents of the test folder.";
    } else if ($ instanceof $titles.CannotReadTestFile) {
      let reason = $.reason;
      let file = $.file;
      _block = (heading(reason) + "I couldn't read the test file ") + $ansi.italic(
        ("\"" + file) + "\"\n",
      );
    } else if ($ instanceof $titles.DuplicateLiteralTitles) {
      let title = $.title;
      let one_file = $.one.file;
      let one_test_name = $.one.test_name;
      let other_file = $.other.file;
      let other_test_name = $.other.test_name;
      let same_file = one_file === other_file;
      let same_function = one_test_name === other_test_name;
      let _block$1;
      if (same_file && same_function) {
        _block$1 = "Both tests are defined in:\n\n  " + $ansi.italic(
          to_function_name(one_file, one_test_name),
        );
      } else {
        _block$1 = (("One test is defined in:\n\n  " + $ansi.italic(
          to_function_name(one_file, one_test_name),
        )) + "\n\nWhile the other is defined in:\n\n  ") + $ansi.italic(
          to_function_name(other_file, other_test_name),
        );
      }
      let location = _block$1;
      _block = (((("it looks like there's some snapshot tests sharing the same title:\n\n  " + $ansi.italic(
        ("\"" + title) + "\"",
      )) + "\n\nSnapshot titles ") + $ansi.bold("must be unique")) + " or you would run into strange diffs\nwhen reviewing them, try changing one of those.\n") + location;
    } else if ($ instanceof $titles.OverlappingPrefixes) {
      throw makeError(
        "panic",
        FILEPATH,
        "birdie",
        743,
        "explain",
        "Prefixes are not implemented yet",
        {}
      )
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "birdie",
        746,
        "explain",
        "Prefixes are not implemented yet",
        {}
      )
    }
  } else {
    let reason = error.reason;
    _block = heading(reason) + "I couldn't figure out the current project's name.";
  }
  let message = _block;
  return message;
}

function snapshot_default_lines(snapshot) {
  let title;
  let info;
  title = snapshot.title;
  info = snapshot.info;
  if (info instanceof Some) {
    let file = info[0].file;
    let test_name = info[0].test_name;
    return toList([
      new InfoLineWithTitle(title, new SplitWords(), "title"),
      new InfoLineWithTitle(file, new Truncate(), "file"),
      new InfoLineWithTitle(test_name, new Truncate(), "name"),
    ]);
  } else {
    return toList([new InfoLineWithTitle(title, new SplitWords(), "title")]);
  }
}

function count_digits_loop(loop$number, loop$digits) {
  while (true) {
    let number = loop$number;
    let digits = loop$digits;
    let $ = number < 10;
    if ($) {
      return 1 + digits;
    } else {
      loop$number = globalThis.Math.trunc(number / 10);
      loop$digits = 1 + digits;
    }
  }
}

function count_digits(number) {
  return count_digits_loop($int.absolute_value(number), 0);
}

function pretty_diff_line(diff_line, padding, shared_line_style) {
  let number;
  let line;
  let kind;
  number = diff_line.number;
  line = diff_line.line;
  kind = diff_line.kind;
  let _block;
  if (kind instanceof $diff.Old) {
    let _block$1;
    let _pipe = (" " + $int.to_string(number));
    _block$1 = $string.pad_end(_pipe, padding - 1, " ");
    let number$1 = _block$1;
    _block = [$ansi.red(number$1), $ansi.red(line), $ansi.red(" - ")];
  } else if (kind instanceof $diff.New) {
    _block = [
      (() => {
        let _pipe = $int.to_string(number);
        let _pipe$1 = $string.pad_start(_pipe, padding - 1, " ");
        let _pipe$2 = $ansi.green(_pipe$1);
        return $ansi.bold(_pipe$2);
      })(),
      $ansi.green(line),
      $ansi.green(" + "),
    ];
  } else {
    _block = [
      (() => {
        let _pipe = $int.to_string(number);
        let _pipe$1 = $string.pad_start(_pipe, padding - 1, " ");
        return $ansi.dim(_pipe$1);
      })(),
      shared_line_style(line),
      " │ ",
    ];
  }
  let $ = _block;
  let pretty_number;
  let pretty_line;
  let separator;
  pretty_number = $[0];
  pretty_line = $[1];
  separator = $[2];
  return (pretty_number + separator) + pretty_line;
}

function truncate(string, max_length) {
  let $ = $string.length(string) > max_length;
  if ($) {
    let _pipe = $string.to_graphemes(string);
    let _pipe$1 = $list.take(_pipe, max_length - 3);
    let _pipe$2 = $string.join(_pipe$1, "");
    return $string.append(_pipe$2, "...");
  } else {
    return string;
  }
}

function do_to_lines(
  loop$lines,
  loop$line,
  loop$line_length,
  loop$words,
  loop$max_length
) {
  while (true) {
    let lines = loop$lines;
    let line = loop$line;
    let line_length = loop$line_length;
    let words = loop$words;
    let max_length = loop$max_length;
    if (words instanceof $Empty) {
      let $ = line === "";
      if ($) {
        return $list.reverse(lines);
      } else {
        return $list.reverse(listPrepend(line, lines));
      }
    } else {
      let word = words.head;
      let rest = words.tail;
      let word_length = $string.length(word);
      let new_line_length = (word_length + line_length) + 1;
      let $ = new_line_length > max_length;
      if ($) {
        loop$lines = listPrepend(line, lines);
        loop$line = "";
        loop$line_length = 0;
        loop$words = words;
        loop$max_length = max_length;
      } else {
        let _block;
        if (line === "") {
          _block = word;
        } else {
          _block = (line + " ") + word;
        }
        let new_line = _block;
        loop$lines = lines;
        loop$line = new_line;
        loop$line_length = new_line_length;
        loop$words = rest;
        loop$max_length = max_length;
      }
    }
  }
}

function to_lines(string, max_length) {
  return $list.flat_map(
    $string.split(string, "\n"),
    (line) => {
      let words = $string.split(line, " ");
      return do_to_lines(toList([]), "", 0, words, max_length);
    },
  );
}

function pretty_info_line(line, width) {
  let _block;
  if (line instanceof InfoLineWithTitle) {
    let title = line.title;
    _block = ["  " + $ansi.blue(title + ": "), $string.length(title) + 4];
  } else {
    _block = ["  ", 2];
  }
  let $ = _block;
  let prefix;
  let prefix_length;
  prefix = $[0];
  prefix_length = $[1];
  let $1 = line.split;
  if ($1 instanceof DoNotSplit) {
    return prefix + line.content;
  } else if ($1 instanceof SplitWords) {
    let $2 = to_lines(line.content, width - prefix_length);
    if ($2 instanceof $Empty) {
      return prefix;
    } else {
      let line$1 = $2.head;
      let lines = $2.tail;
      return $list.fold(
        lines,
        prefix + line$1,
        (acc, line) => {
          return ((acc + "\n") + $string.repeat(" ", prefix_length)) + line;
        },
      );
    }
  } else {
    return prefix + truncate(line.content, width - prefix_length);
  }
}

function toggle_mode(mode) {
  if (mode instanceof ShowDiff) {
    return new HideDiff();
  } else {
    return new ShowDiff();
  }
}

function terminal_width() {
  let $ = $term_size.get();
  if ($ instanceof Ok) {
    let columns = $[0][1];
    return columns;
  } else {
    return 80;
  }
}

function pretty_box(title, content_lines, info_lines, shared_line_style) {
  let width = terminal_width();
  let lines_count = $list.length(content_lines) + 1;
  let padding = count_digits(lines_count) * 2 + 5;
  let title_length = $string.length(title);
  let title_line_right = $string.repeat("─", (width - 5) - title_length);
  let title_line = (("── " + title) + " ─") + title_line_right;
  let _block;
  let _pipe = $list.map(
    info_lines,
    (_capture) => { return pretty_info_line(_capture, width); },
  );
  _block = $string.join(_pipe, "\n");
  let info_lines$1 = _block;
  let _block$1;
  let _pipe$1 = $list.map(
    content_lines,
    (_capture) => {
      return pretty_diff_line(_capture, padding, shared_line_style);
    },
  );
  _block$1 = $string.join(_pipe$1, "\n");
  let content = _block$1;
  let left_padding_line = $string.repeat("─", padding);
  let right_padding_line = $string.repeat("─", (width - padding) - 1);
  let open_line = (left_padding_line + "┬") + right_padding_line;
  let closed_line = (left_padding_line + "┴") + right_padding_line;
  let _pipe$2 = toList([
    title_line,
    "",
    info_lines$1,
    "",
    open_line,
    content,
    closed_line,
  ]);
  return $string.join(_pipe$2, "\n");
}

function new_snapshot_box(snapshot, additional_info_lines) {
  let content;
  content = snapshot.content;
  let _block;
  let _pipe = $string.split(content, "\n");
  _block = $list.index_map(
    _pipe,
    (line, i) => { return new DiffLine(i + 1, line, new $diff.New()); },
  );
  let content$1 = _block;
  return pretty_box(
    "new snapshot",
    content$1,
    $list.flatten(
      toList([snapshot_default_lines(snapshot), additional_info_lines]),
    ),
    (shared_line) => { return shared_line; },
  );
}

function diff_snapshot_box(accepted, new$, additional_info_lines) {
  return pretty_box(
    "mismatched snapshots",
    to_diff_lines(accepted, new$),
    (() => {
      let _pipe = toList([
        snapshot_default_lines(accepted),
        additional_info_lines,
        toList([
          new InfoLineWithNoTitle("", new DoNotSplit()),
          new InfoLineWithNoTitle($ansi.red("- old snapshot"), new DoNotSplit()),
          new InfoLineWithNoTitle(
            $ansi.green("+ new snapshot"),
            new DoNotSplit(),
          ),
        ]),
      ]);
      return $list.flatten(_pipe);
    })(),
    (shared_line) => { return $ansi.dim(shared_line); },
  );
}

function regular_snapshot_box(new$, additional_info_lines) {
  let content;
  content = new$.content;
  let _block;
  let _pipe = $string.split(content, "\n");
  _block = $list.index_map(
    _pipe,
    (line, i) => { return new DiffLine(i + 1, line, new $diff.Shared()); },
  );
  let content$1 = _block;
  return pretty_box(
    "mismatched snapshots",
    content$1,
    (() => {
      let _pipe$1 = toList([snapshot_default_lines(new$), additional_info_lines]);
      return $list.flatten(_pipe$1);
    })(),
    (shared_line) => { return shared_line; },
  );
}

/**
 * Replaces the first occurrence of an element in the list with the given
 * replacement.
 * 
 * @ignore
 */
function replace_first(list, item, replacement) {
  if (list instanceof $Empty) {
    return list;
  } else {
    let first = list.head;
    if (isEqual(first, item)) {
      let rest = list.tail;
      return listPrepend(replacement, rest);
    } else {
      let first = list.head;
      let rest = list.tail;
      return listPrepend(first, replace_first(rest, item, replacement));
    }
  }
}

/**
 * Clear the screen.
 * 
 * @ignore
 */
function clear() {
  $io.print("\u{1b}c");
  return $io.print("\u{1b}[H\u{1b}[J");
}

/**
 * Move the cursor up a given number of lines.
 * 
 * @ignore
 */
function cursor_up(n) {
  return $io.print(("\u{1b}[" + $int.to_string(n)) + "A");
}

/**
 * Clear the line the cursor is currently on.
 * 
 * @ignore
 */
function clear_line() {
  return $io.print("\u{1b}[2K");
}

function report_status(result) {
  if (result instanceof Ok) {
    $io.println($ansi.green("🐦‍⬛ Done!"));
    return exit(0);
  } else {
    let error = result[0];
    $io.println_error($ansi.red("Error: ") + explain(error));
    return exit(1);
  }
}

function ask_yes_or_no(prompt) {
  let $ = get_line(prompt + " [Y/n] ");
  if ($ instanceof Ok) {
    let line = $[0];
    let $1 = (() => {
      let _pipe = $string.lowercase(line);
      return $string.trim(_pipe);
    })();
    if ($1 === "yes") {
      return new Yes();
    } else if ($1 === "y") {
      return new Yes();
    } else if ($1 === "") {
      return new Yes();
    } else {
      return new No();
    }
  } else {
    return new No();
  }
}

/**
 * Asks the user to make a choice: it first prints a reminder of the options
 * and waits for the user to choose one.
 * Will prompt again if the choice is not amongst the possible options.
 * 
 * @ignore
 */
function ask_choice(loop$mode) {
  while (true) {
    let mode = loop$mode;
    let _block;
    if (mode instanceof ShowDiff) {
      _block = " hide diff  ";
    } else {
      _block = " show diff  ";
    }
    let diff_message = _block;
    $io.println(
      (((($ansi.bold($ansi.green("  a")) + " accept     ") + $ansi.dim(
        "accept the new snapshot\n",
      )) + (($ansi.bold($ansi.red("  r")) + " reject     ") + $ansi.dim(
        "reject the new snapshot\n",
      ))) + (($ansi.bold($ansi.yellow("  s")) + " skip       ") + $ansi.dim(
        "skip the snapshot for now\n",
      ))) + (($ansi.bold($ansi.cyan("  d")) + diff_message) + $ansi.dim(
        "toggle snapshot diff\n",
      )),
    );
    clear_line();
    let $ = $result.map(get_line("> "), $string.trim);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 === "a") {
        return new Ok(new AcceptSnapshot());
      } else if ($1 === "r") {
        return new Ok(new RejectSnapshot());
      } else if ($1 === "s") {
        return new Ok(new SkipSnapshot());
      } else if ($1 === "d") {
        return new Ok(new ToggleDiffView());
      } else {
        cursor_up(6);
        loop$mode = mode;
      }
    } else {
      return new Error(new CannotReadUserInput());
    }
  }
}

function serialise(snapshot) {
  let title;
  let content;
  let info;
  title = snapshot.title;
  content = snapshot.content;
  info = snapshot.info;
  let _block;
  if (info instanceof Some) {
    let file = info[0].file;
    let test_name = info[0].test_name;
    _block = toList(["file: " + file, "test_name: " + test_name]);
  } else {
    _block = toList([]);
  }
  let info_lines = _block;
  let _pipe = toList([
    toList([
      "---",
      "version: " + birdie_version,
      "title: " + $string.replace(title, "\n", "\\n"),
    ]),
    info_lines,
    toList(["---", content]),
  ]);
  let _pipe$1 = $list.flatten(_pipe);
  let _pipe$2 = $string.join(_pipe$1, "\n");
  return $string.append(_pipe$2, "\n");
}

/**
 * Save a new snapshot to a given path.
 * 
 * @ignore
 */
function save(snapshot, destination) {
  let $ = $string.ends_with(destination, ".new");
  if ($) {
    let _pipe = $simplifile.write(destination, serialise(snapshot));
    return $result.map_error(
      _pipe,
      (_capture) => {
        return new CannotSaveNewSnapshot(_capture, snapshot.title, destination);
      },
    );
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "birdie",
      421,
      "save",
      "Looks like I've messed up something, all new snapshots should have the `.new` extension",
      {}
    )
  }
}

/**
 * List all the accepted snapshots in a folder. Every file is automatically
 * prepended with the folder so you get the full path of each file.
 * 
 * @ignore
 */
function list_accepted_snapshots(folder) {
  let $ = $simplifile.read_directory(folder);
  if ($ instanceof Ok) {
    let files = $[0];
    return new Ok(
      $list.filter_map(
        files,
        (file) => {
          let $1 = $filepath.extension(file);
          if ($1 instanceof Ok) {
            let extension = $1[0];
            if (extension === "accepted") {
              return new Ok($filepath.join(folder, file));
            } else {
              return new Error(undefined);
            }
          } else {
            return new Error(undefined);
          }
        },
      ),
    );
  } else {
    let reason = $[0];
    return new Error(new CannotReadSnapshots(reason, folder));
  }
}

/**
 * Turns a new snapshot path into the path of the corresponding accepted
 * snapshot.
 * 
 * @ignore
 */
function to_accepted_path(file) {
  return ($filepath.strip_extension(file) + ".") + accepted_extension;
}

function accept_snapshot(new_snapshot_path, titles) {
  return $result.try$(
    read_new(new_snapshot_path),
    (snapshot) => {
      let title;
      let content;
      title = snapshot.title;
      content = snapshot.content;
      let accepted_snapshot_path = to_accepted_path(new_snapshot_path);
      return $result.try$(
        referenced_file_path(),
        (referenced_file) => {
          return $result.try$(
            (() => {
              let $ = $simplifile.is_file(referenced_file);
              if ($ instanceof Ok) {
                return new Ok(undefined);
              } else {
                let _pipe = $simplifile.create_file(referenced_file);
                return $result.map_error(
                  _pipe,
                  (_capture) => {
                    return new CannotCreateReferencedFile(
                      referenced_file,
                      _capture,
                    );
                  },
                );
              }
            })(),
            (_) => {
              return $result.try$(
                (() => {
                  let _pipe = $simplifile.append(
                    referenced_file,
                    $filepath.base_name(accepted_snapshot_path) + "\n",
                  );
                  return $result.map_error(
                    _pipe,
                    (var0) => {
                      return new CannotMarkSnapshotAsReferenced(var0);
                    },
                  );
                })(),
                (_) => {
                  let $ = $titles.find(titles, title);
                  if ($ instanceof Ok) {
                    let $1 = $[0];
                    if ($1 instanceof $titles.Literal) {
                      let info = $1.info;
                      let _block;
                      let _pipe = $simplifile.delete$(new_snapshot_path);
                      _block = $result.map_error(
                        _pipe,
                        (_capture) => {
                          return new CannotAcceptSnapshot(
                            _capture,
                            new_snapshot_path,
                          );
                        },
                      );
                      let delete_new_snapshot = _block;
                      return $result.try$(
                        delete_new_snapshot,
                        (_) => {
                          let _pipe$1 = new Snapshot(
                            title,
                            content,
                            new Some(info),
                          );
                          let _pipe$2 = serialise(_pipe$1);
                          let _pipe$3 = $simplifile.write(
                            accepted_snapshot_path,
                            _pipe$2,
                          );
                          return $result.map_error(
                            _pipe$3,
                            (_capture) => {
                              return new CannotAcceptSnapshot(
                                _capture,
                                accepted_snapshot_path,
                              );
                            },
                          );
                        },
                      );
                    } else {
                      let info = $1.info;
                      let _block;
                      let _pipe = $simplifile.delete$(new_snapshot_path);
                      _block = $result.map_error(
                        _pipe,
                        (_capture) => {
                          return new CannotAcceptSnapshot(
                            _capture,
                            new_snapshot_path,
                          );
                        },
                      );
                      let delete_new_snapshot = _block;
                      return $result.try$(
                        delete_new_snapshot,
                        (_) => {
                          let _pipe$1 = new Snapshot(
                            title,
                            content,
                            new Some(info),
                          );
                          let _pipe$2 = serialise(_pipe$1);
                          let _pipe$3 = $simplifile.write(
                            accepted_snapshot_path,
                            _pipe$2,
                          );
                          return $result.map_error(
                            _pipe$3,
                            (_capture) => {
                              return new CannotAcceptSnapshot(
                                _capture,
                                accepted_snapshot_path,
                              );
                            },
                          );
                        },
                      );
                    }
                  } else {
                    let _pipe = $simplifile.rename(
                      new_snapshot_path,
                      accepted_snapshot_path,
                    );
                    return $result.map_error(
                      _pipe,
                      (_capture) => {
                        return new CannotAcceptSnapshot(
                          _capture,
                          new_snapshot_path,
                        );
                      },
                    );
                  }
                },
              );
            },
          );
        },
      );
    },
  );
}

function update_accepted_snapshots(snapshots_folder, titles) {
  return $result.try$(
    list_accepted_snapshots(snapshots_folder),
    (accepted_snapshots) => {
      return $list.try_each(
        accepted_snapshots,
        (accepted_snapshot) => {
          return $result.try$(
            read_accepted(accepted_snapshot),
            (snapshot) => {
              if (snapshot instanceof Some) {
                let snapshot$1 = snapshot[0];
                let title = snapshot[0].title;
                let info = snapshot[0].info;
                let $ = $titles.find(titles, title);
                if ($ instanceof Ok) {
                  if (info instanceof Some) {
                    let match = $[0];
                    let existing_info = info[0];
                    if (!isEqual(match.info, existing_info)) {
                      let _pipe = new Snapshot(
                        snapshot$1.title,
                        snapshot$1.content,
                        new Some(match.info),
                      );
                      let _pipe$1 = serialise(_pipe);
                      let _pipe$2 = $simplifile.write(
                        accepted_snapshot,
                        _pipe$1,
                      );
                      return $result.map_error(
                        _pipe$2,
                        (_capture) => {
                          return new CannotAcceptSnapshot(
                            _capture,
                            accepted_snapshot,
                          );
                        },
                      );
                    } else {
                      return new Ok(undefined);
                    }
                  } else {
                    let match = $[0];
                    let _pipe = new Snapshot(
                      snapshot$1.title,
                      snapshot$1.content,
                      new Some(match.info),
                    );
                    let _pipe$1 = serialise(_pipe);
                    let _pipe$2 = $simplifile.write(accepted_snapshot, _pipe$1);
                    return $result.map_error(
                      _pipe$2,
                      (_capture) => {
                        return new CannotAcceptSnapshot(
                          _capture,
                          accepted_snapshot,
                        );
                      },
                    );
                  }
                } else {
                  return new Ok(undefined);
                }
              } else {
                return new Ok(undefined);
              }
            },
          );
        },
      );
    },
  );
}

/**
 * Reviews all the new snapshots one by one.
 * 
 * @ignore
 */
function review_loop(new_snapshot_paths, titles, current, out_of, mode) {
  if (new_snapshot_paths instanceof $Empty) {
    return new Ok(undefined);
  } else {
    let new_snapshot_path = new_snapshot_paths.head;
    let rest = new_snapshot_paths.tail;
    clear();
    return $result.try$(
      read_new(new_snapshot_path),
      (new_snapshot) => {
        let _block;
        let $ = $titles.find(titles, new_snapshot.title);
        if ($ instanceof Ok) {
          let $1 = $[0];
          if ($1 instanceof $titles.Literal) {
            let info = $1.info;
            _block = new Some(info);
          } else {
            let info = $1.info;
            _block = new Some(info);
          }
        } else {
          _block = new None();
        }
        let new_snapshot_info = _block;
        let new_snapshot$1 = new Snapshot(
          new_snapshot.title,
          new_snapshot.content,
          new_snapshot_info,
        );
        let accepted_snapshot_path = to_accepted_path(new_snapshot_path);
        return $result.try$(
          read_accepted(accepted_snapshot_path),
          (accepted_snapshot) => {
            let progress = (($ansi.dim("Reviewing ") + $ansi.bold(
              $ansi.yellow($rank.ordinalise(current)),
            )) + $ansi.dim(" out of ")) + $ansi.bold(
              $ansi.yellow($int.to_string(out_of)),
            );
            let _block$1;
            if (accepted_snapshot instanceof Some) {
              if (mode instanceof ShowDiff) {
                let accepted_snapshot$1 = accepted_snapshot[0];
                _block$1 = diff_snapshot_box(
                  accepted_snapshot$1,
                  new_snapshot$1,
                  toList([]),
                );
              } else {
                _block$1 = regular_snapshot_box(new_snapshot$1, toList([]));
              }
            } else {
              _block$1 = new_snapshot_box(new_snapshot$1, toList([]));
            }
            let box = _block$1;
            $io.println(((progress + "\n\n") + box) + "\n");
            return $result.try$(
              ask_choice(mode),
              (choice) => {
                if (choice instanceof AcceptSnapshot) {
                  return $result.try$(
                    accept_snapshot(new_snapshot_path, titles),
                    (_) => {
                      return review_loop(
                        rest,
                        titles,
                        current + 1,
                        out_of,
                        mode,
                      );
                    },
                  );
                } else if (choice instanceof RejectSnapshot) {
                  return $result.try$(
                    reject_snapshot(new_snapshot_path),
                    (_) => {
                      return review_loop(
                        rest,
                        titles,
                        current + 1,
                        out_of,
                        mode,
                      );
                    },
                  );
                } else if (choice instanceof SkipSnapshot) {
                  return review_loop(rest, titles, current + 1, out_of, mode);
                } else {
                  let mode$1 = toggle_mode(mode);
                  return review_loop(
                    new_snapshot_paths,
                    titles,
                    current,
                    out_of,
                    mode$1,
                  );
                }
              },
            );
          },
        );
      },
    );
  }
}

function stale_snapshots_file_names() {
  return $result.try$(
    snapshot_folder(),
    (snapshots_folder) => {
      return $result.try$(
        referenced_file_path(),
        (referenced_file) => {
          let $ = $simplifile.read(referenced_file);
          if ($ instanceof Ok) {
            let non_stale_snapshots = $[0];
            let _block;
            let _pipe = $simplifile.get_files(snapshots_folder);
            let _pipe$1 = $result.unwrap(_pipe, toList([]));
            _block = $list.fold(
              _pipe$1,
              $set.new$(),
              (files, file) => {
                let $1 = isEqual(
                  $filepath.extension(file),
                  new Ok(accepted_extension)
                );
                if ($1) {
                  return $set.insert(files, $filepath.base_name(file));
                } else {
                  return files;
                }
              },
            );
            let existing_accepted_snapshots = _block;
            let non_stale_snapshots$1 = $string.split(non_stale_snapshots, "\n");
            let _pipe$2 = existing_accepted_snapshots;
            let _pipe$3 = $set.drop(_pipe$2, non_stale_snapshots$1);
            let _pipe$4 = $set.to_list(_pipe$3);
            return new Ok(_pipe$4);
          } else {
            let $1 = $[0];
            if ($1 instanceof Enoent) {
              return new Error(new MissingReferencedFile());
            } else {
              let reason = $1;
              return new Error(
                new CannotReadReferencedFile(referenced_file, reason),
              );
            }
          }
        },
      );
    },
  );
}

function check_stale() {
  $io.println("Checking stale snapshots...");
  return $result.try$(
    stale_snapshots_file_names(),
    (stale_snapshots) => {
      if (stale_snapshots instanceof $Empty) {
        return new Ok(undefined);
      } else {
        return new Error(new StaleSnapshotsFound(stale_snapshots));
      }
    },
  );
}

function delete_stale() {
  $io.println("Checking stale snapshots...");
  return $result.try$(
    snapshot_folder(),
    (snapshots_folder) => {
      return $result.try$(
        stale_snapshots_file_names(),
        (stale_snapshots) => {
          let _pipe = $list.try_each(
            stale_snapshots,
            (stale_snapshot) => {
              let _pipe = $filepath.join(snapshots_folder, stale_snapshot);
              return $simplifile.delete$(_pipe);
            },
          );
          return $result.map_error(
            _pipe,
            (_capture) => { return new CannotDeleteStaleSnapshot(_capture); },
          );
        },
      );
    },
  );
}

/**
 * List all the new snapshots in a folder. Every file is automatically
 * prepended with the folder so you get the full path of each file.
 * 
 * @ignore
 */
function list_new_snapshots(folder) {
  let $ = $simplifile.read_directory(folder);
  if ($ instanceof Ok) {
    let files = $[0];
    return new Ok(
      $list.filter_map(
        files,
        (file) => {
          let $1 = $filepath.extension(file);
          if ($1 instanceof Ok) {
            let extension = $1[0];
            if (extension === "new") {
              return new Ok($filepath.join(folder, file));
            } else {
              return new Error(undefined);
            }
          } else {
            return new Error(undefined);
          }
        },
      ),
    );
  } else {
    let reason = $[0];
    return new Error(new CannotReadSnapshots(reason, folder));
  }
}

/**
 * Returns the path where a new snapshot should be saved.
 * 
 * @ignore
 */
function new_destination(snapshot, folder) {
  return ($filepath.join(folder, file_name(snapshot.title)) + ".") + new_extension;
}

function do_snap(content, title) {
  return $result.try$(
    validate_snapshot_title(title),
    (_) => {
      return $result.try$(
        snapshot_folder(),
        (folder) => {
          let new$ = new Snapshot(title, content, new None());
          let new_snapshot_path = new_destination(new$, folder);
          let accepted_snapshot_path = to_accepted_path(new_snapshot_path);
          return $result.try$(
            read_accepted(accepted_snapshot_path),
            (accepted) => {
              if (accepted instanceof Some) {
                let accepted$1 = accepted[0];
                return $result.try$(
                  global_referenced_file(),
                  (referenced_file) => {
                    return $result.try$(
                      (() => {
                        let _pipe = $simplifile.append(
                          referenced_file,
                          $filepath.base_name(accepted_snapshot_path) + "\n",
                        );
                        return $result.map_error(
                          _pipe,
                          (var0) => {
                            return new CannotMarkSnapshotAsReferenced(var0);
                          },
                        );
                      })(),
                      (_) => {
                        let $ = accepted$1.content === new$.content;
                        if ($) {
                          let $1 = $simplifile.delete$(new_snapshot_path);
                          
                          return new Ok(new Same());
                        } else {
                          return $result.try$(
                            save(new$, new_snapshot_path),
                            (_) => {
                              return new Ok(new Different(accepted$1, new$));
                            },
                          );
                        }
                      },
                    );
                  },
                );
              } else {
                return $result.try$(
                  save(new$, new_snapshot_path),
                  (_) => {
                    return new Ok(
                      new NewSnapshotCreated(new$, new_snapshot_path),
                    );
                  },
                );
              }
            },
          );
        },
      );
    },
  );
}

/**
 * Performs a snapshot test with the given title, saving the content to a new
 * snapshot file. All your snapshots will be stored in a folder called
 * `birdie_snapshots` in the project's root.
 *
 * The test will fail if there already is an accepted snapshot with the same
 * title and a different content.
 * The test will also fail if there's no accepted snapshot with the same title
 * to make sure you will review new snapshots as well.
 *
 * > 🚨 A snapshot is saved to a file named after its title, so all titles
 * > should be unique! Otherwise you'd end up comparing unrelated snapshots.
 *
 * > 🐦‍⬛ To review all your snapshots interactively you can run
 * > `gleam run -m birdie`.
 * >
 * > To get an help text and all the available options you can run
 * > `gleam run -m birdie help`.
 */
export function snap(content, title) {
  let $ = do_snap(content, title);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof NewSnapshotCreated) {
      let snapshot = $1.snapshot;
      let hint_message = $ansi.yellow(hint_review_message);
      let hint = new InfoLineWithTitle(hint_message, new DoNotSplit(), "hint");
      let box = new_snapshot_box(snapshot, toList([hint]));
      $io.println_error(("\n\n" + box) + "\n");
      throw makeError(
        "panic",
        FILEPATH,
        "birdie",
        184,
        "snap",
        "Birdie snapshot test failed",
        {}
      )
    } else if ($1 instanceof Different) {
      let accepted = $1.accepted;
      let new$ = $1.new;
      let hint_message = $ansi.yellow(hint_review_message);
      let hint = new InfoLineWithTitle(hint_message, new DoNotSplit(), "hint");
      let box = diff_snapshot_box(accepted, new$, toList([hint]));
      $io.println_error(("\n\n" + box) + "\n");
      throw makeError(
        "panic",
        FILEPATH,
        "birdie",
        193,
        "snap",
        "Birdie snapshot test failed",
        {}
      )
    } else {
      return undefined;
    }
  } else {
    let error = $[0];
    let panic_message = "Birdie snapshot test failed\n" + explain(error);
    throw makeError("panic", FILEPATH, "birdie", 198, "snap", panic_message, {})
  }
}

function do_review(snapshots_folder, titles) {
  return $result.try$(
    list_new_snapshots(snapshots_folder),
    (new_snapshots) => {
      let $ = $list.length(new_snapshots);
      if ($ === 0) {
        $io.println("No new snapshots to review.");
        return new Ok(undefined);
      } else {
        let n = $;
        let result = review_loop(new_snapshots, titles, 1, n, new ShowDiff());
        clear();
        return $result.try$(
          result,
          (_) => {
            $io.println(
              (() => {
                if (n === 1) {
                  return "Reviewed one snapshot";
                } else {
                  let n$1 = n;
                  return ("Reviewed " + $int.to_string(n$1)) + " snapshots";
                }
              })(),
            );
            return new Ok(undefined);
          },
        );
      }
    },
  );
}

function review() {
  return $result.try$(
    snapshot_folder(),
    (snapshots_folder) => {
      let get_titles = $titles.from_test_directory();
      return $result.try$(
        $result.map_error(
          get_titles,
          (var0) => { return new CannotGetTitles(var0); },
        ),
        (titles) => {
          return $result.try$(
            update_accepted_snapshots(snapshots_folder, titles),
            (_) => {
              return $result.try$(
                do_review(snapshots_folder, titles),
                (_) => { return new Ok(undefined); },
              );
            },
          );
        },
      );
    },
  );
}

function accept_all() {
  $io.println("Looking for new snapshots...");
  return $result.try$(
    snapshot_folder(),
    (snapshots_folder) => {
      return $result.try$(
        list_new_snapshots(snapshots_folder),
        (new_snapshots) => {
          let get_titles = $titles.from_test_directory();
          return $result.try$(
            $result.map_error(
              get_titles,
              (var0) => { return new CannotGetTitles(var0); },
            ),
            (titles) => {
              return $result.try$(
                update_accepted_snapshots(snapshots_folder, titles),
                (_) => {
                  let $ = $list.length(new_snapshots);
                  if ($ === 0) {
                    $io.println("No new snapshots to accept.")
                  } else if ($ === 1) {
                    $io.println("Accepting one new snapshot.")
                  } else {
                    let n = $;
                    $io.println(
                      ("Accepting " + $int.to_string(n)) + " new snapshots.",
                    )
                  }
                  return $list.try_each(
                    new_snapshots,
                    (_capture) => { return accept_snapshot(_capture, titles); },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

function reject_all() {
  $io.println("Looking for new snapshots...");
  return $result.try$(
    snapshot_folder(),
    (snapshots_folder) => {
      return $result.try$(
        list_new_snapshots(snapshots_folder),
        (new_snapshots) => {
          let get_titles = $titles.from_test_directory();
          return $result.try$(
            $result.map_error(
              get_titles,
              (var0) => { return new CannotGetTitles(var0); },
            ),
            (titles) => {
              return $result.try$(
                update_accepted_snapshots(snapshots_folder, titles),
                (_) => {
                  let $ = $list.length(new_snapshots);
                  if ($ === 0) {
                    $io.println("No new snapshots to reject.")
                  } else if ($ === 1) {
                    $io.println("Rejecting one new snapshot.")
                  } else {
                    let n = $;
                    $io.println(
                      ("Rejecting " + $int.to_string(n)) + " new snapshots.",
                    )
                  }
                  return $list.try_each(new_snapshots, reject_snapshot);
                },
              );
            },
          );
        },
      );
    },
  );
}

function run_command(command) {
  if (command instanceof Review) {
    return report_status(review());
  } else if (command instanceof Accept) {
    return report_status(accept_all());
  } else if (command instanceof Reject) {
    return report_status(reject_all());
  } else if (command instanceof Help) {
    return $io.println(
      $cli.help_text(birdie_version, new Help(), new FullCommand()),
    );
  } else if (command instanceof Stale) {
    let $ = command.subcommand;
    if ($ instanceof CheckStale) {
      return report_status(check_stale());
    } else {
      return report_status(delete_stale());
    }
  } else {
    let command$1 = command.command;
    let explained = command.explained;
    return $io.println($cli.help_text(birdie_version, command$1, explained));
  }
}

function parse_and_run(loop$args) {
  while (true) {
    let args = loop$args;
    let $ = $cli.parse(args);
    if ($ instanceof Ok) {
      let command = $[0];
      return run_command(command);
    } else {
      let $1 = $[0];
      if ($1 instanceof UnknownCommand) {
        let command = $1.command;
        let $2 = $cli.similar_command(command);
        if ($2 instanceof Ok) {
          let new_command = $2[0];
          let _pipe = $cli.unknown_command_error(command, false);
          $io.println(_pipe)
          let prompt = ("I think you misspelled `" + new_command) + "`, would you like me to run it instead?";
          let $3 = ask_yes_or_no(prompt);
          if ($3 instanceof Yes) {
            let _pipe$1 = replace_first(args, command, new_command);
            loop$args = _pipe$1;
          } else {
            $io.println("\n" + $cli.main_help_text());
            return exit(1);
          }
        } else {
          let _pipe = $cli.unknown_command_error(command, true);
          $io.println(_pipe)
          return exit(1);
        }
      } else if ($1 instanceof UnknownSubcommand) {
        let command = $1.command;
        let subcommand = $1.subcommand;
        let _pipe = $cli.unknown_subcommand_error(
          birdie_version,
          command,
          subcommand,
        );
        $io.println(_pipe)
        return exit(1);
      } else if ($1 instanceof UnexpectedArgument) {
        let command = $1.command;
        let argument = $1.argument;
        let _pipe = $cli.unexpected_argument_error(
          birdie_version,
          command,
          argument,
        );
        $io.println(_pipe)
        return exit(1);
      } else if ($1 instanceof UnknownOption) {
        let command = $1.command;
        let option = $1.option;
        let _pipe = $cli.unknown_option_error(birdie_version, command, option);
        $io.println(_pipe)
        return exit(1);
      } else {
        let command = $1.command;
        let _pipe = $cli.missing_subcommand_error(birdie_version, command);
        $io.println(_pipe)
        return exit(1);
      }
    }
  }
}

/**
 * Reviews the snapshots in the project's folder.
 * This function will behave differently depending on the command line
 * arguments provided to the program.
 * To have a look at all the available options you can run
 * `gleam run -m birdie help`.
 *
 * > 🐦‍⬛ The recommended workflow is to first run your gleeunit tests with
 * > `gleam test` and then review any new/failing snapshot manually running
 * > `gleam run -m birdie`.
 * >
 * > And don't forget to commit your snapshots! Those should be treated as code
 * > and checked with the vcs you're using.
 */
export function main() {
  return parse_and_run($argv.load().arguments);
}
