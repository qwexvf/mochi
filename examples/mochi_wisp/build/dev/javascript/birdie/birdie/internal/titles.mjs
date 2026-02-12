import * as $filepath from "../../../filepath/filepath.mjs";
import * as $glance from "../../../glance/glance.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $trie from "../../../trie_again/trie.mjs";
import * as $project from "../../birdie/internal/project.mjs";
import { Ok, Error, Empty as $Empty, CustomType as $CustomType, isEqual } from "../../gleam.mjs";

class Titles extends $CustomType {
  constructor(literals, prefixes) {
    super();
    this.literals = literals;
    this.prefixes = prefixes;
  }
}

export class TestInfo extends $CustomType {
  constructor(file, test_name) {
    super();
    this.file = file;
    this.test_name = test_name;
  }
}
export const TestInfo$TestInfo = (file, test_name) =>
  new TestInfo(file, test_name);
export const TestInfo$isTestInfo = (value) => value instanceof TestInfo;
export const TestInfo$TestInfo$file = (value) => value.file;
export const TestInfo$TestInfo$0 = (value) => value.file;
export const TestInfo$TestInfo$test_name = (value) => value.test_name;
export const TestInfo$TestInfo$1 = (value) => value.test_name;

export class Literal extends $CustomType {
  constructor(info) {
    super();
    this.info = info;
  }
}
export const Match$Literal = (info) => new Literal(info);
export const Match$isLiteral = (value) => value instanceof Literal;
export const Match$Literal$info = (value) => value.info;
export const Match$Literal$0 = (value) => value.info;

export class Prefix extends $CustomType {
  constructor(info, prefix) {
    super();
    this.info = info;
    this.prefix = prefix;
  }
}
export const Match$Prefix = (info, prefix) => new Prefix(info, prefix);
export const Match$isPrefix = (value) => value instanceof Prefix;
export const Match$Prefix$info = (value) => value.info;
export const Match$Prefix$0 = (value) => value.info;
export const Match$Prefix$prefix = (value) => value.prefix;
export const Match$Prefix$1 = (value) => value.prefix;

export const Match$info = (value) => value.info;

export class CannotFindProjectRoot extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const Error$CannotFindProjectRoot = (reason) =>
  new CannotFindProjectRoot(reason);
export const Error$isCannotFindProjectRoot = (value) =>
  value instanceof CannotFindProjectRoot;
export const Error$CannotFindProjectRoot$reason = (value) => value.reason;
export const Error$CannotFindProjectRoot$0 = (value) => value.reason;

export class CannotReadTestDirectory extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const Error$CannotReadTestDirectory = (reason) =>
  new CannotReadTestDirectory(reason);
export const Error$isCannotReadTestDirectory = (value) =>
  value instanceof CannotReadTestDirectory;
export const Error$CannotReadTestDirectory$reason = (value) => value.reason;
export const Error$CannotReadTestDirectory$0 = (value) => value.reason;

export class CannotReadTestFile extends $CustomType {
  constructor(reason, file) {
    super();
    this.reason = reason;
    this.file = file;
  }
}
export const Error$CannotReadTestFile = (reason, file) =>
  new CannotReadTestFile(reason, file);
export const Error$isCannotReadTestFile = (value) =>
  value instanceof CannotReadTestFile;
export const Error$CannotReadTestFile$reason = (value) => value.reason;
export const Error$CannotReadTestFile$0 = (value) => value.reason;
export const Error$CannotReadTestFile$file = (value) => value.file;
export const Error$CannotReadTestFile$1 = (value) => value.file;

export class DuplicateLiteralTitles extends $CustomType {
  constructor(title, one, other) {
    super();
    this.title = title;
    this.one = one;
    this.other = other;
  }
}
export const Error$DuplicateLiteralTitles = (title, one, other) =>
  new DuplicateLiteralTitles(title, one, other);
export const Error$isDuplicateLiteralTitles = (value) =>
  value instanceof DuplicateLiteralTitles;
export const Error$DuplicateLiteralTitles$title = (value) => value.title;
export const Error$DuplicateLiteralTitles$0 = (value) => value.title;
export const Error$DuplicateLiteralTitles$one = (value) => value.one;
export const Error$DuplicateLiteralTitles$1 = (value) => value.one;
export const Error$DuplicateLiteralTitles$other = (value) => value.other;
export const Error$DuplicateLiteralTitles$2 = (value) => value.other;

export class OverlappingPrefixes extends $CustomType {
  constructor(prefix, info, other_prefix, other_info) {
    super();
    this.prefix = prefix;
    this.info = info;
    this.other_prefix = other_prefix;
    this.other_info = other_info;
  }
}
export const Error$OverlappingPrefixes = (prefix, info, other_prefix, other_info) =>
  new OverlappingPrefixes(prefix, info, other_prefix, other_info);
export const Error$isOverlappingPrefixes = (value) =>
  value instanceof OverlappingPrefixes;
export const Error$OverlappingPrefixes$prefix = (value) => value.prefix;
export const Error$OverlappingPrefixes$0 = (value) => value.prefix;
export const Error$OverlappingPrefixes$info = (value) => value.info;
export const Error$OverlappingPrefixes$1 = (value) => value.info;
export const Error$OverlappingPrefixes$other_prefix = (value) =>
  value.other_prefix;
export const Error$OverlappingPrefixes$2 = (value) => value.other_prefix;
export const Error$OverlappingPrefixes$other_info = (value) => value.other_info;
export const Error$OverlappingPrefixes$3 = (value) => value.other_info;

export class PrefixOverlappingWithLiteralTitle extends $CustomType {
  constructor(prefix, prefix_info, title_info) {
    super();
    this.prefix = prefix;
    this.prefix_info = prefix_info;
    this.title_info = title_info;
  }
}
export const Error$PrefixOverlappingWithLiteralTitle = (prefix, prefix_info, title_info) =>
  new PrefixOverlappingWithLiteralTitle(prefix, prefix_info, title_info);
export const Error$isPrefixOverlappingWithLiteralTitle = (value) =>
  value instanceof PrefixOverlappingWithLiteralTitle;
export const Error$PrefixOverlappingWithLiteralTitle$prefix = (value) =>
  value.prefix;
export const Error$PrefixOverlappingWithLiteralTitle$0 = (value) =>
  value.prefix;
export const Error$PrefixOverlappingWithLiteralTitle$prefix_info = (value) =>
  value.prefix_info;
export const Error$PrefixOverlappingWithLiteralTitle$1 = (value) =>
  value.prefix_info;
export const Error$PrefixOverlappingWithLiteralTitle$title_info = (value) =>
  value.title_info;
export const Error$PrefixOverlappingWithLiteralTitle$2 = (value) =>
  value.title_info;

class Unqualified extends $CustomType {
  constructor(module_alias) {
    super();
    this.module_alias = module_alias;
  }
}

class Qualified extends $CustomType {
  constructor(module_alias, snap_alias) {
    super();
    this.module_alias = module_alias;
    this.snap_alias = snap_alias;
  }
}

class Discarded extends $CustomType {
  constructor(snap_alias) {
    super();
    this.snap_alias = snap_alias;
  }
}

class LiteralTitle extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class PrefixTitle extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export function new$() {
  return new Titles($dict.new$(), $trie.new$());
}

function add_literal_title(titles, title, info) {
  let literals$1 = titles.literals;
  let new_literals = $dict.insert(literals$1, title, info);
  return new Titles(new_literals, titles.prefixes);
}

export function literals(titles) {
  let literals$1;
  literals$1 = titles.literals;
  return literals$1;
}

export function prefixes(titles) {
  let prefixes$1;
  prefixes$1 = titles.prefixes;
  let _pipe = prefixes$1;
  let _pipe$1 = $trie.to_list(_pipe);
  let _pipe$2 = $list.map(
    _pipe$1,
    (pair) => {
      let prefix;
      let info;
      prefix = pair[0];
      info = pair[1];
      return [$string.join(prefix, ""), info];
    },
  );
  return $dict.from_list(_pipe$2);
}

export function find(titles, title) {
  let literal_match = $result.map(
    $dict.get(titles.literals, title),
    (var0) => { return new Literal(var0); },
  );
  return $result.lazy_or(
    literal_match,
    () => {
      let letters = $string.to_graphemes(title);
      return $result.try$(
        $trie.subtrie(titles.prefixes, letters),
        (matching_prefixes) => {
          let $ = $trie.to_list(matching_prefixes);
          if ($ instanceof $Empty) {
            return new Error(undefined);
          } else {
            let prefix = $.head[0];
            let info = $.head[1];
            return new Ok(new Prefix(info, $string.join(prefix, "")));
          }
        },
      );
    },
  );
}

function imported_snap(values) {
  return $list.fold_until(
    values,
    new Error(undefined),
    (nil, value) => {
      let $ = value.alias;
      if ($ instanceof Some) {
        let $1 = value.name;
        if ($1 === "snap") {
          let alias = $[0];
          return new $list.Stop(new Ok(alias));
        } else {
          return new $list.Continue(nil);
        }
      } else {
        let $1 = value.name;
        if ($1 === "snap") {
          return new $list.Stop(new Ok("snap"));
        } else {
          return new $list.Continue(nil);
        }
      }
    },
  );
}

function birdie_import(module) {
  return $list.fold_until(
    module.imports,
    new Error(undefined),
    (nil, import_) => {
      let $ = import_.definition;
      let $1 = $.module;
      if ($1 === "birdie") {
        let birdie_alias = $.alias;
        let unqualified_values = $.unqualified_values;
        if (birdie_alias instanceof Some) {
          let $2 = birdie_alias[0];
          if ($2 instanceof $glance.Named) {
            let module_name = $2[0];
            let $3 = imported_snap(unqualified_values);
            if ($3 instanceof Ok) {
              let snap_alias = $3[0];
              return new $list.Stop(
                new Ok(new Qualified(module_name, snap_alias)),
              );
            } else {
              return new $list.Stop(new Ok(new Unqualified(module_name)));
            }
          } else {
            let $3 = imported_snap(unqualified_values);
            if ($3 instanceof Ok) {
              let snap_alias = $3[0];
              return new $list.Stop(new Ok(new Discarded(snap_alias)));
            } else {
              return new $list.Stop(new Error(undefined));
            }
          }
        } else {
          let $2 = imported_snap(unqualified_values);
          if ($2 instanceof Ok) {
            let snap_alias = $2[0];
            return new $list.Stop(new Ok(new Qualified("birdie", snap_alias)));
          } else {
            return new $list.Stop(new Ok(new Unqualified("birdie")));
          }
        }
      } else {
        return new $list.Continue(nil);
      }
    },
  );
}

function is_snap_function(expression, birdie_import) {
  let is_a_call_to_snap = (module, name) => {
    if (module instanceof Some) {
      if (birdie_import instanceof Unqualified) {
        let module$1 = module[0];
        let birdie = birdie_import.module_alias;
        return ((module$1 + ".") + name) === (birdie + ".snap");
      } else if (birdie_import instanceof Qualified) {
        let module$1 = module[0];
        let birdie = birdie_import.module_alias;
        let snap = birdie_import.snap_alias;
        return ((module$1 + ".") + name) === ((birdie + ".") + snap);
      } else {
        return false;
      }
    } else if (birdie_import instanceof Unqualified) {
      return false;
    } else if (birdie_import instanceof Qualified) {
      let snap = birdie_import.snap_alias;
      return snap === name;
    } else {
      let snap = birdie_import.snap_alias;
      return snap === name;
    }
  };
  if (expression instanceof $glance.Variable) {
    let name = expression.name;
    return is_a_call_to_snap(new None(), name);
  } else if (expression instanceof $glance.FieldAccess) {
    let $ = expression.container;
    if ($ instanceof $glance.Variable) {
      let name = expression.label;
      let module = $.name;
      return is_a_call_to_snap(new Some(module), name);
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function expression_to_snap_title(expression) {
  if (expression instanceof $glance.String) {
    let title = expression.value;
    return new Ok(new LiteralTitle(title));
  } else if (expression instanceof $glance.BinaryOperator) {
    let $ = expression.left;
    if ($ instanceof $glance.String) {
      let $1 = expression.name;
      if ($1 instanceof $glance.Concatenate) {
        let prefix = $.value;
        return new Ok(new PrefixTitle(prefix));
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

function snap_call(birdie_import, expression) {
  if (expression instanceof $glance.Call) {
    let $ = expression.arguments;
    if ($ instanceof $Empty) {
      return new Error(undefined);
    } else {
      let $1 = $.tail;
      if ($1 instanceof $Empty) {
        return new Error(undefined);
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let $3 = $.head;
          if ($3 instanceof $glance.LabelledField) {
            let $4 = $1.head;
            if ($4 instanceof $glance.LabelledField) {
              let $5 = $3.label;
              if ($5 === "content") {
                let $6 = $4.label;
                if ($6 === "title") {
                  let function$ = expression.function;
                  let title = $4.item;
                  let is_snap_function$1 = is_snap_function(
                    function$,
                    birdie_import,
                  );
                  return $bool.guard(
                    !is_snap_function$1,
                    new Error(undefined),
                    () => { return expression_to_snap_title(title); },
                  );
                } else {
                  return new Error(undefined);
                }
              } else if ($5 === "title") {
                let function$ = expression.function;
                let title = $3.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else {
                return new Error(undefined);
              }
            } else if ($4 instanceof $glance.UnlabelledField) {
              let $5 = $3.label;
              if ($5 === "content") {
                let function$ = expression.function;
                let title = $4.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else if ($5 === "title") {
                let function$ = expression.function;
                let title = $3.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else {
                return new Error(undefined);
              }
            } else {
              let $5 = $3.label;
              if ($5 === "title") {
                let function$ = expression.function;
                let title = $3.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else {
                return new Error(undefined);
              }
            }
          } else if ($3 instanceof $glance.UnlabelledField) {
            let $4 = $1.head;
            if ($4 instanceof $glance.LabelledField) {
              let $5 = $4.label;
              if ($5 === "content") {
                let function$ = expression.function;
                let title = $3.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else if ($5 === "title") {
                let function$ = expression.function;
                let title = $4.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else {
                return new Error(undefined);
              }
            } else if ($4 instanceof $glance.UnlabelledField) {
              let function$ = expression.function;
              let title = $4.item;
              let is_snap_function$1 = is_snap_function(
                function$,
                birdie_import,
              );
              return $bool.guard(
                !is_snap_function$1,
                new Error(undefined),
                () => { return expression_to_snap_title(title); },
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
      }
    }
  } else if (expression instanceof $glance.BinaryOperator) {
    let $ = expression.right;
    if ($ instanceof $glance.Call) {
      let $1 = $.arguments;
      if ($1 instanceof $Empty) {
        return new Error(undefined);
      } else {
        let $2 = $1.tail;
        if ($2 instanceof $Empty) {
          let $3 = $1.head;
          if ($3 instanceof $glance.LabelledField) {
            let $4 = expression.name;
            if ($4 instanceof $glance.Pipe) {
              let $5 = $3.label;
              if ($5 === "content") {
                let title = expression.left;
                let function$ = $.function;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else if ($5 === "title") {
                let function$ = $.function;
                let title = $3.item;
                let is_snap_function$1 = is_snap_function(
                  function$,
                  birdie_import,
                );
                return $bool.guard(
                  !is_snap_function$1,
                  new Error(undefined),
                  () => { return expression_to_snap_title(title); },
                );
              } else {
                return new Error(undefined);
              }
            } else {
              return new Error(undefined);
            }
          } else if ($3 instanceof $glance.UnlabelledField) {
            let $4 = expression.name;
            if ($4 instanceof $glance.Pipe) {
              let function$ = $.function;
              let title = $3.item;
              let is_snap_function$1 = is_snap_function(
                function$,
                birdie_import,
              );
              return $bool.guard(
                !is_snap_function$1,
                new Error(undefined),
                () => { return expression_to_snap_title(title); },
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
      }
    } else if ($ instanceof $glance.FnCapture) {
      let $1 = $.label;
      if ($1 instanceof Some) {
        let $2 = expression.name;
        if ($2 instanceof $glance.Pipe) {
          let $3 = $1[0];
          if ($3 === "title") {
            let title = expression.left;
            let function$ = $.function;
            let is_snap_function$1 = is_snap_function(function$, birdie_import);
            return $bool.guard(
              !is_snap_function$1,
              new Error(undefined),
              () => { return expression_to_snap_title(title); },
            );
          } else {
            let $4 = $.arguments_before;
            if ($4 instanceof $Empty) {
              return new Error(undefined);
            } else {
              let $5 = $.arguments_after;
              if ($5 instanceof $Empty) {
                let $6 = $4.tail;
                if ($6 instanceof $Empty) {
                  let $7 = $4.head;
                  if ($7 instanceof $glance.UnlabelledField) {
                    let title = expression.left;
                    let function$ = $.function;
                    let is_snap_function$1 = is_snap_function(
                      function$,
                      birdie_import,
                    );
                    return $bool.guard(
                      !is_snap_function$1,
                      new Error(undefined),
                      () => { return expression_to_snap_title(title); },
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
            }
          }
        } else {
          return new Error(undefined);
        }
      } else {
        let $2 = $.arguments_before;
        if ($2 instanceof $Empty) {
          return new Error(undefined);
        } else {
          let $3 = $.arguments_after;
          if ($3 instanceof $Empty) {
            let $4 = $2.tail;
            if ($4 instanceof $Empty) {
              let $5 = $2.head;
              if ($5 instanceof $glance.UnlabelledField) {
                let $6 = expression.name;
                if ($6 instanceof $glance.Pipe) {
                  let title = expression.left;
                  let function$ = $.function;
                  let is_snap_function$1 = is_snap_function(
                    function$,
                    birdie_import,
                  );
                  return $bool.guard(
                    !is_snap_function$1,
                    new Error(undefined),
                    () => { return expression_to_snap_title(title); },
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
        }
      }
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

function try_or(result, default$, fun) {
  if (result instanceof Ok) {
    let a = result[0];
    return fun(a);
  } else {
    return default$;
  }
}

function try$(result, map_error, fun) {
  if (result instanceof Ok) {
    let a = result[0];
    return fun(a);
  } else {
    let e = result[0];
    return new Error(map_error(e));
  }
}

function try_fold_expression(expression, acc, fun) {
  return $result.try$(
    fun(acc, expression),
    (acc) => {
      if (expression instanceof $glance.Int) {
        return new Ok(acc);
      } else if (expression instanceof $glance.Float) {
        return new Ok(acc);
      } else if (expression instanceof $glance.String) {
        return new Ok(acc);
      } else if (expression instanceof $glance.Variable) {
        return new Ok(acc);
      } else if (expression instanceof $glance.NegateInt) {
        let expression$1 = expression.value;
        return try_fold_expression(expression$1, acc, fun);
      } else if (expression instanceof $glance.NegateBool) {
        let expression$1 = expression.value;
        return try_fold_expression(expression$1, acc, fun);
      } else if (expression instanceof $glance.Block) {
        let statements = expression.statements;
        return try_fold_statements(statements, acc, fun);
      } else if (expression instanceof $glance.Panic) {
        return new Ok(acc);
      } else if (expression instanceof $glance.Todo) {
        return new Ok(acc);
      } else if (expression instanceof $glance.Tuple) {
        let expressions = expression.elements;
        return try_fold_expressions(expressions, acc, fun);
      } else if (expression instanceof $glance.List) {
        let $ = expression.rest;
        if ($ instanceof Some) {
          let elements = expression.elements;
          let rest = $[0];
          return $result.try$(
            try_fold_expressions(elements, acc, fun),
            (acc) => { return try_fold_expression(rest, acc, fun); },
          );
        } else {
          let expressions = expression.elements;
          return try_fold_expressions(expressions, acc, fun);
        }
      } else if (expression instanceof $glance.Fn) {
        let statements = expression.body;
        return try_fold_statements(statements, acc, fun);
      } else if (expression instanceof $glance.RecordUpdate) {
        let record = expression.record;
        let fields = expression.fields;
        return $result.try$(
          try_fold_expression(record, acc, fun),
          (acc) => {
            return $list.try_fold(
              fields,
              acc,
              (acc, field) => {
                let item;
                item = field.item;
                if (item instanceof Some) {
                  let item$1 = item[0];
                  return try_fold_expression(item$1, acc, fun);
                } else {
                  return new Ok(acc);
                }
              },
            );
          },
        );
      } else if (expression instanceof $glance.FieldAccess) {
        let expression$1 = expression.container;
        return try_fold_expression(expression$1, acc, fun);
      } else if (expression instanceof $glance.Call) {
        let function$ = expression.function;
        let arguments$ = expression.arguments;
        return $result.try$(
          try_fold_expression(function$, acc, fun),
          (acc) => { return try_fold_fields(arguments$, acc, fun); },
        );
      } else if (expression instanceof $glance.TupleIndex) {
        let expression$1 = expression.tuple;
        return try_fold_expression(expression$1, acc, fun);
      } else if (expression instanceof $glance.FnCapture) {
        let function$ = expression.function;
        let arguments_before = expression.arguments_before;
        let arguments_after = expression.arguments_after;
        return $result.try$(
          try_fold_expression(function$, acc, fun),
          (acc) => {
            return $result.try$(
              try_fold_fields(arguments_before, acc, fun),
              (acc) => { return try_fold_fields(arguments_after, acc, fun); },
            );
          },
        );
      } else if (expression instanceof $glance.BitString) {
        let segments = expression.segments;
        return $list.try_fold(
          segments,
          acc,
          (acc, _use1) => {
            let segment;
            let options;
            segment = _use1[0];
            options = _use1[1];
            return $result.try$(
              try_fold_expression(segment, acc, fun),
              (acc) => {
                return $list.try_fold(
                  options,
                  acc,
                  (acc, option) => {
                    if (option instanceof $glance.SizeValueOption) {
                      let expression$1 = option[0];
                      return try_fold_expression(expression$1, acc, fun);
                    } else {
                      return new Ok(acc);
                    }
                  },
                );
              },
            );
          },
        );
      } else if (expression instanceof $glance.Case) {
        let subjects = expression.subjects;
        let clauses = expression.clauses;
        return $result.try$(
          try_fold_expressions(subjects, acc, fun),
          (acc) => { return try_fold_clauses(clauses, acc, fun); },
        );
      } else if (expression instanceof $glance.BinaryOperator) {
        let left = expression.left;
        let right = expression.right;
        return $result.try$(
          try_fold_expression(left, acc, fun),
          (acc) => { return try_fold_expression(right, acc, fun); },
        );
      } else {
        let $ = expression.expression;
        if ($ instanceof Some) {
          let expression$1 = $[0];
          return try_fold_expression(expression$1, acc, fun);
        } else {
          return new Ok(acc);
        }
      }
    },
  );
}

function try_fold_clauses(clauses, acc, fun) {
  return $list.try_fold(
    clauses,
    acc,
    (acc, clause) => {
      let $ = clause.guard;
      if ($ instanceof Some) {
        let body = clause.body;
        let guard = $[0];
        return $result.try$(
          try_fold_expression(guard, acc, fun),
          (acc) => { return try_fold_expression(body, acc, fun); },
        );
      } else {
        let body = clause.body;
        return try_fold_expression(body, acc, fun);
      }
    },
  );
}

function try_fold_statements(statements, acc, fun) {
  return $list.try_fold(
    statements,
    acc,
    (acc, statement) => {
      if (statement instanceof $glance.Use) {
        let expression = statement.function;
        return try_fold_expression(expression, acc, fun);
      } else if (statement instanceof $glance.Assignment) {
        let expression = statement.value;
        return try_fold_expression(expression, acc, fun);
      } else if (statement instanceof $glance.Assert) {
        let $ = statement.message;
        if ($ instanceof Some) {
          let expression = statement.expression;
          let message = $[0];
          let $1 = try_fold_expression(expression, acc, fun);
          if ($1 instanceof Ok) {
            let acc$1 = $1[0];
            return try_fold_expression(message, acc$1, fun);
          } else {
            return $1;
          }
        } else {
          let expression = statement.expression;
          return try_fold_expression(expression, acc, fun);
        }
      } else {
        let expression = statement[0];
        return try_fold_expression(expression, acc, fun);
      }
    },
  );
}

export function from_module(titles, name, module) {
  return try_or(
    birdie_import(module),
    new Ok(titles),
    (birdie_import) => {
      return $list.try_fold(
        module.functions,
        titles,
        (titles, function$) => {
          let body = function$.definition.body;
          return try_fold_statements(
            body,
            titles,
            (titles, expression) => {
              let $ = snap_call(birdie_import, expression);
              if ($ instanceof Ok) {
                let $1 = $[0];
                if ($1 instanceof LiteralTitle) {
                  let title = $1[0];
                  let info = new TestInfo(name, function$.definition.name);
                  let $2 = find(titles, title);
                  if ($2 instanceof Ok) {
                    let $3 = $2[0];
                    if ($3 instanceof Literal) {
                      let other_info = $3.info;
                      return new Error(
                        new DuplicateLiteralTitles(title, info, other_info),
                      );
                    } else {
                      let prefix_info = $3.info;
                      let prefix = $3.prefix;
                      return new Error(
                        new PrefixOverlappingWithLiteralTitle(
                          prefix,
                          prefix_info,
                          info,
                        ),
                      );
                    }
                  } else {
                    return new Ok(add_literal_title(titles, title, info));
                  }
                } else {
                  return new Ok(titles);
                }
              } else {
                return new Ok(titles);
              }
            },
          );
        },
      );
    },
  );
}

export function from_test_directory() {
  return try$(
    $project.find_root(),
    (var0) => { return new CannotFindProjectRoot(var0); },
    (root) => {
      let test_directory = $filepath.join(root, "test");
      let get_files = $simplifile.get_files(test_directory);
      return try$(
        get_files,
        (var0) => { return new CannotReadTestDirectory(var0); },
        (files) => {
          return $list.try_fold(
            files,
            new$(),
            (titles, file) => {
              let is_gleam_file = isEqual(
                $filepath.extension(file),
                new Ok("gleam")
              );
              return $bool.guard(
                !is_gleam_file,
                new Ok(titles),
                () => {
                  return try$(
                    $simplifile.read(file),
                    (_capture) => {
                      return new CannotReadTestFile(_capture, file);
                    },
                    (raw_module) => {
                      let $ = $glance.module(raw_module);
                      if ($ instanceof Ok) {
                        let module = $[0];
                        return from_module(titles, file, module);
                      } else {
                        return new Ok(titles);
                      }
                    },
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

function try_fold_fields(fields, acc, fun) {
  return $list.try_fold(
    fields,
    acc,
    (acc, field) => {
      if (field instanceof $glance.LabelledField) {
        let item = field.item;
        return try_fold_expression(item, acc, fun);
      } else if (field instanceof $glance.ShorthandField) {
        return new Ok(acc);
      } else {
        let item = field.item;
        return try_fold_expression(item, acc, fun);
      }
    },
  );
}

function try_fold_expressions(expressions, acc, fun) {
  return $list.try_fold(
    expressions,
    acc,
    (acc, expression) => { return try_fold_expression(expression, acc, fun); },
  );
}
