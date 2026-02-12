import * as $edit_distance from "../../../edit_distance/edit_distance.mjs";
import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";

export class Review extends $CustomType {}
export const Command$Review = () => new Review();
export const Command$isReview = (value) => value instanceof Review;

export class Accept extends $CustomType {}
export const Command$Accept = () => new Accept();
export const Command$isAccept = (value) => value instanceof Accept;

export class Reject extends $CustomType {}
export const Command$Reject = () => new Reject();
export const Command$isReject = (value) => value instanceof Reject;

export class Help extends $CustomType {}
export const Command$Help = () => new Help();
export const Command$isHelp = (value) => value instanceof Help;

export class Stale extends $CustomType {
  constructor(subcommand) {
    super();
    this.subcommand = subcommand;
  }
}
export const Command$Stale = (subcommand) => new Stale(subcommand);
export const Command$isStale = (value) => value instanceof Stale;
export const Command$Stale$subcommand = (value) => value.subcommand;
export const Command$Stale$0 = (value) => value.subcommand;

export class WithHelpOption extends $CustomType {
  constructor(command, explained) {
    super();
    this.command = command;
    this.explained = explained;
  }
}
export const Command$WithHelpOption = (command, explained) =>
  new WithHelpOption(command, explained);
export const Command$isWithHelpOption = (value) =>
  value instanceof WithHelpOption;
export const Command$WithHelpOption$command = (value) => value.command;
export const Command$WithHelpOption$0 = (value) => value.command;
export const Command$WithHelpOption$explained = (value) => value.explained;
export const Command$WithHelpOption$1 = (value) => value.explained;

export class CheckStale extends $CustomType {}
export const StaleSubcommand$CheckStale = () => new CheckStale();
export const StaleSubcommand$isCheckStale = (value) =>
  value instanceof CheckStale;

export class DeleteStale extends $CustomType {}
export const StaleSubcommand$DeleteStale = () => new DeleteStale();
export const StaleSubcommand$isDeleteStale = (value) =>
  value instanceof DeleteStale;

export class FullCommand extends $CustomType {}
export const Explained$FullCommand = () => new FullCommand();
export const Explained$isFullCommand = (value) => value instanceof FullCommand;

export class TopLevelCommand extends $CustomType {}
export const Explained$TopLevelCommand = () => new TopLevelCommand();
export const Explained$isTopLevelCommand = (value) =>
  value instanceof TopLevelCommand;

export class UnknownCommand extends $CustomType {
  constructor(command) {
    super();
    this.command = command;
  }
}
export const Error$UnknownCommand = (command) => new UnknownCommand(command);
export const Error$isUnknownCommand = (value) =>
  value instanceof UnknownCommand;
export const Error$UnknownCommand$command = (value) => value.command;
export const Error$UnknownCommand$0 = (value) => value.command;

export class UnknownSubcommand extends $CustomType {
  constructor(command, subcommand) {
    super();
    this.command = command;
    this.subcommand = subcommand;
  }
}
export const Error$UnknownSubcommand = (command, subcommand) =>
  new UnknownSubcommand(command, subcommand);
export const Error$isUnknownSubcommand = (value) =>
  value instanceof UnknownSubcommand;
export const Error$UnknownSubcommand$command = (value) => value.command;
export const Error$UnknownSubcommand$0 = (value) => value.command;
export const Error$UnknownSubcommand$subcommand = (value) => value.subcommand;
export const Error$UnknownSubcommand$1 = (value) => value.subcommand;

export class UnexpectedArgument extends $CustomType {
  constructor(command, argument) {
    super();
    this.command = command;
    this.argument = argument;
  }
}
export const Error$UnexpectedArgument = (command, argument) =>
  new UnexpectedArgument(command, argument);
export const Error$isUnexpectedArgument = (value) =>
  value instanceof UnexpectedArgument;
export const Error$UnexpectedArgument$command = (value) => value.command;
export const Error$UnexpectedArgument$0 = (value) => value.command;
export const Error$UnexpectedArgument$argument = (value) => value.argument;
export const Error$UnexpectedArgument$1 = (value) => value.argument;

export class UnknownOption extends $CustomType {
  constructor(command, option) {
    super();
    this.command = command;
    this.option = option;
  }
}
export const Error$UnknownOption = (command, option) =>
  new UnknownOption(command, option);
export const Error$isUnknownOption = (value) => value instanceof UnknownOption;
export const Error$UnknownOption$command = (value) => value.command;
export const Error$UnknownOption$0 = (value) => value.command;
export const Error$UnknownOption$option = (value) => value.option;
export const Error$UnknownOption$1 = (value) => value.option;

export class MissingSubcommand extends $CustomType {
  constructor(command) {
    super();
    this.command = command;
  }
}
export const Error$MissingSubcommand = (command) =>
  new MissingSubcommand(command);
export const Error$isMissingSubcommand = (value) =>
  value instanceof MissingSubcommand;
export const Error$MissingSubcommand$command = (value) => value.command;
export const Error$MissingSubcommand$0 = (value) => value.command;

class Command extends $CustomType {}

class Subcommand extends $CustomType {}

function is_help(option) {
  if (option === "-h") {
    return true;
  } else if (option === "--help") {
    return true;
  } else {
    return false;
  }
}

function or_help(command, options) {
  let $ = $list.find(options, (option) => { return !is_help(option); });
  if ($ instanceof Ok) {
    let option = $[0];
    return new Error(new UnknownOption(command, option));
  } else {
    let $1 = $list.any(options, is_help);
    if ($1) {
      return new Ok(new WithHelpOption(command, new FullCommand()));
    } else {
      return new Ok(command);
    }
  }
}

function require_help(command, options) {
  let $ = $list.find(options, (option) => { return !is_help(option); });
  if ($ instanceof Ok) {
    let option = $[0];
    return new Error(new UnknownOption(command, option));
  } else {
    let $1 = $list.any(options, is_help);
    if ($1) {
      return new Ok(new WithHelpOption(command, new TopLevelCommand()));
    } else {
      return new Error(new MissingSubcommand(command));
    }
  }
}

export function parse(args) {
  let $ = $list.partition(
    args,
    (arg) => {
      if (arg.startsWith("--")) {
        return false;
      } else if (arg.startsWith("-")) {
        return false;
      } else {
        return true;
      }
    },
  );
  let commands;
  let options$1;
  commands = $[0];
  options$1 = $[1];
  if (commands instanceof $Empty) {
    let _pipe = new Review();
    return or_help(_pipe, options$1);
  } else {
    let $1 = commands.tail;
    if ($1 instanceof $Empty) {
      let $2 = commands.head;
      if ($2 === "review") {
        let _pipe = new Review();
        return or_help(_pipe, options$1);
      } else if ($2 === "reject") {
        let _pipe = new Reject();
        return or_help(_pipe, options$1);
      } else if ($2 === "accept") {
        let _pipe = new Accept();
        return or_help(_pipe, options$1);
      } else if ($2 === "stale") {
        let _pipe = new Stale(new CheckStale());
        return require_help(_pipe, options$1);
      } else if ($2 === "help") {
        return new Ok(new Help());
      } else {
        let command = $2;
        return new Error(new UnknownCommand(command));
      }
    } else {
      let $2 = commands.head;
      if ($2 === "review") {
        let subcommand = $1.head;
        return new Error(new UnknownSubcommand(new Review(), subcommand));
      } else if ($2 === "reject") {
        let subcommand = $1.head;
        return new Error(new UnknownSubcommand(new Reject(), subcommand));
      } else if ($2 === "accept") {
        let subcommand = $1.head;
        return new Error(new UnknownSubcommand(new Accept(), subcommand));
      } else if ($2 === "stale") {
        let $3 = $1.tail;
        if ($3 instanceof $Empty) {
          let $4 = $1.head;
          if ($4 === "check") {
            let _pipe = new Stale(new CheckStale());
            return or_help(_pipe, options$1);
          } else if ($4 === "delete") {
            let _pipe = new Stale(new DeleteStale());
            return or_help(_pipe, options$1);
          } else {
            let subcommand = $4;
            return new Error(
              new UnknownSubcommand(new Stale(new CheckStale()), subcommand),
            );
          }
        } else {
          let $4 = $1.head;
          if ($4 === "check") {
            let argument = $3.head;
            return new Error(
              new UnexpectedArgument(new Stale(new CheckStale()), argument),
            );
          } else if ($4 === "delete") {
            let argument = $3.head;
            return new Error(
              new UnexpectedArgument(new Stale(new DeleteStale()), argument),
            );
          } else {
            let subcommand = $4;
            return new Error(
              new UnknownSubcommand(new Stale(new CheckStale()), subcommand),
            );
          }
        }
      } else if ($2 === "help") {
        return new Ok(new Help());
      } else {
        let command = $2;
        return new Error(new UnknownCommand(command));
      }
    }
  }
}

export function all_commands() {
  return toList(["accept", "help", "reject", "review", "stale"]);
}

/**
 * This will return one of the allowed commands if there's one that's similar
 * enough to the given one.
 */
export function similar_command(command) {
  let _pipe = $list.filter_map(
    all_commands(),
    (valid_command) => {
      let $ = $edit_distance.levenshtein(command, valid_command);
      let distance = $;
      if (distance <= 3) {
        return new Ok([valid_command, distance]);
      } else {
        return new Error(undefined);
      }
    },
  );
  let _pipe$1 = $list.sort(
    _pipe,
    (one, other) => { return $int.compare(one[1], other[1]); },
  );
  let _pipe$2 = $list.first(_pipe$1);
  return $result.map(_pipe$2, (pair) => { return pair[0]; });
}

function command_to_string(loop$command) {
  while (true) {
    let command = loop$command;
    if (command instanceof Review) {
      return "review";
    } else if (command instanceof Accept) {
      return "accept";
    } else if (command instanceof Reject) {
      return "reject";
    } else if (command instanceof Help) {
      return "help";
    } else if (command instanceof Stale) {
      return "stale";
    } else {
      let command$1 = command.command;
      loop$command = command$1;
    }
  }
}

function style_invalid_value(value) {
  return $ansi.yellow(("'" + value) + "'");
}

function command_menu() {
  return ((((((((($ansi.yellow("Commands:\n") + $ansi.green("  review  ")) + "review all new snapshots one by one\n") + $ansi.green(
    "  accept  ",
  )) + "accept all new snapshots\n") + $ansi.green("  reject  ")) + "reject all new snapshots\n") + $ansi.green(
    "  stale   ",
  )) + "find and remove stale snapshots\n") + $ansi.green("  help    ")) + "print this help text";
}

function usage(commands, arguments_kind) {
  let _block;
  if (arguments_kind instanceof Some) {
    let $ = arguments_kind[0];
    if ($ instanceof Command) {
      _block = $ansi.dim(" <COMMAND> ");
    } else {
      _block = $ansi.dim(" <SUBCOMMAND> ");
    }
  } else {
    _block = " ";
  }
  let command_placeholder = _block;
  let _block$1;
  if (commands instanceof $Empty) {
    _block$1 = "";
  } else {
    _block$1 = " " + $ansi.green($string.join(commands, " "));
  }
  let commands$1 = _block$1;
  return (((($ansi.yellow("Usage: ") + "gleam run -m") + $ansi.green(" birdie")) + commands$1) + command_placeholder) + $ansi.dim(
    "[OPTIONS]",
  );
}

function options() {
  let help_option = ($ansi.green("-h") + ", ") + $ansi.green("--help");
  return (($ansi.yellow("Options:") + "\n  ") + help_option) + "  print this help text";
}

function stale_help_text(subcommand) {
  if (subcommand instanceof Some) {
    let $ = subcommand[0];
    if ($ instanceof CheckStale) {
      return ((((usage(toList(["stale", "check"]), new None()) + "\n\n") + "Check if there's any snapshot that is no longer used by any test.\n") + "This exits with an error status code if any stale snapshot is found.") + "\n\n") + options();
    } else {
      return (((usage(toList(["stale", "delete"]), new None()) + "\n\n") + "Removes any snapshot that is no longer used by any test.") + "\n\n") + options();
    }
  } else {
    let stale_subcommands = ((($ansi.yellow("Subcommands:\n") + $ansi.green(
      "  check   ",
    )) + "check if there's any stale snapshot\n") + $ansi.green("  delete  ")) + "delete all stale snapshots";
    return (((((usage(toList(["stale"]), new Some(new Subcommand())) + "\n\n") + "Find and remove stale snapshots.") + "\n\n") + stale_subcommands) + "\n\n") + options();
  }
}

function review_help_text() {
  return (((usage(toList(["review"]), new None()) + "\n\n") + "Review all new snapshots one by one") + "\n\n") + options();
}

function reject_help_text() {
  return (((usage(toList(["reject"]), new None()) + "\n\n") + "Reject all new snapshots") + "\n\n") + options();
}

function accept_help_text() {
  return (((usage(toList(["accept"]), new None()) + "\n\n") + "Accept all new snapshots") + "\n\n") + options();
}

export function main_help_text() {
  return (((usage(toList([]), new Some(new Command())) + "\n\n") + command_menu()) + "\n\n") + options();
}

export function unknown_command_error(command, show_help_text) {
  let message = ($ansi.red("Error: ") + style_invalid_value(command)) + " is not a valid command";
  if (show_help_text) {
    return (message + "\n\n") + main_help_text();
  } else {
    return message;
  }
}

function help_help_text(birdie_version) {
  return ((($ansi.green("🐦‍⬛ birdie ") + "v") + birdie_version) + "\n\n") + main_help_text();
}

/**
 * Returns the help text for the given command.
 */
export function help_text(loop$birdie_version, loop$command, loop$explained) {
  while (true) {
    let birdie_version = loop$birdie_version;
    let command = loop$command;
    let explained = loop$explained;
    if (command instanceof Review) {
      return review_help_text();
    } else if (command instanceof Accept) {
      return accept_help_text();
    } else if (command instanceof Reject) {
      return reject_help_text();
    } else if (command instanceof Help) {
      return help_help_text(birdie_version);
    } else if (command instanceof Stale) {
      if (explained instanceof FullCommand) {
        let subcommand = command.subcommand;
        return stale_help_text(new Some(subcommand));
      } else {
        return stale_help_text(new None());
      }
    } else {
      let command$1 = command.command;
      let explained$1 = command.explained;
      loop$birdie_version = birdie_version;
      loop$command = command$1;
      loop$explained = explained$1;
    }
  }
}

export function unknown_subcommand_error(birdie_version, command, subcommand) {
  return (($ansi.red("Error: ") + style_invalid_value(subcommand)) + " is not a valid subcommand\n\n") + help_text(
    birdie_version,
    command,
    new TopLevelCommand(),
  );
}

export function unknown_option_error(birdie_version, command, option) {
  return (($ansi.red("Error: ") + style_invalid_value(option)) + " is not a valid option\n\n") + help_text(
    birdie_version,
    command,
    new FullCommand(),
  );
}

export function missing_subcommand_error(birdie_version, command) {
  return (($ansi.red("Error: ") + style_invalid_value(
    command_to_string(command),
  )) + " is missing a required subcommand\n\n") + help_text(
    birdie_version,
    command,
    new TopLevelCommand(),
  );
}

export function unexpected_argument_error(birdie_version, command, argument) {
  return ((($ansi.red("Error: ") + " unexpected argument ") + style_invalid_value(
    argument,
  )) + "\n\n") + help_text(birdie_version, command, new FullCommand());
}
