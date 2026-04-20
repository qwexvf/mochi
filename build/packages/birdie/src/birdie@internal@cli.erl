-module(birdie@internal@cli).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/cli.gleam").
-export([parse/1, all_commands/0, similar_command/1, main_help_text/0, unknown_command_error/2, help_text/3, unknown_subcommand_error/3, unknown_option_error/3, missing_subcommand_error/2, unexpected_argument_error/3]).
-export_type([command/0, stale_subcommand/0, explained/0, error/0, arguments_kind/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type command() :: review |
    accept |
    reject |
    help |
    {stale, stale_subcommand()} |
    {with_help_option, command(), explained()}.

-type stale_subcommand() :: check_stale | delete_stale.

-type explained() :: full_command | top_level_command.

-type error() :: {unknown_command, binary()} |
    {unknown_subcommand, command(), binary()} |
    {unexpected_argument, command(), binary()} |
    {unknown_option, command(), binary()} |
    {missing_subcommand, command()}.

-type arguments_kind() :: command | subcommand.

-file("src/birdie/internal/cli.gleam", 109).
?DOC(false).
-spec is_help(binary()) -> boolean().
is_help(Option) ->
    case Option of
        <<"-h"/utf8>> ->
            true;

        <<"--help"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/birdie/internal/cli.gleam", 84).
?DOC(false).
-spec or_help(command(), list(binary())) -> {ok, command()} | {error, error()}.
or_help(Command, Options) ->
    case gleam@list:find(Options, fun(Option) -> not is_help(Option) end) of
        {ok, Option@1} ->
            {error, {unknown_option, Command, Option@1}};

        {error, _} ->
            case gleam@list:any(Options, fun is_help/1) of
                true ->
                    {ok, {with_help_option, Command, full_command}};

                false ->
                    {ok, Command}
            end
    end.

-file("src/birdie/internal/cli.gleam", 95).
?DOC(false).
-spec require_help(command(), list(binary())) -> {ok, command()} |
    {error, error()}.
require_help(Command, Options) ->
    case gleam@list:find(Options, fun(Option) -> not is_help(Option) end) of
        {ok, Option@1} ->
            {error, {unknown_option, Command, Option@1}};

        {error, _} ->
            case gleam@list:any(Options, fun is_help/1) of
                true ->
                    {ok, {with_help_option, Command, top_level_command}};

                false ->
                    {error, {missing_subcommand, Command}}
            end
    end.

-file("src/birdie/internal/cli.gleam", 44).
?DOC(false).
-spec parse(list(binary())) -> {ok, command()} | {error, error()}.
parse(Args) ->
    {Commands, Options} = gleam@list:partition(Args, fun(Arg) -> case Arg of
                <<"--"/utf8, _/binary>> ->
                    false;

                <<"-"/utf8, _/binary>> ->
                    false;

                _ ->
                    true
            end end),
    case Commands of
        [] ->
            _pipe = review,
            or_help(_pipe, Options);

        [<<"review"/utf8>>] ->
            _pipe@1 = review,
            or_help(_pipe@1, Options);

        [<<"review"/utf8>>, Subcommand | _] ->
            {error, {unknown_subcommand, review, Subcommand}};

        [<<"reject"/utf8>>] ->
            _pipe@2 = reject,
            or_help(_pipe@2, Options);

        [<<"reject"/utf8>>, Subcommand@1 | _] ->
            {error, {unknown_subcommand, reject, Subcommand@1}};

        [<<"accept"/utf8>>] ->
            _pipe@3 = accept,
            or_help(_pipe@3, Options);

        [<<"accept"/utf8>>, Subcommand@2 | _] ->
            {error, {unknown_subcommand, accept, Subcommand@2}};

        [<<"stale"/utf8>>] ->
            _pipe@4 = {stale, check_stale},
            require_help(_pipe@4, Options);

        [<<"stale"/utf8>>, <<"check"/utf8>>] ->
            _pipe@5 = {stale, check_stale},
            or_help(_pipe@5, Options);

        [<<"stale"/utf8>>, <<"check"/utf8>>, Argument | _] ->
            {error, {unexpected_argument, {stale, check_stale}, Argument}};

        [<<"stale"/utf8>>, <<"delete"/utf8>>] ->
            _pipe@6 = {stale, delete_stale},
            or_help(_pipe@6, Options);

        [<<"stale"/utf8>>, <<"delete"/utf8>>, Argument@1 | _] ->
            {error, {unexpected_argument, {stale, delete_stale}, Argument@1}};

        [<<"stale"/utf8>>, Subcommand@3 | _] ->
            {error, {unknown_subcommand, {stale, check_stale}, Subcommand@3}};

        [<<"help"/utf8>> | _] ->
            {ok, help};

        [Command | _] ->
            {error, {unknown_command, Command}}
    end.

-file("src/birdie/internal/cli.gleam", 131).
?DOC(false).
-spec all_commands() -> list(binary()).
all_commands() ->
    [<<"accept"/utf8>>,
        <<"help"/utf8>>,
        <<"reject"/utf8>>,
        <<"review"/utf8>>,
        <<"stale"/utf8>>].

-file("src/birdie/internal/cli.gleam", 119).
?DOC(false).
-spec similar_command(binary()) -> {ok, binary()} | {error, nil}.
similar_command(Command) ->
    _pipe = gleam@list:filter_map(
        all_commands(),
        fun(Valid_command) ->
            case edit_distance:levenshtein(Command, Valid_command) of
                Distance when Distance =< 3 ->
                    {ok, {Valid_command, Distance}};

                _ ->
                    {error, nil}
            end
        end
    ),
    _pipe@1 = gleam@list:sort(
        _pipe,
        fun(One, Other) ->
            gleam@int:compare(erlang:element(2, One), erlang:element(2, Other))
        end
    ),
    _pipe@2 = gleam@list:first(_pipe@1),
    gleam@result:map(_pipe@2, fun(Pair) -> erlang:element(1, Pair) end).

-file("src/birdie/internal/cli.gleam", 193).
?DOC(false).
-spec command_to_string(command()) -> binary().
command_to_string(Command) ->
    case Command of
        {with_help_option, Command@1, _} ->
            command_to_string(Command@1);

        {stale, _} ->
            <<"stale"/utf8>>;

        review ->
            <<"review"/utf8>>;

        accept ->
            <<"accept"/utf8>>;

        reject ->
            <<"reject"/utf8>>;

        help ->
            <<"help"/utf8>>
    end.

-file("src/birdie/internal/cli.gleam", 204).
?DOC(false).
-spec style_invalid_value(binary()) -> binary().
style_invalid_value(Value) ->
    gleam_community@ansi:yellow(<<<<"'"/utf8, Value/binary>>/binary, "'"/utf8>>).

-file("src/birdie/internal/cli.gleam", 305).
?DOC(false).
-spec command_menu() -> binary().
command_menu() ->
    <<<<<<<<<<<<<<<<<<<<(gleam_community@ansi:yellow(<<"Commands:\n"/utf8>>))/binary,
                                            (gleam_community@ansi:green(
                                                <<"  review  "/utf8>>
                                            ))/binary>>/binary,
                                        "review all new snapshots one by one\n"/utf8>>/binary,
                                    (gleam_community@ansi:green(
                                        <<"  accept  "/utf8>>
                                    ))/binary>>/binary,
                                "accept all new snapshots\n"/utf8>>/binary,
                            (gleam_community@ansi:green(<<"  reject  "/utf8>>))/binary>>/binary,
                        "reject all new snapshots\n"/utf8>>/binary,
                    (gleam_community@ansi:green(<<"  stale   "/utf8>>))/binary>>/binary,
                "find and remove stale snapshots\n"/utf8>>/binary,
            (gleam_community@ansi:green(<<"  help    "/utf8>>))/binary>>/binary,
        "print this help text"/utf8>>.

-file("src/birdie/internal/cli.gleam", 324).
?DOC(false).
-spec usage(list(binary()), gleam@option:option(arguments_kind())) -> binary().
usage(Commands, Arguments_kind) ->
    Command_placeholder = case Arguments_kind of
        none ->
            <<" "/utf8>>;

        {some, command} ->
            gleam_community@ansi:dim(<<" <COMMAND> "/utf8>>);

        {some, subcommand} ->
            gleam_community@ansi:dim(<<" <SUBCOMMAND> "/utf8>>)
    end,
    Commands@1 = case Commands of
        [] ->
            <<""/utf8>>;

        [_ | _] ->
            <<" "/utf8,
                (gleam_community@ansi:green(
                    gleam@string:join(Commands, <<" "/utf8>>)
                ))/binary>>
    end,
    <<<<<<<<<<(gleam_community@ansi:yellow(<<"Usage: "/utf8>>))/binary,
                        "gleam run -m"/utf8>>/binary,
                    (gleam_community@ansi:green(<<" birdie"/utf8>>))/binary>>/binary,
                Commands@1/binary>>/binary,
            Command_placeholder/binary>>/binary,
        (gleam_community@ansi:dim(<<"[OPTIONS]"/utf8>>))/binary>>.

-file("src/birdie/internal/cli.gleam", 346).
?DOC(false).
-spec options() -> binary().
options() ->
    Help_option = <<<<(gleam_community@ansi:green(<<"-h"/utf8>>))/binary,
            ", "/utf8>>/binary,
        (gleam_community@ansi:green(<<"--help"/utf8>>))/binary>>,
    <<<<<<(gleam_community@ansi:yellow(<<"Options:"/utf8>>))/binary,
                "\n  "/utf8>>/binary,
            Help_option/binary>>/binary,
        "  print this help text"/utf8>>.

-file("src/birdie/internal/cli.gleam", 235).
?DOC(false).
-spec stale_help_text(gleam@option:option(stale_subcommand())) -> binary().
stale_help_text(Subcommand) ->
    case Subcommand of
        none ->
            Stale_subcommands = <<<<<<<<(gleam_community@ansi:yellow(
                                <<"Subcommands:\n"/utf8>>
                            ))/binary,
                            (gleam_community@ansi:green(<<"  check   "/utf8>>))/binary>>/binary,
                        "check if there's any stale snapshot\n"/utf8>>/binary,
                    (gleam_community@ansi:green(<<"  delete  "/utf8>>))/binary>>/binary,
                "delete all stale snapshots"/utf8>>,
            <<<<<<<<<<<<(usage([<<"stale"/utf8>>], {some, subcommand}))/binary,
                                    "\n\n"/utf8>>/binary,
                                "Find and remove stale snapshots."/utf8>>/binary,
                            "\n\n"/utf8>>/binary,
                        Stale_subcommands/binary>>/binary,
                    "\n\n"/utf8>>/binary,
                (options())/binary>>;

        {some, check_stale} ->
            <<<<<<<<<<(usage([<<"stale"/utf8>>, <<"check"/utf8>>], none))/binary,
                                "\n\n"/utf8>>/binary,
                            "Check if there's any snapshot that is no longer used by any test.\n"/utf8>>/binary,
                        "This exits with an error status code if any stale snapshot is found."/utf8>>/binary,
                    "\n\n"/utf8>>/binary,
                (options())/binary>>;

        {some, delete_stale} ->
            <<<<<<<<(usage([<<"stale"/utf8>>, <<"delete"/utf8>>], none))/binary,
                            "\n\n"/utf8>>/binary,
                        "Removes any snapshot that is no longer used by any test."/utf8>>/binary,
                    "\n\n"/utf8>>/binary,
                (options())/binary>>
    end.

-file("src/birdie/internal/cli.gleam", 271).
?DOC(false).
-spec review_help_text() -> binary().
review_help_text() ->
    <<<<<<<<(usage([<<"review"/utf8>>], none))/binary, "\n\n"/utf8>>/binary,
                "Review all new snapshots one by one"/utf8>>/binary,
            "\n\n"/utf8>>/binary,
        (options())/binary>>.

-file("src/birdie/internal/cli.gleam", 279).
?DOC(false).
-spec reject_help_text() -> binary().
reject_help_text() ->
    <<<<<<<<(usage([<<"reject"/utf8>>], none))/binary, "\n\n"/utf8>>/binary,
                "Reject all new snapshots"/utf8>>/binary,
            "\n\n"/utf8>>/binary,
        (options())/binary>>.

-file("src/birdie/internal/cli.gleam", 287).
?DOC(false).
-spec accept_help_text() -> binary().
accept_help_text() ->
    <<<<<<<<(usage([<<"accept"/utf8>>], none))/binary, "\n\n"/utf8>>/binary,
                "Accept all new snapshots"/utf8>>/binary,
            "\n\n"/utf8>>/binary,
        (options())/binary>>.

-file("src/birdie/internal/cli.gleam", 301).
?DOC(false).
-spec main_help_text() -> binary().
main_help_text() ->
    <<<<<<<<(usage([], {some, command}))/binary, "\n\n"/utf8>>/binary,
                (command_menu())/binary>>/binary,
            "\n\n"/utf8>>/binary,
        (options())/binary>>.

-file("src/birdie/internal/cli.gleam", 137).
?DOC(false).
-spec unknown_command_error(binary(), boolean()) -> binary().
unknown_command_error(Command, Show_help_text) ->
    Message = <<<<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
            (style_invalid_value(Command))/binary>>/binary,
        " is not a valid command"/utf8>>,
    case Show_help_text of
        false ->
            Message;

        true ->
            <<<<Message/binary, "\n\n"/utf8>>/binary,
                (main_help_text())/binary>>
    end.

-file("src/birdie/internal/cli.gleam", 295).
?DOC(false).
-spec help_help_text(binary()) -> binary().
help_help_text(Birdie_version) ->
    <<<<((<<<<(gleam_community@ansi:green(<<"🐦‍⬛ birdie "/utf8>>))/binary,
                    "v"/utf8>>/binary,
                Birdie_version/binary>>))/binary,
            "\n\n"/utf8>>/binary,
        (main_help_text())/binary>>.

-file("src/birdie/internal/cli.gleam", 212).
?DOC(false).
-spec help_text(binary(), command(), explained()) -> binary().
help_text(Birdie_version, Command, Explained) ->
    case {Command, Explained} of
        {help, _} ->
            help_help_text(Birdie_version);

        {accept, _} ->
            accept_help_text();

        {reject, _} ->
            reject_help_text();

        {review, _} ->
            review_help_text();

        {{stale, Subcommand}, full_command} ->
            stale_help_text({some, Subcommand});

        {{stale, _}, top_level_command} ->
            stale_help_text(none);

        {{with_help_option, Command@1, Explained@1}, _} ->
            help_text(Birdie_version, Command@1, Explained@1)
    end.

-file("src/birdie/internal/cli.gleam", 149).
?DOC(false).
-spec unknown_subcommand_error(binary(), command(), binary()) -> binary().
unknown_subcommand_error(Birdie_version, Command, Subcommand) ->
    <<<<<<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
                (style_invalid_value(Subcommand))/binary>>/binary,
            " is not a valid subcommand\n\n"/utf8>>/binary,
        (help_text(Birdie_version, Command, top_level_command))/binary>>.

-file("src/birdie/internal/cli.gleam", 160).
?DOC(false).
-spec unknown_option_error(binary(), command(), binary()) -> binary().
unknown_option_error(Birdie_version, Command, Option) ->
    <<<<<<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
                (style_invalid_value(Option))/binary>>/binary,
            " is not a valid option\n\n"/utf8>>/binary,
        (help_text(Birdie_version, Command, full_command))/binary>>.

-file("src/birdie/internal/cli.gleam", 171).
?DOC(false).
-spec missing_subcommand_error(binary(), command()) -> binary().
missing_subcommand_error(Birdie_version, Command) ->
    <<<<<<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
                (style_invalid_value(command_to_string(Command)))/binary>>/binary,
            " is missing a required subcommand\n\n"/utf8>>/binary,
        (help_text(Birdie_version, Command, top_level_command))/binary>>.

-file("src/birdie/internal/cli.gleam", 181).
?DOC(false).
-spec unexpected_argument_error(binary(), command(), binary()) -> binary().
unexpected_argument_error(Birdie_version, Command, Argument) ->
    <<<<<<<<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
                    " unexpected argument "/utf8>>/binary,
                (style_invalid_value(Argument))/binary>>/binary,
            "\n\n"/utf8>>/binary,
        (help_text(Birdie_version, Command, full_command))/binary>>.
