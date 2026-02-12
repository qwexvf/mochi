-module(birdie@internal@version).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/version.gleam").
-export([new/3, parse/1, compare/2]).
-export_type([version/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type version() :: {version, integer(), integer(), integer()}.

-file("src/birdie/internal/version.gleam", 10).
?DOC(false).
-spec new(integer(), integer(), integer()) -> version().
new(Major, Minor, Patch) ->
    {version, Major, Minor, Patch}.

-file("src/birdie/internal/version.gleam", 14).
?DOC(false).
-spec parse(binary()) -> {ok, version()} | {error, nil}.
parse(Version) ->
    case begin
        _pipe = Version,
        _pipe@1 = gleam@string:trim(_pipe),
        gleam@string:split(_pipe@1, <<"."/utf8>>)
    end of
        [Major, Minor, Patch] ->
            gleam@result:'try'(
                gleam_stdlib:parse_int(Major),
                fun(Major@1) ->
                    gleam@result:'try'(
                        gleam_stdlib:parse_int(Minor),
                        fun(Minor@1) ->
                            gleam@result:'try'(
                                gleam_stdlib:parse_int(Patch),
                                fun(Patch@1) ->
                                    {ok, {version, Major@1, Minor@1, Patch@1}}
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {error, nil}
    end.

-file("src/birdie/internal/version.gleam", 26).
?DOC(false).
-spec compare(version(), version()) -> gleam@order:order().
compare(One, Other) ->
    gleam@order:lazy_break_tie(
        gleam@int:compare(erlang:element(2, One), erlang:element(2, Other)),
        fun() ->
            gleam@order:lazy_break_tie(
                gleam@int:compare(
                    erlang:element(3, One),
                    erlang:element(3, Other)
                ),
                fun() ->
                    gleam@int:compare(
                        erlang:element(4, One),
                        erlang:element(4, Other)
                    )
                end
            )
        end
    ).
