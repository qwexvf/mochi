-module(birdie@internal@project).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/project.gleam").
-export([find_root/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/birdie/internal/project.gleam", 14).
?DOC(false).
-spec do_find_root(binary()) -> {ok, binary()} |
    {error, simplifile:file_error()}.
do_find_root(Path) ->
    Manifest = filepath:join(Path, <<"gleam.toml"/utf8>>),
    case simplifile_erl:is_file(Manifest) of
        {ok, true} ->
            {ok, Path};

        {ok, false} ->
            do_find_root(filepath:join(Path, <<".."/utf8>>));

        {error, Reason} ->
            {error, Reason}
    end.

-file("src/birdie/internal/project.gleam", 10).
?DOC(false).
-spec find_root() -> {ok, binary()} | {error, simplifile:file_error()}.
find_root() ->
    do_find_root(<<"."/utf8>>).
