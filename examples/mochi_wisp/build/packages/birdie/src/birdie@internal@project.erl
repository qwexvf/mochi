-module(birdie@internal@project).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/project.gleam").
-export([find_root/0, name/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/birdie/internal/project.gleam", 16).
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

-file("src/birdie/internal/project.gleam", 12).
?DOC(false).
-spec find_root() -> {ok, binary()} | {error, simplifile:file_error()}.
find_root() ->
    do_find_root(<<"."/utf8>>).

-file("src/birdie/internal/project.gleam", 27).
?DOC(false).
-spec name() -> {ok, binary()} | {error, simplifile:file_error()}.
name() ->
    gleam@result:'try'(
        find_root(),
        fun(Root) ->
            gleam@result:'try'(
                simplifile:read(filepath:join(Root, <<"gleam.toml"/utf8>>)),
                fun(File) ->
                    Toml@1 = case tom:parse(File) of
                        {ok, Toml} -> Toml;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"running birdie in a gleam project with an invalid `gleam.toml` should be impossible"/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"birdie/internal/project"/utf8>>,
                                        function => <<"name"/utf8>>,
                                        line => 30,
                                        value => _assert_fail,
                                        start => 915,
                                        'end' => 952,
                                        pattern_start => 926,
                                        pattern_end => 934})
                    end,
                    Name@1 = case tom:get_string(Toml@1, [<<"name"/utf8>>]) of
                        {ok, Name} -> Name;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"`name` is a required field in `gleam.toml`, it should be impossible to run birdie on a project that doesn't have one"/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"birdie/internal/project"/utf8>>,
                                        function => <<"name"/utf8>>,
                                        line => 32,
                                        value => _assert_fail@1,
                                        start => 1048,
                                        'end' => 1100,
                                        pattern_start => 1059,
                                        pattern_end => 1067})
                    end,
                    {ok, Name@1}
                end
            )
        end
    ).
