-module(decode_input_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/decode_input_test.gleam").
-export([decode_input_valid_test/0, decode_input_missing_test/0, get_dynamic_present_test/0, get_optional_dynamic_absent_test/0]).

-file("test/decode_input_test.gleam", 8).
-spec decode_input_valid_test() -> nil.
decode_input_valid_test() ->
    Args = maps:from_list(
        [{<<"input"/utf8>>,
                mochi@types:record(
                    [{<<"name"/utf8>>, gleam_stdlib:identity(<<"Alice"/utf8>>)},
                        {<<"age"/utf8>>, gleam_stdlib:identity(30)}]
                )}]
    ),
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"name"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Name) ->
                gleam@dynamic@decode:field(
                    <<"age"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(Age) -> gleam@dynamic@decode:success({Name, Age}) end
                )
            end
        )
    end,
    case mochi@query:decode_input(Args, <<"input"/utf8>>, Decoder) of
        {ok, {<<"Alice"/utf8>>, 30}} ->
            nil;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Wrong decoded values"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"decode_input_test"/utf8>>,
                    function => <<"decode_input_valid_test"/utf8>>,
                    line => 28});

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should succeed: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"decode_input_test"/utf8>>,
                    function => <<"decode_input_valid_test"/utf8>>,
                    line => 29})
    end.

-file("test/decode_input_test.gleam", 33).
-spec decode_input_missing_test() -> nil.
decode_input_missing_test() ->
    Args = maps:new(),
    Decoder = {decoder, fun gleam@dynamic@decode:decode_string/1},
    case mochi@query:decode_input(Args, <<"input"/utf8>>, Decoder) of
        {error, Msg} ->
            case gleam_stdlib:contains_string(Msg, <<"input"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Error should mention field name: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"decode_input_test"/utf8>>,
                            function => <<"decode_input_missing_test"/utf8>>,
                            line => 40})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should fail on missing key"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"decode_input_test"/utf8>>,
                    function => <<"decode_input_missing_test"/utf8>>,
                    line => 42})
    end.

-file("test/decode_input_test.gleam", 46).
-spec get_dynamic_present_test() -> nil.
get_dynamic_present_test() ->
    Args = maps:from_list(
        [{<<"file"/utf8>>, gleam_stdlib:identity(<<"path/to/file"/utf8>>)}]
    ),
    case mochi@query:get_dynamic(Args, <<"file"/utf8>>) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should find dynamic value: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"decode_input_test"/utf8>>,
                    function => <<"get_dynamic_present_test"/utf8>>,
                    line => 50})
    end.

-file("test/decode_input_test.gleam", 54).
-spec get_optional_dynamic_absent_test() -> nil.
get_optional_dynamic_absent_test() ->
    Args = maps:new(),
    case mochi@query:get_optional_dynamic(Args, <<"file"/utf8>>) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return None for missing key"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"decode_input_test"/utf8>>,
                    function => <<"get_optional_dynamic_absent_test"/utf8>>,
                    line => 58})
    end.
