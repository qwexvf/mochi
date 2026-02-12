-module(test_fast_parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/test_fast_parser.gleam").
-export([main/0]).

-file("src/test_fast_parser.gleam", 45).
-spec error_to_string(mochi_wisp@fast_parser:parse_error()) -> binary().
error_to_string(E) ->
    case E of
        {lex_error, Msg} ->
            <<"LexError: "/utf8, Msg/binary>>;

        {unexpected_token, Expected, Got} ->
            <<<<<<"UnexpectedToken: expected "/utf8, Expected/binary>>/binary,
                    ", got "/utf8>>/binary,
                Got/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"UnexpectedEOF: expected "/utf8, Expected@1/binary>>
    end.

-file("src/test_fast_parser.gleam", 7).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"Testing simple query..."/utf8>>),
    case mochi_wisp@fast_parser:parse(<<"{ users { id name } }"/utf8>>) of
        {ok, _} ->
            gleam_stdlib:println(<<"Simple query: OK"/utf8>>);

        {error, E} ->
            gleam_stdlib:println(
                <<"Simple query: FAILED - "/utf8, (error_to_string(E))/binary>>
            )
    end,
    gleam_stdlib:println(<<"\nTesting nested query..."/utf8>>),
    case mochi_wisp@fast_parser:parse(
        <<"{ users { id name posts { id title } } }"/utf8>>
    ) of
        {ok, _} ->
            gleam_stdlib:println(<<"Nested query: OK"/utf8>>);

        {error, E@1} ->
            gleam_stdlib:println(
                <<"Nested query: FAILED - "/utf8,
                    (error_to_string(E@1))/binary>>
            )
    end,
    gleam_stdlib:println(<<"\nTesting query with args..."/utf8>>),
    case mochi_wisp@fast_parser:parse(
        <<"{ user(id: \"1\") { id name } }"/utf8>>
    ) of
        {ok, _} ->
            gleam_stdlib:println(<<"Query with args: OK"/utf8>>);

        {error, E@2} ->
            gleam_stdlib:println(
                <<"Query with args: FAILED - "/utf8,
                    (error_to_string(E@2))/binary>>
            )
    end,
    gleam_stdlib:println(<<"\nTesting mutation..."/utf8>>),
    case mochi_wisp@fast_parser:parse(
        <<"mutation { createUser(name: \"Test\") { id } }"/utf8>>
    ) of
        {ok, _} ->
            gleam_stdlib:println(<<"Mutation: OK"/utf8>>);

        {error, E@3} ->
            gleam_stdlib:println(
                <<"Mutation: FAILED - "/utf8, (error_to_string(E@3))/binary>>
            )
    end.
