-module(mochi@lexer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/lexer.gleam").
-export([new_lexer/1, next_token/1, tokenize/1]).
-export_type([token/0, position/0, token_with_position/0, lexer_error/0, lexer_state/0]).

-type token() :: bang |
    dollar |
    amp |
    left_paren |
    right_paren |
    spread |
    colon |
    equals |
    at |
    left_bracket |
    right_bracket |
    left_brace |
    right_brace |
    pipe |
    'query' |
    mutation |
    subscription |
    fragment |
    on |
    true_keyword |
    false_keyword |
    null_keyword |
    {name, binary()} |
    {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    e_o_f.

-type position() :: {position, integer(), integer()}.

-type token_with_position() :: {token_with_position, token(), position()}.

-type lexer_error() :: {unexpected_character, binary(), position()} |
    {invalid_number, binary(), position()} |
    {unterminated_string, position()}.

-type lexer_state() :: {lexer_state, binary(), integer(), integer(), integer()}.

-file("src/mochi/lexer.gleam", 61).
-spec new_lexer(binary()) -> lexer_state().
new_lexer(Input) ->
    {lexer_state, Input, 0, 1, 1}.

-file("src/mochi/lexer.gleam", 196).
-spec peek_char(lexer_state()) -> {ok, binary()} | {error, nil}.
peek_char(Lexer) ->
    case gleam@string:slice(
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        1
    ) of
        <<""/utf8>> ->
            {error, nil};

        Char ->
            {ok, Char}
    end.

-file("src/mochi/lexer.gleam", 203).
-spec advance_char(lexer_state()) -> lexer_state().
advance_char(Lexer) ->
    case peek_char(Lexer) of
        {ok, <<"\n"/utf8>>} ->
            {lexer_state,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer) + 1,
                erlang:element(4, Lexer) + 1,
                1};

        {ok, _} ->
            {lexer_state,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer) + 1,
                erlang:element(4, Lexer),
                erlang:element(5, Lexer) + 1};

        {error, _} ->
            Lexer
    end.

-file("src/mochi/lexer.gleam", 189).
-spec skip_comment(lexer_state()) -> lexer_state().
skip_comment(Lexer) ->
    case peek_char(Lexer) of
        {ok, <<"\n"/utf8>>} ->
            Lexer;

        {error, _} ->
            Lexer;

        {ok, _} ->
            skip_comment(advance_char(Lexer))
    end.

-file("src/mochi/lexer.gleam", 180).
-spec skip_whitespace(lexer_state()) -> lexer_state().
skip_whitespace(Lexer) ->
    case peek_char(Lexer) of
        {ok, <<" "/utf8>>} ->
            skip_whitespace(advance_char(Lexer));

        {ok, <<"\t"/utf8>>} ->
            skip_whitespace(advance_char(Lexer));

        {ok, <<"\n"/utf8>>} ->
            skip_whitespace(advance_char(Lexer));

        {ok, <<"\r"/utf8>>} ->
            skip_whitespace(advance_char(Lexer));

        {ok, <<","/utf8>>} ->
            skip_whitespace(advance_char(Lexer));

        {ok, <<"#"/utf8>>} ->
            skip_whitespace(skip_comment(advance_char(Lexer)));

        _ ->
            Lexer
    end.

-file("src/mochi/lexer.gleam", 222).
-spec read_spread(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_spread(Lexer, Position) ->
    Lexer1 = advance_char(Lexer),
    Lexer2 = advance_char(Lexer1),
    Lexer3 = advance_char(Lexer2),
    case gleam@string:slice(
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        3
    ) of
        <<"..."/utf8>> ->
            {ok, {{token_with_position, spread, Position}, Lexer3}};

        _ ->
            {error, {unexpected_character, <<"."/utf8>>, Position}}
    end.

-file("src/mochi/lexer.gleam", 316).
-spec read_hex_digit(lexer_state(), position()) -> {ok,
        {integer(), lexer_state()}} |
    {error, lexer_error()}.
read_hex_digit(Lexer, Position) ->
    case peek_char(Lexer) of
        {ok, <<"0"/utf8>>} ->
            {ok, {0, advance_char(Lexer)}};

        {ok, <<"1"/utf8>>} ->
            {ok, {1, advance_char(Lexer)}};

        {ok, <<"2"/utf8>>} ->
            {ok, {2, advance_char(Lexer)}};

        {ok, <<"3"/utf8>>} ->
            {ok, {3, advance_char(Lexer)}};

        {ok, <<"4"/utf8>>} ->
            {ok, {4, advance_char(Lexer)}};

        {ok, <<"5"/utf8>>} ->
            {ok, {5, advance_char(Lexer)}};

        {ok, <<"6"/utf8>>} ->
            {ok, {6, advance_char(Lexer)}};

        {ok, <<"7"/utf8>>} ->
            {ok, {7, advance_char(Lexer)}};

        {ok, <<"8"/utf8>>} ->
            {ok, {8, advance_char(Lexer)}};

        {ok, <<"9"/utf8>>} ->
            {ok, {9, advance_char(Lexer)}};

        {ok, <<"a"/utf8>>} ->
            {ok, {10, advance_char(Lexer)}};

        {ok, <<"A"/utf8>>} ->
            {ok, {10, advance_char(Lexer)}};

        {ok, <<"b"/utf8>>} ->
            {ok, {11, advance_char(Lexer)}};

        {ok, <<"B"/utf8>>} ->
            {ok, {11, advance_char(Lexer)}};

        {ok, <<"c"/utf8>>} ->
            {ok, {12, advance_char(Lexer)}};

        {ok, <<"C"/utf8>>} ->
            {ok, {12, advance_char(Lexer)}};

        {ok, <<"d"/utf8>>} ->
            {ok, {13, advance_char(Lexer)}};

        {ok, <<"D"/utf8>>} ->
            {ok, {13, advance_char(Lexer)}};

        {ok, <<"e"/utf8>>} ->
            {ok, {14, advance_char(Lexer)}};

        {ok, <<"E"/utf8>>} ->
            {ok, {14, advance_char(Lexer)}};

        {ok, <<"f"/utf8>>} ->
            {ok, {15, advance_char(Lexer)}};

        {ok, <<"F"/utf8>>} ->
            {ok, {15, advance_char(Lexer)}};

        {ok, C} ->
            {error, {unexpected_character, C, Position}};

        {error, _} ->
            {error, {unterminated_string, Position}}
    end.

-file("src/mochi/lexer.gleam", 421).
-spec count_leading_whitespace_loop(binary(), integer()) -> integer().
count_leading_whitespace_loop(S, Count) ->
    case gleam@string:slice(S, Count, 1) of
        <<" "/utf8>> ->
            count_leading_whitespace_loop(S, Count + 1);

        <<"\t"/utf8>> ->
            count_leading_whitespace_loop(S, Count + 1);

        _ ->
            Count
    end.

-file("src/mochi/lexer.gleam", 417).
-spec count_leading_whitespace(binary()) -> integer().
count_leading_whitespace(S) ->
    count_leading_whitespace_loop(S, 0).

-file("src/mochi/lexer.gleam", 391).
-spec find_common_indent(list(binary())) -> integer().
find_common_indent(Lines) ->
    _pipe = gleam@list:fold(
        Lines,
        -1,
        fun(Acc, Line) -> case gleam@list:is_empty(Lines) of
                true ->
                    Acc;

                false ->
                    Indent = count_leading_whitespace(Line),
                    Is_blank = Indent =:= string:length(Line),
                    case Is_blank of
                        true ->
                            Acc;

                        false ->
                            case (Acc =:= -1) orelse (Indent < Acc) of
                                true ->
                                    Indent;

                                false ->
                                    Acc
                            end
                    end
            end end
    ),
    (fun(N) -> case N =:= -1 of
            true ->
                0;

            false ->
                N
        end end)(_pipe).

-file("src/mochi/lexer.gleam", 428).
-spec drop_leading_blank(list(binary())) -> list(binary()).
drop_leading_blank(Lines) ->
    case Lines of
        [] ->
            [];

        [First | Rest] ->
            case gleam@string:trim(First) =:= <<""/utf8>> of
                true ->
                    drop_leading_blank(Rest);

                false ->
                    Lines
            end
    end.

-file("src/mochi/lexer.gleam", 439).
-spec drop_trailing_blank(list(binary())) -> list(binary()).
drop_trailing_blank(Lines) ->
    drop_leading_blank(Lines).

-file("src/mochi/lexer.gleam", 371).
-spec block_string_value(binary()) -> binary().
block_string_value(Raw) ->
    Lines = gleam@string:split(Raw, <<"\n"/utf8>>),
    Lines@1 = gleam@list:map(
        Lines,
        fun(L) -> gleam@string:replace(L, <<"\r"/utf8>>, <<""/utf8>>) end
    ),
    Common_indent = find_common_indent(Lines@1),
    Lines@2 = gleam@list:index_map(Lines@1, fun(Line, I) -> case I =:= 0 of
                true ->
                    Line;

                false ->
                    gleam@string:drop_start(Line, Common_indent)
            end end),
    Lines@3 = drop_leading_blank(Lines@2),
    Lines@4 = begin
        _pipe = drop_trailing_blank(lists:reverse(Lines@3)),
        lists:reverse(_pipe)
    end,
    gleam@string:join(Lines@4, <<"\n"/utf8>>).

-file("src/mochi/lexer.gleam", 342).
-spec read_block_string(lexer_state(), position(), binary()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_block_string(Lexer, Position, Acc) ->
    case gleam@string:slice(
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        4
    ) of
        <<"\\\"\"\""/utf8>> ->
            Lexer@1 = advance_char(
                advance_char(advance_char(advance_char(Lexer)))
            ),
            read_block_string(Lexer@1, Position, <<Acc/binary, "\"\"\""/utf8>>);

        _ ->
            case gleam@string:slice(
                erlang:element(2, Lexer),
                erlang:element(3, Lexer),
                3
            ) of
                <<"\"\"\""/utf8>> ->
                    Lexer@2 = advance_char(advance_char(advance_char(Lexer))),
                    {ok,
                        {{token_with_position,
                                {string_value, block_string_value(Acc)},
                                Position},
                            Lexer@2}};

                _ ->
                    case peek_char(Lexer) of
                        {error, _} ->
                            {error, {unterminated_string, Position}};

                        {ok, Char} ->
                            read_block_string(
                                advance_char(Lexer),
                                Position,
                                <<Acc/binary, Char/binary>>
                            )
                    end
            end
    end.

-file("src/mochi/lexer.gleam", 568).
-spec read_while_loop(lexer_state(), fun((binary()) -> boolean()), binary()) -> {binary(),
    lexer_state()}.
read_while_loop(Lexer, Predicate, Acc) ->
    case peek_char(Lexer) of
        {ok, Char} ->
            case Predicate(Char) of
                true ->
                    read_while_loop(
                        advance_char(Lexer),
                        Predicate,
                        <<Acc/binary, Char/binary>>
                    );

                false ->
                    {Acc, Lexer}
            end;

        _ ->
            {Acc, Lexer}
    end.

-file("src/mochi/lexer.gleam", 561).
-spec read_while(lexer_state(), fun((binary()) -> boolean())) -> {binary(),
    lexer_state()}.
read_while(Lexer, Predicate) ->
    read_while_loop(Lexer, Predicate, <<""/utf8>>).

-file("src/mochi/lexer.gleam", 583).
-spec is_digit(binary()) -> boolean().
is_digit(Char) ->
    case Char of
        <<"0"/utf8>> ->
            true;

        <<"1"/utf8>> ->
            true;

        <<"2"/utf8>> ->
            true;

        <<"3"/utf8>> ->
            true;

        <<"4"/utf8>> ->
            true;

        <<"5"/utf8>> ->
            true;

        <<"6"/utf8>> ->
            true;

        <<"7"/utf8>> ->
            true;

        <<"8"/utf8>> ->
            true;

        <<"9"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/mochi/lexer.gleam", 470).
-spec read_number_with_prefix(lexer_state(), position(), binary()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_number_with_prefix(Lexer, Position, Prefix) ->
    {Int_part, Lexer@1} = read_while(Lexer, fun is_digit/1),
    {Frac_part, Lexer@4} = case peek_char(Lexer@1) of
        {ok, <<"."/utf8>>} ->
            Lexer@2 = advance_char(Lexer@1),
            {Digits, Lexer@3} = read_while(Lexer@2, fun is_digit/1),
            {<<"."/utf8, Digits/binary>>, Lexer@3};

        _ ->
            {<<""/utf8>>, Lexer@1}
    end,
    {Exp_part, Lexer@8} = case peek_char(Lexer@4) of
        {ok, <<"e"/utf8>>} ->
            E_char = case peek_char(Lexer@4) of
                {ok, C} ->
                    C;

                _ ->
                    <<"e"/utf8>>
            end,
            Lexer@5 = advance_char(Lexer@4),
            {Sign, Lexer@6} = case peek_char(Lexer@5) of
                {ok, <<"+"/utf8>>} ->
                    S = case peek_char(Lexer@5) of
                        {ok, C@1} ->
                            C@1;

                        _ ->
                            <<""/utf8>>
                    end,
                    {S, advance_char(Lexer@5)};

                {ok, <<"-"/utf8>>} ->
                    S = case peek_char(Lexer@5) of
                        {ok, C@1} ->
                            C@1;

                        _ ->
                            <<""/utf8>>
                    end,
                    {S, advance_char(Lexer@5)};

                _ ->
                    {<<""/utf8>>, Lexer@5}
            end,
            {Exp_digits, Lexer@7} = read_while(Lexer@6, fun is_digit/1),
            {<<<<E_char/binary, Sign/binary>>/binary, Exp_digits/binary>>,
                Lexer@7};

        {ok, <<"E"/utf8>>} ->
            E_char = case peek_char(Lexer@4) of
                {ok, C} ->
                    C;

                _ ->
                    <<"e"/utf8>>
            end,
            Lexer@5 = advance_char(Lexer@4),
            {Sign, Lexer@6} = case peek_char(Lexer@5) of
                {ok, <<"+"/utf8>>} ->
                    S = case peek_char(Lexer@5) of
                        {ok, C@1} ->
                            C@1;

                        _ ->
                            <<""/utf8>>
                    end,
                    {S, advance_char(Lexer@5)};

                {ok, <<"-"/utf8>>} ->
                    S = case peek_char(Lexer@5) of
                        {ok, C@1} ->
                            C@1;

                        _ ->
                            <<""/utf8>>
                    end,
                    {S, advance_char(Lexer@5)};

                _ ->
                    {<<""/utf8>>, Lexer@5}
            end,
            {Exp_digits, Lexer@7} = read_while(Lexer@6, fun is_digit/1),
            {<<<<E_char/binary, Sign/binary>>/binary, Exp_digits/binary>>,
                Lexer@7};

        _ ->
            {<<""/utf8>>, Lexer@4}
    end,
    Number_str = <<<<<<Prefix/binary, Int_part/binary>>/binary,
            Frac_part/binary>>/binary,
        Exp_part/binary>>,
    Is_float = (Frac_part /= <<""/utf8>>) orelse (Exp_part /= <<""/utf8>>),
    case Is_float of
        true ->
            Parse_str = case gleam_stdlib:contains_string(
                Number_str,
                <<"."/utf8>>
            ) of
                true ->
                    Number_str;

                false ->
                    case gleam@string:split_once(Number_str, <<"e"/utf8>>) of
                        {ok, {Base, Exp}} ->
                            <<<<Base/binary, ".0e"/utf8>>/binary, Exp/binary>>;

                        {error, _} ->
                            case gleam@string:split_once(
                                Number_str,
                                <<"E"/utf8>>
                            ) of
                                {ok, {Base@1, Exp@1}} ->
                                    <<<<Base@1/binary, ".0E"/utf8>>/binary,
                                        Exp@1/binary>>;

                                {error, _} ->
                                    Number_str
                            end
                    end
            end,
            case gleam_stdlib:parse_float(Parse_str) of
                {ok, Value} ->
                    {ok,
                        {{token_with_position, {float_value, Value}, Position},
                            Lexer@8}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end;

        false ->
            case gleam_stdlib:parse_int(Number_str) of
                {ok, Value@1} ->
                    {ok,
                        {{token_with_position, {int_value, Value@1}, Position},
                            Lexer@8}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end
    end.

-file("src/mochi/lexer.gleam", 443).
-spec read_negative_number(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_negative_number(Lexer, Position) ->
    Lexer@1 = advance_char(Lexer),
    case peek_char(Lexer@1) of
        {ok, <<"0"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"1"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"2"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"3"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"4"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"5"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"6"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"7"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"8"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        {ok, <<"9"/utf8>>} ->
            read_number_with_prefix(Lexer@1, Position, <<"-"/utf8>>);

        _ ->
            {error, {unexpected_character, <<"-"/utf8>>, Position}}
    end.

-file("src/mochi/lexer.gleam", 463).
-spec read_number(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_number(Lexer, Position) ->
    read_number_with_prefix(Lexer, Position, <<""/utf8>>).

-file("src/mochi/lexer.gleam", 594).
-spec is_letter(binary()) -> boolean().
is_letter(Char) ->
    case Char of
        <<"a"/utf8>> ->
            true;

        <<"b"/utf8>> ->
            true;

        <<"c"/utf8>> ->
            true;

        <<"d"/utf8>> ->
            true;

        <<"e"/utf8>> ->
            true;

        <<"f"/utf8>> ->
            true;

        <<"g"/utf8>> ->
            true;

        <<"h"/utf8>> ->
            true;

        <<"i"/utf8>> ->
            true;

        <<"j"/utf8>> ->
            true;

        <<"k"/utf8>> ->
            true;

        <<"l"/utf8>> ->
            true;

        <<"m"/utf8>> ->
            true;

        <<"n"/utf8>> ->
            true;

        <<"o"/utf8>> ->
            true;

        <<"p"/utf8>> ->
            true;

        <<"q"/utf8>> ->
            true;

        <<"r"/utf8>> ->
            true;

        <<"s"/utf8>> ->
            true;

        <<"t"/utf8>> ->
            true;

        <<"u"/utf8>> ->
            true;

        <<"v"/utf8>> ->
            true;

        <<"w"/utf8>> ->
            true;

        <<"x"/utf8>> ->
            true;

        <<"y"/utf8>> ->
            true;

        <<"z"/utf8>> ->
            true;

        <<"A"/utf8>> ->
            true;

        <<"B"/utf8>> ->
            true;

        <<"C"/utf8>> ->
            true;

        <<"D"/utf8>> ->
            true;

        <<"E"/utf8>> ->
            true;

        <<"F"/utf8>> ->
            true;

        <<"G"/utf8>> ->
            true;

        <<"H"/utf8>> ->
            true;

        <<"I"/utf8>> ->
            true;

        <<"J"/utf8>> ->
            true;

        <<"K"/utf8>> ->
            true;

        <<"L"/utf8>> ->
            true;

        <<"M"/utf8>> ->
            true;

        <<"N"/utf8>> ->
            true;

        <<"O"/utf8>> ->
            true;

        <<"P"/utf8>> ->
            true;

        <<"Q"/utf8>> ->
            true;

        <<"R"/utf8>> ->
            true;

        <<"S"/utf8>> ->
            true;

        <<"T"/utf8>> ->
            true;

        <<"U"/utf8>> ->
            true;

        <<"V"/utf8>> ->
            true;

        <<"W"/utf8>> ->
            true;

        <<"X"/utf8>> ->
            true;

        <<"Y"/utf8>> ->
            true;

        <<"Z"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/mochi/lexer.gleam", 590).
-spec is_name_continue(binary()) -> boolean().
is_name_continue(Char) ->
    (is_letter(Char) orelse is_digit(Char)) orelse (Char =:= <<"_"/utf8>>).

-file("src/mochi/lexer.gleam", 540).
-spec read_name(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_name(Lexer, Position) ->
    {Name, New_lexer} = read_while(Lexer, fun is_name_continue/1),
    Token = case Name of
        <<"query"/utf8>> ->
            'query';

        <<"mutation"/utf8>> ->
            mutation;

        <<"subscription"/utf8>> ->
            subscription;

        <<"fragment"/utf8>> ->
            fragment;

        <<"on"/utf8>> ->
            on;

        <<"true"/utf8>> ->
            true_keyword;

        <<"false"/utf8>> ->
            false_keyword;

        <<"null"/utf8>> ->
            null_keyword;

        _ ->
            {name, Name}
    end,
    {ok, {{token_with_position, Token, Position}, New_lexer}}.

-file("src/mochi/lexer.gleam", 282).
-spec read_unicode_escape(lexer_state(), position(), binary()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_unicode_escape(Lexer, Position, Acc) ->
    case read_hex_digit(Lexer, Position) of
        {error, E} ->
            {error, E};

        {ok, {H1, Lexer@1}} ->
            case read_hex_digit(Lexer@1, Position) of
                {error, E@1} ->
                    {error, E@1};

                {ok, {H2, Lexer@2}} ->
                    case read_hex_digit(Lexer@2, Position) of
                        {error, E@2} ->
                            {error, E@2};

                        {ok, {H3, Lexer@3}} ->
                            case read_hex_digit(Lexer@3, Position) of
                                {error, E@3} ->
                                    {error, E@3};

                                {ok, {H4, Lexer@4}} ->
                                    Codepoint = (((H1 * 4096) + (H2 * 256)) + (H3
                                    * 16))
                                    + H4,
                                    case gleam@string:utf_codepoint(Codepoint) of
                                        {ok, Cp} ->
                                            read_string_loop(
                                                Lexer@4,
                                                Position,
                                                <<Acc/binary,
                                                    (gleam_stdlib:utf_codepoint_list_to_string(
                                                        [Cp]
                                                    ))/binary>>
                                            );

                                        {error, _} ->
                                            {error,
                                                {invalid_number,
                                                    <<"\\uXXXX"/utf8>>,
                                                    Position}}
                                    end
                            end
                    end
            end
    end.

-file("src/mochi/lexer.gleam", 251).
-spec read_string_loop(lexer_state(), position(), binary()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_string_loop(Lexer, Position, Acc) ->
    case peek_char(Lexer) of
        {error, _} ->
            {error, {unterminated_string, Position}};

        {ok, <<"\""/utf8>>} ->
            {ok,
                {{token_with_position, {string_value, Acc}, Position},
                    advance_char(Lexer)}};

        {ok, <<"\\"/utf8>>} ->
            Lexer@1 = advance_char(Lexer),
            case peek_char(Lexer@1) of
                {ok, <<"\""/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\""/utf8>>
                    );

                {ok, <<"\\"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\\"/utf8>>
                    );

                {ok, <<"/"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "/"/utf8>>
                    );

                {ok, <<"b"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\x{0008}"/utf8>>
                    );

                {ok, <<"f"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\x{000C}"/utf8>>
                    );

                {ok, <<"n"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\n"/utf8>>
                    );

                {ok, <<"r"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\r"/utf8>>
                    );

                {ok, <<"t"/utf8>>} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, "\t"/utf8>>
                    );

                {ok, <<"u"/utf8>>} ->
                    read_unicode_escape(advance_char(Lexer@1), Position, Acc);

                {ok, Char} ->
                    read_string_loop(
                        advance_char(Lexer@1),
                        Position,
                        <<Acc/binary, Char/binary>>
                    );

                {error, _} ->
                    {error, {unterminated_string, Position}}
            end;

        {ok, Char@1} ->
            read_string_loop(
                advance_char(Lexer),
                Position,
                <<Acc/binary, Char@1/binary>>
            )
    end.

-file("src/mochi/lexer.gleam", 236).
-spec read_string(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_string(Lexer, Position) ->
    case gleam@string:slice(
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        3
    ) of
        <<"\"\"\""/utf8>> ->
            read_block_string(
                advance_char(advance_char(advance_char(Lexer))),
                Position,
                <<""/utf8>>
            );

        _ ->
            read_string_loop(advance_char(Lexer), Position, <<""/utf8>>)
    end.

-file("src/mochi/lexer.gleam", 86).
-spec next_token(lexer_state()) -> {ok, {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
next_token(Lexer) ->
    Lexer@1 = skip_whitespace(Lexer),
    case peek_char(Lexer@1) of
        {error, _} ->
            {ok,
                {{token_with_position,
                        e_o_f,
                        {position,
                            erlang:element(4, Lexer@1),
                            erlang:element(5, Lexer@1)}},
                    Lexer@1}};

        {ok, Char} ->
            Position = {position,
                erlang:element(4, Lexer@1),
                erlang:element(5, Lexer@1)},
            case Char of
                <<"!"/utf8>> ->
                    {ok,
                        {{token_with_position, bang, Position},
                            advance_char(Lexer@1)}};

                <<"$"/utf8>> ->
                    {ok,
                        {{token_with_position, dollar, Position},
                            advance_char(Lexer@1)}};

                <<"&"/utf8>> ->
                    {ok,
                        {{token_with_position, amp, Position},
                            advance_char(Lexer@1)}};

                <<"("/utf8>> ->
                    {ok,
                        {{token_with_position, left_paren, Position},
                            advance_char(Lexer@1)}};

                <<")"/utf8>> ->
                    {ok,
                        {{token_with_position, right_paren, Position},
                            advance_char(Lexer@1)}};

                <<":"/utf8>> ->
                    {ok,
                        {{token_with_position, colon, Position},
                            advance_char(Lexer@1)}};

                <<"="/utf8>> ->
                    {ok,
                        {{token_with_position, equals, Position},
                            advance_char(Lexer@1)}};

                <<"@"/utf8>> ->
                    {ok,
                        {{token_with_position, at, Position},
                            advance_char(Lexer@1)}};

                <<"["/utf8>> ->
                    {ok,
                        {{token_with_position, left_bracket, Position},
                            advance_char(Lexer@1)}};

                <<"]"/utf8>> ->
                    {ok,
                        {{token_with_position, right_bracket, Position},
                            advance_char(Lexer@1)}};

                <<"{"/utf8>> ->
                    {ok,
                        {{token_with_position, left_brace, Position},
                            advance_char(Lexer@1)}};

                <<"}"/utf8>> ->
                    {ok,
                        {{token_with_position, right_brace, Position},
                            advance_char(Lexer@1)}};

                <<"|"/utf8>> ->
                    {ok,
                        {{token_with_position, pipe, Position},
                            advance_char(Lexer@1)}};

                <<"."/utf8>> ->
                    read_spread(Lexer@1, Position);

                <<"\""/utf8>> ->
                    read_string(Lexer@1, Position);

                <<"-"/utf8>> ->
                    read_negative_number(Lexer@1, Position);

                <<"0"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"1"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"2"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"3"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"4"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"5"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"6"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"7"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"8"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"9"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"a"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"b"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"c"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"d"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"e"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"f"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"g"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"h"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"i"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"j"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"k"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"l"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"m"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"n"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"o"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"p"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"q"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"r"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"s"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"t"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"u"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"v"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"w"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"x"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"y"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"z"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"A"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"B"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"C"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"D"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"E"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"F"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"G"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"H"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"I"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"J"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"K"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"L"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"M"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"N"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"O"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"P"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"Q"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"R"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"S"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"T"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"U"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"V"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"W"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"X"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"Y"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"Z"/utf8>> ->
                    read_name(Lexer@1, Position);

                <<"_"/utf8>> ->
                    read_name(Lexer@1, Position);

                _ ->
                    {error, {unexpected_character, Char, Position}}
            end
    end.

-file("src/mochi/lexer.gleam", 70).
-spec tokenize_loop(lexer_state(), list(token_with_position())) -> {ok,
        list(token_with_position())} |
    {error, lexer_error()}.
tokenize_loop(Lexer, Tokens) ->
    case next_token(Lexer) of
        {ok, {{token_with_position, e_o_f, _}, _}} ->
            _pipe = lists:reverse(
                [{token_with_position,
                        e_o_f,
                        {position,
                            erlang:element(4, Lexer),
                            erlang:element(5, Lexer)}} |
                    Tokens]
            ),
            {ok, _pipe};

        {ok, {Token, New_lexer}} ->
            tokenize_loop(New_lexer, [Token | Tokens]);

        {error, Err} ->
            {error, Err}
    end.

-file("src/mochi/lexer.gleam", 65).
-spec tokenize(binary()) -> {ok, list(token_with_position())} |
    {error, lexer_error()}.
tokenize(Input) ->
    Lexer = new_lexer(Input),
    tokenize_loop(Lexer, []).
