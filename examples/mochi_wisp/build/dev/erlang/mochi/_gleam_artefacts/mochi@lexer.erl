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

-file("src/mochi/lexer.gleam", 187).
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

-file("src/mochi/lexer.gleam", 194).
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

-file("src/mochi/lexer.gleam", 179).
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

        _ ->
            Lexer
    end.

-file("src/mochi/lexer.gleam", 213).
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

-file("src/mochi/lexer.gleam", 234).
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

-file("src/mochi/lexer.gleam", 227).
-spec read_string(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_string(Lexer, Position) ->
    read_string_loop(advance_char(Lexer), Position, <<""/utf8>>).

-file("src/mochi/lexer.gleam", 320).
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

-file("src/mochi/lexer.gleam", 313).
-spec read_while(lexer_state(), fun((binary()) -> boolean())) -> {binary(),
    lexer_state()}.
read_while(Lexer, Predicate) ->
    read_while_loop(Lexer, Predicate, <<""/utf8>>).

-file("src/mochi/lexer.gleam", 335).
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

-file("src/mochi/lexer.gleam", 288).
-spec is_digit_or_dot(binary()) -> boolean().
is_digit_or_dot(C) ->
    is_digit(C) orelse (C =:= <<"."/utf8>>).

-file("src/mochi/lexer.gleam", 264).
-spec read_number(lexer_state(), position()) -> {ok,
        {token_with_position(), lexer_state()}} |
    {error, lexer_error()}.
read_number(Lexer, Position) ->
    {Number_str, New_lexer} = read_while(Lexer, fun is_digit_or_dot/1),
    case gleam_stdlib:contains_string(Number_str, <<"."/utf8>>) of
        true ->
            case gleam_stdlib:parse_float(Number_str) of
                {ok, Value} ->
                    {ok,
                        {{token_with_position, {float_value, Value}, Position},
                            New_lexer}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end;

        false ->
            case gleam_stdlib:parse_int(Number_str) of
                {ok, Value@1} ->
                    {ok,
                        {{token_with_position, {int_value, Value@1}, Position},
                            New_lexer}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end
    end.

-file("src/mochi/lexer.gleam", 346).
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

-file("src/mochi/lexer.gleam", 342).
-spec is_name_continue(binary()) -> boolean().
is_name_continue(Char) ->
    (is_letter(Char) orelse is_digit(Char)) orelse (Char =:= <<"_"/utf8>>).

-file("src/mochi/lexer.gleam", 292).
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
