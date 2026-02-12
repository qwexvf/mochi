-module(mochi@sdl_lexer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/sdl_lexer.gleam").
-export([new_sdl_lexer/1, next_sdl_token/1, tokenize_sdl/1]).
-export_type([s_d_l_token/0, position/0, s_d_l_token_with_position/0, s_d_l_lexer_error/0, s_d_l_lexer_state/0]).

-type s_d_l_token() :: left_brace |
    right_brace |
    left_paren |
    right_paren |
    left_bracket |
    right_bracket |
    colon |
    bang |
    equals |
    at |
    pipe |
    amp |
    type |
    interface |
    union |
    scalar |
    enum |
    input |
    directive |
    schema |
    extend |
    implements |
    {name, binary()} |
    {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    {boolean_value, boolean()} |
    e_o_f |
    {comment, binary()} |
    {description, binary()}.

-type position() :: {position, integer(), integer()}.

-type s_d_l_token_with_position() :: {s_d_l_token_with_position,
        s_d_l_token(),
        position()}.

-type s_d_l_lexer_error() :: {unexpected_character, binary(), position()} |
    {invalid_number, binary(), position()} |
    {unterminated_string, position()} |
    {unterminated_description, position()}.

-type s_d_l_lexer_state() :: {s_d_l_lexer_state,
        binary(),
        integer(),
        integer(),
        integer()}.

-file("src/mochi/sdl_lexer.gleam", 70).
-spec new_sdl_lexer(binary()) -> s_d_l_lexer_state().
new_sdl_lexer(Input) ->
    {s_d_l_lexer_state, Input, 0, 1, 1}.

-file("src/mochi/sdl_lexer.gleam", 334).
-spec peek_char(s_d_l_lexer_state()) -> {ok, binary()} | {error, nil}.
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

-file("src/mochi/sdl_lexer.gleam", 341).
-spec peek_string(s_d_l_lexer_state()) -> binary().
peek_string(Lexer) ->
    gleam@string:slice(erlang:element(2, Lexer), erlang:element(3, Lexer), 3).

-file("src/mochi/sdl_lexer.gleam", 345).
-spec peek_string_n(s_d_l_lexer_state(), integer()) -> binary().
peek_string_n(Lexer, N) ->
    gleam@string:slice(erlang:element(2, Lexer), erlang:element(3, Lexer), N).

-file("src/mochi/sdl_lexer.gleam", 349).
-spec advance_char(s_d_l_lexer_state()) -> s_d_l_lexer_state().
advance_char(Lexer) ->
    case peek_char(Lexer) of
        {ok, <<"\n"/utf8>>} ->
            {s_d_l_lexer_state,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer) + 1,
                erlang:element(4, Lexer) + 1,
                1};

        _ ->
            {s_d_l_lexer_state,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer) + 1,
                erlang:element(4, Lexer),
                erlang:element(5, Lexer) + 1}
    end.

-file("src/mochi/sdl_lexer.gleam", 204).
-spec skip_whitespace_and_comments(s_d_l_lexer_state()) -> s_d_l_lexer_state().
skip_whitespace_and_comments(Lexer) ->
    case peek_char(Lexer) of
        {ok, <<" "/utf8>>} ->
            skip_whitespace_and_comments(advance_char(Lexer));

        {ok, <<"\t"/utf8>>} ->
            skip_whitespace_and_comments(advance_char(Lexer));

        {ok, <<"\n"/utf8>>} ->
            skip_whitespace_and_comments(advance_char(Lexer));

        {ok, <<"\r"/utf8>>} ->
            skip_whitespace_and_comments(advance_char(Lexer));

        {ok, <<","/utf8>>} ->
            skip_whitespace_and_comments(advance_char(Lexer));

        _ ->
            Lexer
    end.

-file("src/mochi/sdl_lexer.gleam", 371).
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

-file("src/mochi/sdl_lexer.gleam", 429).
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

-file("src/mochi/sdl_lexer.gleam", 367).
-spec is_name_continue(binary()) -> boolean().
is_name_continue(Char) ->
    (is_letter(Char) orelse is_digit(Char)) orelse (Char =:= <<"_"/utf8>>).

-file("src/mochi/sdl_lexer.gleam", 241).
-spec read_name_chars(s_d_l_lexer_state(), binary()) -> {binary(),
    s_d_l_lexer_state()}.
read_name_chars(Lexer, Acc) ->
    case peek_char(Lexer) of
        {ok, Char} ->
            case is_name_continue(Char) of
                true ->
                    read_name_chars(
                        advance_char(Lexer),
                        <<Acc/binary, Char/binary>>
                    );

                false ->
                    {Acc, Lexer}
            end;

        _ ->
            {Acc, Lexer}
    end.

-file("src/mochi/sdl_lexer.gleam", 212).
-spec read_name_or_keyword(s_d_l_lexer_state(), position()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
read_name_or_keyword(Lexer, Position) ->
    {Name, New_lexer} = read_name_chars(Lexer, <<""/utf8>>),
    Token = case Name of
        <<"type"/utf8>> ->
            type;

        <<"interface"/utf8>> ->
            interface;

        <<"union"/utf8>> ->
            union;

        <<"scalar"/utf8>> ->
            scalar;

        <<"enum"/utf8>> ->
            enum;

        <<"input"/utf8>> ->
            input;

        <<"directive"/utf8>> ->
            directive;

        <<"schema"/utf8>> ->
            schema;

        <<"extend"/utf8>> ->
            extend;

        <<"implements"/utf8>> ->
            implements;

        <<"true"/utf8>> ->
            {boolean_value, true};

        <<"false"/utf8>> ->
            {boolean_value, false};

        _ ->
            {name, Name}
    end,
    {ok, {{s_d_l_token_with_position, Token, Position}, New_lexer}}.

-file("src/mochi/sdl_lexer.gleam", 436).
-spec read_string_chars(s_d_l_lexer_state(), binary()) -> {ok,
        {binary(), s_d_l_lexer_state()}} |
    {error, nil}.
read_string_chars(Lexer, Acc) ->
    case peek_char(Lexer) of
        {ok, <<"\""/utf8>>} ->
            {ok, {Acc, Lexer}};

        {ok, <<"\n"/utf8>>} ->
            {error, nil};

        {ok, Char} ->
            read_string_chars(advance_char(Lexer), <<Acc/binary, Char/binary>>);

        {error, _} ->
            {error, nil}
    end.

-file("src/mochi/sdl_lexer.gleam", 256).
-spec read_string(s_d_l_lexer_state(), position()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
read_string(Lexer, Position) ->
    Lexer@1 = advance_char(Lexer),
    case read_string_chars(Lexer@1, <<""/utf8>>) of
        {ok, {Value, New_lexer}} ->
            case peek_char(New_lexer) of
                {ok, <<"\""/utf8>>} ->
                    Final_lexer = advance_char(New_lexer),
                    {ok,
                        {{s_d_l_token_with_position,
                                {string_value, Value},
                                Position},
                            Final_lexer}};

                _ ->
                    {error, {unterminated_string, Position}}
            end;

        {error, _} ->
            {error, {unterminated_string, Position}}
    end.

-file("src/mochi/sdl_lexer.gleam", 449).
-spec read_description_chars(s_d_l_lexer_state(), binary()) -> {ok,
        {binary(), s_d_l_lexer_state()}} |
    {error, nil}.
read_description_chars(Lexer, Acc) ->
    case peek_string_n(Lexer, 3) of
        <<"\"\"\""/utf8>> ->
            {ok, {Acc, Lexer}};

        _ ->
            case peek_char(Lexer) of
                {ok, Char} ->
                    read_description_chars(
                        advance_char(Lexer),
                        <<Acc/binary, Char/binary>>
                    );

                {error, _} ->
                    {error, nil}
            end
    end.

-file("src/mochi/sdl_lexer.gleam", 276).
-spec read_description(s_d_l_lexer_state(), position()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
read_description(Lexer, Position) ->
    Lexer@1 = advance_char(advance_char(advance_char(Lexer))),
    case read_description_chars(Lexer@1, <<""/utf8>>) of
        {ok, {Content, New_lexer}} ->
            case peek_string_n(New_lexer, 3) of
                <<"\"\"\""/utf8>> ->
                    Final_lexer = advance_char(
                        advance_char(advance_char(New_lexer))
                    ),
                    {ok,
                        {{s_d_l_token_with_position,
                                {description, Content},
                                Position},
                            Final_lexer}};

                _ ->
                    {error, {unterminated_description, Position}}
            end;

        {error, _} ->
            {error, {unterminated_description, Position}}
    end.

-file("src/mochi/sdl_lexer.gleam", 464).
-spec read_comment_chars(s_d_l_lexer_state(), binary()) -> {binary(),
    s_d_l_lexer_state()}.
read_comment_chars(Lexer, Acc) ->
    case peek_char(Lexer) of
        {ok, <<"\n"/utf8>>} ->
            {Acc, Lexer};

        {ok, Char} ->
            read_comment_chars(advance_char(Lexer), <<Acc/binary, Char/binary>>);

        {error, _} ->
            {Acc, Lexer}
    end.

-file("src/mochi/sdl_lexer.gleam", 299).
-spec read_comment(s_d_l_lexer_state(), position()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
read_comment(Lexer, Position) ->
    Lexer@1 = advance_char(Lexer),
    {Content, New_lexer} = read_comment_chars(Lexer@1, <<""/utf8>>),
    {ok, {{s_d_l_token_with_position, {comment, Content}, Position}, New_lexer}}.

-file("src/mochi/sdl_lexer.gleam", 475).
-spec read_number_chars(s_d_l_lexer_state(), binary()) -> {binary(),
    s_d_l_lexer_state()}.
read_number_chars(Lexer, Acc) ->
    case peek_char(Lexer) of
        {ok, Char} ->
            case (is_digit(Char) orelse (Char =:= <<"."/utf8>>)) orelse (Char
            =:= <<"-"/utf8>>) of
                true ->
                    read_number_chars(
                        advance_char(Lexer),
                        <<Acc/binary, Char/binary>>
                    );

                false ->
                    {Acc, Lexer}
            end;

        _ ->
            {Acc, Lexer}
    end.

-file("src/mochi/sdl_lexer.gleam", 309).
-spec read_number(s_d_l_lexer_state(), position()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
read_number(Lexer, Position) ->
    {Number_str, New_lexer} = read_number_chars(Lexer, <<""/utf8>>),
    case gleam_stdlib:contains_string(Number_str, <<"."/utf8>>) of
        true ->
            case gleam_stdlib:parse_float(Number_str) of
                {ok, Value} ->
                    {ok,
                        {{s_d_l_token_with_position,
                                {float_value, Value},
                                Position},
                            New_lexer}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end;

        false ->
            case gleam_stdlib:parse_int(Number_str) of
                {ok, Value@1} ->
                    {ok,
                        {{s_d_l_token_with_position,
                                {int_value, Value@1},
                                Position},
                            New_lexer}};

                {error, _} ->
                    {error, {invalid_number, Number_str, Position}}
            end
    end.

-file("src/mochi/sdl_lexer.gleam", 97).
-spec next_sdl_token(s_d_l_lexer_state()) -> {ok,
        {s_d_l_token_with_position(), s_d_l_lexer_state()}} |
    {error, s_d_l_lexer_error()}.
next_sdl_token(Lexer) ->
    Lexer@1 = skip_whitespace_and_comments(Lexer),
    case peek_char(Lexer@1) of
        {error, _} ->
            {ok,
                {{s_d_l_token_with_position,
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
                <<"{"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, left_brace, Position},
                            advance_char(Lexer@1)}};

                <<"}"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, right_brace, Position},
                            advance_char(Lexer@1)}};

                <<"("/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, left_paren, Position},
                            advance_char(Lexer@1)}};

                <<")"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, right_paren, Position},
                            advance_char(Lexer@1)}};

                <<"["/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, left_bracket, Position},
                            advance_char(Lexer@1)}};

                <<"]"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, right_bracket, Position},
                            advance_char(Lexer@1)}};

                <<":"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, colon, Position},
                            advance_char(Lexer@1)}};

                <<"!"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, bang, Position},
                            advance_char(Lexer@1)}};

                <<"="/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, equals, Position},
                            advance_char(Lexer@1)}};

                <<"@"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, at, Position},
                            advance_char(Lexer@1)}};

                <<"|"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, pipe, Position},
                            advance_char(Lexer@1)}};

                <<"&"/utf8>> ->
                    {ok,
                        {{s_d_l_token_with_position, amp, Position},
                            advance_char(Lexer@1)}};

                <<"\""/utf8>> ->
                    case peek_string(Lexer@1) of
                        <<"\"\"\""/utf8>> ->
                            read_description(Lexer@1, Position);

                        _ ->
                            read_string(Lexer@1, Position)
                    end;

                <<"#"/utf8>> ->
                    read_comment(Lexer@1, Position);

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

                <<"-"/utf8>> ->
                    read_number(Lexer@1, Position);

                <<"a"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"b"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"c"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"d"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"e"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"f"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"g"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"h"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"i"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"j"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"k"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"l"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"m"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"n"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"o"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"p"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"q"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"r"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"s"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"t"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"u"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"v"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"w"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"x"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"y"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"z"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"A"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"B"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"C"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"D"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"E"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"F"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"G"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"H"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"I"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"J"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"K"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"L"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"M"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"N"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"O"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"P"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"Q"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"R"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"S"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"T"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"U"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"V"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"W"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"X"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"Y"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"Z"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                <<"_"/utf8>> ->
                    read_name_or_keyword(Lexer@1, Position);

                _ ->
                    {error, {unexpected_character, Char, Position}}
            end
    end.

-file("src/mochi/sdl_lexer.gleam", 81).
-spec tokenize_loop(s_d_l_lexer_state(), list(s_d_l_token_with_position())) -> {ok,
        list(s_d_l_token_with_position())} |
    {error, s_d_l_lexer_error()}.
tokenize_loop(Lexer, Tokens) ->
    case next_sdl_token(Lexer) of
        {ok, {{s_d_l_token_with_position, e_o_f, _}, _}} ->
            _pipe = lists:reverse(
                [{s_d_l_token_with_position,
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

-file("src/mochi/sdl_lexer.gleam", 74).
-spec tokenize_sdl(binary()) -> {ok, list(s_d_l_token_with_position())} |
    {error, s_d_l_lexer_error()}.
tokenize_sdl(Input) ->
    Lexer = new_sdl_lexer(Input),
    tokenize_loop(Lexer, []).
