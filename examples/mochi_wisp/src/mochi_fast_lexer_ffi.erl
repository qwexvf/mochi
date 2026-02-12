-module(mochi_fast_lexer_ffi).
-export([tokenize/1]).

%% Fast GraphQL lexer using binary pattern matching
%% Achieves O(n) complexity vs O(n²) in the Gleam version

-define(is_whitespace(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r orelse C =:= $,)).
-define(is_digit(C), (C >= $0 andalso C =< $9)).
-define(is_letter(C), ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z))).
-define(is_name_start(C), (?is_letter(C) orelse C =:= $_)).
-define(is_name_continue(C), (?is_name_start(C) orelse ?is_digit(C))).

%% Main entry point
tokenize(Input) when is_binary(Input) ->
    try
        Tokens = tokenize_loop(Input, 0, 1, 1, []),
        {ok, lists:reverse(Tokens)}
    catch
        throw:{error, Error} -> {error, Error}
    end;
tokenize(Input) when is_list(Input) ->
    tokenize(list_to_binary(Input)).

tokenize_loop(Input, Pos, Line, Col, Acc) ->
    case skip_whitespace(Input, Pos, Line, Col) of
        {eof, _, NewLine, NewCol} ->
            [make_token(eof, NewLine, NewCol) | Acc];
        {NewPos, NewLine, NewCol} ->
            case Input of
                <<_:NewPos/binary, C, _/binary>> ->
                    {Token, NextPos, NextLine, NextCol} =
                        read_token(Input, NewPos, NewLine, NewCol, C),
                    tokenize_loop(Input, NextPos, NextLine, NextCol, [Token | Acc]);
                _ ->
                    [make_token(eof, NewLine, NewCol) | Acc]
            end
    end.

skip_whitespace(Input, Pos, Line, Col) ->
    case Input of
        <<_:Pos/binary, C, _/binary>> when ?is_whitespace(C) ->
            case C of
                $\n -> skip_whitespace(Input, Pos + 1, Line + 1, 1);
                _ -> skip_whitespace(Input, Pos + 1, Line, Col + 1)
            end;
        <<_:Pos/binary, $#, _/binary>> ->
            skip_comment(Input, Pos + 1, Line, Col + 1);
        <<_:Pos/binary, _, _/binary>> ->
            {Pos, Line, Col};
        _ ->
            {eof, Pos, Line, Col}
    end.

skip_comment(Input, Pos, Line, Col) ->
    case Input of
        <<_:Pos/binary, $\n, _/binary>> ->
            skip_whitespace(Input, Pos + 1, Line + 1, 1);
        <<_:Pos/binary, _, _/binary>> ->
            skip_comment(Input, Pos + 1, Line, Col + 1);
        _ ->
            {eof, Pos, Line, Col}
    end.

read_token(Input, Pos, Line, Col, C) ->
    case C of
        $! -> {make_token(bang, Line, Col), Pos + 1, Line, Col + 1};
        $$ -> {make_token(dollar, Line, Col), Pos + 1, Line, Col + 1};
        $& -> {make_token(amp, Line, Col), Pos + 1, Line, Col + 1};
        $( -> {make_token(left_paren, Line, Col), Pos + 1, Line, Col + 1};
        $) -> {make_token(right_paren, Line, Col), Pos + 1, Line, Col + 1};
        $: -> {make_token(colon, Line, Col), Pos + 1, Line, Col + 1};
        $= -> {make_token(equals, Line, Col), Pos + 1, Line, Col + 1};
        $@ -> {make_token(at, Line, Col), Pos + 1, Line, Col + 1};
        $[ -> {make_token(left_bracket, Line, Col), Pos + 1, Line, Col + 1};
        $] -> {make_token(right_bracket, Line, Col), Pos + 1, Line, Col + 1};
        ${ -> {make_token(left_brace, Line, Col), Pos + 1, Line, Col + 1};
        $} -> {make_token(right_brace, Line, Col), Pos + 1, Line, Col + 1};
        $| -> {make_token(pipe, Line, Col), Pos + 1, Line, Col + 1};
        $. -> read_spread(Input, Pos, Line, Col);
        $" -> read_string(Input, Pos + 1, Line, Col + 1, [], Line, Col);
        $- -> read_number(Input, Pos, Line, Col, true);
        _ when ?is_digit(C) -> read_number(Input, Pos, Line, Col, false);
        _ when ?is_name_start(C) -> read_name(Input, Pos, Line, Col);
        _ -> throw({error, {unexpected_character, <<C>>, {position, Line, Col}}})
    end.

read_spread(Input, Pos, Line, Col) ->
    case Input of
        <<_:Pos/binary, "...", _/binary>> ->
            {make_token(spread, Line, Col), Pos + 3, Line, Col + 3};
        _ ->
            throw({error, {unexpected_character, <<".">>, {position, Line, Col}}})
    end.

%% Read string - accumulate chars in reverse, then reverse at end
read_string(Input, Pos, Line, Col, Acc, StartLine, StartCol) ->
    case Input of
        <<_:Pos/binary, $", _/binary>> ->
            Value = list_to_binary(lists:reverse(Acc)),
            {make_token({string_value, Value}, StartLine, StartCol), Pos + 1, Line, Col + 1};
        <<_:Pos/binary, $\\, Escaped, _/binary>> ->
            {Char, Skip} = case Escaped of
                $" -> {$", 2};
                $\\ -> {$\\, 2};
                $/ -> {$/, 2};
                $b -> {$\b, 2};
                $f -> {$\f, 2};
                $n -> {$\n, 2};
                $r -> {$\r, 2};
                $t -> {$\t, 2};
                _ -> {Escaped, 2}
            end,
            read_string(Input, Pos + Skip, Line, Col + Skip, [Char | Acc], StartLine, StartCol);
        <<_:Pos/binary, $\n, _/binary>> ->
            read_string(Input, Pos + 1, Line + 1, 1, [$\n | Acc], StartLine, StartCol);
        <<_:Pos/binary, C2, _/binary>> ->
            read_string(Input, Pos + 1, Line, Col + 1, [C2 | Acc], StartLine, StartCol);
        _ ->
            throw({error, {unterminated_string, {position, StartLine, StartCol}}})
    end.

%% Read number - integers and floats
read_number(Input, Pos, Line, Col, IsNegative) ->
    StartPos = case IsNegative of true -> Pos + 1; false -> Pos end,
    {IntPart, NextPos} = read_digits(Input, StartPos, []),
    case Input of
        <<_:NextPos/binary, $., _/binary>> ->
            {FracPart, FracPos} = read_digits(Input, NextPos + 1, []),
            NumStr = case IsNegative of
                true -> <<"-", IntPart/binary, ".", FracPart/binary>>;
                false -> <<IntPart/binary, ".", FracPart/binary>>
            end,
            Value = binary_to_float(NumStr),
            Len = FracPos - Pos,
            {make_token({float_value, Value}, Line, Col), FracPos, Line, Col + Len};
        _ ->
            NumStr = case IsNegative of
                true -> <<"-", IntPart/binary>>;
                false -> IntPart
            end,
            Value = binary_to_integer(NumStr),
            Len = NextPos - Pos,
            {make_token({int_value, Value}, Line, Col), NextPos, Line, Col + Len}
    end.

read_digits(Input, Pos, Acc) ->
    case Input of
        <<_:Pos/binary, C, _/binary>> when ?is_digit(C) ->
            read_digits(Input, Pos + 1, [C | Acc]);
        _ ->
            {list_to_binary(lists:reverse(Acc)), Pos}
    end.

%% Read name/keyword - accumulate in reverse list
read_name(Input, Pos, Line, Col) ->
    {Name, NextPos} = read_name_chars(Input, Pos, []),
    TokenType = keyword_or_name(Name),
    Len = NextPos - Pos,
    {make_token(TokenType, Line, Col), NextPos, Line, Col + Len}.

read_name_chars(Input, Pos, Acc) ->
    case Input of
        <<_:Pos/binary, C, _/binary>> when ?is_name_continue(C) ->
            read_name_chars(Input, Pos + 1, [C | Acc]);
        _ ->
            {list_to_binary(lists:reverse(Acc)), Pos}
    end.

keyword_or_name(Name) ->
    case Name of
        <<"query">> -> query;
        <<"mutation">> -> mutation;
        <<"subscription">> -> subscription;
        <<"fragment">> -> fragment;
        <<"on">> -> on;
        <<"true">> -> true_keyword;
        <<"false">> -> false_keyword;
        <<"null">> -> null_keyword;
        _ -> {name, Name}
    end.

%% Create token records matching Gleam's TokenWithPosition type
make_token(Type, Line, Col) ->
    {token_with_position, to_gleam_token(Type), {position, Line, Col}}.

%% Convert to Gleam token types
%% Note: Gleam compiles variants to snake_case atoms
%% EOF -> e_o_f, Query -> 'query', FragmentKw -> fragment_kw, etc.
to_gleam_token(bang) -> bang;
to_gleam_token(dollar) -> dollar;
to_gleam_token(amp) -> amp;
to_gleam_token(left_paren) -> left_paren;
to_gleam_token(right_paren) -> right_paren;
to_gleam_token(spread) -> spread;
to_gleam_token(colon) -> colon;
to_gleam_token(equals) -> equals;
to_gleam_token(at) -> at;
to_gleam_token(left_bracket) -> left_bracket;
to_gleam_token(right_bracket) -> right_bracket;
to_gleam_token(left_brace) -> left_brace;
to_gleam_token(right_brace) -> right_brace;
to_gleam_token(pipe) -> pipe;
to_gleam_token(query) -> 'query';  %% Quoted - reserved word in Erlang
to_gleam_token(mutation) -> mutation;
to_gleam_token(subscription) -> subscription;
to_gleam_token(fragment) -> fragment_kw;  %% FragmentKw in Gleam
to_gleam_token(on) -> on;
to_gleam_token(true_keyword) -> true_kw;  %% TrueKw in Gleam
to_gleam_token(false_keyword) -> false_kw;  %% FalseKw in Gleam
to_gleam_token(null_keyword) -> null_kw;  %% NullKw in Gleam
to_gleam_token(eof) -> e_o_f;  %% EOF in Gleam compiles to e_o_f
to_gleam_token({name, Value}) -> {name, Value};
to_gleam_token({int_value, Value}) -> {int_value, Value};
to_gleam_token({float_value, Value}) -> {float_value, Value};
to_gleam_token({string_value, Value}) -> {string_value, Value}.
