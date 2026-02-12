-module(mochi_fast_parser_ffi).
-export([convert_tokens/1, format_error/1]).

%% Convert tokens from Erlang lexer format to Gleam types
convert_tokens(Tokens) ->
    [convert_token(T) || T <- Tokens].

convert_token({token_with_position, Token, {position, Line, Col}}) ->
    {token_with_pos, convert_token_type(Token), {position, Line, Col}}.

%% Note: Gleam compiles variants to snake_case atoms
%% The lexer FFI already does this conversion, but this is here for safety
convert_token_type(bang) -> bang;
convert_token_type(dollar) -> dollar;
convert_token_type(amp) -> amp;
convert_token_type(left_paren) -> left_paren;
convert_token_type(right_paren) -> right_paren;
convert_token_type(spread) -> spread;
convert_token_type(colon) -> colon;
convert_token_type(equals) -> equals;
convert_token_type(at) -> at;
convert_token_type(left_bracket) -> left_bracket;
convert_token_type(right_bracket) -> right_bracket;
convert_token_type(left_brace) -> left_brace;
convert_token_type(right_brace) -> right_brace;
convert_token_type(pipe) -> pipe;
convert_token_type('query') -> 'query';
convert_token_type(mutation) -> mutation;
convert_token_type(subscription) -> subscription;
convert_token_type(fragment_kw) -> fragment_kw;
convert_token_type(on) -> on;
convert_token_type(true_kw) -> true_kw;
convert_token_type(false_kw) -> false_kw;
convert_token_type(null_kw) -> null_kw;
convert_token_type(e_o_f) -> e_o_f;
convert_token_type({name, Value}) -> {name, Value};
convert_token_type({int_value, Value}) -> {int_value, Value};
convert_token_type({float_value, Value}) -> {float_value, Value};
convert_token_type({string_value, Value}) -> {string_value, Value}.

%% Format lexer errors
format_error({unexpected_character, Char, {position, Line, Col}}) ->
    iolist_to_binary(io_lib:format("Unexpected character '~s' at line ~p, column ~p",
                                   [Char, Line, Col]));
format_error({unterminated_string, {position, Line, Col}}) ->
    iolist_to_binary(io_lib:format("Unterminated string at line ~p, column ~p",
                                   [Line, Col]));
format_error({invalid_number, Value, {position, Line, Col}}) ->
    iolist_to_binary(io_lib:format("Invalid number '~s' at line ~p, column ~p",
                                   [Value, Line, Col]));
format_error(Other) ->
    iolist_to_binary(io_lib:format("Lexer error: ~p", [Other])).
