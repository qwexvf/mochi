-module(lexer_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/lexer_test.gleam").
-export([punctuation_tokens_test/0, spread_token_test/0, keyword_query_test/0, keyword_mutation_test/0, keyword_subscription_test/0, keyword_fragment_test/0, keyword_on_test/0, keyword_true_test/0, keyword_false_test/0, keyword_null_test/0, simple_name_test/0, name_with_digits_and_underscore_test/0, underscore_leading_name_test/0, positive_integer_test/0, zero_integer_test/0, large_integer_test/0, negative_integer_test/0, negative_zero_test/0, negative_large_integer_test/0, simple_float_test/0, negative_float_test/0, float_with_lowercase_exponent_test/0, float_with_uppercase_exponent_test/0, float_with_positive_exponent_sign_test/0, float_with_negative_exponent_test/0, integer_with_exponent_test/0, negative_float_with_exponent_test/0, simple_string_test/0, empty_string_test/0, string_with_escape_quote_test/0, string_with_escape_backslash_test/0, string_with_escape_newline_test/0, string_with_escape_tab_test/0, unterminated_string_is_error_test/0, comment_is_ignored_test/0, inline_comment_is_ignored_test/0, comment_at_end_of_file_test/0, multiple_comment_lines_test/0, commas_are_ignored_test/0, mixed_whitespace_test/0, position_tracks_line_and_column_test/0, full_query_tokenizes_test/0, mutation_with_negative_default_tokenizes_test/0, unicode_escape_basic_latin_test/0, unicode_escape_latin_supplement_test/0, unicode_escape_zero_test/0, unicode_escape_lowercase_hex_test/0, unicode_escape_mixed_case_hex_test/0, unicode_escape_in_middle_of_string_test/0, unicode_escape_multiple_in_string_test/0, block_string_simple_test/0, block_string_empty_test/0, block_string_preserves_newlines_test/0, block_string_strips_common_indent_test/0, block_string_escaped_triple_quote_test/0, block_string_leading_trailing_blank_lines_stripped_test/0, block_string_unterminated_is_error_test/0, block_string_in_query_test/0]).

-file("test/lexer_test.gleam", 6).
-spec tokens(binary()) -> {ok, list(mochi@lexer:token())} |
    {error, mochi@lexer:lexer_error()}.
tokens(Input) ->
    case mochi@lexer:tokenize(Input) of
        {ok, Ts} ->
            {ok, gleam@list:map(Ts, fun(T) -> erlang:element(2, T) end)};

        {error, E} ->
            {error, E}
    end.

-file("test/lexer_test.gleam", 13).
-spec inspect_error(mochi@lexer:lexer_error()) -> binary().
inspect_error(E) ->
    case E of
        {unexpected_character, C, _} ->
            <<<<"UnexpectedCharacter("/utf8, C/binary>>/binary, ")"/utf8>>;

        {invalid_number, N, _} ->
            <<<<"InvalidNumber("/utf8, N/binary>>/binary, ")"/utf8>>;

        {unterminated_string, _} ->
            <<"UnterminatedString"/utf8>>
    end.

-file("test/lexer_test.gleam", 45).
-spec inspect_error_or_ok(
    {ok, list(mochi@lexer:token())} | {error, mochi@lexer:lexer_error()}
) -> binary().
inspect_error_or_ok(R) ->
    case R of
        {ok, _} ->
            <<"ok-but-wrong-tokens"/utf8>>;

        {error, E} ->
            inspect_error(E)
    end.

-file("test/lexer_test.gleam", 23).
-spec punctuation_tokens_test() -> nil.
punctuation_tokens_test() ->
    case tokens(<<"! $ & ( ) : = @ [ ] { } |"/utf8>>) of
        {ok,
            [bang,
                dollar,
                amp,
                left_paren,
                right_paren,
                colon,
                equals,
                at,
                left_bracket,
                right_bracket,
                left_brace,
                right_brace,
                pipe,
                e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Unexpected: "/utf8,
                        (inspect_error_or_ok(Other))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"punctuation_tokens_test"/utf8>>,
                    line => 41})
    end.

-file("test/lexer_test.gleam", 52).
-spec spread_token_test() -> nil.
spread_token_test() ->
    case tokens(<<"..."/utf8>>) of
        {ok, [spread, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"spread_token_test"/utf8>>,
                    line => 55})
    end.

-file("test/lexer_test.gleam", 61).
-spec keyword_query_test() -> nil.
keyword_query_test() ->
    case tokens(<<"query"/utf8>>) of
        {ok, ['query', e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_query_test"/utf8>>,
                    line => 64})
    end.

-file("test/lexer_test.gleam", 68).
-spec keyword_mutation_test() -> nil.
keyword_mutation_test() ->
    case tokens(<<"mutation"/utf8>>) of
        {ok, [mutation, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_mutation_test"/utf8>>,
                    line => 71})
    end.

-file("test/lexer_test.gleam", 75).
-spec keyword_subscription_test() -> nil.
keyword_subscription_test() ->
    case tokens(<<"subscription"/utf8>>) of
        {ok, [subscription, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_subscription_test"/utf8>>,
                    line => 78})
    end.

-file("test/lexer_test.gleam", 82).
-spec keyword_fragment_test() -> nil.
keyword_fragment_test() ->
    case tokens(<<"fragment"/utf8>>) of
        {ok, [fragment, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_fragment_test"/utf8>>,
                    line => 85})
    end.

-file("test/lexer_test.gleam", 89).
-spec keyword_on_test() -> nil.
keyword_on_test() ->
    case tokens(<<"on"/utf8>>) of
        {ok, [on, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_on_test"/utf8>>,
                    line => 92})
    end.

-file("test/lexer_test.gleam", 96).
-spec keyword_true_test() -> nil.
keyword_true_test() ->
    case tokens(<<"true"/utf8>>) of
        {ok, [true_keyword, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_true_test"/utf8>>,
                    line => 99})
    end.

-file("test/lexer_test.gleam", 103).
-spec keyword_false_test() -> nil.
keyword_false_test() ->
    case tokens(<<"false"/utf8>>) of
        {ok, [false_keyword, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_false_test"/utf8>>,
                    line => 106})
    end.

-file("test/lexer_test.gleam", 110).
-spec keyword_null_test() -> nil.
keyword_null_test() ->
    case tokens(<<"null"/utf8>>) of
        {ok, [null_keyword, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"keyword_null_test"/utf8>>,
                    line => 113})
    end.

-file("test/lexer_test.gleam", 119).
-spec simple_name_test() -> nil.
simple_name_test() ->
    case tokens(<<"hello"/utf8>>) of
        {ok, [{name, <<"hello"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"simple_name_test"/utf8>>,
                    line => 122})
    end.

-file("test/lexer_test.gleam", 126).
-spec name_with_digits_and_underscore_test() -> nil.
name_with_digits_and_underscore_test() ->
    case tokens(<<"field_name_42"/utf8>>) of
        {ok, [{name, <<"field_name_42"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"name_with_digits_and_underscore_test"/utf8>>,
                    line => 129})
    end.

-file("test/lexer_test.gleam", 133).
-spec underscore_leading_name_test() -> nil.
underscore_leading_name_test() ->
    case tokens(<<"_privateField"/utf8>>) of
        {ok, [{name, <<"_privateField"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"underscore_leading_name_test"/utf8>>,
                    line => 136})
    end.

-file("test/lexer_test.gleam", 142).
-spec positive_integer_test() -> nil.
positive_integer_test() ->
    case tokens(<<"42"/utf8>>) of
        {ok, [{int_value, 42}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"positive_integer_test"/utf8>>,
                    line => 145})
    end.

-file("test/lexer_test.gleam", 149).
-spec zero_integer_test() -> nil.
zero_integer_test() ->
    case tokens(<<"0"/utf8>>) of
        {ok, [{int_value, 0}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"zero_integer_test"/utf8>>,
                    line => 152})
    end.

-file("test/lexer_test.gleam", 156).
-spec large_integer_test() -> nil.
large_integer_test() ->
    case tokens(<<"1000000"/utf8>>) of
        {ok, [{int_value, 1000000}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"large_integer_test"/utf8>>,
                    line => 159})
    end.

-file("test/lexer_test.gleam", 164).
-spec negative_integer_test() -> nil.
negative_integer_test() ->
    case tokens(<<"-5"/utf8>>) of
        {ok, [{int_value, -5}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"negative_integer_test"/utf8>>,
                    line => 167})
    end.

-file("test/lexer_test.gleam", 171).
-spec negative_zero_test() -> nil.
negative_zero_test() ->
    case tokens(<<"-0"/utf8>>) of
        {ok, [{int_value, 0}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"negative_zero_test"/utf8>>,
                    line => 174})
    end.

-file("test/lexer_test.gleam", 178).
-spec negative_large_integer_test() -> nil.
negative_large_integer_test() ->
    case tokens(<<"-1000"/utf8>>) of
        {ok, [{int_value, -1000}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"negative_large_integer_test"/utf8>>,
                    line => 181})
    end.

-file("test/lexer_test.gleam", 187).
-spec simple_float_test() -> nil.
simple_float_test() ->
    case tokens(<<"3.14"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 3.13) andalso (V < 3.15) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"float value out of expected range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"simple_float_test"/utf8>>,
                            line => 192})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"simple_float_test"/utf8>>,
                    line => 194})
    end.

-file("test/lexer_test.gleam", 198).
-spec negative_float_test() -> nil.
negative_float_test() ->
    case tokens(<<"-1.5"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > -1.6) andalso (V < -1.4) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"negative float value out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"negative_float_test"/utf8>>,
                            line => 203})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"negative_float_test"/utf8>>,
                    line => 205})
    end.

-file("test/lexer_test.gleam", 209).
-spec float_with_lowercase_exponent_test() -> nil.
float_with_lowercase_exponent_test() ->
    case tokens(<<"1.5e2"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 149.0) andalso (V < 151.0) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"exponent float value out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"float_with_lowercase_exponent_test"/utf8>>,
                            line => 214})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"float_with_lowercase_exponent_test"/utf8>>,
                    line => 216})
    end.

-file("test/lexer_test.gleam", 220).
-spec float_with_uppercase_exponent_test() -> nil.
float_with_uppercase_exponent_test() ->
    case tokens(<<"2.0E3"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 1999.0) andalso (V < 2001.0) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"uppercase exponent float out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"float_with_uppercase_exponent_test"/utf8>>,
                            line => 225})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"float_with_uppercase_exponent_test"/utf8>>,
                    line => 227})
    end.

-file("test/lexer_test.gleam", 231).
-spec float_with_positive_exponent_sign_test() -> nil.
float_with_positive_exponent_sign_test() ->
    case tokens(<<"1.0e+2"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 99.0) andalso (V < 101.0) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"positive exponent sign float out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"float_with_positive_exponent_sign_test"/utf8>>,
                            line => 236})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"float_with_positive_exponent_sign_test"/utf8>>,
                    line => 238})
    end.

-file("test/lexer_test.gleam", 242).
-spec float_with_negative_exponent_test() -> nil.
float_with_negative_exponent_test() ->
    case tokens(<<"1.0e-2"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 0.009) andalso (V < 0.011) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"negative exponent float out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"float_with_negative_exponent_test"/utf8>>,
                            line => 247})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"float_with_negative_exponent_test"/utf8>>,
                    line => 249})
    end.

-file("test/lexer_test.gleam", 253).
-spec integer_with_exponent_test() -> nil.
integer_with_exponent_test() ->
    case tokens(<<"1e3"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > 999.0) andalso (V < 1001.0) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"integer exponent float out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"integer_with_exponent_test"/utf8>>,
                            line => 258})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"integer_with_exponent_test"/utf8>>,
                    line => 260})
    end.

-file("test/lexer_test.gleam", 264).
-spec negative_float_with_exponent_test() -> nil.
negative_float_with_exponent_test() ->
    case tokens(<<"-1.5e2"/utf8>>) of
        {ok, [{float_value, V}, e_o_f]} ->
            case (V > -151.0) andalso (V < -149.0) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"negative exponent float out of range"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"negative_float_with_exponent_test"/utf8>>,
                            line => 269})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"negative_float_with_exponent_test"/utf8>>,
                    line => 271})
    end.

-file("test/lexer_test.gleam", 277).
-spec simple_string_test() -> nil.
simple_string_test() ->
    case tokens(<<"\"hello\""/utf8>>) of
        {ok, [{string_value, <<"hello"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"simple_string_test"/utf8>>,
                    line => 280})
    end.

-file("test/lexer_test.gleam", 284).
-spec empty_string_test() -> nil.
empty_string_test() ->
    case tokens(<<"\"\""/utf8>>) of
        {ok, [{string_value, <<""/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"empty_string_test"/utf8>>,
                    line => 287})
    end.

-file("test/lexer_test.gleam", 291).
-spec string_with_escape_quote_test() -> nil.
string_with_escape_quote_test() ->
    case tokens(<<"\"say \\\"hi\\\"\""/utf8>>) of
        {ok, [{string_value, <<"say \"hi\""/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"string_with_escape_quote_test"/utf8>>,
                    line => 294})
    end.

-file("test/lexer_test.gleam", 298).
-spec string_with_escape_backslash_test() -> nil.
string_with_escape_backslash_test() ->
    case tokens(<<"\"a\\\\b\""/utf8>>) of
        {ok, [{string_value, <<"a\\b"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"string_with_escape_backslash_test"/utf8>>,
                    line => 301})
    end.

-file("test/lexer_test.gleam", 305).
-spec string_with_escape_newline_test() -> nil.
string_with_escape_newline_test() ->
    case tokens(<<"\"line1\\nline2\""/utf8>>) of
        {ok, [{string_value, <<"line1\nline2"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"string_with_escape_newline_test"/utf8>>,
                    line => 308})
    end.

-file("test/lexer_test.gleam", 312).
-spec string_with_escape_tab_test() -> nil.
string_with_escape_tab_test() ->
    case tokens(<<"\"col1\\tcol2\""/utf8>>) of
        {ok, [{string_value, <<"col1\tcol2"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"string_with_escape_tab_test"/utf8>>,
                    line => 315})
    end.

-file("test/lexer_test.gleam", 319).
-spec unterminated_string_is_error_test() -> nil.
unterminated_string_is_error_test() ->
    case tokens(<<"\"unterminated"/utf8>>) of
        {error, {unterminated_string, _}} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unterminated_string_is_error_test"/utf8>>,
                    line => 322})
    end.

-file("test/lexer_test.gleam", 328).
-spec comment_is_ignored_test() -> nil.
comment_is_ignored_test() ->
    case tokens(<<"# this is a comment\n42"/utf8>>) of
        {ok, [{int_value, 42}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"comment_is_ignored_test"/utf8>>,
                    line => 331})
    end.

-file("test/lexer_test.gleam", 335).
-spec inline_comment_is_ignored_test() -> nil.
inline_comment_is_ignored_test() ->
    case tokens(<<"{ field # comment here\n}"/utf8>>) of
        {ok, [left_brace, {name, <<"field"/utf8>>}, right_brace, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"inline_comment_is_ignored_test"/utf8>>,
                    line => 339})
    end.

-file("test/lexer_test.gleam", 343).
-spec comment_at_end_of_file_test() -> nil.
comment_at_end_of_file_test() ->
    case tokens(<<"hello # trailing comment"/utf8>>) of
        {ok, [{name, <<"hello"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"comment_at_end_of_file_test"/utf8>>,
                    line => 346})
    end.

-file("test/lexer_test.gleam", 350).
-spec multiple_comment_lines_test() -> nil.
multiple_comment_lines_test() ->
    case tokens(<<"# line 1\n# line 2\nfield"/utf8>>) of
        {ok, [{name, <<"field"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"multiple_comment_lines_test"/utf8>>,
                    line => 353})
    end.

-file("test/lexer_test.gleam", 359).
-spec commas_are_ignored_test() -> nil.
commas_are_ignored_test() ->
    case tokens(<<"a, b, c"/utf8>>) of
        {ok,
            [{name, <<"a"/utf8>>},
                {name, <<"b"/utf8>>},
                {name, <<"c"/utf8>>},
                e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"commas_are_ignored_test"/utf8>>,
                    line => 362})
    end.

-file("test/lexer_test.gleam", 366).
-spec mixed_whitespace_test() -> nil.
mixed_whitespace_test() ->
    case tokens(<<"a  \t  b\n\rc"/utf8>>) of
        {ok,
            [{name, <<"a"/utf8>>},
                {name, <<"b"/utf8>>},
                {name, <<"c"/utf8>>},
                e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"mixed_whitespace_test"/utf8>>,
                    line => 369})
    end.

-file("test/lexer_test.gleam", 375).
-spec position_tracks_line_and_column_test() -> nil.
position_tracks_line_and_column_test() ->
    case mochi@lexer:tokenize(<<"ab\ncd"/utf8>>) of
        {ok,
            [{token_with_position, {name, <<"ab"/utf8>>}, {position, 1, 1}},
                {token_with_position, {name, <<"cd"/utf8>>}, {position, 2, 1}},
                {token_with_position, e_o_f, _}]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Unexpected position result: "/utf8,
                        (case Other of
                            {ok, _} ->
                                <<"ok-wrong-tokens"/utf8>>;

                            {error, E} ->
                                inspect_error(E)
                        end)/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"position_tracks_line_and_column_test"/utf8>>,
                    line => 383})
    end.

-file("test/lexer_test.gleam", 395).
-spec full_query_tokenizes_test() -> nil.
full_query_tokenizes_test() ->
    case tokens(
        <<"# Get a user
      query GetUser($id: ID!) {
        user(id: $id) {
          id
          name
          email
        }
      }"/utf8>>
    ) of
        {ok, Ts} ->
            case gleam@list:contains(Ts, 'query') of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected Query token in result"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"full_query_tokenizes_test"/utf8>>,
                            line => 411})
            end;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Failed to tokenize real query: "/utf8,
                        (inspect_error(E))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"full_query_tokenizes_test"/utf8>>,
                    line => 415})
    end.

-file("test/lexer_test.gleam", 419).
-spec mutation_with_negative_default_tokenizes_test() -> nil.
mutation_with_negative_default_tokenizes_test() ->
    case tokens(<<"mutation { adjust(delta: -10) { result } }"/utf8>>) of
        {ok, Ts} ->
            case gleam@list:contains(Ts, {int_value, -10}) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected IntValue(-10) in token list"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"mutation_with_negative_default_tokenizes_test"/utf8>>,
                            line => 424})
            end;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Failed to tokenize negative default: "/utf8,
                        (inspect_error(E))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"mutation_with_negative_default_tokenizes_test"/utf8>>,
                    line => 428})
    end.

-file("test/lexer_test.gleam", 434).
-spec unicode_escape_basic_latin_test() -> nil.
unicode_escape_basic_latin_test() ->
    case tokens(<<"\"\\u0041\""/utf8>>) of
        {ok, [{string_value, <<"A"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_basic_latin_test"/utf8>>,
                    line => 437})
    end.

-file("test/lexer_test.gleam", 441).
-spec unicode_escape_latin_supplement_test() -> nil.
unicode_escape_latin_supplement_test() ->
    case tokens(<<"\"caf\\u00E9\""/utf8>>) of
        {ok, [{string_value, <<"café"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_latin_supplement_test"/utf8>>,
                    line => 444})
    end.

-file("test/lexer_test.gleam", 448).
-spec unicode_escape_zero_test() -> nil.
unicode_escape_zero_test() ->
    case tokens(<<"\"\\u0000\""/utf8>>) of
        {ok, [{string_value, <<"\x{0000}"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_zero_test"/utf8>>,
                    line => 451})
    end.

-file("test/lexer_test.gleam", 455).
-spec unicode_escape_lowercase_hex_test() -> nil.
unicode_escape_lowercase_hex_test() ->
    case tokens(<<"\"\\u006e\""/utf8>>) of
        {ok, [{string_value, <<"n"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_lowercase_hex_test"/utf8>>,
                    line => 458})
    end.

-file("test/lexer_test.gleam", 462).
-spec unicode_escape_mixed_case_hex_test() -> nil.
unicode_escape_mixed_case_hex_test() ->
    case tokens(<<"\"\\u004F\""/utf8>>) of
        {ok, [{string_value, <<"O"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_mixed_case_hex_test"/utf8>>,
                    line => 465})
    end.

-file("test/lexer_test.gleam", 469).
-spec unicode_escape_in_middle_of_string_test() -> nil.
unicode_escape_in_middle_of_string_test() ->
    case tokens(<<"\"hel\\u006Co\""/utf8>>) of
        {ok, [{string_value, <<"hello"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_in_middle_of_string_test"/utf8>>,
                    line => 472})
    end.

-file("test/lexer_test.gleam", 476).
-spec unicode_escape_multiple_in_string_test() -> nil.
unicode_escape_multiple_in_string_test() ->
    case tokens(<<"\"\\u0048\\u0069\""/utf8>>) of
        {ok, [{string_value, <<"Hi"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"unicode_escape_multiple_in_string_test"/utf8>>,
                    line => 479})
    end.

-file("test/lexer_test.gleam", 485).
-spec block_string_simple_test() -> nil.
block_string_simple_test() ->
    case tokens(<<"\"\"\"hello\"\"\""/utf8>>) of
        {ok, [{string_value, <<"hello"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_simple_test"/utf8>>,
                    line => 488})
    end.

-file("test/lexer_test.gleam", 492).
-spec block_string_empty_test() -> nil.
block_string_empty_test() ->
    case tokens(<<"\"\"\"\"\"\""/utf8>>) of
        {ok, [{string_value, <<""/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_empty_test"/utf8>>,
                    line => 495})
    end.

-file("test/lexer_test.gleam", 499).
-spec block_string_preserves_newlines_test() -> nil.
block_string_preserves_newlines_test() ->
    case tokens(<<"\"\"\"line1\nline2\"\"\""/utf8>>) of
        {ok, [{string_value, <<"line1\nline2"/utf8>>}, e_o_f]} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_preserves_newlines_test"/utf8>>,
                    line => 502})
    end.

-file("test/lexer_test.gleam", 506).
-spec block_string_strips_common_indent_test() -> nil.
block_string_strips_common_indent_test() ->
    case tokens(<<"\"\"\"
    hello
    world
    \"\"\""/utf8>>) of
        {ok, [{string_value, V}, e_o_f]} ->
            case V =:= <<"hello\nworld"/utf8>> of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<<<"Unexpected value: '"/utf8,
                                    V/binary>>/binary,
                                "'"/utf8>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"block_string_strips_common_indent_test"/utf8>>,
                            line => 518})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_strips_common_indent_test"/utf8>>,
                    line => 520})
    end.

-file("test/lexer_test.gleam", 524).
-spec block_string_escaped_triple_quote_test() -> nil.
block_string_escaped_triple_quote_test() ->
    case tokens(<<"\"\"\"say \\\"\"\"hello\\\"\"\"\"\"\""/utf8>>) of
        {ok, [{string_value, V}, e_o_f]} ->
            case V =:= <<"say \"\"\"hello\"\"\""/utf8>> of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<<<"Unexpected value: '"/utf8,
                                    V/binary>>/binary,
                                "'"/utf8>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"block_string_escaped_triple_quote_test"/utf8>>,
                            line => 529})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_escaped_triple_quote_test"/utf8>>,
                    line => 531})
    end.

-file("test/lexer_test.gleam", 535).
-spec block_string_leading_trailing_blank_lines_stripped_test() -> nil.
block_string_leading_trailing_blank_lines_stripped_test() ->
    case tokens(<<"\"\"\"\n\n  hello\n\n\"\"\""/utf8>>) of
        {ok, [{string_value, V}, e_o_f]} ->
            case V =:= <<"hello"/utf8>> of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<<<"Unexpected value: '"/utf8,
                                    V/binary>>/binary,
                                "'"/utf8>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"block_string_leading_trailing_blank_lines_stripped_test"/utf8>>,
                            line => 540})
            end;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_leading_trailing_blank_lines_stripped_test"/utf8>>,
                    line => 542})
    end.

-file("test/lexer_test.gleam", 546).
-spec block_string_unterminated_is_error_test() -> nil.
block_string_unterminated_is_error_test() ->
    case tokens(<<"\"\"\"unterminated"/utf8>>) of
        {error, _} ->
            nil;

        Other ->
            erlang:error(#{gleam_error => panic,
                    message => inspect_error_or_ok(Other),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_unterminated_is_error_test"/utf8>>,
                    line => 549})
    end.

-file("test/lexer_test.gleam", 553).
-spec block_string_in_query_test() -> nil.
block_string_in_query_test() ->
    case tokens(<<"{ field(arg: \"\"\"block value\"\"\") { id } }"/utf8>>) of
        {ok, Ts} ->
            case gleam@list:any(
                Ts,
                fun(T) -> T =:= {string_value, <<"block value"/utf8>>} end
            ) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected StringValue(\"block value\") in token list"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lexer_test"/utf8>>,
                            function => <<"block_string_in_query_test"/utf8>>,
                            line => 558})
            end;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Lex error: "/utf8,
                        (inspect_error(E))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lexer_test"/utf8>>,
                    function => <<"block_string_in_query_test"/utf8>>,
                    line => 560})
    end.
