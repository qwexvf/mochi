-module(json_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/json_test.gleam").
-export([encode_null_test/0, encode_true_test/0, encode_false_test/0, encode_positive_int_test/0, encode_negative_int_test/0, encode_zero_test/0, encode_float_test/0, encode_negative_float_test/0, encode_simple_string_test/0, encode_empty_string_test/0, encode_string_with_quotes_test/0, encode_string_with_backslash_test/0, encode_string_with_newline_test/0, encode_string_with_tab_test/0, encode_string_with_carriage_return_test/0, encode_empty_list_test/0, encode_int_list_test/0, encode_string_list_test/0, encode_mixed_list_test/0, encode_nested_list_test/0, encode_empty_dict_test/0, encode_simple_dict_test/0, encode_dict_with_multiple_keys_test/0, encode_nested_dict_test/0, encode_pretty_simple_object_test/0, encode_pretty_empty_array_test/0, encode_pretty_empty_object_test/0, encode_pretty_array_with_indent_test/0, response_to_json_success_test/0, response_to_json_failure_test/0, response_to_json_partial_test/0, response_to_json_with_extensions_test/0, response_to_json_pretty_test/0, encode_complex_structure_test/0, encode_array_of_objects_test/0, encode_dict_direct_test/0, encode_list_direct_test/0, encode_string_value_test/0, encode_int_direct_test/0, encode_float_direct_test/0, encode_bool_true_direct_test/0, encode_bool_false_direct_test/0, encode_null_direct_test/0, error_with_path_json_test/0, error_with_location_json_test/0, error_with_extensions_json_test/0]).

-file("test/json_test.gleam", 14).
-spec encode_null_test() -> boolean().
encode_null_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(nil)),
    _assert_subject = Result =:= <<"null"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_null_test"/utf8>>,
                        line => 16,
                        value => _assert_fail,
                        start => 415,
                        'end' => 449,
                        pattern_start => 426,
                        pattern_end => 430})
    end.

-file("test/json_test.gleam", 19).
-spec encode_true_test() -> boolean().
encode_true_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(true)),
    _assert_subject = Result =:= <<"true"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_true_test"/utf8>>,
                        line => 21,
                        value => _assert_fail,
                        start => 534,
                        'end' => 568,
                        pattern_start => 545,
                        pattern_end => 549})
    end.

-file("test/json_test.gleam", 24).
-spec encode_false_test() -> boolean().
encode_false_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(false)),
    _assert_subject = Result =:= <<"false"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_false_test"/utf8>>,
                        line => 26,
                        value => _assert_fail,
                        start => 655,
                        'end' => 690,
                        pattern_start => 666,
                        pattern_end => 670})
    end.

-file("test/json_test.gleam", 29).
-spec encode_positive_int_test() -> boolean().
encode_positive_int_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(42)),
    _assert_subject = Result =:= <<"42"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_positive_int_test"/utf8>>,
                        line => 31,
                        value => _assert_fail,
                        start => 781,
                        'end' => 813,
                        pattern_start => 792,
                        pattern_end => 796})
    end.

-file("test/json_test.gleam", 34).
-spec encode_negative_int_test() -> boolean().
encode_negative_int_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(-123)),
    _assert_subject = Result =:= <<"-123"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_negative_int_test"/utf8>>,
                        line => 36,
                        value => _assert_fail,
                        start => 906,
                        'end' => 940,
                        pattern_start => 917,
                        pattern_end => 921})
    end.

-file("test/json_test.gleam", 39).
-spec encode_zero_test() -> boolean().
encode_zero_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(0)),
    _assert_subject = Result =:= <<"0"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_zero_test"/utf8>>,
                        line => 41,
                        value => _assert_fail,
                        start => 1022,
                        'end' => 1053,
                        pattern_start => 1033,
                        pattern_end => 1037})
    end.

-file("test/json_test.gleam", 44).
-spec encode_float_test() -> boolean().
encode_float_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(3.14)),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"3.14"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_float_test"/utf8>>,
                        line => 47,
                        value => _assert_fail,
                        start => 1193,
                        'end' => 1242,
                        pattern_start => 1204,
                        pattern_end => 1208})
    end.

-file("test/json_test.gleam", 50).
-spec encode_negative_float_test() -> boolean().
encode_negative_float_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(-2.5)),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"-2.5"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_negative_float_test"/utf8>>,
                        line => 52,
                        value => _assert_fail,
                        start => 1337,
                        'end' => 1386,
                        pattern_start => 1348,
                        pattern_end => 1352})
    end.

-file("test/json_test.gleam", 59).
-spec encode_simple_string_test() -> boolean().
encode_simple_string_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"hello"/utf8>>)),
    _assert_subject = Result =:= <<"\"hello\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_simple_string_test"/utf8>>,
                        line => 61,
                        value => _assert_fail,
                        start => 1660,
                        'end' => 1699,
                        pattern_start => 1671,
                        pattern_end => 1675})
    end.

-file("test/json_test.gleam", 64).
-spec encode_empty_string_test() -> boolean().
encode_empty_string_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<""/utf8>>)),
    _assert_subject = Result =:= <<"\"\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_empty_string_test"/utf8>>,
                        line => 66,
                        value => _assert_fail,
                        start => 1790,
                        'end' => 1824,
                        pattern_start => 1801,
                        pattern_end => 1805})
    end.

-file("test/json_test.gleam", 69).
-spec encode_string_with_quotes_test() -> boolean().
encode_string_with_quotes_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"say \"hello\""/utf8>>)),
    _assert_subject = Result =:= <<"\"say \\\"hello\\\"\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_with_quotes_test"/utf8>>,
                        line => 71,
                        value => _assert_fail,
                        start => 1934,
                        'end' => 1985,
                        pattern_start => 1945,
                        pattern_end => 1949})
    end.

-file("test/json_test.gleam", 74).
-spec encode_string_with_backslash_test() -> boolean().
encode_string_with_backslash_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"path\\to\\file"/utf8>>)),
    _assert_subject = Result =:= <<"\"path\\\\to\\\\file\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_with_backslash_test"/utf8>>,
                        line => 76,
                        value => _assert_fail,
                        start => 2099,
                        'end' => 2151,
                        pattern_start => 2110,
                        pattern_end => 2114})
    end.

-file("test/json_test.gleam", 79).
-spec encode_string_with_newline_test() -> boolean().
encode_string_with_newline_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"line1\nline2"/utf8>>)),
    _assert_subject = Result =:= <<"\"line1\\nline2\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_with_newline_test"/utf8>>,
                        line => 81,
                        value => _assert_fail,
                        start => 2261,
                        'end' => 2308,
                        pattern_start => 2272,
                        pattern_end => 2276})
    end.

-file("test/json_test.gleam", 84).
-spec encode_string_with_tab_test() -> boolean().
encode_string_with_tab_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"col1\tcol2"/utf8>>)),
    _assert_subject = Result =:= <<"\"col1\\tcol2\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_with_tab_test"/utf8>>,
                        line => 86,
                        value => _assert_fail,
                        start => 2412,
                        'end' => 2457,
                        pattern_start => 2423,
                        pattern_end => 2427})
    end.

-file("test/json_test.gleam", 89).
-spec encode_string_with_carriage_return_test() -> boolean().
encode_string_with_carriage_return_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(<<"line1\rline2"/utf8>>)),
    _assert_subject = Result =:= <<"\"line1\\rline2\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_with_carriage_return_test"/utf8>>,
                        line => 91,
                        value => _assert_fail,
                        start => 2575,
                        'end' => 2622,
                        pattern_start => 2586,
                        pattern_end => 2590})
    end.

-file("test/json_test.gleam", 98).
-spec encode_empty_list_test() -> boolean().
encode_empty_list_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity([])),
    _assert_subject = Result =:= <<"[]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_empty_list_test"/utf8>>,
                        line => 100,
                        value => _assert_fail,
                        start => 2892,
                        'end' => 2924,
                        pattern_start => 2903,
                        pattern_end => 2907})
    end.

-file("test/json_test.gleam", 103).
-spec encode_int_list_test() -> boolean().
encode_int_list_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity([1, 2, 3])),
    _assert_subject = Result =:= <<"[1,2,3]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_int_list_test"/utf8>>,
                        line => 105,
                        value => _assert_fail,
                        start => 3018,
                        'end' => 3055,
                        pattern_start => 3029,
                        pattern_end => 3033})
    end.

-file("test/json_test.gleam", 108).
-spec encode_string_list_test() -> boolean().
encode_string_list_test() ->
    Result = mochi@json:encode(
        gleam_stdlib:identity([<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>])
    ),
    _assert_subject = Result =:= <<"[\"a\",\"b\",\"c\"]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_list_test"/utf8>>,
                        line => 110,
                        value => _assert_fail,
                        start => 3158,
                        'end' => 3207,
                        pattern_start => 3169,
                        pattern_end => 3173})
    end.

-file("test/json_test.gleam", 113).
-spec encode_mixed_list_test() -> boolean().
encode_mixed_list_test() ->
    Items = [gleam_stdlib:identity(1),
        gleam_stdlib:identity(<<"two"/utf8>>),
        gleam_stdlib:identity(true)],
    Result = mochi@json:encode(gleam_stdlib:identity(Items)),
    _assert_subject = Result =:= <<"[1,\"two\",true]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_mixed_list_test"/utf8>>,
                        line => 121,
                        value => _assert_fail,
                        start => 3459,
                        'end' => 3505,
                        pattern_start => 3470,
                        pattern_end => 3474})
    end.

-file("test/json_test.gleam", 124).
-spec encode_nested_list_test() -> boolean().
encode_nested_list_test() ->
    Nested = [[1, 2], [3, 4]],
    Result = mochi@json:encode(gleam_stdlib:identity(Nested)),
    _assert_subject = Result =:= <<"[[1,2],[3,4]]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_nested_list_test"/utf8>>,
                        line => 127,
                        value => _assert_fail,
                        start => 3631,
                        'end' => 3674,
                        pattern_start => 3642,
                        pattern_end => 3646})
    end.

-file("test/json_test.gleam", 134).
-spec encode_empty_dict_test() -> boolean().
encode_empty_dict_test() ->
    Result = mochi@json:encode(gleam_stdlib:identity(maps:new())),
    _assert_subject = Result =:= <<"{}"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_empty_dict_test"/utf8>>,
                        line => 136,
                        value => _assert_fail,
                        start => 3953,
                        'end' => 3985,
                        pattern_start => 3964,
                        pattern_end => 3968})
    end.

-file("test/json_test.gleam", 139).
-spec encode_simple_dict_test() -> boolean().
encode_simple_dict_test() ->
    D = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"John"/utf8>>)}]
    ),
    Result = mochi@json:encode(gleam_stdlib:identity(D)),
    _assert_subject = Result =:= <<"{\"name\":\"John\"}"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_simple_dict_test"/utf8>>,
                        line => 142,
                        value => _assert_fail,
                        start => 4138,
                        'end' => 4187,
                        pattern_start => 4149,
                        pattern_end => 4153})
    end.

-file("test/json_test.gleam", 145).
-spec encode_dict_with_multiple_keys_test() -> boolean().
encode_dict_with_multiple_keys_test() ->
    D = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"John"/utf8>>)},
            {<<"age"/utf8>>, gleam_stdlib:identity(30)}]
    ),
    Result = mochi@json:encode(gleam_stdlib:identity(D)),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"name\":\"John\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(Result, <<"\"age\":30"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_dict_with_multiple_keys_test"/utf8>>,
                        line => 153,
                        value => _assert_fail,
                        start => 4470,
                        'end' => 4581,
                        pattern_start => 4481,
                        pattern_end => 4485})
    end.

-file("test/json_test.gleam", 158).
-spec encode_nested_dict_test() -> boolean().
encode_nested_dict_test() ->
    Inner = maps:from_list(
        [{<<"city"/utf8>>, gleam_stdlib:identity(<<"NYC"/utf8>>)}]
    ),
    Outer = maps:from_list([{<<"address"/utf8>>, gleam_stdlib:identity(Inner)}]),
    Result = mochi@json:encode(gleam_stdlib:identity(Outer)),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"address\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(Result, <<"\"city\":\"NYC\""/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_nested_dict_test"/utf8>>,
                        line => 162,
                        value => _assert_fail,
                        start => 4811,
                        'end' => 4922,
                        pattern_start => 4822,
                        pattern_end => 4826})
    end.

-file("test/json_test.gleam", 175).
-spec encode_pretty_simple_object_test() -> boolean().
encode_pretty_simple_object_test() ->
    D = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"John"/utf8>>)}]
    ),
    Result = mochi@json:encode_pretty(gleam_stdlib:identity(D), 2),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"\n"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_pretty_simple_object_test"/utf8>>,
                        line => 179,
                        value => _assert_fail,
                        start => 5549,
                        'end' => 5596,
                        pattern_start => 5560,
                        pattern_end => 5564})
    end.

-file("test/json_test.gleam", 182).
-spec encode_pretty_empty_array_test() -> boolean().
encode_pretty_empty_array_test() ->
    Result = mochi@json:encode_pretty(gleam_stdlib:identity([]), 2),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"["/utf8>>) andalso gleam_stdlib:contains_string(
        Result,
        <<"]"/utf8>>
    ),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_pretty_empty_array_test"/utf8>>,
                        line => 185,
                        value => _assert_fail,
                        start => 5756,
                        'end' => 5834,
                        pattern_start => 5767,
                        pattern_end => 5771})
    end.

-file("test/json_test.gleam", 188).
-spec encode_pretty_empty_object_test() -> boolean().
encode_pretty_empty_object_test() ->
    Result = mochi@json:encode_pretty(gleam_stdlib:identity(maps:new()), 2),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"{"/utf8>>) andalso gleam_stdlib:contains_string(
        Result,
        <<"}"/utf8>>
    ),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_pretty_empty_object_test"/utf8>>,
                        line => 191,
                        value => _assert_fail,
                        start => 6005,
                        'end' => 6083,
                        pattern_start => 6016,
                        pattern_end => 6020})
    end.

-file("test/json_test.gleam", 194).
-spec encode_pretty_array_with_indent_test() -> boolean().
encode_pretty_array_with_indent_test() ->
    Result = mochi@json:encode_pretty(gleam_stdlib:identity([1, 2, 3]), 4),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"    "/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_pretty_array_with_indent_test"/utf8>>,
                        line => 197,
                        value => _assert_fail,
                        start => 6238,
                        'end' => 6287,
                        pattern_start => 6249,
                        pattern_end => 6253})
    end.

-file("test/json_test.gleam", 204).
-spec response_to_json_success_test() -> boolean().
response_to_json_success_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"user"/utf8>>, gleam_stdlib:identity(<<"john"/utf8>>)}]
        )
    ),
    Resp = mochi@response:success(Data),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"\"data\""/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"response_to_json_success_test"/utf8>>,
                        line => 209,
                        value => _assert_fail,
                        start => 6684,
                        'end' => 6737,
                        pattern_start => 6695,
                        pattern_end => 6699})
    end.

-file("test/json_test.gleam", 212).
-spec response_to_json_failure_test() -> boolean().
response_to_json_failure_test() ->
    Errors = [mochi@error:error(<<"Something went wrong"/utf8>>)],
    Resp = mochi@response:failure(Errors),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"errors\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(
        Result,
        <<"Something went wrong"/utf8>>
    ),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"response_to_json_failure_test"/utf8>>,
                        line => 216,
                        value => _assert_fail,
                        start => 6913,
                        'end' => 7027,
                        pattern_start => 6924,
                        pattern_end => 6928})
    end.

-file("test/json_test.gleam", 221).
-spec response_to_json_partial_test() -> boolean().
response_to_json_partial_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list([{<<"partial"/utf8>>, gleam_stdlib:identity(true)}])
    ),
    Errors = [mochi@error:error(<<"Warning"/utf8>>)],
    Resp = mochi@response:partial(Data, Errors),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"\"data\""/utf8>>)
    andalso gleam_stdlib:contains_string(Result, <<"\"errors\""/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"response_to_json_partial_test"/utf8>>,
                        line => 227,
                        value => _assert_fail,
                        start => 7286,
                        'end' => 7384,
                        pattern_start => 7297,
                        pattern_end => 7301})
    end.

-file("test/json_test.gleam", 231).
-spec response_to_json_with_extensions_test() -> boolean().
response_to_json_with_extensions_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Resp = begin
        _pipe = mochi@response:success(Data),
        mochi@response:with_extension(
            _pipe,
            <<"requestId"/utf8>>,
            gleam_stdlib:identity(<<"req-123"/utf8>>)
        )
    end,
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"extensions\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(Result, <<"requestId"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"response_to_json_with_extensions_test"/utf8>>,
                        line => 237,
                        value => _assert_fail,
                        start => 7628,
                        'end' => 7735,
                        pattern_start => 7639,
                        pattern_end => 7643})
    end.

-file("test/json_test.gleam", 242).
-spec response_to_json_pretty_test() -> boolean().
response_to_json_pretty_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"user"/utf8>>, gleam_stdlib:identity(<<"john"/utf8>>)}]
        )
    ),
    Resp = mochi@response:success(Data),
    Result = mochi@response:to_json_pretty(Resp),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"\n"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"response_to_json_pretty_test"/utf8>>,
                        line => 247,
                        value => _assert_fail,
                        start => 7951,
                        'end' => 7998,
                        pattern_start => 7962,
                        pattern_end => 7966})
    end.

-file("test/json_test.gleam", 254).
-spec encode_complex_structure_test() -> boolean().
encode_complex_structure_test() ->
    User = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)},
            {<<"name"/utf8>>, gleam_stdlib:identity(<<"John Doe"/utf8>>)},
            {<<"age"/utf8>>, gleam_stdlib:identity(30)},
            {<<"active"/utf8>>, gleam_stdlib:identity(true)},
            {<<"email"/utf8>>,
                gleam_stdlib:identity(<<"john@example.com"/utf8>>)},
            {<<"phone"/utf8>>, gleam_stdlib:identity(nil)},
            {<<"roles"/utf8>>,
                gleam_stdlib:identity([<<"admin"/utf8>>, <<"user"/utf8>>])}]
    ),
    Result = mochi@json:encode(gleam_stdlib:identity(User)),
    _assert_subject = ((((gleam_stdlib:contains_string(
        Result,
        <<"\"id\":\"1\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(
        Result,
        <<"\"name\":\"John Doe\""/utf8>>
    ))
    andalso gleam_stdlib:contains_string(Result, <<"\"age\":30"/utf8>>))
    andalso gleam_stdlib:contains_string(Result, <<"\"active\":true"/utf8>>))
    andalso gleam_stdlib:contains_string(
        Result,
        <<"\"email\":\"john@example.com\""/utf8>>
    ))
    andalso gleam_stdlib:contains_string(Result, <<"\"roles\""/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_complex_structure_test"/utf8>>,
                        line => 268,
                        value => _assert_fail,
                        start => 8695,
                        'end' => 9016,
                        pattern_start => 8706,
                        pattern_end => 8710})
    end.

-file("test/json_test.gleam", 277).
-spec encode_array_of_objects_test() -> boolean().
encode_array_of_objects_test() ->
    Users = [maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)},
                {<<"name"/utf8>>, gleam_stdlib:identity(<<"Alice"/utf8>>)}]
        ),
        maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(<<"2"/utf8>>)},
                {<<"name"/utf8>>, gleam_stdlib:identity(<<"Bob"/utf8>>)}]
        )],
    Result = mochi@json:encode(gleam_stdlib:identity(Users)),
    _assert_subject = ((gleam_stdlib:contains_string(Result, <<"Alice"/utf8>>)
    andalso gleam_stdlib:contains_string(Result, <<"Bob"/utf8>>))
    andalso gleam_stdlib:string_starts_with(Result, <<"["/utf8>>))
    andalso gleam_stdlib:string_ends_with(Result, <<"]"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_array_of_objects_test"/utf8>>,
                        line => 289,
                        value => _assert_fail,
                        start => 9354,
                        'end' => 9522,
                        pattern_start => 9365,
                        pattern_end => 9369})
    end.

-file("test/json_test.gleam", 300).
-spec encode_dict_direct_test() -> boolean().
encode_dict_direct_test() ->
    D = maps:from_list(
        [{<<"key"/utf8>>, gleam_stdlib:identity(<<"value"/utf8>>)}]
    ),
    Result = mochi@json:encode_dict(D),
    _assert_subject = Result =:= <<"{\"key\":\"value\"}"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_dict_direct_test"/utf8>>,
                        line => 303,
                        value => _assert_fail,
                        start => 9853,
                        'end' => 9902,
                        pattern_start => 9864,
                        pattern_end => 9868})
    end.

-file("test/json_test.gleam", 306).
-spec encode_list_direct_test() -> boolean().
encode_list_direct_test() ->
    Items = [gleam_stdlib:identity(1),
        gleam_stdlib:identity(2),
        gleam_stdlib:identity(3)],
    Result = mochi@json:encode_list(Items),
    _assert_subject = Result =:= <<"[1,2,3]"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_list_direct_test"/utf8>>,
                        line => 309,
                        value => _assert_fail,
                        start => 10060,
                        'end' => 10097,
                        pattern_start => 10071,
                        pattern_end => 10075})
    end.

-file("test/json_test.gleam", 312).
-spec encode_string_value_test() -> boolean().
encode_string_value_test() ->
    Result = mochi@json:encode_string_value(<<"test"/utf8>>),
    _assert_subject = Result =:= <<"\"test\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_string_value_test"/utf8>>,
                        line => 314,
                        value => _assert_fail,
                        start => 10187,
                        'end' => 10225,
                        pattern_start => 10198,
                        pattern_end => 10202})
    end.

-file("test/json_test.gleam", 317).
-spec encode_int_direct_test() -> boolean().
encode_int_direct_test() ->
    Result = mochi@json:encode_int(42),
    _assert_subject = Result =:= <<"42"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_int_direct_test"/utf8>>,
                        line => 319,
                        value => _assert_fail,
                        start => 10300,
                        'end' => 10332,
                        pattern_start => 10311,
                        pattern_end => 10315})
    end.

-file("test/json_test.gleam", 322).
-spec encode_float_direct_test() -> boolean().
encode_float_direct_test() ->
    Result = mochi@json:encode_float_value(3.14),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"3.14"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_float_direct_test"/utf8>>,
                        line => 324,
                        value => _assert_fail,
                        start => 10419,
                        'end' => 10468,
                        pattern_start => 10430,
                        pattern_end => 10434})
    end.

-file("test/json_test.gleam", 327).
-spec encode_bool_true_direct_test() -> boolean().
encode_bool_true_direct_test() ->
    Result = mochi@json:encode_bool(true),
    _assert_subject = Result =:= <<"true"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_bool_true_direct_test"/utf8>>,
                        line => 329,
                        value => _assert_fail,
                        start => 10552,
                        'end' => 10586,
                        pattern_start => 10563,
                        pattern_end => 10567})
    end.

-file("test/json_test.gleam", 332).
-spec encode_bool_false_direct_test() -> boolean().
encode_bool_false_direct_test() ->
    Result = mochi@json:encode_bool(false),
    _assert_subject = Result =:= <<"false"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_bool_false_direct_test"/utf8>>,
                        line => 334,
                        value => _assert_fail,
                        start => 10672,
                        'end' => 10707,
                        pattern_start => 10683,
                        pattern_end => 10687})
    end.

-file("test/json_test.gleam", 337).
-spec encode_null_direct_test() -> boolean().
encode_null_direct_test() ->
    Result = mochi@json:encode_null(),
    _assert_subject = Result =:= <<"null"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"encode_null_direct_test"/utf8>>,
                        line => 339,
                        value => _assert_fail,
                        start => 10782,
                        'end' => 10816,
                        pattern_start => 10793,
                        pattern_end => 10797})
    end.

-file("test/json_test.gleam", 346).
-spec error_with_path_json_test() -> boolean().
error_with_path_json_test() ->
    Err = mochi@error:error_at(
        <<"Field not found"/utf8>>,
        [<<"user"/utf8>>, <<"email"/utf8>>]
    ),
    Errors = [Err],
    Resp = mochi@response:failure(Errors),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(Result, <<"\"path\""/utf8>>)
    andalso gleam_stdlib:contains_string(Result, <<"\"user\""/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"error_with_path_json_test"/utf8>>,
                        line => 351,
                        value => _assert_fail,
                        start => 11211,
                        'end' => 11307,
                        pattern_start => 11222,
                        pattern_end => 11226})
    end.

-file("test/json_test.gleam", 355).
-spec error_with_location_json_test() -> boolean().
error_with_location_json_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Syntax error"/utf8>>),
        mochi@error:at_location(_pipe, 10, 5)
    end,
    Errors = [Err],
    Resp = mochi@response:failure(Errors),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"locations\""/utf8>>
    ),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"error_with_location_json_test"/utf8>>,
                        line => 362,
                        value => _assert_fail,
                        start => 11527,
                        'end' => 11585,
                        pattern_start => 11538,
                        pattern_end => 11542})
    end.

-file("test/json_test.gleam", 365).
-spec error_with_extensions_json_test() -> boolean().
error_with_extensions_json_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Custom error"/utf8>>),
        mochi@error:with_code(_pipe, <<"CUSTOM_ERROR"/utf8>>)
    end,
    Errors = [Err],
    Resp = mochi@response:failure(Errors),
    Result = mochi@response:to_json(Resp),
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"extensions\""/utf8>>
    )
    andalso gleam_stdlib:contains_string(Result, <<"CUSTOM_ERROR"/utf8>>),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"json_test"/utf8>>,
                        function => <<"error_with_extensions_json_test"/utf8>>,
                        line => 372,
                        value => _assert_fail,
                        start => 11814,
                        'end' => 11924,
                        pattern_start => 11825,
                        pattern_end => 11829})
    end.
