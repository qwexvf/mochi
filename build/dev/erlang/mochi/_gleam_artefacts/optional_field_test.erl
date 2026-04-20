-module(optional_field_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/optional_field_test.gleam").
-export([optional_string_some_serializes_as_string_test/0, optional_string_none_serializes_as_null_test/0, optional_int_some_serializes_as_int_test/0, optional_int_none_serializes_as_null_test/0, optional_float_some_serializes_as_float_test/0, optional_bool_some_serializes_as_bool_test/0, nullable_decoder_handles_null_round_trip_test/0, nullable_decoder_handles_some_round_trip_test/0, nullable_decoder_handles_mixed_round_trip_test/0, record_with_optional_fields_serializes_correctly_test/0]).
-export_type([profile/0]).

-type profile() :: {profile,
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        gleam@option:option(float()),
        gleam@option:option(boolean())}.

-file("test/optional_field_test.gleam", 24).
-spec profile_extractor() -> fun((profile()) -> gleam@dynamic:dynamic_()).
profile_extractor() ->
    fun(P) ->
        mochi@types:record(
            [mochi@types:field(<<"id"/utf8>>, erlang:element(2, P)),
                {<<"name"/utf8>>, mochi@types:option(erlang:element(3, P))},
                {<<"age"/utf8>>, mochi@types:option(erlang:element(4, P))},
                {<<"score"/utf8>>, mochi@types:option(erlang:element(5, P))},
                {<<"active"/utf8>>, mochi@types:option(erlang:element(6, P))}]
        )
    end.

-file("test/optional_field_test.gleam", 40).
-spec optional_string_some_serializes_as_string_test() -> boolean().
optional_string_some_serializes_as_string_test() ->
    Dyn = mochi@types:option({some, <<"Alice"/utf8>>}),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"\"Alice\""/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_string_some_serializes_as_string_test"/utf8>>,
                        line => 42,
                        value => _assert_fail,
                        start => 1251,
                        'end' => 1300,
                        pattern_start => 1262,
                        pattern_end => 1266})
    end.

-file("test/optional_field_test.gleam", 45).
-spec optional_string_none_serializes_as_null_test() -> boolean().
optional_string_none_serializes_as_null_test() ->
    Opt = none,
    Dyn = mochi@types:option(Opt),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"null"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_string_none_serializes_as_null_test"/utf8>>,
                        line => 48,
                        value => _assert_fail,
                        start => 1425,
                        'end' => 1469,
                        pattern_start => 1436,
                        pattern_end => 1440})
    end.

-file("test/optional_field_test.gleam", 51).
-spec optional_int_some_serializes_as_int_test() -> boolean().
optional_int_some_serializes_as_int_test() ->
    Dyn = mochi@types:option({some, 42}),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"42"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_int_some_serializes_as_int_test"/utf8>>,
                        line => 53,
                        value => _assert_fail,
                        start => 1562,
                        'end' => 1604,
                        pattern_start => 1573,
                        pattern_end => 1577})
    end.

-file("test/optional_field_test.gleam", 56).
-spec optional_int_none_serializes_as_null_test() -> boolean().
optional_int_none_serializes_as_null_test() ->
    Opt = none,
    Dyn = mochi@types:option(Opt),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"null"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_int_none_serializes_as_null_test"/utf8>>,
                        line => 59,
                        value => _assert_fail,
                        start => 1723,
                        'end' => 1767,
                        pattern_start => 1734,
                        pattern_end => 1738})
    end.

-file("test/optional_field_test.gleam", 62).
-spec optional_float_some_serializes_as_float_test() -> boolean().
optional_float_some_serializes_as_float_test() ->
    Dyn = mochi@types:option({some, 3.14}),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"3.14"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_float_some_serializes_as_float_test"/utf8>>,
                        line => 64,
                        value => _assert_fail,
                        start => 1866,
                        'end' => 1910,
                        pattern_start => 1877,
                        pattern_end => 1881})
    end.

-file("test/optional_field_test.gleam", 67).
-spec optional_bool_some_serializes_as_bool_test() -> boolean().
optional_bool_some_serializes_as_bool_test() ->
    Dyn = mochi@types:option({some, true}),
    _assert_subject = mochi@json:encode(Dyn) =:= <<"true"/utf8>>,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"optional_bool_some_serializes_as_bool_test"/utf8>>,
                        line => 69,
                        value => _assert_fail,
                        start => 2007,
                        'end' => 2051,
                        pattern_start => 2018,
                        pattern_end => 2022})
    end.

-file("test/optional_field_test.gleam", 76).
-spec decode_profile(gleam@dynamic:dynamic_()) -> {ok, profile()} |
    {error, binary()}.
decode_profile(Dyn) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"id"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Id) ->
                gleam@dynamic@decode:optional_field(
                    <<"name"/utf8>>,
                    none,
                    mochi@types:nullable(
                        {decoder, fun gleam@dynamic@decode:decode_string/1}
                    ),
                    fun(Name) ->
                        gleam@dynamic@decode:optional_field(
                            <<"age"/utf8>>,
                            none,
                            mochi@types:nullable(
                                {decoder, fun gleam@dynamic@decode:decode_int/1}
                            ),
                            fun(Age) ->
                                gleam@dynamic@decode:optional_field(
                                    <<"score"/utf8>>,
                                    none,
                                    mochi@types:nullable(
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_float/1}
                                    ),
                                    fun(Score) ->
                                        gleam@dynamic@decode:optional_field(
                                            <<"active"/utf8>>,
                                            none,
                                            mochi@types:nullable(
                                                {decoder,
                                                    fun gleam@dynamic@decode:decode_bool/1}
                                            ),
                                            fun(Active) ->
                                                gleam@dynamic@decode:success(
                                                    {profile,
                                                        Id,
                                                        Name,
                                                        Age,
                                                        Score,
                                                        Active}
                                                )
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    case gleam@dynamic@decode:run(Dyn, Decoder) of
        {ok, P} ->
            {ok, P};

        {error, _} ->
            {error, <<"decode failed"/utf8>>}
    end.

-file("test/optional_field_test.gleam", 103).
-spec nullable_decoder_handles_null_round_trip_test() -> boolean().
nullable_decoder_handles_null_round_trip_test() ->
    P = {profile, <<"u1"/utf8>>, none, none, none, none},
    Encoded = (profile_extractor())(P),
    Decoded@1 = case decode_profile(Encoded) of
        {ok, Decoded} -> Decoded;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_null_round_trip_test"/utf8>>,
                        line => 106,
                        value => _assert_fail,
                        start => 3155,
                        'end' => 3203,
                        pattern_start => 3166,
                        pattern_end => 3177})
    end,
    _assert_subject = Decoded@1 =:= P,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_null_round_trip_test"/utf8>>,
                        line => 107,
                        value => _assert_fail@1,
                        start => 3206,
                        'end' => 3236,
                        pattern_start => 3217,
                        pattern_end => 3221})
    end.

-file("test/optional_field_test.gleam", 110).
-spec nullable_decoder_handles_some_round_trip_test() -> boolean().
nullable_decoder_handles_some_round_trip_test() ->
    P = {profile,
        <<"u1"/utf8>>,
        {some, <<"Alice"/utf8>>},
        {some, 30},
        {some, 99.5},
        {some, true}},
    Encoded = (profile_extractor())(P),
    Decoded@1 = case decode_profile(Encoded) of
        {ok, Decoded} -> Decoded;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_some_round_trip_test"/utf8>>,
                        line => 120,
                        value => _assert_fail,
                        start => 3482,
                        'end' => 3530,
                        pattern_start => 3493,
                        pattern_end => 3504})
    end,
    _assert_subject = Decoded@1 =:= P,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_some_round_trip_test"/utf8>>,
                        line => 121,
                        value => _assert_fail@1,
                        start => 3533,
                        'end' => 3563,
                        pattern_start => 3544,
                        pattern_end => 3548})
    end.

-file("test/optional_field_test.gleam", 124).
-spec nullable_decoder_handles_mixed_round_trip_test() -> boolean().
nullable_decoder_handles_mixed_round_trip_test() ->
    P = {profile,
        <<"u1"/utf8>>,
        {some, <<"Alice"/utf8>>},
        none,
        {some, 99.5},
        none},
    Encoded = (profile_extractor())(P),
    Decoded@1 = case decode_profile(Encoded) of
        {ok, Decoded} -> Decoded;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_mixed_round_trip_test"/utf8>>,
                        line => 134,
                        value => _assert_fail,
                        start => 3800,
                        'end' => 3848,
                        pattern_start => 3811,
                        pattern_end => 3822})
    end,
    _assert_subject = Decoded@1 =:= P,
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"nullable_decoder_handles_mixed_round_trip_test"/utf8>>,
                        line => 135,
                        value => _assert_fail@1,
                        start => 3851,
                        'end' => 3881,
                        pattern_start => 3862,
                        pattern_end => 3866})
    end.

-file("test/optional_field_test.gleam", 142).
-spec record_with_optional_fields_serializes_correctly_test() -> boolean().
record_with_optional_fields_serializes_correctly_test() ->
    None_int = none,
    Dyn = mochi@types:record(
        [mochi@types:field(<<"id"/utf8>>, <<"u1"/utf8>>),
            {<<"name"/utf8>>, mochi@types:option({some, <<"Alice"/utf8>>})},
            {<<"age"/utf8>>, mochi@types:option(None_int)}]
    ),
    Result = mochi@json:encode(Dyn),
    case gleam_stdlib:contains_string(Result, <<"\"name\":\"Alice\""/utf8>>) of
        true -> nil;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"record_with_optional_fields_serializes_correctly_test"/utf8>>,
                        line => 152,
                        value => _assert_fail,
                        start => 4478,
                        'end' => 4541,
                        pattern_start => 4489,
                        pattern_end => 4493})
    end,
    _assert_subject = gleam_stdlib:contains_string(
        Result,
        <<"\"age\":null"/utf8>>
    ),
    case _assert_subject of
        true -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"optional_field_test"/utf8>>,
                        function => <<"record_with_optional_fields_serializes_correctly_test"/utf8>>,
                        line => 153,
                        value => _assert_fail@1,
                        start => 4544,
                        'end' => 4601,
                        pattern_start => 4555,
                        pattern_end => 4559})
    end.
