-module(mochi@scalars).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/scalars.gleam").
-export([date_time/0, date/0, json/0, email/0, url/0, uuid/0]).

-file("src/mochi/scalars.gleam", 10).
-spec validate_string(
    gleam@dynamic:dynamic_(),
    fun((binary()) -> {ok, binary()} | {error, binary()})
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
validate_string(Value, Validate) ->
    gleam@result:'try'(
        begin
            _pipe = gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ),
            gleam@result:map_error(
                _pipe,
                fun(_) -> <<"Expected a string value"/utf8>> end
            )
        end,
        fun(S) ->
            gleam@result:map(
                Validate(S),
                fun(Validated) -> gleam_stdlib:identity(Validated) end
            )
        end
    ).

-file("src/mochi/scalars.gleam", 22).
-spec is_digit_char(binary()) -> boolean().
is_digit_char(C) ->
    case C of
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

-file("src/mochi/scalars.gleam", 29).
-spec date_time() -> mochi@schema:scalar_type().
date_time() ->
    Validate = fun(Value) ->
        validate_string(
            Value,
            fun(S) ->
                Valid = ((string:length(S) > 0) andalso (gleam_stdlib:contains_string(
                    S,
                    <<"T"/utf8>>
                )
                orelse (string:length(S) =:= 10)))
                andalso begin
                    _pipe = gleam@string:first(S),
                    _pipe@1 = gleam@result:map(_pipe, fun is_digit_char/1),
                    gleam@result:unwrap(_pipe@1, false)
                end,
                case Valid of
                    true ->
                        {ok, S};

                    false ->
                        {error,
                            <<"DateTime must be a valid ISO 8601 string"/utf8>>}
                end
            end
        )
    end,
    _pipe@2 = mochi@schema:scalar(<<"DateTime"/utf8>>),
    _pipe@3 = mochi@schema:scalar_description(
        _pipe@2,
        <<"ISO 8601 date-time scalar"/utf8>>
    ),
    _pipe@4 = mochi@schema:serialize(_pipe@3, fun(Value@1) -> {ok, Value@1} end),
    _pipe@5 = mochi@schema:parse_value(_pipe@4, Validate),
    mochi@schema:parse_literal(_pipe@5, Validate).

-file("src/mochi/scalars.gleam", 81).
-spec drop(list(binary()), integer()) -> list(binary()).
drop(Chars, N) ->
    case {N, Chars} of
        {0, Rest} ->
            Rest;

        {_, []} ->
            [];

        {N@1, [_ | Rest@1]} ->
            drop(Rest@1, N@1 - 1)
    end.

-file("src/mochi/scalars.gleam", 89).
-spec first_char(list(binary())) -> {ok, binary()} | {error, nil}.
first_char(Chars) ->
    case Chars of
        [C | _] ->
            {ok, C};

        [] ->
            {error, nil}
    end.

-file("src/mochi/scalars.gleam", 51).
-spec date() -> mochi@schema:scalar_type().
date() ->
    Validate = fun(Value) ->
        validate_string(
            Value,
            fun(S) ->
                Chars = gleam@string:to_graphemes(S),
                case string:length(S) =:= 10 of
                    false ->
                        {error, <<"Date must be in YYYY-MM-DD format"/utf8>>};

                    true ->
                        Dash4 = case begin
                            _pipe = Chars,
                            _pipe@1 = drop(_pipe, 4),
                            first_char(_pipe@1)
                        end of
                            {ok, C} ->
                                C =:= <<"-"/utf8>>;

                            {error, _} ->
                                false
                        end,
                        Dash7 = case begin
                            _pipe@2 = Chars,
                            _pipe@3 = drop(_pipe@2, 7),
                            first_char(_pipe@3)
                        end of
                            {ok, C@1} ->
                                C@1 =:= <<"-"/utf8>>;

                            {error, _} ->
                                false
                        end,
                        case Dash4 andalso Dash7 of
                            true ->
                                {ok, S};

                            false ->
                                {error,
                                    <<"Date must be in YYYY-MM-DD format"/utf8>>}
                        end
                end
            end
        )
    end,
    _pipe@4 = mochi@schema:scalar(<<"Date"/utf8>>),
    _pipe@5 = mochi@schema:scalar_description(
        _pipe@4,
        <<"ISO 8601 date scalar (YYYY-MM-DD)"/utf8>>
    ),
    _pipe@6 = mochi@schema:serialize(_pipe@5, fun(Value@1) -> {ok, Value@1} end),
    _pipe@7 = mochi@schema:parse_value(_pipe@6, Validate),
    mochi@schema:parse_literal(_pipe@7, Validate).

-file("src/mochi/scalars.gleam", 96).
-spec json() -> mochi@schema:scalar_type().
json() ->
    _pipe = mochi@schema:scalar(<<"JSON"/utf8>>),
    _pipe@1 = mochi@schema:scalar_description(
        _pipe,
        <<"Arbitrary JSON value"/utf8>>
    ),
    _pipe@2 = mochi@schema:serialize(_pipe@1, fun(Value) -> {ok, Value} end),
    _pipe@3 = mochi@schema:parse_value(
        _pipe@2,
        fun(Value@1) -> {ok, Value@1} end
    ),
    mochi@schema:parse_literal(_pipe@3, fun(Value@2) -> {ok, Value@2} end).

-file("src/mochi/scalars.gleam", 104).
-spec email() -> mochi@schema:scalar_type().
email() ->
    Validate = fun(Value) ->
        validate_string(
            Value,
            fun(S) ->
                Parts = gleam@string:split(S, <<"@"/utf8>>),
                case Parts of
                    [_, Domain] ->
                        case gleam_stdlib:contains_string(Domain, <<"."/utf8>>) of
                            true ->
                                {ok, S};

                            false ->
                                {error,
                                    <<"Email must contain a domain with a dot"/utf8>>}
                        end;

                    _ ->
                        {error, <<"Email must contain exactly one @"/utf8>>}
                end
            end
        )
    end,
    _pipe = mochi@schema:scalar(<<"Email"/utf8>>),
    _pipe@1 = mochi@schema:scalar_description(
        _pipe,
        <<"Email address scalar"/utf8>>
    ),
    _pipe@2 = mochi@schema:serialize(_pipe@1, fun(Value@1) -> {ok, Value@1} end),
    _pipe@3 = mochi@schema:parse_value(_pipe@2, Validate),
    mochi@schema:parse_literal(_pipe@3, Validate).

-file("src/mochi/scalars.gleam", 125).
-spec url() -> mochi@schema:scalar_type().
url() ->
    Validate = fun(Value) ->
        validate_string(
            Value,
            fun(S) ->
                case gleam_stdlib:string_starts_with(S, <<"http://"/utf8>>)
                orelse gleam_stdlib:string_starts_with(S, <<"https://"/utf8>>) of
                    true ->
                        {ok, S};

                    false ->
                        {error,
                            <<"URL must start with http:// or https://"/utf8>>}
                end
            end
        )
    end,
    _pipe = mochi@schema:scalar(<<"URL"/utf8>>),
    _pipe@1 = mochi@schema:scalar_description(_pipe, <<"URL scalar"/utf8>>),
    _pipe@2 = mochi@schema:serialize(_pipe@1, fun(Value@1) -> {ok, Value@1} end),
    _pipe@3 = mochi@schema:parse_value(_pipe@2, Validate),
    mochi@schema:parse_literal(_pipe@3, Validate).

-file("src/mochi/scalars.gleam", 143).
-spec uuid() -> mochi@schema:scalar_type().
uuid() ->
    Validate = fun(Value) ->
        validate_string(
            Value,
            fun(S) ->
                Chars = gleam@string:to_graphemes(S),
                D8 = case begin
                    _pipe = Chars,
                    _pipe@1 = drop(_pipe, 8),
                    first_char(_pipe@1)
                end of
                    {ok, C} ->
                        C =:= <<"-"/utf8>>;

                    {error, _} ->
                        false
                end,
                D13 = case begin
                    _pipe@2 = Chars,
                    _pipe@3 = drop(_pipe@2, 13),
                    first_char(_pipe@3)
                end of
                    {ok, C@1} ->
                        C@1 =:= <<"-"/utf8>>;

                    {error, _} ->
                        false
                end,
                D18 = case begin
                    _pipe@4 = Chars,
                    _pipe@5 = drop(_pipe@4, 18),
                    first_char(_pipe@5)
                end of
                    {ok, C@2} ->
                        C@2 =:= <<"-"/utf8>>;

                    {error, _} ->
                        false
                end,
                D23 = case begin
                    _pipe@6 = Chars,
                    _pipe@7 = drop(_pipe@6, 23),
                    first_char(_pipe@7)
                end of
                    {ok, C@3} ->
                        C@3 =:= <<"-"/utf8>>;

                    {error, _} ->
                        false
                end,
                case ((((string:length(S) =:= 36) andalso D8) andalso D13)
                andalso D18)
                andalso D23 of
                    true ->
                        {ok, S};

                    false ->
                        {error,
                            <<"UUID must be 36 characters with dashes at positions 8, 13, 18, 23"/utf8>>}
                end
            end
        )
    end,
    _pipe@8 = mochi@schema:scalar(<<"UUID"/utf8>>),
    _pipe@9 = mochi@schema:scalar_description(_pipe@8, <<"UUID scalar"/utf8>>),
    _pipe@10 = mochi@schema:serialize(
        _pipe@9,
        fun(Value@1) -> {ok, Value@1} end
    ),
    _pipe@11 = mochi@schema:parse_value(_pipe@10, Validate),
    mochi@schema:parse_literal(_pipe@11, Validate).
