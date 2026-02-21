-module(birl@duration).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birl/duration.gleam").
-export([add/2, subtract/2, scale_up/2, scale_down/2, micro_seconds/1, compare/2, milli_seconds/1, seconds/1, minutes/1, hours/1, days/1, weeks/1, months/1, years/1, new/1, decompose/1, blur_to/2, blur/1, accurate_new/1, accurate_decompose/1, parse/1]).
-export_type([duration/0, unit/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type duration() :: {duration, integer()}.

-type unit() :: micro_second |
    milli_second |
    second |
    minute |
    hour |
    day |
    week |
    month |
    year.

-file("src/birl/duration.gleam", 25).
-spec add(duration(), duration()) -> duration().
add(A, B) ->
    {duration, A@1} = A,
    {duration, B@1} = B,
    {duration, A@1 + B@1}.

-file("src/birl/duration.gleam", 31).
-spec subtract(duration(), duration()) -> duration().
subtract(A, B) ->
    {duration, A@1} = A,
    {duration, B@1} = B,
    {duration, A@1 - B@1}.

-file("src/birl/duration.gleam", 37).
-spec scale_up(duration(), integer()) -> duration().
scale_up(Value, Factor) ->
    {duration, Value@1} = Value,
    {duration, Value@1 * Factor}.

-file("src/birl/duration.gleam", 42).
-spec scale_down(duration(), integer()) -> duration().
scale_down(Value, Factor) ->
    {duration, Value@1} = Value,
    {duration, case Factor of
            0 -> 0;
            Gleam@denominator -> Value@1 div Gleam@denominator
        end}.

-file("src/birl/duration.gleam", 47).
-spec micro_seconds(integer()) -> duration().
micro_seconds(Value) ->
    {duration, Value}.

-file("src/birl/duration.gleam", 83).
-spec compare(duration(), duration()) -> gleam@order:order().
compare(A, B) ->
    {duration, Dta} = A,
    {duration, Dtb} = B,
    case {Dta =:= Dtb, Dta < Dtb} of
        {true, _} ->
            eq;

        {_, true} ->
            lt;

        {_, false} ->
            gt
    end.

-file("src/birl/duration.gleam", 385).
-spec extract(integer(), integer()) -> {integer(), integer()}.
extract(Duration, Unit_value) ->
    {case Unit_value of
            0 -> 0;
            Gleam@denominator -> Duration div Gleam@denominator
        end, case Unit_value of
            0 -> 0;
            Gleam@denominator@1 -> Duration rem Gleam@denominator@1
        end}.

-file("src/birl/duration.gleam", 51).
-spec milli_seconds(integer()) -> duration().
milli_seconds(Value) ->
    {duration, Value * 1000}.

-file("src/birl/duration.gleam", 55).
-spec seconds(integer()) -> duration().
seconds(Value) ->
    {duration, Value * 1000000}.

-file("src/birl/duration.gleam", 59).
-spec minutes(integer()) -> duration().
minutes(Value) ->
    {duration, Value * 60000000}.

-file("src/birl/duration.gleam", 63).
-spec hours(integer()) -> duration().
hours(Value) ->
    {duration, Value * 3600000000}.

-file("src/birl/duration.gleam", 67).
-spec days(integer()) -> duration().
days(Value) ->
    {duration, Value * 86400000000}.

-file("src/birl/duration.gleam", 71).
-spec weeks(integer()) -> duration().
weeks(Value) ->
    {duration, Value * 604800000000}.

-file("src/birl/duration.gleam", 75).
-spec months(integer()) -> duration().
months(Value) ->
    {duration, Value * 2592000000000}.

-file("src/birl/duration.gleam", 79).
-spec years(integer()) -> duration().
years(Value) ->
    {duration, Value * 31536000000000}.

-file("src/birl/duration.gleam", 95).
?DOC(" use this if you need short durations where a year just means 365 days and a month just means 30 days\n").
-spec new(list({integer(), unit()})) -> duration().
new(Values) ->
    _pipe = Values,
    _pipe@1 = gleam@list:fold(_pipe, 0, fun(Total, Current) -> case Current of
                {Amount, micro_second} ->
                    Total + Amount;

                {Amount@1, milli_second} ->
                    Total + (Amount@1 * 1000);

                {Amount@2, second} ->
                    Total + (Amount@2 * 1000000);

                {Amount@3, minute} ->
                    Total + (Amount@3 * 60000000);

                {Amount@4, hour} ->
                    Total + (Amount@4 * 3600000000);

                {Amount@5, day} ->
                    Total + (Amount@5 * 86400000000);

                {Amount@6, week} ->
                    Total + (Amount@6 * 604800000000);

                {Amount@7, month} ->
                    Total + (Amount@7 * 2592000000000);

                {Amount@8, year} ->
                    Total + (Amount@8 * 31536000000000)
            end end),
    {duration, _pipe@1}.

-file("src/birl/duration.gleam", 133).
?DOC(" use this if you need short durations where a year just means 365 days and a month just means 30 days\n").
-spec decompose(duration()) -> list({integer(), unit()}).
decompose(Duration) ->
    {duration, Value} = Duration,
    Absolute_value = gleam@int:absolute_value(Value),
    {Years, Remaining} = extract(Absolute_value, 31536000000000),
    {Months, Remaining@1} = extract(Remaining, 2592000000000),
    {Weeks, Remaining@2} = extract(Remaining@1, 604800000000),
    {Days, Remaining@3} = extract(Remaining@2, 86400000000),
    {Hours, Remaining@4} = extract(Remaining@3, 3600000000),
    {Minutes, Remaining@5} = extract(Remaining@4, 60000000),
    {Seconds, Remaining@6} = extract(Remaining@5, 1000000),
    {Milli_seconds, Remaining@7} = extract(Remaining@6, 1000),
    _pipe = [{Years, year},
        {Months, month},
        {Weeks, week},
        {Days, day},
        {Hours, hour},
        {Minutes, minute},
        {Seconds, second},
        {Milli_seconds, milli_second},
        {Remaining@7, micro_second}],
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Item) -> erlang:element(1, Item) > 0 end
    ),
    gleam@list:map(_pipe@1, fun(Item@1) -> case Value < 0 of
                true ->
                    {-1 * erlang:element(1, Item@1), erlang:element(2, Item@1)};

                false ->
                    Item@1
            end end).

-file("src/birl/duration.gleam", 243).
-spec unit_values(unit()) -> integer().
unit_values(Unit) ->
    case Unit of
        year ->
            31536000000000;

        month ->
            2592000000000;

        week ->
            604800000000;

        day ->
            86400000000;

        hour ->
            3600000000;

        minute ->
            60000000;

        second ->
            1000000;

        milli_second ->
            1000;

        micro_second ->
            1
    end.

-file("src/birl/duration.gleam", 206).
?DOC(
    " approximates the duration by only the given unit\n"
    " \n"
    " if the duration is not an integer multiple of the unit,\n"
    " the remainder will be disgarded if it's less than two thirds of the unit,\n"
    " otherwise a single unit will be added to the multiplier.\n"
    " \n"
    "   - `blur_to(days(16), Month)` ->  `0`\n"
    "   - `blur_to(days(20), Month)` ->  `1`\n"
).
-spec blur_to(duration(), unit()) -> integer().
blur_to(Duration, Unit) ->
    Unit_value = unit_values(Unit),
    {duration, Value} = Duration,
    {Unit_counts, Remaining} = extract(Value, Unit_value),
    case Remaining >= ((Unit_value * 2) div 3) of
        true ->
            Unit_counts + 1;

        false ->
            Unit_counts
    end.

-file("src/birl/duration.gleam", 257).
-spec inner_blur(list({integer(), unit()})) -> {integer(), unit()}.
inner_blur(Values) ->
    case Values of
        [] ->
            {0, micro_second};

        [Single] ->
            Single;

        [Smaller, Larger | Rest] ->
            Smaller_unit_value = unit_values(erlang:element(2, Smaller)),
            Larger_unit_value = unit_values(erlang:element(2, Larger)),
            At_least_two_thirds = (erlang:element(1, Smaller) * Smaller_unit_value)
            < ((Larger_unit_value * 2) div 3),
            Rounded = case At_least_two_thirds of
                true ->
                    Larger;

                false ->
                    {erlang:element(1, Larger) + 1, erlang:element(2, Larger)}
            end,
            inner_blur([Rounded | Rest])
    end.

-file("src/birl/duration.gleam", 217).
?DOC(" approximates the duration by a value in a single unit\n").
-spec blur(duration()) -> {integer(), unit()}.
blur(Duration) ->
    _pipe = decompose(Duration),
    _pipe@1 = lists:reverse(_pipe),
    inner_blur(_pipe@1).

-file("src/birl/duration.gleam", 114).
?DOC(" use this if you need very long durations where small inaccuracies could lead to large errors\n").
-spec accurate_new(list({integer(), unit()})) -> duration().
accurate_new(Values) ->
    _pipe = Values,
    _pipe@1 = gleam@list:fold(_pipe, 0, fun(Total, Current) -> case Current of
                {Amount, micro_second} ->
                    Total + Amount;

                {Amount@1, milli_second} ->
                    Total + (Amount@1 * 1000);

                {Amount@2, second} ->
                    Total + (Amount@2 * 1000000);

                {Amount@3, minute} ->
                    Total + (Amount@3 * 60000000);

                {Amount@4, hour} ->
                    Total + (Amount@4 * 3600000000);

                {Amount@5, day} ->
                    Total + (Amount@5 * 86400000000);

                {Amount@6, week} ->
                    Total + (Amount@6 * 604800000000);

                {Amount@7, month} ->
                    Total + (Amount@7 * 2629746000000);

                {Amount@8, year} ->
                    Total + (Amount@8 * 31556952000000)
            end end),
    {duration, _pipe@1}.

-file("src/birl/duration.gleam", 166).
?DOC(" use this if you need very long durations where small inaccuracies could lead to large errors\n").
-spec accurate_decompose(duration()) -> list({integer(), unit()}).
accurate_decompose(Duration) ->
    {duration, Value} = Duration,
    Absolute_value = gleam@int:absolute_value(Value),
    {Years, Remaining} = extract(Absolute_value, 31556952000000),
    {Months, Remaining@1} = extract(Remaining, 2629746000000),
    {Weeks, Remaining@2} = extract(Remaining@1, 604800000000),
    {Days, Remaining@3} = extract(Remaining@2, 86400000000),
    {Hours, Remaining@4} = extract(Remaining@3, 3600000000),
    {Minutes, Remaining@5} = extract(Remaining@4, 60000000),
    {Seconds, Remaining@6} = extract(Remaining@5, 1000000),
    {Milli_seconds, Remaining@7} = extract(Remaining@6, 1000),
    _pipe = [{Years, year},
        {Months, month},
        {Weeks, week},
        {Days, day},
        {Hours, hour},
        {Minutes, minute},
        {Seconds, second},
        {Milli_seconds, milli_second},
        {Remaining@7, micro_second}],
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Item) -> erlang:element(1, Item) > 0 end
    ),
    gleam@list:map(_pipe@1, fun(Item@1) -> case Value < 0 of
                true ->
                    {-1 * erlang:element(1, Item@1), erlang:element(2, Item@1)};

                false ->
                    Item@1
            end end).

-file("src/birl/duration.gleam", 327).
?DOC(
    " you can use this function to create a new duration using expressions like:\n"
    "\n"
    "     \"accurate: 1 Year - 2days + 152M -1h + 25 years + 25secs\"\n"
    "\n"
    " where the units are:\n"
    "\n"
    "     Year:         y, Y, YEAR, years, Years, ...\n"
    "\n"
    "     Month:        mon, Month, mONths, ...\n"
    "\n"
    "     Week:         w, W, Week, weeks, ...\n"
    "\n"
    "     Day:          d, D, day, Days, ...\n"
    "\n"
    "     Hour:         h, H, Hour, Hours, ...\n"
    "\n"
    "     Minute:       m, M, Min, minute, Minutes, ...\n"
    "\n"
    "     Second:       s, S, sec, Secs, second, Seconds, ...\n"
    "\n"
    "     MilliSecond:  ms, Msec, mSecs, milliSecond, MilliSecond, ...\n"
    "\n"
    " numbers with no unit are considered as microseconds.\n"
    " specifying `accurate:` is equivalent to using `accurate_new`.\n"
).
-spec parse(binary()) -> {ok, duration()} | {error, nil}.
parse(Expression) ->
    Re@1 = case gleam@regexp:from_string(
        <<"([+|\\-])?\\s*(\\d+)\\s*(\\w+)?"/utf8>>
    ) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"parse"/utf8>>,
                        line => 328,
                        value => _assert_fail,
                        start => 8616,
                        'end' => 8689,
                        pattern_start => 8627,
                        pattern_end => 8633})
    end,
    {Constructor, Expression@3} = case gleam_stdlib:string_starts_with(
        Expression,
        <<"accurate:"/utf8>>
    ) of
        true ->
            Expression@2 = case gleam@string:split(Expression, <<":"/utf8>>) of
                [_, Expression@1] -> Expression@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl/duration"/utf8>>,
                                function => <<"parse"/utf8>>,
                                line => 334,
                                value => _assert_fail@1,
                                start => 8803,
                                'end' => 8861,
                                pattern_start => 8814,
                                pattern_end => 8829})
            end,
            {fun accurate_new/1, Expression@2};

        false ->
            {fun new/1, Expression}
    end,
    case begin
        _pipe = Expression@3,
        _pipe@1 = string:lowercase(_pipe),
        _pipe@2 = gleam@regexp:scan(Re@1, _pipe@1),
        gleam@list:try_map(_pipe@2, fun(Item) -> case Item of
                    {match, _, [Sign_option, {some, Amount_string}]} ->
                        gleam@result:then(
                            gleam_stdlib:parse_int(Amount_string),
                            fun(Amount) -> case Sign_option of
                                    {some, <<"-"/utf8>>} ->
                                        {ok, {-1 * Amount, micro_second}};

                                    none ->
                                        {ok, {Amount, micro_second}};

                                    {some, <<"+"/utf8>>} ->
                                        {ok, {Amount, micro_second}};

                                    {some, _} ->
                                        {error, nil}
                                end end
                        );

                    {match,
                        _,
                        [Sign_option@1, {some, Amount_string@1}, {some, Unit}]} ->
                        gleam@result:then(
                            gleam_stdlib:parse_int(Amount_string@1),
                            fun(Amount@1) ->
                                gleam@result:then(
                                    gleam@list:find(
                                        [{year,
                                                [<<"y"/utf8>>,
                                                    <<"year"/utf8>>,
                                                    <<"years"/utf8>>]},
                                            {month,
                                                [<<"mon"/utf8>>,
                                                    <<"month"/utf8>>,
                                                    <<"months"/utf8>>]},
                                            {week,
                                                [<<"w"/utf8>>,
                                                    <<"week"/utf8>>,
                                                    <<"weeks"/utf8>>]},
                                            {day,
                                                [<<"d"/utf8>>,
                                                    <<"day"/utf8>>,
                                                    <<"days"/utf8>>]},
                                            {hour,
                                                [<<"h"/utf8>>,
                                                    <<"hour"/utf8>>,
                                                    <<"hours"/utf8>>]},
                                            {minute,
                                                [<<"m"/utf8>>,
                                                    <<"min"/utf8>>,
                                                    <<"minute"/utf8>>,
                                                    <<"minutes"/utf8>>]},
                                            {second,
                                                [<<"s"/utf8>>,
                                                    <<"sec"/utf8>>,
                                                    <<"secs"/utf8>>,
                                                    <<"second"/utf8>>,
                                                    <<"seconds"/utf8>>]},
                                            {milli_second,
                                                [<<"ms"/utf8>>,
                                                    <<"msec"/utf8>>,
                                                    <<"msecs"/utf8>>,
                                                    <<"millisecond"/utf8>>,
                                                    <<"milliseconds"/utf8>>,
                                                    <<"milli-second"/utf8>>,
                                                    <<"milli-seconds"/utf8>>,
                                                    <<"milli_second"/utf8>>,
                                                    <<"milli_seconds"/utf8>>]}],
                                        fun(Item@1) ->
                                            gleam@list:contains(
                                                erlang:element(2, Item@1),
                                                Unit
                                            )
                                        end
                                    ),
                                    fun(_use0) ->
                                        {Unit@1, _} = _use0,
                                        case Sign_option@1 of
                                            {some, <<"-"/utf8>>} ->
                                                {ok, {-1 * Amount@1, Unit@1}};

                                            none ->
                                                {ok, {Amount@1, Unit@1}};

                                            {some, <<"+"/utf8>>} ->
                                                {ok, {Amount@1, Unit@1}};

                                            {some, _} ->
                                                {error, nil}
                                        end
                                    end
                                )
                            end
                        );

                    _ ->
                        {error, nil}
                end end)
    end of
        {ok, Values} ->
            _pipe@3 = Values,
            _pipe@4 = Constructor(_pipe@3),
            {ok, _pipe@4};

        {error, nil} ->
            {error, nil}
    end.
