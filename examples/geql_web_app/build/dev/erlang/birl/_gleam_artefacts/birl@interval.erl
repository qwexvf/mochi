-module(birl@interval).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birl/interval.gleam").
-export([from_start_and_end/2, from_start_and_duration/2, shift/2, scale_up/2, scale_down/2, includes/2, contains/2, intersection/2, get_bounds/1]).
-export_type([interval/0]).

-opaque interval() :: {interval, birl:time(), birl:time()}.

-file("src/birl/interval.gleam", 12).
-spec from_start_and_end(birl:time(), birl:time()) -> {ok, interval()} |
    {error, nil}.
from_start_and_end(Start, End) ->
    case birl:compare(Start, End) of
        eq ->
            {error, nil};

        lt ->
            {ok, {interval, Start, End}};

        gt ->
            {ok, {interval, End, Start}}
    end.

-file("src/birl/interval.gleam", 20).
-spec from_start_and_duration(birl:time(), birl@duration:duration()) -> {ok,
        interval()} |
    {error, nil}.
from_start_and_duration(Start, Duration) ->
    from_start_and_end(Start, birl:add(Start, Duration)).

-file("src/birl/interval.gleam", 24).
-spec shift(interval(), birl@duration:duration()) -> interval().
shift(Interval, Duration) ->
    case Interval of
        {interval, Start, End} ->
            {interval, birl:add(Start, Duration), birl:add(End, Duration)}
    end.

-file("src/birl/interval.gleam", 31).
-spec scale_up(interval(), integer()) -> interval().
scale_up(Interval, Factor) ->
    case Interval of
        {interval, Start, End} ->
            Interval@2 = case begin
                _pipe = birl:difference(End, Start),
                _pipe@1 = birl@duration:scale_up(_pipe, Factor),
                from_start_and_duration(Start, _pipe@1)
            end of
                {ok, Interval@1} -> Interval@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl/interval"/utf8>>,
                                function => <<"scale_up"/utf8>>,
                                line => 34,
                                value => _assert_fail,
                                start => 806,
                                'end' => 949,
                                pattern_start => 817,
                                pattern_end => 829})
            end,
            Interval@2
    end.

-file("src/birl/interval.gleam", 43).
-spec scale_down(interval(), integer()) -> interval().
scale_down(Interval, Factor) ->
    case Interval of
        {interval, Start, End} ->
            Interval@2 = case begin
                _pipe = birl:difference(End, Start),
                _pipe@1 = birl@duration:scale_down(_pipe, Factor),
                from_start_and_duration(Start, _pipe@1)
            end of
                {ok, Interval@1} -> Interval@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl/interval"/utf8>>,
                                function => <<"scale_down"/utf8>>,
                                line => 46,
                                value => _assert_fail,
                                start => 1085,
                                'end' => 1230,
                                pattern_start => 1096,
                                pattern_end => 1108})
            end,
            Interval@2
    end.

-file("src/birl/interval.gleam", 72).
-spec includes(interval(), birl:time()) -> boolean().
includes(Interval, Time) ->
    case Interval of
        {interval, Start, End} ->
            gleam@list:contains([eq, lt], birl:compare(Start, Time)) andalso gleam@list:contains(
                [eq, gt],
                birl:compare(End, Time)
            )
    end.

-file("src/birl/interval.gleam", 80).
-spec contains(interval(), interval()) -> boolean().
contains(A, B) ->
    case B of
        {interval, Start, End} ->
            includes(A, Start) andalso includes(A, End)
    end.

-file("src/birl/interval.gleam", 55).
-spec intersection(interval(), interval()) -> gleam@option:option(interval()).
intersection(A, B) ->
    case {contains(A, B), contains(B, A)} of
        {true, false} ->
            {some, B};

        {false, true} ->
            {some, A};

        {_, _} ->
            {interval, A_start, A_end} = A,
            {interval, B_start, B_end} = B,
            case {includes(A, B_start), includes(B, A_start)} of
                {true, false} ->
                    {some, {interval, B_start, A_end}};

                {false, true} ->
                    {some, {interval, A_start, B_end}};

                {_, _} ->
                    none
            end
    end.

-file("src/birl/interval.gleam", 86).
-spec get_bounds(interval()) -> {birl:time(), birl:time()}.
get_bounds(Interval) ->
    case Interval of
        {interval, Start, End} ->
            {Start, End}
    end.
