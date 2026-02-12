-module(birdie@internal@diff).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/diff.gleam").
-export([histogram/2]).
-export_type([diff_line/0, diff_line_kind/0, occurs/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type diff_line() :: {diff_line, integer(), binary(), diff_line_kind()}.

-type diff_line_kind() :: old | new | shared.

-type occurs(KKQ) :: {one, integer(), list(KKQ), list(KKQ)} |
    {other, integer(), list(KKQ), list(KKQ)} |
    {both, integer(), list(KKQ), list(KKQ), list(KKQ), list(KKQ)}.

-file("src/birdie/internal/diff.gleam", 23).
?DOC(false).
-spec match_diff_lines(
    list(diff_line()),
    list(binary()),
    integer(),
    list(binary()),
    integer(),
    list(binary())
) -> list(diff_line()).
match_diff_lines(Lines, Lcs, Line_one, One, Line_other, Other) ->
    case {Lcs, One, Other} of
        {[], [], []} ->
            lists:reverse(Lines);

        {[], [First | One@1], Other@1} ->
            _pipe = [{diff_line, Line_one, First, old} | Lines],
            match_diff_lines(
                _pipe,
                Lcs,
                Line_one + 1,
                One@1,
                Line_other,
                Other@1
            );

        {[], [], [First@1 | Other@2]} ->
            _pipe@1 = [{diff_line, Line_other, First@1, new} | Lines],
            match_diff_lines(
                _pipe@1,
                Lcs,
                Line_one,
                One,
                Line_other + 1,
                Other@2
            );

        {[First_common | _], [First_one | One@2], Other@3} when First_common =/= First_one ->
            _pipe@2 = [{diff_line, Line_one, First_one, old} | Lines],
            match_diff_lines(
                _pipe@2,
                Lcs,
                Line_one + 1,
                One@2,
                Line_other,
                Other@3
            );

        {[First_common@1 | _], One@3, [First_other | Other@4]} when First_common@1 =/= First_other ->
            _pipe@3 = [{diff_line, Line_other, First_other, new} | Lines],
            match_diff_lines(
                _pipe@3,
                Lcs,
                Line_one,
                One@3,
                Line_other + 1,
                Other@4
            );

        {[First_common@2 | Lcs@1], [_ | One@4], [_ | Other@5]} ->
            _pipe@4 = [{diff_line, Line_other, First_common@2, shared} | Lines],
            match_diff_lines(
                _pipe@4,
                Lcs@1,
                Line_one + 1,
                One@4,
                Line_other + 1,
                Other@5
            );

        {[_ | _], [], _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie/internal/diff"/utf8>>,
                    function => <<"match_diff_lines"/utf8>>,
                    line => 66});

        {[_ | _], _, []} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie/internal/diff"/utf8>>,
                    function => <<"match_diff_lines"/utf8>>,
                    line => 66})
    end.

-file("src/birdie/internal/diff.gleam", 174).
?DOC(false).
-spec sum_occurrences(occurs(KLX), occurs(KLX)) -> occurs(KLX).
sum_occurrences(One, Other) ->
    case {One, Other} of
        {{one, N, _, _}, {one, M, Before, After}} ->
            {one, N + M, Before, After};

        {{other, N@1, _, _}, {other, M@1, Before@1, After@1}} ->
            {other, N@1 + M@1, Before@1, After@1};

        {{one, N@2, Before_one, After_one},
            {other, M@2, Before_other, After_other}} ->
            {both, N@2 + M@2, Before_one, After_one, Before_other, After_other};

        {{both, N@2, Before_one, After_one, _, _},
            {other, M@2, Before_other, After_other}} ->
            {both, N@2 + M@2, Before_one, After_one, Before_other, After_other};

        {_, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable: sum_occurrences"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie/internal/diff"/utf8>>,
                    function => <<"sum_occurrences"/utf8>>,
                    line => 183})
    end.

-file("src/birdie/internal/diff.gleam", 150).
?DOC(false).
-spec histogram_add(
    gleam@dict:dict(KLL, occurs(KLL)),
    list(KLL),
    fun((integer(), list(KLL), list(KLL)) -> occurs(KLL)),
    list(KLL)
) -> gleam@dict:dict(KLL, occurs(KLL)).
histogram_add(Histogram, List, To_occurrence, Reverse_prefix) ->
    case List of
        [] ->
            Histogram;

        [First | Rest] ->
            _pipe = begin
                gleam@dict:upsert(
                    Histogram,
                    First,
                    fun(Previous) ->
                        New_occurrence = To_occurrence(1, Reverse_prefix, Rest),
                        case Previous of
                            {some, Occurrence} ->
                                sum_occurrences(Occurrence, New_occurrence);

                            none ->
                                New_occurrence
                        end
                    end
                )
            end,
            histogram_add(_pipe, Rest, To_occurrence, [First | Reverse_prefix])
    end.

-file("src/birdie/internal/diff.gleam", 121).
?DOC(false).
-spec lowest_occurrence_common_item(list(KLD), list(KLD)) -> gleam@option:option({KLD,
    integer(),
    list(KLD),
    list(KLD),
    list(KLD),
    list(KLD)}).
lowest_occurrence_common_item(One, Other) ->
    Histogram = begin
        _pipe = histogram_add(
            maps:new(),
            One,
            fun(Field@0, Field@1, Field@2) -> {one, Field@0, Field@1, Field@2} end,
            []
        ),
        histogram_add(
            _pipe,
            Other,
            fun(Field@0, Field@1, Field@2) -> {other, Field@0, Field@1, Field@2} end,
            []
        )
    end,
    gleam@dict:fold(Histogram, none, fun(Lowest, A, Occurs) -> case Occurs of
                {one, _, _, _} ->
                    Lowest;

                {other, _, _, _} ->
                    Lowest;

                {both, N, Before_one, After_one, Before_other, After_other} ->
                    case Lowest of
                        none ->
                            {some,
                                {A,
                                    N,
                                    Before_one,
                                    After_one,
                                    Before_other,
                                    After_other}};

                        {some, {_, M, _, _, _, _}} ->
                            case M =< N of
                                true ->
                                    Lowest;

                                false ->
                                    _pipe@1 = {A,
                                        N,
                                        Before_one,
                                        After_one,
                                        Before_other,
                                        After_other},
                                    {some, _pipe@1}
                            end
                    end
            end end).

-file("src/birdie/internal/diff.gleam", 200).
?DOC(false).
-spec do_pop_common_prefix(list(KMH), list(KMH), list(KMH)) -> {list(KMH),
    list(KMH),
    list(KMH)}.
do_pop_common_prefix(Reverse_prefix, One, Other) ->
    case {One, Other} of
        {[First_one | One@1], [First_other | Other@1]} when First_one =:= First_other ->
            do_pop_common_prefix([First_one | Reverse_prefix], One@1, Other@1);

        {_, _} ->
            {Reverse_prefix, One, Other}
    end.

-file("src/birdie/internal/diff.gleam", 192).
?DOC(false).
-spec pop_common_prefix(list(KMB), list(KMB)) -> {list(KMB),
    list(KMB),
    list(KMB)}.
pop_common_prefix(One, Other) ->
    {Reverse_prefix, One@1, Other@1} = do_pop_common_prefix([], One, Other),
    {lists:reverse(Reverse_prefix), One@1, Other@1}.

-file("src/birdie/internal/diff.gleam", 215).
?DOC(false).
-spec pop_common_suffix(list(KMO), list(KMO)) -> {list(KMO),
    list(KMO),
    list(KMO)}.
pop_common_suffix(One, Other) ->
    {Suffix, Reverse_one, Reverse_other} = do_pop_common_prefix(
        [],
        lists:reverse(One),
        lists:reverse(Other)
    ),
    {Suffix, lists:reverse(Reverse_one), lists:reverse(Reverse_other)}.

-file("src/birdie/internal/diff.gleam", 74).
?DOC(false).
-spec lcs(list(KKZ), list(KKZ)) -> list(KKZ).
lcs(One, Other) ->
    {Prefix, One@1, Other@1} = pop_common_prefix(One, Other),
    {Suffix, One@2, Other@2} = pop_common_suffix(One@1, Other@1),
    case lowest_occurrence_common_item(One@2, Other@2) of
        none ->
            lists:append([Prefix, Suffix]);

        {some, {Item, _, Before_a, After_a, Before_b, After_b}} ->
            lists:append(
                [Prefix,
                    lcs(lists:reverse(Before_a), lists:reverse(Before_b)),
                    [Item],
                    lcs(After_a, After_b),
                    Suffix]
            )
    end.

-file("src/birdie/internal/diff.gleam", 16).
?DOC(false).
-spec histogram(binary(), binary()) -> list(diff_line()).
histogram(One, Other) ->
    One_lines = gleam@string:split(One, <<"\n"/utf8>>),
    Other_lines = gleam@string:split(Other, <<"\n"/utf8>>),
    Lcs = lcs(One_lines, Other_lines),
    match_diff_lines([], Lcs, 1, One_lines, 1, Other_lines).
