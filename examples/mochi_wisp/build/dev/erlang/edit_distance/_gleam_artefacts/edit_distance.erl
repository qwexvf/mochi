-module(edit_distance).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/edit_distance.gleam").
-export([levenshtein/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/edit_distance.gleam", 76).
-spec update_distances(
    binary(),
    list(binary()),
    list(integer()),
    integer(),
    list(integer())
) -> list(integer()).
update_distances(
    Grapheme,
    Other,
    Previous_distances,
    Last_distance,
    New_distances
) ->
    case {Other, Previous_distances} of
        {[], _} ->
            lists:reverse(New_distances);

        {_, []} ->
            lists:reverse(New_distances);

        {_, [_]} ->
            lists:reverse(New_distances);

        {[First | Other@1], [Previous_distance | Rest]} ->
            Second_distance@1 = case Rest of
                [Second_distance | _] -> Second_distance;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"edit_distance"/utf8>>,
                                function => <<"update_distances"/utf8>>,
                                line => 101,
                                value => _assert_fail,
                                start => 3799,
                                'end' => 3838,
                                pattern_start => 3810,
                                pattern_end => 3831})
            end,
            Insertion_distance = Last_distance + 1,
            Deletion_distance = Second_distance@1 + 1,
            Substitution_distance = case Grapheme =:= First of
                false ->
                    Previous_distance + 1;

                true ->
                    Previous_distance
            end,
            New_distance = begin
                _pipe = Substitution_distance,
                _pipe@1 = gleam@int:min(_pipe, Insertion_distance),
                gleam@int:min(_pipe@1, Deletion_distance)
            end,
            New_distances@1 = [New_distance | New_distances],
            update_distances(
                Grapheme,
                Other@1,
                Rest,
                New_distance,
                New_distances@1
            )
    end.

-file("src/edit_distance.gleam", 37).
-spec levenshtein_loop(
    list(binary()),
    list(binary()),
    list(integer()),
    integer()
) -> integer().
levenshtein_loop(One, Other, Distances, Prefix_size) ->
    case One of
        [] ->
            Distance@1 = case gleam@list:last(Distances) of
                {ok, Distance} -> Distance;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"distance list will always have at least one item"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"edit_distance"/utf8>>,
                                function => <<"levenshtein_loop"/utf8>>,
                                line => 52,
                                value => _assert_fail,
                                start => 1910,
                                'end' => 1956,
                                pattern_start => 1921,
                                pattern_end => 1933})
            end,
            Distance@1;

        [First | Rest] ->
            Prefix_size@1 = Prefix_size + 1,
            New_distances = [Prefix_size@1],
            Distance_list = update_distances(
                First,
                Other,
                Distances,
                Prefix_size@1,
                New_distances
            ),
            levenshtein_loop(Rest, Other, Distance_list, Prefix_size@1)
    end.

-file("src/edit_distance.gleam", 20).
?DOC(
    " Compute the edit distance between two strings using the\n"
    " [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance).\n"
    " The Levenshtein distance between two strings is the number of edits that\n"
    " will get you from one string to the other; the allowed edits are:\n"
    " - insertion: adding a new character to one of the two strings\n"
    " - deletion: removing a character from one of the two strings\n"
    " - replacemente: replace a character with a new one in one of the two strings\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " assert 2 == levenshtein(\"gleam\", \"beam\")\n"
    " assert 1 == levenshtein(\"cat\", \"cap\")\n"
    " ```\n"
).
-spec levenshtein(binary(), binary()) -> integer().
levenshtein(One, Other) ->
    case {One, Other} of
        {_, _} when One =:= Other ->
            0;

        {<<""/utf8>>, String} ->
            string:length(String);

        {String, <<""/utf8>>} ->
            string:length(String);

        {One@1, Other@1} ->
            One@2 = gleam@string:to_graphemes(One@1),
            Other@2 = gleam@string:to_graphemes(Other@1),
            Distance_list = gleam@list:range(0, erlang:length(Other@2)),
            levenshtein_loop(One@2, Other@2, Distance_list, 0)
    end.
