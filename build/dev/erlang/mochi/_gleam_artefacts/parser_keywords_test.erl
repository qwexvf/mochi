-module(parser_keywords_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/parser_keywords_test.gleam").
-export([keyword_query_as_field_name_test/0, keyword_mutation_as_field_name_test/0, keyword_subscription_as_field_name_test/0, keyword_fragment_as_field_name_test/0, keyword_on_as_field_name_test/0, keyword_true_as_field_name_test/0, keyword_false_as_field_name_test/0, keyword_null_as_field_name_test/0, keyword_as_alias_test/0, keyword_as_alias_target_test/0, keyword_on_as_nested_field_test/0, keyword_fragment_as_nested_field_test/0]).

-file("test/parser_keywords_test.gleam", 4).
-spec assert_parses(binary()) -> nil.
assert_parses(Q) ->
    case mochi@parser:parse(Q) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected parse success but got error: "/utf8,
                        (gleam@string:inspect(E))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"parser_keywords_test"/utf8>>,
                    function => <<"assert_parses"/utf8>>,
                    line => 8})
    end.

-file("test/parser_keywords_test.gleam", 12).
-spec keyword_query_as_field_name_test() -> nil.
keyword_query_as_field_name_test() ->
    assert_parses(<<"{ query { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 16).
-spec keyword_mutation_as_field_name_test() -> nil.
keyword_mutation_as_field_name_test() ->
    assert_parses(<<"{ mutation { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 20).
-spec keyword_subscription_as_field_name_test() -> nil.
keyword_subscription_as_field_name_test() ->
    assert_parses(<<"{ subscription { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 24).
-spec keyword_fragment_as_field_name_test() -> nil.
keyword_fragment_as_field_name_test() ->
    assert_parses(<<"{ fragment { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 28).
-spec keyword_on_as_field_name_test() -> nil.
keyword_on_as_field_name_test() ->
    assert_parses(<<"{ on { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 32).
-spec keyword_true_as_field_name_test() -> nil.
keyword_true_as_field_name_test() ->
    assert_parses(<<"{ true { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 36).
-spec keyword_false_as_field_name_test() -> nil.
keyword_false_as_field_name_test() ->
    assert_parses(<<"{ false { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 40).
-spec keyword_null_as_field_name_test() -> nil.
keyword_null_as_field_name_test() ->
    assert_parses(<<"{ null { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 44).
-spec keyword_as_alias_test() -> nil.
keyword_as_alias_test() ->
    assert_parses(<<"{ query: myField { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 48).
-spec keyword_as_alias_target_test() -> nil.
keyword_as_alias_target_test() ->
    assert_parses(<<"{ myAlias: query { id } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 52).
-spec keyword_on_as_nested_field_test() -> nil.
keyword_on_as_nested_field_test() ->
    assert_parses(<<"{ user { on } }"/utf8>>).

-file("test/parser_keywords_test.gleam", 56).
-spec keyword_fragment_as_nested_field_test() -> nil.
keyword_fragment_as_nested_field_test() ->
    assert_parses(<<"{ post { fragment body } }"/utf8>>).
