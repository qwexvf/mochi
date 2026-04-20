-module(mochi_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/mochi_test.gleam").
-export([main/0, basic_query_test/0, named_query_test/0, nested_fields_test/0, basic_mutation_test/0, named_mutation_test/0]).

-file("test/mochi_test.gleam", 4).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/mochi_test.gleam", 8).
-spec basic_query_test() -> nil.
basic_query_test() ->
    Query = <<"{ user { entries { id } } }"/utf8>>,
    Result = mochi:parse(Query),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failed to parse valid GraphQL query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_test"/utf8>>,
                    function => <<"basic_query_test"/utf8>>,
                    line => 14})
    end.

-file("test/mochi_test.gleam", 18).
-spec named_query_test() -> nil.
named_query_test() ->
    Query = <<"query GetUser { user { name id } }"/utf8>>,
    Result = mochi:parse(Query),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failed to parse named GraphQL query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_test"/utf8>>,
                    function => <<"named_query_test"/utf8>>,
                    line => 24})
    end.

-file("test/mochi_test.gleam", 28).
-spec nested_fields_test() -> nil.
nested_fields_test() ->
    Query = <<"{ user { profile { name avatar { url size } } posts { title content } } }"/utf8>>,
    Result = mochi:parse(Query),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failed to parse nested GraphQL query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_test"/utf8>>,
                    function => <<"nested_fields_test"/utf8>>,
                    line => 35})
    end.

-file("test/mochi_test.gleam", 39).
-spec basic_mutation_test() -> nil.
basic_mutation_test() ->
    Mutation = <<"mutation { createUser { id name } }"/utf8>>,
    Result = mochi:parse(Mutation),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failed to parse basic GraphQL mutation"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_test"/utf8>>,
                    function => <<"basic_mutation_test"/utf8>>,
                    line => 45})
    end.

-file("test/mochi_test.gleam", 49).
-spec named_mutation_test() -> nil.
named_mutation_test() ->
    Mutation = <<"mutation CreateUser { createUser { id name email } }"/utf8>>,
    Result = mochi:parse(Mutation),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failed to parse named GraphQL mutation"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_test"/utf8>>,
                    function => <<"named_mutation_test"/utf8>>,
                    line => 55})
    end.
