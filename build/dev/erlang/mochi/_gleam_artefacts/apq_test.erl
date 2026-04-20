-module(apq_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/apq_test.gleam").
-export([new_store_test/0, register_and_lookup_test/0, lookup_nonexistent_test/0, with_queries_test/0, register_same_query_twice_test/0, hash_is_deterministic_test/0, hash_differs_for_different_queries_test/0, hash_is_64_chars_test/0, hash_normalises_whitespace_test/0, process_not_found_test/0, process_register_new_query_test/0, process_retrieve_cached_test/0, process_hash_mismatch_test/0, parse_extension_present_test/0, parse_extension_absent_test/0]).

-file("test/apq_test.gleam", 7).
-spec new_store_test() -> nil.
new_store_test() ->
    _pipe = mochi@apq:new(),
    _pipe@1 = mochi@apq:size(_pipe),
    gleeunit@should:equal(_pipe@1, 0).

-file("test/apq_test.gleam", 13).
-spec register_and_lookup_test() -> nil.
register_and_lookup_test() ->
    Store = mochi@apq:new(),
    Query = <<"{ hello }"/utf8>>,
    {Store@1, H} = mochi@apq:register(Store, Query),
    _pipe = mochi@apq:size(Store@1),
    gleeunit@should:equal(_pipe, 1),
    _pipe@1 = mochi@apq:lookup(Store@1, H),
    gleeunit@should:equal(_pipe@1, {some, Query}).

-file("test/apq_test.gleam", 22).
-spec lookup_nonexistent_test() -> nil.
lookup_nonexistent_test() ->
    _pipe = mochi@apq:new(),
    _pipe@1 = mochi@apq:lookup(_pipe, <<"nonexistent"/utf8>>),
    gleeunit@should:equal(_pipe@1, none).

-file("test/apq_test.gleam", 28).
-spec with_queries_test() -> nil.
with_queries_test() ->
    _pipe = mochi@apq:with_queries([<<"{ hello }"/utf8>>, <<"{ world }"/utf8>>]),
    _pipe@1 = mochi@apq:size(_pipe),
    gleeunit@should:equal(_pipe@1, 2).

-file("test/apq_test.gleam", 34).
-spec register_same_query_twice_test() -> nil.
register_same_query_twice_test() ->
    Store = mochi@apq:new(),
    {Store@1, H1} = mochi@apq:register(Store, <<"{ hello }"/utf8>>),
    {Store@2, H2} = mochi@apq:register(Store@1, <<"{ hello }"/utf8>>),
    gleeunit@should:equal(H1, H2),
    _pipe = mochi@apq:size(Store@2),
    gleeunit@should:equal(_pipe, 1).

-file("test/apq_test.gleam", 42).
-spec hash_is_deterministic_test() -> nil.
hash_is_deterministic_test() ->
    gleeunit@should:equal(
        mochi@apq:hash(<<"{ hello }"/utf8>>),
        mochi@apq:hash(<<"{ hello }"/utf8>>)
    ).

-file("test/apq_test.gleam", 46).
-spec hash_differs_for_different_queries_test() -> nil.
hash_differs_for_different_queries_test() ->
    gleeunit@should:not_equal(
        mochi@apq:hash(<<"{ hello }"/utf8>>),
        mochi@apq:hash(<<"{ world }"/utf8>>)
    ).

-file("test/apq_test.gleam", 50).
-spec hash_is_64_chars_test() -> nil.
hash_is_64_chars_test() ->
    _pipe = mochi@apq:hash(<<"{ hello }"/utf8>>),
    (fun(H) -> gleeunit@should:equal(string:length(H), 64) end)(_pipe).

-file("test/apq_test.gleam", 55).
-spec hash_normalises_whitespace_test() -> nil.
hash_normalises_whitespace_test() ->
    gleeunit@should:equal(
        mochi@apq:hash(<<"{ hello }"/utf8>>),
        mochi@apq:hash(<<"  {  hello  }  "/utf8>>)
    ).

-file("test/apq_test.gleam", 59).
-spec process_not_found_test() -> nil.
process_not_found_test() ->
    _pipe = mochi@apq:new(),
    _pipe@1 = mochi@apq:process(_pipe, none, <<"fakehash"/utf8>>),
    gleeunit@should:equal(_pipe@1, {error, not_found}).

-file("test/apq_test.gleam", 65).
-spec process_register_new_query_test() -> nil.
process_register_new_query_test() ->
    Store = mochi@apq:new(),
    Query = <<"{ hello }"/utf8>>,
    H = mochi@apq:hash(Query),
    case mochi@apq:process(Store, {some, Query}, H) of
        {ok, {Store2, Returned}} ->
            gleeunit@should:equal(Returned, Query),
            _pipe = mochi@apq:size(Store2),
            gleeunit@should:equal(_pipe, 1);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/apq_test.gleam", 79).
-spec process_retrieve_cached_test() -> nil.
process_retrieve_cached_test() ->
    Store = mochi@apq:new(),
    Query = <<"{ hello }"/utf8>>,
    {Store@1, H} = mochi@apq:register(Store, Query),
    case mochi@apq:process(Store@1, none, H) of
        {ok, {_, Returned}} ->
            gleeunit@should:equal(Returned, Query);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/apq_test.gleam", 90).
-spec process_hash_mismatch_test() -> nil.
process_hash_mismatch_test() ->
    Store = mochi@apq:new(),
    Wrong = <<"0000000000000000000000000000000000000000000000000000000000000000"/utf8>>,
    case mochi@apq:process(Store, {some, <<"{ hello }"/utf8>>}, Wrong) of
        {error, {hash_mismatch, E, _}} ->
            gleeunit@should:equal(E, Wrong);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/apq_test.gleam", 100).
-spec parse_extension_present_test() -> nil.
parse_extension_present_test() ->
    Ext = maps:from_list(
        [{<<"persistedQuery"/utf8>>,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"version"/utf8>>, gleam_stdlib:identity(1)},
                            {<<"sha256Hash"/utf8>>,
                                gleam_stdlib:identity(<<"abc123"/utf8>>)}]
                    )
                )}]
    ),
    case mochi@apq:parse_extension(Ext) of
        {some, {extension, V, H}} ->
            gleeunit@should:equal(V, 1),
            gleeunit@should:equal(H, <<"abc123"/utf8>>);

        none ->
            gleeunit@should:fail()
    end.

-file("test/apq_test.gleam", 123).
-spec parse_extension_absent_test() -> nil.
parse_extension_absent_test() ->
    _pipe = mochi@apq:parse_extension(maps:new()),
    gleeunit@should:equal(_pipe, none).
