-module(dataloader_cache_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/dataloader_cache_test.gleam").
-export([shared_cache_stores_and_retrieves_test/0, shared_cache_put_many_test/0, shared_cache_invalidate_test/0, shared_cache_same_name_returns_same_table_test/0]).

-file("test/dataloader_cache_test.gleam", 6).
-spec shared_cache_stores_and_retrieves_test() -> nil.
shared_cache_stores_and_retrieves_test() ->
    Cache = mochi@dataloader_cache:new(<<"test_dl_shared_1"/utf8>>),
    mochi@dataloader_cache:put(Cache, <<"key1"/utf8>>, {ok, <<"value1"/utf8>>}),
    _pipe = mochi@dataloader_cache:get(Cache, <<"key1"/utf8>>),
    gleeunit@should:equal(_pipe, {some, {ok, <<"value1"/utf8>>}}),
    _pipe@1 = mochi@dataloader_cache:get(Cache, <<"missing"/utf8>>),
    gleeunit@should:equal(_pipe@1, none),
    mochi@dataloader_cache:clear(Cache).

-file("test/dataloader_cache_test.gleam", 17).
-spec shared_cache_put_many_test() -> nil.
shared_cache_put_many_test() ->
    Cache = mochi@dataloader_cache:new(<<"test_dl_shared_2"/utf8>>),
    mochi@dataloader_cache:put_many(
        Cache,
        [{<<"a"/utf8>>, {ok, <<"1"/utf8>>}}, {<<"b"/utf8>>, {ok, <<"2"/utf8>>}}]
    ),
    _pipe = mochi@dataloader_cache:get(Cache, <<"a"/utf8>>),
    gleeunit@should:equal(_pipe, {some, {ok, <<"1"/utf8>>}}),
    _pipe@1 = mochi@dataloader_cache:get(Cache, <<"b"/utf8>>),
    gleeunit@should:equal(_pipe@1, {some, {ok, <<"2"/utf8>>}}),
    mochi@dataloader_cache:clear(Cache).

-file("test/dataloader_cache_test.gleam", 26).
-spec shared_cache_invalidate_test() -> nil.
shared_cache_invalidate_test() ->
    Cache = mochi@dataloader_cache:new(<<"test_dl_shared_3"/utf8>>),
    mochi@dataloader_cache:put(Cache, <<"k"/utf8>>, {ok, <<"v"/utf8>>}),
    mochi@dataloader_cache:invalidate(Cache, <<"k"/utf8>>),
    _pipe = mochi@dataloader_cache:get(Cache, <<"k"/utf8>>),
    gleeunit@should:equal(_pipe, none).

-file("test/dataloader_cache_test.gleam", 34).
-spec shared_cache_same_name_returns_same_table_test() -> nil.
shared_cache_same_name_returns_same_table_test() ->
    Cache1 = mochi@dataloader_cache:new(<<"test_dl_shared_4"/utf8>>),
    Cache2 = mochi@dataloader_cache:new(<<"test_dl_shared_4"/utf8>>),
    mochi@dataloader_cache:put(Cache1, <<"x"/utf8>>, {ok, 42}),
    _pipe = mochi@dataloader_cache:get(Cache2, <<"x"/utf8>>),
    gleeunit@should:equal(_pipe, {some, {ok, 42}}),
    mochi@dataloader_cache:clear(Cache1).
