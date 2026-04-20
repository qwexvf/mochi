-module(mochi@document_cache).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/document_cache.gleam").
-export([new_with_max/1, get/2, put/3, size/1, new/0]).
-export_type([document_cache/0, cache_inner/0]).

-opaque document_cache() :: {document_cache, cache_inner()}.

-type cache_inner() :: any().

-file("src/mochi/document_cache.gleam", 27).
-spec new_with_max(integer()) -> document_cache().
new_with_max(Max_size) ->
    {document_cache, document_cache_ffi:new(Max_size)}.

-file("src/mochi/document_cache.gleam", 31).
-spec get(document_cache(), binary()) -> {ok, mochi@ast:document()} |
    {error, nil}.
get(Cache, Query) ->
    document_cache_ffi:get(erlang:element(2, Cache), Query).

-file("src/mochi/document_cache.gleam", 35).
-spec put(document_cache(), binary(), mochi@ast:document()) -> nil.
put(Cache, Query, Doc) ->
    document_cache_ffi:put(erlang:element(2, Cache), Query, Doc).

-file("src/mochi/document_cache.gleam", 39).
-spec size(document_cache()) -> integer().
size(Cache) ->
    document_cache_ffi:size(erlang:element(2, Cache)).

-file("src/mochi/document_cache.gleam", 23).
-spec new() -> document_cache().
new() ->
    {document_cache, document_cache_ffi:new(1000)}.
