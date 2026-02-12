-module(mochi_query_cache_ffi).
-export([init/0, get/1, put/2, stats/0, clear/0, size/0]).

-define(TABLE, mochi_query_cache).
-define(STATS_TABLE, mochi_query_cache_stats).

%% Initialize the cache tables
init() ->
    %% Create main cache table with read concurrency for performance
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,
    %% Create stats table
    case ets:info(?STATS_TABLE) of
        undefined ->
            ets:new(?STATS_TABLE, [
                named_table,
                public,
                set,
                {write_concurrency, true}
            ]),
            ets:insert(?STATS_TABLE, {hits, 0}),
            ets:insert(?STATS_TABLE, {misses, 0});
        _ ->
            ok
    end,
    nil.

%% Get a cached query by its hash
get(Query) ->
    Hash = erlang:phash2(Query),
    case ets:lookup(?TABLE, Hash) of
        [{Hash, Document}] ->
            %% Cache hit - increment counter
            ets:update_counter(?STATS_TABLE, hits, 1),
            {some, Document};
        [] ->
            %% Cache miss - increment counter
            ets:update_counter(?STATS_TABLE, misses, 1),
            none
    end.

%% Store a parsed query
put(Query, Document) ->
    Hash = erlang:phash2(Query),
    ets:insert(?TABLE, {Hash, Document}),
    nil.

%% Get cache statistics
stats() ->
    [{hits, Hits}] = ets:lookup(?STATS_TABLE, hits),
    [{misses, Misses}] = ets:lookup(?STATS_TABLE, misses),
    Size = ets:info(?TABLE, size),
    {cache_stats, Hits, Misses, Size}.

%% Clear the cache
clear() ->
    ets:delete_all_objects(?TABLE),
    ets:insert(?STATS_TABLE, {hits, 0}),
    ets:insert(?STATS_TABLE, {misses, 0}),
    nil.

%% Get cache size
size() ->
    ets:info(?TABLE, size).
