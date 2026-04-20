-module(mochi@dataloader_cache).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/dataloader_cache.gleam").
-export([new/1, get/2, put/3, put_many/2, invalidate/2, clear/1]).
-export_type([shared_cache/0, cache_inner/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque shared_cache() :: {shared_cache, cache_inner()}.

-type cache_inner() :: any().

-file("src/mochi/dataloader_cache.gleam", 30).
?DOC(
    " Create an ETS-backed shared cache. The same name always returns the same\n"
    " underlying ETS table, so it is safe to call from multiple processes or on\n"
    " every request — only one table is created per name.\n"
).
-spec new(binary()) -> shared_cache().
new(Name) ->
    {shared_cache, dataloader_cache_ffi:new(Name)}.

-file("src/mochi/dataloader_cache.gleam", 35).
?DOC(" Look up a key. Returns `Some(result)` if present, `None` if not cached.\n").
-spec get(shared_cache(), any()) -> gleam@option:option({ok, any()} |
    {error, binary()}).
get(Cache, Key) ->
    case dataloader_cache_ffi:get(erlang:element(2, Cache), Key) of
        {ok, V} ->
            {some, V};

        {error, _} ->
            none
    end.

-file("src/mochi/dataloader_cache.gleam", 43).
?DOC(" Store a single result.\n").
-spec put(shared_cache(), any(), {ok, any()} | {error, binary()}) -> nil.
put(Cache, Key, Value) ->
    dataloader_cache_ffi:put(erlang:element(2, Cache), Key, Value).

-file("src/mochi/dataloader_cache.gleam", 48).
?DOC(" Store multiple results at once.\n").
-spec put_many(shared_cache(), list({any(), {ok, any()} | {error, binary()}})) -> nil.
put_many(Cache, Pairs) ->
    dataloader_cache_ffi:put_many(erlang:element(2, Cache), Pairs).

-file("src/mochi/dataloader_cache.gleam", 56).
?DOC(" Remove a key — call this after a mutation that invalidates cached data.\n").
-spec invalidate(shared_cache(), any()) -> nil.
invalidate(Cache, Key) ->
    dataloader_cache_ffi:invalidate(erlang:element(2, Cache), Key).

-file("src/mochi/dataloader_cache.gleam", 61).
?DOC(" Wipe all entries from the cache.\n").
-spec clear(shared_cache()) -> nil.
clear(Cache) ->
    dataloader_cache_ffi:clear(erlang:element(2, Cache)).
