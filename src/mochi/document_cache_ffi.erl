-module(document_cache_ffi).
-export([new/2, get/2, put/3, size/1]).

%% ETS-backed parsed-query cache, with a byte-size threshold.
%%
%% Cache hits are only ~3 µs cheaper than parsing on small queries (post
%% lexer-rewrite parser). For queries below `MinSize` bytes the lookup
%% overhead can match or exceed the saving, so we skip the cache entirely
%% — both reads and writes — keeping the table populated only with
%% entries where caching pays off.
new(MaxSize, MinSize) ->
    Table = ets:new(mochi_document_cache, [
        set, public,
        {read_concurrency, true},
        {write_concurrency, true},
        {decentralized_counters, true}
    ]),
    {document_cache, Table, MaxSize, MinSize}.

get({document_cache, _Table, _MaxSize, MinSize}, Key)
        when byte_size(Key) < MinSize ->
    %% Below threshold — let the caller re-parse, it's faster than the
    %% ets lookup + term copy.
    {error, nil};
get({document_cache, Table, _MaxSize, _MinSize}, Key) ->
    case ets:lookup_element(Table, Key, 2, '$cache_miss') of
        '$cache_miss' -> {error, nil};
        Value         -> {ok, Value}
    end.

put({document_cache, _Table, _MaxSize, MinSize}, Key, _Value)
        when byte_size(Key) < MinSize ->
    nil;
put({document_cache, Table, MaxSize, _MinSize}, Key, Value) ->
    case ets:info(Table, size) < MaxSize of
        true  -> ets:insert(Table, {Key, Value});
        false -> ok
    end,
    nil.

size({document_cache, Table, _MaxSize, _MinSize}) ->
    ets:info(Table, size).
