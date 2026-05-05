-module(document_cache_ffi).
-export([new/1, get/2, put/3, size/1]).

%% ETS-backed parsed-query cache.
%%
%% Tuned for the common case of many concurrent readers + rare writers
%% (one write per unique query, then nothing). The flags below all matter
%% under load:
%%
%%   read_concurrency       - per-bucket reader locks instead of one global
%%   write_concurrency      - even though writes are rare, the option also
%%                            enables fine-grained meta-info access at
%%                            read time on modern OTP, removing a hot lock
%%   decentralized_counters - avoids serializing the table size counter
%%                            across schedulers (default is `false` for set
%%                            tables; `true` is essentially free here)
new(MaxSize) ->
    Table = ets:new(mochi_document_cache, [
        set, public,
        {read_concurrency, true},
        {write_concurrency, true},
        {decentralized_counters, true}
    ]),
    {document_cache, Table, MaxSize}.

%% `lookup_element/4` returns the value directly and skips allocating the
%% list+tuple wrapper that `lookup/2` produces on the calling process heap.
%% On miss it returns the supplied default atom — no exception path.
get({document_cache, Table, _MaxSize}, Key) ->
    case ets:lookup_element(Table, Key, 2, '$cache_miss') of
        '$cache_miss' -> {error, nil};
        Value         -> {ok, Value}
    end.

put({document_cache, Table, MaxSize}, Key, Value) ->
    case ets:info(Table, size) < MaxSize of
        true  -> ets:insert(Table, {Key, Value});
        false -> ok
    end,
    nil.

size({document_cache, Table, _MaxSize}) ->
    ets:info(Table, size).
