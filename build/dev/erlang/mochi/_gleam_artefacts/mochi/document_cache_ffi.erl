-module(document_cache_ffi).
-export([new/1, get/2, put/3, size/1]).

new(MaxSize) ->
    Table = ets:new(mochi_document_cache, [set, public, {read_concurrency, true}]),
    {document_cache, Table, MaxSize}.

get({document_cache, Table, _MaxSize}, Key) ->
    case ets:lookup(Table, Key) of
        [{_, Value}] -> {ok, Value};
        []           -> {error, nil}
    end.

put({document_cache, Table, MaxSize}, Key, Value) ->
    case ets:info(Table, size) < MaxSize of
        true  -> ets:insert(Table, {Key, Value});
        false -> ok
    end,
    nil.

size({document_cache, Table, _MaxSize}) ->
    ets:info(Table, size).
