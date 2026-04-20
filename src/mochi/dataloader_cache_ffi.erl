-module(dataloader_cache_ffi).
-export([new/1, get/2, put/3, put_many/2, invalidate/2, clear/1]).

new(Name) ->
    TableName = list_to_atom("mochi_dl_" ++ binary_to_list(Name)),
    try
        ets:new(TableName, [named_table, set, public, {read_concurrency, true}])
    catch
        error:badarg -> TableName
    end,
    {shared_cache, TableName}.

get({shared_cache, Table}, Key) ->
    case ets:lookup(Table, Key) of
        [{_, Value}] -> {ok, Value};
        []           -> {error, nil}
    end.

put({shared_cache, Table}, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    nil.

put_many({shared_cache, Table}, Pairs) ->
    ets:insert(Table, [{K, V} || {K, V} <- Pairs]),
    nil.

invalidate({shared_cache, Table}, Key) ->
    ets:delete(Table, Key),
    nil.

clear({shared_cache, Table}) ->
    ets:delete_all_objects(Table),
    nil.
