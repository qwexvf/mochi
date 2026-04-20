-module(mochi@dataloader).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/dataloader.gleam").
-export([default_options/0, new_with_options/2, new/1, load_many/2, clear_cache/1, clear_key/2, prime/3, prime_error/3, load/2, int_loader/1, string_loader/1, int_batch_loader/1, string_batch_loader/1, int_loader_result/3, string_loader_result/3, int_key/1, string_key/1]).
-export_type([data_loader_options/0, data_loader_state/2, data_loader/2]).

-type data_loader_options() :: {data_loader_options, integer(), boolean()}.

-type data_loader_state(AGG, AGH) :: {data_loader_state,
        fun((list(AGG)) -> {ok, list({ok, AGH} | {error, binary()})} |
            {error, binary()}),
        data_loader_options(),
        gleam@dict:dict(AGG, {ok, AGH} | {error, binary()})}.

-opaque data_loader(AGI, AGJ) :: {data_loader, data_loader_state(AGI, AGJ)}.

-file("src/mochi/dataloader.gleam", 13).
-spec default_options() -> data_loader_options().
default_options() ->
    {data_loader_options, 100, true}.

-file("src/mochi/dataloader.gleam", 33).
-spec new_with_options(
    fun((list(AGY)) -> {ok, list({ok, AGZ} | {error, binary()})} |
        {error, binary()}),
    data_loader_options()
) -> data_loader(AGY, AGZ).
new_with_options(Batch_load_fn, Options) ->
    {data_loader, {data_loader_state, Batch_load_fn, Options, maps:new()}}.

-file("src/mochi/dataloader.gleam", 29).
-spec new(
    fun((list(AGS)) -> {ok, list({ok, AGT} | {error, binary()})} |
        {error, binary()})
) -> data_loader(AGS, AGT).
new(Batch_load_fn) ->
    new_with_options(Batch_load_fn, default_options()).

-file("src/mochi/dataloader.gleam", 55).
-spec load_many(data_loader(AHM, AHN), list(AHM)) -> {data_loader(AHM, AHN),
    list({ok, AHN} | {error, binary()})}.
load_many(Loader, Keys) ->
    case Keys of
        [] ->
            {Loader, []};

        _ ->
            {data_loader, State} = Loader,
            {Result_map, Uncached} = gleam@list:fold(
                Keys,
                {maps:new(), []},
                fun(Acc, Key) ->
                    {Known, To_fetch} = Acc,
                    case {erlang:element(3, erlang:element(3, State)),
                        gleam_stdlib:map_get(erlang:element(4, State), Key)} of
                        {true, {ok, R}} ->
                            {gleam@dict:insert(Known, Key, R), To_fetch};

                        {_, _} ->
                            {Known, [Key | To_fetch]}
                    end
                end
            ),
            Unique_uncached = gleam@list:unique(lists:reverse(Uncached)),
            case Unique_uncached of
                [] ->
                    Results = gleam@list:map(
                        Keys,
                        fun(K) -> case gleam_stdlib:map_get(Result_map, K) of
                                {ok, R@1} ->
                                    R@1;

                                {error, _} ->
                                    {error, <<"Key not found"/utf8>>}
                            end end
                    ),
                    {Loader, Results};

                _ ->
                    Chunks = gleam@list:sized_chunk(
                        Unique_uncached,
                        erlang:element(2, erlang:element(3, State))
                    ),
                    case gleam@list:try_map(
                        Chunks,
                        fun(Chunk) -> case (erlang:element(2, State))(Chunk) of
                                {error, E} ->
                                    {error,
                                        <<"Batch load failed: "/utf8, E/binary>>};

                                {ok, Results@1} ->
                                    case erlang:length(Results@1) =:= erlang:length(
                                        Chunk
                                    ) of
                                        false ->
                                            {error,
                                                <<"Batch returned wrong number of results"/utf8>>};

                                        true ->
                                            {ok,
                                                gleam@list:zip(Chunk, Results@1)}
                                    end
                            end end
                    ) of
                        {error, E@1} ->
                            {Loader,
                                gleam@list:map(
                                    Keys,
                                    fun(K@1) ->
                                        case gleam_stdlib:map_get(
                                            Result_map,
                                            K@1
                                        ) of
                                            {ok, R@2} ->
                                                R@2;

                                            {error, _} ->
                                                {error, E@1}
                                        end
                                    end
                                )};

                        {ok, Pairs_per_chunk} ->
                            Fresh = maps:from_list(
                                lists:append(Pairs_per_chunk)
                            ),
                            New_cache = case erlang:element(
                                3,
                                erlang:element(3, State)
                            ) of
                                true ->
                                    maps:merge(erlang:element(4, State), Fresh);

                                false ->
                                    erlang:element(4, State)
                            end,
                            New_loader = {data_loader,
                                {data_loader_state,
                                    erlang:element(2, State),
                                    erlang:element(3, State),
                                    New_cache}},
                            All_results = maps:merge(Result_map, Fresh),
                            Results@2 = gleam@list:map(
                                Keys,
                                fun(K@2) ->
                                    case gleam_stdlib:map_get(All_results, K@2) of
                                        {ok, R@3} ->
                                            R@3;

                                        {error, _} ->
                                            {error,
                                                <<"Key not found in batch results"/utf8>>}
                                    end
                                end
                            ),
                            {New_loader, Results@2}
                    end
            end
    end.

-file("src/mochi/dataloader.gleam", 137).
-spec clear_cache(data_loader(AHW, AHX)) -> data_loader(AHW, AHX).
clear_cache(Loader) ->
    {data_loader, State} = Loader,
    {data_loader,
        {data_loader_state,
            erlang:element(2, State),
            erlang:element(3, State),
            maps:new()}}.

-file("src/mochi/dataloader.gleam", 142).
-spec clear_key(data_loader(AIC, AID), AIC) -> data_loader(AIC, AID).
clear_key(Loader, Key) ->
    {data_loader, State} = Loader,
    {data_loader,
        {data_loader_state,
            erlang:element(2, State),
            erlang:element(3, State),
            gleam@dict:delete(erlang:element(4, State), Key)}}.

-file("src/mochi/dataloader.gleam", 150).
-spec prime(data_loader(AII, AIJ), AII, AIJ) -> data_loader(AII, AIJ).
prime(Loader, Key, Value) ->
    {data_loader, State} = Loader,
    case erlang:element(3, erlang:element(3, State)) of
        true ->
            {data_loader,
                {data_loader_state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    gleam@dict:insert(
                        erlang:element(4, State),
                        Key,
                        {ok, Value}
                    )}};

        false ->
            Loader
    end.

-file("src/mochi/dataloader.gleam", 168).
-spec prime_error(data_loader(AIO, AIP), AIO, binary()) -> data_loader(AIO, AIP).
prime_error(Loader, Key, Error) ->
    {data_loader, State} = Loader,
    case erlang:element(3, erlang:element(3, State)) of
        true ->
            {data_loader,
                {data_loader_state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    gleam@dict:insert(
                        erlang:element(4, State),
                        Key,
                        {error, Error}
                    )}};

        false ->
            Loader
    end.

-file("src/mochi/dataloader.gleam", 186).
-spec fetch_single(data_loader(AIU, AIV), AIU) -> {data_loader(AIU, AIV),
    {ok, AIV} | {error, binary()}}.
fetch_single(Loader, Key) ->
    {data_loader, State} = Loader,
    case (erlang:element(2, State))([Key]) of
        {ok, [Result]} ->
            New_cache = case erlang:element(3, erlang:element(3, State)) of
                true ->
                    gleam@dict:insert(erlang:element(4, State), Key, Result);

                false ->
                    erlang:element(4, State)
            end,
            {{data_loader,
                    {data_loader_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        New_cache}},
                Result};

        {ok, _} ->
            {Loader, {error, <<"Batch returned wrong number of results"/utf8>>}};

        {error, E} ->
            {Loader, {error, <<"Batch load failed: "/utf8, E/binary>>}}
    end.

-file("src/mochi/dataloader.gleam", 44).
-spec load(data_loader(AHE, AHF), AHE) -> {data_loader(AHE, AHF),
    {ok, AHF} | {error, binary()}}.
load(Loader, Key) ->
    {data_loader, State} = Loader,
    case {erlang:element(3, erlang:element(3, State)),
        gleam_stdlib:map_get(erlang:element(4, State), Key)} of
        {true, {ok, Cached}} ->
            {Loader, Cached};

        {_, _} ->
            fetch_single(Loader, Key)
    end.

-file("src/mochi/dataloader.gleam", 204).
-spec int_loader(
    fun((integer()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
int_loader(Lookup) ->
    new(
        fun(Keys) ->
            {ok,
                gleam@list:map(
                    Keys,
                    fun(Key) ->
                        case gleam@dynamic@decode:run(
                            Key,
                            {decoder, fun gleam@dynamic@decode:decode_int/1}
                        ) of
                            {ok, Id} ->
                                Lookup(Id);

                            {error, _} ->
                                {error, <<"Invalid integer key"/utf8>>}
                        end
                    end
                )}
        end
    ).

-file("src/mochi/dataloader.gleam", 219).
-spec string_loader(
    fun((binary()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
string_loader(Lookup) ->
    new(
        fun(Keys) ->
            {ok,
                gleam@list:map(
                    Keys,
                    fun(Key) ->
                        case gleam@dynamic@decode:run(
                            Key,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, S} ->
                                Lookup(S);

                            {error, _} ->
                                {error, <<"Invalid string key"/utf8>>}
                        end
                    end
                )}
        end
    ).

-file("src/mochi/dataloader.gleam", 234).
-spec int_batch_loader(
    fun((list(integer())) -> {ok, list(gleam@dynamic:dynamic_())} |
        {error, binary()})
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
int_batch_loader(Batch_lookup) ->
    new(
        fun(Keys) ->
            case gleam@list:try_map(
                Keys,
                fun(Key) ->
                    gleam@dynamic@decode:run(
                        Key,
                        {decoder, fun gleam@dynamic@decode:decode_int/1}
                    )
                end
            ) of
                {error, _} ->
                    {error, <<"int_batch_loader: key is not an integer"/utf8>>};

                {ok, Int_keys} ->
                    case Batch_lookup(Int_keys) of
                        {ok, Results} ->
                            {ok,
                                gleam@list:map(
                                    Results,
                                    fun(Field@0) -> {ok, Field@0} end
                                )};

                        {error, E} ->
                            {error, E}
                    end
            end
        end
    ).

-file("src/mochi/dataloader.gleam", 249).
-spec string_batch_loader(
    fun((list(binary())) -> {ok, list(gleam@dynamic:dynamic_())} |
        {error, binary()})
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
string_batch_loader(Batch_lookup) ->
    new(
        fun(Keys) ->
            case gleam@list:try_map(
                Keys,
                fun(Key) ->
                    gleam@dynamic@decode:run(
                        Key,
                        {decoder, fun gleam@dynamic@decode:decode_string/1}
                    )
                end
            ) of
                {error, _} ->
                    {error, <<"string_batch_loader: key is not a string"/utf8>>};

                {ok, String_keys} ->
                    case Batch_lookup(String_keys) of
                        {ok, Results} ->
                            {ok,
                                gleam@list:map(
                                    Results,
                                    fun(Field@0) -> {ok, Field@0} end
                                )};

                        {error, E} ->
                            {error, E}
                    end
            end
        end
    ).

-file("src/mochi/dataloader.gleam", 264).
-spec int_loader_result(
    fun((integer()) -> {ok, AJW} | {error, any()}),
    fun((AJW) -> gleam@dynamic:dynamic_()),
    binary()
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
int_loader_result(Find_fn, Encoder, Not_found_error) ->
    int_loader(fun(Id) -> case Find_fn(Id) of
                {ok, Value} ->
                    {ok, Encoder(Value)};

                {error, _} ->
                    {error, Not_found_error}
            end end).

-file("src/mochi/dataloader.gleam", 277).
-spec string_loader_result(
    fun((binary()) -> {ok, AKC} | {error, any()}),
    fun((AKC) -> gleam@dynamic:dynamic_()),
    binary()
) -> data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
string_loader_result(Find_fn, Encoder, Not_found_error) ->
    string_loader(fun(Key) -> case Find_fn(Key) of
                {ok, Value} ->
                    {ok, Encoder(Value)};

                {error, _} ->
                    {error, Not_found_error}
            end end).

-file("src/mochi/dataloader.gleam", 290).
-spec int_key(integer()) -> gleam@dynamic:dynamic_().
int_key(Id) ->
    gleam_stdlib:identity(Id).

-file("src/mochi/dataloader.gleam", 294).
-spec string_key(binary()) -> gleam@dynamic:dynamic_().
string_key(S) ->
    gleam_stdlib:identity(S).
