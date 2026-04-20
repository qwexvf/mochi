-module(mochi@apq).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/apq.gleam").
-export([new/0, lookup/2, size/1, parse_extension/1, hash/1, with_queries/1, register/2, process/3]).
-export_type([store/0, extension/0, error/0]).

-type store() :: {store, gleam@dict:dict(binary(), binary())}.

-type extension() :: {extension, integer(), binary()}.

-type error() :: not_found | {hash_mismatch, binary(), binary()}.

-file("src/mochi/apq.gleam", 23).
-spec new() -> store().
new() ->
    {store, maps:new()}.

-file("src/mochi/apq.gleam", 40).
-spec lookup(store(), binary()) -> gleam@option:option(binary()).
lookup(Store, H) ->
    _pipe = gleam_stdlib:map_get(erlang:element(2, Store), H),
    gleam@option:from_result(_pipe).

-file("src/mochi/apq.gleam", 44).
-spec size(store()) -> integer().
size(Store) ->
    maps:size(erlang:element(2, Store)).

-file("src/mochi/apq.gleam", 72).
-spec parse_extension(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> gleam@option:option(extension()).
parse_extension(Extensions) ->
    case gleam_stdlib:map_get(Extensions, <<"persistedQuery"/utf8>>) of
        {error, _} ->
            none;

        {ok, Pq} ->
            case gleam@dynamic@decode:run(
                Pq,
                gleam@dynamic@decode:field(
                    <<"version"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(Version) ->
                        gleam@dynamic@decode:field(
                            <<"sha256Hash"/utf8>>,
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            fun(H) ->
                                gleam@dynamic@decode:success({Version, H})
                            end
                        )
                    end
                )
            ) of
                {ok, {Version@1, H@1}} ->
                    {some, {extension, Version@1, H@1}};

                {error, _} ->
                    none
            end
    end.

-file("src/mochi/apq.gleam", 112).
-spec collapse_spaces(binary()) -> binary().
collapse_spaces(S) ->
    case gleam_stdlib:contains_string(S, <<"  "/utf8>>) of
        true ->
            collapse_spaces(
                gleam@string:replace(S, <<"  "/utf8>>, <<" "/utf8>>)
            );

        false ->
            S
    end.

-file("src/mochi/apq.gleam", 104).
-spec normalise_whitespace(binary()) -> binary().
normalise_whitespace(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\n"/utf8>>, <<" "/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\r"/utf8>>, <<" "/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\t"/utf8>>, <<" "/utf8>>),
    collapse_spaces(_pipe@3).

-file("src/mochi/apq.gleam", 94).
-spec hash(binary()) -> binary().
hash(Query) ->
    _pipe = Query,
    _pipe@1 = gleam@string:trim(_pipe),
    _pipe@2 = normalise_whitespace(_pipe@1),
    _pipe@3 = gleam_stdlib:identity(_pipe@2),
    _pipe@4 = gleam@crypto:hash(sha256, _pipe@3),
    _pipe@5 = gleam_stdlib:base16_encode(_pipe@4),
    string:lowercase(_pipe@5).

-file("src/mochi/apq.gleam", 27).
-spec with_queries(list(binary())) -> store().
with_queries(Queries) ->
    Query_dict = gleam@list:fold(
        Queries,
        maps:new(),
        fun(Acc, Query) -> gleam@dict:insert(Acc, hash(Query), Query) end
    ),
    {store, Query_dict}.

-file("src/mochi/apq.gleam", 35).
-spec register(store(), binary()) -> {store(), binary()}.
register(Store, Query) ->
    H = hash(Query),
    {{store, gleam@dict:insert(erlang:element(2, Store), H, Query)}, H}.

-file("src/mochi/apq.gleam", 48).
-spec process(store(), gleam@option:option(binary()), binary()) -> {ok,
        {store(), binary()}} |
    {error, error()}.
process(Store, Query, H) ->
    case lookup(Store, H) of
        {some, Stored} ->
            {ok, {Store, Stored}};

        none ->
            case Query of
                none ->
                    {error, not_found};

                {some, Q} ->
                    Actual = hash(Q),
                    case Actual =:= H of
                        true ->
                            {New_store, _} = register(Store, Q),
                            {ok, {New_store, Q}};

                        false ->
                            {error, {hash_mismatch, H, Actual}}
                    end
            end
    end.
