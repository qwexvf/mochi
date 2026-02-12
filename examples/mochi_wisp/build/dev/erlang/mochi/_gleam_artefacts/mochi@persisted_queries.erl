-module(mochi@persisted_queries).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/persisted_queries.gleam").
-export([new/0, lookup/2, size/1, hash_query/1, with_queries/1, register/2, process_apq/3, parse_extension/1]).
-export_type([persisted_query_store/0, a_p_q_extension/0, a_p_q_result/0, a_p_q_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type persisted_query_store() :: {persisted_query_store,
        gleam@dict:dict(binary(), binary())}.

-type a_p_q_extension() :: {a_p_q_extension, integer(), binary()}.

-type a_p_q_result() :: {query_found, binary()} |
    {query_not_found, binary()} |
    {query_registered, binary()}.

-type a_p_q_error() :: persisted_query_not_found |
    invalid_hash |
    {hash_mismatch, binary(), binary()}.

-file("src/mochi/persisted_queries.gleam", 40).
?DOC(" Create a new empty persisted query store\n").
-spec new() -> persisted_query_store().
new() ->
    {persisted_query_store, maps:new()}.

-file("src/mochi/persisted_queries.gleam", 66).
?DOC(" Look up a query by its hash\n").
-spec lookup(persisted_query_store(), binary()) -> gleam@option:option(binary()).
lookup(Store, Hash) ->
    _pipe = gleam_stdlib:map_get(erlang:element(2, Store), Hash),
    gleam@option:from_result(_pipe).

-file("src/mochi/persisted_queries.gleam", 75).
?DOC(" Get the number of stored queries\n").
-spec size(persisted_query_store()) -> integer().
size(Store) ->
    maps:size(erlang:element(2, Store)).

-file("src/mochi/persisted_queries.gleam", 167).
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

-file("src/mochi/persisted_queries.gleam", 158).
-spec collapse_whitespace(binary()) -> binary().
collapse_whitespace(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\n"/utf8>>, <<" "/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\r"/utf8>>, <<" "/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\t"/utf8>>, <<" "/utf8>>),
    collapse_spaces(_pipe@3).

-file("src/mochi/persisted_queries.gleam", 152).
?DOC(
    " Normalize a query for consistent hashing\n"
    " Removes extra whitespace while preserving string literals\n"
).
-spec normalize_query(binary()) -> binary().
normalize_query(Query) ->
    _pipe = Query,
    _pipe@1 = gleam@string:trim(_pipe),
    collapse_whitespace(_pipe@1).

-file("src/mochi/persisted_queries.gleam", 139).
?DOC(" Compute SHA256 hash of a query string\n").
-spec hash_query(binary()) -> binary().
hash_query(Query) ->
    _pipe = Query,
    _pipe@1 = normalize_query(_pipe),
    mochi_hash_ffi:sha256_hex(_pipe@1).

-file("src/mochi/persisted_queries.gleam", 45).
?DOC(" Create a store with pre-registered queries\n").
-spec with_queries(list(binary())) -> persisted_query_store().
with_queries(Queries) ->
    Query_dict = gleam@list:fold(
        Queries,
        maps:new(),
        fun(Acc, Query) ->
            Hash = hash_query(Query),
            gleam@dict:insert(Acc, Hash, Query)
        end
    ),
    {persisted_query_store, Query_dict}.

-file("src/mochi/persisted_queries.gleam", 55).
?DOC(" Register a query in the store\n").
-spec register(persisted_query_store(), binary()) -> {persisted_query_store(),
    binary()}.
register(Store, Query) ->
    Hash = hash_query(Query),
    New_store = {persisted_query_store,
        gleam@dict:insert(erlang:element(2, Store), Hash, Query)},
    {New_store, Hash}.

-file("src/mochi/persisted_queries.gleam", 89).
?DOC(
    " Process an APQ request\n"
    "\n"
    " According to the APQ protocol:\n"
    " 1. Client sends hash without query -> return PersistedQueryNotFound\n"
    " 2. Client sends hash with query -> verify hash, store query, return query\n"
    " 3. Client sends hash, query found in store -> return stored query\n"
).
-spec process_apq(
    persisted_query_store(),
    gleam@option:option(binary()),
    binary()
) -> {ok, {persisted_query_store(), binary()}} | {error, a_p_q_error()}.
process_apq(Store, Query, Hash) ->
    case lookup(Store, Hash) of
        {some, Stored_query} ->
            {ok, {Store, Stored_query}};

        none ->
            case Query of
                none ->
                    {error, persisted_query_not_found};

                {some, Q} ->
                    Actual_hash = hash_query(Q),
                    case Actual_hash =:= Hash of
                        true ->
                            {New_store, _} = register(Store, Q),
                            {ok, {New_store, Q}};

                        false ->
                            {error, {hash_mismatch, Hash, Actual_hash}}
                    end
            end
    end.

-file("src/mochi/persisted_queries.gleam", 178).
-spec extract_apq_fields(gleam@dynamic:dynamic_()) -> {ok,
        {integer(), binary()}} |
    {error, nil}.
extract_apq_fields(_) ->
    {error, nil}.

-file("src/mochi/persisted_queries.gleam", 119).
?DOC(" Parse APQ extension from request extensions\n").
-spec parse_extension(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> gleam@option:option(a_p_q_extension()).
parse_extension(Extensions) ->
    case gleam_stdlib:map_get(Extensions, <<"persistedQuery"/utf8>>) of
        {error, _} ->
            none;

        {ok, Pq} ->
            case extract_apq_fields(Pq) of
                {ok, {Version, Hash}} ->
                    {some, {a_p_q_extension, Version, Hash}};

                {error, _} ->
                    none
            end
    end.
