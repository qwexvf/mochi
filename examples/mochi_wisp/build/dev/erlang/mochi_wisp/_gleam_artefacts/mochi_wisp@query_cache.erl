-module(mochi_wisp@query_cache).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/query_cache.gleam").
-export([init/0, get_or_parse/1, get_or_parse_fast/1, stats/0, clear/0, size/0]).
-export_type([cache_stats/0, fast_parse_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type cache_stats() :: {cache_stats, integer(), integer(), integer()}.

-type fast_parse_error() :: {fast_parse_error, binary()}.

-file("src/mochi_wisp/query_cache.gleam", 16).
?DOC(" Initialize the query cache (call once at startup)\n").
-spec init() -> nil.
init() ->
    mochi_query_cache_ffi:init().

-file("src/mochi_wisp/query_cache.gleam", 56).
-spec format_fast_error(mochi_wisp@fast_parser:parse_error()) -> binary().
format_fast_error(Error) ->
    case Error of
        {lex_error, Msg} ->
            <<"Lex error: "/utf8, Msg/binary>>;

        {unexpected_token, Expected, Got} ->
            <<<<<<"Unexpected token: expected "/utf8, Expected/binary>>/binary,
                    ", got "/utf8>>/binary,
                Got/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"Unexpected end of input: expected "/utf8, Expected@1/binary>>
    end.

-file("src/mochi_wisp/query_cache.gleam", 19).
?DOC(" Get a parsed query from cache, or parse and cache it (using standard parser)\n").
-spec get_or_parse(binary()) -> {ok, mochi@ast:document()} |
    {error, mochi@parser:parse_error()}.
get_or_parse(Query) ->
    case mochi_query_cache_ffi:get(Query) of
        {some, Doc} ->
            {ok, Doc};

        none ->
            case mochi@parser:parse(Query) of
                {ok, Doc@1} ->
                    mochi_query_cache_ffi:put(Query, Doc@1),
                    {ok, Doc@1};

                {error, E} ->
                    {error, E}
            end
    end.

-file("src/mochi_wisp/query_cache.gleam", 41).
?DOC(
    " Get a parsed query from cache, or parse using FAST parser and cache it\n"
    " Uses O(n) binary lexer and cursor-based parser vs O(n²) standard parser\n"
).
-spec get_or_parse_fast(binary()) -> {ok, mochi@ast:document()} |
    {error, fast_parse_error()}.
get_or_parse_fast(Query) ->
    case mochi_query_cache_ffi:get(Query) of
        {some, Doc} ->
            {ok, Doc};

        none ->
            case mochi_wisp@fast_parser:parse(Query) of
                {ok, Doc@1} ->
                    mochi_query_cache_ffi:put(Query, Doc@1),
                    {ok, Doc@1};

                {error, E} ->
                    {error, {fast_parse_error, format_fast_error(E)}}
            end
    end.

-file("src/mochi_wisp/query_cache.gleam", 76).
?DOC(" Get cache statistics\n").
-spec stats() -> cache_stats().
stats() ->
    mochi_query_cache_ffi:stats().

-file("src/mochi_wisp/query_cache.gleam", 80).
?DOC(" Clear the cache\n").
-spec clear() -> nil.
clear() ->
    mochi_query_cache_ffi:clear().

-file("src/mochi_wisp/query_cache.gleam", 84).
?DOC(" Get cache size\n").
-spec size() -> integer().
size() ->
    mochi_query_cache_ffi:size().
