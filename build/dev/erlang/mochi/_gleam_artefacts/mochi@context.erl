-module(mochi@context).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/context.gleam").
-export([request_info/3, get_header/2, get_authorization/1, get_bearer_token/1, new_pipeline/0, add_builder/2, build_context/3, try_build_context/3, to_dynamic/1, add_to_context/2, add_to_context_or/3, require/1, transform/1, bearer_token_builder/0, require_bearer_token/0, request_metadata_builder/0, headers_builder/0]).
-export_type([request_info/0, context_pipeline/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type request_info() :: {request_info,
        gleam@dict:dict(binary(), binary()),
        binary(),
        binary()}.

-opaque context_pipeline() :: {context_pipeline,
        list(fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
                gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
            {error, binary()}))}.

-file("src/mochi/context.gleam", 43).
?DOC(" Create a new RequestInfo\n").
-spec request_info(gleam@dict:dict(binary(), binary()), binary(), binary()) -> request_info().
request_info(Headers, Method, Path) ->
    {request_info, Headers, Method, Path}.

-file("src/mochi/context.gleam", 52).
?DOC(" Get a header value from request info (case-insensitive)\n").
-spec get_header(request_info(), binary()) -> {ok, binary()} | {error, nil}.
get_header(Info, Name) ->
    Lower_name = string:lowercase(Name),
    _pipe = maps:to_list(erlang:element(2, Info)),
    gleam@list:find_map(
        _pipe,
        fun(Pair) ->
            {K, V} = Pair,
            case string:lowercase(K) =:= Lower_name of
                true ->
                    {ok, V};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/mochi/context.gleam", 65).
?DOC(" Get the Authorization header value\n").
-spec get_authorization(request_info()) -> {ok, binary()} | {error, nil}.
get_authorization(Info) ->
    get_header(Info, <<"authorization"/utf8>>).

-file("src/mochi/context.gleam", 70).
?DOC(" Get bearer token from Authorization header\n").
-spec get_bearer_token(request_info()) -> {ok, binary()} | {error, nil}.
get_bearer_token(Info) ->
    case get_authorization(Info) of
        {ok, Auth} ->
            case Auth of
                <<"Bearer "/utf8, Token/binary>> ->
                    {ok, Token};

                <<"bearer "/utf8, Token@1/binary>> ->
                    {ok, Token@1};

                _ ->
                    {error, nil}
            end;

        {error, _} ->
            {error, nil}
    end.

-file("src/mochi/context.gleam", 88).
?DOC(" Create a new empty context pipeline\n").
-spec new_pipeline() -> context_pipeline().
new_pipeline() ->
    {context_pipeline, []}.

-file("src/mochi/context.gleam", 94).
?DOC(
    " Add a context builder to the pipeline\n"
    " Builders execute in the order they are added\n"
).
-spec add_builder(
    context_pipeline(),
    fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
            gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
        {error, binary()})
) -> context_pipeline().
add_builder(Pipeline, Builder) ->
    {context_pipeline, lists:append(erlang:element(2, Pipeline), [Builder])}.

-file("src/mochi/context.gleam", 103).
?DOC(
    " Build the execution context by running all builders in sequence\n"
    " Each builder receives the output of the previous builder\n"
).
-spec build_context(
    context_pipeline(),
    request_info(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> {ok, gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}.
build_context(Pipeline, Request, Initial) ->
    gleam@list:fold(
        erlang:element(2, Pipeline),
        {ok, Initial},
        fun(Acc, Builder) -> case Acc of
                {ok, Ctx} ->
                    Builder(Request, Ctx);

                {error, E} ->
                    {error, E}
            end end
    ).

-file("src/mochi/context.gleam", 117).
?DOC(" Try to build context, returning initial context on any error\n").
-spec try_build_context(
    context_pipeline(),
    request_info(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
try_build_context(Pipeline, Request, Initial) ->
    _pipe = build_context(Pipeline, Request, Initial),
    gleam@result:unwrap(_pipe, Initial).

-file("src/mochi/context.gleam", 127).
?DOC(" Convert context dict to Dynamic for use with ExecutionContext\n").
-spec to_dynamic(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
to_dynamic(Ctx) ->
    gleam_stdlib:identity(Ctx).

-file("src/mochi/context.gleam", 136).
?DOC(" Create a builder that adds a value to the context dict\n").
-spec add_to_context(
    binary(),
    fun((request_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
add_to_context(Key, Extractor) ->
    fun(Request, Ctx) -> case Extractor(Request) of
            {ok, Value} ->
                {ok, gleam@dict:insert(Ctx, Key, Value)};

            {error, E} ->
                {error, E}
        end end.

-file("src/mochi/context.gleam", 149).
?DOC(" Create a builder that adds a value to context, with a fallback on error\n").
-spec add_to_context_or(
    binary(),
    fun((request_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}),
    gleam@dynamic:dynamic_()
) -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
add_to_context_or(Key, Extractor, Default) ->
    fun(Request, Ctx) ->
        Value = case Extractor(Request) of
            {ok, V} ->
                V;

            {error, _} ->
                Default
        end,
        {ok, gleam@dict:insert(Ctx, Key, Value)}
    end.

-file("src/mochi/context.gleam", 164).
?DOC(" Create a builder that validates a condition or fails\n").
-spec require(
    fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
            nil} |
        {error, binary()})
) -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
require(Validator) ->
    fun(Request, Ctx) -> case Validator(Request, Ctx) of
            {ok, _} ->
                {ok, Ctx};

            {error, E} ->
                {error, E}
        end end.

-file("src/mochi/context.gleam", 176).
?DOC(" Create a builder that transforms the entire context\n").
-spec transform(
    fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
            gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
        {error, binary()})
) -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
transform(Transformer) ->
    Transformer.

-file("src/mochi/context.gleam", 188).
?DOC(" Builder that extracts the bearer token and adds it to context as \"token\"\n").
-spec bearer_token_builder() -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
bearer_token_builder() ->
    add_to_context_or(
        <<"token"/utf8>>,
        fun(Request) -> _pipe = get_bearer_token(Request),
            _pipe@1 = gleam@result:map(_pipe, fun gleam_stdlib:identity/1),
            gleam@result:map_error(
                _pipe@1,
                fun(_) -> <<"No bearer token found"/utf8>> end
            ) end,
        gleam_stdlib:identity(nil)
    ).

-file("src/mochi/context.gleam", 201).
?DOC(" Builder that requires a bearer token to be present\n").
-spec require_bearer_token() -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
require_bearer_token() ->
    require(fun(Request, _) -> case get_bearer_token(Request) of
                {ok, _} ->
                    {ok, nil};

                {error, _} ->
                    {error,
                        <<"Authorization required: Bearer token missing"/utf8>>}
            end end).

-file("src/mochi/context.gleam", 211).
?DOC(" Builder that adds request metadata to context\n").
-spec request_metadata_builder() -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
request_metadata_builder() ->
    fun(Request, Ctx) ->
        Metadata = maps:from_list(
            [{<<"method"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, Request))},
                {<<"path"/utf8>>,
                    gleam_stdlib:identity(erlang:element(4, Request))}]
        ),
        {ok,
            gleam@dict:insert(
                Ctx,
                <<"request"/utf8>>,
                gleam_stdlib:identity(Metadata)
            )}
    end.

-file("src/mochi/context.gleam", 223).
?DOC(" Builder that adds all headers to context under \"headers\" key\n").
-spec headers_builder() -> fun((request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, binary()}).
headers_builder() ->
    fun(Request, Ctx) ->
        Headers_dynamic = gleam@dict:map_values(
            erlang:element(2, Request),
            fun(_, V) -> gleam_stdlib:identity(V) end
        ),
        {ok,
            gleam@dict:insert(
                Ctx,
                <<"headers"/utf8>>,
                gleam_stdlib:identity(Headers_dynamic)
            )}
    end.
