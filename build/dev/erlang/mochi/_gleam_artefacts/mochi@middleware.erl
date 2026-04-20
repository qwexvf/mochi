-module(mochi@middleware).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/middleware.gleam").
-export([new_resolution/3, set_value/2, set_error/2, put_private/3, get_private/2, has_value/1, has_error/1, new_pipeline/0, add_middleware/2, middleware/2, with_priority/2, with_filter/2, execute_with_middleware/5, to_executor_fn/1, logging_middleware/1, auth_middleware/2, rate_limit_middleware/3, caching_middleware/3, timing_middleware/2, transform_middleware/1, validation_middleware/1, error_wrapper_middleware/1]).
-export_type([resolver_context/0, resolution/0, middleware_def/0, field_filter/0, middleware_pipeline/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type resolver_context() :: {resolver_context,
        binary(),
        binary(),
        list(binary()),
        mochi@schema:resolver_info()}.

-type resolution() :: {resolution,
        gleam@option:option(gleam@dynamic:dynamic_()),
        gleam@option:option(binary()),
        binary(),
        binary(),
        resolver_context(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())}.

-type middleware_def() :: {middleware_def,
        binary(),
        integer(),
        gleam@option:option(field_filter()),
        fun((resolution(), fun((resolution()) -> resolution())) -> resolution())}.

-type field_filter() :: {specific_fields, list({binary(), binary()})} |
    {type_fields, list(binary())} |
    {named_fields, list(binary())} |
    all_fields.

-opaque middleware_pipeline() :: {middleware_pipeline, list(middleware_def())}.

-file("src/mochi/middleware.gleam", 89).
?DOC(" Create a new resolution state for a field\n").
-spec new_resolution(binary(), binary(), resolver_context()) -> resolution().
new_resolution(Field_name, Parent_type, Context) ->
    {resolution, none, none, Field_name, Parent_type, Context, maps:new()}.

-file("src/mochi/middleware.gleam", 105).
?DOC(" Set the resolved value\n").
-spec set_value(resolution(), gleam@dynamic:dynamic_()) -> resolution().
set_value(Resolution, Value) ->
    {resolution,
        {some, Value},
        none,
        erlang:element(4, Resolution),
        erlang:element(5, Resolution),
        erlang:element(6, Resolution),
        erlang:element(7, Resolution)}.

-file("src/mochi/middleware.gleam", 110).
?DOC(" Set an error on the resolution\n").
-spec set_error(resolution(), binary()) -> resolution().
set_error(Resolution, Error) ->
    {resolution,
        none,
        {some, Error},
        erlang:element(4, Resolution),
        erlang:element(5, Resolution),
        erlang:element(6, Resolution),
        erlang:element(7, Resolution)}.

-file("src/mochi/middleware.gleam", 115).
?DOC(" Add private data to the resolution\n").
-spec put_private(resolution(), binary(), gleam@dynamic:dynamic_()) -> resolution().
put_private(Resolution, Key, Value) ->
    {resolution,
        erlang:element(2, Resolution),
        erlang:element(3, Resolution),
        erlang:element(4, Resolution),
        erlang:element(5, Resolution),
        erlang:element(6, Resolution),
        gleam@dict:insert(erlang:element(7, Resolution), Key, Value)}.

-file("src/mochi/middleware.gleam", 124).
?DOC(" Get private data from the resolution\n").
-spec get_private(resolution(), binary()) -> gleam@option:option(gleam@dynamic:dynamic_()).
get_private(Resolution, Key) ->
    case gleam_stdlib:map_get(erlang:element(7, Resolution), Key) of
        {ok, V} ->
            {some, V};

        {error, _} ->
            none
    end.

-file("src/mochi/middleware.gleam", 132).
?DOC(" Check if resolution has a value\n").
-spec has_value(resolution()) -> boolean().
has_value(Resolution) ->
    gleam@option:is_some(erlang:element(2, Resolution)).

-file("src/mochi/middleware.gleam", 137).
?DOC(" Check if resolution has an error\n").
-spec has_error(resolution()) -> boolean().
has_error(Resolution) ->
    gleam@option:is_some(erlang:element(3, Resolution)).

-file("src/mochi/middleware.gleam", 146).
?DOC(" Create a new empty middleware pipeline\n").
-spec new_pipeline() -> middleware_pipeline().
new_pipeline() ->
    {middleware_pipeline, []}.

-file("src/mochi/middleware.gleam", 151).
?DOC(" Add middleware to the pipeline\n").
-spec add_middleware(middleware_pipeline(), middleware_def()) -> middleware_pipeline().
add_middleware(Pipeline, Mw) ->
    New_list = begin
        _pipe = [Mw | erlang:element(2, Pipeline)],
        gleam@list:sort(
            _pipe,
            fun(A, B) ->
                gleam@int:compare(erlang:element(3, A), erlang:element(3, B))
            end
        )
    end,
    {middleware_pipeline, New_list}.

-file("src/mochi/middleware.gleam", 162).
?DOC(" Create a basic middleware definition\n").
-spec middleware(
    binary(),
    fun((resolution(), fun((resolution()) -> resolution())) -> resolution())
) -> middleware_def().
middleware(Name, Mw_fn) ->
    {middleware_def, Name, 100, none, Mw_fn}.

-file("src/mochi/middleware.gleam", 172).
?DOC(" Set the priority of a middleware (lower runs first)\n").
-spec with_priority(middleware_def(), integer()) -> middleware_def().
with_priority(Mw, Priority) ->
    {middleware_def,
        erlang:element(2, Mw),
        Priority,
        erlang:element(4, Mw),
        erlang:element(5, Mw)}.

-file("src/mochi/middleware.gleam", 177).
?DOC(" Set a field filter on middleware\n").
-spec with_filter(middleware_def(), field_filter()) -> middleware_def().
with_filter(Mw, Filter) ->
    {middleware_def,
        erlang:element(2, Mw),
        erlang:element(3, Mw),
        {some, Filter},
        erlang:element(5, Mw)}.

-file("src/mochi/middleware.gleam", 182).
?DOC(" Check if a middleware applies to a given field\n").
-spec middleware_applies(middleware_def(), binary(), binary()) -> boolean().
middleware_applies(Mw, Parent_type, Field_name) ->
    case erlang:element(4, Mw) of
        none ->
            true;

        {some, all_fields} ->
            true;

        {some, {specific_fields, Pairs}} ->
            gleam@list:any(
                Pairs,
                fun(Pair) ->
                    (erlang:element(1, Pair) =:= Parent_type) andalso (erlang:element(
                        2,
                        Pair
                    )
                    =:= Field_name)
                end
            );

        {some, {type_fields, Types}} ->
            gleam@list:contains(Types, Parent_type);

        {some, {named_fields, Names}} ->
            gleam@list:contains(Names, Field_name)
    end.

-file("src/mochi/middleware.gleam", 267).
?DOC(" Execute the middleware chain recursively\n").
-spec execute_chain(
    list(middleware_def()),
    resolution(),
    fun((resolution()) -> resolution())
) -> resolution().
execute_chain(Middleware, Resolution, Next) ->
    case Middleware of
        [] ->
            Next(Resolution);

        [Mw | Rest] ->
            Continue = fun(Res) -> execute_chain(Rest, Res, Next) end,
            (erlang:element(5, Mw))(Resolution, Continue)
    end.

-file("src/mochi/middleware.gleam", 222).
?DOC(" Execute a resolver with the middleware pipeline\n").
-spec execute_with_middleware(
    middleware_pipeline(),
    binary(),
    mochi@schema:field_definition(),
    mochi@schema:resolver_info(),
    fun((mochi@schema:resolver_info()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
execute_with_middleware(Pipeline, Parent_type, Field_def, Info, Resolver) ->
    Context = {resolver_context,
        erlang:element(2, Field_def),
        Parent_type,
        [],
        Info},
    Resolution = new_resolution(
        erlang:element(2, Field_def),
        Parent_type,
        Context
    ),
    Applicable_middleware = gleam@list:filter(
        erlang:element(2, Pipeline),
        fun(Mw) ->
            middleware_applies(Mw, Parent_type, erlang:element(2, Field_def))
        end
    ),
    Final_resolution = execute_chain(
        Applicable_middleware,
        Resolution,
        fun(Res) -> case Resolver(Info) of
                {ok, Value} ->
                    set_value(Res, Value);

                {error, Msg} ->
                    set_error(Res, Msg)
            end end
    ),
    case erlang:element(3, Final_resolution) of
        {some, Err} ->
            {error, Err};

        none ->
            case erlang:element(2, Final_resolution) of
                {some, Value@1} ->
                    {ok, Value@1};

                none ->
                    {ok, gleam_stdlib:identity(nil)}
            end
    end.

-file("src/mochi/middleware.gleam", 215).
?DOC(
    " Convert a MiddlewarePipeline to a schema.MiddlewareFn\n"
    "\n"
    " This is used to wire a middleware pipeline into the execution context\n"
    " without creating a circular dependency between schema and middleware.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let pipeline = middleware.new_pipeline()\n"
    "   |> middleware.add_middleware(middleware.logging_middleware(io.println))\n"
    "\n"
    " let ctx = schema.execution_context(user_ctx)\n"
    "   |> schema.with_middleware(middleware.to_executor_fn(pipeline))\n"
    " ```\n"
).
-spec to_executor_fn(middleware_pipeline()) -> fun((binary(), mochi@schema:field_definition(), mochi@schema:resolver_info(), fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}).
to_executor_fn(Pipeline) ->
    fun(Parent_type, Field_def, Info, Resolver) ->
        execute_with_middleware(
            Pipeline,
            Parent_type,
            Field_def,
            Info,
            Resolver
        )
    end.

-file("src/mochi/middleware.gleam", 287).
?DOC(" Logging middleware that logs field resolution\n").
-spec logging_middleware(fun((binary()) -> nil)) -> middleware_def().
logging_middleware(Log_fn) ->
    middleware(
        <<"logging"/utf8>>,
        fun(Resolution, Next) ->
            Log_fn(
                <<<<<<"Resolving "/utf8,
                            (erlang:element(5, Resolution))/binary>>/binary,
                        "."/utf8>>/binary,
                    (erlang:element(4, Resolution))/binary>>
            ),
            Result = Next(Resolution),
            case erlang:element(3, Result) of
                {some, Err} ->
                    Log_fn(
                        <<<<<<<<<<"Error resolving "/utf8,
                                            (erlang:element(5, Resolution))/binary>>/binary,
                                        "."/utf8>>/binary,
                                    (erlang:element(4, Resolution))/binary>>/binary,
                                ": "/utf8>>/binary,
                            Err/binary>>
                    );

                none ->
                    Log_fn(
                        <<<<<<"Resolved "/utf8,
                                    (erlang:element(5, Resolution))/binary>>/binary,
                                "."/utf8>>/binary,
                            (erlang:element(4, Resolution))/binary>>
                    )
            end,
            Result
        end
    ).

-file("src/mochi/middleware.gleam", 313).
?DOC(" Authorization middleware that checks for a specific role in context\n").
-spec auth_middleware(
    fun((gleam@dynamic:dynamic_()) -> gleam@option:option(binary())),
    binary()
) -> middleware_def().
auth_middleware(Role_extractor, Required_role) ->
    _pipe = middleware(
        <<"auth"/utf8>>,
        fun(Resolution, Next) ->
            case Role_extractor(
                erlang:element(
                    2,
                    erlang:element(
                        4,
                        erlang:element(5, erlang:element(6, Resolution))
                    )
                )
            ) of
                {some, Role} when Role =:= Required_role ->
                    Next(Resolution);

                {some, _} ->
                    set_error(
                        Resolution,
                        <<<<"Forbidden: requires role '"/utf8,
                                Required_role/binary>>/binary,
                            "'"/utf8>>
                    );

                none ->
                    set_error(
                        Resolution,
                        <<"Unauthorized: authentication required"/utf8>>
                    )
            end
        end
    ),
    with_priority(_pipe, 10).

-file("src/mochi/middleware.gleam", 333).
?DOC(
    " Rate limiting middleware (simple in-memory version)\n"
    " Note: For production, use external state (Redis, etc.)\n"
).
-spec rate_limit_middleware(
    fun((resolution()) -> binary()),
    integer(),
    fun((binary(), integer()) -> boolean())
) -> middleware_def().
rate_limit_middleware(Key_extractor, Max_requests, Checker) ->
    _pipe = middleware(
        <<"rate_limit"/utf8>>,
        fun(Resolution, Next) ->
            Key = Key_extractor(Resolution),
            case Checker(Key, Max_requests) of
                true ->
                    Next(Resolution);

                false ->
                    set_error(Resolution, <<"Rate limit exceeded"/utf8>>)
            end
        end
    ),
    with_priority(_pipe, 5).

-file("src/mochi/middleware.gleam", 349).
?DOC(" Caching middleware that caches field results\n").
-spec caching_middleware(
    fun((resolution()) -> binary()),
    fun((binary()) -> gleam@option:option(gleam@dynamic:dynamic_())),
    fun((binary(), gleam@dynamic:dynamic_()) -> nil)
) -> middleware_def().
caching_middleware(Cache_key, Get_cached, Set_cached) ->
    _pipe = middleware(
        <<"cache"/utf8>>,
        fun(Resolution, Next) ->
            Key = Cache_key(Resolution),
            case Get_cached(Key) of
                {some, Cached} ->
                    set_value(Resolution, Cached);

                none ->
                    Result = Next(Resolution),
                    case erlang:element(2, Result) of
                        {some, Value} ->
                            Set_cached(Key, Value),
                            Result;

                        none ->
                            Result
                    end
            end
        end
    ),
    with_priority(_pipe, 20).

-file("src/mochi/middleware.gleam", 374).
?DOC(" Timing middleware that records field resolution time\n").
-spec timing_middleware(
    fun(() -> integer()),
    fun((binary(), binary(), integer()) -> nil)
) -> middleware_def().
timing_middleware(Get_time, Record_timing) ->
    _pipe = middleware(
        <<"timing"/utf8>>,
        fun(Resolution, Next) ->
            Start = Get_time(),
            Result = Next(Resolution),
            Duration = Get_time() - Start,
            Record_timing(
                erlang:element(5, Resolution),
                erlang:element(4, Resolution),
                Duration
            ),
            Result
        end
    ),
    with_priority(_pipe, 1).

-file("src/mochi/middleware.gleam", 389).
?DOC(" Transform middleware that transforms the resolved value\n").
-spec transform_middleware(
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_())
) -> middleware_def().
transform_middleware(Transform) ->
    _pipe = middleware(
        <<"transform"/utf8>>,
        fun(Resolution, Next) ->
            Result = Next(Resolution),
            case erlang:element(2, Result) of
                {some, Value} ->
                    set_value(Result, Transform(Value));

                none ->
                    Result
            end
        end
    ),
    with_priority(_pipe, 200).

-file("src/mochi/middleware.gleam", 401).
?DOC(" Validation middleware that validates arguments\n").
-spec validation_middleware(
    fun((mochi@schema:resolver_info()) -> {ok, nil} | {error, binary()})
) -> middleware_def().
validation_middleware(Validator) ->
    _pipe = middleware(
        <<"validation"/utf8>>,
        fun(Resolution, Next) ->
            case Validator(erlang:element(5, erlang:element(6, Resolution))) of
                {ok, _} ->
                    Next(Resolution);

                {error, Msg} ->
                    set_error(
                        Resolution,
                        <<"Validation error: "/utf8, Msg/binary>>
                    )
            end
        end
    ),
    with_priority(_pipe, 15).

-file("src/mochi/middleware.gleam", 414).
?DOC(" Error wrapping middleware that transforms errors\n").
-spec error_wrapper_middleware(fun((binary()) -> binary())) -> middleware_def().
error_wrapper_middleware(Wrapper) ->
    _pipe = middleware(
        <<"error_wrapper"/utf8>>,
        fun(Resolution, Next) ->
            Result = Next(Resolution),
            case erlang:element(3, Result) of
                {some, Err} ->
                    set_error(Result, Wrapper(Err));

                none ->
                    Result
            end
        end
    ),
    with_priority(_pipe, 250).
