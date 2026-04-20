-module(mochi@telemetry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/telemetry.gleam").
-export([get_timestamp_ns/0, disabled/0, with_handler/1, with_field_tracking/1, without_field_tracking/1, with_dataloader_tracking/1, new_context/1, from_option/1, emit/2, emit_if_present/2, record_parse_start/1, record_parse_end/2, record_validation_start/1, record_validation_end/3, record_operation_start/3, record_operation_end/4, record_field_start/4, record_field_end/5, record_dataloader_batch/4, record_custom/3, build_tracing_extension/1, build_metrics_summary/1, format_duration/1, format_metrics_summary/1, to_schema_fn/1]).
-export_type([operation_type/0, telemetry_event/0, telemetry_config/0, telemetry_context/0, metrics_summary/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type operation_type() :: 'query' | mutation | subscription.

-type telemetry_event() :: {parse_start, integer()} |
    {parse_end, integer(), boolean()} |
    {validation_start, integer()} |
    {validation_end, integer(), boolean(), integer()} |
    {operation_start,
        integer(),
        gleam@option:option(binary()),
        operation_type()} |
    {operation_end,
        integer(),
        gleam@option:option(binary()),
        boolean(),
        integer()} |
    {field_resolve_start, integer(), binary(), binary(), list(binary())} |
    {field_resolve_end,
        integer(),
        binary(),
        binary(),
        list(binary()),
        boolean(),
        integer()} |
    {data_loader_batch, integer(), binary(), integer(), integer()} |
    {custom,
        integer(),
        binary(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())}.

-type telemetry_config() :: {telemetry_config,
        boolean(),
        boolean(),
        boolean(),
        fun((telemetry_event()) -> nil)}.

-type telemetry_context() :: {telemetry_context,
        telemetry_config(),
        list(telemetry_event()),
        gleam@option:option(integer()),
        gleam@dict:dict(binary(), {integer(), integer()}),
        list(binary())}.

-type metrics_summary() :: {metrics_summary,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        list({binary(), integer()})}.

-file("src/mochi/telemetry.gleam", 110).
?DOC(" Get current timestamp in nanoseconds (monotonic clock)\n").
-spec get_timestamp_ns() -> integer().
get_timestamp_ns() ->
    mochi_time_ffi:monotonic_time_ns().

-file("src/mochi/telemetry.gleam", 117).
?DOC(" Create a disabled telemetry config\n").
-spec disabled() -> telemetry_config().
disabled() ->
    {telemetry_config, false, false, false, fun(_) -> nil end}.

-file("src/mochi/telemetry.gleam", 127).
?DOC(" Create an enabled telemetry config with a handler\n").
-spec with_handler(fun((telemetry_event()) -> nil)) -> telemetry_config().
with_handler(Handler) ->
    {telemetry_config, true, false, true, Handler}.

-file("src/mochi/telemetry.gleam", 137).
?DOC(" Enable field-level tracking (can be expensive for large queries)\n").
-spec with_field_tracking(telemetry_config()) -> telemetry_config().
with_field_tracking(Config) ->
    {telemetry_config,
        erlang:element(2, Config),
        true,
        erlang:element(4, Config),
        erlang:element(5, Config)}.

-file("src/mochi/telemetry.gleam", 142).
?DOC(" Disable field-level tracking\n").
-spec without_field_tracking(telemetry_config()) -> telemetry_config().
without_field_tracking(Config) ->
    {telemetry_config,
        erlang:element(2, Config),
        false,
        erlang:element(4, Config),
        erlang:element(5, Config)}.

-file("src/mochi/telemetry.gleam", 147).
?DOC(" Enable DataLoader tracking\n").
-spec with_dataloader_tracking(telemetry_config()) -> telemetry_config().
with_dataloader_tracking(Config) ->
    {telemetry_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        true,
        erlang:element(5, Config)}.

-file("src/mochi/telemetry.gleam", 156).
?DOC(" Create a new telemetry context from config\n").
-spec new_context(telemetry_config()) -> telemetry_context().
new_context(Config) ->
    {telemetry_context, Config, [], none, maps:new(), []}.

-file("src/mochi/telemetry.gleam", 167).
?DOC(" Create a context from an Option config\n").
-spec from_option(gleam@option:option(telemetry_config())) -> gleam@option:option(telemetry_context()).
from_option(Config) ->
    case Config of
        {some, C} ->
            {some, new_context(C)};

        none ->
            none
    end.

-file("src/mochi/telemetry.gleam", 179).
?DOC(" Emit an event to the telemetry context\n").
-spec emit(telemetry_context(), telemetry_event()) -> telemetry_context().
emit(Ctx, Event) ->
    case erlang:element(2, erlang:element(2, Ctx)) of
        false ->
            Ctx;

        true ->
            (erlang:element(5, erlang:element(2, Ctx)))(Event),
            {telemetry_context,
                erlang:element(2, Ctx),
                [Event | erlang:element(3, Ctx)],
                erlang:element(4, Ctx),
                erlang:element(5, Ctx),
                erlang:element(6, Ctx)}
    end.

-file("src/mochi/telemetry.gleam", 192).
?DOC(" Emit an event if telemetry context is present\n").
-spec emit_if_present(
    gleam@option:option(telemetry_context()),
    telemetry_event()
) -> gleam@option:option(telemetry_context()).
emit_if_present(Ctx, Event) ->
    case Ctx of
        {some, C} ->
            {some, emit(C, Event)};

        none ->
            none
    end.

-file("src/mochi/telemetry.gleam", 207).
?DOC(" Record parse start\n").
-spec record_parse_start(telemetry_context()) -> telemetry_context().
record_parse_start(Ctx) ->
    emit(Ctx, {parse_start, mochi_time_ffi:monotonic_time_ns()}).

-file("src/mochi/telemetry.gleam", 212).
?DOC(" Record parse end\n").
-spec record_parse_end(telemetry_context(), boolean()) -> telemetry_context().
record_parse_end(Ctx, Success) ->
    emit(Ctx, {parse_end, mochi_time_ffi:monotonic_time_ns(), Success}).

-file("src/mochi/telemetry.gleam", 220).
?DOC(" Record validation start\n").
-spec record_validation_start(telemetry_context()) -> telemetry_context().
record_validation_start(Ctx) ->
    emit(Ctx, {validation_start, mochi_time_ffi:monotonic_time_ns()}).

-file("src/mochi/telemetry.gleam", 225).
?DOC(" Record validation end\n").
-spec record_validation_end(telemetry_context(), boolean(), integer()) -> telemetry_context().
record_validation_end(Ctx, Success, Error_count) ->
    emit(
        Ctx,
        {validation_end,
            mochi_time_ffi:monotonic_time_ns(),
            Success,
            Error_count}
    ).

-file("src/mochi/telemetry.gleam", 234).
?DOC(" Record operation start\n").
-spec record_operation_start(
    telemetry_context(),
    gleam@option:option(binary()),
    operation_type()
) -> telemetry_context().
record_operation_start(Ctx, Operation_name, Operation_type) ->
    Timestamp = mochi_time_ffi:monotonic_time_ns(),
    Ctx@1 = {telemetry_context,
        erlang:element(2, Ctx),
        erlang:element(3, Ctx),
        {some, Timestamp},
        erlang:element(5, Ctx),
        erlang:element(6, Ctx)},
    emit(Ctx@1, {operation_start, Timestamp, Operation_name, Operation_type}).

-file("src/mochi/telemetry.gleam", 245).
?DOC(" Record operation end\n").
-spec record_operation_end(
    telemetry_context(),
    gleam@option:option(binary()),
    boolean(),
    integer()
) -> telemetry_context().
record_operation_end(Ctx, Operation_name, Success, Error_count) ->
    emit(
        Ctx,
        {operation_end,
            mochi_time_ffi:monotonic_time_ns(),
            Operation_name,
            Success,
            Error_count}
    ).

-file("src/mochi/telemetry.gleam", 258).
?DOC(" Record field resolve start (if field tracking is enabled)\n").
-spec record_field_start(
    telemetry_context(),
    binary(),
    binary(),
    list(binary())
) -> telemetry_context().
record_field_start(Ctx, Field_name, Parent_type, Path) ->
    case erlang:element(3, erlang:element(2, Ctx)) of
        false ->
            Ctx;

        true ->
            Timestamp = mochi_time_ffi:monotonic_time_ns(),
            Key = <<<<Parent_type/binary, "."/utf8>>/binary, Field_name/binary>>,
            Ctx@1 = {telemetry_context,
                erlang:element(2, Ctx),
                erlang:element(3, Ctx),
                erlang:element(4, Ctx),
                gleam@dict:insert(erlang:element(5, Ctx), Key, {Timestamp, 0}),
                Path},
            emit(
                Ctx@1,
                {field_resolve_start, Timestamp, Field_name, Parent_type, Path}
            )
    end.

-file("src/mochi/telemetry.gleam", 281).
?DOC(" Record field resolve end (if field tracking is enabled)\n").
-spec record_field_end(
    telemetry_context(),
    binary(),
    binary(),
    list(binary()),
    boolean()
) -> telemetry_context().
record_field_end(Ctx, Field_name, Parent_type, Path, Success) ->
    case erlang:element(3, erlang:element(2, Ctx)) of
        false ->
            Ctx;

        true ->
            Timestamp = mochi_time_ffi:monotonic_time_ns(),
            Key = <<<<Parent_type/binary, "."/utf8>>/binary, Field_name/binary>>,
            Duration = case gleam_stdlib:map_get(erlang:element(5, Ctx), Key) of
                {ok, {Start, _}} ->
                    Timestamp - Start;

                {error, _} ->
                    0
            end,
            Ctx@1 = {telemetry_context,
                erlang:element(2, Ctx),
                erlang:element(3, Ctx),
                erlang:element(4, Ctx),
                gleam@dict:insert(erlang:element(5, Ctx), Key, {0, Duration}),
                erlang:element(6, Ctx)},
            emit(
                Ctx@1,
                {field_resolve_end,
                    Timestamp,
                    Field_name,
                    Parent_type,
                    Path,
                    Success,
                    Duration}
            )
    end.

-file("src/mochi/telemetry.gleam", 318).
?DOC(" Record DataLoader batch execution\n").
-spec record_dataloader_batch(
    telemetry_context(),
    binary(),
    integer(),
    integer()
) -> telemetry_context().
record_dataloader_batch(Ctx, Loader_name, Batch_size, Duration_ns) ->
    case erlang:element(4, erlang:element(2, Ctx)) of
        false ->
            Ctx;

        true ->
            emit(
                Ctx,
                {data_loader_batch,
                    mochi_time_ffi:monotonic_time_ns(),
                    Loader_name,
                    Batch_size,
                    Duration_ns}
            )
    end.

-file("src/mochi/telemetry.gleam", 340).
?DOC(" Record a custom event\n").
-spec record_custom(
    telemetry_context(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> telemetry_context().
record_custom(Ctx, Name, Data) ->
    emit(Ctx, {custom, mochi_time_ffi:monotonic_time_ns(), Name, Data}).

-file("src/mochi/telemetry.gleam", 409).
-spec is_parse_start(telemetry_event()) -> gleam@option:option(integer()).
is_parse_start(E) ->
    case E of
        {parse_start, T} ->
            {some, T};

        _ ->
            none
    end.

-file("src/mochi/telemetry.gleam", 416).
-spec is_parse_end(telemetry_event()) -> gleam@option:option(integer()).
is_parse_end(E) ->
    case E of
        {parse_end, T, _} ->
            {some, T};

        _ ->
            none
    end.

-file("src/mochi/telemetry.gleam", 423).
-spec is_validation_start(telemetry_event()) -> gleam@option:option(integer()).
is_validation_start(E) ->
    case E of
        {validation_start, T} ->
            {some, T};

        _ ->
            none
    end.

-file("src/mochi/telemetry.gleam", 430).
-spec is_validation_end(telemetry_event()) -> gleam@option:option(integer()).
is_validation_end(E) ->
    case E of
        {validation_end, T, _, _} ->
            {some, T};

        _ ->
            none
    end.

-file("src/mochi/telemetry.gleam", 437).
-spec find_phase_timing(
    list(telemetry_event()),
    fun((telemetry_event()) -> gleam@option:option(integer())),
    fun((telemetry_event()) -> gleam@option:option(integer()))
) -> {integer(), integer()}.
find_phase_timing(Events, Start_check, End_check) ->
    Start = begin
        _pipe = gleam@list:find_map(Events, fun(E) -> case Start_check(E) of
                    {some, V} ->
                        {ok, V};

                    none ->
                        {error, nil}
                end end),
        gleam@result:unwrap(_pipe, 0)
    end,
    End = begin
        _pipe@1 = gleam@list:find_map(Events, fun(E@1) -> case End_check(E@1) of
                    {some, V@1} ->
                        {ok, V@1};

                    none ->
                        {error, nil}
                end end),
        gleam@result:unwrap(_pipe@1, 0)
    end,
    {Start, End - Start}.

-file("src/mochi/telemetry.gleam", 393).
-spec build_phase_timing(list(telemetry_event()), binary()) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
build_phase_timing(Events, Phase) ->
    {Start, Duration} = case Phase of
        <<"parse"/utf8>> ->
            find_phase_timing(Events, fun is_parse_start/1, fun is_parse_end/1);

        <<"validation"/utf8>> ->
            find_phase_timing(
                Events,
                fun is_validation_start/1,
                fun is_validation_end/1
            );

        _ ->
            {0, 0}
    end,
    maps:from_list(
        [{<<"startOffset"/utf8>>, gleam_stdlib:identity(Start)},
            {<<"duration"/utf8>>, gleam_stdlib:identity(Duration)}]
    ).

-file("src/mochi/telemetry.gleam", 461).
-spec build_resolver_list(list(telemetry_event())) -> list(gleam@dynamic:dynamic_()).
build_resolver_list(Events) ->
    _pipe = Events,
    gleam@list:filter_map(_pipe, fun(E) -> case E of
                {field_resolve_end, _, Field, Parent, Path, _, Duration} ->
                    {ok,
                        gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"path"/utf8>>, gleam_stdlib:identity(Path)},
                                    {<<"parentType"/utf8>>,
                                        gleam_stdlib:identity(Parent)},
                                    {<<"fieldName"/utf8>>,
                                        gleam_stdlib:identity(Field)},
                                    {<<"returnType"/utf8>>,
                                        gleam_stdlib:identity(<<""/utf8>>)},
                                    {<<"startOffset"/utf8>>,
                                        gleam_stdlib:identity(0)},
                                    {<<"duration"/utf8>>,
                                        gleam_stdlib:identity(Duration)}]
                            )
                        )};

                _ ->
                    {error, nil}
            end end).

-file("src/mochi/telemetry.gleam", 354).
?DOC(
    " Build an Apollo-compatible tracing extension from the telemetry context\n"
    " This can be included in the GraphQL response extensions\n"
).
-spec build_tracing_extension(telemetry_context()) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
build_tracing_extension(Ctx) ->
    Version = 1,
    {Start_time, End_time, Duration} = case erlang:element(4, Ctx) of
        {some, Start} ->
            End = mochi_time_ffi:monotonic_time_ns(),
            {Start, End, End - Start};

        none ->
            {0, 0, 0}
    end,
    Resolvers = build_resolver_list(erlang:element(3, Ctx)),
    Parsing = build_phase_timing(erlang:element(3, Ctx), <<"parse"/utf8>>),
    Validation = build_phase_timing(
        erlang:element(3, Ctx),
        <<"validation"/utf8>>
    ),
    maps:from_list(
        [{<<"version"/utf8>>, gleam_stdlib:identity(Version)},
            {<<"startTime"/utf8>>, gleam_stdlib:identity(Start_time)},
            {<<"endTime"/utf8>>, gleam_stdlib:identity(End_time)},
            {<<"duration"/utf8>>, gleam_stdlib:identity(Duration)},
            {<<"parsing"/utf8>>, gleam_stdlib:identity(Parsing)},
            {<<"validation"/utf8>>, gleam_stdlib:identity(Validation)},
            {<<"execution"/utf8>>,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"resolvers"/utf8>>,
                                gleam_stdlib:identity(Resolvers)}]
                    )
                )}]
    ).

-file("src/mochi/telemetry.gleam", 508).
?DOC(" Build a metrics summary from the telemetry context\n").
-spec build_metrics_summary(telemetry_context()) -> metrics_summary().
build_metrics_summary(Ctx) ->
    Total = case erlang:element(4, Ctx) of
        {some, Start} ->
            mochi_time_ffi:monotonic_time_ns() - Start;

        none ->
            0
    end,
    Parse = find_phase_timing(
        erlang:element(3, Ctx),
        fun is_parse_start/1,
        fun is_parse_end/1
    ),
    Validation = find_phase_timing(
        erlang:element(3, Ctx),
        fun is_validation_start/1,
        fun is_validation_end/1
    ),
    Field_count = begin
        _pipe = erlang:element(3, Ctx),
        gleam@list:count(_pipe, fun(E) -> case E of
                    {field_resolve_end, _, _, _, _, _, _} ->
                        true;

                    _ ->
                        false
                end end)
    end,
    Error_count = begin
        _pipe@1 = erlang:element(3, Ctx),
        gleam@list:count(_pipe@1, fun(E@1) -> case E@1 of
                    {field_resolve_end, _, _, _, _, false, _} ->
                        true;

                    {operation_end, _, _, false, _} ->
                        true;

                    _ ->
                        false
                end end)
    end,
    Slowest = begin
        _pipe@2 = erlang:element(5, Ctx),
        _pipe@3 = maps:to_list(_pipe@2),
        _pipe@4 = gleam@list:map(
            _pipe@3,
            fun(Kv) ->
                {erlang:element(1, Kv),
                    erlang:element(2, erlang:element(2, Kv))}
            end
        ),
        _pipe@5 = gleam@list:sort(
            _pipe@4,
            fun(A, B) ->
                gleam@int:compare(erlang:element(2, B), erlang:element(2, A))
            end
        ),
        gleam@list:take(_pipe@5, 10)
    end,
    {metrics_summary,
        Total,
        erlang:element(2, Parse),
        erlang:element(2, Validation),
        Field_count,
        Error_count,
        Slowest}.

-file("src/mochi/telemetry.gleam", 556).
?DOC(" Format duration in nanoseconds to human-readable string\n").
-spec format_duration(integer()) -> binary().
format_duration(Ns) ->
    case Ns of
        N when N < 1000 ->
            <<(erlang:integer_to_binary(N))/binary, "ns"/utf8>>;

        N@1 when N@1 < 1000000 ->
            <<(erlang:integer_to_binary(N@1 div 1000))/binary, "us"/utf8>>;

        N@2 when N@2 < 1000000000 ->
            <<(erlang:integer_to_binary(N@2 div 1000000))/binary, "ms"/utf8>>;

        N@3 ->
            <<(erlang:integer_to_binary(N@3 div 1000000000))/binary, "s"/utf8>>
    end.

-file("src/mochi/telemetry.gleam", 566).
?DOC(" Format metrics summary to string for logging\n").
-spec format_metrics_summary(metrics_summary()) -> binary().
format_metrics_summary(Summary) ->
    Lines = [<<"GraphQL Request Metrics:"/utf8>>,
        <<"  Total: "/utf8,
            (format_duration(erlang:element(2, Summary)))/binary>>,
        <<"  Parse: "/utf8,
            (format_duration(erlang:element(3, Summary)))/binary>>,
        <<"  Validate: "/utf8,
            (format_duration(erlang:element(4, Summary)))/binary>>,
        <<"  Fields: "/utf8,
            (erlang:integer_to_binary(erlang:element(5, Summary)))/binary>>,
        <<"  Errors: "/utf8,
            (erlang:integer_to_binary(erlang:element(6, Summary)))/binary>>],
    Slowest_lines = case erlang:element(7, Summary) of
        [] ->
            [];

        Fields ->
            [<<"  Slowest fields:"/utf8>> |
                gleam@list:map(
                    Fields,
                    fun(F) ->
                        <<<<<<"    "/utf8, (erlang:element(1, F))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (format_duration(erlang:element(2, F)))/binary>>
                    end
                )]
    end,
    gleam@string:join(lists:append(Lines, Slowest_lines), <<"\n"/utf8>>).

-file("src/mochi/telemetry.gleam", 601).
?DOC(
    " Convert a TelemetryConfig into a schema.TelemetryFn callback.\n"
    "\n"
    " This bridges the full telemetry system into the executor's event callback.\n"
    " Use this with `schema.with_telemetry_fn/2` to enable instrumentation:\n"
    "\n"
    " ```gleam\n"
    " let config = telemetry.with_handler(fn(event) { echo event })\n"
    " let ctx = schema.execution_context(user_data)\n"
    "   |> schema.with_telemetry_fn(telemetry.to_schema_fn(config))\n"
    " ```\n"
).
-spec to_schema_fn(telemetry_config()) -> fun((mochi@schema:schema_event()) -> nil).
to_schema_fn(Config) ->
    fun(Event) -> case erlang:element(2, Config) of
            false ->
                nil;

            true ->
                case Event of
                    schema_parse_start ->
                        (erlang:element(5, Config))(
                            {parse_start, mochi_time_ffi:monotonic_time_ns()}
                        );

                    {schema_parse_end, Success, _} ->
                        (erlang:element(5, Config))(
                            {parse_end,
                                mochi_time_ffi:monotonic_time_ns(),
                                Success}
                        );

                    schema_validation_start ->
                        (erlang:element(5, Config))(
                            {validation_start,
                                mochi_time_ffi:monotonic_time_ns()}
                        );

                    {schema_validation_end, Success@1, Error_count, _} ->
                        (erlang:element(5, Config))(
                            {validation_end,
                                mochi_time_ffi:monotonic_time_ns(),
                                Success@1,
                                Error_count}
                        );

                    {schema_field_start, Field_name, Parent_type, Path} ->
                        case erlang:element(3, Config) of
                            false ->
                                nil;

                            true ->
                                (erlang:element(5, Config))(
                                    {field_resolve_start,
                                        mochi_time_ffi:monotonic_time_ns(),
                                        Field_name,
                                        Parent_type,
                                        Path}
                                )
                        end;

                    {schema_field_end,
                        Field_name@1,
                        Parent_type@1,
                        Path@1,
                        Success@2,
                        Duration_ns} ->
                        case erlang:element(3, Config) of
                            false ->
                                nil;

                            true ->
                                (erlang:element(5, Config))(
                                    {field_resolve_end,
                                        mochi_time_ffi:monotonic_time_ns(),
                                        Field_name@1,
                                        Parent_type@1,
                                        Path@1,
                                        Success@2,
                                        Duration_ns}
                                )
                        end;

                    {schema_operation_start, Operation_name} ->
                        (erlang:element(5, Config))(
                            {operation_start,
                                mochi_time_ffi:monotonic_time_ns(),
                                Operation_name,
                                'query'}
                        );

                    {schema_operation_end,
                        Operation_name@1,
                        Success@3,
                        Error_count@1} ->
                        (erlang:element(5, Config))(
                            {operation_end,
                                mochi_time_ffi:monotonic_time_ns(),
                                Operation_name@1,
                                Success@3,
                                Error_count@1}
                        );

                    {schema_data_loader_batch,
                        Loader_name,
                        Batch_size,
                        Duration_ns@1} ->
                        case erlang:element(4, Config) of
                            false ->
                                nil;

                            true ->
                                (erlang:element(5, Config))(
                                    {data_loader_batch,
                                        mochi_time_ffi:monotonic_time_ns(),
                                        Loader_name,
                                        Batch_size,
                                        Duration_ns@1}
                                )
                        end
                end
        end end.
