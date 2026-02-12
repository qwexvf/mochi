-module(mochi_wisp@supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/supervisor.gleam").
-export([start_cache_manager/0, start_metrics_collector/0, init_app/0, get_cache_stats/1, get_metrics/1, record_request/3, clear_cache/1, warm_cache/2]).
-export_type([cache_message/0, cache_stats/0, cache_state/0, metrics_message/0, metrics/0, metrics_state/0, app_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type cache_message() :: {get_stats,
        gleam@erlang@process:subject(cache_stats())} |
    clear_cache |
    {warm_cache, list(binary())} |
    {get_size, gleam@erlang@process:subject(integer())}.

-type cache_stats() :: {cache_stats, integer(), integer(), integer(), float()}.

-type cache_state() :: {cache_state, boolean()}.

-type metrics_message() :: {record_request, integer(), boolean()} |
    {get_metrics, gleam@erlang@process:subject(metrics())} |
    reset_metrics.

-type metrics() :: {metrics,
        integer(),
        integer(),
        integer(),
        integer(),
        float(),
        integer(),
        integer()}.

-type metrics_state() :: {metrics_state,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type app_state() :: {app_state,
        gleam@erlang@process:subject(cache_message()),
        gleam@erlang@process:subject(metrics_message())}.

-file("src/mochi_wisp/supervisor.gleam", 51).
-spec handle_cache_message(cache_state(), cache_message()) -> gleam@otp@actor:next(cache_state(), cache_message()).
handle_cache_message(State, Message) ->
    case Message of
        {get_stats, Reply_to} ->
            Stats = mochi_query_cache_ffi:stats(),
            Total = erlang:element(2, Stats) + erlang:element(3, Stats),
            Hit_rate = case Total of
                0 ->
                    +0.0;

                _ ->
                    (case erlang:float(Total) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> erlang:float(
                            erlang:element(2, Stats)
                        )
                        / Gleam@denominator
                    end) * 100.0
            end,
            gleam@otp@actor:send(
                Reply_to,
                {cache_stats,
                    erlang:element(2, Stats),
                    erlang:element(3, Stats),
                    erlang:element(4, Stats),
                    Hit_rate}
            ),
            gleam@otp@actor:continue(State);

        clear_cache ->
            mochi_query_cache_ffi:clear(),
            logging:log(info, <<"Query cache cleared"/utf8>>),
            gleam@otp@actor:continue(State);

        {warm_cache, Queries} ->
            gleam@list:each(
                Queries,
                fun(Q) ->
                    _ = mochi_wisp@query_cache:get_or_parse(Q),
                    nil
                end
            ),
            logging:log(
                info,
                <<<<"Cache warmed with "/utf8,
                        (erlang:integer_to_binary(erlang:length(Queries)))/binary>>/binary,
                    " queries"/utf8>>
            ),
            gleam@otp@actor:continue(State);

        {get_size, Reply_to@1} ->
            gleam@otp@actor:send(Reply_to@1, mochi_query_cache_ffi:size()),
            gleam@otp@actor:continue(State)
    end.

-file("src/mochi_wisp/supervisor.gleam", 44).
?DOC(" Start the cache manager actor\n").
-spec start_cache_manager() -> {ok,
        gleam@erlang@process:subject(cache_message())} |
    {error, gleam@otp@actor:start_error()}.
start_cache_manager() ->
    _pipe = gleam@otp@actor:new({cache_state, true}),
    _pipe@1 = gleam@otp@actor:on_message(_pipe, fun handle_cache_message/2),
    _pipe@2 = gleam@otp@actor:start(_pipe@1),
    gleam@result:map(_pipe@2, fun(Started) -> erlang:element(3, Started) end).

-file("src/mochi_wisp/supervisor.gleam", 154).
-spec handle_metrics_message(metrics_state(), metrics_message()) -> gleam@otp@actor:next(metrics_state(), metrics_message()).
handle_metrics_message(State, Message) ->
    case Message of
        {record_request, Duration_us, Success} ->
            New_state = {metrics_state,
                erlang:element(2, State) + 1,
                case Success of
                    true ->
                        erlang:element(3, State) + 1;

                    false ->
                        erlang:element(3, State)
                end,
                case Success of
                    true ->
                        erlang:element(4, State);

                    false ->
                        erlang:element(4, State) + 1
                end,
                erlang:element(5, State) + Duration_us,
                gleam@int:max(erlang:element(6, State), Duration_us),
                gleam@int:min(erlang:element(7, State), Duration_us)},
            gleam@otp@actor:continue(New_state);

        {get_metrics, Reply_to} ->
            Avg = case erlang:element(2, State) of
                0 ->
                    +0.0;

                N ->
                    case erlang:float(N) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> erlang:float(
                            erlang:element(5, State)
                        )
                        / Gleam@denominator
                    end
            end,
            gleam@otp@actor:send(
                Reply_to,
                {metrics,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    Avg,
                    erlang:element(6, State),
                    case erlang:element(2, State) of
                        0 ->
                            0;

                        _ ->
                            erlang:element(7, State)
                    end}
            ),
            gleam@otp@actor:continue(State);

        reset_metrics ->
            logging:log(info, <<"Metrics reset"/utf8>>),
            gleam@otp@actor:continue({metrics_state, 0, 0, 0, 0, 0, 999999999})
    end.

-file("src/mochi_wisp/supervisor.gleam", 140).
?DOC(" Start the metrics collector actor\n").
-spec start_metrics_collector() -> {ok,
        gleam@erlang@process:subject(metrics_message())} |
    {error, gleam@otp@actor:start_error()}.
start_metrics_collector() ->
    _pipe = gleam@otp@actor:new({metrics_state, 0, 0, 0, 0, 0, 999999999}),
    _pipe@1 = gleam@otp@actor:on_message(_pipe, fun handle_metrics_message/2),
    _pipe@2 = gleam@otp@actor:start(_pipe@1),
    gleam@result:map(_pipe@2, fun(Started) -> erlang:element(3, Started) end).

-file("src/mochi_wisp/supervisor.gleam", 231).
?DOC(" Initialize the application with all supervised actors\n").
-spec init_app() -> {ok, app_state()} | {error, binary()}.
init_app() ->
    logging:log(info, <<"Initializing OTP application..."/utf8>>),
    mochi_query_cache_ffi:init(),
    logging:log(info, <<"Query cache initialized"/utf8>>),
    Cache_result = start_cache_manager(),
    gleam@result:'try'(
        begin
            _pipe = Cache_result,
            gleam@result:map_error(
                _pipe,
                fun(_) -> <<"Failed to start cache manager"/utf8>> end
            )
        end,
        fun(Cache_manager) ->
            logging:log(info, <<"Cache manager actor started"/utf8>>),
            Metrics_result = start_metrics_collector(),
            gleam@result:'try'(
                begin
                    _pipe@1 = Metrics_result,
                    gleam@result:map_error(
                        _pipe@1,
                        fun(_) ->
                            <<"Failed to start metrics collector"/utf8>>
                        end
                    )
                end,
                fun(Metrics_collector) ->
                    logging:log(
                        info,
                        <<"Metrics collector actor started"/utf8>>
                    ),
                    logging:log(
                        info,
                        <<"OTP application initialized successfully"/utf8>>
                    ),
                    {ok, {app_state, Cache_manager, Metrics_collector}}
                end
            )
        end
    ).

-file("src/mochi_wisp/supervisor.gleam", 264).
?DOC(" Get cache stats (blocking call to actor)\n").
-spec get_cache_stats(app_state()) -> cache_stats().
get_cache_stats(App) ->
    gleam@otp@actor:call(
        erlang:element(2, App),
        5000,
        fun(Field@0) -> {get_stats, Field@0} end
    ).

-file("src/mochi_wisp/supervisor.gleam", 269).
?DOC(" Get metrics (blocking call to actor)\n").
-spec get_metrics(app_state()) -> metrics().
get_metrics(App) ->
    gleam@otp@actor:call(
        erlang:element(3, App),
        5000,
        fun(Field@0) -> {get_metrics, Field@0} end
    ).

-file("src/mochi_wisp/supervisor.gleam", 274).
?DOC(" Record a request completion\n").
-spec record_request(app_state(), integer(), boolean()) -> nil.
record_request(App, Duration_us, Success) ->
    gleam@otp@actor:send(
        erlang:element(3, App),
        {record_request, Duration_us, Success}
    ).

-file("src/mochi_wisp/supervisor.gleam", 279).
?DOC(" Clear the query cache\n").
-spec clear_cache(app_state()) -> nil.
clear_cache(App) ->
    gleam@otp@actor:send(erlang:element(2, App), clear_cache).

-file("src/mochi_wisp/supervisor.gleam", 284).
?DOC(" Warm up the cache with common queries\n").
-spec warm_cache(app_state(), list(binary())) -> nil.
warm_cache(App, Queries) ->
    gleam@otp@actor:send(erlang:element(2, App), {warm_cache, Queries}).
