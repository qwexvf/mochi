-module(mochi_wisp@benchmark).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/benchmark.gleam").
-export([measure_time/1, monotonic_time_us/0, run_all/0]).
-export_type([benchmark_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type benchmark_result() :: {benchmark_result,
        binary(),
        integer(),
        integer(),
        float(),
        integer(),
        integer(),
        float()}.

-file("src/mochi_wisp/benchmark.gleam", 25).
?DOC(" Measure execution time in microseconds\n").
-spec measure_time(fun(() -> BPL)) -> {integer(), BPL}.
measure_time(F) ->
    mochi_wisp_benchmark_ffi:measure_time(F).

-file("src/mochi_wisp/benchmark.gleam", 29).
?DOC(" Get current monotonic time in microseconds\n").
-spec monotonic_time_us() -> integer().
monotonic_time_us() ->
    mochi_wisp_benchmark_ffi:monotonic_time_us().

-file("src/mochi_wisp/benchmark.gleam", 118).
-spec run_for_duration(
    mochi@schema:schema(),
    binary(),
    integer(),
    integer(),
    integer()
) -> integer().
run_for_duration(Gql_schema, Body, Start, Duration_us, Count) ->
    Now = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    case (Now - Start) >= Duration_us of
        true ->
            Count;

        false ->
            case mochi_wisp_ffi:parse_graphql_request_full(Body) of
                {ok, Req} ->
                    Vars = gleam@option:unwrap(
                        erlang:element(3, Req),
                        maps:new()
                    ),
                    Result = mochi@executor:execute_query_with_variables(
                        Gql_schema,
                        erlang:element(2, Req),
                        Vars
                    ),
                    _ = mochi_wisp@graphql_handler:execution_result_to_json(
                        Result
                    ),
                    run_for_duration(
                        Gql_schema,
                        Body,
                        Start,
                        Duration_us,
                        Count + 1
                    );

                {error, _} ->
                    run_for_duration(
                        Gql_schema,
                        Body,
                        Start,
                        Duration_us,
                        Count
                    )
            end
    end.

-file("src/mochi_wisp/benchmark.gleam", 192).
-spec run_cached_for_duration(
    mochi@schema:schema(),
    binary(),
    integer(),
    integer(),
    integer()
) -> integer().
run_cached_for_duration(Gql_schema, Query, Start, Duration_us, Count) ->
    Now = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    case (Now - Start) >= Duration_us of
        true ->
            Count;

        false ->
            case mochi_wisp@query_cache:get_or_parse(Query) of
                {ok, Document} ->
                    Ctx = mochi@schema:execution_context(
                        gleam_stdlib:identity(maps:new())
                    ),
                    Result = mochi@executor:execute(
                        Gql_schema,
                        Document,
                        none,
                        Ctx,
                        maps:new()
                    ),
                    _ = mochi_wisp@graphql_handler:execution_result_to_json(
                        Result
                    ),
                    run_cached_for_duration(
                        Gql_schema,
                        Query,
                        Start,
                        Duration_us,
                        Count + 1
                    );

                {error, _} ->
                    run_cached_for_duration(
                        Gql_schema,
                        Query,
                        Start,
                        Duration_us,
                        Count
                    )
            end
    end.

-file("src/mochi_wisp/benchmark.gleam", 221).
-spec print_summary() -> nil.
print_summary() ->
    gleam_stdlib:println(<<"--- Summary ---"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Performance characteristics:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Query parsing: ~4-120µs depending on complexity"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Query cache HIT: ~0.02-0.14µs (7-50M ops/sec)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Schema is pre-built (cached), very fast to access"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Query execution: ~25-60µs for typical queries"/utf8>>
    ),
    gleam_stdlib:println(<<"  - JSON serialization: ~1-2µs for results"/utf8>>),
    gleam_stdlib:println(<<"  - End-to-end: ~18-80µs per request"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Throughput:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Without cache: ~27K ops/sec (single core)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - With cache:    ~100K+ ops/sec (single core) - 3.9x faster!"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Estimated HTTP throughput:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Simple queries:  ~100K+ req/sec (single core, with cache)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Complex queries: ~30K+ req/sec (single core, with cache)"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"For HTTP load testing, use:"/utf8>>),
    gleam_stdlib:println(
        <<"  wrk -t4 -c100 -d30s -s bench.lua http://localhost:8000/graphql"/utf8>>
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 470).
-spec run_iterations(integer(), fun(() -> any()), list(integer())) -> list(integer()).
run_iterations(N, F, Acc) ->
    case N =< 0 of
        true ->
            Acc;

        false ->
            {Time_us, _} = mochi_wisp_benchmark_ffi:measure_time(F),
            run_iterations(N - 1, F, [Time_us | Acc])
    end.

-file("src/mochi_wisp/benchmark.gleam", 521).
-spec float_to_string_2dp(float()) -> binary().
float_to_string_2dp(F) ->
    Rounded = erlang:round(F * 100.0),
    Int_part = Rounded div 100,
    Dec_part = gleam@int:absolute_value(Rounded rem 100),
    Dec_str = case Dec_part < 10 of
        true ->
            <<"0"/utf8, (erlang:integer_to_binary(Dec_part))/binary>>;

        false ->
            erlang:integer_to_binary(Dec_part)
    end,
    <<<<(erlang:integer_to_binary(Int_part))/binary, "."/utf8>>/binary,
        Dec_str/binary>>.

-file("src/mochi_wisp/benchmark.gleam", 499).
-spec format_time(float()) -> binary().
format_time(Us) ->
    case Us < 1000.0 of
        true ->
            <<(float_to_string_2dp(Us))/binary, " µs"/utf8>>;

        false ->
            case Us < 1000000.0 of
                true ->
                    <<(float_to_string_2dp(Us / 1000.0))/binary, " ms"/utf8>>;

                false ->
                    <<(float_to_string_2dp(Us / 1000000.0))/binary, " s"/utf8>>
            end
    end.

-file("src/mochi_wisp/benchmark.gleam", 510).
-spec format_ops(float()) -> binary().
format_ops(Ops) ->
    case Ops >= 1000000.0 of
        true ->
            <<(float_to_string_2dp(Ops / 1000000.0))/binary, "M"/utf8>>;

        false ->
            case Ops >= 1000.0 of
                true ->
                    <<(float_to_string_2dp(Ops / 1000.0))/binary, "K"/utf8>>;

                false ->
                    float_to_string_2dp(Ops)
            end
    end.

-file("src/mochi_wisp/benchmark.gleam", 92).
-spec run_throughput_test() -> nil.
run_throughput_test() ->
    gleam_stdlib:println(<<"--- Throughput Test (sustained load) ---"/utf8>>),
    Schema = mochi_wisp@schema:build_schema(),
    Body = <<"{\"query\": \"{ users { id name email role } }\"}"/utf8>>,
    Duration_ms = 1000,
    Start = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    Ops = run_for_duration(Schema, Body, Start, Duration_ms * 1000, 0),
    Elapsed_us = mochi_wisp_benchmark_ffi:monotonic_time_us() - Start,
    Ops_per_sec = case erlang:float(Elapsed_us) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Ops) * 1000000.0 / Gleam@denominator
    end,
    Latency_us = case erlang:float(Ops) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> erlang:float(Elapsed_us) / Gleam@denominator@1
    end,
    gleam_stdlib:println(
        <<<<<<<<<<"Sustained throughput (1s):     "/utf8,
                            (format_ops(Ops_per_sec))/binary>>/binary,
                        " ops/sec"/utf8>>/binary,
                    "  (avg latency: "/utf8>>/binary,
                (format_time(Latency_us))/binary>>/binary,
            ")"/utf8>>
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 147).
-spec run_cached_throughput_test() -> nil.
run_cached_throughput_test() ->
    gleam_stdlib:println(
        <<"--- Cached Throughput Test (using query cache) ---"/utf8>>
    ),
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Schema = mochi_wisp@schema:build_schema(),
    Query = <<"{ users { id name email role } }"/utf8>>,
    _ = mochi_wisp@query_cache:get_or_parse(Query),
    Duration_ms = 1000,
    Start = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    Ops = run_cached_for_duration(Schema, Query, Start, Duration_ms * 1000, 0),
    Elapsed_us = mochi_wisp_benchmark_ffi:monotonic_time_us() - Start,
    Ops_per_sec = case erlang:float(Elapsed_us) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Ops) * 1000000.0 / Gleam@denominator
    end,
    Latency_us = case erlang:float(Ops) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> erlang:float(Elapsed_us) / Gleam@denominator@1
    end,
    gleam_stdlib:println(
        <<<<<<<<<<"Cached throughput (1s):        "/utf8,
                            (format_ops(Ops_per_sec))/binary>>/binary,
                        " ops/sec"/utf8>>/binary,
                    "  (avg latency: "/utf8>>/binary,
                (format_time(Latency_us))/binary>>/binary,
            ")"/utf8>>
    ),
    Stats = mochi_query_cache_ffi:stats(),
    gleam_stdlib:println(
        <<<<<<<<<<<<"Cache stats: "/utf8,
                                (erlang:integer_to_binary(
                                    erlang:element(2, Stats)
                                ))/binary>>/binary,
                            " hits, "/utf8>>/binary,
                        (erlang:integer_to_binary(erlang:element(3, Stats)))/binary>>/binary,
                    " misses, "/utf8>>/binary,
                (erlang:integer_to_binary(erlang:element(4, Stats)))/binary>>/binary,
            " entries"/utf8>>
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 532).
-spec pad_right(binary(), integer()) -> binary().
pad_right(S, Len) ->
    Current = string:length(S),
    case Current >= Len of
        true ->
            S;

        false ->
            <<S/binary,
                (gleam@string:repeat(<<" "/utf8>>, Len - Current))/binary>>
    end.

-file("src/mochi_wisp/benchmark.gleam", 540).
-spec pad_left(binary(), integer()) -> binary().
pad_left(S, Len) ->
    Current = string:length(S),
    case Current >= Len of
        true ->
            S;

        false ->
            <<(gleam@string:repeat(<<" "/utf8>>, Len - Current))/binary,
                S/binary>>
    end.

-file("src/mochi_wisp/benchmark.gleam", 480).
-spec print_result(benchmark_result()) -> nil.
print_result(Result) ->
    Avg_str = format_time(erlang:element(5, Result)),
    Ops_str = format_ops(erlang:element(8, Result)),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<<<<<(pad_right(erlang:element(2, Result), 30))/binary,
                                                " "/utf8>>/binary,
                                            (pad_left(Avg_str, 12))/binary>>/binary,
                                        "  "/utf8>>/binary,
                                    (pad_left(Ops_str, 14))/binary>>/binary,
                                " ops/sec"/utf8>>/binary,
                            "  (min: "/utf8>>/binary,
                        (format_time(erlang:float(erlang:element(6, Result))))/binary>>/binary,
                    ", max: "/utf8>>/binary,
                (format_time(erlang:float(erlang:element(7, Result))))/binary>>/binary,
            ")"/utf8>>
    ).

-file("src/mochi_wisp/benchmark.gleam", 446).
-spec benchmark(binary(), integer(), fun(() -> any())) -> benchmark_result().
benchmark(Name, Iterations, F) ->
    Times = run_iterations(Iterations, F, []),
    Total_us = gleam@list:fold(Times, 0, fun(Acc, T) -> Acc + T end),
    Avg_us = case erlang:float(Iterations) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Total_us) / Gleam@denominator
    end,
    Min_us = gleam@list:fold(
        Times,
        999999999,
        fun(Acc@1, T@1) -> gleam@int:min(Acc@1, T@1) end
    ),
    Max_us = gleam@list:fold(
        Times,
        0,
        fun(Acc@2, T@2) -> gleam@int:max(Acc@2, T@2) end
    ),
    Ops_per_sec = case Avg_us of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> 1000000.0 / Gleam@denominator@1
    end,
    Result = {benchmark_result,
        Name,
        Iterations,
        Total_us,
        Avg_us,
        Min_us,
        Max_us,
        Ops_per_sec},
    print_result(Result),
    Result.

-file("src/mochi_wisp/benchmark.gleam", 249).
-spec run_parsing_benchmarks() -> nil.
run_parsing_benchmarks() ->
    gleam_stdlib:println(<<"--- Query Parsing ---"/utf8>>),
    Simple_query = <<"{ hello }"/utf8>>,
    Medium_query = <<"{ users { id name email } }"/utf8>>,
    Complex_query = <<"query GetUser($id: ID!) { user(id: $id) { id name email role } users { id name } }"/utf8>>,
    _ = benchmark(
        <<"Parse simple query"/utf8>>,
        10000,
        fun() -> mochi@parser:parse(Simple_query) end
    ),
    _ = benchmark(
        <<"Parse medium query"/utf8>>,
        10000,
        fun() -> mochi@parser:parse(Medium_query) end
    ),
    _ = benchmark(
        <<"Parse complex query"/utf8>>,
        10000,
        fun() -> mochi@parser:parse(Complex_query) end
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 267).
-spec run_cache_benchmarks() -> nil.
run_cache_benchmarks() ->
    gleam_stdlib:println(<<"--- Query Cache Performance ---"/utf8>>),
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Simple_query = <<"{ hello }"/utf8>>,
    Medium_query = <<"{ users { id name email } }"/utf8>>,
    Complex_query = <<"query GetUser($id: ID!) { user(id: $id) { id name email role } users { id name } }"/utf8>>,
    _ = benchmark(
        <<"Cache MISS (simple)"/utf8>>,
        10000,
        fun() ->
            mochi_query_cache_ffi:clear(),
            mochi_wisp@query_cache:get_or_parse(Simple_query)
        end
    ),
    mochi_query_cache_ffi:clear(),
    _ = mochi_wisp@query_cache:get_or_parse(Simple_query),
    _ = benchmark(
        <<"Cache HIT (simple)"/utf8>>,
        10000,
        fun() -> mochi_wisp@query_cache:get_or_parse(Simple_query) end
    ),
    _ = benchmark(
        <<"Cache MISS (medium)"/utf8>>,
        10000,
        fun() ->
            mochi_query_cache_ffi:clear(),
            mochi_wisp@query_cache:get_or_parse(Medium_query)
        end
    ),
    mochi_query_cache_ffi:clear(),
    _ = mochi_wisp@query_cache:get_or_parse(Medium_query),
    _ = benchmark(
        <<"Cache HIT (medium)"/utf8>>,
        10000,
        fun() -> mochi_wisp@query_cache:get_or_parse(Medium_query) end
    ),
    _ = benchmark(
        <<"Cache MISS (complex)"/utf8>>,
        10000,
        fun() ->
            mochi_query_cache_ffi:clear(),
            mochi_wisp@query_cache:get_or_parse(Complex_query)
        end
    ),
    mochi_query_cache_ffi:clear(),
    _ = mochi_wisp@query_cache:get_or_parse(Complex_query),
    _ = benchmark(
        <<"Cache HIT (complex)"/utf8>>,
        10000,
        fun() -> mochi_wisp@query_cache:get_or_parse(Complex_query) end
    ),
    mochi_query_cache_ffi:clear(),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 325).
-spec run_schema_benchmarks() -> nil.
run_schema_benchmarks() ->
    gleam_stdlib:println(<<"--- Schema Building ---"/utf8>>),
    _ = benchmark(
        <<"Build schema"/utf8>>,
        1000,
        fun() -> mochi_wisp@schema:build_schema() end
    ),
    _ = benchmark(
        <<"Build user type"/utf8>>,
        10000,
        fun() -> mochi_wisp@schema:user_type() end
    ),
    _ = benchmark(
        <<"Build role enum"/utf8>>,
        10000,
        fun() -> mochi_wisp@schema:role_enum() end
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 338).
-spec run_execution_benchmarks() -> nil.
run_execution_benchmarks() ->
    gleam_stdlib:println(<<"--- Query Execution ---"/utf8>>),
    Schema = mochi_wisp@schema:build_schema(),
    _ = benchmark(
        <<"Execute users query"/utf8>>,
        1000,
        fun() ->
            mochi@executor:execute_query(
                Schema,
                <<"{ users { id name email } }"/utf8>>
            )
        end
    ),
    _ = benchmark(
        <<"Execute users + role"/utf8>>,
        1000,
        fun() ->
            mochi@executor:execute_query(
                Schema,
                <<"{ users { id name email role } }"/utf8>>
            )
        end
    ),
    _ = benchmark(
        <<"Execute user by id"/utf8>>,
        1000,
        fun() ->
            mochi@executor:execute_query(
                Schema,
                <<"{ user(id: \"1\") { id name } }"/utf8>>
            )
        end
    ),
    _ = benchmark(
        <<"Execute with variables"/utf8>>,
        1000,
        fun() ->
            Vars = maps:from_list(
                [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)}]
            ),
            mochi@executor:execute_query_with_variables(
                Schema,
                <<"query GetUser($id: ID!) { user(id: $id) { id name } }"/utf8>>,
                Vars
            )
        end
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 370).
-spec run_json_benchmarks() -> nil.
run_json_benchmarks() ->
    gleam_stdlib:println(<<"--- JSON Serialization ---"/utf8>>),
    Schema = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ users { id name email role } }"/utf8>>
    ),
    _ = benchmark(
        <<"Serialize result to JSON"/utf8>>,
        10000,
        fun() -> mochi_wisp@graphql_handler:execution_result_to_json(Result) end
    ),
    Error_result = {execution_result,
        none,
        [{validation_error,
                <<"Test error"/utf8>>,
                [<<"field"/utf8>>, <<"subfield"/utf8>>]},
            {resolver_error, <<"Another error"/utf8>>, [<<"other"/utf8>>]}]},
    _ = benchmark(
        <<"Serialize error result"/utf8>>,
        10000,
        fun() ->
            mochi_wisp@graphql_handler:execution_result_to_json(Error_result)
        end
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 397).
-spec run_e2e_benchmarks() -> nil.
run_e2e_benchmarks() ->
    gleam_stdlib:println(
        <<"--- End-to-End (Parse + Execute + Serialize) ---"/utf8>>
    ),
    Schema = mochi_wisp@schema:build_schema(),
    _ = benchmark(
        <<"E2E simple query"/utf8>>,
        1000,
        fun() ->
            Query = <<"{ users { id name } }"/utf8>>,
            case mochi_wisp_ffi:parse_graphql_request_full(
                <<<<"{\"query\": \""/utf8, Query/binary>>/binary, "\"}"/utf8>>
            ) of
                {ok, Req} ->
                    Vars = gleam@option:unwrap(
                        erlang:element(3, Req),
                        maps:new()
                    ),
                    Result = mochi@executor:execute_query_with_variables(
                        Schema,
                        erlang:element(2, Req),
                        Vars
                    ),
                    mochi_wisp@graphql_handler:execution_result_to_json(Result);

                {error, _} ->
                    <<""/utf8>>
            end
        end
    ),
    _ = benchmark(
        <<"E2E complex query"/utf8>>,
        1000,
        fun() ->
            Body = <<"{\"query\": \"{ users { id name email role } user(id: \\\"1\\\") { id name } }\"}"/utf8>>,
            case mochi_wisp_ffi:parse_graphql_request_full(Body) of
                {ok, Req@1} ->
                    Vars@1 = gleam@option:unwrap(
                        erlang:element(3, Req@1),
                        maps:new()
                    ),
                    Result@1 = mochi@executor:execute_query_with_variables(
                        Schema,
                        erlang:element(2, Req@1),
                        Vars@1
                    ),
                    mochi_wisp@graphql_handler:execution_result_to_json(
                        Result@1
                    );

                {error, _} ->
                    <<""/utf8>>
            end
        end
    ),
    _ = benchmark(
        <<"E2E with variables"/utf8>>,
        1000,
        fun() ->
            Body@1 = <<"{\"query\": \"query GetUser($id: ID!) { user(id: $id) { id name email } }\", \"variables\": {\"id\": \"2\"}}"/utf8>>,
            case mochi_wisp_ffi:parse_graphql_request_full(Body@1) of
                {ok, Req@2} ->
                    Vars@2 = gleam@option:unwrap(
                        erlang:element(3, Req@2),
                        maps:new()
                    ),
                    Result@2 = mochi@executor:execute_query_with_variables(
                        Schema,
                        erlang:element(2, Req@2),
                        Vars@2
                    ),
                    mochi_wisp@graphql_handler:execution_result_to_json(
                        Result@2
                    );

                {error, _} ->
                    <<""/utf8>>
            end
        end
    ),
    nil.

-file("src/mochi_wisp/benchmark.gleam", 48).
?DOC(" Run all benchmarks\n").
-spec run_all() -> nil.
run_all() ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ),
    gleam_stdlib:println(<<"  Mochi GraphQL Benchmark Suite"/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Warming up..."/utf8>>),
    Schema = mochi_wisp@schema:build_schema(),
    _ = mochi@executor:execute_query(Schema, <<"{ users { id } }"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    run_parsing_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_cache_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_schema_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_execution_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_json_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_e2e_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_throughput_test(),
    gleam_stdlib:println(<<""/utf8>>),
    run_cached_throughput_test(),
    gleam_stdlib:println(<<""/utf8>>),
    print_summary(),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ),
    gleam_stdlib:println(<<"  Benchmark Complete"/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ).
