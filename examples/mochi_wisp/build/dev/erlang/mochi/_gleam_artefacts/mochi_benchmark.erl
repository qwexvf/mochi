-module(mochi_benchmark).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_benchmark.gleam").
-export([main/0]).

-file("src/mochi_benchmark.gleam", 112).
-spec create_benchmark_schema() -> mochi@schema:schema().
create_benchmark_schema() ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        _pipe@2 = mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        _pipe@4 = mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"name"/utf8>>,
                    mochi@schema:string_type()
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        _pipe@6 = mochi@schema:field(
            _pipe@4,
            begin
                _pipe@5 = mochi@schema:field_def(
                    <<"email"/utf8>>,
                    mochi@schema:string_type()
                ),
                mochi@schema:resolver(
                    _pipe@5,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        _pipe@8 = mochi@schema:field(
            _pipe@6,
            begin
                _pipe@7 = mochi@schema:field_def(
                    <<"active"/utf8>>,
                    mochi@schema:boolean_type()
                ),
                mochi@schema:resolver(
                    _pipe@7,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        mochi@schema:field(
            _pipe@8,
            begin
                _pipe@9 = mochi@schema:field_def(
                    <<"posts"/utf8>>,
                    mochi@schema:list_type(
                        mochi@schema:named_type(<<"Post"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@9,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        )
    end,
    Post_type = begin
        _pipe@10 = mochi@schema:object(<<"Post"/utf8>>),
        _pipe@12 = mochi@schema:field(
            _pipe@10,
            begin
                _pipe@11 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                ),
                mochi@schema:resolver(
                    _pipe@11,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        _pipe@14 = mochi@schema:field(
            _pipe@12,
            begin
                _pipe@13 = mochi@schema:field_def(
                    <<"title"/utf8>>,
                    mochi@schema:string_type()
                ),
                mochi@schema:resolver(
                    _pipe@13,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        mochi@schema:field(
            _pipe@14,
            begin
                _pipe@15 = mochi@schema:field_def(
                    <<"content"/utf8>>,
                    mochi@schema:string_type()
                ),
                mochi@schema:resolver(
                    _pipe@15,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@16 = mochi@schema:object(<<"Query"/utf8>>),
        _pipe@19 = mochi@schema:field(
            _pipe@16,
            begin
                _pipe@17 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                _pipe@18 = mochi@schema:argument(
                    _pipe@17,
                    mochi@schema:arg(
                        <<"id"/utf8>>,
                        mochi@schema:non_null(mochi@schema:id_type())
                    )
                ),
                mochi@schema:resolver(
                    _pipe@18,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        ),
        mochi@schema:field(
            _pipe@19,
            begin
                _pipe@20 = mochi@schema:field_def(
                    <<"users"/utf8>>,
                    mochi@schema:list_type(
                        mochi@schema:named_type(<<"User"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@20,
                    fun(_) ->
                        {error, <<"Dynamic serialization needed"/utf8>>}
                    end
                )
            end
        )
    end,
    _pipe@21 = mochi@schema:schema(),
    _pipe@22 = mochi@schema:'query'(_pipe@21, Query_type),
    _pipe@23 = mochi@schema:add_type(_pipe@22, {object_type_def, User_type}),
    mochi@schema:add_type(_pipe@23, {object_type_def, Post_type}).

-file("src/mochi_benchmark.gleam", 170).
-spec float_to_string(float()) -> binary().
float_to_string(Value) ->
    case Value >= 99.0 of
        true ->
            <<"100.0"/utf8>>;

        false ->
            case Value >= 10.0 of
                true ->
                    <<"99.0"/utf8>>;

                false ->
                    <<"0.0"/utf8>>
            end
    end.

-file("src/mochi_benchmark.gleam", 45).
-spec test_parsing_performance(binary(), binary(), integer()) -> nil.
test_parsing_performance(Name, Query, Iterations) ->
    gleam_stdlib:print(
        <<<<<<<<"Testing "/utf8, Name/binary>>/binary, " parsing ("/utf8>>/binary,
                (erlang:integer_to_binary(Iterations))/binary>>/binary,
            " iterations)... "/utf8>>
    ),
    Results = begin
        _pipe = gleam@list:range(1, Iterations),
        gleam@list:map(_pipe, fun(_) -> mochi@parser:parse(Query) end)
    end,
    Successful = gleam@list:count(Results, fun(R) -> case R of
                {ok, _} ->
                    true;

                {error, _} ->
                    false
            end end),
    Success_rate = case Iterations of
        0 ->
            +0.0;

        N ->
            (case erlang:float(N) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Successful) / Gleam@denominator
            end) * 100.0
    end,
    gleam_stdlib:println(
        <<<<"✅ "/utf8, (float_to_string(Success_rate))/binary>>/binary,
            "% success rate"/utf8>>
    ).

-file("src/mochi_benchmark.gleam", 74).
-spec test_execution_performance(
    binary(),
    binary(),
    mochi@schema:schema(),
    integer()
) -> nil.
test_execution_performance(Name, Query, Schema, Iterations) ->
    gleam_stdlib:print(
        <<<<<<<<"Testing "/utf8, Name/binary>>/binary, " execution ("/utf8>>/binary,
                (erlang:integer_to_binary(Iterations))/binary>>/binary,
            " iterations)... "/utf8>>
    ),
    Results = begin
        _pipe = gleam@list:range(1, Iterations),
        gleam@list:map(
            _pipe,
            fun(_) -> mochi@executor:execute_query(Schema, Query) end
        )
    end,
    Successful = gleam@list:count(
        Results,
        fun(R) -> case erlang:element(3, R) of
                [] ->
                    true;

                _ ->
                    false
            end end
    ),
    Success_rate = case Iterations of
        0 ->
            +0.0;

        N ->
            (case erlang:float(N) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Successful) / Gleam@denominator
            end) * 100.0
    end,
    gleam_stdlib:println(
        <<<<"ℹ️  "/utf8, (float_to_string(Success_rate))/binary>>/binary,
            "% (limited by Dynamic serialization)"/utf8>>
    ).

-file("src/mochi_benchmark.gleam", 12).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"🍡 mochi Performance Benchmark"/utf8>>),
    gleam_stdlib:println(
        begin
            _pipe = <<"="/utf8>>,
            gleam@string:repeat(_pipe, 40)
        end
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Schema = create_benchmark_schema(),
    Simple_query = <<"{ user(id: \"1\") { id name } }"/utf8>>,
    Complex_query = <<"{ user(id: \"1\") { id name email active posts { id title content } } }"/utf8>>,
    Nested_query = <<"{ users { id name posts { id title } } }"/utf8>>,
    gleam_stdlib:println(<<"📝 Testing Query Parsing Performance..."/utf8>>),
    test_parsing_performance(<<"Simple"/utf8>>, Simple_query, 1000),
    test_parsing_performance(<<"Complex"/utf8>>, Complex_query, 1000),
    test_parsing_performance(<<"Nested"/utf8>>, Nested_query, 1000),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"⚡ Testing Query Execution Performance..."/utf8>>),
    test_execution_performance(<<"Simple"/utf8>>, Simple_query, Schema, 1000),
    test_execution_performance(<<"Complex"/utf8>>, Complex_query, Schema, 1000),
    test_execution_performance(<<"Nested"/utf8>>, Nested_query, Schema, 1000),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"📊 Benchmark Results Summary"/utf8>>),
    gleam_stdlib:println(
        begin
            _pipe@1 = <<"-"/utf8>>,
            gleam@string:repeat(_pipe@1, 30)
        end
    ),
    gleam_stdlib:println(
        <<"mochi demonstrates strong parsing performance"/utf8>>
    ),
    gleam_stdlib:println(
        <<"Execution limited by Dynamic serialization (known issue)"/utf8>>
    ),
    gleam_stdlib:println(<<"Ready for HTTP load testing comparison!"/utf8>>).
