-module(mochi_benchmark).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_benchmark.gleam").
-export([main/0]).

-file("src/mochi_benchmark.gleam", 53).
-spec timed(binary(), fun(() -> nil)) -> nil.
timed(Label, F) ->
    T0 = mochi_time_ffi:monotonic_time_ns(),
    F(),
    Elapsed_us = (mochi_time_ffi:monotonic_time_ns() - T0) div 1000,
    gleam_stdlib:println(
        <<<<<<<<"  "/utf8, Label/binary>>/binary, "  "/utf8>>/binary,
                (erlang:integer_to_binary(Elapsed_us))/binary>>/binary,
            " µs total"/utf8>>
    ).

-file("src/mochi_benchmark.gleam", 60).
-spec bench_parse(binary(), integer()) -> nil.
bench_parse(Query, N) ->
    gleam@list:each(
        gleam@list:repeat(nil, N),
        fun(_) ->
            _ = mochi@parser:parse(Query),
            nil
        end
    ).

-file("src/mochi_benchmark.gleam", 67).
-spec bench_cache(binary(), integer()) -> nil.
bench_cache(Query, N) ->
    Cache = mochi@document_cache:new(),
    gleam@list:each(
        gleam@list:repeat(nil, N),
        fun(_) -> case mochi@document_cache:get(Cache, Query) of
                {ok, _} ->
                    nil;

                {error, _} ->
                    case mochi@parser:parse(Query) of
                        {ok, Doc} ->
                            mochi@document_cache:put(Cache, Query, Doc);

                        {error, _} ->
                            nil
                    end
            end end
    ).

-file("src/mochi_benchmark.gleam", 81).
-spec bench_execute(binary(), mochi@schema:schema(), integer()) -> nil.
bench_execute(Query, S, N) ->
    gleam@list:each(
        gleam@list:repeat(nil, N),
        fun(_) ->
            _ = mochi@executor:execute_query(S, Query),
            nil
        end
    ).

-file("src/mochi_benchmark.gleam", 88).
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"Alice"/utf8>>)} end
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
                        {ok,
                            gleam_stdlib:identity(<<"alice@example.com"/utf8>>)}
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
                    fun(_) -> {ok, gleam_stdlib:identity(true)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity([])} end
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"Hello"/utf8>>)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"World"/utf8>>)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity(<<"user"/utf8>>)} end
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
                    fun(_) -> {ok, gleam_stdlib:identity([])} end
                )
            end
        )
    end,
    _pipe@21 = mochi@schema:schema(),
    _pipe@22 = mochi@schema:'query'(_pipe@21, Query_type),
    _pipe@23 = mochi@schema:add_type(_pipe@22, {object_type_def, User_type}),
    mochi@schema:add_type(_pipe@23, {object_type_def, Post_type}).

-file("src/mochi_benchmark.gleam", 14).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"mochi Performance Benchmark"/utf8>>),
    gleam_stdlib:println(gleam@string:repeat(<<"="/utf8>>, 44)),
    gleam_stdlib:println(<<""/utf8>>),
    Schema_def = create_benchmark_schema(),
    Simple = <<"{ user(id: \"1\") { id name } }"/utf8>>,
    Complex = <<"{ user(id: \"1\") { id name email active posts { id title content } } }"/utf8>>,
    Nested = <<"{ users { id name posts { id title } } }"/utf8>>,
    N = 5000,
    gleam_stdlib:println(
        <<<<"Parse (no cache)                   "/utf8,
                (erlang:integer_to_binary(N))/binary>>/binary,
            " iters"/utf8>>
    ),
    gleam_stdlib:println(gleam@string:repeat(<<"-"/utf8>>, 44)),
    timed(<<"simple "/utf8>>, fun() -> bench_parse(Simple, N) end),
    timed(<<"complex"/utf8>>, fun() -> bench_parse(Complex, N) end),
    timed(<<"nested "/utf8>>, fun() -> bench_parse(Nested, N) end),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"Parse (document cache)             "/utf8,
                (erlang:integer_to_binary(N))/binary>>/binary,
            " iters"/utf8>>
    ),
    gleam_stdlib:println(gleam@string:repeat(<<"-"/utf8>>, 44)),
    timed(<<"simple "/utf8>>, fun() -> bench_cache(Simple, N) end),
    timed(<<"complex"/utf8>>, fun() -> bench_cache(Complex, N) end),
    timed(<<"nested "/utf8>>, fun() -> bench_cache(Nested, N) end),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"Execute (schema cache enabled)     "/utf8,
                (erlang:integer_to_binary(N))/binary>>/binary,
            " iters"/utf8>>
    ),
    gleam_stdlib:println(gleam@string:repeat(<<"-"/utf8>>, 44)),
    timed(<<"simple "/utf8>>, fun() -> bench_execute(Simple, Schema_def, N) end),
    timed(
        <<"complex"/utf8>>,
        fun() -> bench_execute(Complex, Schema_def, N) end
    ),
    timed(<<"nested "/utf8>>, fun() -> bench_execute(Nested, Schema_def, N) end).
