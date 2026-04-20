-module(batch_helpers_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/batch_helpers_test.gleam").
-export([add_types_test/0, add_enums_test/0, add_inputs_test/0, add_mutations_batch_test/0, add_scalars_test/0, add_interfaces_test/0, add_unions_test/0]).
-export_type([item/0]).

-type item() :: {item, binary(), binary()}.

-file("test/batch_helpers_test.gleam", 18).
-spec item_type(binary()) -> mochi@schema:object_type().
item_type(Name) ->
    _pipe = mochi@types:object(Name),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(I) -> erlang:element(2, I) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(I@1) -> erlang:element(3, I@1) end
    ),
    mochi@types:build(
        _pipe@2,
        fun(_) -> {ok, {item, <<"1"/utf8>>, <<"test"/utf8>>}} end
    ).

-file("test/batch_helpers_test.gleam", 29).
-spec add_types_test() -> nil.
add_types_test() ->
    Schema = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"item"/utf8>>,
                mochi@schema:named_type(<<"Foo"/utf8>>),
                fun(_) -> {ok, {item, <<"1"/utf8>>, <<"foo"/utf8>>}} end,
                fun(I) -> gleam_stdlib:identity(I) end
            )
        ),
        _pipe@2 = mochi@query:add_types(
            _pipe@1,
            [item_type(<<"Foo"/utf8>>), item_type(<<"Bar"/utf8>>)]
        ),
        mochi@query:build(_pipe@2)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ item { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_types should register types correctly"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_types_test"/utf8>>,
                    line => 46})
    end.

-file("test/batch_helpers_test.gleam", 54).
-spec add_enums_test() -> nil.
add_enums_test() ->
    E1 = begin
        _pipe = mochi@types:enum_type(<<"Color"/utf8>>),
        _pipe@1 = mochi@types:value(_pipe, <<"RED"/utf8>>),
        _pipe@2 = mochi@types:value(_pipe@1, <<"BLUE"/utf8>>),
        mochi@types:build_enum(_pipe@2)
    end,
    E2 = begin
        _pipe@3 = mochi@types:enum_type(<<"Size"/utf8>>),
        _pipe@4 = mochi@types:value(_pipe@3, <<"SMALL"/utf8>>),
        _pipe@5 = mochi@types:value(_pipe@4, <<"LARGE"/utf8>>),
        mochi@types:build_enum(_pipe@5)
    end,
    Schema = begin
        _pipe@6 = mochi@query:new(),
        _pipe@7 = mochi@query:add_query(
            _pipe@6,
            mochi@query:'query'(
                <<"ping"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"pong"/utf8>>} end,
                fun(S) -> gleam_stdlib:identity(S) end
            )
        ),
        _pipe@8 = mochi@query:add_enums(_pipe@7, [E1, E2]),
        mochi@query:build(_pipe@8)
    end,
    Result = mochi@executor:execute_query(Schema, <<"{ ping }"/utf8>>),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_enums should not break schema"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_enums_test"/utf8>>,
                    line => 80})
    end.

-file("test/batch_helpers_test.gleam", 88).
-spec add_inputs_test() -> nil.
add_inputs_test() ->
    Input1 = {input_object_type,
        <<"CreateInput"/utf8>>,
        none,
        maps:from_list(
            [{<<"name"/utf8>>,
                    {input_field_definition,
                        <<"name"/utf8>>,
                        none,
                        {non_null, {named, <<"String"/utf8>>}},
                        none}}]
        )},
    Input2 = {input_object_type,
        <<"UpdateInput"/utf8>>,
        none,
        maps:from_list(
            [{<<"id"/utf8>>,
                    {input_field_definition,
                        <<"id"/utf8>>,
                        none,
                        {non_null, {named, <<"ID"/utf8>>}},
                        none}}]
        )},
    Schema_def = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"ping"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"pong"/utf8>>} end,
                fun(S) -> gleam_stdlib:identity(S) end
            )
        ),
        _pipe@2 = mochi@query:add_inputs(_pipe@1, [Input1, Input2]),
        mochi@query:build(_pipe@2)
    end,
    Result = mochi@executor:execute_query(Schema_def, <<"{ ping }"/utf8>>),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_inputs should not break schema"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_inputs_test"/utf8>>,
                    line => 136})
    end.

-file("test/batch_helpers_test.gleam", 144).
-spec add_mutations_batch_test() -> nil.
add_mutations_batch_test() ->
    M1 = mochi@query:mutation(
        <<"doA"/utf8>>,
        [mochi@query:arg(
                <<"x"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:string_type(),
        fun(Args) -> mochi@query:get_string(Args, <<"x"/utf8>>) end,
        fun(X, _) -> {ok, X} end,
        fun(S) -> gleam_stdlib:identity(S) end
    ),
    M2 = mochi@query:mutation(
        <<"doB"/utf8>>,
        [mochi@query:arg(
                <<"y"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:string_type(),
        fun(Args@1) -> mochi@query:get_string(Args@1, <<"y"/utf8>>) end,
        fun(Y, _) -> {ok, Y} end,
        fun(S@1) -> gleam_stdlib:identity(S@1) end
    ),
    Schema_def = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"ping"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"pong"/utf8>>} end,
                fun(S@2) -> gleam_stdlib:identity(S@2) end
            )
        ),
        _pipe@2 = mochi@query:add_mutations(_pipe@1, [M1, M2]),
        mochi@query:build(_pipe@2)
    end,
    R1 = mochi@executor:execute_query(
        Schema_def,
        <<"mutation { doA(x: \"hello\") }"/utf8>>
    ),
    R2 = mochi@executor:execute_query(
        Schema_def,
        <<"mutation { doB(y: \"world\") }"/utf8>>
    ),
    case {erlang:element(3, R1), erlang:element(3, R2)} of
        {[], []} ->
            nil;

        {_, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Both batch-added mutations should work"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_mutations_batch_test"/utf8>>,
                    line => 180})
    end.

-file("test/batch_helpers_test.gleam", 188).
-spec add_scalars_test() -> nil.
add_scalars_test() ->
    S1 = {scalar_type,
        <<"DateTime"/utf8>>,
        {some, <<"ISO-8601 date-time"/utf8>>},
        fun(V) -> {ok, V} end,
        fun(V@1) -> {ok, V@1} end,
        fun(V@2) -> {ok, V@2} end},
    S2 = {scalar_type,
        <<"JSON"/utf8>>,
        {some, <<"Arbitrary JSON"/utf8>>},
        fun(V@3) -> {ok, V@3} end,
        fun(V@4) -> {ok, V@4} end,
        fun(V@5) -> {ok, V@5} end},
    Schema_def = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"now"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"2024-01-01"/utf8>>} end,
                fun(S) -> gleam_stdlib:identity(S) end
            )
        ),
        _pipe@2 = mochi@query:add_scalars(_pipe@1, [S1, S2]),
        mochi@query:build(_pipe@2)
    end,
    Result = mochi@executor:execute_query(Schema_def, <<"{ now }"/utf8>>),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_scalars should not break schema"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_scalars_test"/utf8>>,
                    line => 223})
    end.

-file("test/batch_helpers_test.gleam", 231).
-spec add_interfaces_test() -> nil.
add_interfaces_test() ->
    Iface = {interface_type, <<"Node"/utf8>>, none, maps:new(), none},
    Schema_def = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"ping"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"pong"/utf8>>} end,
                fun(S) -> gleam_stdlib:identity(S) end
            )
        ),
        _pipe@2 = mochi@query:add_interfaces(_pipe@1, [Iface]),
        mochi@query:build(_pipe@2)
    end,
    Result = mochi@executor:execute_query(Schema_def, <<"{ ping }"/utf8>>),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_interfaces should not break schema"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_interfaces_test"/utf8>>,
                    line => 253})
    end.

-file("test/batch_helpers_test.gleam", 261).
-spec add_unions_test() -> nil.
add_unions_test() ->
    Union = {union_type, <<"SearchResult"/utf8>>, none, [], none},
    Schema_def = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"ping"/utf8>>,
                mochi@schema:string_type(),
                fun(_) -> {ok, <<"pong"/utf8>>} end,
                fun(S) -> gleam_stdlib:identity(S) end
            )
        ),
        _pipe@2 = mochi@query:add_unions(_pipe@1, [Union]),
        mochi@query:build(_pipe@2)
    end,
    Result = mochi@executor:execute_query(Schema_def, <<"{ ping }"/utf8>>),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"add_unions should not break schema"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"batch_helpers_test"/utf8>>,
                    function => <<"add_unions_test"/utf8>>,
                    line => 283})
    end.
