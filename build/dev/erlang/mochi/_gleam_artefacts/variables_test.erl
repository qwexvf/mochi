-module(variables_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/variables_test.gleam").
-export([parse_query_with_variables_test/0, parse_multiple_variables_test/0, parse_list_type_variable_test/0, parse_field_with_string_argument_test/0, parse_field_with_int_argument_test/0, parse_field_with_variable_argument_test/0, parse_field_with_boolean_argument_test/0, parse_field_with_null_argument_test/0, parse_field_with_list_argument_test/0, parse_field_with_object_argument_test/0, parse_multiple_arguments_test/0, executor_with_variables_test/0]).
-export_type([test_user/0]).

-type test_user() :: {test_user, binary(), binary(), integer()}.

-file("test/variables_test.gleam", 17).
-spec assert_eq(UUF, UUF, binary()) -> nil.
assert_eq(A, B, Message) ->
    case A =:= B of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => Message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"assert_eq"/utf8>>,
                    line => 20})
    end.

-file("test/variables_test.gleam", 24).
-spec parse_ok(binary()) -> mochi@ast:document().
parse_ok(Query) ->
    case mochi@parser:parse(Query) of
        {ok, Doc} ->
            Doc;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"parse_ok"/utf8>>,
                    line => 27})
    end.

-file("test/variables_test.gleam", 31).
-spec get_first_operation(mochi@ast:document()) -> mochi@ast:operation().
get_first_operation(Doc) ->
    case erlang:element(2, Doc) of
        [{operation_definition, Op} | _] ->
            Op;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected operation"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"get_first_operation"/utf8>>,
                    line => 34})
    end.

-file("test/variables_test.gleam", 38).
-spec get_variable_definitions(mochi@ast:operation()) -> list(mochi@ast:variable_definition()).
get_variable_definitions(Op) ->
    case Op of
        {operation, _, _, Vars, _, _} ->
            Vars;

        {shorthand_query, _} ->
            []
    end.

-file("test/variables_test.gleam", 45).
-spec get_first_field(mochi@ast:operation()) -> mochi@ast:field().
get_first_field(Op) ->
    Ss@2 = case Op of
        {operation, _, _, _, _, Ss} ->
            Ss;

        {shorthand_query, Ss@1} ->
            Ss@1
    end,
    case erlang:element(2, Ss@2) of
        [{field_selection, F} | _] ->
            F;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"get_first_field"/utf8>>,
                    line => 52})
    end.

-file("test/variables_test.gleam", 60).
-spec parse_query_with_variables_test() -> nil.
parse_query_with_variables_test() ->
    Doc = parse_ok(
        <<"query GetUser($id: ID!) { user(id: $id) { name } }"/utf8>>
    ),
    Vars = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_variable_definitions(_pipe@1)
    end,
    assert_eq(
        Vars,
        [{variable_definition,
                <<"id"/utf8>>,
                {non_null_type, {named_type, <<"ID"/utf8>>}},
                none,
                []}],
        <<"Should have one ID! variable"/utf8>>
    ).

-file("test/variables_test.gleam", 78).
-spec parse_multiple_variables_test() -> nil.
parse_multiple_variables_test() ->
    Doc = parse_ok(
        <<"query GetUsers($limit: Int, $offset: Int = 0) { users { id } }"/utf8>>
    ),
    Vars = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_variable_definitions(_pipe@1)
    end,
    assert_eq(erlang:length(Vars), 2, <<"Should have two variables"/utf8>>),
    case Vars of
        [First, Second] ->
            assert_eq(
                erlang:element(2, First),
                <<"limit"/utf8>>,
                <<"First should be limit"/utf8>>
            ),
            assert_eq(
                erlang:element(2, Second),
                <<"offset"/utf8>>,
                <<"Second should be offset"/utf8>>
            ),
            assert_eq(
                erlang:element(4, Second),
                {some, {int_value, 0}},
                <<"Offset should have default 0"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected two variables"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"parse_multiple_variables_test"/utf8>>,
                    line => 95})
    end.

-file("test/variables_test.gleam", 101).
-spec parse_list_type_variable_test() -> nil.
parse_list_type_variable_test() ->
    Doc = parse_ok(
        <<"query GetByIds($ids: [ID!]!) { users(ids: $ids) { name } }"/utf8>>
    ),
    Vars = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_variable_definitions(_pipe@1)
    end,
    case Vars of
        [{variable_definition, <<"ids"/utf8>>, T, _, _}] ->
            assert_eq(
                T,
                {non_null_type,
                    {list_type, {non_null_type, {named_type, <<"ID"/utf8>>}}}},
                <<"Type should be [ID!]!"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected ids variable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"variables_test"/utf8>>,
                    function => <<"parse_list_type_variable_test"/utf8>>,
                    line => 113})
    end.

-file("test/variables_test.gleam", 121).
-spec parse_field_with_string_argument_test() -> nil.
parse_field_with_string_argument_test() ->
    Doc = parse_ok(<<"{ user(id: \"123\") { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"id"/utf8>>, {string_value, <<"123"/utf8>>}}],
        <<"Should have string argument"/utf8>>
    ).

-file("test/variables_test.gleam", 132).
-spec parse_field_with_int_argument_test() -> nil.
parse_field_with_int_argument_test() ->
    Doc = parse_ok(<<"{ users(limit: 10) { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"limit"/utf8>>, {int_value, 10}}],
        <<"Should have int argument"/utf8>>
    ).

-file("test/variables_test.gleam", 143).
-spec parse_field_with_variable_argument_test() -> nil.
parse_field_with_variable_argument_test() ->
    Doc = parse_ok(
        <<"query GetUser($id: ID!) { user(id: $id) { name } }"/utf8>>
    ),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"id"/utf8>>, {variable_value, <<"id"/utf8>>}}],
        <<"Should have variable argument"/utf8>>
    ).

-file("test/variables_test.gleam", 154).
-spec parse_field_with_boolean_argument_test() -> nil.
parse_field_with_boolean_argument_test() ->
    Doc = parse_ok(<<"{ users(active: true) { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"active"/utf8>>, {boolean_value, true}}],
        <<"Should have boolean argument"/utf8>>
    ).

-file("test/variables_test.gleam", 165).
-spec parse_field_with_null_argument_test() -> nil.
parse_field_with_null_argument_test() ->
    Doc = parse_ok(<<"{ user(email: null) { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"email"/utf8>>, null_value}],
        <<"Should have null argument"/utf8>>
    ).

-file("test/variables_test.gleam", 176).
-spec parse_field_with_list_argument_test() -> nil.
parse_field_with_list_argument_test() ->
    Doc = parse_ok(<<"{ users(ids: [\"1\", \"2\", \"3\"]) { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    Expected_list = {list_value,
        [{string_value, <<"1"/utf8>>},
            {string_value, <<"2"/utf8>>},
            {string_value, <<"3"/utf8>>}]},
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"ids"/utf8>>, Expected_list}],
        <<"Should have list argument"/utf8>>
    ).

-file("test/variables_test.gleam", 193).
-spec parse_field_with_object_argument_test() -> nil.
parse_field_with_object_argument_test() ->
    Doc = parse_ok(
        <<"{ createUser(input: {name: \"John\", age: 30}) { id } }"/utf8>>
    ),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    Expected_obj = {object_value,
        [{object_field, <<"name"/utf8>>, {string_value, <<"John"/utf8>>}},
            {object_field, <<"age"/utf8>>, {int_value, 30}}]},
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"input"/utf8>>, Expected_obj}],
        <<"Should have object argument"/utf8>>
    ).

-file("test/variables_test.gleam", 209).
-spec parse_multiple_arguments_test() -> nil.
parse_multiple_arguments_test() ->
    Doc = parse_ok(
        <<"{ users(limit: 10, offset: 20, active: true) { name } }"/utf8>>
    ),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:length(erlang:element(4, Field)),
        3,
        <<"Should have three arguments"/utf8>>
    ).

-file("test/variables_test.gleam", 224).
-spec decode_test_user(gleam@dynamic:dynamic_()) -> {ok, test_user()} |
    {error, binary()}.
decode_test_user(_) ->
    {ok, {test_user, <<"1"/utf8>>, <<"Test User"/utf8>>, 25}}.

-file("test/variables_test.gleam", 228).
-spec create_test_schema() -> mochi@schema:schema().
create_test_schema() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(3, U@1) end
        ),
        _pipe@3 = mochi@types:int(
            _pipe@2,
            <<"age"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_test_user/1)
    end,
    User_query = mochi@query:query_with_args(
        <<"user"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args) -> _pipe@4 = gleam_stdlib:map_get(Args, <<"id"/utf8>>),
            _pipe@5 = gleam@result:map(_pipe@4, fun(_) -> <<"1"/utf8>> end),
            gleam@result:map_error(_pipe@5, fun(_) -> <<"Missing id"/utf8>> end) end,
        fun(Id, _) -> {ok, {test_user, Id, <<"User "/utf8, Id/binary>>, 25}} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe@6 = mochi@query:new(),
    _pipe@7 = mochi@query:add_query(_pipe@6, User_query),
    _pipe@8 = mochi@query:add_type(_pipe@7, User_type),
    mochi@query:build(_pipe@8).

-file("test/variables_test.gleam", 258).
-spec executor_with_variables_test() -> nil.
executor_with_variables_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"query GetUser($id: ID!) { user(id: $id) { name } }"/utf8>>,
    Variables = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"123"/utf8>>)}]
    ),
    _ = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        Variables
    ),
    nil.
