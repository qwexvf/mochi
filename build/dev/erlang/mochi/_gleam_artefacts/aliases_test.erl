-module(aliases_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/aliases_test.gleam").
-export([parse_simple_alias_test/0, parse_alias_with_arguments_test/0, parse_multiple_aliases_same_field_test/0, parse_alias_without_arguments_test/0, parse_nested_aliases_test/0, parse_field_without_alias_test/0, parse_mixed_alias_and_no_alias_test/0, parse_alias_in_named_query_test/0, execute_simple_alias_test/0, execute_alias_with_arguments_test/0, execute_multiple_aliases_same_field_test/0, execute_alias_without_selection_set_test/0, execute_nested_aliases_test/0, execute_mixed_alias_and_no_alias_test/0, execute_alias_with_variables_test/0, execute_multiple_scalar_aliases_test/0, execute_deeply_nested_aliases_test/0, parse_alias_same_as_field_name_test/0, execute_alias_same_as_field_name_test/0]).
-export_type([test_user/0]).

-type test_user() :: {test_user, binary(), binary(), binary()}.

-file("test/aliases_test.gleam", 28).
-spec assert_eq(HFV, HFV, binary()) -> nil.
assert_eq(A, B, Message) ->
    case A =:= B of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => Message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"assert_eq"/utf8>>,
                    line => 31})
    end.

-file("test/aliases_test.gleam", 35).
-spec assert_true(boolean(), binary()) -> nil.
assert_true(Condition, Message) ->
    case Condition of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => Message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"assert_true"/utf8>>,
                    line => 38})
    end.

-file("test/aliases_test.gleam", 42).
-spec parse_ok(binary()) -> mochi@ast:document().
parse_ok(Query_str) ->
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Doc;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"parse_ok"/utf8>>,
                    line => 45})
    end.

-file("test/aliases_test.gleam", 49).
-spec get_first_operation(mochi@ast:document()) -> mochi@ast:operation().
get_first_operation(Doc) ->
    case erlang:element(2, Doc) of
        [{operation_definition, Op} | _] ->
            Op;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected operation"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"get_first_operation"/utf8>>,
                    line => 52})
    end.

-file("test/aliases_test.gleam", 56).
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
                    module => <<"aliases_test"/utf8>>,
                    function => <<"get_first_field"/utf8>>,
                    line => 63})
    end.

-file("test/aliases_test.gleam", 67).
-spec get_all_fields(mochi@ast:operation()) -> list(mochi@ast:field()).
get_all_fields(Op) ->
    Ss@2 = case Op of
        {operation, _, _, _, _, Ss} ->
            Ss;

        {shorthand_query, Ss@1} ->
            Ss@1
    end,
    gleam@list:filter_map(erlang:element(2, Ss@2), fun(Sel) -> case Sel of
                {field_selection, F} ->
                    {ok, F};

                _ ->
                    {error, nil}
            end end).

-file("test/aliases_test.gleam", 80).
-spec decode_test_user(gleam@dynamic:dynamic_()) -> {ok, test_user()} |
    {error, binary()}.
decode_test_user(_) ->
    {ok,
        {test_user,
            <<"1"/utf8>>,
            <<"Test User"/utf8>>,
            <<"test@example.com"/utf8>>}}.

-file("test/aliases_test.gleam", 88).
-spec create_test_schema() -> mochi@schema:schema().
create_test_schema() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:description(
            _pipe,
            <<"A user in the system"/utf8>>
        ),
        _pipe@2 = mochi@types:id(
            _pipe@1,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(3, U@1) end
        ),
        _pipe@4 = mochi@types:string(
            _pipe@3,
            <<"email"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@4, fun decode_test_user/1)
    end,
    User_query = mochi@query:query_with_args(
        <<"user"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args) -> _pipe@5 = gleam_stdlib:map_get(Args, <<"id"/utf8>>),
            _pipe@6 = gleam@result:map(_pipe@5, fun(_) -> <<"1"/utf8>> end),
            gleam@result:map_error(_pipe@6, fun(_) -> <<"Missing id"/utf8>> end) end,
        fun(Id, _) ->
            {ok,
                {test_user,
                    Id,
                    <<"User "/utf8, Id/binary>>,
                    <<"user@example.com"/utf8>>}}
        end,
        fun gleam_stdlib:identity/1
    ),
    Name_query = mochi@query:'query'(
        <<"name"/utf8>>,
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"John Doe"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Greeting_query = mochi@query:'query'(
        <<"greeting"/utf8>>,
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"Hello, World!"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe@7 = mochi@query:new(),
    _pipe@8 = mochi@query:add_query(_pipe@7, User_query),
    _pipe@9 = mochi@query:add_query(_pipe@8, Name_query),
    _pipe@10 = mochi@query:add_query(_pipe@9, Greeting_query),
    _pipe@11 = mochi@query:add_type(_pipe@10, User_type),
    mochi@query:build(_pipe@11).

-file("test/aliases_test.gleam", 142).
-spec parse_simple_alias_test() -> nil.
parse_simple_alias_test() ->
    Doc = parse_ok(<<"{ userName: name }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"userName"/utf8>>},
        <<"Field should have alias 'userName'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"name"/utf8>>,
        <<"Field name should be 'name'"/utf8>>
    ).

-file("test/aliases_test.gleam", 150).
-spec parse_alias_with_arguments_test() -> nil.
parse_alias_with_arguments_test() ->
    Doc = parse_ok(<<"{ admin: user(id: \"1\") { name } }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"admin"/utf8>>},
        <<"Field should have alias 'admin'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"user"/utf8>>,
        <<"Field name should be 'user'"/utf8>>
    ),
    assert_eq(
        erlang:element(4, Field),
        [{argument, <<"id"/utf8>>, {string_value, <<"1"/utf8>>}}],
        <<"Should have id argument"/utf8>>
    ),
    assert_true(
        gleam@option:is_some(erlang:element(6, Field)),
        <<"Should have selection set"/utf8>>
    ).

-file("test/aliases_test.gleam", 164).
-spec parse_multiple_aliases_same_field_test() -> nil.
parse_multiple_aliases_same_field_test() ->
    Doc = parse_ok(
        <<"{ a: user(id: \"1\") { name } b: user(id: \"2\") { name } }"/utf8>>
    ),
    Fields = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_all_fields(_pipe@1)
    end,
    assert_eq(erlang:length(Fields), 2, <<"Should have two fields"/utf8>>),
    case Fields of
        [First, Second] ->
            assert_eq(
                erlang:element(2, First),
                {some, <<"a"/utf8>>},
                <<"First field should have alias 'a'"/utf8>>
            ),
            assert_eq(
                erlang:element(3, First),
                <<"user"/utf8>>,
                <<"First field name should be 'user'"/utf8>>
            ),
            assert_eq(
                erlang:element(2, Second),
                {some, <<"b"/utf8>>},
                <<"Second field should have alias 'b'"/utf8>>
            ),
            assert_eq(
                erlang:element(3, Second),
                <<"user"/utf8>>,
                <<"Second field name should be 'user'"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected two fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"parse_multiple_aliases_same_field_test"/utf8>>,
                    line => 179})
    end.

-file("test/aliases_test.gleam", 183).
-spec parse_alias_without_arguments_test() -> nil.
parse_alias_without_arguments_test() ->
    Doc = parse_ok(<<"{ myName: name }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"myName"/utf8>>},
        <<"Field should have alias 'myName'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"name"/utf8>>,
        <<"Field name should be 'name'"/utf8>>
    ),
    assert_eq(erlang:element(4, Field), [], <<"Should have no arguments"/utf8>>).

-file("test/aliases_test.gleam", 192).
-spec parse_nested_aliases_test() -> nil.
parse_nested_aliases_test() ->
    Doc = parse_ok(
        <<"{ adminUser: user(id: \"1\") { userName: name userEmail: email } }"/utf8>>
    ),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"adminUser"/utf8>>},
        <<"Root field should have alias 'adminUser'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"user"/utf8>>,
        <<"Root field name should be 'user'"/utf8>>
    ),
    case erlang:element(6, Field) of
        {some, Ss} ->
            Nested_fields = gleam@list:filter_map(
                erlang:element(2, Ss),
                fun(Sel) -> case Sel of
                        {field_selection, F} ->
                            {ok, F};

                        _ ->
                            {error, nil}
                    end end
            ),
            assert_eq(
                erlang:length(Nested_fields),
                2,
                <<"Should have two nested fields"/utf8>>
            ),
            case Nested_fields of
                [First, Second] ->
                    assert_eq(
                        erlang:element(2, First),
                        {some, <<"userName"/utf8>>},
                        <<"First nested field should have alias 'userName'"/utf8>>
                    ),
                    assert_eq(
                        erlang:element(3, First),
                        <<"name"/utf8>>,
                        <<"First nested field name should be 'name'"/utf8>>
                    ),
                    assert_eq(
                        erlang:element(2, Second),
                        {some, <<"userEmail"/utf8>>},
                        <<"Second nested field should have alias 'userEmail'"/utf8>>
                    ),
                    assert_eq(
                        erlang:element(3, Second),
                        <<"email"/utf8>>,
                        <<"Second nested field name should be 'email'"/utf8>>
                    );

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected two nested fields"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"aliases_test"/utf8>>,
                            function => <<"parse_nested_aliases_test"/utf8>>,
                            line => 242})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected selection set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"parse_nested_aliases_test"/utf8>>,
                    line => 245})
    end.

-file("test/aliases_test.gleam", 249).
-spec parse_field_without_alias_test() -> nil.
parse_field_without_alias_test() ->
    Doc = parse_ok(<<"{ name }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        none,
        <<"Field should have no alias"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"name"/utf8>>,
        <<"Field name should be 'name'"/utf8>>
    ).

-file("test/aliases_test.gleam", 257).
-spec parse_mixed_alias_and_no_alias_test() -> nil.
parse_mixed_alias_and_no_alias_test() ->
    Doc = parse_ok(<<"{ userName: name email age }"/utf8>>),
    Fields = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_all_fields(_pipe@1)
    end,
    assert_eq(erlang:length(Fields), 3, <<"Should have three fields"/utf8>>),
    case Fields of
        [First, Second, Third] ->
            assert_eq(
                erlang:element(2, First),
                {some, <<"userName"/utf8>>},
                <<"First field should have alias"/utf8>>
            ),
            assert_eq(
                erlang:element(3, First),
                <<"name"/utf8>>,
                <<"First field name should be 'name'"/utf8>>
            ),
            assert_eq(
                erlang:element(2, Second),
                none,
                <<"Second field should have no alias"/utf8>>
            ),
            assert_eq(
                erlang:element(3, Second),
                <<"email"/utf8>>,
                <<"Second field name should be 'email'"/utf8>>
            ),
            assert_eq(
                erlang:element(2, Third),
                none,
                <<"Third field should have no alias"/utf8>>
            ),
            assert_eq(
                erlang:element(3, Third),
                <<"age"/utf8>>,
                <<"Third field name should be 'age'"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected three fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"parse_mixed_alias_and_no_alias_test"/utf8>>,
                    line => 274})
    end.

-file("test/aliases_test.gleam", 278).
-spec parse_alias_in_named_query_test() -> nil.
parse_alias_in_named_query_test() ->
    Doc = parse_ok(
        <<"query GetUsers { adminUser: user(id: \"1\") { name } }"/utf8>>
    ),
    Op = begin
        _pipe = Doc,
        get_first_operation(_pipe)
    end,
    Field = get_first_field(Op),
    case Op of
        {operation, _, Op_name, _, _, _} ->
            assert_eq(
                Op_name,
                {some, <<"GetUsers"/utf8>>},
                <<"Query should have name"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected named query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"aliases_test"/utf8>>,
                    function => <<"parse_alias_in_named_query_test"/utf8>>,
                    line => 286})
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"adminUser"/utf8>>},
        <<"Field should have alias 'adminUser'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"user"/utf8>>,
        <<"Field name should be 'user'"/utf8>>
    ).

-file("test/aliases_test.gleam", 304).
-spec execute_simple_alias_test() -> nil.
execute_simple_alias_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ userName: name }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 313).
-spec execute_alias_with_arguments_test() -> nil.
execute_alias_with_arguments_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ admin: user(id: \"1\") { name } }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 322).
-spec execute_multiple_aliases_same_field_test() -> nil.
execute_multiple_aliases_same_field_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ adminUser: user(id: \"1\") { name } regularUser: user(id: \"2\") { name } }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 332).
-spec execute_alias_without_selection_set_test() -> nil.
execute_alias_without_selection_set_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ myName: name }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 341).
-spec execute_nested_aliases_test() -> nil.
execute_nested_aliases_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ admin: user(id: \"1\") { userName: name userEmail: email } }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 351).
-spec execute_mixed_alias_and_no_alias_test() -> nil.
execute_mixed_alias_and_no_alias_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ admin: user(id: \"1\") { userName: name email } }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 360).
-spec execute_alias_with_variables_test() -> nil.
execute_alias_with_variables_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"query GetUser($userId: ID!) { admin: user(id: $userId) { name } }"/utf8>>,
    Variables = maps:from_list(
        [{<<"userId"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)}]
    ),
    _ = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        Variables
    ),
    nil.

-file("test/aliases_test.gleam", 372).
-spec execute_multiple_scalar_aliases_test() -> nil.
execute_multiple_scalar_aliases_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ myName: name myGreeting: greeting }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 382).
-spec execute_deeply_nested_aliases_test() -> nil.
execute_deeply_nested_aliases_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ adminUser: user(id: \"1\") { userId: id userName: name userEmail: email } }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.

-file("test/aliases_test.gleam", 397).
-spec parse_alias_same_as_field_name_test() -> nil.
parse_alias_same_as_field_name_test() ->
    Doc = parse_ok(<<"{ name: name }"/utf8>>),
    Field = begin
        _pipe = Doc,
        _pipe@1 = get_first_operation(_pipe),
        get_first_field(_pipe@1)
    end,
    assert_eq(
        erlang:element(2, Field),
        {some, <<"name"/utf8>>},
        <<"Field should have alias 'name'"/utf8>>
    ),
    assert_eq(
        erlang:element(3, Field),
        <<"name"/utf8>>,
        <<"Field name should be 'name'"/utf8>>
    ).

-file("test/aliases_test.gleam", 406).
-spec execute_alias_same_as_field_name_test() -> nil.
execute_alias_same_as_field_name_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ name: name }"/utf8>>,
    _ = mochi@executor:execute_query(Test_schema, Query_str),
    nil.
