-module(null_propagation_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/null_propagation_test.gleam").
-export([nullable_field_returns_null_test/0, non_null_field_returns_null_test/0, null_bubbles_to_nullable_parent_test/0, null_bubbles_to_root_when_all_non_null_test/0, errors_collected_during_null_propagation_test/0, list_with_non_null_items_null_test/0, nested_non_null_fields_test/0, valid_non_null_field_test/0, nullable_list_items_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), gleam@option:option(binary())}.

-file("test/null_propagation_test.gleam", 20).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok,
        {user,
            <<"1"/utf8>>,
            <<"Test"/utf8>>,
            {some, <<"test@example.com"/utf8>>}}}.

-file("test/null_propagation_test.gleam", 28).
-spec build_test_schema_with_user_resolver(
    fun((mochi@schema:execution_context()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> mochi@schema:schema().
build_test_schema_with_user_resolver(User_resolver) ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        mochi@schema:named_type(<<"User"/utf8>>),
        User_resolver,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, User_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    mochi@query:build(_pipe@5).

-file("test/null_propagation_test.gleam", 49).
-spec build_test_schema_with_required_user_resolver(
    fun((mochi@schema:execution_context()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> mochi@schema:schema().
build_test_schema_with_required_user_resolver(User_resolver) ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    User_query = mochi@query:'query'(
        <<"requiredUser"/utf8>>,
        mochi@schema:non_null(mochi@schema:named_type(<<"User"/utf8>>)),
        User_resolver,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, User_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    mochi@query:build(_pipe@5).

-file("test/null_propagation_test.gleam", 77).
-spec nullable_field_returns_null_test() -> nil.
nullable_field_returns_null_test() ->
    Test_schema = build_test_schema_with_user_resolver(
        fun(_) -> {ok, gleam_stdlib:identity(nil)} end
    ),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ user { id name } }"/utf8>>
    ),
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have data even when nullable field is null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nullable_field_returns_null_test"/utf8>>,
                    line => 90})
    end,
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have no errors for nullable field returning null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nullable_field_returns_null_test"/utf8>>,
                    line => 95})
    end.

-file("test/null_propagation_test.gleam", 103).
-spec non_null_field_returns_null_test() -> nil.
non_null_field_returns_null_test() ->
    Test_schema = build_test_schema_with_required_user_resolver(
        fun(_) -> {ok, gleam_stdlib:identity(nil)} end
    ),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ requiredUser { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [{null_value_error, _, _, _} | _] ->
            nil;

        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error for non-null field returning null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"non_null_field_returns_null_test"/utf8>>,
                    line => 117});

        _ ->
            nil
    end,
    case erlang:element(2, Result) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Data should be null when non-null field at root returns null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"non_null_field_returns_null_test"/utf8>>,
                    line => 126})
    end.

-file("test/null_propagation_test.gleam", 134).
-spec null_bubbles_to_nullable_parent_test() -> nil.
null_bubbles_to_nullable_parent_test() ->
    Email_type = begin
        _pipe = mochi@schema:object(<<"Email"/utf8>>),
        mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"address"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) -> {ok, gleam_stdlib:identity(nil)} end
                )
            end
        )
    end,
    User_type = begin
        _pipe@2 = mochi@schema:object(<<"User"/utf8>>),
        _pipe@4 = mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:id_type()
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
                )
            end
        ),
        mochi@schema:field(
            _pipe@4,
            begin
                _pipe@5 = mochi@schema:field_def(
                    <<"email"/utf8>>,
                    mochi@schema:named_type(<<"Email"/utf8>>)
                ),
                mochi@schema:resolver(
                    _pipe@5,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"address"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@6 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@6,
            begin
                _pipe@7 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                mochi@schema:resolver(
                    _pipe@7,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"id"/utf8>>,
                                            gleam_stdlib:identity(<<"1"/utf8>>)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Test_schema = begin
        _pipe@8 = mochi@schema:schema(),
        _pipe@9 = mochi@schema:'query'(_pipe@8, Query_type),
        _pipe@10 = mochi@schema:add_type(_pipe@9, {object_type_def, User_type}),
        mochi@schema:add_type(_pipe@10, {object_type_def, Email_type})
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ user { id email { address } } }"/utf8>>
    ),
    case erlang:element(3, Result) /= [] of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error for non-null field returning null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"null_bubbles_to_nullable_parent_test"/utf8>>,
                    line => 188})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Data should exist when null bubbles to nullable parent"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"null_bubbles_to_nullable_parent_test"/utf8>>,
                    line => 194})
    end.

-file("test/null_propagation_test.gleam", 202).
-spec null_bubbles_to_root_when_all_non_null_test() -> nil.
null_bubbles_to_root_when_all_non_null_test() ->
    Email_type = begin
        _pipe = mochi@schema:object(<<"Email"/utf8>>),
        mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"address"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) -> {ok, gleam_stdlib:identity(nil)} end
                )
            end
        )
    end,
    User_type = begin
        _pipe@2 = mochi@schema:object(<<"User"/utf8>>),
        _pipe@4 = mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:id_type()
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
                )
            end
        ),
        mochi@schema:field(
            _pipe@4,
            begin
                _pipe@5 = mochi@schema:field_def(
                    <<"email"/utf8>>,
                    mochi@schema:non_null(
                        mochi@schema:named_type(<<"Email"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@5,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"address"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@6 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@6,
            begin
                _pipe@7 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:non_null(
                        mochi@schema:named_type(<<"User"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@7,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"id"/utf8>>,
                                            gleam_stdlib:identity(<<"1"/utf8>>)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Test_schema = begin
        _pipe@8 = mochi@schema:schema(),
        _pipe@9 = mochi@schema:'query'(_pipe@8, Query_type),
        _pipe@10 = mochi@schema:add_type(_pipe@9, {object_type_def, User_type}),
        mochi@schema:add_type(_pipe@10, {object_type_def, Email_type})
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ user { id email { address } } }"/utf8>>
    ),
    case erlang:element(3, Result) /= [] of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"null_bubbles_to_root_when_all_non_null_test"/utf8>>,
                    line => 254})
    end,
    case erlang:element(2, Result) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Data should be null when null bubbles to root"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"null_bubbles_to_root_when_all_non_null_test"/utf8>>,
                    line => 260})
    end.

-file("test/null_propagation_test.gleam", 268).
-spec errors_collected_during_null_propagation_test() -> nil.
errors_collected_during_null_propagation_test() ->
    Test_schema = build_test_schema_with_required_user_resolver(
        fun(_) -> {ok, gleam_stdlib:identity(nil)} end
    ),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ requiredUser { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [{null_value_error, _, Path, _} | _] ->
            case Path /= [] of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Error path should not be empty"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"null_propagation_test"/utf8>>,
                            function => <<"errors_collected_during_null_propagation_test"/utf8>>,
                            line => 283})
            end;

        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"errors_collected_during_null_propagation_test"/utf8>>,
                    line => 286});

        _ ->
            nil
    end.

-file("test/null_propagation_test.gleam", 298).
-spec list_with_non_null_items_null_test() -> nil.
list_with_non_null_items_null_test() ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:id_type()
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@2 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"users"/utf8>>,
                    mochi@schema:list_type(
                        mochi@schema:non_null(
                            mochi@schema:named_type(<<"User"/utf8>>)
                        )
                    )
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) ->
                        Item1 = gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"id"/utf8>>,
                                        gleam_stdlib:identity(<<"1"/utf8>>)}]
                            )
                        ),
                        Item2 = gleam_stdlib:identity(nil),
                        Item3 = gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"id"/utf8>>,
                                        gleam_stdlib:identity(<<"3"/utf8>>)}]
                            )
                        ),
                        {ok, gleam_stdlib:identity([Item1, Item2, Item3])}
                    end
                )
            end
        )
    end,
    Test_schema = begin
        _pipe@4 = mochi@schema:schema(),
        _pipe@5 = mochi@schema:'query'(_pipe@4, Query_type),
        mochi@schema:add_type(_pipe@5, {object_type_def, User_type})
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ users { id } }"/utf8>>
    ),
    case erlang:element(3, Result) /= [] of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error for null item in [User!] list"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"list_with_non_null_items_null_test"/utf8>>,
                    line => 337})
    end.

-file("test/null_propagation_test.gleam", 345).
-spec nested_non_null_fields_test() -> nil.
nested_non_null_fields_test() ->
    Address_type = begin
        _pipe = mochi@schema:object(<<"Address"/utf8>>),
        mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"street"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) -> {ok, gleam_stdlib:identity(nil)} end
                )
            end
        )
    end,
    User_type = begin
        _pipe@2 = mochi@schema:object(<<"User"/utf8>>),
        _pipe@4 = mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:id_type()
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
                )
            end
        ),
        mochi@schema:field(
            _pipe@4,
            begin
                _pipe@5 = mochi@schema:field_def(
                    <<"address"/utf8>>,
                    mochi@schema:non_null(
                        mochi@schema:named_type(<<"Address"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@5,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"street"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@6 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@6,
            begin
                _pipe@7 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                mochi@schema:resolver(
                    _pipe@7,
                    fun(_) ->
                        {ok,
                            gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"id"/utf8>>,
                                            gleam_stdlib:identity(<<"1"/utf8>>)}]
                                )
                            )}
                    end
                )
            end
        )
    end,
    Test_schema = begin
        _pipe@8 = mochi@schema:schema(),
        _pipe@9 = mochi@schema:'query'(_pipe@8, Query_type),
        _pipe@10 = mochi@schema:add_type(_pipe@9, {object_type_def, User_type}),
        mochi@schema:add_type(_pipe@10, {object_type_def, Address_type})
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ user { id address { street } } }"/utf8>>
    ),
    case erlang:element(3, Result) /= [] of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have error for nested non-null returning null"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nested_non_null_fields_test"/utf8>>,
                    line => 394})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Data should exist when null stops at nullable parent"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nested_non_null_fields_test"/utf8>>,
                    line => 400})
    end.

-file("test/null_propagation_test.gleam", 408).
-spec valid_non_null_field_test() -> nil.
valid_non_null_field_test() ->
    Test_schema = build_test_schema_with_required_user_resolver(
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)},
                            {<<"name"/utf8>>,
                                gleam_stdlib:identity(<<"Test User"/utf8>>)}]
                    )
                )}
        end
    ),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ requiredUser { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have no errors for valid non-null field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"valid_non_null_field_test"/utf8>>,
                    line => 428})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have data for valid query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"valid_non_null_field_test"/utf8>>,
                    line => 434})
    end.

-file("test/null_propagation_test.gleam", 442).
-spec nullable_list_items_test() -> nil.
nullable_list_items_test() ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        mochi@schema:field(
            _pipe,
            begin
                _pipe@1 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:id_type()
                ),
                mochi@schema:resolver(
                    _pipe@1,
                    fun(_) -> {ok, gleam_stdlib:identity(<<"1"/utf8>>)} end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@2 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@2,
            begin
                _pipe@3 = mochi@schema:field_def(
                    <<"users"/utf8>>,
                    mochi@schema:list_type(
                        mochi@schema:named_type(<<"User"/utf8>>)
                    )
                ),
                mochi@schema:resolver(
                    _pipe@3,
                    fun(_) ->
                        Item1 = gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"id"/utf8>>,
                                        gleam_stdlib:identity(<<"1"/utf8>>)}]
                            )
                        ),
                        Item2 = gleam_stdlib:identity(nil),
                        Item3 = gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"id"/utf8>>,
                                        gleam_stdlib:identity(<<"3"/utf8>>)}]
                            )
                        ),
                        {ok, gleam_stdlib:identity([Item1, Item2, Item3])}
                    end
                )
            end
        )
    end,
    Test_schema = begin
        _pipe@4 = mochi@schema:schema(),
        _pipe@5 = mochi@schema:'query'(_pipe@4, Query_type),
        mochi@schema:add_type(_pipe@5, {object_type_def, User_type})
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ users { id } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have no errors for nullable list items"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nullable_list_items_test"/utf8>>,
                    line => 476})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"null_propagation_test"/utf8>>,
                    function => <<"nullable_list_items_test"/utf8>>,
                    line => 482})
    end.
