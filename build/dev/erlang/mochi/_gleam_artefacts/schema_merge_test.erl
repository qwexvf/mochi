-module(schema_merge_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/schema_merge_test.gleam").
-export([merge_query_and_mutation_schemas_test/0, merge_schema_query_with_args_test/0, merge_schema_mutation_test/0, merge_schema_update_mutation_test/0, merge_preserves_all_queries_and_mutations_test/0, merge_empty_builder_is_identity_test/0, merge_multiple_schemas_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), binary()}.

-file("test/schema_merge_test.gleam", 17).
-spec user_type() -> mochi@schema:object_type().
user_type() ->
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
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"email"/utf8>>,
        fun(U@2) -> erlang:element(4, U@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Alice"/utf8>>,
                    <<"alice@example.com"/utf8>>}}
        end
    ).

-file("test/schema_merge_test.gleam", 25).
-spec sample_users() -> list(user()).
sample_users() ->
    [{user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>},
        {user, <<"2"/utf8>>, <<"Bob"/utf8>>, <<"bob@example.com"/utf8>>}].

-file("test/schema_merge_test.gleam", 36).
-spec user_queries_schema() -> mochi@query:schema_builder().
user_queries_schema() ->
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, sample_users()} end,
        fun(Users) -> gleam_stdlib:identity(Users) end
    ),
    User_query = mochi@query:query_with_args(
        <<"user"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args) -> mochi@query:get_id(Args, <<"id"/utf8>>) end,
        fun(_, _) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Alice"/utf8>>,
                    <<"alice@example.com"/utf8>>}}
        end,
        fun(User) -> gleam_stdlib:identity(User) end
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Users_query),
    _pipe@2 = mochi@query:add_query(_pipe@1, User_query),
    mochi@query:add_type(_pipe@2, user_type()).

-file("test/schema_merge_test.gleam", 65).
-spec user_mutations_schema() -> mochi@query:schema_builder().
user_mutations_schema() ->
    Create_user = mochi@query:mutation(
        <<"createUser"/utf8>>,
        [mochi@query:arg(
                <<"name"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@query:arg(
                <<"email"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args) ->
            case {mochi@query:get_string(Args, <<"name"/utf8>>),
                mochi@query:get_string(Args, <<"email"/utf8>>)} of
                {{ok, Name}, {ok, Email}} ->
                    {ok, {Name, Email}};

                {_, _} ->
                    {error, <<"Missing name or email"/utf8>>}
            end
        end,
        fun(Input, _) ->
            {Name@1, Email@1} = Input,
            {ok, {user, <<"3"/utf8>>, Name@1, Email@1}}
        end,
        fun(User) -> gleam_stdlib:identity(User) end
    ),
    Update_user = mochi@query:mutation(
        <<"updateUser"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            ),
            mochi@query:arg(
                <<"name"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args@1) ->
            case {mochi@query:get_id(Args@1, <<"id"/utf8>>),
                mochi@query:get_string(Args@1, <<"name"/utf8>>)} of
                {{ok, Id}, {ok, Name@2}} ->
                    {ok, {Id, Name@2}};

                {_, _} ->
                    {error, <<"Missing id or name"/utf8>>}
            end
        end,
        fun(Input@1, _) ->
            {Id@1, Name@3} = Input@1,
            {ok, {user, Id@1, Name@3, <<"updated@example.com"/utf8>>}}
        end,
        fun(User@1) -> gleam_stdlib:identity(User@1) end
    ),
    _pipe = mochi@query:new(),
    mochi@query:add_mutations(_pipe, [Create_user, Update_user]).

-file("test/schema_merge_test.gleam", 116).
-spec merge_query_and_mutation_schemas_test() -> nil.
merge_query_and_mutation_schemas_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, user_mutations_schema()),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ users { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query 'users' should resolve without errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_query_and_mutation_schemas_test"/utf8>>,
                    line => 127})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query 'users' should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_query_and_mutation_schemas_test"/utf8>>,
                    line => 132})
    end.

-file("test/schema_merge_test.gleam", 136).
-spec merge_schema_query_with_args_test() -> nil.
merge_schema_query_with_args_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, user_mutations_schema()),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ user(id: \"1\") { id name email } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query 'user(id:)' should resolve without errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_query_with_args_test"/utf8>>,
                    line => 147})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query 'user(id:)' should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_query_with_args_test"/utf8>>,
                    line => 152})
    end.

-file("test/schema_merge_test.gleam", 156).
-spec merge_schema_mutation_test() -> nil.
merge_schema_mutation_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, user_mutations_schema()),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"mutation { createUser(name: \"Charlie\", email: \"charlie@example.com\") { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation 'createUser' should resolve without errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_mutation_test"/utf8>>,
                    line => 170})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation 'createUser' should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_mutation_test"/utf8>>,
                    line => 175})
    end.

-file("test/schema_merge_test.gleam", 179).
-spec merge_schema_update_mutation_test() -> nil.
merge_schema_update_mutation_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, user_mutations_schema()),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"mutation { updateUser(id: \"1\", name: \"Updated\") { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation 'updateUser' should resolve without errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_update_mutation_test"/utf8>>,
                    line => 193})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation 'updateUser' should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_schema_update_mutation_test"/utf8>>,
                    line => 198})
    end.

-file("test/schema_merge_test.gleam", 202).
-spec merge_preserves_all_queries_and_mutations_test() -> nil.
merge_preserves_all_queries_and_mutations_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, user_mutations_schema()),
        mochi@query:build(_pipe@1)
    end,
    R1 = mochi@executor:execute_query(Schema, <<"{ users { id } }"/utf8>>),
    R2 = mochi@executor:execute_query(
        Schema,
        <<"{ user(id: \"1\") { id } }"/utf8>>
    ),
    case {erlang:element(3, R1), erlang:element(3, R2)} of
        {[], []} ->
            nil;

        {_, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Both queries should be available after merge"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_preserves_all_queries_and_mutations_test"/utf8>>,
                    line => 214})
    end,
    R3 = mochi@executor:execute_query(
        Schema,
        <<"mutation { createUser(name: \"A\", email: \"a@b.com\") { id } }"/utf8>>
    ),
    R4 = mochi@executor:execute_query(
        Schema,
        <<"mutation { updateUser(id: \"1\", name: \"B\") { id } }"/utf8>>
    ),
    case {erlang:element(3, R3), erlang:element(3, R4)} of
        {[], []} ->
            nil;

        {_, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Both mutations should be available after merge"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_preserves_all_queries_and_mutations_test"/utf8>>,
                    line => 231})
    end.

-file("test/schema_merge_test.gleam", 235).
-spec merge_empty_builder_is_identity_test() -> nil.
merge_empty_builder_is_identity_test() ->
    Schema = begin
        _pipe = user_queries_schema(),
        _pipe@1 = mochi@query:merge(_pipe, mochi@query:new()),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ users { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Merging with empty builder should not break anything"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_empty_builder_is_identity_test"/utf8>>,
                    line => 245})
    end.

-file("test/schema_merge_test.gleam", 249).
-spec merge_multiple_schemas_test() -> nil.
merge_multiple_schemas_test() ->
    Queries = user_queries_schema(),
    Mutations = user_mutations_schema(),
    Extra = begin
        _pipe = mochi@query:new(),
        mochi@query:add_query(
            _pipe,
            mochi@query:'query'(
                <<"userCount"/utf8>>,
                mochi@schema:int_type(),
                fun(_) -> {ok, 42} end,
                fun(Count) -> gleam_stdlib:identity(Count) end
            )
        )
    end,
    Schema = begin
        _pipe@1 = Queries,
        _pipe@2 = mochi@query:merge(_pipe@1, Mutations),
        _pipe@3 = mochi@query:merge(_pipe@2, Extra),
        mochi@query:build(_pipe@3)
    end,
    R1 = mochi@executor:execute_query(Schema, <<"{ users { id } }"/utf8>>),
    R2 = mochi@executor:execute_query(Schema, <<"{ userCount }"/utf8>>),
    R3 = mochi@executor:execute_query(
        Schema,
        <<"mutation { createUser(name: \"X\", email: \"x@y.com\") { id } }"/utf8>>
    ),
    case {erlang:element(3, R1), erlang:element(3, R2), erlang:element(3, R3)} of
        {[], [], []} ->
            nil;

        {_, _, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"All three merged domains should work"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"schema_merge_test"/utf8>>,
                    function => <<"merge_multiple_schemas_test"/utf8>>,
                    line => 282})
    end.
