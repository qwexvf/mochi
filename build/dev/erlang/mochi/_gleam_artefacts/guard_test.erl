-module(guard_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/guard_test.gleam").
-export([schema_guard_on_field_without_resolver_is_noop_test/0, query_guard_stacking_test/0, schema_guard_allows_resolver_test/0, schema_guard_blocks_resolver_test/0, schema_guards_multiple_all_pass_test/0, schema_guards_first_fails_test/0, schema_guards_second_fails_test/0, schema_guards_order_first_checked_first_test/0, query_guard_allows_test/0, query_guard_blocks_test/0, guard_error_message_preserved_test/0, mutation_guard_allows_test/0, mutation_guard_blocks_test/0, field_guard_allows_test/0, field_guard_blocks_test/0, subscription_guard_allows_test/0, subscription_guard_blocks_test/0, high_level_all_of_test/0, high_level_all_of_fails_test/0, high_level_any_of_test/0, high_level_any_of_all_fail_test/0, any_of_used_as_guard_test/0, all_guards_all_pass_test/0, all_guards_one_fails_test/0, all_guards_first_fails_short_circuits_test/0, any_guard_one_passes_test/0, any_guard_all_fail_test/0, any_guard_first_passes_short_circuits_test/0, any_guard_empty_list_test/0, any_guard_returns_last_error_test/0]).
-export_type([user/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type user() :: {user, binary(), binary()}.

-file("test/guard_test.gleam", 21).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>}}.

-file("test/guard_test.gleam", 25).
-spec user_to_dynamic(user()) -> gleam@dynamic:dynamic_().
user_to_dynamic(U) ->
    mochi@types:record(
        [mochi@types:field(<<"id"/utf8>>, erlang:element(2, U)),
            mochi@types:field(<<"name"/utf8>>, erlang:element(3, U))]
    ).

-file("test/guard_test.gleam", 29).
-spec default_ctx() -> mochi@schema:execution_context().
default_ctx() ->
    mochi@schema:execution_context(gleam_stdlib:identity(maps:new())).

-file("test/guard_test.gleam", 38).
?DOC(" A guard that always allows\n").
-spec allow_guard(mochi@schema:execution_context()) -> {ok, nil} |
    {error, binary()}.
allow_guard(_) ->
    {ok, nil}.

-file("test/guard_test.gleam", 43).
?DOC(" A guard that always denies\n").
-spec deny_guard(mochi@schema:execution_context()) -> {ok, nil} |
    {error, binary()}.
deny_guard(_) ->
    {error, <<"Access denied"/utf8>>}.

-file("test/guard_test.gleam", 48).
?DOC(" A low-level guard (takes ResolverInfo) that always allows\n").
-spec low_level_allow(mochi@schema:resolver_info()) -> {ok, nil} |
    {error, binary()}.
low_level_allow(_) ->
    {ok, nil}.

-file("test/guard_test.gleam", 53).
?DOC(" A low-level guard (takes ResolverInfo) that always denies\n").
-spec low_level_deny(mochi@schema:resolver_info()) -> {ok, nil} |
    {error, binary()}.
low_level_deny(_) ->
    {error, <<"Forbidden"/utf8>>}.

-file("test/guard_test.gleam", 159).
-spec schema_guard_on_field_without_resolver_is_noop_test() -> nil.
schema_guard_on_field_without_resolver_is_noop_test() ->
    Field_def = mochi@schema:field_def(
        <<"test"/utf8>>,
        mochi@schema:string_type()
    ),
    Guarded = mochi@schema:guard(Field_def, fun low_level_deny/1),
    gleeunit@should:equal(erlang:element(6, Guarded), none).

-file("test/guard_test.gleam", 200).
-spec query_guard_stacking_test() -> nil.
query_guard_stacking_test() ->
    Users_query = begin
        _pipe = mochi@query:'query'(
            <<"users"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
            fun(_) -> {ok, [{user, <<"1"/utf8>>, <<"Alice"/utf8>>}]} end,
            fun(Users) -> gleam_stdlib:identity(Users) end
        ),
        _pipe@1 = mochi@query:with_guard(_pipe, fun allow_guard/1),
        mochi@query:with_guard(_pipe@1, fun deny_guard/1)
    end,
    User_type = begin
        _pipe@2 = mochi@types:object(<<"User"/utf8>>),
        _pipe@3 = mochi@types:id(
            _pipe@2,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        _pipe@4 = mochi@types:string(
            _pipe@3,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(3, U@1) end
        ),
        mochi@types:build(_pipe@4, fun decode_user/1)
    end,
    Test_schema = begin
        _pipe@5 = mochi@query:new(),
        _pipe@6 = mochi@query:add_query(_pipe@5, Users_query),
        _pipe@7 = mochi@query:add_type(_pipe@6, User_type),
        mochi@query:build(_pipe@7)
    end,
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ users { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 334).
-spec build_schema_with_guard(
    fun((mochi@schema:resolver_info()) -> {ok, nil} | {error, binary()})
) -> mochi@schema:schema().
build_schema_with_guard(Guard_fn) ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        _pipe@1 = mochi@schema:id_field(_pipe, <<"id"/utf8>>),
        mochi@schema:required_string_field(_pipe@1, <<"name"/utf8>>)
    end,
    User_field = begin
        _pipe@2 = mochi@schema:field_def(
            <<"user"/utf8>>,
            mochi@schema:named_type(<<"User"/utf8>>)
        ),
        _pipe@3 = mochi@schema:field_description(_pipe@2, <<"Get a user"/utf8>>),
        _pipe@4 = mochi@schema:resolver(
            _pipe@3,
            fun(_) ->
                {ok, user_to_dynamic({user, <<"1"/utf8>>, <<"Alice"/utf8>>})}
            end
        ),
        mochi@schema:guard(_pipe@4, Guard_fn)
    end,
    Query_type = begin
        _pipe@5 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(_pipe@5, User_field)
    end,
    _pipe@6 = mochi@schema:schema(),
    _pipe@7 = mochi@schema:'query'(_pipe@6, Query_type),
    mochi@schema:add_type(_pipe@7, {object_type_def, User_type}).

-file("test/guard_test.gleam", 61).
-spec schema_guard_allows_resolver_test() -> nil.
schema_guard_allows_resolver_test() ->
    Test_schema = build_schema_with_guard(fun low_level_allow/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 76).
-spec schema_guard_blocks_resolver_test() -> nil.
schema_guard_blocks_resolver_test() ->
    Test_schema = build_schema_with_guard(fun low_level_deny/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 357).
-spec build_schema_with_guards(
    list(fun((mochi@schema:resolver_info()) -> {ok, nil} | {error, binary()}))
) -> mochi@schema:schema().
build_schema_with_guards(Guard_fns) ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        _pipe@1 = mochi@schema:id_field(_pipe, <<"id"/utf8>>),
        mochi@schema:required_string_field(_pipe@1, <<"name"/utf8>>)
    end,
    User_field = begin
        _pipe@2 = mochi@schema:field_def(
            <<"user"/utf8>>,
            mochi@schema:named_type(<<"User"/utf8>>)
        ),
        _pipe@3 = mochi@schema:resolver(
            _pipe@2,
            fun(_) ->
                {ok, user_to_dynamic({user, <<"1"/utf8>>, <<"Alice"/utf8>>})}
            end
        ),
        mochi@schema:guards(_pipe@3, Guard_fns)
    end,
    Query_type = begin
        _pipe@4 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(_pipe@4, User_field)
    end,
    _pipe@5 = mochi@schema:schema(),
    _pipe@6 = mochi@schema:'query'(_pipe@5, Query_type),
    mochi@schema:add_type(_pipe@6, {object_type_def, User_type}).

-file("test/guard_test.gleam", 90).
-spec schema_guards_multiple_all_pass_test() -> nil.
schema_guards_multiple_all_pass_test() ->
    Test_schema = build_schema_with_guards(
        [fun low_level_allow/1, fun low_level_allow/1]
    ),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 105).
-spec schema_guards_first_fails_test() -> nil.
schema_guards_first_fails_test() ->
    Test_schema = build_schema_with_guards(
        [fun low_level_deny/1, fun low_level_allow/1]
    ),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 119).
-spec schema_guards_second_fails_test() -> nil.
schema_guards_second_fails_test() ->
    Test_schema = build_schema_with_guards(
        [fun low_level_allow/1, fun low_level_deny/1]
    ),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 133).
-spec schema_guards_order_first_checked_first_test() -> nil.
schema_guards_order_first_checked_first_test() ->
    Guard_a = fun(_) -> {error, <<"Guard A failed"/utf8>>} end,
    Guard_b = fun(_) -> {error, <<"Guard B failed"/utf8>>} end,
    Test_schema = build_schema_with_guards([Guard_a, Guard_b]),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    Has_guard_a_error = begin
        _pipe = erlang:element(3, Result),
        gleam@list:any(
            _pipe,
            fun(Err) -> erlang:element(2, Err) =:= <<"Guard A failed"/utf8>> end
        )
    end,
    gleeunit@should:be_true(Has_guard_a_error).

-file("test/guard_test.gleam", 379).
-spec build_query_with_guard(
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> mochi@schema:schema().
build_query_with_guard(Guard_fn) ->
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
    Users_query = begin
        _pipe@3 = mochi@query:'query'(
            <<"users"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
            fun(_) -> {ok, [{user, <<"1"/utf8>>, <<"Alice"/utf8>>}]} end,
            fun(Users) -> gleam_stdlib:identity(Users) end
        ),
        mochi@query:with_guard(_pipe@3, Guard_fn)
    end,
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/guard_test.gleam", 171).
-spec query_guard_allows_test() -> nil.
query_guard_allows_test() ->
    Test_schema = build_query_with_guard(fun allow_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ users { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 186).
-spec query_guard_blocks_test() -> nil.
query_guard_blocks_test() ->
    Test_schema = build_query_with_guard(fun deny_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ users { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 306).
-spec guard_error_message_preserved_test() -> nil.
guard_error_message_preserved_test() ->
    Custom_guard = fun(_) ->
        {error, <<"You must be an admin to access this resource"/utf8>>}
    end,
    Test_schema = build_query_with_guard(Custom_guard),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ users { id } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    Has_admin_error = begin
        _pipe = erlang:element(3, Result),
        gleam@list:any(
            _pipe,
            fun(Err) ->
                erlang:element(2, Err) =:= <<"You must be an admin to access this resource"/utf8>>
            end
        )
    end,
    gleeunit@should:be_true(Has_admin_error).

-file("test/guard_test.gleam", 403).
-spec build_mutation_with_guard(
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> mochi@schema:schema().
build_mutation_with_guard(Guard_fn) ->
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
    Create_user_mutation = begin
        _pipe@3 = mochi@query:mutation(
            <<"createUser"/utf8>>,
            [mochi@query:arg(
                    <<"name"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                )],
            mochi@schema:named_type(<<"User"/utf8>>),
            fun(Args) -> mochi@query:get_string(Args, <<"name"/utf8>>) end,
            fun(_, _) -> {ok, {user, <<"2"/utf8>>, <<"Bob"/utf8>>}} end,
            fun(User) -> gleam_stdlib:identity(User) end
        ),
        mochi@query:mutation_with_guard(_pipe@3, Guard_fn)
    end,
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_mutation(_pipe@4, Create_user_mutation),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/guard_test.gleam", 239).
-spec mutation_guard_allows_test() -> nil.
mutation_guard_allows_test() ->
    Test_schema = build_mutation_with_guard(fun allow_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"mutation { createUser(name: \"Bob\") { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 254).
-spec mutation_guard_blocks_test() -> nil.
mutation_guard_blocks_test() ->
    Test_schema = build_mutation_with_guard(fun deny_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"mutation { createUser(name: \"Bob\") { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 429).
-spec build_field_with_guard(
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> mochi@schema:schema().
build_field_with_guard(Guard_fn) ->
    Secret_field = begin
        _pipe = mochi@query:field(
            <<"secret"/utf8>>,
            mochi@schema:string_type(),
            fun(_) -> {ok, nil} end,
            fun(_, _) -> {ok, <<"top-secret-value"/utf8>>} end,
            fun(S) -> gleam_stdlib:identity(S) end
        ),
        mochi@query:field_with_guard(_pipe, Guard_fn)
    end,
    User_type = begin
        _pipe@1 = mochi@types:object(<<"User"/utf8>>),
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
        _pipe@4 = mochi@types:build(_pipe@3, fun decode_user/1),
        mochi@schema:field(
            _pipe@4,
            mochi@query:field_def_to_schema(Secret_field)
        )
    end,
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) -> {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>}} end,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@5 = mochi@query:new(),
    _pipe@6 = mochi@query:add_query(_pipe@5, User_query),
    _pipe@7 = mochi@query:add_type(_pipe@6, User_type),
    mochi@query:build(_pipe@7).

-file("test/guard_test.gleam", 272).
-spec field_guard_allows_test() -> nil.
field_guard_allows_test() ->
    Test_schema = build_field_with_guard(fun allow_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name secret } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 287).
-spec field_guard_blocks_test() -> nil.
field_guard_blocks_test() ->
    Test_schema = build_field_with_guard(fun deny_guard/1),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ user { id name secret } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/guard_test.gleam", 468).
-spec subscription_guard_allows_test() -> nil.
subscription_guard_allows_test() ->
    Sub = begin
        _pipe = mochi@query:subscription(
            <<"onMessage"/utf8>>,
            mochi@schema:named_type(<<"User"/utf8>>),
            <<"messages"/utf8>>,
            fun(U) -> gleam_stdlib:identity(U) end
        ),
        mochi@query:subscription_with_guard(_pipe, fun allow_guard/1)
    end,
    Field_def = mochi@query:subscription_to_field_def(Sub),
    gleeunit@should:equal(erlang:element(2, Field_def), <<"onMessage"/utf8>>).

-file("test/guard_test.gleam", 482).
-spec subscription_guard_blocks_test() -> binary().
subscription_guard_blocks_test() ->
    Sub = begin
        _pipe = mochi@query:subscription(
            <<"onMessage"/utf8>>,
            mochi@schema:named_type(<<"User"/utf8>>),
            <<"messages"/utf8>>,
            fun(U) -> gleam_stdlib:identity(U) end
        ),
        mochi@query:subscription_with_guard(_pipe, fun deny_guard/1)
    end,
    Field_def = mochi@query:subscription_to_field_def(Sub),
    case erlang:element(9, Field_def) of
        {some, Topic_fn} ->
            Result = Topic_fn(maps:new(), default_ctx()),
            gleeunit@should:be_error(Result);

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"expected topic_fn to be Some"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"guard_test"/utf8>>,
                    function => <<"subscription_guard_blocks_test"/utf8>>,
                    line => 499})
    end.

-file("test/guard_test.gleam", 571).
-spec high_level_all_of_test() -> nil.
high_level_all_of_test() ->
    Combined = mochi@query:all_of([fun allow_guard/1, fun allow_guard/1]),
    gleeunit@should:be_ok(Combined(default_ctx())).

-file("test/guard_test.gleam", 576).
-spec high_level_all_of_fails_test() -> binary().
high_level_all_of_fails_test() ->
    Combined = mochi@query:all_of([fun allow_guard/1, fun deny_guard/1]),
    gleeunit@should:be_error(Combined(default_ctx())).

-file("test/guard_test.gleam", 581).
-spec high_level_any_of_test() -> nil.
high_level_any_of_test() ->
    Combined = mochi@query:any_of([fun deny_guard/1, fun allow_guard/1]),
    gleeunit@should:be_ok(Combined(default_ctx())).

-file("test/guard_test.gleam", 586).
-spec high_level_any_of_all_fail_test() -> binary().
high_level_any_of_all_fail_test() ->
    Combined = mochi@query:any_of([fun deny_guard/1, fun deny_guard/1]),
    gleeunit@should:be_error(Combined(default_ctx())).

-file("test/guard_test.gleam", 591).
-spec any_of_used_as_guard_test() -> nil.
any_of_used_as_guard_test() ->
    Test_schema = build_query_with_guard(
        mochi@query:any_of([fun deny_guard/1, fun allow_guard/1])
    ),
    Result = mochi@executor:execute_query_with_context(
        Test_schema,
        <<"{ users { id name } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/guard_test.gleam", 607).
-spec make_resolver_info() -> mochi@schema:resolver_info().
make_resolver_info() ->
    {resolver_info,
        none,
        maps:new(),
        default_ctx(),
        gleam_stdlib:identity(maps:new())}.

-file("test/guard_test.gleam", 507).
-spec all_guards_all_pass_test() -> nil.
all_guards_all_pass_test() ->
    Combined = mochi@schema:all_guards(
        [fun low_level_allow/1, fun low_level_allow/1]
    ),
    Info = make_resolver_info(),
    gleeunit@should:be_ok(Combined(Info)).

-file("test/guard_test.gleam", 513).
-spec all_guards_one_fails_test() -> binary().
all_guards_one_fails_test() ->
    Combined = mochi@schema:all_guards(
        [fun low_level_allow/1, fun low_level_deny/1]
    ),
    Info = make_resolver_info(),
    gleeunit@should:be_error(Combined(Info)).

-file("test/guard_test.gleam", 519).
-spec all_guards_first_fails_short_circuits_test() -> nil.
all_guards_first_fails_short_circuits_test() ->
    Guard_a = fun(_) -> {error, <<"A failed"/utf8>>} end,
    Guard_b = fun(_) -> {error, <<"B failed"/utf8>>} end,
    Combined = mochi@schema:all_guards([Guard_a, Guard_b]),
    Info = make_resolver_info(),
    Result = Combined(Info),
    gleeunit@should:equal(Result, {error, <<"A failed"/utf8>>}).

-file("test/guard_test.gleam", 533).
-spec any_guard_one_passes_test() -> nil.
any_guard_one_passes_test() ->
    Combined = mochi@schema:any_guard(
        [fun low_level_deny/1, fun low_level_allow/1]
    ),
    Info = make_resolver_info(),
    gleeunit@should:be_ok(Combined(Info)).

-file("test/guard_test.gleam", 539).
-spec any_guard_all_fail_test() -> binary().
any_guard_all_fail_test() ->
    Combined = mochi@schema:any_guard(
        [fun low_level_deny/1, fun low_level_deny/1]
    ),
    Info = make_resolver_info(),
    gleeunit@should:be_error(Combined(Info)).

-file("test/guard_test.gleam", 545).
-spec any_guard_first_passes_short_circuits_test() -> nil.
any_guard_first_passes_short_circuits_test() ->
    Combined = mochi@schema:any_guard(
        [fun low_level_allow/1, fun low_level_deny/1]
    ),
    Info = make_resolver_info(),
    gleeunit@should:be_ok(Combined(Info)).

-file("test/guard_test.gleam", 551).
-spec any_guard_empty_list_test() -> binary().
any_guard_empty_list_test() ->
    Combined = mochi@schema:any_guard([]),
    Info = make_resolver_info(),
    gleeunit@should:be_error(Combined(Info)).

-file("test/guard_test.gleam", 557).
-spec any_guard_returns_last_error_test() -> nil.
any_guard_returns_last_error_test() ->
    Guard_a = fun(_) -> {error, <<"A failed"/utf8>>} end,
    Guard_b = fun(_) -> {error, <<"B failed"/utf8>>} end,
    Combined = mochi@schema:any_guard([Guard_a, Guard_b]),
    Info = make_resolver_info(),
    gleeunit@should:equal(Combined(Info), {error, <<"B failed"/utf8>>}).
