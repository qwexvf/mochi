-module(middleware_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/middleware_test.gleam").
-export([parse_directive_on_operation_test/0, parse_directive_on_field_test/0, parse_directive_on_fragment_definition_test/0, parse_directive_on_fragment_spread_test/0, parse_directive_on_inline_fragment_test/0, unknown_directive_is_rejected_test/0, deprecated_directive_not_allowed_on_field_test/0, skip_directive_on_field_is_valid_test/0, include_directive_on_fragment_spread_is_valid_test/0, missing_required_variable_causes_error_test/0, missing_optional_variable_is_ok_test/0, provided_variable_of_correct_type_is_ok_test/0, provided_bool_for_string_variable_causes_error_test/0, middleware_pipeline_runs_test/0, middleware_logging_runs_without_error_test/0, middleware_to_executor_fn_wraps_pipeline_test/0, middleware_with_field_filter_test/0, middleware_no_pipeline_executes_normally_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), integer()}.

-file("test/middleware_test.gleam", 26).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>, 30}}.

-file("test/middleware_test.gleam", 30).
-spec build_test_schema() -> mochi@schema:schema().
build_test_schema() ->
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
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        {named, <<"User"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {user, <<"1"/utf8>>, <<"Alice"/utf8>>, 30}
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    User_by_id_query = mochi@query:query_with_args(
        <<"userById"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        {named, <<"User"/utf8>>},
        fun(_) -> {ok, <<"1"/utf8>>} end,
        fun(_, _) -> {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>, 30}} end,
        fun(U@4) -> gleam_stdlib:identity(U@4) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, User_query),
    _pipe@6 = mochi@query:add_query(_pipe@5, User_by_id_query),
    _pipe@7 = mochi@query:add_type(_pipe@6, User_type),
    mochi@query:build(_pipe@7).

-file("test/middleware_test.gleam", 67).
-spec parse_directive_on_operation_test() -> nil.
parse_directive_on_operation_test() ->
    Query_str = <<"query GetUser @deprecated { user { id } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case erlang:element(2, Doc) of
                [{operation_definition, Op}] ->
                    Directives = case Op of
                        {operation, _, _, _, D, _} ->
                            D;

                        {shorthand_query, _} ->
                            []
                    end,
                    gleeunit@should:not_equal(Directives, []);

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected one operation"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"parse_directive_on_operation_test"/utf8>>,
                            line => 80})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"parse_directive_on_operation_test"/utf8>>,
                    line => 83})
    end.

-file("test/middleware_test.gleam", 87).
-spec parse_directive_on_field_test() -> nil.
parse_directive_on_field_test() ->
    Query_str = <<"{ user { id name @skip(if: true) } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            gleeunit@should:equal(erlang:length(erlang:element(2, Doc)), 1);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"parse_directive_on_field_test"/utf8>>,
                    line => 91})
    end.

-file("test/middleware_test.gleam", 95).
-spec parse_directive_on_fragment_definition_test() -> nil.
parse_directive_on_fragment_definition_test() ->
    Query_str = <<"fragment UserFields on User @deprecated { id name } query { user { ...UserFields } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Frag = gleam@list:find(erlang:element(2, Doc), fun(D) -> case D of
                        {fragment_definition, _} ->
                            true;

                        _ ->
                            false
                    end end),
            case Frag of
                {ok, {fragment_definition, F}} ->
                    gleeunit@should:not_equal(erlang:element(4, F), []);

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected fragment with directive"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"parse_directive_on_fragment_definition_test"/utf8>>,
                            line => 109})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"parse_directive_on_fragment_definition_test"/utf8>>,
                    line => 112})
    end.

-file("test/middleware_test.gleam", 116).
-spec parse_directive_on_fragment_spread_test() -> nil.
parse_directive_on_fragment_spread_test() ->
    Query_str = <<"fragment UserFields on User { id } query { user { ...UserFields @skip(if: false) } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Op = gleam@list:find(erlang:element(2, Doc), fun(D) -> case D of
                        {operation_definition, _} ->
                            true;

                        _ ->
                            false
                    end end),
            case Op of
                {ok, {operation_definition, {operation, _, _, _, _, Ss}}} ->
                    case erlang:element(2, Ss) of
                        [{field_selection, User_field}] ->
                            case erlang:element(6, User_field) of
                                {some, Inner_ss} ->
                                    case erlang:element(2, Inner_ss) of
                                        [{fragment_spread, Spread}] ->
                                            gleeunit@should:not_equal(
                                                erlang:element(3, Spread),
                                                []
                                            );

                                        _ ->
                                            erlang:error(#{gleam_error => panic,
                                                    message => <<"Expected fragment spread"/utf8>>,
                                                    file => <<?FILEPATH/utf8>>,
                                                    module => <<"middleware_test"/utf8>>,
                                                    function => <<"parse_directive_on_fragment_spread_test"/utf8>>,
                                                    line => 137})
                                    end;

                                none ->
                                    erlang:error(#{gleam_error => panic,
                                            message => <<"Expected selection set on user field"/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"middleware_test"/utf8>>,
                                            function => <<"parse_directive_on_fragment_spread_test"/utf8>>,
                                            line => 140})
                            end;

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Expected user field"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"middleware_test"/utf8>>,
                                    function => <<"parse_directive_on_fragment_spread_test"/utf8>>,
                                    line => 143})
                    end;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected named query operation"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"parse_directive_on_fragment_spread_test"/utf8>>,
                            line => 146})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"parse_directive_on_fragment_spread_test"/utf8>>,
                    line => 149})
    end.

-file("test/middleware_test.gleam", 153).
-spec parse_directive_on_inline_fragment_test() -> nil.
parse_directive_on_inline_fragment_test() ->
    Query_str = <<"{ user { ... on User @skip(if: false) { id } } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            gleeunit@should:equal(erlang:length(erlang:element(2, Doc)), 1);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"parse_directive_on_inline_fragment_test"/utf8>>,
                    line => 157})
    end.

-file("test/middleware_test.gleam", 165).
-spec unknown_directive_is_rejected_test() -> nil.
unknown_directive_is_rejected_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user { id name @nonexistent } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have failed with UnknownDirective"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"unknown_directive_is_rejected_test"/utf8>>,
                            line => 171});

                {error, Errors} ->
                    Has_unknown = gleam@list:any(Errors, fun(E) -> case E of
                                {unknown_directive, <<"nonexistent"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end),
                    gleeunit@should:be_true(Has_unknown)
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"unknown_directive_is_rejected_test"/utf8>>,
                    line => 183})
    end.

-file("test/middleware_test.gleam", 187).
-spec deprecated_directive_not_allowed_on_field_test() -> nil.
deprecated_directive_not_allowed_on_field_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user { id name @deprecated } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have failed with DirectiveNotAllowed"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"deprecated_directive_not_allowed_on_field_test"/utf8>>,
                            line => 195});

                {error, Errors} ->
                    Has_not_allowed = gleam@list:any(Errors, fun(E) -> case E of
                                {directive_not_allowed,
                                    <<"deprecated"/utf8>>,
                                    _} ->
                                    true;

                                _ ->
                                    false
                            end end),
                    gleeunit@should:be_true(Has_not_allowed)
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"deprecated_directive_not_allowed_on_field_test"/utf8>>,
                    line => 207})
    end.

-file("test/middleware_test.gleam", 211).
-spec skip_directive_on_field_is_valid_test() -> nil.
skip_directive_on_field_is_valid_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user { id name @skip(if: true) } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Should be valid but got errors: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"skip_directive_on_field_is_valid_test"/utf8>>,
                            line => 220})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"skip_directive_on_field_is_valid_test"/utf8>>,
                    line => 223})
    end.

-file("test/middleware_test.gleam", 227).
-spec include_directive_on_fragment_spread_is_valid_test() -> nil.
include_directive_on_fragment_spread_is_valid_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"fragment UserFields on User { id } query { user { ...UserFields @include(if: true) } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Should be valid but got errors: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"middleware_test"/utf8>>,
                            function => <<"include_directive_on_fragment_spread_is_valid_test"/utf8>>,
                            line => 237})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"include_directive_on_fragment_spread_is_valid_test"/utf8>>,
                    line => 240})
    end.

-file("test/middleware_test.gleam", 248).
-spec missing_required_variable_causes_error_test() -> nil.
missing_required_variable_causes_error_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"query GetUser($id: ID!) { userById(id: $id) { name } }"/utf8>>,
    Result = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        maps:new()
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/middleware_test.gleam", 257).
-spec missing_optional_variable_is_ok_test() -> nil.
missing_optional_variable_is_ok_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"query GetUser($name: String) { user { id } }"/utf8>>,
    Result = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        maps:new()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/middleware_test.gleam", 266).
-spec provided_variable_of_correct_type_is_ok_test() -> nil.
provided_variable_of_correct_type_is_ok_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"query GetUser($id: ID!) { userById(id: $id) { name } }"/utf8>>,
    Vars = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)}]
    ),
    Result = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        Vars
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/middleware_test.gleam", 275).
-spec provided_bool_for_string_variable_causes_error_test() -> nil.
provided_bool_for_string_variable_causes_error_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"query GetUser($name: String!) { user { id } }"/utf8>>,
    Vars = maps:from_list([{<<"name"/utf8>>, gleam_stdlib:identity(true)}]),
    Result = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        Vars
    ),
    gleeunit@should:not_equal(erlang:element(3, Result), []).

-file("test/middleware_test.gleam", 290).
-spec middleware_pipeline_runs_test() -> nil.
middleware_pipeline_runs_test() ->
    Test_schema = build_test_schema(),
    Pipeline = begin
        _pipe = mochi@middleware:new_pipeline(),
        mochi@middleware:add_middleware(
            _pipe,
            mochi@middleware:transform_middleware(fun(Value) -> Value end)
        )
    end,
    Ctx = begin
        _pipe@1 = mochi@schema:execution_context(
            gleam_stdlib:identity(maps:new())
        ),
        mochi@schema:with_middleware(
            _pipe@1,
            mochi@middleware:to_executor_fn(Pipeline)
        )
    end,
    Query_str = <<"{ user { name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Result = mochi@executor:execute(
                Test_schema,
                Doc,
                none,
                Ctx,
                maps:new()
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"middleware_pipeline_runs_test"/utf8>>,
                    line => 314})
    end.

-file("test/middleware_test.gleam", 318).
-spec middleware_logging_runs_without_error_test() -> nil.
middleware_logging_runs_without_error_test() ->
    Test_schema = build_test_schema(),
    Pipeline = begin
        _pipe = mochi@middleware:new_pipeline(),
        mochi@middleware:add_middleware(
            _pipe,
            mochi@middleware:logging_middleware(fun(_) -> nil end)
        )
    end,
    Ctx = begin
        _pipe@1 = mochi@schema:execution_context(
            gleam_stdlib:identity(maps:new())
        ),
        mochi@schema:with_middleware(
            _pipe@1,
            mochi@middleware:to_executor_fn(Pipeline)
        )
    end,
    Query_str = <<"{ user { id name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Result = mochi@executor:execute(
                Test_schema,
                Doc,
                none,
                Ctx,
                maps:new()
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"middleware_logging_runs_without_error_test"/utf8>>,
                    line => 339})
    end.

-file("test/middleware_test.gleam", 343).
-spec middleware_to_executor_fn_wraps_pipeline_test() -> nil.
middleware_to_executor_fn_wraps_pipeline_test() ->
    Pipeline = mochi@middleware:new_pipeline(),
    Mw_fn = mochi@middleware:to_executor_fn(Pipeline),
    Field_def = mochi@schema:field_def(
        <<"name"/utf8>>,
        {named, <<"String"/utf8>>}
    ),
    Info = {resolver_info,
        none,
        maps:new(),
        mochi@schema:execution_context(gleam_stdlib:identity(nil)),
        gleam_stdlib:identity(nil)},
    Result = Mw_fn(
        <<"Query"/utf8>>,
        Field_def,
        Info,
        fun(_) -> {ok, gleam_stdlib:identity(<<"hello"/utf8>>)} end
    ),
    case Result of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Middleware fn failed: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"middleware_to_executor_fn_wraps_pipeline_test"/utf8>>,
                    line => 363})
    end.

-file("test/middleware_test.gleam", 367).
-spec middleware_with_field_filter_test() -> nil.
middleware_with_field_filter_test() ->
    Test_schema = build_test_schema(),
    Mw = begin
        _pipe = mochi@middleware:middleware(
            <<"passthrough"/utf8>>,
            fun(Resolution, Next) -> Next(Resolution) end
        ),
        mochi@middleware:with_filter(_pipe, {named_fields, [<<"name"/utf8>>]})
    end,
    Pipeline = begin
        _pipe@1 = mochi@middleware:new_pipeline(),
        mochi@middleware:add_middleware(_pipe@1, Mw)
    end,
    Ctx = begin
        _pipe@2 = mochi@schema:execution_context(
            gleam_stdlib:identity(maps:new())
        ),
        mochi@schema:with_middleware(
            _pipe@2,
            mochi@middleware:to_executor_fn(Pipeline)
        )
    end,
    Query_str = <<"{ user { id name age } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Result = mochi@executor:execute(
                Test_schema,
                Doc,
                none,
                Ctx,
                maps:new()
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"middleware_with_field_filter_test"/utf8>>,
                    line => 392})
    end.

-file("test/middleware_test.gleam", 396).
-spec middleware_no_pipeline_executes_normally_test() -> nil.
middleware_no_pipeline_executes_normally_test() ->
    Test_schema = build_test_schema(),
    Ctx = mochi@schema:execution_context(gleam_stdlib:identity(maps:new())),
    Query_str = <<"{ user { id name age } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            Result = mochi@executor:execute(
                Test_schema,
                Doc,
                none,
                Ctx,
                maps:new()
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"middleware_test"/utf8>>,
                    function => <<"middleware_no_pipeline_executes_normally_test"/utf8>>,
                    line => 409})
    end.
