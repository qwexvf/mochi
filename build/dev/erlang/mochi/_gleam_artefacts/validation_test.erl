-module(validation_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/validation_test.gleam").
-export([valid_simple_query_test/0, valid_query_with_args_test/0, valid_introspection_query_test/0, valid_typename_query_test/0, invalid_unknown_field_test/0, invalid_missing_required_arg_test/0, invalid_unknown_argument_test/0, invalid_selection_required_test/0, invalid_selection_not_allowed_test/0, invalid_undefined_fragment_test/0, valid_query_with_fragment_test/0, invalid_duplicate_operation_names_test/0, invalid_anonymous_not_alone_test/0, error_formatting_test/0, multiple_error_formatting_test/0, invalid_duplicate_argument_test/0, invalid_duplicate_variable_test/0, invalid_unused_fragment_test/0, valid_skip_directive_test/0, valid_include_directive_test/0, invalid_unknown_directive_test/0, valid_inline_fragment_test/0, valid_multiple_directives_test/0, validate_query_valid_test/0, validate_query_invalid_syntax_test/0, validate_query_unknown_field_test/0, validate_query_garbage_input_test/0, invalid_subscription_multiple_root_fields_test/0, subscription_fragment_spread_parses_test/0, variable_object_type_is_rejected_test/0, variable_scalar_type_is_accepted_test/0, variable_string_type_is_accepted_test/0, fragment_on_scalar_is_rejected_test/0, fragment_on_object_type_is_accepted_test/0, skip_without_if_is_rejected_test/0, include_without_if_is_rejected_test/0, skip_with_boolean_if_is_accepted_test/0, include_with_variable_if_tracks_variable_test/0, skip_unknown_argument_is_rejected_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), binary(), integer()}.

-file("test/validation_test.gleam", 20).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Test"/utf8>>, <<"test@example.com"/utf8>>, 25}}.

-file("test/validation_test.gleam", 24).
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
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"email"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        _pipe@4 = mochi@types:int(
            _pipe@3,
            <<"age"/utf8>>,
            fun(U@3) -> erlang:element(5, U@3) end
        ),
        mochi@types:build(_pipe@4, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, []} end,
        fun(_) -> gleam_stdlib:identity([]) end
    ),
    User_query = mochi@query:query_with_args(
        <<"user"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) -> {ok, <<"1"/utf8>>} end,
        fun(_, _) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Test"/utf8>>,
                    <<"test@example.com"/utf8>>,
                    25}}
        end,
        fun(U@4) -> gleam_stdlib:identity(U@4) end
    ),
    _pipe@5 = mochi@query:new(),
    _pipe@6 = mochi@query:add_query(_pipe@5, Users_query),
    _pipe@7 = mochi@query:add_query(_pipe@6, User_query),
    _pipe@8 = mochi@query:add_type(_pipe@7, User_type),
    mochi@query:build(_pipe@8).

-file("test/validation_test.gleam", 62).
-spec valid_simple_query_test() -> nil.
valid_simple_query_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_simple_query_test"/utf8>>,
                            line => 73})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_simple_query_test"/utf8>>,
                    line => 77})
    end.

-file("test/validation_test.gleam", 81).
-spec valid_query_with_args_test() -> nil.
valid_query_with_args_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user(id: \"1\") { id name email } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_query_with_args_test"/utf8>>,
                            line => 92})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_query_with_args_test"/utf8>>,
                    line => 96})
    end.

-file("test/validation_test.gleam", 100).
-spec valid_introspection_query_test() -> nil.
valid_introspection_query_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ __schema { types { name } } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_introspection_query_test"/utf8>>,
                            line => 111})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_introspection_query_test"/utf8>>,
                    line => 115})
    end.

-file("test/validation_test.gleam", 119).
-spec valid_typename_query_test() -> nil.
valid_typename_query_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { __typename id } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_typename_query_test"/utf8>>,
                            line => 130})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_typename_query_test"/utf8>>,
                    line => 134})
    end.

-file("test/validation_test.gleam", 142).
-spec invalid_unknown_field_test() -> nil.
invalid_unknown_field_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id unknownField } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for unknown field"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_unknown_field_test"/utf8>>,
                            line => 150});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {unknown_field,
                                    <<"unknownField"/utf8>>,
                                    <<"User"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have UnknownField error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_unknown_field_test"/utf8>>,
                                    line => 161})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_unknown_field_test"/utf8>>,
                    line => 166})
    end.

-file("test/validation_test.gleam", 174).
-spec invalid_missing_required_arg_test() -> nil.
invalid_missing_required_arg_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user { id name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for missing required arg"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_missing_required_arg_test"/utf8>>,
                            line => 183});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {missing_required_argument,
                                    <<"user"/utf8>>,
                                    <<"id"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have MissingRequiredArgument error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_missing_required_arg_test"/utf8>>,
                                    line => 194})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_missing_required_arg_test"/utf8>>,
                    line => 199})
    end.

-file("test/validation_test.gleam", 207).
-spec invalid_unknown_argument_test() -> nil.
invalid_unknown_argument_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id name(foo: \"bar\") } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for unknown argument"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_unknown_argument_test"/utf8>>,
                            line => 215});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {unknown_argument,
                                    <<"name"/utf8>>,
                                    <<"foo"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have UnknownArgument error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_unknown_argument_test"/utf8>>,
                                    line => 226})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_unknown_argument_test"/utf8>>,
                    line => 231})
    end.

-file("test/validation_test.gleam", 239).
-spec invalid_selection_required_test() -> nil.
invalid_selection_required_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for missing selection set"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_selection_required_test"/utf8>>,
                            line => 248});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {selection_set_required, <<"users"/utf8>>, _} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have SelectionSetRequired error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_selection_required_test"/utf8>>,
                                    line => 259})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_selection_required_test"/utf8>>,
                    line => 264})
    end.

-file("test/validation_test.gleam", 272).
-spec invalid_selection_not_allowed_test() -> nil.
invalid_selection_not_allowed_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { name { foo } } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for selection set on scalar"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_selection_not_allowed_test"/utf8>>,
                            line => 281});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {selection_set_not_allowed, <<"name"/utf8>>, _} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have SelectionSetNotAllowed error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_selection_not_allowed_test"/utf8>>,
                                    line => 292})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_selection_not_allowed_test"/utf8>>,
                    line => 297})
    end.

-file("test/validation_test.gleam", 305).
-spec invalid_undefined_fragment_test() -> nil.
invalid_undefined_fragment_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { ...UserFields } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for undefined fragment"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_undefined_fragment_test"/utf8>>,
                            line => 313});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {undefined_fragment, <<"UserFields"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have UndefinedFragment error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_undefined_fragment_test"/utf8>>,
                                    line => 324})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_undefined_fragment_test"/utf8>>,
                    line => 329})
    end.

-file("test/validation_test.gleam", 337).
-spec valid_query_with_fragment_test() -> nil.
valid_query_with_fragment_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"
    { users { ...UserFields } }
    fragment UserFields on User { id name }
  "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_query_with_fragment_test"/utf8>>,
                            line => 352})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_query_with_fragment_test"/utf8>>,
                    line => 356})
    end.

-file("test/validation_test.gleam", 364).
-spec invalid_duplicate_operation_names_test() -> nil.
invalid_duplicate_operation_names_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"
    query GetUsers { users { id } }
    query GetUsers { users { name } }
  "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for duplicate operation names"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_duplicate_operation_names_test"/utf8>>,
                            line => 376});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {duplicate_operation_name, <<"GetUsers"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have DuplicateOperationName error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_duplicate_operation_names_test"/utf8>>,
                                    line => 387})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_duplicate_operation_names_test"/utf8>>,
                    line => 392})
    end.

-file("test/validation_test.gleam", 400).
-spec invalid_anonymous_not_alone_test() -> nil.
invalid_anonymous_not_alone_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"
    { users { id } }
    query Named { users { name } }
  "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for anonymous operation not alone"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_anonymous_not_alone_test"/utf8>>,
                            line => 413});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                anonymous_operation_not_alone ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have AnonymousOperationNotAlone error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_anonymous_not_alone_test"/utf8>>,
                                    line => 424})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_anonymous_not_alone_test"/utf8>>,
                    line => 429})
    end.

-file("test/validation_test.gleam", 437).
-spec error_formatting_test() -> nil.
error_formatting_test() ->
    Error = {unknown_field, <<"badField"/utf8>>, <<"User"/utf8>>},
    Formatted = mochi@validation:format_error(Error),
    case Formatted =:= <<"Cannot query field \"badField\" on type \"User\""/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Error formatting incorrect"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"error_formatting_test"/utf8>>,
                    line => 444})
    end.

-file("test/validation_test.gleam", 448).
-spec multiple_error_formatting_test() -> nil.
multiple_error_formatting_test() ->
    Errors = [{unknown_field, <<"field1"/utf8>>, <<"Type1"/utf8>>},
        {unknown_field, <<"field2"/utf8>>, <<"Type2"/utf8>>}],
    Formatted = mochi@validation:format_errors(Errors),
    case Formatted =:= <<"Cannot query field \"field1\" on type \"Type1\"\nCannot query field \"field2\" on type \"Type2\""/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Multiple error formatting incorrect"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"multiple_error_formatting_test"/utf8>>,
                    line => 460})
    end.

-file("test/validation_test.gleam", 468).
-spec invalid_duplicate_argument_test() -> nil.
invalid_duplicate_argument_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ user(id: \"1\", id: \"2\") { name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for duplicate argument"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_duplicate_argument_test"/utf8>>,
                            line => 477});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {duplicate_argument,
                                    <<"user"/utf8>>,
                                    <<"id"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have DuplicateArgument error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_duplicate_argument_test"/utf8>>,
                                    line => 488})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_duplicate_argument_test"/utf8>>,
                    line => 493})
    end.

-file("test/validation_test.gleam", 501).
-spec invalid_duplicate_variable_test() -> nil.
invalid_duplicate_variable_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"query Test($id: ID!, $id: ID!) { user(id: $id) { name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for duplicate variable"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_duplicate_variable_test"/utf8>>,
                            line => 510});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {duplicate_variable, <<"id"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have DuplicateVariable error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_duplicate_variable_test"/utf8>>,
                                    line => 521})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_duplicate_variable_test"/utf8>>,
                    line => 526})
    end.

-file("test/validation_test.gleam", 534).
-spec invalid_unused_fragment_test() -> nil.
invalid_unused_fragment_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"
    { users { id name } }
    fragment UnusedFields on User { email age }
  "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for unused fragment"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_unused_fragment_test"/utf8>>,
                            line => 547});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {unused_fragment, <<"UnusedFields"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have UnusedFragment error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_unused_fragment_test"/utf8>>,
                                    line => 558})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_unused_fragment_test"/utf8>>,
                    line => 563})
    end.

-file("test/validation_test.gleam", 571).
-spec valid_skip_directive_test() -> nil.
valid_skip_directive_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id name @skip(if: true) } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_skip_directive_test"/utf8>>,
                            line => 582})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_skip_directive_test"/utf8>>,
                    line => 586})
    end.

-file("test/validation_test.gleam", 594).
-spec valid_include_directive_test() -> nil.
valid_include_directive_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id @include(if: true) name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_include_directive_test"/utf8>>,
                            line => 605})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_include_directive_test"/utf8>>,
                    line => 609})
    end.

-file("test/validation_test.gleam", 617).
-spec invalid_unknown_directive_test() -> nil.
invalid_unknown_directive_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id @custom name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Validation should fail for unknown directive"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_unknown_directive_test"/utf8>>,
                            line => 626});

                {error, Errors} ->
                    case gleam@list:find(Errors, fun(E) -> case E of
                                {unknown_directive, <<"custom"/utf8>>} ->
                                    true;

                                _ ->
                                    false
                            end end) of
                        {ok, _} ->
                            nil;

                        {error, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Should have UnknownDirective error"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_unknown_directive_test"/utf8>>,
                                    line => 637})
                    end
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_unknown_directive_test"/utf8>>,
                    line => 642})
    end.

-file("test/validation_test.gleam", 650).
-spec valid_inline_fragment_test() -> nil.
valid_inline_fragment_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { ... on User { id name } } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_inline_fragment_test"/utf8>>,
                            line => 661})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_inline_fragment_test"/utf8>>,
                    line => 665})
    end.

-file("test/validation_test.gleam", 673).
-spec valid_multiple_directives_test() -> nil.
valid_multiple_directives_test() ->
    Test_schema = build_test_schema(),
    Query_str = <<"{ users { id @skip(if: false) @include(if: true) name } }"/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Test_schema) of
                {ok, _} ->
                    nil;

                {error, Errors} ->
                    Msg = mochi@validation:format_errors(Errors),
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Validation failed unexpectedly: "/utf8,
                                Msg/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"valid_multiple_directives_test"/utf8>>,
                            line => 685})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"valid_multiple_directives_test"/utf8>>,
                    line => 689})
    end.

-file("test/validation_test.gleam", 697).
-spec validate_query_valid_test() -> nil.
validate_query_valid_test() ->
    Test_schema = build_test_schema(),
    Result = mochi@validation:validate_query(
        <<"{ users { id name } }"/utf8>>,
        Test_schema
    ),
    case Result of
        {ok, _} ->
            nil;

        {error, Errors} ->
            Msg = mochi@validation:format_errors(Errors),
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected valid query, got errors: "/utf8,
                        Msg/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"validate_query_valid_test"/utf8>>,
                    line => 704})
    end.

-file("test/validation_test.gleam", 709).
-spec validate_query_invalid_syntax_test() -> nil.
validate_query_invalid_syntax_test() ->
    Test_schema = build_test_schema(),
    Result = mochi@validation:validate_query(
        <<"{ users { id "/utf8>>,
        Test_schema
    ),
    case Result of
        {error, Errors} ->
            case Errors /= [] of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected non-empty error list for invalid syntax"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"validate_query_invalid_syntax_test"/utf8>>,
                            line => 717})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected error for syntactically invalid query"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"validate_query_invalid_syntax_test"/utf8>>,
                    line => 719})
    end.

-file("test/validation_test.gleam", 723).
-spec validate_query_unknown_field_test() -> nil.
validate_query_unknown_field_test() ->
    Test_schema = build_test_schema(),
    Result = mochi@validation:validate_query(
        <<"{ users { nonexistentField } }"/utf8>>,
        Test_schema
    ),
    case Result of
        {error, Errors} ->
            case Errors /= [] of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected errors for unknown field"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"validate_query_unknown_field_test"/utf8>>,
                            line => 731})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected error for unknown field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"validate_query_unknown_field_test"/utf8>>,
                    line => 733})
    end.

-file("test/validation_test.gleam", 737).
-spec validate_query_garbage_input_test() -> nil.
validate_query_garbage_input_test() ->
    Test_schema = build_test_schema(),
    Result = mochi@validation:validate_query(<<"!@#$%^&*()"/utf8>>, Test_schema),
    case Result of
        {error, Errors} ->
            case Errors /= [] of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected non-empty error list for garbage input"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"validate_query_garbage_input_test"/utf8>>,
                            line => 745})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected error for unparseable query string"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"validate_query_garbage_input_test"/utf8>>,
                    line => 747})
    end.

-file("test/validation_test.gleam", 755).
-spec invalid_subscription_multiple_root_fields_test() -> nil.
invalid_subscription_multiple_root_fields_test() ->
    Sub_schema = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_subscription(
            _pipe,
            mochi@query:subscription(
                <<"onUser"/utf8>>,
                {named, <<"String"/utf8>>},
                <<"onUser"/utf8>>,
                fun gleam_stdlib:identity/1
            )
        ),
        _pipe@2 = mochi@query:add_subscription(
            _pipe@1,
            mochi@query:subscription(
                <<"onPost"/utf8>>,
                {named, <<"String"/utf8>>},
                <<"onPost"/utf8>>,
                fun gleam_stdlib:identity/1
            )
        ),
        mochi@query:build(_pipe@2)
    end,
    case mochi@parser:parse(<<"subscription { onUser onPost }"/utf8>>) of
        {ok, Doc} ->
            case mochi@validation:validate(Doc, Sub_schema) of
                {error, Errors} ->
                    case Errors /= [] of
                        true ->
                            nil;

                        false ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Expected error for subscription with multiple root fields"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"validation_test"/utf8>>,
                                    function => <<"invalid_subscription_multiple_root_fields_test"/utf8>>,
                                    line => 779})
                    end;

                {ok, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected validation to fail for subscription with multiple root fields"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"invalid_subscription_multiple_root_fields_test"/utf8>>,
                            line => 782})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"invalid_subscription_multiple_root_fields_test"/utf8>>,
                    line => 785})
    end.

-file("test/validation_test.gleam", 792).
-spec subscription_fragment_spread_parses_test() -> nil.
subscription_fragment_spread_parses_test() ->
    Sub_schema = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_subscription(
            _pipe,
            mochi@query:subscription(
                <<"onUserCreated"/utf8>>,
                {named, <<"String"/utf8>>},
                <<"onUserCreated"/utf8>>,
                fun gleam_stdlib:identity/1
            )
        ),
        _pipe@2 = mochi@query:add_subscription(
            _pipe@1,
            mochi@query:subscription(
                <<"onUserUpdated"/utf8>>,
                {named, <<"String"/utf8>>},
                <<"onUserUpdated"/utf8>>,
                fun gleam_stdlib:identity/1
            )
        ),
        mochi@query:build(_pipe@2)
    end,
    Query_str = <<"
    subscription {
      ...SubFields
    }

    fragment SubFields on Subscription {
      onUserCreated
      onUserUpdated
    }
    "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Document} ->
            case mochi@validation:validate(Document, Sub_schema) of
                {error, Errors} ->
                    Has_multiple_fields_error = gleam@list:any(
                        Errors,
                        fun(Err) -> case Err of
                                {subscription_multiple_root_fields, _} ->
                                    true;

                                _ ->
                                    false
                            end end
                    ),
                    case Has_multiple_fields_error of
                        true ->
                            nil;

                        false ->
                            nil
                    end;

                {ok, _} ->
                    nil
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"subscription_fragment_spread_parses_test"/utf8>>,
                    line => 850})
    end.

-file("test/validation_test.gleam", 858).
-spec variable_object_type_is_rejected_test() -> nil.
variable_object_type_is_rejected_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"query ($u: User) { users { id } }"/utf8>>,
        S
    ) of
        {error, Errors} ->
            Has_error = gleam@list:any(Errors, fun(E) -> case E of
                        {variable_not_input_type, <<"u"/utf8>>, <<"User"/utf8>>} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_error of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Expected VariableNotInputType but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"variable_object_type_is_rejected_test"/utf8>>,
                            line => 872})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for object-type variable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"variable_object_type_is_rejected_test"/utf8>>,
                    line => 878})
    end.

-file("test/validation_test.gleam", 882).
-spec variable_scalar_type_is_accepted_test() -> nil.
variable_scalar_type_is_accepted_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"query ($id: ID!) { user(id: $id) { name } }"/utf8>>,
        S
    ) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected valid query but got: "/utf8,
                        (mochi@validation:format_errors(Errors))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"variable_scalar_type_is_accepted_test"/utf8>>,
                    line => 889})
    end.

-file("test/validation_test.gleam", 895).
-spec variable_string_type_is_accepted_test() -> nil.
variable_string_type_is_accepted_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"query ($n: String) { users { id } }"/utf8>>,
        S
    ) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            Has_input_type_error = gleam@list:any(Errors, fun(E) -> case E of
                        {variable_not_input_type, _, _} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_input_type_error of
                true ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"String should be accepted as input type but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"variable_string_type_is_accepted_test"/utf8>>,
                            line => 909});

                false ->
                    nil
            end
    end.

-file("test/validation_test.gleam", 923).
-spec fragment_on_scalar_is_rejected_test() -> nil.
fragment_on_scalar_is_rejected_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"{ users { id } } fragment F on String { id }"/utf8>>,
        S
    ) of
        {error, Errors} ->
            Has_error = gleam@list:any(Errors, fun(E) -> case E of
                        {fragment_on_non_composite_type,
                            <<"F"/utf8>>,
                            <<"String"/utf8>>} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_error of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Expected FragmentOnNonCompositeType but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"fragment_on_scalar_is_rejected_test"/utf8>>,
                            line => 939})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for fragment on scalar"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"fragment_on_scalar_is_rejected_test"/utf8>>,
                    line => 945})
    end.

-file("test/validation_test.gleam", 949).
-spec fragment_on_object_type_is_accepted_test() -> nil.
fragment_on_object_type_is_accepted_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"{ users { ...F } } fragment F on User { id name }"/utf8>>,
        S
    ) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            Has_composite_error = gleam@list:any(Errors, fun(E) -> case E of
                        {fragment_on_non_composite_type, _, _} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_composite_error of
                true ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"User should be accepted as composite type but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"fragment_on_object_type_is_accepted_test"/utf8>>,
                            line => 968});

                false ->
                    nil
            end
    end.

-file("test/validation_test.gleam", 982).
-spec skip_without_if_is_rejected_test() -> nil.
skip_without_if_is_rejected_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(<<"{ users { id @skip } }"/utf8>>, S) of
        {error, Errors} ->
            Has_error = gleam@list:any(Errors, fun(E) -> case E of
                        {missing_required_argument,
                            <<"@skip"/utf8>>,
                            <<"if"/utf8>>} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_error of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Expected MissingRequiredArgument for @skip but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"skip_without_if_is_rejected_test"/utf8>>,
                            line => 996})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for @skip without if"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"skip_without_if_is_rejected_test"/utf8>>,
                    line => 1002})
    end.

-file("test/validation_test.gleam", 1006).
-spec include_without_if_is_rejected_test() -> nil.
include_without_if_is_rejected_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"{ users { id @include } }"/utf8>>,
        S
    ) of
        {error, Errors} ->
            Has_error = gleam@list:any(Errors, fun(E) -> case E of
                        {missing_required_argument,
                            <<"@include"/utf8>>,
                            <<"if"/utf8>>} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_error of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Expected MissingRequiredArgument for @include but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"include_without_if_is_rejected_test"/utf8>>,
                            line => 1020})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for @include without if"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"include_without_if_is_rejected_test"/utf8>>,
                    line => 1026})
    end.

-file("test/validation_test.gleam", 1030).
-spec skip_with_boolean_if_is_accepted_test() -> nil.
skip_with_boolean_if_is_accepted_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"{ users { id @skip(if: true) } }"/utf8>>,
        S
    ) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected valid query but got: "/utf8,
                        (mochi@validation:format_errors(Errors))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"skip_with_boolean_if_is_accepted_test"/utf8>>,
                    line => 1035})
    end.

-file("test/validation_test.gleam", 1041).
-spec include_with_variable_if_tracks_variable_test() -> nil.
include_with_variable_if_tracks_variable_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"query ($show: Boolean!) { users { id @include(if: $show) } }"/utf8>>,
        S
    ) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected valid query but got: "/utf8,
                        (mochi@validation:format_errors(Errors))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"include_with_variable_if_tracks_variable_test"/utf8>>,
                    line => 1051})
    end.

-file("test/validation_test.gleam", 1057).
-spec skip_unknown_argument_is_rejected_test() -> nil.
skip_unknown_argument_is_rejected_test() ->
    S = build_test_schema(),
    case mochi@validation:validate_query(
        <<"{ users { id @skip(if: true, extra: 1) } }"/utf8>>,
        S
    ) of
        {error, Errors} ->
            Has_error = gleam@list:any(Errors, fun(E) -> case E of
                        {unknown_argument, <<"@skip"/utf8>>, <<"extra"/utf8>>} ->
                            true;

                        _ ->
                            false
                    end end),
            case Has_error of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<"Expected UnknownArgument for extra arg on @skip but got: "/utf8,
                                (mochi@validation:format_errors(Errors))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"validation_test"/utf8>>,
                            function => <<"skip_unknown_argument_is_rejected_test"/utf8>>,
                            line => 1073})
            end;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for unknown @skip argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"validation_test"/utf8>>,
                    function => <<"skip_unknown_argument_is_rejected_test"/utf8>>,
                    line => 1079})
    end.
