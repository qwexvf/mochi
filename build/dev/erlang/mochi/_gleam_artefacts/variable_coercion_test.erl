-module(variable_coercion_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/variable_coercion_test.gleam").
-export([explicit_null_for_nullable_string_variable_coerces_test/0, explicit_null_for_nullable_id_variable_coerces_test/0, explicit_null_for_nullable_int_variable_coerces_test/0, explicit_null_for_nullable_float_variable_coerces_test/0, explicit_null_for_nullable_boolean_variable_coerces_test/0, explicit_null_for_non_null_variable_errors_test/0, non_null_string_value_for_nullable_string_variable_ok_test/0, number_value_for_nullable_string_variable_errors_test/0, missing_nullable_variable_coerces_to_null_test/0, missing_non_null_variable_errors_test/0]).

-file("test/variable_coercion_test.gleam", 21).
-spec build_schema() -> mochi@schema:schema().
build_schema() ->
    Echo_string = mochi@query:query_with_args(
        <<"echoString"/utf8>>,
        [mochi@query:arg(<<"x"/utf8>>, mochi@schema:string_type())],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Echo_id = mochi@query:query_with_args(
        <<"echoId"/utf8>>,
        [mochi@query:arg(<<"x"/utf8>>, mochi@schema:id_type())],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Echo_int = mochi@query:query_with_args(
        <<"echoInt"/utf8>>,
        [mochi@query:arg(<<"x"/utf8>>, mochi@schema:int_type())],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Echo_float = mochi@query:query_with_args(
        <<"echoFloat"/utf8>>,
        [mochi@query:arg(<<"x"/utf8>>, mochi@schema:float_type())],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Echo_bool = mochi@query:query_with_args(
        <<"echoBool"/utf8>>,
        [mochi@query:arg(<<"x"/utf8>>, mochi@schema:boolean_type())],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    Echo_required = mochi@query:query_with_args(
        <<"echoRequired"/utf8>>,
        [mochi@query:arg(
                <<"x"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:string_type(),
        fun(_) -> {ok, <<"ok"/utf8>>} end,
        fun(_, _) -> {ok, <<"ok"/utf8>>} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Echo_string),
    _pipe@2 = mochi@query:add_query(_pipe@1, Echo_id),
    _pipe@3 = mochi@query:add_query(_pipe@2, Echo_int),
    _pipe@4 = mochi@query:add_query(_pipe@3, Echo_float),
    _pipe@5 = mochi@query:add_query(_pipe@4, Echo_bool),
    _pipe@6 = mochi@query:add_query(_pipe@5, Echo_required),
    mochi@query:build(_pipe@6).

-file("test/variable_coercion_test.gleam", 92).
-spec null_dyn() -> gleam@dynamic:dynamic_().
null_dyn() ->
    gleam_stdlib:identity(nil).

-file("test/variable_coercion_test.gleam", 96).
-spec error_message(mochi@executor:execution_error()) -> binary().
error_message(Err) ->
    case Err of
        {validation_error, M, _, _} ->
            M;

        {resolver_error, M@1, _, _} ->
            M@1;

        {type_error, M@2, _, _} ->
            M@2;

        {null_value_error, M@3, _, _} ->
            M@3
    end.

-file("test/variable_coercion_test.gleam", 105).
-spec has_error_containing(mochi@executor:execution_result(), binary()) -> boolean().
has_error_containing(Result, Needle) ->
    gleam@list:any(
        erlang:element(3, Result),
        fun(E) -> gleam_stdlib:contains_string(error_message(E), Needle) end
    ).

-file("test/variable_coercion_test.gleam", 114).
-spec variable_errors(mochi@executor:execution_result()) -> list(binary()).
variable_errors(Result) ->
    _pipe = erlang:element(3, Result),
    _pipe@1 = gleam@list:map(_pipe, fun error_message/1),
    gleam@list:filter(
        _pipe@1,
        fun(M) -> gleam_stdlib:contains_string(M, <<"Variable \"$"/utf8>>) end
    ).

-file("test/variable_coercion_test.gleam", 124).
-spec explicit_null_for_nullable_string_variable_coerces_test() -> nil.
explicit_null_for_nullable_string_variable_coerces_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String) { echoString(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_nullable_string_variable_coerces_test"/utf8>>,
                line => 131,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 3888,
                    'end' => 3911
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 3915,
                    'end' => 3917
                    },
                start => 3881,
                'end' => 3917,
                expression_start => 3888})
    end.

-file("test/variable_coercion_test.gleam", 134).
-spec explicit_null_for_nullable_id_variable_coerces_test() -> nil.
explicit_null_for_nullable_id_variable_coerces_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: ID) { echoId(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_nullable_id_variable_coerces_test"/utf8>>,
                line => 139,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 4183,
                    'end' => 4206
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 4210,
                    'end' => 4212
                    },
                start => 4176,
                'end' => 4212,
                expression_start => 4183})
    end.

-file("test/variable_coercion_test.gleam", 142).
-spec explicit_null_for_nullable_int_variable_coerces_test() -> nil.
explicit_null_for_nullable_int_variable_coerces_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: Int) { echoInt(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_nullable_int_variable_coerces_test"/utf8>>,
                line => 147,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 4481,
                    'end' => 4504
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 4508,
                    'end' => 4510
                    },
                start => 4474,
                'end' => 4510,
                expression_start => 4481})
    end.

-file("test/variable_coercion_test.gleam", 150).
-spec explicit_null_for_nullable_float_variable_coerces_test() -> nil.
explicit_null_for_nullable_float_variable_coerces_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: Float) { echoFloat(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_nullable_float_variable_coerces_test"/utf8>>,
                line => 155,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 4785,
                    'end' => 4808
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 4812,
                    'end' => 4814
                    },
                start => 4778,
                'end' => 4814,
                expression_start => 4785})
    end.

-file("test/variable_coercion_test.gleam", 158).
-spec explicit_null_for_nullable_boolean_variable_coerces_test() -> nil.
explicit_null_for_nullable_boolean_variable_coerces_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: Boolean) { echoBool(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_nullable_boolean_variable_coerces_test"/utf8>>,
                line => 163,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 5092,
                    'end' => 5115
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 5119,
                    'end' => 5121
                    },
                start => 5085,
                'end' => 5121,
                expression_start => 5092})
    end.

-file("test/variable_coercion_test.gleam", 170).
-spec explicit_null_for_non_null_variable_errors_test() -> nil.
explicit_null_for_non_null_variable_errors_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String!) { echoRequired(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, null_dyn()}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = <<"Expected non-null value but got null"/utf8>>,
    case has_error_containing(Result, _assert_subject) of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"explicit_null_for_non_null_variable_errors_test"/utf8>>,
                line => 176,
                kind => function_call,
                arguments => [#{kind => expression,
                        value => Result,
                        start => 5641,
                        'end' => 5647
                        }, #{kind => literal,
                        value => _assert_subject,
                        start => 5649,
                        'end' => 5687
                        }],
                start => 5613,
                'end' => 5688,
                expression_start => 5620})
    end.

-file("test/variable_coercion_test.gleam", 183).
-spec non_null_string_value_for_nullable_string_variable_ok_test() -> nil.
non_null_string_value_for_nullable_string_variable_ok_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String) { echoString(x: $x) }"/utf8>>,
    Vars = maps:from_list(
        [{<<"x"/utf8>>, gleam_stdlib:identity(<<"hello"/utf8>>)}]
    ),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"non_null_string_value_for_nullable_string_variable_ok_test"/utf8>>,
                line => 189,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 6195,
                    'end' => 6218
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 6222,
                    'end' => 6224
                    },
                start => 6188,
                'end' => 6224,
                expression_start => 6195})
    end.

-file("test/variable_coercion_test.gleam", 192).
-spec number_value_for_nullable_string_variable_errors_test() -> nil.
number_value_for_nullable_string_variable_errors_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String) { echoString(x: $x) }"/utf8>>,
    Vars = maps:from_list([{<<"x"/utf8>>, gleam_stdlib:identity(42)}]),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = <<"Expected String, got incompatible type"/utf8>>,
    case has_error_containing(Result, _assert_subject) of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"number_value_for_nullable_string_variable_errors_test"/utf8>>,
                line => 198,
                kind => function_call,
                arguments => [#{kind => expression,
                        value => Result,
                        start => 6532,
                        'end' => 6538
                        }, #{kind => literal,
                        value => _assert_subject,
                        start => 6540,
                        'end' => 6580
                        }],
                start => 6504,
                'end' => 6581,
                expression_start => 6511})
    end.

-file("test/variable_coercion_test.gleam", 205).
-spec missing_nullable_variable_coerces_to_null_test() -> nil.
missing_nullable_variable_coerces_to_null_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String) { echoString(x: $x) }"/utf8>>,
    Vars = maps:new(),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = variable_errors(Result),
    _assert_subject@1 = [],
    case _assert_subject =:= _assert_subject@1 of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"missing_nullable_variable_coerces_to_null_test"/utf8>>,
                line => 212,
                kind => binary_operator,
                operator => '==',
                left => #{kind => expression,
                    value => _assert_subject,
                    start => 7109,
                    'end' => 7132
                    },
                right => #{kind => literal,
                    value => _assert_subject@1,
                    start => 7136,
                    'end' => 7138
                    },
                start => 7102,
                'end' => 7138,
                expression_start => 7109})
    end.

-file("test/variable_coercion_test.gleam", 215).
-spec missing_non_null_variable_errors_test() -> nil.
missing_non_null_variable_errors_test() ->
    Sch = build_schema(),
    Q = <<"query Q($x: String!) { echoRequired(x: $x) }"/utf8>>,
    Vars = maps:new(),
    Result = mochi@executor:execute_query_with_variables(Sch, Q, Vars),
    _assert_subject = <<"is not provided"/utf8>>,
    case has_error_containing(Result, _assert_subject) of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"variable_coercion_test"/utf8>>,
                function => <<"missing_non_null_variable_errors_test"/utf8>>,
                line => 221,
                kind => function_call,
                arguments => [#{kind => expression,
                        value => Result,
                        start => 7397,
                        'end' => 7403
                        }, #{kind => literal,
                        value => _assert_subject,
                        start => 7405,
                        'end' => 7422
                        }],
                start => 7369,
                'end' => 7423,
                expression_start => 7376})
    end.
