-module(input_coercion_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/input_coercion_test.gleam").
-export([coerce_string_valid_test/0, coerce_string_invalid_type_test/0, coerce_int_valid_test/0, coerce_int_invalid_type_test/0, coerce_float_from_int_test/0, coerce_float_valid_test/0, coerce_boolean_valid_test/0, coerce_id_from_string_test/0, coerce_id_from_int_test/0, coerce_non_null_valid_test/0, coerce_non_null_with_null_test/0, coerce_nullable_with_null_test/0, coerce_enum_valid_test/0, coerce_enum_invalid_value_test/0, coerce_enum_string_not_allowed_test/0, coerce_list_valid_test/0, coerce_list_invalid_item_test/0, coerce_single_value_to_list_test/0, coerce_input_object_valid_test/0, coerce_input_object_missing_required_field_test/0, coerce_input_object_unknown_field_test/0, coerce_input_object_wrong_field_type_test/0, coerce_input_object_optional_field_missing_test/0, format_type_mismatch_error_test/0, format_invalid_enum_error_test/0, format_missing_field_error_test/0, format_unknown_field_error_test/0, format_null_not_allowed_error_test/0, coerce_arguments_valid_test/0, coerce_arguments_with_default_test/0, coerce_arguments_missing_required_test/0]).

-file("test/input_coercion_test.gleam", 12).
-spec test_schema() -> mochi@schema:schema().
test_schema() ->
    Role_enum = {enum_type,
        <<"Role"/utf8>>,
        none,
        maps:from_list(
            [{<<"ADMIN"/utf8>>,
                    {enum_value_definition,
                        <<"ADMIN"/utf8>>,
                        none,
                        gleam_stdlib:identity(<<"ADMIN"/utf8>>),
                        false,
                        none}},
                {<<"USER"/utf8>>,
                    {enum_value_definition,
                        <<"USER"/utf8>>,
                        none,
                        gleam_stdlib:identity(<<"USER"/utf8>>),
                        false,
                        none}}]
        )},
    Create_user_input = {input_object_type,
        <<"CreateUserInput"/utf8>>,
        none,
        maps:from_list(
            [{<<"name"/utf8>>,
                    {input_field_definition,
                        <<"name"/utf8>>,
                        none,
                        {non_null, {named, <<"String"/utf8>>}},
                        none}},
                {<<"age"/utf8>>,
                    {input_field_definition,
                        <<"age"/utf8>>,
                        none,
                        {named, <<"Int"/utf8>>},
                        none}},
                {<<"role"/utf8>>,
                    {input_field_definition,
                        <<"role"/utf8>>,
                        none,
                        {non_null, {named, <<"Role"/utf8>>}},
                        none}}]
        )},
    {schema,
        none,
        none,
        none,
        maps:from_list(
            [{<<"Role"/utf8>>, {enum_type_def, Role_enum}},
                {<<"CreateUserInput"/utf8>>,
                    {input_object_type_def, Create_user_input}}]
        ),
        maps:new(),
        none}.

-file("test/input_coercion_test.gleam", 94).
-spec coerce_string_valid_test() -> gleam@dynamic:dynamic_().
coerce_string_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {string_value, <<"hello"/utf8>>},
        {named, <<"String"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 107).
-spec coerce_string_invalid_type_test() -> nil.
coerce_string_invalid_type_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {int_value, 42},
        {named, <<"String"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {type_mismatch, _, <<"String"/utf8>>, <<"Int"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected TypeMismatch error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_string_invalid_type_test"/utf8>>,
                    line => 119})
    end.

-file("test/input_coercion_test.gleam", 123).
-spec coerce_int_valid_test() -> gleam@dynamic:dynamic_().
coerce_int_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {int_value, 42},
        {named, <<"Int"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 136).
-spec coerce_int_invalid_type_test() -> nil.
coerce_int_invalid_type_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {string_value, <<"42"/utf8>>},
        {named, <<"Int"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {type_mismatch, _, <<"Int"/utf8>>, <<"String"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected TypeMismatch error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_int_invalid_type_test"/utf8>>,
                    line => 148})
    end.

-file("test/input_coercion_test.gleam", 152).
-spec coerce_float_from_int_test() -> gleam@dynamic:dynamic_().
coerce_float_from_int_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {int_value, 42},
        {named, <<"Float"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 166).
-spec coerce_float_valid_test() -> gleam@dynamic:dynamic_().
coerce_float_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {float_value, 3.14},
        {named, <<"Float"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 179).
-spec coerce_boolean_valid_test() -> gleam@dynamic:dynamic_().
coerce_boolean_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {boolean_value, true},
        {named, <<"Boolean"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 192).
-spec coerce_id_from_string_test() -> gleam@dynamic:dynamic_().
coerce_id_from_string_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {string_value, <<"abc123"/utf8>>},
        {named, <<"ID"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 205).
-spec coerce_id_from_int_test() -> gleam@dynamic:dynamic_().
coerce_id_from_int_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {int_value, 123},
        {named, <<"ID"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 223).
-spec coerce_non_null_valid_test() -> gleam@dynamic:dynamic_().
coerce_non_null_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {string_value, <<"hello"/utf8>>},
        {non_null, {named, <<"String"/utf8>>}},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 236).
-spec coerce_non_null_with_null_test() -> nil.
coerce_non_null_with_null_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        null_value,
        {non_null, {named, <<"String"/utf8>>}},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {null_not_allowed, _}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected NullNotAllowed error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_non_null_with_null_test"/utf8>>,
                    line => 248})
    end.

-file("test/input_coercion_test.gleam", 252).
-spec coerce_nullable_with_null_test() -> gleam@dynamic:dynamic_().
coerce_nullable_with_null_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        null_value,
        {named, <<"String"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 269).
-spec coerce_enum_valid_test() -> gleam@dynamic:dynamic_().
coerce_enum_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {enum_value, <<"ADMIN"/utf8>>},
        {named, <<"Role"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 282).
-spec coerce_enum_invalid_value_test() -> nil.
coerce_enum_invalid_value_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {enum_value, <<"SUPERUSER"/utf8>>},
        {named, <<"Role"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {invalid_enum_value, _, <<"Role"/utf8>>, <<"SUPERUSER"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected InvalidEnumValue error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_enum_invalid_value_test"/utf8>>,
                    line => 294})
    end.

-file("test/input_coercion_test.gleam", 298).
-spec coerce_enum_string_not_allowed_test() -> nil.
coerce_enum_string_not_allowed_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {string_value, <<"ADMIN"/utf8>>},
        {named, <<"Role"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {type_mismatch, _, _, <<"String"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected TypeMismatch error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_enum_string_not_allowed_test"/utf8>>,
                    line => 311})
    end.

-file("test/input_coercion_test.gleam", 319).
-spec coerce_list_valid_test() -> gleam@dynamic:dynamic_().
coerce_list_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {list_value, [{int_value, 1}, {int_value, 2}, {int_value, 3}]},
        {list, {named, <<"Int"/utf8>>}},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 332).
-spec coerce_list_invalid_item_test() -> nil.
coerce_list_invalid_item_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {list_value,
            [{int_value, 1}, {string_value, <<"two"/utf8>>}, {int_value, 3}]},
        {list, {named, <<"Int"/utf8>>}},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    case Result of
        {error, {type_mismatch, Path, <<"Int"/utf8>>, <<"String"/utf8>>}} ->
            case Path of
                [<<"field"/utf8>>, <<"1"/utf8>>] ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected path to include index"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"input_coercion_test"/utf8>>,
                            function => <<"coerce_list_invalid_item_test"/utf8>>,
                            line => 347})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected TypeMismatch error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_list_invalid_item_test"/utf8>>,
                    line => 350})
    end.

-file("test/input_coercion_test.gleam", 354).
-spec coerce_single_value_to_list_test() -> gleam@dynamic:dynamic_().
coerce_single_value_to_list_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {int_value, 42},
        {list, {named, <<"Int"/utf8>>}},
        test_schema(),
        maps:new(),
        [<<"field"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 372).
-spec coerce_input_object_valid_test() -> gleam@dynamic:dynamic_().
coerce_input_object_valid_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {object_value,
            [{object_field, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
                {object_field, <<"age"/utf8>>, {int_value, 30}},
                {object_field, <<"role"/utf8>>, {enum_value, <<"USER"/utf8>>}}]},
        {named, <<"CreateUserInput"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"input"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 389).
-spec coerce_input_object_missing_required_field_test() -> nil.
coerce_input_object_missing_required_field_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {object_value,
            [{object_field, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
                {object_field, <<"age"/utf8>>, {int_value, 30}}]},
        {named, <<"CreateUserInput"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"input"/utf8>>]
    ),
    case Result of
        {error, {missing_required_field, _, <<"role"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected MissingRequiredField error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_input_object_missing_required_field_test"/utf8>>,
                    line => 405})
    end.

-file("test/input_coercion_test.gleam", 409).
-spec coerce_input_object_unknown_field_test() -> nil.
coerce_input_object_unknown_field_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {object_value,
            [{object_field, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
                {object_field,
                    <<"email"/utf8>>,
                    {string_value, <<"alice@example.com"/utf8>>}},
                {object_field, <<"role"/utf8>>, {enum_value, <<"USER"/utf8>>}}]},
        {named, <<"CreateUserInput"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"input"/utf8>>]
    ),
    case Result of
        {error,
            {unknown_field, _, <<"email"/utf8>>, <<"CreateUserInput"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected UnknownField error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_input_object_unknown_field_test"/utf8>>,
                    line => 425})
    end.

-file("test/input_coercion_test.gleam", 429).
-spec coerce_input_object_wrong_field_type_test() -> nil.
coerce_input_object_wrong_field_type_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {object_value,
            [{object_field, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
                {object_field,
                    <<"age"/utf8>>,
                    {string_value, <<"thirty"/utf8>>}},
                {object_field, <<"role"/utf8>>, {enum_value, <<"USER"/utf8>>}}]},
        {named, <<"CreateUserInput"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"input"/utf8>>]
    ),
    case Result of
        {error, {type_mismatch, Path, <<"Int"/utf8>>, <<"String"/utf8>>}} ->
            case Path of
                [<<"input"/utf8>>, <<"age"/utf8>>] ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected path to include field name"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"input_coercion_test"/utf8>>,
                            function => <<"coerce_input_object_wrong_field_type_test"/utf8>>,
                            line => 447})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected TypeMismatch error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_input_object_wrong_field_type_test"/utf8>>,
                    line => 450})
    end.

-file("test/input_coercion_test.gleam", 454).
-spec coerce_input_object_optional_field_missing_test() -> gleam@dynamic:dynamic_().
coerce_input_object_optional_field_missing_test() ->
    Result = mochi@input_coercion:coerce_argument_value(
        {object_value,
            [{object_field, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
                {object_field, <<"role"/utf8>>, {enum_value, <<"USER"/utf8>>}}]},
        {named, <<"CreateUserInput"/utf8>>},
        test_schema(),
        maps:new(),
        [<<"input"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 475).
-spec format_type_mismatch_error_test() -> nil.
format_type_mismatch_error_test() ->
    Error = {type_mismatch,
        [<<"field"/utf8>>, <<"nested"/utf8>>],
        <<"String"/utf8>>,
        <<"Int"/utf8>>},
    Message = mochi@input_coercion:format_error(Error),
    gleeunit@should:equal(
        Message,
        <<"Type mismatch at field.nested: expected String, got Int"/utf8>>
    ).

-file("test/input_coercion_test.gleam", 485).
-spec format_invalid_enum_error_test() -> nil.
format_invalid_enum_error_test() ->
    Error = {invalid_enum_value,
        [<<"input"/utf8>>, <<"role"/utf8>>],
        <<"Role"/utf8>>,
        <<"SUPERUSER"/utf8>>},
    Message = mochi@input_coercion:format_error(Error),
    gleeunit@should:equal(
        Message,
        <<"Invalid enum value at input.role: 'SUPERUSER' is not a valid Role"/utf8>>
    ).

-file("test/input_coercion_test.gleam", 496).
-spec format_missing_field_error_test() -> nil.
format_missing_field_error_test() ->
    Error = {missing_required_field, [<<"input"/utf8>>], <<"name"/utf8>>},
    Message = mochi@input_coercion:format_error(Error),
    gleeunit@should:equal(
        Message,
        <<"Missing required field 'name' at input"/utf8>>
    ).

-file("test/input_coercion_test.gleam", 503).
-spec format_unknown_field_error_test() -> nil.
format_unknown_field_error_test() ->
    Error = {unknown_field,
        [<<"input"/utf8>>],
        <<"email"/utf8>>,
        <<"CreateUserInput"/utf8>>},
    Message = mochi@input_coercion:format_error(Error),
    gleeunit@should:equal(
        Message,
        <<"Unknown field 'email' on input type CreateUserInput at input"/utf8>>
    ).

-file("test/input_coercion_test.gleam", 513).
-spec format_null_not_allowed_error_test() -> nil.
format_null_not_allowed_error_test() ->
    Error = {null_not_allowed, [<<"field"/utf8>>]},
    Message = mochi@input_coercion:format_error(Error),
    gleeunit@should:equal(Message, <<"Null value not allowed at field"/utf8>>).

-file("test/input_coercion_test.gleam", 524).
-spec coerce_arguments_valid_test() -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
coerce_arguments_valid_test() ->
    Arg_defs = maps:from_list(
        [{<<"name"/utf8>>,
                {argument_definition,
                    <<"name"/utf8>>,
                    none,
                    {non_null, {named, <<"String"/utf8>>}},
                    none}},
            {<<"age"/utf8>>,
                {argument_definition,
                    <<"age"/utf8>>,
                    none,
                    {named, <<"Int"/utf8>>},
                    none}}]
    ),
    Ast_args = [{argument, <<"name"/utf8>>, {string_value, <<"Alice"/utf8>>}},
        {argument, <<"age"/utf8>>, {int_value, 30}}],
    Result = mochi@input_coercion:coerce_arguments(
        Ast_args,
        Arg_defs,
        test_schema(),
        maps:new(),
        [<<"createUser"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 564).
-spec coerce_arguments_with_default_test() -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
coerce_arguments_with_default_test() ->
    Arg_defs = maps:from_list(
        [{<<"limit"/utf8>>,
                {argument_definition,
                    <<"limit"/utf8>>,
                    none,
                    {named, <<"Int"/utf8>>},
                    {some, gleam_stdlib:identity(10)}}}]
    ),
    Ast_args = [],
    Result = mochi@input_coercion:coerce_arguments(
        Ast_args,
        Arg_defs,
        test_schema(),
        maps:new(),
        [<<"users"/utf8>>]
    ),
    gleeunit@should:be_ok(Result).

-file("test/input_coercion_test.gleam", 592).
-spec coerce_arguments_missing_required_test() -> nil.
coerce_arguments_missing_required_test() ->
    Arg_defs = maps:from_list(
        [{<<"id"/utf8>>,
                {argument_definition,
                    <<"id"/utf8>>,
                    none,
                    {non_null, {named, <<"ID"/utf8>>}},
                    none}}]
    ),
    Ast_args = [],
    Result = mochi@input_coercion:coerce_arguments(
        Ast_args,
        Arg_defs,
        test_schema(),
        maps:new(),
        [<<"user"/utf8>>]
    ),
    case Result of
        {error, {missing_required_field, _, <<"id"/utf8>>}} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected MissingRequiredField error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"input_coercion_test"/utf8>>,
                    function => <<"coerce_arguments_missing_required_test"/utf8>>,
                    line => 619})
    end.
