-module(mochi@input_coercion).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/input_coercion.gleam").
-export([format_error/1, coerce_argument_value/5, coerce_arguments/5]).
-export_type([coercion_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Input value coercion and validation for GraphQL arguments.\n"
    "\n"
    " This module validates and coerces input values (arguments, variables)\n"
    " against their declared types in the schema. It ensures:\n"
    " - Type compatibility (Int vs String, etc.)\n"
    " - Enum value validity\n"
    " - Input object field validation\n"
    " - Non-null constraint enforcement\n"
    " - List item type validation\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let result = input_coercion.coerce_argument_value(\n"
    "   ast.StringValue(\"hello\"),\n"
    "   schema.Named(\"String\"),\n"
    "   schema,\n"
    "   variables,\n"
    "   [\"createUser\", \"input\", \"name\"],\n"
    " )\n"
    " // Ok(dynamic(\"hello\"))\n"
    " ```\n"
).

-type coercion_error() :: {type_mismatch, list(binary()), binary(), binary()} |
    {invalid_enum_value, list(binary()), binary(), binary()} |
    {missing_required_field, list(binary()), binary()} |
    {unknown_field, list(binary()), binary(), binary()} |
    {null_not_allowed, list(binary())} |
    {unknown_type, list(binary()), binary()} |
    {scalar_coercion_failed, list(binary()), binary(), binary()}.

-file("src/mochi/input_coercion.gleam", 255).
?DOC(" Validate that no unknown fields are provided\n").
-spec validate_no_unknown_fields(
    list(mochi@ast:object_field()),
    mochi@schema:input_object_type(),
    list(binary())
) -> {ok, nil} | {error, coercion_error()}.
validate_no_unknown_fields(Provided_fields, Input_type, Path) ->
    gleam@list:try_each(
        Provided_fields,
        fun(Field) ->
            case gleam@dict:has_key(
                erlang:element(4, Input_type),
                erlang:element(2, Field)
            ) of
                true ->
                    {ok, nil};

                false ->
                    {error,
                        {unknown_field,
                            Path,
                            erlang:element(2, Field),
                            erlang:element(2, Input_type)}}
            end
        end
    ).

-file("src/mochi/input_coercion.gleam", 383).
?DOC(" Convert an AST value to Dynamic (without validation)\n").
-spec ast_value_to_dynamic(mochi@ast:value()) -> gleam@dynamic:dynamic_().
ast_value_to_dynamic(Value) ->
    case Value of
        {int_value, I} ->
            gleam_stdlib:identity(I);

        {float_value, F} ->
            gleam_stdlib:identity(F);

        {string_value, S} ->
            gleam_stdlib:identity(S);

        {boolean_value, B} ->
            gleam_stdlib:identity(B);

        null_value ->
            gleam_stdlib:identity(nil);

        {enum_value, E} ->
            gleam_stdlib:identity(E);

        {variable_value, Name} ->
            gleam_stdlib:identity(Name);

        {list_value, Values} ->
            gleam_stdlib:identity(
                gleam@list:map(Values, fun ast_value_to_dynamic/1)
            );

        {object_value, Fields} ->
            gleam_stdlib:identity(
                gleam@list:fold(
                    Fields,
                    maps:new(),
                    fun(Acc, F@1) ->
                        gleam@dict:insert(
                            Acc,
                            erlang:element(2, F@1),
                            ast_value_to_dynamic(erlang:element(3, F@1))
                        )
                    end
                )
            )
    end.

-file("src/mochi/input_coercion.gleam", 367).
?DOC(" Coerce to a custom scalar type\n").
-spec coerce_custom_scalar(
    mochi@ast:value(),
    mochi@schema:scalar_type(),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_custom_scalar(Value, Scalar_type, Path) ->
    Raw_value = ast_value_to_dynamic(Value),
    case (erlang:element(6, Scalar_type))(Raw_value) of
        {ok, Parsed} ->
            {ok, Parsed};

        {error, Reason} ->
            {error,
                {scalar_coercion_failed,
                    Path,
                    erlang:element(2, Scalar_type),
                    Reason}}
    end.

-file("src/mochi/input_coercion.gleam", 404).
?DOC(" Get a human-readable name for an AST value type\n").
-spec value_type_name(mochi@ast:value()) -> binary().
value_type_name(Value) ->
    case Value of
        {int_value, _} ->
            <<"Int"/utf8>>;

        {float_value, _} ->
            <<"Float"/utf8>>;

        {string_value, _} ->
            <<"String"/utf8>>;

        {boolean_value, _} ->
            <<"Boolean"/utf8>>;

        null_value ->
            <<"Null"/utf8>>;

        {enum_value, _} ->
            <<"Enum"/utf8>>;

        {list_value, _} ->
            <<"List"/utf8>>;

        {object_value, _} ->
            <<"Object"/utf8>>;

        {variable_value, _} ->
            <<"Variable"/utf8>>
    end.

-file("src/mochi/input_coercion.gleam", 128).
?DOC(" Coerce to String scalar\n").
-spec coerce_string(mochi@ast:value(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_string(Value, Path) ->
    case Value of
        {string_value, S} ->
            {ok, gleam_stdlib:identity(S)};

        Other ->
            {error,
                {type_mismatch, Path, <<"String"/utf8>>, value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 136).
?DOC(" Coerce to Int scalar\n").
-spec coerce_int(mochi@ast:value(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_int(Value, Path) ->
    case Value of
        {int_value, I} ->
            {ok, gleam_stdlib:identity(I)};

        Other ->
            {error,
                {type_mismatch, Path, <<"Int"/utf8>>, value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 144).
?DOC(" Coerce to Float scalar (Int coerces to Float per spec)\n").
-spec coerce_float(mochi@ast:value(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_float(Value, Path) ->
    case Value of
        {float_value, F} ->
            {ok, gleam_stdlib:identity(F)};

        {int_value, I} ->
            {ok, gleam_stdlib:identity(erlang:float(I))};

        Other ->
            {error,
                {type_mismatch, Path, <<"Float"/utf8>>, value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 153).
?DOC(" Coerce to Boolean scalar\n").
-spec coerce_boolean(mochi@ast:value(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_boolean(Value, Path) ->
    case Value of
        {boolean_value, B} ->
            {ok, gleam_stdlib:identity(B)};

        Other ->
            {error,
                {type_mismatch,
                    Path,
                    <<"Boolean"/utf8>>,
                    value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 161).
?DOC(" Coerce to ID scalar (accepts String or Int per spec)\n").
-spec coerce_id(mochi@ast:value(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_id(Value, Path) ->
    case Value of
        {string_value, S} ->
            {ok, gleam_stdlib:identity(S)};

        {int_value, I} ->
            {ok, gleam_stdlib:identity(erlang:integer_to_binary(I))};

        Other ->
            {error,
                {type_mismatch, Path, <<"ID"/utf8>>, value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 200).
?DOC(" Coerce to an enum type\n").
-spec coerce_enum(mochi@ast:value(), mochi@schema:enum_type(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, coercion_error()}.
coerce_enum(Value, Enum_type, Path) ->
    case Value of
        {enum_value, Enum_value} ->
            case gleam@dict:has_key(erlang:element(4, Enum_type), Enum_value) of
                true ->
                    {ok, gleam_stdlib:identity(Enum_value)};

                false ->
                    {error,
                        {invalid_enum_value,
                            Path,
                            erlang:element(2, Enum_type),
                            Enum_value}}
            end;

        {string_value, _} ->
            {error,
                {type_mismatch,
                    Path,
                    <<(erlang:element(2, Enum_type))/binary,
                        " (use enum value without quotes)"/utf8>>,
                    <<"String"/utf8>>}};

        Other ->
            {error,
                {type_mismatch,
                    Path,
                    erlang:element(2, Enum_type),
                    value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 468).
?DOC(" Format a path as a dot-separated string\n").
-spec format_path(list(binary())) -> binary().
format_path(Path) ->
    case Path of
        [] ->
            <<"(root)"/utf8>>;

        _ ->
            gleam@string:join(Path, <<"."/utf8>>)
    end.

-file("src/mochi/input_coercion.gleam", 423).
?DOC(" Format a coercion error as a human-readable message\n").
-spec format_error(coercion_error()) -> binary().
format_error(Error) ->
    case Error of
        {type_mismatch, Path, Expected, Got} ->
            <<<<<<<<<<"Type mismatch at "/utf8, (format_path(Path))/binary>>/binary,
                            ": expected "/utf8>>/binary,
                        Expected/binary>>/binary,
                    ", got "/utf8>>/binary,
                Got/binary>>;

        {invalid_enum_value, Path@1, Enum_name, Value} ->
            <<<<<<<<<<"Invalid enum value at "/utf8,
                                (format_path(Path@1))/binary>>/binary,
                            ": '"/utf8>>/binary,
                        Value/binary>>/binary,
                    "' is not a valid "/utf8>>/binary,
                Enum_name/binary>>;

        {missing_required_field, Path@2, Field} ->
            <<<<<<"Missing required field '"/utf8, Field/binary>>/binary,
                    "' at "/utf8>>/binary,
                (format_path(Path@2))/binary>>;

        {unknown_field, Path@3, Field@1, Type_name} ->
            <<<<<<<<<<"Unknown field '"/utf8, Field@1/binary>>/binary,
                            "' on input type "/utf8>>/binary,
                        Type_name/binary>>/binary,
                    " at "/utf8>>/binary,
                (format_path(Path@3))/binary>>;

        {null_not_allowed, Path@4} ->
            <<"Null value not allowed at "/utf8, (format_path(Path@4))/binary>>;

        {unknown_type, Path@5, Type_name@1} ->
            <<<<<<"Unknown type '"/utf8, Type_name@1/binary>>/binary,
                    "' at "/utf8>>/binary,
                (format_path(Path@5))/binary>>;

        {scalar_coercion_failed, Path@6, Scalar_name, Reason} ->
            <<<<<<<<<<"Invalid "/utf8, Scalar_name/binary>>/binary,
                            " value at "/utf8>>/binary,
                        (format_path(Path@6))/binary>>/binary,
                    ": "/utf8>>/binary,
                Reason/binary>>
    end.

-file("src/mochi/input_coercion.gleam", 558).
-spec add_missing_defaults(
    list({binary(), mochi@schema:argument_definition()}),
    gleam@set:set(binary()),
    list(binary()),
    list({binary(), gleam@dynamic:dynamic_()})
) -> {ok, list({binary(), gleam@dynamic:dynamic_()})} |
    {error, coercion_error()}.
add_missing_defaults(Arg_defs, Provided_names, Field_path, Acc) ->
    case Arg_defs of
        [] ->
            {ok, Acc};

        [{Name, Def} | Rest] ->
            case gleam@set:contains(Provided_names, Name) of
                true ->
                    add_missing_defaults(Rest, Provided_names, Field_path, Acc);

                false ->
                    case erlang:element(5, Def) of
                        {some, Default} ->
                            add_missing_defaults(
                                Rest,
                                Provided_names,
                                Field_path,
                                [{Name, Default} | Acc]
                            );

                        none ->
                            case erlang:element(4, Def) of
                                {non_null, _} ->
                                    {error,
                                        {missing_required_field,
                                            Field_path,
                                            Name}};

                                _ ->
                                    add_missing_defaults(
                                        Rest,
                                        Provided_names,
                                        Field_path,
                                        Acc
                                    )
                            end
                    end
            end
    end.

-file("src/mochi/input_coercion.gleam", 269).
?DOC(" Coerce all fields of an input object\n").
-spec coerce_input_fields(
    list({binary(), mochi@schema:input_field_definition()}),
    list(mochi@ast:object_field()),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary()),
    list({binary(), gleam@dynamic:dynamic_()})
) -> {ok, list({binary(), gleam@dynamic:dynamic_()})} |
    {error, coercion_error()}.
coerce_input_fields(
    Expected_fields,
    Provided_fields,
    Schema,
    Variables,
    Path,
    Acc
) ->
    case Expected_fields of
        [] ->
            {ok, lists:reverse(Acc)};

        [{Field_name, Field_def} | Rest] ->
            Field_path = lists:append(Path, [Field_name]),
            Provided_value = begin
                _pipe = gleam@list:find(
                    Provided_fields,
                    fun(F) -> erlang:element(2, F) =:= Field_name end
                ),
                gleam@result:map(_pipe, fun(F@1) -> erlang:element(3, F@1) end)
            end,
            gleam@result:'try'(case Provided_value of
                    {ok, Value} ->
                        coerce_argument_value(
                            Value,
                            erlang:element(4, Field_def),
                            Schema,
                            Variables,
                            Field_path
                        );

                    {error, _} ->
                        case erlang:element(5, Field_def) of
                            {some, Default} ->
                                {ok, Default};

                            none ->
                                case erlang:element(4, Field_def) of
                                    {non_null, _} ->
                                        {error,
                                            {missing_required_field,
                                                Path,
                                                Field_name}};

                                    _ ->
                                        {ok, gleam_stdlib:identity(nil)}
                                end
                        end
                end, fun(Coerced) ->
                    coerce_input_fields(
                        Rest,
                        Provided_fields,
                        Schema,
                        Variables,
                        Path,
                        [{Field_name, Coerced} | Acc]
                    )
                end)
    end.

-file("src/mochi/input_coercion.gleam", 62).
?DOC(
    " Coerce and validate an argument value against its declared type.\n"
    "\n"
    " This is the main entry point for input validation. It recursively\n"
    " validates the value structure against the schema type definition.\n"
).
-spec coerce_argument_value(
    mochi@ast:value(),
    mochi@schema:field_type(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_argument_value(Value, Expected_type, Schema, Variables, Path) ->
    case {Expected_type, Value} of
        {{non_null, _}, null_value} ->
            {error, {null_not_allowed, Path}};

        {{non_null, Inner}, Other} ->
            coerce_argument_value(Other, Inner, Schema, Variables, Path);

        {_, null_value} ->
            {ok, gleam_stdlib:identity(nil)};

        {_, {variable_value, Var_name}} ->
            case gleam_stdlib:map_get(Variables, Var_name) of
                {ok, Val} ->
                    {ok, Val};

                {error, _} ->
                    {ok, gleam_stdlib:identity(nil)}
            end;

        {{list, Inner_type}, {list_value, Items}} ->
            coerce_list(Items, Inner_type, Schema, Variables, Path);

        {{list, Inner_type@1}, Single_value} ->
            gleam@result:'try'(
                coerce_argument_value(
                    Single_value,
                    Inner_type@1,
                    Schema,
                    Variables,
                    lists:append(Path, [<<"0"/utf8>>])
                ),
                fun(Coerced) -> {ok, gleam_stdlib:identity([Coerced])} end
            );

        {{named, Type_name}, _} ->
            coerce_named_type(Value, Type_name, Schema, Variables, Path)
    end.

-file("src/mochi/input_coercion.gleam", 109).
?DOC(" Coerce a value against a named type (scalar, enum, or input object)\n").
-spec coerce_named_type(
    mochi@ast:value(),
    binary(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_named_type(Value, Type_name, Schema, Variables, Path) ->
    case Type_name of
        <<"String"/utf8>> ->
            coerce_string(Value, Path);

        <<"Int"/utf8>> ->
            coerce_int(Value, Path);

        <<"Float"/utf8>> ->
            coerce_float(Value, Path);

        <<"Boolean"/utf8>> ->
            coerce_boolean(Value, Path);

        <<"ID"/utf8>> ->
            coerce_id(Value, Path);

        _ ->
            coerce_custom_type(Value, Type_name, Schema, Variables, Path)
    end.

-file("src/mochi/input_coercion.gleam", 170).
?DOC(" Coerce to a custom type (enum, input object, or custom scalar)\n").
-spec coerce_custom_type(
    mochi@ast:value(),
    binary(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_custom_type(Value, Type_name, Schema, Variables, Path) ->
    case gleam_stdlib:map_get(erlang:element(5, Schema), Type_name) of
        {ok, {enum_type_def, Enum_type}} ->
            coerce_enum(Value, Enum_type, Path);

        {ok, {input_object_type_def, Input_type}} ->
            coerce_input_object(Value, Input_type, Schema, Variables, Path);

        {ok, {scalar_type_def, Scalar_type}} ->
            coerce_custom_scalar(Value, Scalar_type, Path);

        {ok, _} ->
            {error,
                {type_mismatch,
                    Path,
                    <<Type_name/binary, " (input type)"/utf8>>,
                    <<"output type"/utf8>>}};

        {error, _} ->
            {error, {unknown_type, Path, Type_name}}
    end.

-file("src/mochi/input_coercion.gleam", 224).
?DOC(" Coerce to an input object type\n").
-spec coerce_input_object(
    mochi@ast:value(),
    mochi@schema:input_object_type(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_input_object(Value, Input_type, Schema, Variables, Path) ->
    case Value of
        {object_value, Fields} ->
            gleam@result:'try'(
                validate_no_unknown_fields(Fields, Input_type, Path),
                fun(_) ->
                    gleam@result:'try'(
                        coerce_input_fields(
                            begin
                                _pipe = erlang:element(4, Input_type),
                                maps:to_list(_pipe)
                            end,
                            Fields,
                            Schema,
                            Variables,
                            Path,
                            []
                        ),
                        fun(Coerced_fields) ->
                            {ok,
                                gleam_stdlib:identity(
                                    maps:from_list(Coerced_fields)
                                )}
                        end
                    )
                end
            );

        Other ->
            {error,
                {type_mismatch,
                    Path,
                    erlang:element(2, Input_type),
                    value_type_name(Other)}}
    end.

-file("src/mochi/input_coercion.gleam", 338).
-spec coerce_list_items(
    list(mochi@ast:value()),
    mochi@schema:field_type(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary()),
    integer(),
    list(gleam@dynamic:dynamic_())
) -> {ok, list(gleam@dynamic:dynamic_())} | {error, coercion_error()}.
coerce_list_items(Items, Inner_type, Schema, Variables, Path, Index, Acc) ->
    case Items of
        [] ->
            {ok, Acc};

        [Item | Rest] ->
            Item_path = lists:append(Path, [erlang:integer_to_binary(Index)]),
            gleam@result:'try'(
                coerce_argument_value(
                    Item,
                    Inner_type,
                    Schema,
                    Variables,
                    Item_path
                ),
                fun(Coerced) ->
                    coerce_list_items(
                        Rest,
                        Inner_type,
                        Schema,
                        Variables,
                        Path,
                        Index + 1,
                        [Coerced | Acc]
                    )
                end
            )
    end.

-file("src/mochi/input_coercion.gleam", 325).
?DOC(" Coerce a list value\n").
-spec coerce_list(
    list(mochi@ast:value()),
    mochi@schema:field_type(),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, coercion_error()}.
coerce_list(Items, Inner_type, Schema, Variables, Path) ->
    gleam@result:'try'(
        coerce_list_items(Items, Inner_type, Schema, Variables, Path, 0, []),
        fun(Coerced_items) ->
            {ok, gleam_stdlib:identity(lists:reverse(Coerced_items))}
        end
    ).

-file("src/mochi/input_coercion.gleam", 516).
-spec coerce_provided_arguments(
    list(mochi@ast:argument()),
    gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary()),
    list({binary(), gleam@dynamic:dynamic_()})
) -> {ok, list({binary(), gleam@dynamic:dynamic_()})} |
    {error, coercion_error()}.
coerce_provided_arguments(Args, Arg_defs, Schema, Variables, Field_path, Acc) ->
    case Args of
        [] ->
            {ok, Acc};

        [Arg | Rest] ->
            Arg_path = lists:append(Field_path, [erlang:element(2, Arg)]),
            gleam@result:'try'(
                case gleam_stdlib:map_get(Arg_defs, erlang:element(2, Arg)) of
                    {ok, Def} ->
                        {ok, Def};

                    {error, _} ->
                        {ok,
                            {argument_definition,
                                erlang:element(2, Arg),
                                none,
                                {named, <<"String"/utf8>>},
                                none}}
                end,
                fun(Arg_def) ->
                    gleam@result:'try'(
                        coerce_argument_value(
                            erlang:element(3, Arg),
                            erlang:element(4, Arg_def),
                            Schema,
                            Variables,
                            Arg_path
                        ),
                        fun(Coerced) ->
                            coerce_provided_arguments(
                                Rest,
                                Arg_defs,
                                Schema,
                                Variables,
                                Field_path,
                                [{erlang:element(2, Arg), Coerced} | Acc]
                            )
                        end
                    )
                end
            )
    end.

-file("src/mochi/input_coercion.gleam", 480).
?DOC(" Coerce all arguments for a field, returning either all coerced values or the first error\n").
-spec coerce_arguments(
    list(mochi@ast:argument()),
    gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    mochi@schema:schema(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, coercion_error()}.
coerce_arguments(Ast_args, Arg_defs, Schema, Variables, Field_path) ->
    Provided_names = begin
        _pipe = Ast_args,
        _pipe@1 = gleam@list:map(_pipe, fun(A) -> erlang:element(2, A) end),
        gleam@set:from_list(_pipe@1)
    end,
    gleam@result:'try'(
        coerce_provided_arguments(
            Ast_args,
            Arg_defs,
            Schema,
            Variables,
            Field_path,
            []
        ),
        fun(Coerced_provided) ->
            gleam@result:'try'(
                add_missing_defaults(
                    begin
                        _pipe@2 = Arg_defs,
                        maps:to_list(_pipe@2)
                    end,
                    Provided_names,
                    Field_path,
                    Coerced_provided
                ),
                fun(With_defaults) -> {ok, maps:from_list(With_defaults)} end
            )
        end
    ).
