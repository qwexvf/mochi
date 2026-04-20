-module(mochi@executor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/executor.gleam").
-export([get_or_parse/2, execute_with_operation_name/6, execute/5, execute_query_with_variables/3, execute_query/2, execute_query_with_context/4, execute_query_debug_with_variables/3, execute_query_debug/2]).
-export_type([execution_result/0, execution_error/0, query_execution_context/0, field_context/0, debug_context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type execution_result() :: {execution_result,
        gleam@option:option(gleam@dynamic:dynamic_()),
        list(execution_error())}.

-type execution_error() :: {validation_error,
        binary(),
        list(binary()),
        gleam@option:option({integer(), integer()})} |
    {resolver_error,
        binary(),
        list(binary()),
        gleam@option:option({integer(), integer()})} |
    {type_error,
        binary(),
        list(binary()),
        gleam@option:option({integer(), integer()})} |
    {null_value_error,
        binary(),
        list(binary()),
        gleam@option:option({integer(), integer()})}.

-type query_execution_context() :: {query_execution_context,
        mochi@schema:schema(),
        gleam@option:option(gleam@dynamic:dynamic_()),
        mochi@schema:execution_context(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        gleam@dict:dict(binary(), mochi@ast:fragment())}.

-type field_context() :: {field_context,
        gleam@option:option(gleam@dynamic:dynamic_()),
        binary(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        list(binary())}.

-type debug_context() :: {debug_context, boolean(), integer(), integer()}.

-file("src/mochi/executor.gleam", 81).
-spec ok_result(gleam@dynamic:dynamic_()) -> execution_result().
ok_result(Data) ->
    {execution_result, {some, Data}, []}.

-file("src/mochi/executor.gleam", 85).
-spec error_result(execution_error()) -> execution_result().
error_result(Error) ->
    {execution_result, none, [Error]}.

-file("src/mochi/executor.gleam", 89).
-spec validation_error(binary(), list(binary())) -> execution_result().
validation_error(Msg, Path) ->
    error_result({validation_error, Msg, Path, none}).

-file("src/mochi/executor.gleam", 93).
-spec validation_error_at(
    binary(),
    list(binary()),
    gleam@option:option({integer(), integer()})
) -> execution_result().
validation_error_at(Msg, Path, Loc) ->
    error_result({validation_error, Msg, Path, Loc}).

-file("src/mochi/executor.gleam", 101).
-spec resolver_error(binary(), list(binary())) -> execution_result().
resolver_error(Msg, Path) ->
    error_result({resolver_error, Msg, Path, none}).

-file("src/mochi/executor.gleam", 105).
-spec resolver_error_at(
    binary(),
    list(binary()),
    gleam@option:option({integer(), integer()})
) -> execution_result().
resolver_error_at(Msg, Path, Loc) ->
    error_result({resolver_error, Msg, Path, Loc}).

-file("src/mochi/executor.gleam", 113).
-spec type_error(binary(), list(binary())) -> execution_result().
type_error(Msg, Path) ->
    error_result({type_error, Msg, Path, none}).

-file("src/mochi/executor.gleam", 117).
-spec null_value_error(binary(), list(binary())) -> execution_result().
null_value_error(Msg, Path) ->
    error_result({null_value_error, Msg, Path, none}).

-file("src/mochi/executor.gleam", 121).
-spec null_value_error_at(
    binary(),
    list(binary()),
    gleam@option:option({integer(), integer()})
) -> execution_result().
null_value_error_at(Msg, Path, Loc) ->
    error_result({null_value_error, Msg, Path, Loc}).

-file("src/mochi/executor.gleam", 210).
?DOC(" Get the operation name from a definition\n").
-spec get_operation_name_from_def(mochi@ast:definition()) -> gleam@option:option(binary()).
get_operation_name_from_def(Def) ->
    case Def of
        {operation_definition, {operation, _, Name, _, _, _}} ->
            Name;

        {operation_definition, {shorthand_query, _}} ->
            none;

        {fragment_definition, _} ->
            none
    end.

-file("src/mochi/executor.gleam", 178).
?DOC(" Find an operation in the document by name\n").
-spec find_operation_by_name(
    mochi@ast:document(),
    gleam@option:option(binary())
) -> {ok, mochi@ast:definition()} | {error, binary()}.
find_operation_by_name(Document, Operation_name) ->
    Operations = begin
        _pipe = erlang:element(2, Document),
        gleam@list:filter(_pipe, fun(Def) -> case Def of
                    {operation_definition, _} ->
                        true;

                    {fragment_definition, _} ->
                        false
                end end)
    end,
    case {Operation_name, Operations} of
        {none, [Single]} ->
            {ok, Single};

        {none, []} ->
            {error, <<"Document contains no operations"/utf8>>};

        {none, _} ->
            {error,
                <<"Document contains multiple operations; operation name is required"/utf8>>};

        {{some, Name}, Ops} ->
            _pipe@1 = Ops,
            _pipe@2 = gleam@list:find(
                _pipe@1,
                fun(Op) -> get_operation_name_from_def(Op) =:= {some, Name} end
            ),
            gleam@result:map_error(
                _pipe@2,
                fun(_) ->
                    <<<<"Operation '"/utf8, Name/binary>>/binary,
                        "' not found in document"/utf8>>
                end
            )
    end.

-file("src/mochi/executor.gleam", 218).
-spec extract_fragments(mochi@ast:document()) -> gleam@dict:dict(binary(), mochi@ast:fragment()).
extract_fragments(Document) ->
    _pipe = erlang:element(2, Document),
    _pipe@1 = gleam@list:filter_map(_pipe, fun(Def) -> case Def of
                {fragment_definition, Fragment} ->
                    {ok, {erlang:element(2, Fragment), Fragment}};

                {operation_definition, _} ->
                    {error, nil}
            end end),
    maps:from_list(_pipe@1).

-file("src/mochi/executor.gleam", 299).
-spec get_variable_definitions(mochi@ast:operation()) -> list(mochi@ast:variable_definition()).
get_variable_definitions(Operation) ->
    case Operation of
        {operation, _, _, Defs, _, _} ->
            Defs;

        {shorthand_query, _} ->
            []
    end.

-file("src/mochi/executor.gleam", 390).
-spec is_non_null_ast_type(mochi@ast:type()) -> boolean().
is_non_null_ast_type(T) ->
    case T of
        {non_null_type, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/executor.gleam", 398).
?DOC(" Convert an AST type to a schema FieldType for input coercion\n").
-spec ast_type_to_schema_type(mochi@ast:type()) -> mochi@schema:field_type().
ast_type_to_schema_type(T) ->
    case T of
        {named_type, Name} ->
            {named, Name};

        {list_type, Inner} ->
            {list, ast_type_to_schema_type(Inner)};

        {non_null_type, Inner@1} ->
            {non_null, ast_type_to_schema_type(Inner@1)}
    end.

-file("src/mochi/executor.gleam", 459).
-spec check_named_type_value(
    gleam@dynamic:dynamic_(),
    binary(),
    mochi@schema:schema()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
check_named_type_value(Value, Type_name, Schema_def) ->
    case Type_name of
        <<"String"/utf8>> ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, _} ->
                    {ok, Value};

                {error, _} ->
                    {error, <<"Expected String, got incompatible type"/utf8>>}
            end;

        <<"Int"/utf8>> ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_int/1}
            ) of
                {ok, _} ->
                    {ok, Value};

                {error, _} ->
                    {error, <<"Expected Int, got incompatible type"/utf8>>}
            end;

        <<"Float"/utf8>> ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_float/1}
            ) of
                {ok, _} ->
                    {ok, Value};

                {error, _} ->
                    case gleam@dynamic@decode:run(
                        Value,
                        {decoder, fun gleam@dynamic@decode:decode_int/1}
                    ) of
                        {ok, I} ->
                            {ok, gleam_stdlib:identity(erlang:float(I))};

                        {error, _} ->
                            {error,
                                <<"Expected Float, got incompatible type"/utf8>>}
                    end
            end;

        <<"Boolean"/utf8>> ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_bool/1}
            ) of
                {ok, _} ->
                    {ok, Value};

                {error, _} ->
                    {error, <<"Expected Boolean, got incompatible type"/utf8>>}
            end;

        <<"ID"/utf8>> ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, _} ->
                    {ok, Value};

                {error, _} ->
                    case gleam@dynamic@decode:run(
                        Value,
                        {decoder, fun gleam@dynamic@decode:decode_int/1}
                    ) of
                        {ok, I@1} ->
                            {ok,
                                gleam_stdlib:identity(
                                    erlang:integer_to_binary(I@1)
                                )};

                        {error, _} ->
                            {error,
                                <<"Expected ID (String or Int), got incompatible type"/utf8>>}
                    end
            end;

        _ ->
            case gleam_stdlib:map_get(erlang:element(5, Schema_def), Type_name) of
                {ok, {enum_type_def, Enum_def}} ->
                    case gleam@dynamic@decode:run(
                        Value,
                        {decoder, fun gleam@dynamic@decode:decode_string/1}
                    ) of
                        {ok, S} ->
                            case gleam@dict:has_key(
                                erlang:element(4, Enum_def),
                                S
                            ) of
                                true ->
                                    {ok, Value};

                                false ->
                                    {error,
                                        <<<<<<"Value \""/utf8, S/binary>>/binary,
                                                "\" is not a valid value for enum "/utf8>>/binary,
                                            Type_name/binary>>}
                            end;

                        {error, _} ->
                            {error,
                                <<"Expected enum value for "/utf8,
                                    Type_name/binary>>}
                    end;

                {ok, {input_object_type_def, _}} ->
                    {ok, Value};

                {ok, {scalar_type_def, _}} ->
                    {ok, Value};

                _ ->
                    {ok, Value}
            end
    end.

-file("src/mochi/executor.gleam", 528).
-spec is_null_value(gleam@dynamic:dynamic_()) -> boolean().
is_null_value(Value) ->
    case Value =:= gleam_stdlib:identity(nil) of
        true ->
            true;

        false ->
            case gleam@dynamic@decode:run(
                Value,
                gleam@dynamic@decode:optional(
                    {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                )
            ) of
                {ok, none} ->
                    true;

                _ ->
                    false
            end
    end.

-file("src/mochi/executor.gleam", 417).
-spec check_value_against_schema_type(
    gleam@dynamic:dynamic_(),
    mochi@schema:field_type(),
    mochi@schema:schema()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
check_value_against_schema_type(Value, Field_type, Schema_def) ->
    case Field_type of
        {non_null, Inner} ->
            case is_null_value(Value) of
                true ->
                    {error, <<"Expected non-null value but got null"/utf8>>};

                false ->
                    check_value_against_schema_type(Value, Inner, Schema_def)
            end;

        {list, Inner@1} ->
            case gleam@dynamic@decode:run(
                Value,
                gleam@dynamic@decode:list(
                    {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                )
            ) of
                {ok, Items} ->
                    Results = gleam@list:index_map(
                        Items,
                        fun(Item, Idx) ->
                            _pipe = check_value_against_schema_type(
                                Item,
                                Inner@1,
                                Schema_def
                            ),
                            gleam@result:map_error(
                                _pipe,
                                fun(Msg) ->
                                    <<<<<<"At index "/utf8,
                                                (erlang:integer_to_binary(Idx))/binary>>/binary,
                                            ": "/utf8>>/binary,
                                        Msg/binary>>
                                end
                            )
                        end
                    ),
                    Errors = gleam@list:filter_map(Results, fun(R) -> case R of
                                {ok, _} ->
                                    {error, nil};

                                {error, Msg@1} ->
                                    {ok, Msg@1}
                            end end),
                    case Errors of
                        [] ->
                            {ok, Value};

                        [First | _] ->
                            {error, First}
                    end;

                {error, _} ->
                    check_value_against_schema_type(Value, Inner@1, Schema_def)
            end;

        {named, Type_name} ->
            check_named_type_value(Value, Type_name, Schema_def)
    end.

-file("src/mochi/executor.gleam", 408).
?DOC(
    " Check that a Dynamic value is compatible with the declared variable type.\n"
    " Returns Ok with the (possibly coerced) value, or Error with a message.\n"
).
-spec check_variable_type(
    gleam@dynamic:dynamic_(),
    mochi@ast:type(),
    mochi@schema:schema()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
check_variable_type(Value, Declared_type, Schema_def) ->
    Schema_type = ast_type_to_schema_type(Declared_type),
    check_value_against_schema_type(Value, Schema_type, Schema_def).

-file("src/mochi/executor.gleam", 310).
?DOC(
    " Coerce variable values against their declared types.\n"
    " Returns Error with a list of ExecutionErrors if any variable is invalid.\n"
).
-spec coerce_variable_values(
    query_execution_context(),
    list(mochi@ast:variable_definition())
) -> {ok, gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, list(execution_error())}.
coerce_variable_values(Context, Var_defs) ->
    {Coerced, Errors} = gleam@list:fold(
        Var_defs,
        {erlang:element(5, Context), []},
        fun(Acc, Var_def) ->
            {Values, Errs} = Acc,
            Var_name = erlang:element(2, Var_def),
            Declared_type = erlang:element(3, Var_def),
            Provided = gleam_stdlib:map_get(Values, Var_name),
            case {Provided, erlang:element(4, Var_def)} of
                {{ok, Value}, _} ->
                    case {is_null_value(Value),
                        is_non_null_ast_type(Declared_type)} of
                        {true, false} ->
                            {gleam@dict:insert(Values, Var_name, Value), Errs};

                        {_, _} ->
                            case check_variable_type(
                                Value,
                                Declared_type,
                                erlang:element(2, Context)
                            ) of
                                {ok, Coerced_value} ->
                                    {gleam@dict:insert(
                                            Values,
                                            Var_name,
                                            Coerced_value
                                        ),
                                        Errs};

                                {error, Msg} ->
                                    {Values,
                                        [{validation_error,
                                                <<<<<<"Variable \"$"/utf8,
                                                            Var_name/binary>>/binary,
                                                        "\": "/utf8>>/binary,
                                                    Msg/binary>>,
                                                [],
                                                none} |
                                            Errs]}
                            end
                    end;

                {{error, _}, {some, Default_ast}} ->
                    Default_value = mochi@input_coercion:coerce_argument_value(
                        Default_ast,
                        ast_type_to_schema_type(Declared_type),
                        erlang:element(2, Context),
                        maps:new(),
                        [<<"$"/utf8, Var_name/binary>>]
                    ),
                    case Default_value of
                        {ok, V} ->
                            {gleam@dict:insert(Values, Var_name, V), Errs};

                        {error, _} ->
                            {Values, Errs}
                    end;

                {{error, _}, none} ->
                    case is_non_null_ast_type(Declared_type) of
                        true ->
                            {Values,
                                [{validation_error,
                                        <<<<"Variable \"$"/utf8,
                                                Var_name/binary>>/binary,
                                            "\" of required type is not provided"/utf8>>,
                                        [],
                                        none} |
                                    Errs]};

                        false ->
                            {gleam@dict:insert(
                                    Values,
                                    Var_name,
                                    gleam_stdlib:identity(nil)
                                ),
                                Errs}
                    end
            end
        end
    ),
    case Errors of
        [] ->
            {ok, Coerced};

        _ ->
            {error, lists:reverse(Errors)}
    end.

-file("src/mochi/executor.gleam", 541).
-spec get_root_type(mochi@schema:schema(), mochi@ast:operation()) -> gleam@option:option(mochi@schema:object_type()).
get_root_type(Schema_def, Operation) ->
    case Operation of
        {operation, 'query', _, _, _, _} ->
            erlang:element(2, Schema_def);

        {operation, mutation, _, _, _, _} ->
            erlang:element(3, Schema_def);

        {operation, subscription, _, _, _, _} ->
            erlang:element(4, Schema_def);

        {shorthand_query, _} ->
            erlang:element(2, Schema_def)
    end.

-file("src/mochi/executor.gleam", 554).
-spec get_selection_set(mochi@ast:operation()) -> mochi@ast:selection_set().
get_selection_set(Operation) ->
    case Operation of
        {operation, _, _, _, _, Ss} ->
            Ss;

        {shorthand_query, Ss@1} ->
            Ss@1
    end.

-file("src/mochi/executor.gleam", 689).
-spec does_type_apply(mochi@schema:schema(), binary(), binary()) -> boolean().
does_type_apply(Schema_def, Object_type_name, Type_condition) ->
    case Object_type_name =:= Type_condition of
        true ->
            true;

        false ->
            Implements_interface = case gleam_stdlib:map_get(
                erlang:element(5, Schema_def),
                Object_type_name
            ) of
                {ok, {object_type_def, Obj}} ->
                    gleam@list:any(
                        erlang:element(5, Obj),
                        fun(Iface) ->
                            erlang:element(2, Iface) =:= Type_condition
                        end
                    );

                _ ->
                    false
            end,
            case Implements_interface of
                true ->
                    true;

                false ->
                    case gleam_stdlib:map_get(
                        erlang:element(5, Schema_def),
                        Type_condition
                    ) of
                        {ok, {union_type_def, Union_type}} ->
                            gleam@list:any(
                                erlang:element(4, Union_type),
                                fun(T) ->
                                    erlang:element(2, T) =:= Object_type_name
                                end
                            );

                        _ ->
                            false
                    end
            end
    end.

-file("src/mochi/executor.gleam", 815).
-spec decode_bool_from_dynamic(gleam@dynamic:dynamic_()) -> gleam@option:option(boolean()).
decode_bool_from_dynamic(Value) ->
    case gleam@dynamic@decode:run(
        Value,
        {decoder, fun gleam@dynamic@decode:decode_bool/1}
    ) of
        {ok, B} ->
            {some, B};

        {error, _} ->
            none
    end.

-file("src/mochi/executor.gleam", 800).
?DOC(" Evaluate a Value to a boolean, handling variables\n").
-spec eval_bool_value(
    mochi@ast:value(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@option:option(boolean()).
eval_bool_value(Value, Variables) ->
    case Value of
        {boolean_value, B} ->
            {some, B};

        {variable_value, Name} ->
            _pipe = gleam_stdlib:map_get(Variables, Name),
            _pipe@1 = gleam@result:map(_pipe, fun decode_bool_from_dynamic/1),
            gleam@result:unwrap(_pipe@1, none);

        _ ->
            none
    end.

-file("src/mochi/executor.gleam", 782).
?DOC(" Get a boolean argument value from a specific directive\n").
-spec get_directive_bool_arg(
    list(mochi@ast:directive()),
    binary(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@option:option(boolean()).
get_directive_bool_arg(Directives, Directive_name, Arg_name, Variables) ->
    _pipe = Directives,
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(D) -> erlang:element(2, D) =:= Directive_name end
    ),
    _pipe@5 = gleam@result:map(
        _pipe@1,
        fun(Directive) -> _pipe@2 = erlang:element(3, Directive),
            _pipe@3 = gleam@list:find(
                _pipe@2,
                fun(Arg) -> erlang:element(2, Arg) =:= Arg_name end
            ),
            _pipe@4 = gleam@result:map(
                _pipe@3,
                fun(Arg@1) ->
                    eval_bool_value(erlang:element(3, Arg@1), Variables)
                end
            ),
            gleam@result:unwrap(_pipe@4, none) end
    ),
    gleam@result:unwrap(_pipe@5, none).

-file("src/mochi/executor.gleam", 766).
?DOC(" Check if a field should be included based on @skip and @include directives\n").
-spec should_include_field(
    list(mochi@ast:directive()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> boolean().
should_include_field(Directives, Variables) ->
    Skip_value = get_directive_bool_arg(
        Directives,
        <<"skip"/utf8>>,
        <<"if"/utf8>>,
        Variables
    ),
    Include_value = get_directive_bool_arg(
        Directives,
        <<"include"/utf8>>,
        <<"if"/utf8>>,
        Variables
    ),
    case {Skip_value, Include_value} of
        {{some, true}, _} ->
            false;

        {_, {some, false}} ->
            false;

        {_, _} ->
            true
    end.

-file("src/mochi/executor.gleam", 870).
?DOC(" Skip built-in directives and call continuation for custom ones\n").
-spec skip_builtin_directive(
    binary(),
    gleam@dynamic:dynamic_(),
    fun(() -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
skip_builtin_directive(Name, Value, Next) ->
    case Name of
        <<"skip"/utf8>> ->
            {ok, Value};

        <<"include"/utf8>> ->
            {ok, Value};

        <<"deprecated"/utf8>> ->
            {ok, Value};

        _ ->
            Next()
    end.

-file("src/mochi/executor.gleam", 882).
?DOC(" Get directive definition or pass through value if not found\n").
-spec with_directive_def(
    mochi@schema:schema(),
    binary(),
    gleam@dynamic:dynamic_(),
    fun((mochi@schema:directive_definition()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
with_directive_def(Schema_def, Name, Value, Next) ->
    case gleam_stdlib:map_get(erlang:element(6, Schema_def), Name) of
        {ok, Directive_def} ->
            Next(Directive_def);

        {error, _} ->
            {ok, Value}
    end.

-file("src/mochi/executor.gleam", 895).
?DOC(" Get directive handler or pass through value if none defined\n").
-spec with_directive_handler(
    mochi@schema:directive_definition(),
    gleam@dynamic:dynamic_(),
    fun((fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
with_directive_handler(Directive_def, Value, Next) ->
    case erlang:element(7, Directive_def) of
        {some, Handler} ->
            Next(Handler);

        none ->
            {ok, Value}
    end.

-file("src/mochi/executor.gleam", 916).
-spec get_list_elements(gleam@dynamic:dynamic_()) -> gleam@option:option(list(gleam@dynamic:dynamic_())).
get_list_elements(Value) ->
    case gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ) of
        {ok, Items} ->
            {some, Items};

        {error, _} ->
            none
    end.

-file("src/mochi/executor.gleam", 923).
-spec is_null(gleam@dynamic:dynamic_()) -> boolean().
is_null(Value) ->
    case gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ) of
        {ok, none} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/executor.gleam", 991).
-spec require_field(
    mochi@schema:object_type(),
    binary(),
    list(binary()),
    gleam@option:option({integer(), integer()}),
    fun((mochi@schema:field_definition()) -> execution_result())
) -> execution_result().
require_field(Object_type, Field_name, Path, Location, Next) ->
    case gleam_stdlib:map_get(erlang:element(4, Object_type), Field_name) of
        {ok, Field_def} ->
            Next(Field_def);

        {error, _} ->
            validation_error_at(
                <<<<<<<<"Field '"/utf8, Field_name/binary>>/binary,
                            "' not found on type '"/utf8>>/binary,
                        (erlang:element(2, Object_type))/binary>>/binary,
                    "'"/utf8>>,
                Path,
                Location
            )
    end.

-file("src/mochi/executor.gleam", 1075).
?DOC(" Emit a SchemaEvent to the telemetry callback if one is configured.\n").
-spec emit_telemetry(
    mochi@schema:execution_context(),
    mochi@schema:schema_event()
) -> nil.
emit_telemetry(Exec_context, Event) ->
    case erlang:element(6, Exec_context) of
        {some, Fn_} ->
            Fn_(Event);

        none ->
            nil
    end.

-file("src/mochi/executor.gleam", 1087).
?DOC(
    " Execute a resolver through the middleware function if one is configured,\n"
    " otherwise call the resolver directly. Emits telemetry field events.\n"
).
-spec execute_resolver_with_middleware(
    mochi@schema:execution_context(),
    mochi@schema:field_definition(),
    mochi@schema:resolver_info(),
    fun((mochi@schema:resolver_info()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()}),
    binary()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
execute_resolver_with_middleware(
    Exec_context,
    Field_def,
    Resolver_info,
    Resolver,
    Parent_type_name
) ->
    Path = [],
    emit_telemetry(
        Exec_context,
        {schema_field_start,
            erlang:element(2, Field_def),
            Parent_type_name,
            Path}
    ),
    Start_ns = mochi_time_ffi:monotonic_time_ns(),
    Result = case erlang:element(4, Exec_context) of
        {some, Mw_fn} ->
            Mw_fn(Parent_type_name, Field_def, Resolver_info, Resolver);

        none ->
            Resolver(Resolver_info)
    end,
    Duration_ns = mochi_time_ffi:monotonic_time_ns() - Start_ns,
    emit_telemetry(
        Exec_context,
        {schema_field_end,
            erlang:element(2, Field_def),
            Parent_type_name,
            Path,
            begin
                _pipe = Result,
                gleam@result:is_ok(_pipe)
            end,
            Duration_ns}
    ),
    Result.

-file("src/mochi/executor.gleam", 1238).
?DOC(" Check if a field type is a list type (unwrapping NonNull)\n").
-spec is_list_field_type(mochi@schema:field_type()) -> boolean().
is_list_field_type(Field_type) ->
    case Field_type of
        {list, _} ->
            true;

        {non_null, Inner} ->
            is_list_field_type(Inner);

        {named, _} ->
            false
    end.

-file("src/mochi/executor.gleam", 1247).
?DOC(" Check if a field type is non-null at the outermost level\n").
-spec is_non_null_type(mochi@schema:field_type()) -> boolean().
is_non_null_type(Field_type) ->
    case Field_type of
        {non_null, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/executor.gleam", 1255).
?DOC(" Get the inner type of a list field type\n").
-spec get_list_inner_type(mochi@schema:field_type()) -> mochi@schema:field_type().
get_list_inner_type(Field_type) ->
    case Field_type of
        {list, Inner} ->
            Inner;

        {non_null, Inner@1} ->
            get_list_inner_type(Inner@1);

        _ ->
            Field_type
    end.

-file("src/mochi/executor.gleam", 1485).
?DOC(" Require a type resolver function exists\n").
-spec require_type_resolver(
    gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, binary()} |
        {error, binary()})),
    list(binary()),
    fun((fun((gleam@dynamic:dynamic_()) -> {ok, binary()} | {error, binary()})) -> execution_result())
) -> execution_result().
require_type_resolver(Resolve_type, Path, Next) ->
    case Resolve_type of
        {some, Resolver} ->
            Next(Resolver);

        none ->
            type_error(
                <<"Abstract type requires a resolve_type function"/utf8>>,
                Path
            )
    end.

-file("src/mochi/executor.gleam", 1497).
?DOC(" Require the type resolver successfully returns a type name\n").
-spec require_resolved_type(
    fun((gleam@dynamic:dynamic_()) -> {ok, binary()} | {error, binary()}),
    gleam@dynamic:dynamic_(),
    list(binary()),
    fun((binary()) -> execution_result())
) -> execution_result().
require_resolved_type(Resolver, Value, Path, Next) ->
    case Resolver(Value) of
        {ok, Type_name} ->
            Next(Type_name);

        {error, Msg} ->
            resolver_error(<<"resolve_type failed: "/utf8, Msg/binary>>, Path)
    end.

-file("src/mochi/executor.gleam", 1510).
?DOC(" Require the type name resolves to an object type in the schema\n").
-spec require_object_type(
    mochi@schema:schema(),
    binary(),
    list(binary()),
    fun((mochi@schema:object_type()) -> execution_result())
) -> execution_result().
require_object_type(Schema_def, Type_name, Path, Next) ->
    case gleam_stdlib:map_get(erlang:element(5, Schema_def), Type_name) of
        {ok, {object_type_def, Concrete_type}} ->
            Next(Concrete_type);

        {ok, _} ->
            type_error(
                <<"resolve_type returned non-object type: "/utf8,
                    Type_name/binary>>,
                Path
            );

        {error, _} ->
            type_error(
                <<"resolve_type returned unknown type: "/utf8,
                    Type_name/binary>>,
                Path
            )
    end.

-file("src/mochi/executor.gleam", 1572).
?DOC(
    " Coerce an AST value to Dynamic without type validation.\n"
    " Used for directive arguments where type checking is less critical.\n"
).
-spec coerce_value_simple(
    mochi@ast:value(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dynamic:dynamic_().
coerce_value_simple(Value, Variables) ->
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
            _pipe = gleam_stdlib:map_get(Variables, Name),
            gleam@result:unwrap(_pipe, gleam_stdlib:identity(nil));

        {list_value, Values} ->
            gleam_stdlib:identity(
                gleam@list:map(
                    Values,
                    fun(_capture) ->
                        coerce_value_simple(_capture, Variables)
                    end
                )
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
                            coerce_value_simple(
                                erlang:element(3, F@1),
                                Variables
                            )
                        )
                    end
                )
            )
    end.

-file("src/mochi/executor.gleam", 907).
?DOC(" Coerce directive arguments from AST to Dynamic values\n").
-spec coerce_directive_arguments(
    list(mochi@ast:argument()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
coerce_directive_arguments(Args, Variables) ->
    gleam@list:fold(
        Args,
        maps:new(),
        fun(Acc, Arg) ->
            gleam@dict:insert(
                Acc,
                erlang:element(2, Arg),
                coerce_value_simple(erlang:element(3, Arg), Variables)
            )
        end
    ).

-file("src/mochi/executor.gleam", 854).
?DOC(
    " Apply a single directive handler to a value.\n"
    " Returns the transformed value or an error.\n"
).
-spec apply_single_directive(
    mochi@schema:schema(),
    mochi@ast:directive(),
    gleam@dynamic:dynamic_(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
apply_single_directive(Schema_def, Directive, Value, Variables, _) ->
    skip_builtin_directive(
        erlang:element(2, Directive),
        Value,
        fun() ->
            with_directive_def(
                Schema_def,
                erlang:element(2, Directive),
                Value,
                fun(Directive_def) ->
                    with_directive_handler(
                        Directive_def,
                        Value,
                        fun(Handler) ->
                            Args = coerce_directive_arguments(
                                erlang:element(3, Directive),
                                Variables
                            ),
                            Handler(Args, Value)
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/executor.gleam", 830).
?DOC(
    " Apply custom directive handlers to a resolved field value.\n"
    " Directives are applied in order (left to right as they appear in the query).\n"
    " Built-in directives (@skip, @include, @deprecated) are skipped as they\n"
    " are handled specially elsewhere.\n"
).
-spec apply_custom_directives(
    mochi@schema:schema(),
    list(mochi@ast:directive()),
    gleam@dynamic:dynamic_(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary())
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
apply_custom_directives(Schema_def, Directives, Value, Variables, Path) ->
    gleam@list:fold(Directives, {ok, Value}, fun(Acc, Directive) -> case Acc of
                {error, Msg} ->
                    {error, Msg};

                {ok, Current_value} ->
                    apply_single_directive(
                        Schema_def,
                        Directive,
                        Current_value,
                        Variables,
                        Path
                    )
            end end).

-file("src/mochi/executor.gleam", 1694).
-spec decode_string_from_dynamic(gleam@dynamic:dynamic_()) -> gleam@option:option(binary()).
decode_string_from_dynamic(Value) ->
    case gleam@dynamic@decode:run(
        Value,
        {decoder, fun gleam@dynamic@decode:decode_string/1}
    ) of
        {ok, S} ->
            {some, S};

        {error, _} ->
            none
    end.

-file("src/mochi/executor.gleam", 1680).
-spec extract_string_value(
    mochi@ast:value(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@option:option(binary()).
extract_string_value(Value, Variables) ->
    case Value of
        {string_value, S} ->
            {some, S};

        {variable_value, Var_name} ->
            _pipe = gleam_stdlib:map_get(Variables, Var_name),
            _pipe@1 = gleam@result:map(_pipe, fun decode_string_from_dynamic/1),
            gleam@result:unwrap(_pipe@1, none);

        _ ->
            none
    end.

-file("src/mochi/executor.gleam", 1670).
-spec get_string_argument(
    list(mochi@ast:argument()),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@option:option(binary()).
get_string_argument(Args, Name, Variables) ->
    _pipe = gleam@list:find(
        Args,
        fun(Arg) -> erlang:element(2, Arg) =:= Name end
    ),
    _pipe@1 = gleam@result:map(
        _pipe,
        fun(Arg@1) ->
            extract_string_value(erlang:element(3, Arg@1), Variables)
        end
    ),
    gleam@result:unwrap(_pipe@1, none).

-file("src/mochi/executor.gleam", 1870).
?DOC(" Helper to create a simple introspection field definition (nullable)\n").
-spec make_introspection_field(binary(), binary()) -> mochi@schema:field_definition().
make_introspection_field(Name, Type_name) ->
    {field_definition,
        Name,
        none,
        {named, Type_name},
        maps:new(),
        none,
        false,
        none,
        none}.

-file("src/mochi/executor.gleam", 1887).
?DOC(" Helper to create a non-null introspection field definition\n").
-spec make_introspection_field_non_null(binary(), binary()) -> mochi@schema:field_definition().
make_introspection_field_non_null(Name, Type_name) ->
    {field_definition,
        Name,
        none,
        {non_null, {named, Type_name}},
        maps:new(),
        none,
        false,
        none,
        none}.

-file("src/mochi/executor.gleam", 1805).
?DOC(" Build the __InputValue introspection ObjectType\n").
-spec get_introspection_input_value_type() -> mochi@schema:object_type().
get_introspection_input_value_type() ->
    {object_type,
        <<"__InputValue"/utf8>>,
        {some, <<"Arguments provided to Fields or Directives."/utf8>>},
        maps:from_list(
            [{<<"name"/utf8>>,
                    make_introspection_field_non_null(
                        <<"name"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"type"/utf8>>,
                    make_introspection_field_non_null(
                        <<"type"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"defaultValue"/utf8>>,
                    make_introspection_field(
                        <<"defaultValue"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"isDeprecated"/utf8>>,
                    make_introspection_field(
                        <<"isDeprecated"/utf8>>,
                        <<"Boolean"/utf8>>
                    )},
                {<<"deprecationReason"/utf8>>,
                    make_introspection_field(
                        <<"deprecationReason"/utf8>>,
                        <<"String"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1825).
?DOC(" Build the __EnumValue introspection ObjectType\n").
-spec get_introspection_enum_value_type() -> mochi@schema:object_type().
get_introspection_enum_value_type() ->
    {object_type,
        <<"__EnumValue"/utf8>>,
        {some, <<"One possible value for a given Enum."/utf8>>},
        maps:from_list(
            [{<<"name"/utf8>>,
                    make_introspection_field_non_null(
                        <<"name"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"isDeprecated"/utf8>>,
                    make_introspection_field_non_null(
                        <<"isDeprecated"/utf8>>,
                        <<"Boolean"/utf8>>
                    )},
                {<<"deprecationReason"/utf8>>,
                    make_introspection_field(
                        <<"deprecationReason"/utf8>>,
                        <<"String"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1904).
?DOC(" Helper to create a nullable list introspection field definition\n").
-spec make_introspection_field_list(binary(), binary()) -> mochi@schema:field_definition().
make_introspection_field_list(Name, Type_name) ->
    {field_definition,
        Name,
        none,
        {list, {non_null, {named, Type_name}}},
        maps:new(),
        none,
        false,
        none,
        none}.

-file("src/mochi/executor.gleam", 1744).
?DOC(" Build the __Type introspection ObjectType\n").
-spec get_introspection_type_type() -> mochi@schema:object_type().
get_introspection_type_type() ->
    {object_type,
        <<"__Type"/utf8>>,
        {some,
            <<"The fundamental unit of any GraphQL Schema is the type."/utf8>>},
        maps:from_list(
            [{<<"kind"/utf8>>,
                    make_introspection_field_non_null(
                        <<"kind"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"name"/utf8>>,
                    make_introspection_field(<<"name"/utf8>>, <<"String"/utf8>>)},
                {<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"specifiedByURL"/utf8>>,
                    make_introspection_field(
                        <<"specifiedByURL"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"fields"/utf8>>,
                    make_introspection_field_list(
                        <<"fields"/utf8>>,
                        <<"__Field"/utf8>>
                    )},
                {<<"interfaces"/utf8>>,
                    make_introspection_field_list(
                        <<"interfaces"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"possibleTypes"/utf8>>,
                    make_introspection_field_list(
                        <<"possibleTypes"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"enumValues"/utf8>>,
                    make_introspection_field_list(
                        <<"enumValues"/utf8>>,
                        <<"__EnumValue"/utf8>>
                    )},
                {<<"inputFields"/utf8>>,
                    make_introspection_field_list(
                        <<"inputFields"/utf8>>,
                        <<"__InputValue"/utf8>>
                    )},
                {<<"ofType"/utf8>>,
                    make_introspection_field(
                        <<"ofType"/utf8>>,
                        <<"__Type"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1921).
?DOC(" Helper to create a non-null list introspection field definition\n").
-spec make_introspection_field_list_non_null(binary(), binary()) -> mochi@schema:field_definition().
make_introspection_field_list_non_null(Name, Type_name) ->
    {field_definition,
        Name,
        none,
        {non_null, {list, {non_null, {named, Type_name}}}},
        maps:new(),
        none,
        false,
        none,
        none}.

-file("src/mochi/executor.gleam", 1719).
?DOC(" Build the __Schema introspection ObjectType\n").
-spec get_introspection_schema_type() -> mochi@schema:object_type().
get_introspection_schema_type() ->
    {object_type,
        <<"__Schema"/utf8>>,
        {some,
            <<"A GraphQL Schema defines the capabilities of a GraphQL server."/utf8>>},
        maps:from_list(
            [{<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"types"/utf8>>,
                    make_introspection_field_list_non_null(
                        <<"types"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"queryType"/utf8>>,
                    make_introspection_field_non_null(
                        <<"queryType"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"mutationType"/utf8>>,
                    make_introspection_field(
                        <<"mutationType"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"subscriptionType"/utf8>>,
                    make_introspection_field(
                        <<"subscriptionType"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"directives"/utf8>>,
                    make_introspection_field_list_non_null(
                        <<"directives"/utf8>>,
                        <<"__Directive"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1780).
?DOC(" Build the __Field introspection ObjectType\n").
-spec get_introspection_field_type() -> mochi@schema:object_type().
get_introspection_field_type() ->
    {object_type,
        <<"__Field"/utf8>>,
        {some,
            <<"Object and Interface types are described by a list of Fields."/utf8>>},
        maps:from_list(
            [{<<"name"/utf8>>,
                    make_introspection_field_non_null(
                        <<"name"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"args"/utf8>>,
                    make_introspection_field_list_non_null(
                        <<"args"/utf8>>,
                        <<"__InputValue"/utf8>>
                    )},
                {<<"type"/utf8>>,
                    make_introspection_field_non_null(
                        <<"type"/utf8>>,
                        <<"__Type"/utf8>>
                    )},
                {<<"isDeprecated"/utf8>>,
                    make_introspection_field_non_null(
                        <<"isDeprecated"/utf8>>,
                        <<"Boolean"/utf8>>
                    )},
                {<<"deprecationReason"/utf8>>,
                    make_introspection_field(
                        <<"deprecationReason"/utf8>>,
                        <<"String"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1846).
?DOC(" Build the __Directive introspection ObjectType\n").
-spec get_introspection_directive_type() -> mochi@schema:object_type().
get_introspection_directive_type() ->
    {object_type,
        <<"__Directive"/utf8>>,
        {some,
            <<"A Directive provides a way to describe alternate runtime execution."/utf8>>},
        maps:from_list(
            [{<<"name"/utf8>>,
                    make_introspection_field_non_null(
                        <<"name"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"description"/utf8>>,
                    make_introspection_field(
                        <<"description"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"isRepeatable"/utf8>>,
                    make_introspection_field_non_null(
                        <<"isRepeatable"/utf8>>,
                        <<"Boolean"/utf8>>
                    )},
                {<<"locations"/utf8>>,
                    make_introspection_field_list_non_null(
                        <<"locations"/utf8>>,
                        <<"String"/utf8>>
                    )},
                {<<"args"/utf8>>,
                    make_introspection_field_list_non_null(
                        <<"args"/utf8>>,
                        <<"__InputValue"/utf8>>
                    )}]
        ),
        []}.

-file("src/mochi/executor.gleam", 1706).
?DOC(" Get introspection ObjectType by name (for __Schema, __Type, __Field, etc.)\n").
-spec get_introspection_object_type(binary()) -> gleam@option:option(mochi@schema:object_type()).
get_introspection_object_type(Name) ->
    case Name of
        <<"__Schema"/utf8>> ->
            {some, get_introspection_schema_type()};

        <<"__Type"/utf8>> ->
            {some, get_introspection_type_type()};

        <<"__Field"/utf8>> ->
            {some, get_introspection_field_type()};

        <<"__InputValue"/utf8>> ->
            {some, get_introspection_input_value_type()};

        <<"__EnumValue"/utf8>> ->
            {some, get_introspection_enum_value_type()};

        <<"__Directive"/utf8>> ->
            {some, get_introspection_directive_type()};

        _ ->
            none
    end.

-file("src/mochi/executor.gleam", 1546).
-spec get_field_type_definition(
    mochi@schema:schema(),
    mochi@schema:field_type()
) -> {ok, mochi@schema:type_definition()} | {error, binary()}.
get_field_type_definition(Schema_def, Field_type) ->
    case Field_type of
        {named, Name} ->
            case get_introspection_object_type(Name) of
                {some, Obj} ->
                    {ok, {object_type_def, Obj}};

                none ->
                    _pipe = gleam_stdlib:map_get(
                        erlang:element(5, Schema_def),
                        Name
                    ),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) ->
                            <<<<"Type '"/utf8, Name/binary>>/binary,
                                "' not found in schema"/utf8>>
                        end
                    )
            end;

        {non_null, Inner} ->
            get_field_type_definition(Schema_def, Inner);

        {list, Inner@1} ->
            get_field_type_definition(Schema_def, Inner@1)
    end.

-file("src/mochi/executor.gleam", 2223).
-spec get_all_type_names(mochi@schema:schema()) -> list(binary()).
get_all_type_names(Schema_def) ->
    Builtin = [<<"String"/utf8>>,
        <<"Int"/utf8>>,
        <<"Float"/utf8>>,
        <<"Boolean"/utf8>>,
        <<"ID"/utf8>>],
    Introspection = [<<"__Schema"/utf8>>,
        <<"__Type"/utf8>>,
        <<"__Field"/utf8>>,
        <<"__InputValue"/utf8>>,
        <<"__EnumValue"/utf8>>,
        <<"__Directive"/utf8>>,
        <<"__DirectiveLocation"/utf8>>,
        <<"__TypeKind"/utf8>>],
    User_types = maps:keys(erlang:element(5, Schema_def)),
    Root_types = begin
        _pipe = [erlang:element(2, Schema_def),
            erlang:element(3, Schema_def),
            erlang:element(4, Schema_def)],
        gleam@list:filter_map(
            _pipe,
            fun(Opt) ->
                _pipe@1 = gleam@option:map(
                    Opt,
                    fun(O) -> erlang:element(2, O) end
                ),
                gleam@option:to_result(_pipe@1, nil)
            end
        )
    end,
    _pipe@2 = lists:append([Builtin, User_types, Root_types, Introspection]),
    gleam@list:unique(_pipe@2).

-file("src/mochi/executor.gleam", 2266).
-spec get_scalar_description(binary()) -> binary().
get_scalar_description(Name) ->
    case Name of
        <<"String"/utf8>> ->
            <<"The String scalar type represents textual data"/utf8>>;

        <<"Int"/utf8>> ->
            <<"The Int scalar type represents non-fractional signed whole numeric values"/utf8>>;

        <<"Float"/utf8>> ->
            <<"The Float scalar type represents signed double-precision fractional values"/utf8>>;

        <<"Boolean"/utf8>> ->
            <<"The Boolean scalar type represents true or false"/utf8>>;

        <<"ID"/utf8>> ->
            <<"The ID scalar type represents a unique identifier"/utf8>>;

        _ ->
            <<""/utf8>>
    end.

-file("src/mochi/executor.gleam", 2430).
-spec build_union_introspection(
    mochi@schema:schema(),
    mochi@schema:union_type()
) -> gleam@dynamic:dynamic_().
build_union_introspection(Schema_def, Union) ->
    Possible = gleam@list:map(
        erlang:element(4, Union),
        fun(T) ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>, gleam_stdlib:identity(<<"OBJECT"/utf8>>)},
                        {<<"name"/utf8>>,
                            gleam_stdlib:identity(erlang:element(2, T))},
                        {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
                )
            )
        end
    ),
    _ = Schema_def,
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"kind"/utf8>>, gleam_stdlib:identity(<<"UNION"/utf8>>)},
                {<<"name"/utf8>>,
                    gleam_stdlib:identity(erlang:element(2, Union))},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@option:unwrap(
                            erlang:element(3, Union),
                            <<""/utf8>>
                        )
                    )},
                {<<"possibleTypes"/utf8>>, gleam_stdlib:identity(Possible)},
                {<<"fields"/utf8>>, gleam_stdlib:identity(nil)},
                {<<"interfaces"/utf8>>, gleam_stdlib:identity(nil)},
                {<<"enumValues"/utf8>>, gleam_stdlib:identity(nil)},
                {<<"inputFields"/utf8>>, gleam_stdlib:identity(nil)},
                {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
        )
    ).

-file("src/mochi/executor.gleam", 2523).
-spec make_type_object(
    binary(),
    binary(),
    binary(),
    gleam@option:option(list(gleam@dynamic:dynamic_())),
    gleam@option:option(list(gleam@dynamic:dynamic_())),
    gleam@option:option(list(gleam@dynamic:dynamic_())),
    gleam@option:option(list(gleam@dynamic:dynamic_())),
    gleam@option:option(list(gleam@dynamic:dynamic_()))
) -> gleam@dynamic:dynamic_().
make_type_object(
    Kind,
    Name,
    Description,
    Fields,
    Interfaces,
    Enum_values,
    Input_fields,
    Possible_types
) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"kind"/utf8>>, gleam_stdlib:identity(Kind)},
                {<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                {<<"description"/utf8>>, gleam_stdlib:identity(Description)},
                {<<"fields"/utf8>>,
                    begin
                        _pipe = gleam@option:map(
                            Fields,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe, gleam_stdlib:identity(nil))
                    end},
                {<<"interfaces"/utf8>>,
                    begin
                        _pipe@1 = gleam@option:map(
                            Interfaces,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@1, gleam_stdlib:identity(nil))
                    end},
                {<<"possibleTypes"/utf8>>,
                    begin
                        _pipe@2 = gleam@option:map(
                            Possible_types,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@2, gleam_stdlib:identity(nil))
                    end},
                {<<"enumValues"/utf8>>,
                    begin
                        _pipe@3 = gleam@option:map(
                            Enum_values,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@3, gleam_stdlib:identity(nil))
                    end},
                {<<"inputFields"/utf8>>,
                    begin
                        _pipe@4 = gleam@option:map(
                            Input_fields,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@4, gleam_stdlib:identity(nil))
                    end},
                {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
        )
    ).

-file("src/mochi/executor.gleam", 2253).
-spec build_scalar_introspection(binary()) -> gleam@dynamic:dynamic_().
build_scalar_introspection(Name) ->
    make_type_object(
        <<"SCALAR"/utf8>>,
        Name,
        get_scalar_description(Name),
        none,
        none,
        none,
        none,
        none
    ).

-file("src/mochi/executor.gleam", 2355).
-spec build_enum_introspection_filtered(mochi@schema:enum_type(), boolean()) -> gleam@dynamic:dynamic_().
build_enum_introspection_filtered(Enum, Include_deprecated) ->
    Values = begin
        _pipe = maps:to_list(erlang:element(4, Enum)),
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(Kv) ->
                {_, Def} = Kv,
                Include_deprecated orelse not erlang:element(5, Def)
            end
        ),
        gleam@list:map(
            _pipe@1,
            fun(Kv@1) ->
                {Name, Def@1} = Kv@1,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                            {<<"description"/utf8>>,
                                gleam_stdlib:identity(
                                    gleam@option:unwrap(
                                        erlang:element(3, Def@1),
                                        <<""/utf8>>
                                    )
                                )},
                            {<<"isDeprecated"/utf8>>,
                                gleam_stdlib:identity(erlang:element(5, Def@1))},
                            {<<"deprecationReason"/utf8>>,
                                case erlang:element(6, Def@1) of
                                    {some, Reason} ->
                                        gleam_stdlib:identity(Reason);

                                    none ->
                                        gleam_stdlib:identity(nil)
                                end}]
                    )
                )
            end
        )
    end,
    make_type_object(
        <<"ENUM"/utf8>>,
        erlang:element(2, Enum),
        gleam@option:unwrap(erlang:element(3, Enum), <<""/utf8>>),
        none,
        none,
        {some, Values},
        none,
        none
    ).

-file("src/mochi/executor.gleam", 2351).
-spec build_enum_introspection(mochi@schema:enum_type()) -> gleam@dynamic:dynamic_().
build_enum_introspection(Enum) ->
    build_enum_introspection_filtered(Enum, true).

-file("src/mochi/executor.gleam", 2490).
-spec build_meta_type_introspection(binary()) -> gleam@dynamic:dynamic_().
build_meta_type_introspection(Name) ->
    case Name of
        <<"__Schema"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__Type"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__Field"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__InputValue"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__EnumValue"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__Directive"/utf8>> ->
            make_type_object(
                <<"OBJECT"/utf8>>,
                Name,
                <<"Introspection type"/utf8>>,
                {some, []},
                {some, []},
                none,
                none,
                none
            );

        <<"__TypeKind"/utf8>> ->
            make_type_object(
                <<"ENUM"/utf8>>,
                Name,
                <<"Introspection enum"/utf8>>,
                none,
                none,
                {some, []},
                none,
                none
            );

        <<"__DirectiveLocation"/utf8>> ->
            make_type_object(
                <<"ENUM"/utf8>>,
                Name,
                <<"Introspection enum"/utf8>>,
                none,
                none,
                {some, []},
                none,
                none
            );

        _ ->
            gleam_stdlib:identity(nil)
    end.

-file("src/mochi/executor.gleam", 2622).
-spec serialize_default_value(gleam@option:option(gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
serialize_default_value(Default_value) ->
    case Default_value of
        none ->
            gleam_stdlib:identity(nil);

        {some, V} ->
            case gleam@dynamic@decode:run(
                V,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, S} ->
                    gleam_stdlib:identity(S);

                {error, _} ->
                    case gleam@dynamic@decode:run(
                        V,
                        {decoder, fun gleam@dynamic@decode:decode_int/1}
                    ) of
                        {ok, I} ->
                            gleam_stdlib:identity(erlang:integer_to_binary(I));

                        {error, _} ->
                            case gleam@dynamic@decode:run(
                                V,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_bool/1}
                            ) of
                                {ok, true} ->
                                    gleam_stdlib:identity(<<"true"/utf8>>);

                                {ok, false} ->
                                    gleam_stdlib:identity(<<"false"/utf8>>);

                                {error, _} ->
                                    gleam_stdlib:identity(nil)
                            end
                    end
            end
    end.

-file("src/mochi/executor.gleam", 2674).
-spec get_type_kind(mochi@schema:schema(), binary()) -> binary().
get_type_kind(Schema_def, Name) ->
    case Name of
        <<"String"/utf8>> ->
            <<"SCALAR"/utf8>>;

        <<"Int"/utf8>> ->
            <<"SCALAR"/utf8>>;

        <<"Float"/utf8>> ->
            <<"SCALAR"/utf8>>;

        <<"Boolean"/utf8>> ->
            <<"SCALAR"/utf8>>;

        <<"ID"/utf8>> ->
            <<"SCALAR"/utf8>>;

        _ ->
            case gleam_stdlib:map_get(erlang:element(5, Schema_def), Name) of
                {ok, {object_type_def, _}} ->
                    <<"OBJECT"/utf8>>;

                {ok, {scalar_type_def, _}} ->
                    <<"SCALAR"/utf8>>;

                {ok, {enum_type_def, _}} ->
                    <<"ENUM"/utf8>>;

                {ok, {interface_type_def, _}} ->
                    <<"INTERFACE"/utf8>>;

                {ok, {union_type_def, _}} ->
                    <<"UNION"/utf8>>;

                {ok, {input_object_type_def, _}} ->
                    <<"INPUT_OBJECT"/utf8>>;

                {error, _} ->
                    case begin
                        _pipe = [erlang:element(2, Schema_def),
                            erlang:element(3, Schema_def),
                            erlang:element(4, Schema_def)],
                        gleam@list:find(
                            _pipe,
                            fun(Opt) ->
                                _pipe@1 = gleam@option:map(
                                    Opt,
                                    fun(O) -> erlang:element(2, O) =:= Name end
                                ),
                                gleam@option:unwrap(_pipe@1, false)
                            end
                        )
                    end of
                        {ok, _} ->
                            <<"OBJECT"/utf8>>;

                        {error, _} ->
                            <<"OBJECT"/utf8>>
                    end
            end
    end.

-file("src/mochi/executor.gleam", 2642).
-spec build_field_type_introspection(
    mochi@schema:schema(),
    mochi@schema:field_type()
) -> gleam@dynamic:dynamic_().
build_field_type_introspection(Schema_def, Field_type) ->
    case Field_type of
        {non_null, Inner} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(<<"NON_NULL"/utf8>>)},
                        {<<"name"/utf8>>, gleam_stdlib:identity(nil)},
                        {<<"ofType"/utf8>>,
                            build_field_type_introspection(Schema_def, Inner)}]
                )
            );

        {list, Inner@1} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>, gleam_stdlib:identity(<<"LIST"/utf8>>)},
                        {<<"name"/utf8>>, gleam_stdlib:identity(nil)},
                        {<<"ofType"/utf8>>,
                            build_field_type_introspection(Schema_def, Inner@1)}]
                )
            );

        {named, Name} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(
                                get_type_kind(Schema_def, Name)
                            )},
                        {<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                        {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
                )
            )
    end.

-file("src/mochi/executor.gleam", 1989).
?DOC(" Build introspection for a single directive\n").
-spec build_directive_introspection(
    mochi@schema:schema(),
    mochi@schema:directive_definition()
) -> gleam@dynamic:dynamic_().
build_directive_introspection(Schema_def, Directive) ->
    Args = begin
        _pipe = erlang:element(4, Directive),
        _pipe@1 = maps:to_list(_pipe),
        gleam@list:map(
            _pipe@1,
            fun(Kv) ->
                {Arg_name, Arg_def} = Kv,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"name"/utf8>>, gleam_stdlib:identity(Arg_name)},
                            {<<"description"/utf8>>,
                                gleam_stdlib:identity(
                                    gleam@option:unwrap(
                                        erlang:element(3, Arg_def),
                                        <<""/utf8>>
                                    )
                                )},
                            {<<"type"/utf8>>,
                                build_field_type_introspection(
                                    Schema_def,
                                    erlang:element(4, Arg_def)
                                )},
                            {<<"defaultValue"/utf8>>,
                                gleam_stdlib:identity(nil)}]
                    )
                )
            end
        )
    end,
    Locations = begin
        _pipe@2 = erlang:element(5, Directive),
        gleam@list:map(
            _pipe@2,
            fun(Loc) ->
                gleam_stdlib:identity(
                    mochi@schema:directive_location_to_string(Loc)
                )
            end
        )
    end,
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"name"/utf8>>,
                    gleam_stdlib:identity(erlang:element(2, Directive))},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@option:unwrap(
                            erlang:element(3, Directive),
                            <<""/utf8>>
                        )
                    )},
                {<<"locations"/utf8>>, gleam_stdlib:identity(Locations)},
                {<<"args"/utf8>>, gleam_stdlib:identity(Args)},
                {<<"isRepeatable"/utf8>>,
                    gleam_stdlib:identity(erlang:element(6, Directive))}]
        )
    ).

-file("src/mochi/executor.gleam", 2035).
?DOC(" @skip directive - conditionally skip a field\n").
-spec build_skip_directive_introspection(mochi@schema:schema()) -> gleam@dynamic:dynamic_().
build_skip_directive_introspection(Schema_def) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"name"/utf8>>, gleam_stdlib:identity(<<"skip"/utf8>>)},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        <<"Directs the executor to skip this field or fragment when the `if` argument is true."/utf8>>
                    )},
                {<<"locations"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(<<"FIELD"/utf8>>),
                            gleam_stdlib:identity(<<"FRAGMENT_SPREAD"/utf8>>),
                            gleam_stdlib:identity(<<"INLINE_FRAGMENT"/utf8>>)]
                    )},
                {<<"args"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"name"/utf8>>,
                                            gleam_stdlib:identity(<<"if"/utf8>>)},
                                        {<<"description"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"Skipped when true."/utf8>>
                                            )},
                                        {<<"type"/utf8>>,
                                            build_field_type_introspection(
                                                Schema_def,
                                                {non_null,
                                                    {named, <<"Boolean"/utf8>>}}
                                            )},
                                        {<<"defaultValue"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )]
                    )},
                {<<"isRepeatable"/utf8>>, gleam_stdlib:identity(false)}]
        )
    ).

-file("src/mochi/executor.gleam", 2078).
?DOC(" @include directive - conditionally include a field\n").
-spec build_include_directive_introspection(mochi@schema:schema()) -> gleam@dynamic:dynamic_().
build_include_directive_introspection(Schema_def) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"name"/utf8>>, gleam_stdlib:identity(<<"include"/utf8>>)},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        <<"Directs the executor to include this field or fragment only when the `if` argument is true."/utf8>>
                    )},
                {<<"locations"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(<<"FIELD"/utf8>>),
                            gleam_stdlib:identity(<<"FRAGMENT_SPREAD"/utf8>>),
                            gleam_stdlib:identity(<<"INLINE_FRAGMENT"/utf8>>)]
                    )},
                {<<"args"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"name"/utf8>>,
                                            gleam_stdlib:identity(<<"if"/utf8>>)},
                                        {<<"description"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"Included when true."/utf8>>
                                            )},
                                        {<<"type"/utf8>>,
                                            build_field_type_introspection(
                                                Schema_def,
                                                {non_null,
                                                    {named, <<"Boolean"/utf8>>}}
                                            )},
                                        {<<"defaultValue"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )]
                    )},
                {<<"isRepeatable"/utf8>>, gleam_stdlib:identity(false)}]
        )
    ).

-file("src/mochi/executor.gleam", 2121).
?DOC(" @deprecated directive - marks a field or enum value as deprecated\n").
-spec build_deprecated_directive_introspection(mochi@schema:schema()) -> gleam@dynamic:dynamic_().
build_deprecated_directive_introspection(Schema_def) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"name"/utf8>>, gleam_stdlib:identity(<<"deprecated"/utf8>>)},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        <<"Marks an element of a GraphQL schema as no longer supported."/utf8>>
                    )},
                {<<"locations"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(<<"FIELD_DEFINITION"/utf8>>),
                            gleam_stdlib:identity(
                                <<"ARGUMENT_DEFINITION"/utf8>>
                            ),
                            gleam_stdlib:identity(
                                <<"INPUT_FIELD_DEFINITION"/utf8>>
                            ),
                            gleam_stdlib:identity(<<"ENUM_VALUE"/utf8>>)]
                    )},
                {<<"args"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"name"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"reason"/utf8>>
                                            )},
                                        {<<"description"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/)."/utf8>>
                                            )},
                                        {<<"type"/utf8>>,
                                            build_field_type_introspection(
                                                Schema_def,
                                                {named, <<"String"/utf8>>}
                                            )},
                                        {<<"defaultValue"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"\"No longer supported\""/utf8>>
                                            )}]
                                )
                            )]
                    )},
                {<<"isRepeatable"/utf8>>, gleam_stdlib:identity(false)}]
        )
    ).

-file("src/mochi/executor.gleam", 2172).
?DOC(" @specifiedBy directive - provides a URL for a custom scalar specification\n").
-spec build_specified_by_directive_introspection(mochi@schema:schema()) -> gleam@dynamic:dynamic_().
build_specified_by_directive_introspection(Schema_def) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"name"/utf8>>, gleam_stdlib:identity(<<"specifiedBy"/utf8>>)},
                {<<"description"/utf8>>,
                    gleam_stdlib:identity(
                        <<"Exposes a URL that specifies the behavior of this scalar."/utf8>>
                    )},
                {<<"locations"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(<<"SCALAR"/utf8>>)]
                    )},
                {<<"args"/utf8>>,
                    gleam_stdlib:identity(
                        [gleam_stdlib:identity(
                                maps:from_list(
                                    [{<<"name"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"url"/utf8>>
                                            )},
                                        {<<"description"/utf8>>,
                                            gleam_stdlib:identity(
                                                <<"The URL that specifies the behavior of this scalar."/utf8>>
                                            )},
                                        {<<"type"/utf8>>,
                                            build_field_type_introspection(
                                                Schema_def,
                                                {non_null,
                                                    {named, <<"String"/utf8>>}}
                                            )},
                                        {<<"defaultValue"/utf8>>,
                                            gleam_stdlib:identity(nil)}]
                                )
                            )]
                    )},
                {<<"isRepeatable"/utf8>>, gleam_stdlib:identity(false)}]
        )
    ).

-file("src/mochi/executor.gleam", 1967).
?DOC(" Build introspection data for all directives (built-in + custom)\n").
-spec build_directives_introspection(mochi@schema:schema()) -> gleam@dynamic:dynamic_().
build_directives_introspection(Schema_def) ->
    Builtin_directives = [build_skip_directive_introspection(Schema_def),
        build_include_directive_introspection(Schema_def),
        build_deprecated_directive_introspection(Schema_def),
        build_specified_by_directive_introspection(Schema_def)],
    Custom_directives = begin
        _pipe = erlang:element(6, Schema_def),
        _pipe@1 = maps:to_list(_pipe),
        gleam@list:map(
            _pipe@1,
            fun(Kv) ->
                {_, Directive_def} = Kv,
                build_directive_introspection(Schema_def, Directive_def)
            end
        )
    end,
    gleam_stdlib:identity(lists:append(Builtin_directives, Custom_directives)).

-file("src/mochi/executor.gleam", 2461).
-spec build_input_introspection(
    mochi@schema:schema(),
    mochi@schema:input_object_type()
) -> gleam@dynamic:dynamic_().
build_input_introspection(Schema_def, Input) ->
    Fields = begin
        _pipe = maps:to_list(erlang:element(4, Input)),
        gleam@list:map(
            _pipe,
            fun(Kv) ->
                {Name, Def} = Kv,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                            {<<"description"/utf8>>,
                                gleam_stdlib:identity(
                                    gleam@option:unwrap(
                                        erlang:element(3, Def),
                                        <<""/utf8>>
                                    )
                                )},
                            {<<"type"/utf8>>,
                                build_field_type_introspection(
                                    Schema_def,
                                    erlang:element(4, Def)
                                )},
                            {<<"defaultValue"/utf8>>,
                                serialize_default_value(erlang:element(5, Def))}]
                    )
                )
            end
        )
    end,
    make_type_object(
        <<"INPUT_OBJECT"/utf8>>,
        erlang:element(2, Input),
        gleam@option:unwrap(erlang:element(3, Input), <<""/utf8>>),
        none,
        none,
        none,
        {some, Fields},
        none
    ).

-file("src/mochi/executor.gleam", 2575).
-spec build_fields_introspection_filtered(
    mochi@schema:schema(),
    gleam@dict:dict(binary(), mochi@schema:field_definition()),
    boolean()
) -> list(gleam@dynamic:dynamic_()).
build_fields_introspection_filtered(Schema_def, Fields, Include_deprecated) ->
    _pipe = maps:to_list(Fields),
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Kv) ->
            {_, Def} = Kv,
            Include_deprecated orelse not erlang:element(7, Def)
        end
    ),
    gleam@list:map(
        _pipe@1,
        fun(Kv@1) ->
            {Name, Def@1} = Kv@1,
            Args = begin
                _pipe@2 = maps:to_list(erlang:element(5, Def@1)),
                gleam@list:map(
                    _pipe@2,
                    fun(Arg_kv) ->
                        {Arg_name, Arg_def} = Arg_kv,
                        gleam_stdlib:identity(
                            maps:from_list(
                                [{<<"name"/utf8>>,
                                        gleam_stdlib:identity(Arg_name)},
                                    {<<"description"/utf8>>,
                                        gleam_stdlib:identity(
                                            gleam@option:unwrap(
                                                erlang:element(3, Arg_def),
                                                <<""/utf8>>
                                            )
                                        )},
                                    {<<"type"/utf8>>,
                                        build_field_type_introspection(
                                            Schema_def,
                                            erlang:element(4, Arg_def)
                                        )},
                                    {<<"defaultValue"/utf8>>,
                                        serialize_default_value(
                                            erlang:element(5, Arg_def)
                                        )}]
                            )
                        )
                    end
                )
            end,
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                        {<<"description"/utf8>>,
                            gleam_stdlib:identity(
                                gleam@option:unwrap(
                                    erlang:element(3, Def@1),
                                    <<""/utf8>>
                                )
                            )},
                        {<<"args"/utf8>>, gleam_stdlib:identity(Args)},
                        {<<"type"/utf8>>,
                            build_field_type_introspection(
                                Schema_def,
                                erlang:element(4, Def@1)
                            )},
                        {<<"isDeprecated"/utf8>>,
                            gleam_stdlib:identity(erlang:element(7, Def@1))},
                        {<<"deprecationReason"/utf8>>,
                            case erlang:element(8, Def@1) of
                                {some, Reason} ->
                                    gleam_stdlib:identity(Reason);

                                none ->
                                    gleam_stdlib:identity(nil)
                            end}]
                )
            )
        end
    ).

-file("src/mochi/executor.gleam", 2568).
-spec build_fields_introspection(
    mochi@schema:schema(),
    gleam@dict:dict(binary(), mochi@schema:field_definition())
) -> list(gleam@dynamic:dynamic_()).
build_fields_introspection(Schema_def, Fields) ->
    build_fields_introspection_filtered(Schema_def, Fields, true).

-file("src/mochi/executor.gleam", 2324).
-spec build_object_introspection(
    mochi@schema:schema(),
    mochi@schema:object_type()
) -> gleam@dynamic:dynamic_().
build_object_introspection(Schema_def, Obj) ->
    Fields = build_fields_introspection(Schema_def, erlang:element(4, Obj)),
    Interfaces = gleam@list:map(
        erlang:element(5, Obj),
        fun(I) ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(<<"INTERFACE"/utf8>>)},
                        {<<"name"/utf8>>,
                            gleam_stdlib:identity(erlang:element(2, I))},
                        {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
                )
            )
        end
    ),
    make_type_object(
        <<"OBJECT"/utf8>>,
        erlang:element(2, Obj),
        gleam@option:unwrap(erlang:element(3, Obj), <<""/utf8>>),
        {some, Fields},
        {some, Interfaces},
        none,
        none,
        none
    ).

-file("src/mochi/executor.gleam", 2214).
-spec build_type_ref(
    mochi@schema:schema(),
    gleam@option:option(mochi@schema:object_type())
) -> gleam@dynamic:dynamic_().
build_type_ref(Schema_def, Obj) ->
    _pipe = Obj,
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(O) -> build_object_introspection(Schema_def, O) end
    ),
    gleam@option:unwrap(_pipe@1, gleam_stdlib:identity(nil)).

-file("src/mochi/executor.gleam", 2286).
-spec lookup_root_type(mochi@schema:schema(), binary()) -> gleam@dynamic:dynamic_().
lookup_root_type(Schema_def, Name) ->
    _pipe = [erlang:element(2, Schema_def),
        erlang:element(3, Schema_def),
        erlang:element(4, Schema_def)],
    _pipe@2 = gleam@list:find(
        _pipe,
        fun(Opt) ->
            _pipe@1 = gleam@option:map(
                Opt,
                fun(O) -> erlang:element(2, O) =:= Name end
            ),
            gleam@option:unwrap(_pipe@1, false)
        end
    ),
    _pipe@4 = gleam@result:map(
        _pipe@2,
        fun(Opt@1) ->
            _pipe@3 = gleam@option:map(
                Opt@1,
                fun(_capture) ->
                    build_object_introspection(Schema_def, _capture)
                end
            ),
            gleam@option:unwrap(_pipe@3, gleam_stdlib:identity(nil))
        end
    ),
    gleam@result:unwrap(_pipe@4, build_meta_type_introspection(Name)).

-file("src/mochi/executor.gleam", 2391).
-spec build_interface_introspection(
    mochi@schema:schema(),
    mochi@schema:interface_type()
) -> gleam@dynamic:dynamic_().
build_interface_introspection(Schema_def, Iface) ->
    Fields = build_fields_introspection(Schema_def, erlang:element(4, Iface)),
    Possible_types = begin
        _pipe = maps:values(erlang:element(5, Schema_def)),
        gleam@list:filter_map(_pipe, fun(Type_def) -> case Type_def of
                    {object_type_def, Obj} ->
                        case gleam@list:any(
                            erlang:element(5, Obj),
                            fun(I) ->
                                erlang:element(2, I) =:= erlang:element(
                                    2,
                                    Iface
                                )
                            end
                        ) of
                            true ->
                                {ok,
                                    gleam_stdlib:identity(
                                        maps:from_list(
                                            [{<<"kind"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        <<"OBJECT"/utf8>>
                                                    )},
                                                {<<"name"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(2, Obj)
                                                    )},
                                                {<<"ofType"/utf8>>,
                                                    gleam_stdlib:identity(nil)}]
                                        )
                                    )};

                            false ->
                                {error, nil}
                        end;

                    _ ->
                        {error, nil}
                end end)
    end,
    make_type_object(
        <<"INTERFACE"/utf8>>,
        erlang:element(2, Iface),
        gleam@option:unwrap(erlang:element(3, Iface), <<""/utf8>>),
        {some, Fields},
        none,
        none,
        none,
        {some, Possible_types}
    ).

-file("src/mochi/executor.gleam", 2298).
-spec build_type_def_introspection(
    mochi@schema:schema(),
    mochi@schema:type_definition()
) -> gleam@dynamic:dynamic_().
build_type_def_introspection(Schema_def, Type_def) ->
    case Type_def of
        {object_type_def, Obj} ->
            build_object_introspection(Schema_def, Obj);

        {scalar_type_def, Scalar} ->
            make_type_object(
                <<"SCALAR"/utf8>>,
                erlang:element(2, Scalar),
                gleam@option:unwrap(erlang:element(3, Scalar), <<""/utf8>>),
                none,
                none,
                none,
                none,
                none
            );

        {enum_type_def, Enum} ->
            build_enum_introspection(Enum);

        {interface_type_def, Iface} ->
            build_interface_introspection(Schema_def, Iface);

        {union_type_def, Union} ->
            build_union_introspection(Schema_def, Union);

        {input_object_type_def, Input} ->
            build_input_introspection(Schema_def, Input)
    end.

-file("src/mochi/executor.gleam", 2279).
-spec lookup_type_introspection(mochi@schema:schema(), binary()) -> gleam@dynamic:dynamic_().
lookup_type_introspection(Schema_def, Name) ->
    case gleam_stdlib:map_get(erlang:element(5, Schema_def), Name) of
        {ok, Type_def} ->
            build_type_def_introspection(Schema_def, Type_def);

        {error, _} ->
            lookup_root_type(Schema_def, Name)
    end.

-file("src/mochi/executor.gleam", 2245).
-spec build_type_introspection(mochi@schema:schema(), binary()) -> gleam@dynamic:dynamic_().
build_type_introspection(Schema_def, Name) ->
    case Name of
        <<"String"/utf8>> ->
            build_scalar_introspection(Name);

        <<"Int"/utf8>> ->
            build_scalar_introspection(Name);

        <<"Float"/utf8>> ->
            build_scalar_introspection(Name);

        <<"Boolean"/utf8>> ->
            build_scalar_introspection(Name);

        <<"ID"/utf8>> ->
            build_scalar_introspection(Name);

        _ ->
            lookup_type_introspection(Schema_def, Name)
    end.

-file("src/mochi/executor.gleam", 1943).
-spec build_schema_introspection(mochi@schema:schema(), mochi@ast:field()) -> gleam@dynamic:dynamic_().
build_schema_introspection(Schema_def, _) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"queryType"/utf8>>,
                    build_type_ref(Schema_def, erlang:element(2, Schema_def))},
                {<<"mutationType"/utf8>>,
                    build_type_ref(Schema_def, erlang:element(3, Schema_def))},
                {<<"subscriptionType"/utf8>>,
                    build_type_ref(Schema_def, erlang:element(4, Schema_def))},
                {<<"types"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@list:map(
                            get_all_type_names(Schema_def),
                            fun(_capture) ->
                                build_type_introspection(Schema_def, _capture)
                            end
                        )
                    )},
                {<<"directives"/utf8>>,
                    build_directives_introspection(Schema_def)}]
        )
    ).

-file("src/mochi/executor.gleam", 2704).
-spec make_field(binary(), gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_().
make_field(Name, Value) ->
    gleam_stdlib:identity(maps:from_list([{Name, Value}])).

-file("src/mochi/executor.gleam", 1160).
?DOC(" Check for null values and handle non-null field constraints\n").
-spec with_non_null_check(
    gleam@dynamic:dynamic_(),
    mochi@schema:field_type(),
    binary(),
    list(binary()),
    gleam@option:option({integer(), integer()}),
    fun((gleam@dynamic:dynamic_()) -> execution_result())
) -> execution_result().
with_non_null_check(
    Resolved,
    Field_type,
    Response_name,
    Field_path,
    Field_location,
    Next
) ->
    case {is_null(Resolved), is_non_null_type(Field_type)} of
        {true, true} ->
            null_value_error_at(
                <<<<"Cannot return null for non-null field '"/utf8,
                        Response_name/binary>>/binary,
                    "'"/utf8>>,
                Field_path,
                Field_location
            );

        {true, false} ->
            ok_result(make_field(Response_name, gleam_stdlib:identity(nil)));

        {false, _} ->
            Next(Resolved)
    end.

-file("src/mochi/executor.gleam", 1389).
?DOC(" Handle null errors in list results based on nullability\n").
-spec handle_list_null_error(
    mochi@schema:field_type(),
    binary(),
    list(execution_error())
) -> execution_result().
handle_list_null_error(Field_type, Response_name, Errors) ->
    case is_non_null_type(Field_type) of
        true ->
            {execution_result, none, Errors};

        false ->
            {execution_result,
                {some, make_field(Response_name, gleam_stdlib:identity(nil))},
                Errors}
    end.

-file("src/mochi/executor.gleam", 1349).
?DOC(" Aggregate results from list item executions\n").
-spec aggregate_list_results(
    list(execution_result()),
    mochi@schema:field_type(),
    binary()
) -> execution_result().
aggregate_list_results(Results, Field_type, Response_name) ->
    {Data_acc, Errors_acc, Has_null_error} = gleam@list:fold(
        Results,
        {[], [], false},
        fun(Acc, Result) ->
            {Data_list, Error_list, Null_found} = Acc,
            New_data = case erlang:element(2, Result) of
                {some, D} ->
                    [D | Data_list];

                none ->
                    Data_list
            end,
            New_null = Null_found orelse gleam@list:any(
                erlang:element(3, Result),
                fun(E) -> case E of
                        {null_value_error, _, _, _} ->
                            true;

                        _ ->
                            false
                    end end
            ),
            {New_data, [erlang:element(3, Result) | Error_list], New_null}
        end
    ),
    Errors = begin
        _pipe = lists:reverse(Errors_acc),
        lists:append(_pipe)
    end,
    Data_list@1 = lists:reverse(Data_acc),
    case {Has_null_error, Errors} of
        {true, _} ->
            handle_list_null_error(Field_type, Response_name, Errors);

        {false, []} ->
            ok_result(
                make_field(Response_name, gleam_stdlib:identity(Data_list@1))
            );

        {false, _} ->
            {execution_result,
                {some,
                    make_field(
                        Response_name,
                        gleam_stdlib:identity(Data_list@1)
                    )},
                Errors}
    end.

-file("src/mochi/executor.gleam", 1405).
?DOC(" Wrap an execution result's data in a field\n").
-spec wrap_result_in_field(execution_result(), binary()) -> execution_result().
wrap_result_in_field(Result, Name) ->
    case erlang:element(2, Result) of
        {some, Data} ->
            {execution_result,
                {some, make_field(Name, Data)},
                erlang:element(3, Result)};

        none ->
            Result
    end.

-file("src/mochi/executor.gleam", 1221).
?DOC(" Handle null propagation from sub-selection results\n").
-spec handle_sub_selection_result(
    execution_result(),
    mochi@schema:field_type(),
    binary()
) -> execution_result().
handle_sub_selection_result(Sub_result, Field_type, Response_name) ->
    case {erlang:element(2, Sub_result), is_non_null_type(Field_type)} of
        {{some, _}, _} ->
            wrap_result_in_field(Sub_result, Response_name);

        {none, true} ->
            Sub_result;

        {none, false} ->
            {execution_result,
                {some, make_field(Response_name, gleam_stdlib:identity(nil))},
                erlang:element(3, Sub_result)}
    end.

-file("src/mochi/executor.gleam", 1525).
-spec resolve_from_parent(
    gleam@option:option(gleam@dynamic:dynamic_()),
    binary(),
    binary(),
    list(binary())
) -> execution_result().
resolve_from_parent(Parent, Field_name, Response_name, Field_path) ->
    _pipe = Parent,
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(P) ->
            case gleam@dynamic@decode:run(
                P,
                gleam@dynamic@decode:dict(
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                )
            ) of
                {ok, D} ->
                    case gleam_stdlib:map_get(D, Field_name) of
                        {ok, Value} ->
                            ok_result(make_field(Response_name, Value));

                        {error, _} ->
                            ok_result(
                                make_field(
                                    Response_name,
                                    gleam_stdlib:identity(nil)
                                )
                            )
                    end;

                {error, _} ->
                    ok_result(
                        make_field(Response_name, gleam_stdlib:identity(nil))
                    )
            end
        end
    ),
    gleam@option:unwrap(
        _pipe@1,
        resolver_error(<<"No resolver and no parent value"/utf8>>, Field_path)
    ).

-file("src/mochi/executor.gleam", 2751).
-spec get_cached(mochi@schema:schema(), binary()) -> {ok, mochi@ast:document()} |
    {error, nil}.
get_cached(Schema_def, Query) ->
    case erlang:element(7, Schema_def) of
        none ->
            {error, nil};

        {some, Cache} ->
            mochi@document_cache:get(Cache, Query)
    end.

-file("src/mochi/executor.gleam", 2761).
-spec cache_put(mochi@schema:schema(), binary(), mochi@ast:document()) -> nil.
cache_put(Schema_def, Query, Doc) ->
    case erlang:element(7, Schema_def) of
        none ->
            nil;

        {some, Cache} ->
            mochi@document_cache:put(Cache, Query, Doc)
    end.

-file("src/mochi/executor.gleam", 2737).
-spec get_or_parse(mochi@schema:schema(), binary()) -> {ok,
        mochi@ast:document()} |
    {error, mochi@parser:parse_error()}.
get_or_parse(Schema_def, Query) ->
    case get_cached(Schema_def, Query) of
        {ok, Doc} ->
            {ok, Doc};

        {error, _} ->
            gleam@result:map(
                mochi@parser:parse(Query),
                fun(Doc@1) ->
                    cache_put(Schema_def, Query, Doc@1),
                    Doc@1
                end
            )
    end.

-file("src/mochi/executor.gleam", 2921).
-spec log_step(debug_context(), binary()) -> nil.
log_step(Debug, Msg) ->
    case erlang:element(2, Debug) of
        true ->
            gleam_stdlib:println(
                <<<<(gleam@string:repeat(
                            <<"  "/utf8>>,
                            erlang:element(3, Debug)
                        ))/binary,
                        "📋 "/utf8>>/binary,
                    Msg/binary>>
            );

        false ->
            nil
    end.

-file("src/mochi/executor.gleam", 2928).
-spec log_info(debug_context(), binary()) -> nil.
log_info(Debug, Msg) ->
    case erlang:element(2, Debug) of
        true ->
            gleam_stdlib:println(
                <<<<(gleam@string:repeat(
                            <<"  "/utf8>>,
                            erlang:element(3, Debug)
                        ))/binary,
                        "🔍 "/utf8>>/binary,
                    Msg/binary>>
            );

        false ->
            nil
    end.

-file("src/mochi/executor.gleam", 2935).
-spec log_success(debug_context(), binary()) -> nil.
log_success(Debug, Msg) ->
    case erlang:element(2, Debug) of
        true ->
            gleam_stdlib:println(
                <<<<(gleam@string:repeat(
                            <<"  "/utf8>>,
                            erlang:element(3, Debug)
                        ))/binary,
                        "✅ "/utf8>>/binary,
                    Msg/binary>>
            );

        false ->
            nil
    end.

-file("src/mochi/executor.gleam", 2942).
-spec log_error(debug_context(), binary()) -> nil.
log_error(Debug, Msg) ->
    case erlang:element(2, Debug) of
        true ->
            gleam_stdlib:println(
                <<<<(gleam@string:repeat(
                            <<"  "/utf8>>,
                            erlang:element(3, Debug)
                        ))/binary,
                        "❌ "/utf8>>/binary,
                    Msg/binary>>
            );

        false ->
            nil
    end.

-file("src/mochi/executor.gleam", 2949).
-spec format_token(mochi@lexer:token()) -> binary().
format_token(Token) ->
    case Token of
        {name, V} ->
            <<<<"\""/utf8, V/binary>>/binary, "\""/utf8>>;

        {int_value, V@1} ->
            erlang:integer_to_binary(V@1);

        {float_value, _} ->
            <<"Float"/utf8>>;

        {string_value, _} ->
            <<"String"/utf8>>;

        e_o_f ->
            <<"end of document"/utf8>>;

        bang ->
            <<"!"/utf8>>;

        dollar ->
            <<"$"/utf8>>;

        left_paren ->
            <<"("/utf8>>;

        right_paren ->
            <<")"/utf8>>;

        left_brace ->
            <<"{"/utf8>>;

        right_brace ->
            <<"}"/utf8>>;

        left_bracket ->
            <<"["/utf8>>;

        right_bracket ->
            <<"]"/utf8>>;

        colon ->
            <<":"/utf8>>;

        equals ->
            <<"="/utf8>>;

        spread ->
            <<"..."/utf8>>;

        pipe ->
            <<"|"/utf8>>;

        at ->
            <<"@"/utf8>>;

        amp ->
            <<"&"/utf8>>;

        'query' ->
            <<"query"/utf8>>;

        mutation ->
            <<"mutation"/utf8>>;

        subscription ->
            <<"subscription"/utf8>>;

        fragment ->
            <<"fragment"/utf8>>;

        on ->
            <<"on"/utf8>>;

        true_keyword ->
            <<"true"/utf8>>;

        false_keyword ->
            <<"false"/utf8>>;

        null_keyword ->
            <<"null"/utf8>>
    end.

-file("src/mochi/executor.gleam", 2981).
-spec format_parse_error(mochi@parser:parse_error()) -> binary().
format_parse_error(Error) ->
    case Error of
        {lex_error, _} ->
            <<"Lexer error"/utf8>>;

        {unexpected_token, Expected, Got, _} ->
            <<<<<<"Expected "/utf8, Expected/binary>>/binary, ", got "/utf8>>/binary,
                (format_token(Got))/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"Unexpected end of document, expected "/utf8, Expected@1/binary>>
    end.

-file("src/mochi/executor.gleam", 621).
-spec execute_inline_fragment(
    query_execution_context(),
    mochi@ast:inline_fragment_value(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_inline_fragment(Context, Inline, Object_type, Field_context) ->
    case erlang:element(2, Inline) of
        none ->
            execute_selection_set(
                Context,
                erlang:element(4, Inline),
                Object_type,
                Field_context
            );

        {some, Type_name} ->
            case does_type_apply(
                erlang:element(2, Context),
                erlang:element(2, Object_type),
                Type_name
            ) of
                true ->
                    execute_selection_set(
                        Context,
                        erlang:element(4, Inline),
                        Object_type,
                        Field_context
                    );

                false ->
                    ok_result(gleam_stdlib:identity(maps:new()))
            end
    end.

-file("src/mochi/executor.gleam", 565).
-spec execute_selection_set(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_selection_set(Context, Selection_set, Object_type, Field_context) ->
    {Data_dict, Errors_acc, Has_none} = gleam@list:fold(
        erlang:element(2, Selection_set),
        {maps:new(), [], false},
        fun(Acc, Selection) ->
            {Data_map, Errors_list, None_found} = Acc,
            Result = execute_selection(
                Context,
                Selection,
                Object_type,
                Field_context
            ),
            New_map = case erlang:element(2, Result) of
                {some, D} ->
                    case gleam@dynamic@decode:run(
                        D,
                        gleam@dynamic@decode:dict(
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                        )
                    ) of
                        {ok, Field_dict} ->
                            maps:merge(Data_map, Field_dict);

                        {error, _} ->
                            Data_map
                    end;

                none ->
                    Data_map
            end,
            New_none = None_found orelse gleam@option:is_none(
                erlang:element(2, Result)
            ),
            {New_map, [erlang:element(3, Result) | Errors_list], New_none}
        end
    ),
    Errors = begin
        _pipe = lists:reverse(Errors_acc),
        lists:append(_pipe)
    end,
    case {Has_none, gleam@dict:is_empty(Data_dict), Errors} of
        {true, _, _} ->
            {execution_result, none, Errors};

        {false, true, []} ->
            ok_result(gleam_stdlib:identity(maps:new()));

        {false, true, _} ->
            {execution_result, none, Errors};

        {false, false, _} ->
            {execution_result, {some, gleam_stdlib:identity(Data_dict)}, Errors}
    end.

-file("src/mochi/executor.gleam", 605).
-spec execute_selection(
    query_execution_context(),
    mochi@ast:selection(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_selection(Context, Selection, Object_type, Field_context) ->
    case Selection of
        {field_selection, Field} ->
            execute_field(Context, Field, Object_type, Field_context);

        {fragment_spread, Spread} ->
            execute_fragment_spread(Context, Spread, Object_type, Field_context);

        {inline_fragment, Inline} ->
            execute_inline_fragment(Context, Inline, Object_type, Field_context)
    end.

-file("src/mochi/executor.gleam", 242).
-spec execute_operation(query_execution_context(), mochi@ast:operation()) -> execution_result().
execute_operation(Context, Operation) ->
    Root_type = get_root_type(erlang:element(2, Context), Operation),
    Selection_set = get_selection_set(Operation),
    Var_defs = get_variable_definitions(Operation),
    Operation_name = case Operation of
        {operation, _, Name, _, _, _} ->
            Name;

        {shorthand_query, _} ->
            none
    end,
    emit_telemetry(
        erlang:element(4, Context),
        {schema_operation_start, Operation_name}
    ),
    Exec_result = case coerce_variable_values(Context, Var_defs) of
        {error, Errors} ->
            {execution_result, none, Errors};

        {ok, Coerced_variables} ->
            Context@1 = {query_execution_context,
                erlang:element(2, Context),
                erlang:element(3, Context),
                erlang:element(4, Context),
                Coerced_variables,
                erlang:element(6, Context)},
            _pipe = Root_type,
            _pipe@1 = gleam@option:map(
                _pipe,
                fun(Obj_type) ->
                    Field_ctx = {field_context,
                        erlang:element(3, Context@1),
                        <<"root"/utf8>>,
                        maps:new(),
                        []},
                    execute_selection_set(
                        Context@1,
                        Selection_set,
                        Obj_type,
                        Field_ctx
                    )
                end
            ),
            gleam@option:unwrap(
                _pipe@1,
                validation_error(
                    <<"Schema does not define a root type for this operation"/utf8>>,
                    []
                )
            )
    end,
    emit_telemetry(
        erlang:element(4, Context),
        {schema_operation_end,
            Operation_name,
            gleam@list:is_empty(erlang:element(3, Exec_result)),
            erlang:length(erlang:element(3, Exec_result))}
    ),
    Exec_result.

-file("src/mochi/executor.gleam", 229).
-spec execute_definition(query_execution_context(), mochi@ast:definition()) -> execution_result().
execute_definition(Context, Definition) ->
    case Definition of
        {operation_definition, Operation} ->
            execute_operation(Context, Operation);

        {fragment_definition, _} ->
            ok_result(gleam_stdlib:identity(maps:new()))
    end.

-file("src/mochi/executor.gleam", 152).
?DOC(
    " Execute with a specific operation name selected\n"
    " This is useful for documents containing multiple operations\n"
).
-spec execute_with_operation_name(
    mochi@schema:schema(),
    mochi@ast:document(),
    gleam@option:option(gleam@dynamic:dynamic_()),
    mochi@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@option:option(binary())
) -> execution_result().
execute_with_operation_name(
    Schema_def,
    Document,
    Root_value,
    Execution_context,
    Variable_values,
    Operation_name
) ->
    Fragments = extract_fragments(Document),
    Context = {query_execution_context,
        Schema_def,
        Root_value,
        Execution_context,
        Variable_values,
        Fragments},
    case find_operation_by_name(Document, Operation_name) of
        {ok, Operation_def} ->
            execute_definition(Context, Operation_def);

        {error, Msg} ->
            validation_error(Msg, [])
    end.

-file("src/mochi/executor.gleam", 133).
-spec execute(
    mochi@schema:schema(),
    mochi@ast:document(),
    gleam@option:option(gleam@dynamic:dynamic_()),
    mochi@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute(Schema_def, Document, Root_value, Execution_context, Variable_values) ->
    execute_with_operation_name(
        Schema_def,
        Document,
        Root_value,
        Execution_context,
        Variable_values,
        none
    ).

-file("src/mochi/executor.gleam", 653).
-spec execute_fragment_spread(
    query_execution_context(),
    mochi@ast:fragment_spread_value(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_fragment_spread(Context, Spread, Object_type, Field_context) ->
    case gleam_stdlib:map_get(
        erlang:element(6, Context),
        erlang:element(2, Spread)
    ) of
        {ok, Fragment} ->
            case does_type_apply(
                erlang:element(2, Context),
                erlang:element(2, Object_type),
                erlang:element(3, Fragment)
            ) of
                true ->
                    execute_selection_set(
                        Context,
                        erlang:element(5, Fragment),
                        Object_type,
                        Field_context
                    );

                false ->
                    ok_result(gleam_stdlib:identity(maps:new()))
            end;

        {error, _} ->
            validation_error(
                <<<<"Fragment '"/utf8, (erlang:element(2, Spread))/binary>>/binary,
                    "' is not defined"/utf8>>,
                erlang:element(5, Field_context)
            )
    end.

-file("src/mochi/executor.gleam", 1458).
-spec execute_abstract_type(
    query_execution_context(),
    mochi@ast:selection_set(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary()),
    gleam@dynamic:dynamic_(),
    gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, binary()} |
        {error, binary()}))
) -> execution_result().
execute_abstract_type(
    Context,
    Sub_selection_set,
    Field_args,
    Field_path,
    Resolved_value,
    Resolve_type
) ->
    require_type_resolver(
        Resolve_type,
        Field_path,
        fun(Resolver) ->
            require_resolved_type(
                Resolver,
                Resolved_value,
                Field_path,
                fun(Type_name) ->
                    require_object_type(
                        erlang:element(2, Context),
                        Type_name,
                        Field_path,
                        fun(Concrete_type) ->
                            Sub_ctx = {field_context,
                                {some, Resolved_value},
                                <<""/utf8>>,
                                Field_args,
                                Field_path},
                            execute_selection_set(
                                Context,
                                Sub_selection_set,
                                Concrete_type,
                                Sub_ctx
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/executor.gleam", 1415).
-spec execute_sub_selection(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(binary()),
    gleam@dynamic:dynamic_()
) -> execution_result().
execute_sub_selection(
    Context,
    Sub_selection_set,
    Field_def,
    Field_args,
    Field_path,
    Resolved_value
) ->
    case get_field_type_definition(
        erlang:element(2, Context),
        erlang:element(4, Field_def)
    ) of
        {ok, {object_type_def, Sub_type}} ->
            Sub_ctx = {field_context,
                {some, Resolved_value},
                erlang:element(2, Field_def),
                Field_args,
                Field_path},
            execute_selection_set(Context, Sub_selection_set, Sub_type, Sub_ctx);

        {ok, {interface_type_def, Iface}} ->
            execute_abstract_type(
                Context,
                Sub_selection_set,
                Field_args,
                Field_path,
                Resolved_value,
                erlang:element(5, Iface)
            );

        {ok, {union_type_def, Union}} ->
            execute_abstract_type(
                Context,
                Sub_selection_set,
                Field_args,
                Field_path,
                Resolved_value,
                erlang:element(5, Union)
            );

        {ok, _} ->
            type_error(
                <<"Cannot execute selection set on non-object type"/utf8>>,
                Field_path
            );

        {error, Msg} ->
            type_error(Msg, Field_path)
    end.

-file("src/mochi/executor.gleam", 1316).
?DOC(" Execute selection set on a single list item\n").
-spec execute_list_item(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dynamic:dynamic_(),
    integer(),
    list(binary()),
    boolean()
) -> execution_result().
execute_list_item(
    Context,
    Selection_set,
    Field_def,
    Field_args,
    Element,
    Index,
    Field_path,
    Items_non_null
) ->
    Item_path = lists:append(Field_path, [erlang:integer_to_binary(Index)]),
    case {is_null(Element), Items_non_null} of
        {true, true} ->
            null_value_error(
                <<"Cannot return null for non-null list item at index "/utf8,
                    (erlang:integer_to_binary(Index))/binary>>,
                Item_path
            );

        {true, false} ->
            ok_result(gleam_stdlib:identity(nil));

        {false, _} ->
            execute_sub_selection(
                Context,
                Selection_set,
                Field_def,
                Field_args,
                Item_path,
                Element
            )
    end.

-file("src/mochi/executor.gleam", 1264).
?DOC(" Execute selection set on a list of items\n").
-spec execute_list_sub_selection(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    list(binary()),
    gleam@dynamic:dynamic_()
) -> execution_result().
execute_list_sub_selection(
    Context,
    Sub_selection_set,
    Field_def,
    Field_args,
    Response_name,
    Field_path,
    Resolved_value
) ->
    case get_list_elements(Resolved_value) of
        {some, Elements} ->
            Inner_type = get_list_inner_type(erlang:element(4, Field_def)),
            Inner_field_def = {field_definition,
                erlang:element(2, Field_def),
                erlang:element(3, Field_def),
                Inner_type,
                erlang:element(5, Field_def),
                erlang:element(6, Field_def),
                erlang:element(7, Field_def),
                erlang:element(8, Field_def),
                erlang:element(9, Field_def)},
            Items_are_non_null = is_non_null_type(Inner_type),
            Results = gleam@list:index_map(
                Elements,
                fun(Element, Index) ->
                    execute_list_item(
                        Context,
                        Sub_selection_set,
                        Inner_field_def,
                        Field_args,
                        Element,
                        Index,
                        Field_path,
                        Items_are_non_null
                    )
                end
            ),
            aggregate_list_results(
                Results,
                erlang:element(4, Field_def),
                Response_name
            );

        none ->
            Sub_result = execute_sub_selection(
                Context,
                Sub_selection_set,
                Field_def,
                Field_args,
                Field_path,
                Resolved_value
            ),
            handle_sub_selection_result(
                Sub_result,
                erlang:element(4, Field_def),
                Response_name
            )
    end.

-file("src/mochi/executor.gleam", 1181).
?DOC(" Handle selection set execution for a field with a resolved value\n").
-spec handle_selection_set(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    list(binary()),
    gleam@dynamic:dynamic_()
) -> execution_result().
handle_selection_set(
    Context,
    Sub_ss,
    Field_def,
    Field_args,
    Response_name,
    Field_path,
    Resolved
) ->
    case is_list_field_type(erlang:element(4, Field_def)) of
        true ->
            execute_list_sub_selection(
                Context,
                Sub_ss,
                Field_def,
                Field_args,
                Response_name,
                Field_path,
                Resolved
            );

        false ->
            Sub_result = execute_sub_selection(
                Context,
                Sub_ss,
                Field_def,
                Field_args,
                Field_path,
                Resolved
            ),
            handle_sub_selection_result(
                Sub_result,
                erlang:element(4, Field_def),
                Response_name
            )
    end.

-file("src/mochi/executor.gleam", 1125).
-spec handle_resolved_value(
    query_execution_context(),
    mochi@ast:field(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    list(binary()),
    gleam@dynamic:dynamic_()
) -> execution_result().
handle_resolved_value(
    Context,
    Field,
    Field_def,
    Field_args,
    Response_name,
    Field_path,
    Resolved
) ->
    Field_location = gleam@option:map(
        erlang:element(7, Field),
        fun(Pos) -> {erlang:element(2, Pos), erlang:element(3, Pos)} end
    ),
    with_non_null_check(
        Resolved,
        erlang:element(4, Field_def),
        Response_name,
        Field_path,
        Field_location,
        fun(Resolved_value) -> case erlang:element(6, Field) of
                none ->
                    ok_result(make_field(Response_name, Resolved_value));

                {some, Sub_ss} ->
                    handle_selection_set(
                        Context,
                        Sub_ss,
                        Field_def,
                        Field_args,
                        Response_name,
                        Field_path,
                        Resolved_value
                    )
            end end
    ).

-file("src/mochi/executor.gleam", 1013).
-spec resolve_field(
    query_execution_context(),
    mochi@ast:field(),
    mochi@schema:field_definition(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    list(binary()),
    fun((mochi@schema:resolver_info()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()}),
    gleam@option:option(gleam@dynamic:dynamic_()),
    list(mochi@ast:directive())
) -> execution_result().
resolve_field(
    Context,
    Field,
    Field_def,
    Field_args,
    Response_name,
    Field_path,
    Resolver,
    Parent_value,
    Directives
) ->
    Field_location = gleam@option:map(
        erlang:element(7, Field),
        fun(Pos) -> {erlang:element(2, Pos), erlang:element(3, Pos)} end
    ),
    Resolver_info = {resolver_info,
        Parent_value,
        Field_args,
        erlang:element(4, Context),
        gleam_stdlib:identity(maps:new())},
    Resolve_result = execute_resolver_with_middleware(
        erlang:element(4, Context),
        Field_def,
        Resolver_info,
        Resolver,
        Response_name
    ),
    case Resolve_result of
        {ok, Resolved} ->
            case apply_custom_directives(
                erlang:element(2, Context),
                Directives,
                Resolved,
                erlang:element(5, Context),
                Field_path
            ) of
                {ok, Transformed} ->
                    handle_resolved_value(
                        Context,
                        Field,
                        Field_def,
                        Field_args,
                        Response_name,
                        Field_path,
                        Transformed
                    );

                {error, Msg} ->
                    resolver_error_at(Msg, Field_path, Field_location)
            end;

        {error, Msg@1} ->
            resolver_error_at(Msg@1, Field_path, Field_location)
    end.

-file("src/mochi/executor.gleam", 930).
-spec execute_regular_field(
    query_execution_context(),
    mochi@ast:field(),
    mochi@schema:object_type(),
    field_context(),
    binary(),
    list(binary())
) -> execution_result().
execute_regular_field(
    Context,
    Field,
    Object_type,
    Field_context,
    Response_name,
    Field_path
) ->
    Field_location = gleam@option:map(
        erlang:element(7, Field),
        fun(Pos) -> {erlang:element(2, Pos), erlang:element(3, Pos)} end
    ),
    require_field(
        Object_type,
        erlang:element(3, Field),
        Field_path,
        Field_location,
        fun(Field_def) ->
            case mochi@input_coercion:coerce_arguments(
                erlang:element(4, Field),
                erlang:element(5, Field_def),
                erlang:element(2, Context),
                erlang:element(5, Context),
                Field_path
            ) of
                {ok, Field_args} ->
                    case erlang:element(6, Field_def) of
                        {some, Resolver} ->
                            resolve_field(
                                Context,
                                Field,
                                Field_def,
                                Field_args,
                                Response_name,
                                Field_path,
                                Resolver,
                                erlang:element(2, Field_context),
                                erlang:element(5, Field)
                            );

                        none ->
                            resolve_from_parent(
                                erlang:element(2, Field_context),
                                erlang:element(3, Field),
                                Response_name,
                                Field_path
                            )
                    end;

                {error, Coercion_error} ->
                    validation_error_at(
                        mochi@input_coercion:format_error(Coercion_error),
                        Field_path,
                        Field_location
                    )
            end
        end
    ).

-file("src/mochi/executor.gleam", 1600).
-spec execute_introspection_schema(
    query_execution_context(),
    mochi@ast:field(),
    binary()
) -> execution_result().
execute_introspection_schema(Context, Field, Response_name) ->
    Introspection_data = build_schema_introspection(
        erlang:element(2, Context),
        Field
    ),
    case erlang:element(6, Field) of
        none ->
            ok_result(make_field(Response_name, Introspection_data));

        {some, Selection_set} ->
            Schema_type = get_introspection_schema_type(),
            Field_ctx = {field_context,
                {some, Introspection_data},
                <<"__schema"/utf8>>,
                maps:new(),
                [Response_name]},
            Result = execute_selection_set(
                Context,
                Selection_set,
                Schema_type,
                Field_ctx
            ),
            wrap_result_in_field(Result, Response_name)
    end.

-file("src/mochi/executor.gleam", 1626).
-spec execute_introspection_type(
    query_execution_context(),
    mochi@ast:field(),
    list(binary()),
    binary()
) -> execution_result().
execute_introspection_type(Context, Field, Field_path, Response_name) ->
    _pipe = get_string_argument(
        erlang:element(4, Field),
        <<"name"/utf8>>,
        erlang:element(5, Context)
    ),
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(Name) ->
            Introspection_data = build_type_introspection(
                erlang:element(2, Context),
                Name
            ),
            case is_null(Introspection_data) of
                true ->
                    ok_result(
                        make_field(Response_name, gleam_stdlib:identity(nil))
                    );

                false ->
                    case erlang:element(6, Field) of
                        none ->
                            ok_result(
                                make_field(Response_name, Introspection_data)
                            );

                        {some, Selection_set} ->
                            Type_type = get_introspection_type_type(),
                            Field_ctx = {field_context,
                                {some, Introspection_data},
                                <<"__type"/utf8>>,
                                maps:new(),
                                Field_path},
                            Result = execute_selection_set(
                                Context,
                                Selection_set,
                                Type_type,
                                Field_ctx
                            ),
                            wrap_result_in_field(Result, Response_name)
                    end
            end
        end
    ),
    gleam@option:unwrap(
        _pipe@1,
        validation_error(
            <<"Missing required argument 'name' for __type"/utf8>>,
            Field_path
        )
    ).

-file("src/mochi/executor.gleam", 723).
-spec execute_field(
    query_execution_context(),
    mochi@ast:field(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_field(Context, Field, Object_type, Field_context) ->
    case should_include_field(
        erlang:element(5, Field),
        erlang:element(5, Context)
    ) of
        false ->
            ok_result(gleam_stdlib:identity(maps:new()));

        true ->
            Response_name = gleam@option:unwrap(
                erlang:element(2, Field),
                erlang:element(3, Field)
            ),
            Field_path = lists:append(
                erlang:element(5, Field_context),
                [Response_name]
            ),
            case erlang:element(3, Field) of
                <<"__typename"/utf8>> ->
                    ok_result(
                        make_field(
                            Response_name,
                            gleam_stdlib:identity(
                                erlang:element(2, Object_type)
                            )
                        )
                    );

                <<"__schema"/utf8>> ->
                    execute_introspection_schema(Context, Field, Response_name);

                <<"__type"/utf8>> ->
                    execute_introspection_type(
                        Context,
                        Field,
                        Field_path,
                        Response_name
                    );

                _ ->
                    execute_regular_field(
                        Context,
                        Field,
                        Object_type,
                        Field_context,
                        Response_name,
                        Field_path
                    )
            end
    end.

-file("src/mochi/executor.gleam", 2719).
-spec execute_query_with_variables(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute_query_with_variables(Schema_def, Query, Variables) ->
    Ctx = mochi@schema:execution_context(gleam_stdlib:identity(maps:new())),
    case get_or_parse(Schema_def, Query) of
        {ok, Document} ->
            execute(Schema_def, Document, none, Ctx, Variables);

        {error, Error} ->
            Loc = case Error of
                {unexpected_token, _, _, Pos} ->
                    {some, {erlang:element(2, Pos), erlang:element(3, Pos)}};

                _ ->
                    none
            end,
            validation_error_at(
                <<"Parse error: "/utf8, (format_parse_error(Error))/binary>>,
                [],
                Loc
            )
    end.

-file("src/mochi/executor.gleam", 2712).
-spec execute_query(mochi@schema:schema(), binary()) -> execution_result().
execute_query(Schema_def, Query) ->
    execute_query_with_variables(Schema_def, Query, maps:new()).

-file("src/mochi/executor.gleam", 2786).
?DOC(
    " Execute a query with a custom execution context (enables full telemetry).\n"
    "\n"
    " Use this function when you need telemetry for parsing and validation phases.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let config = telemetry.with_handler(fn(event) { echo event })\n"
    " let ctx = schema.execution_context(user_data)\n"
    "   |> schema.with_telemetry_fn(telemetry.to_schema_fn(config))\n"
    "\n"
    " let result = executor.execute_query_with_context(\n"
    "   my_schema,\n"
    "   \"{ users { name } }\",\n"
    "   dict.new(),\n"
    "   ctx,\n"
    " )\n"
    " ```\n"
).
-spec execute_query_with_context(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    mochi@schema:execution_context()
) -> execution_result().
execute_query_with_context(Schema_def, Query, Variables, Ctx) ->
    emit_telemetry(Ctx, schema_parse_start),
    Parse_start = mochi_time_ffi:monotonic_time_ns(),
    Parse_result = case get_cached(Schema_def, Query) of
        {ok, Doc} ->
            {ok, {Doc, true}};

        {error, _} ->
            gleam@result:map(
                mochi@parser:parse(Query),
                fun(Doc@1) -> {Doc@1, false} end
            )
    end,
    case Parse_result of
        {ok, {Document, Was_cached}} ->
            Parse_duration = mochi_time_ffi:monotonic_time_ns() - Parse_start,
            emit_telemetry(Ctx, {schema_parse_end, true, Parse_duration}),
            case Was_cached of
                true ->
                    execute(Schema_def, Document, none, Ctx, Variables);

                false ->
                    emit_telemetry(Ctx, schema_validation_start),
                    Validation_start = mochi_time_ffi:monotonic_time_ns(),
                    case mochi@validation:validate_located(Document, Schema_def) of
                        {ok, _} ->
                            Validation_duration = mochi_time_ffi:monotonic_time_ns(
                                
                            )
                            - Validation_start,
                            emit_telemetry(
                                Ctx,
                                {schema_validation_end,
                                    true,
                                    0,
                                    Validation_duration}
                            ),
                            cache_put(Schema_def, Query, Document),
                            execute(Schema_def, Document, none, Ctx, Variables);

                        {error, Validation_errors} ->
                            Validation_duration@1 = mochi_time_ffi:monotonic_time_ns(
                                
                            )
                            - Validation_start,
                            Error_count = erlang:length(Validation_errors),
                            emit_telemetry(
                                Ctx,
                                {schema_validation_end,
                                    false,
                                    Error_count,
                                    Validation_duration@1}
                            ),
                            Errors = gleam@list:map(
                                Validation_errors,
                                fun(Le) ->
                                    {Err, Loc} = Le,
                                    {validation_error,
                                        mochi@validation:format_error(Err),
                                        [],
                                        Loc}
                                end
                            ),
                            {execution_result, none, Errors}
                    end
            end;

        {error, Error} ->
            Parse_duration@1 = mochi_time_ffi:monotonic_time_ns() - Parse_start,
            emit_telemetry(Ctx, {schema_parse_end, false, Parse_duration@1}),
            Loc@1 = case Error of
                {unexpected_token, _, _, Pos} ->
                    {some, {erlang:element(2, Pos), erlang:element(3, Pos)}};

                _ ->
                    none
            end,
            validation_error_at(
                <<"Parse error: "/utf8, (format_parse_error(Error))/binary>>,
                [],
                Loc@1
            )
    end.

-file("src/mochi/executor.gleam", 2876).
-spec execute_query_debug_with_variables(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute_query_debug_with_variables(Schema_def, Query, Variables) ->
    Debug = {debug_context, true, 0, 1},
    log_step(Debug, <<"Starting GraphQL Query Execution"/utf8>>),
    log_info(Debug, <<"Query: "/utf8, Query/binary>>),
    log_step(Debug, <<"Parsing Query"/utf8>>),
    case mochi@parser:parse(Query) of
        {ok, Document} ->
            log_success(Debug, <<"Parse successful"/utf8>>),
            log_info(
                Debug,
                <<"Definitions: "/utf8,
                    (erlang:integer_to_binary(
                        erlang:length(erlang:element(2, Document))
                    ))/binary>>
            ),
            log_step(Debug, <<"Executing Query"/utf8>>),
            Result = execute_query_with_variables(Schema_def, Query, Variables),
            case erlang:element(2, Result) of
                {some, _} ->
                    log_success(Debug, <<"Query executed successfully!"/utf8>>);

                none ->
                    log_error(
                        Debug,
                        <<<<"Query execution failed: "/utf8,
                                (erlang:integer_to_binary(
                                    erlang:length(erlang:element(3, Result))
                                ))/binary>>/binary,
                            " errors"/utf8>>
                    )
            end,
            Result;

        {error, Error} ->
            log_error(
                Debug,
                <<"Parse failed: "/utf8, (format_parse_error(Error))/binary>>
            ),
            Loc = case Error of
                {unexpected_token, _, _, Pos} ->
                    {some, {erlang:element(2, Pos), erlang:element(3, Pos)}};

                _ ->
                    none
            end,
            validation_error_at(
                <<"Parse error: "/utf8, (format_parse_error(Error))/binary>>,
                [],
                Loc
            )
    end.

-file("src/mochi/executor.gleam", 2869).
-spec execute_query_debug(mochi@schema:schema(), binary()) -> execution_result().
execute_query_debug(Schema_def, Query) ->
    execute_query_debug_with_variables(Schema_def, Query, maps:new()).
