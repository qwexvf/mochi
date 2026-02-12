-module(mochi@executor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/executor.gleam").
-export([execute/5, execute_query_with_variables/3, execute_query/2, execute_query_debug_with_variables/3, execute_query_debug/2]).
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

-type execution_error() :: {validation_error, binary(), list(binary())} |
    {resolver_error, binary(), list(binary())} |
    {type_error, binary(), list(binary())} |
    {null_value_error, binary(), list(binary())}.

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

-file("src/mochi/executor.gleam", 56).
-spec ok_result(gleam@dynamic:dynamic_()) -> execution_result().
ok_result(Data) ->
    {execution_result, {some, Data}, []}.

-file("src/mochi/executor.gleam", 60).
-spec error_result(execution_error()) -> execution_result().
error_result(Error) ->
    {execution_result, none, [Error]}.

-file("src/mochi/executor.gleam", 64).
-spec validation_error(binary(), list(binary())) -> execution_result().
validation_error(Msg, Path) ->
    error_result({validation_error, Msg, Path}).

-file("src/mochi/executor.gleam", 68).
-spec resolver_error(binary(), list(binary())) -> execution_result().
resolver_error(Msg, Path) ->
    error_result({resolver_error, Msg, Path}).

-file("src/mochi/executor.gleam", 72).
-spec type_error(binary(), list(binary())) -> execution_result().
type_error(Msg, Path) ->
    error_result({type_error, Msg, Path}).

-file("src/mochi/executor.gleam", 76).
-spec null_value_error(binary(), list(binary())) -> execution_result().
null_value_error(Msg, Path) ->
    error_result({null_value_error, Msg, Path}).

-file("src/mochi/executor.gleam", 117).
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

-file("src/mochi/executor.gleam", 165).
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

-file("src/mochi/executor.gleam", 178).
-spec get_selection_set(mochi@ast:operation()) -> mochi@ast:selection_set().
get_selection_set(Operation) ->
    case Operation of
        {operation, _, _, _, _, Ss} ->
            Ss;

        {shorthand_query, Ss@1} ->
            Ss@1
    end.

-file("src/mochi/executor.gleam", 312).
-spec does_type_apply(mochi@schema:schema(), binary(), binary()) -> boolean().
does_type_apply(Schema_def, Object_type_name, Type_condition) ->
    case Object_type_name =:= Type_condition of
        true ->
            true;

        false ->
            case gleam_stdlib:map_get(
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
            end
    end.

-file("src/mochi/executor.gleam", 419).
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
            _pipe@1 = gleam@result:map(_pipe, fun mochi_ffi:dynamic_to_bool/1),
            gleam@result:unwrap(_pipe@1, none);

        _ ->
            none
    end.

-file("src/mochi/executor.gleam", 401).
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

-file("src/mochi/executor.gleam", 377).
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
    case Skip_value of
        {some, true} ->
            false;

        _ ->
            Include_value = get_directive_bool_arg(
                Directives,
                <<"include"/utf8>>,
                <<"if"/utf8>>,
                Variables
            ),
            case Include_value of
                {some, false} ->
                    false;

                _ ->
                    true
            end
    end.

-file("src/mochi/executor.gleam", 486).
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

-file("src/mochi/executor.gleam", 498).
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

-file("src/mochi/executor.gleam", 511).
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

-file("src/mochi/executor.gleam", 580).
-spec require_field(
    mochi@schema:object_type(),
    binary(),
    list(binary()),
    fun((mochi@schema:field_definition()) -> execution_result())
) -> execution_result().
require_field(Object_type, Field_name, Path, Next) ->
    case gleam_stdlib:map_get(erlang:element(4, Object_type), Field_name) of
        {ok, Field_def} ->
            Next(Field_def);

        {error, _} ->
            validation_error(
                <<<<<<<<"Field '"/utf8, Field_name/binary>>/binary,
                            "' not found on type '"/utf8>>/binary,
                        (erlang:element(2, Object_type))/binary>>/binary,
                    "'"/utf8>>,
                Path
            )
    end.

-file("src/mochi/executor.gleam", 754).
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

-file("src/mochi/executor.gleam", 763).
?DOC(" Check if a field type is non-null at the outermost level\n").
-spec is_non_null_type(mochi@schema:field_type()) -> boolean().
is_non_null_type(Field_type) ->
    case Field_type of
        {non_null, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/executor.gleam", 771).
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

-file("src/mochi/executor.gleam", 994).
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

-file("src/mochi/executor.gleam", 1006).
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

-file("src/mochi/executor.gleam", 1019).
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

-file("src/mochi/executor.gleam", 1047).
-spec get_field_type_definition(
    mochi@schema:schema(),
    mochi@schema:field_type()
) -> {ok, mochi@schema:type_definition()} | {error, binary()}.
get_field_type_definition(Schema_def, Field_type) ->
    case Field_type of
        {named, Name} ->
            _pipe = gleam_stdlib:map_get(erlang:element(5, Schema_def), Name),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<<<"Type '"/utf8, Name/binary>>/binary,
                        "' not found in schema"/utf8>>
                end
            );

        {non_null, Inner} ->
            get_field_type_definition(Schema_def, Inner);

        {list, Inner@1} ->
            get_field_type_definition(Schema_def, Inner@1)
    end.

-file("src/mochi/executor.gleam", 1076).
-spec add_default_values(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    list(mochi@ast:argument())
) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
add_default_values(Args, Arg_defs, Provided_args) ->
    Provided_names = gleam@list:fold(
        Provided_args,
        gleam@set:new(),
        fun(Acc, A) -> gleam@set:insert(Acc, erlang:element(2, A)) end
    ),
    gleam@dict:fold(
        Arg_defs,
        Args,
        fun(Acc@1, Name, Def) ->
            case {gleam@set:contains(Provided_names, Name),
                erlang:element(5, Def)} of
                {false, {some, Default}} ->
                    gleam@dict:insert(Acc@1, Name, Default);

                {_, _} ->
                    Acc@1
            end
        end
    ).

-file("src/mochi/executor.gleam", 1093).
-spec coerce_value(
    mochi@ast:value(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dynamic:dynamic_().
coerce_value(Value, Variables) ->
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
                    fun(_capture) -> coerce_value(_capture, Variables) end
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
                            coerce_value(erlang:element(3, F@1), Variables)
                        )
                    end
                )
            )
    end.

-file("src/mochi/executor.gleam", 523).
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
                coerce_value(erlang:element(3, Arg), Variables)
            )
        end
    ).

-file("src/mochi/executor.gleam", 470).
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

-file("src/mochi/executor.gleam", 446).
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

-file("src/mochi/executor.gleam", 1064).
-spec coerce_arguments(
    list(mochi@ast:argument()),
    gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
coerce_arguments(Ast_args, Arg_defs, Variables) ->
    _pipe = Ast_args,
    _pipe@1 = gleam@list:fold(
        _pipe,
        maps:new(),
        fun(Acc, Arg) ->
            gleam@dict:insert(
                Acc,
                erlang:element(2, Arg),
                coerce_value(erlang:element(3, Arg), Variables)
            )
        end
    ),
    add_default_values(_pipe@1, Arg_defs, Ast_args).

-file("src/mochi/executor.gleam", 1158).
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
            _pipe@1 = gleam@result:map(_pipe, fun mochi_ffi:dynamic_to_string/1),
            gleam@result:unwrap(_pipe@1, none);

        _ ->
            none
    end.

-file("src/mochi/executor.gleam", 1148).
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

-file("src/mochi/executor.gleam", 1203).
-spec build_type_ref(gleam@option:option(mochi@schema:object_type())) -> gleam@dynamic:dynamic_().
build_type_ref(Obj) ->
    _pipe = Obj,
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(O) ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"name"/utf8>>,
                            gleam_stdlib:identity(erlang:element(2, O))}]
                )
            )
        end
    ),
    gleam@option:unwrap(_pipe@1, gleam_stdlib:identity(nil)).

-file("src/mochi/executor.gleam", 1211).
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

-file("src/mochi/executor.gleam", 1253).
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

-file("src/mochi/executor.gleam", 1368).
-spec build_union_introspection(mochi@schema:union_type()) -> gleam@dynamic:dynamic_().
build_union_introspection(Union) ->
    Possible = gleam@list:map(
        erlang:element(4, Union),
        fun(T) ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>, gleam_stdlib:identity(<<"OBJECT"/utf8>>)},
                        {<<"name"/utf8>>,
                            gleam_stdlib:identity(erlang:element(2, T))}]
                )
            )
        end
    ),
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

-file("src/mochi/executor.gleam", 1449).
-spec make_type_object(
    binary(),
    binary(),
    binary(),
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
    Input_fields
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
                {<<"possibleTypes"/utf8>>, gleam_stdlib:identity(nil)},
                {<<"enumValues"/utf8>>,
                    begin
                        _pipe@2 = gleam@option:map(
                            Enum_values,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@2, gleam_stdlib:identity(nil))
                    end},
                {<<"inputFields"/utf8>>,
                    begin
                        _pipe@3 = gleam@option:map(
                            Input_fields,
                            fun gleam_stdlib:identity/1
                        ),
                        gleam@option:unwrap(_pipe@3, gleam_stdlib:identity(nil))
                    end},
                {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
        )
    ).

-file("src/mochi/executor.gleam", 1241).
-spec build_scalar_introspection(binary()) -> gleam@dynamic:dynamic_().
build_scalar_introspection(Name) ->
    make_type_object(
        <<"SCALAR"/utf8>>,
        Name,
        get_scalar_description(Name),
        none,
        none,
        none,
        none
    ).

-file("src/mochi/executor.gleam", 1327).
-spec build_enum_introspection(mochi@schema:enum_type()) -> gleam@dynamic:dynamic_().
build_enum_introspection(Enum) ->
    Values = begin
        _pipe = maps:to_list(erlang:element(4, Enum)),
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
                            {<<"isDeprecated"/utf8>>,
                                gleam_stdlib:identity(erlang:element(5, Def))},
                            {<<"deprecationReason"/utf8>>,
                                case erlang:element(6, Def) of
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
        none
    ).

-file("src/mochi/executor.gleam", 1418).
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
                none
            );

        _ ->
            gleam_stdlib:identity(nil)
    end.

-file("src/mochi/executor.gleam", 1556).
-spec get_type_kind(binary()) -> binary().
get_type_kind(Name) ->
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
            <<"OBJECT"/utf8>>
    end.

-file("src/mochi/executor.gleam", 1527).
-spec build_field_type_introspection(mochi@schema:field_type()) -> gleam@dynamic:dynamic_().
build_field_type_introspection(Field_type) ->
    case Field_type of
        {non_null, Inner} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(<<"NON_NULL"/utf8>>)},
                        {<<"name"/utf8>>, gleam_stdlib:identity(nil)},
                        {<<"ofType"/utf8>>,
                            build_field_type_introspection(Inner)}]
                )
            );

        {list, Inner@1} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>, gleam_stdlib:identity(<<"LIST"/utf8>>)},
                        {<<"name"/utf8>>, gleam_stdlib:identity(nil)},
                        {<<"ofType"/utf8>>,
                            build_field_type_introspection(Inner@1)}]
                )
            );

        {named, Name} ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(get_type_kind(Name))},
                        {<<"name"/utf8>>, gleam_stdlib:identity(Name)},
                        {<<"ofType"/utf8>>, gleam_stdlib:identity(nil)}]
                )
            )
    end.

-file("src/mochi/executor.gleam", 1393).
-spec build_input_introspection(mochi@schema:input_object_type()) -> gleam@dynamic:dynamic_().
build_input_introspection(Input) ->
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
                                    erlang:element(4, Def)
                                )},
                            {<<"defaultValue"/utf8>>,
                                gleam_stdlib:identity(nil)}]
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
        {some, Fields}
    ).

-file("src/mochi/executor.gleam", 1489).
-spec build_fields_introspection(
    gleam@dict:dict(binary(), mochi@schema:field_definition())
) -> list(gleam@dynamic:dynamic_()).
build_fields_introspection(Fields) ->
    _pipe = maps:to_list(Fields),
    gleam@list:map(
        _pipe,
        fun(Kv) ->
            {Name, Def} = Kv,
            Args = begin
                _pipe@1 = maps:to_list(erlang:element(5, Def)),
                gleam@list:map(
                    _pipe@1,
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
                                            erlang:element(4, Arg_def)
                                        )},
                                    {<<"defaultValue"/utf8>>,
                                        gleam_stdlib:identity(nil)}]
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
                                    erlang:element(3, Def),
                                    <<""/utf8>>
                                )
                            )},
                        {<<"args"/utf8>>, gleam_stdlib:identity(Args)},
                        {<<"type"/utf8>>,
                            build_field_type_introspection(
                                erlang:element(4, Def)
                            )},
                        {<<"isDeprecated"/utf8>>,
                            gleam_stdlib:identity(erlang:element(7, Def))},
                        {<<"deprecationReason"/utf8>>,
                            case erlang:element(8, Def) of
                                {some, Reason} ->
                                    gleam_stdlib:identity(Reason);

                                none ->
                                    gleam_stdlib:identity(nil)
                            end}]
                )
            )
        end
    ).

-file("src/mochi/executor.gleam", 1305).
-spec build_object_introspection(mochi@schema:object_type()) -> gleam@dynamic:dynamic_().
build_object_introspection(Obj) ->
    Fields = build_fields_introspection(erlang:element(4, Obj)),
    Interfaces = gleam@list:map(
        erlang:element(5, Obj),
        fun(I) ->
            gleam_stdlib:identity(
                maps:from_list(
                    [{<<"kind"/utf8>>,
                            gleam_stdlib:identity(<<"INTERFACE"/utf8>>)},
                        {<<"name"/utf8>>,
                            gleam_stdlib:identity(erlang:element(2, I))}]
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
        none
    ).

-file("src/mochi/executor.gleam", 1273).
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
            _pipe@3 = gleam@option:map(Opt@1, fun build_object_introspection/1),
            gleam@option:unwrap(_pipe@3, gleam_stdlib:identity(nil))
        end
    ),
    gleam@result:unwrap(_pipe@4, build_meta_type_introspection(Name)).

-file("src/mochi/executor.gleam", 1355).
-spec build_interface_introspection(mochi@schema:interface_type()) -> gleam@dynamic:dynamic_().
build_interface_introspection(Iface) ->
    Fields = build_fields_introspection(erlang:element(4, Iface)),
    make_type_object(
        <<"INTERFACE"/utf8>>,
        erlang:element(2, Iface),
        gleam@option:unwrap(erlang:element(3, Iface), <<""/utf8>>),
        {some, Fields},
        none,
        none,
        none
    ).

-file("src/mochi/executor.gleam", 1285).
-spec build_type_def_introspection(mochi@schema:type_definition()) -> gleam@dynamic:dynamic_().
build_type_def_introspection(Type_def) ->
    case Type_def of
        {object_type_def, Obj} ->
            build_object_introspection(Obj);

        {scalar_type_def, Scalar} ->
            make_type_object(
                <<"SCALAR"/utf8>>,
                erlang:element(2, Scalar),
                gleam@option:unwrap(erlang:element(3, Scalar), <<""/utf8>>),
                none,
                none,
                none,
                none
            );

        {enum_type_def, Enum} ->
            build_enum_introspection(Enum);

        {interface_type_def, Iface} ->
            build_interface_introspection(Iface);

        {union_type_def, Union} ->
            build_union_introspection(Union);

        {input_object_type_def, Input} ->
            build_input_introspection(Input)
    end.

-file("src/mochi/executor.gleam", 1266).
-spec lookup_type_introspection(mochi@schema:schema(), binary()) -> gleam@dynamic:dynamic_().
lookup_type_introspection(Schema_def, Name) ->
    case gleam_stdlib:map_get(erlang:element(5, Schema_def), Name) of
        {ok, Type_def} ->
            build_type_def_introspection(Type_def);

        {error, _} ->
            lookup_root_type(Schema_def, Name)
    end.

-file("src/mochi/executor.gleam", 1233).
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

-file("src/mochi/executor.gleam", 1180).
-spec build_schema_introspection(mochi@schema:schema(), mochi@ast:field()) -> gleam@dynamic:dynamic_().
build_schema_introspection(Schema_def, _) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"queryType"/utf8>>,
                    build_type_ref(erlang:element(2, Schema_def))},
                {<<"mutationType"/utf8>>,
                    build_type_ref(erlang:element(3, Schema_def))},
                {<<"subscriptionType"/utf8>>,
                    build_type_ref(erlang:element(4, Schema_def))},
                {<<"types"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@list:map(
                            get_all_type_names(Schema_def),
                            fun(_capture) ->
                                build_type_introspection(Schema_def, _capture)
                            end
                        )
                    )},
                {<<"directives"/utf8>>, gleam_stdlib:identity([])}]
        )
    ).

-file("src/mochi/executor.gleam", 1567).
-spec make_field(binary(), gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_().
make_field(Name, Value) ->
    gleam_stdlib:identity(maps:from_list([{Name, Value}])).

-file("src/mochi/executor.gleam", 679).
?DOC(" Check for null values and handle non-null field constraints\n").
-spec with_non_null_check(
    gleam@dynamic:dynamic_(),
    mochi@schema:field_type(),
    binary(),
    list(binary()),
    fun((gleam@dynamic:dynamic_()) -> execution_result())
) -> execution_result().
with_non_null_check(Resolved, Field_type, Response_name, Field_path, Next) ->
    case {mochi_ffi:is_null(Resolved), is_non_null_type(Field_type)} of
        {true, true} ->
            null_value_error(
                <<<<"Cannot return null for non-null field '"/utf8,
                        Response_name/binary>>/binary,
                    "'"/utf8>>,
                Field_path
            );

        {true, false} ->
            ok_result(make_field(Response_name, gleam_stdlib:identity(nil)));

        {false, _} ->
            Next(Resolved)
    end.

-file("src/mochi/executor.gleam", 902).
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

-file("src/mochi/executor.gleam", 861).
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
                        {null_value_error, _, _} ->
                            true;

                        _ ->
                            false
                    end end
            ),
            {New_data,
                lists:append(erlang:element(3, Result), Error_list),
                New_null}
        end
    ),
    Errors = lists:reverse(Errors_acc),
    case Has_null_error of
        true ->
            handle_list_null_error(Field_type, Response_name, Errors);

        false ->
            Data_list@1 = lists:reverse(Data_acc),
            case Errors of
                [] ->
                    ok_result(
                        make_field(
                            Response_name,
                            gleam_stdlib:identity(Data_list@1)
                        )
                    );

                _ ->
                    {execution_result,
                        {some,
                            make_field(
                                Response_name,
                                gleam_stdlib:identity(Data_list@1)
                            )},
                        Errors}
            end
    end.

-file("src/mochi/executor.gleam", 918).
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

-file("src/mochi/executor.gleam", 734).
?DOC(" Handle null propagation from sub-selection results\n").
-spec handle_sub_selection_result(
    execution_result(),
    mochi@schema:field_type(),
    binary()
) -> execution_result().
handle_sub_selection_result(Sub_result, Field_type, Response_name) ->
    case erlang:element(2, Sub_result) of
        {some, _} ->
            wrap_result_in_field(Sub_result, Response_name);

        none ->
            case is_non_null_type(Field_type) of
                true ->
                    Sub_result;

                false ->
                    {execution_result,
                        {some,
                            make_field(
                                Response_name,
                                gleam_stdlib:identity(nil)
                            )},
                        erlang:element(3, Sub_result)}
            end
    end.

-file("src/mochi/executor.gleam", 1034).
-spec resolve_from_parent(
    gleam@option:option(gleam@dynamic:dynamic_()),
    binary(),
    binary(),
    list(binary())
) -> execution_result().
resolve_from_parent(Parent, _, Response_name, Field_path) ->
    _pipe = Parent,
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(_) ->
            ok_result(make_field(Response_name, gleam_stdlib:identity(nil)))
        end
    ),
    gleam@option:unwrap(
        _pipe@1,
        resolver_error(<<"No resolver and no parent value"/utf8>>, Field_path)
    ).

-file("src/mochi/executor.gleam", 1118).
-spec execute_introspection_schema(
    query_execution_context(),
    mochi@ast:field(),
    binary()
) -> execution_result().
execute_introspection_schema(Context, Field, Response_name) ->
    ok_result(
        make_field(
            Response_name,
            build_schema_introspection(erlang:element(2, Context), Field)
        )
    ).

-file("src/mochi/executor.gleam", 1129).
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
            ok_result(
                make_field(
                    Response_name,
                    build_type_introspection(erlang:element(2, Context), Name)
                )
            )
        end
    ),
    gleam@option:unwrap(
        _pipe@1,
        validation_error(
            <<"Missing required argument 'name' for __type"/utf8>>,
            Field_path
        )
    ).

-file("src/mochi/executor.gleam", 1571).
-spec merge_results(list(gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
merge_results(Results) ->
    case Results of
        [] ->
            gleam_stdlib:identity(maps:new());

        _ ->
            gleam_stdlib:identity(Results)
    end.

-file("src/mochi/executor.gleam", 1658).
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

-file("src/mochi/executor.gleam", 1665).
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

-file("src/mochi/executor.gleam", 1672).
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

-file("src/mochi/executor.gleam", 1679).
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

-file("src/mochi/executor.gleam", 1686).
-spec format_parse_error(mochi@parser:parse_error()) -> binary().
format_parse_error(Error) ->
    case Error of
        {lex_error, _} ->
            <<"Lexer error"/utf8>>;

        {unexpected_token, Expected, _, _} ->
            <<"Expected "/utf8, Expected/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"Unexpected EOF, expected "/utf8, Expected@1/binary>>
    end.

-file("src/mochi/executor.gleam", 244).
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

-file("src/mochi/executor.gleam", 189).
-spec execute_selection_set(
    query_execution_context(),
    mochi@ast:selection_set(),
    mochi@schema:object_type(),
    field_context()
) -> execution_result().
execute_selection_set(Context, Selection_set, Object_type, Field_context) ->
    {Data_acc, Errors_acc, Has_none} = gleam@list:fold(
        erlang:element(2, Selection_set),
        {[], [], false},
        fun(Acc, Selection) ->
            {Data_list, Errors_list, None_found} = Acc,
            Result = execute_selection(
                Context,
                Selection,
                Object_type,
                Field_context
            ),
            New_data = case erlang:element(2, Result) of
                {some, D} ->
                    [D | Data_list];

                none ->
                    Data_list
            end,
            New_none = None_found orelse gleam@option:is_none(
                erlang:element(2, Result)
            ),
            {New_data,
                lists:append(erlang:element(3, Result), Errors_list),
                New_none}
        end
    ),
    Data_list@1 = lists:reverse(Data_acc),
    Errors = lists:reverse(Errors_acc),
    case Has_none of
        true ->
            {execution_result, none, Errors};

        false ->
            case {Data_list@1, Errors} of
                {[], []} ->
                    ok_result(gleam_stdlib:identity(maps:new()));

                {[], _} ->
                    {execution_result, none, Errors};

                {_, _} ->
                    {execution_result,
                        {some, merge_results(Data_list@1)},
                        Errors}
            end
    end.

-file("src/mochi/executor.gleam", 228).
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

-file("src/mochi/executor.gleam", 139).
-spec execute_operation(query_execution_context(), mochi@ast:operation()) -> execution_result().
execute_operation(Context, Operation) ->
    Root_type = get_root_type(erlang:element(2, Context), Operation),
    Selection_set = get_selection_set(Operation),
    _pipe = Root_type,
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(Obj_type) ->
            Field_ctx = {field_context,
                erlang:element(3, Context),
                <<"root"/utf8>>,
                maps:new(),
                []},
            execute_selection_set(Context, Selection_set, Obj_type, Field_ctx)
        end
    ),
    gleam@option:unwrap(
        _pipe@1,
        validation_error(
            <<"Schema does not define a root type for this operation"/utf8>>,
            []
        )
    ).

-file("src/mochi/executor.gleam", 128).
-spec execute_definition(query_execution_context(), mochi@ast:definition()) -> execution_result().
execute_definition(Context, Definition) ->
    case Definition of
        {operation_definition, Operation} ->
            execute_operation(Context, Operation);

        {fragment_definition, _} ->
            validation_error(
                <<"Fragment definitions not yet supported"/utf8>>,
                []
            )
    end.

-file("src/mochi/executor.gleam", 84).
-spec execute(
    mochi@schema:schema(),
    mochi@ast:document(),
    gleam@option:option(gleam@dynamic:dynamic_()),
    mochi@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute(Schema_def, Document, Root_value, Execution_context, Variable_values) ->
    Fragments = extract_fragments(Document),
    Context = {query_execution_context,
        Schema_def,
        Root_value,
        Execution_context,
        Variable_values,
        Fragments},
    _pipe = erlang:element(2, Document),
    _pipe@1 = gleam@list:find(_pipe, fun(Def) -> case Def of
                {operation_definition, _} ->
                    true;

                {fragment_definition, _} ->
                    false
            end end),
    _pipe@2 = gleam@result:map(
        _pipe@1,
        fun(_capture) -> execute_definition(Context, _capture) end
    ),
    gleam@result:unwrap(
        _pipe@2,
        validation_error(
            <<"Document must contain at least one operation"/utf8>>,
            []
        )
    ).

-file("src/mochi/executor.gleam", 276).
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

-file("src/mochi/executor.gleam", 971).
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

-file("src/mochi/executor.gleam", 928).
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

-file("src/mochi/executor.gleam", 828).
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
    case {mochi_ffi:is_null(Element), Items_non_null} of
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

-file("src/mochi/executor.gleam", 780).
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
    case mochi_ffi:get_list_elements(Resolved_value) of
        {some, Elements} ->
            Inner_type = get_list_inner_type(erlang:element(4, Field_def)),
            Inner_field_def = {field_definition,
                erlang:element(2, Field_def),
                erlang:element(3, Field_def),
                Inner_type,
                erlang:element(5, Field_def),
                erlang:element(6, Field_def),
                erlang:element(7, Field_def),
                erlang:element(8, Field_def)},
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

-file("src/mochi/executor.gleam", 698).
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

-file("src/mochi/executor.gleam", 647).
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
    with_non_null_check(
        Resolved,
        erlang:element(4, Field_def),
        Response_name,
        Field_path,
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

-file("src/mochi/executor.gleam", 600).
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
    Resolver_info = {resolver_info,
        Parent_value,
        Field_args,
        erlang:element(4, Context),
        gleam_stdlib:identity(maps:new())},
    case Resolver(Resolver_info) of
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
                    resolver_error(Msg, Field_path)
            end;

        {error, Msg@1} ->
            resolver_error(Msg@1, Field_path)
    end.

-file("src/mochi/executor.gleam", 540).
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
    require_field(
        Object_type,
        erlang:element(3, Field),
        Field_path,
        fun(Field_def) ->
            Field_args = coerce_arguments(
                erlang:element(4, Field),
                erlang:element(5, Field_def),
                erlang:element(5, Context)
            ),
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
            end
        end
    ).

-file("src/mochi/executor.gleam", 334).
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

-file("src/mochi/executor.gleam", 1589).
-spec execute_query_with_variables(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute_query_with_variables(Schema_def, Query, Variables) ->
    _pipe = mochi@parser:parse(Query),
    _pipe@1 = gleam@result:map(
        _pipe,
        fun(Document) ->
            Ctx = mochi@schema:execution_context(
                gleam_stdlib:identity(maps:new())
            ),
            execute(Schema_def, Document, none, Ctx, Variables)
        end
    ),
    gleam@result:unwrap(
        _pipe@1,
        validation_error(<<"Failed to parse query"/utf8>>, [])
    ).

-file("src/mochi/executor.gleam", 1582).
-spec execute_query(mochi@schema:schema(), binary()) -> execution_result().
execute_query(Schema_def, Query) ->
    execute_query_with_variables(Schema_def, Query, maps:new()).

-file("src/mochi/executor.gleam", 1617).
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
            validation_error(
                <<"Parse error: "/utf8, (format_parse_error(Error))/binary>>,
                []
            )
    end.

-file("src/mochi/executor.gleam", 1610).
-spec execute_query_debug(mochi@schema:schema(), binary()) -> execution_result().
execute_query_debug(Schema_def, Query) ->
    execute_query_debug_with_variables(Schema_def, Query, maps:new()).
