-module(mochi@validation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/validation.gleam").
-export([format_error/1, format_errors/1, validate/2, validate_query/2]).
-export_type([validation_error/0, validation_context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type validation_error() :: {unknown_field, binary(), binary()} |
    {missing_required_argument, binary(), binary()} |
    {unknown_argument, binary(), binary()} |
    {undefined_fragment, binary()} |
    {invalid_type_condition, binary(), binary()} |
    {undefined_variable, binary()} |
    {unused_variable, binary()} |
    {duplicate_operation_name, binary()} |
    {duplicate_fragment_name, binary()} |
    anonymous_operation_not_alone |
    {subscription_multiple_root_fields, gleam@option:option(binary())} |
    {circular_fragment_reference, binary(), list(binary())} |
    {directive_not_allowed, binary(), binary()} |
    {unknown_directive, binary()} |
    {selection_set_required, binary(), binary()} |
    {selection_set_not_allowed, binary(), binary()}.

-type validation_context() :: {validation_context,
        mochi@schema:schema(),
        gleam@dict:dict(binary(), mochi@ast:fragment()),
        gleam@set:set(binary()),
        gleam@set:set(binary()),
        list(validation_error()),
        gleam@option:option(mochi@schema:object_type()),
        list(binary())}.

-file("src/mochi/validation.gleam", 126).
-spec collect_fragments(mochi@ast:document()) -> gleam@dict:dict(binary(), mochi@ast:fragment()).
collect_fragments(Document) ->
    gleam@list:fold(
        erlang:element(2, Document),
        maps:new(),
        fun(Acc, Def) -> case Def of
                {fragment_definition, Fragment} ->
                    gleam@dict:insert(
                        Acc,
                        erlang:element(2, Fragment),
                        Fragment
                    );

                _ ->
                    Acc
            end end
    ).

-file("src/mochi/validation.gleam", 112).
-spec init_context(mochi@schema:schema(), mochi@ast:document()) -> validation_context().
init_context(Schema, Document) ->
    Fragments = collect_fragments(Document),
    {validation_context,
        Schema,
        Fragments,
        gleam@set:new(),
        gleam@set:new(),
        [],
        erlang:element(2, Schema),
        []}.

-file("src/mochi/validation.gleam", 136).
-spec add_error(validation_context(), validation_error()) -> validation_context().
add_error(Ctx, Error) ->
    {validation_context,
        erlang:element(2, Ctx),
        erlang:element(3, Ctx),
        erlang:element(4, Ctx),
        erlang:element(5, Ctx),
        [Error | erlang:element(6, Ctx)],
        erlang:element(7, Ctx),
        erlang:element(8, Ctx)}.

-file("src/mochi/validation.gleam", 238).
-spec set_current_type(
    validation_context(),
    gleam@option:option(mochi@schema:object_type())
) -> validation_context().
set_current_type(Ctx, Type_opt) ->
    {validation_context,
        erlang:element(2, Ctx),
        erlang:element(3, Ctx),
        erlang:element(4, Ctx),
        erlang:element(5, Ctx),
        erlang:element(6, Ctx),
        Type_opt,
        erlang:element(8, Ctx)}.

-file("src/mochi/validation.gleam", 245).
-spec track_defined_variables(
    validation_context(),
    list(mochi@ast:variable_definition())
) -> validation_context().
track_defined_variables(Ctx, Var_defs) ->
    Defined = gleam@list:fold(
        Var_defs,
        gleam@set:new(),
        fun(Acc, Var_def) ->
            gleam@set:insert(Acc, erlang:element(2, Var_def))
        end
    ),
    {validation_context,
        erlang:element(2, Ctx),
        erlang:element(3, Ctx),
        Defined,
        gleam@set:new(),
        erlang:element(6, Ctx),
        erlang:element(7, Ctx),
        erlang:element(8, Ctx)}.

-file("src/mochi/validation.gleam", 260).
-spec validate_unused_variables(validation_context()) -> validation_context().
validate_unused_variables(Ctx) ->
    Unused = gleam@set:difference(
        erlang:element(4, Ctx),
        erlang:element(5, Ctx)
    ),
    gleam@set:fold(
        Unused,
        Ctx,
        fun(Ctx@1, Var_name) ->
            add_error(Ctx@1, {unused_variable, Var_name})
        end
    ).

-file("src/mochi/validation.gleam", 267).
-spec validate_subscription_single_root(
    validation_context(),
    mochi@ast:selection_set(),
    gleam@option:option(binary())
) -> validation_context().
validate_subscription_single_root(Ctx, Selection_set, Op_name) ->
    Root_fields = gleam@list:filter(
        erlang:element(2, Selection_set),
        fun(Sel) -> case Sel of
                {field_selection, Field} ->
                    erlang:element(3, Field) /= <<"__typename"/utf8>>;

                _ ->
                    true
            end end
    ),
    case erlang:length(Root_fields) > 1 of
        true ->
            add_error(Ctx, {subscription_multiple_root_fields, Op_name});

        false ->
            Ctx
    end.

-file("src/mochi/validation.gleam", 336).
?DOC(" Require a current type exists in context\n").
-spec require_current_type(
    validation_context(),
    fun((mochi@schema:object_type()) -> validation_context())
) -> validation_context().
require_current_type(Ctx, Next) ->
    case erlang:element(7, Ctx) of
        {some, Obj_type} ->
            Next(Obj_type);

        none ->
            Ctx
    end.

-file("src/mochi/validation.gleam", 347).
?DOC(" Require a field definition exists on the object type\n").
-spec require_field_def(
    validation_context(),
    mochi@schema:object_type(),
    binary(),
    fun((mochi@schema:field_definition()) -> validation_context())
) -> validation_context().
require_field_def(Ctx, Obj_type, Field_name, Next) ->
    case gleam_stdlib:map_get(erlang:element(4, Obj_type), Field_name) of
        {ok, Field_def} ->
            Next(Field_def);

        {error, _} ->
            add_error(
                Ctx,
                {unknown_field, Field_name, erlang:element(2, Obj_type)}
            )
    end.

-file("src/mochi/validation.gleam", 391).
-spec is_required_type(mochi@schema:field_type()) -> boolean().
is_required_type(Field_type) ->
    case Field_type of
        {non_null, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/validation.gleam", 359).
-spec validate_field_arguments(
    validation_context(),
    mochi@ast:field(),
    mochi@schema:field_definition()
) -> validation_context().
validate_field_arguments(Ctx, Field, Field_def) ->
    Provided_args = gleam@list:fold(
        erlang:element(4, Field),
        maps:new(),
        fun(Acc, Arg) ->
            gleam@dict:insert(Acc, erlang:element(2, Arg), true)
        end
    ),
    Ctx@2 = gleam@list:fold(
        erlang:element(4, Field),
        Ctx,
        fun(Ctx@1, Arg@1) ->
            case gleam@dict:has_key(
                erlang:element(5, Field_def),
                erlang:element(2, Arg@1)
            ) of
                true ->
                    Ctx@1;

                false ->
                    add_error(
                        Ctx@1,
                        {unknown_argument,
                            erlang:element(3, Field),
                            erlang:element(2, Arg@1)}
                    )
            end
        end
    ),
    gleam@dict:fold(
        erlang:element(5, Field_def),
        Ctx@2,
        fun(Ctx@3, Arg_name, Arg_def) ->
            case is_required_type(erlang:element(4, Arg_def)) of
                true ->
                    case gleam@dict:has_key(Provided_args, Arg_name) of
                        true ->
                            Ctx@3;

                        false ->
                            add_error(
                                Ctx@3,
                                {missing_required_argument,
                                    erlang:element(3, Field),
                                    Arg_name}
                            )
                    end;

                false ->
                    Ctx@3
            end
        end
    ).

-file("src/mochi/validation.gleam", 421).
-spec get_base_type_name(mochi@schema:field_type()) -> binary().
get_base_type_name(Field_type) ->
    case Field_type of
        {named, Name} ->
            Name;

        {non_null, Inner} ->
            get_base_type_name(Inner);

        {list, Inner@1} ->
            get_base_type_name(Inner@1)
    end.

-file("src/mochi/validation.gleam", 429).
-spec is_leaf_type(mochi@schema:schema(), binary()) -> boolean().
is_leaf_type(Schema, Type_name) ->
    case Type_name of
        <<"String"/utf8>> ->
            true;

        <<"Int"/utf8>> ->
            true;

        <<"Float"/utf8>> ->
            true;

        <<"Boolean"/utf8>> ->
            true;

        <<"ID"/utf8>> ->
            true;

        _ ->
            case gleam_stdlib:map_get(erlang:element(5, Schema), Type_name) of
                {ok, {scalar_type_def, _}} ->
                    true;

                {ok, {enum_type_def, _}} ->
                    true;

                _ ->
                    false
            end
    end.

-file("src/mochi/validation.gleam", 442).
-spec get_object_type(mochi@schema:schema(), binary()) -> gleam@option:option(mochi@schema:object_type()).
get_object_type(Schema, Type_name) ->
    case Type_name of
        <<"Query"/utf8>> ->
            erlang:element(2, Schema);

        <<"Mutation"/utf8>> ->
            erlang:element(3, Schema);

        <<"Subscription"/utf8>> ->
            erlang:element(4, Schema);

        _ ->
            case gleam_stdlib:map_get(erlang:element(5, Schema), Type_name) of
                {ok, {object_type_def, Obj}} ->
                    {some, Obj};

                _ ->
                    none
            end
    end.

-file("src/mochi/validation.gleam", 465).
-spec track_value_variables(validation_context(), mochi@ast:value()) -> validation_context().
track_value_variables(Ctx, Value) ->
    case Value of
        {variable_value, Name} ->
            Ctx@1 = case gleam@set:contains(erlang:element(4, Ctx), Name) of
                true ->
                    {validation_context,
                        erlang:element(2, Ctx),
                        erlang:element(3, Ctx),
                        erlang:element(4, Ctx),
                        gleam@set:insert(erlang:element(5, Ctx), Name),
                        erlang:element(6, Ctx),
                        erlang:element(7, Ctx),
                        erlang:element(8, Ctx)};

                false ->
                    add_error(Ctx, {undefined_variable, Name})
            end,
            Ctx@1;

        {list_value, Values} ->
            gleam@list:fold(
                Values,
                Ctx,
                fun(Ctx@2, V) -> track_value_variables(Ctx@2, V) end
            );

        {object_value, Fields} ->
            gleam@list:fold(
                Fields,
                Ctx,
                fun(Ctx@3, F) ->
                    track_value_variables(Ctx@3, erlang:element(3, F))
                end
            );

        _ ->
            Ctx
    end.

-file("src/mochi/validation.gleam", 456).
-spec track_argument_variables(validation_context(), list(mochi@ast:argument())) -> validation_context().
track_argument_variables(Ctx, Arguments) ->
    gleam@list:fold(
        Arguments,
        Ctx,
        fun(Ctx@1, Arg) ->
            track_value_variables(Ctx@1, erlang:element(3, Arg))
        end
    ).

-file("src/mochi/validation.gleam", 323).
?DOC(" Skip validation for introspection fields, just track variables\n").
-spec skip_introspection_field(
    binary(),
    validation_context(),
    list(mochi@ast:argument()),
    fun((validation_context()) -> validation_context())
) -> validation_context().
skip_introspection_field(Field_name, Ctx, Arguments, Next) ->
    case gleam_stdlib:string_starts_with(Field_name, <<"__"/utf8>>) of
        true ->
            track_argument_variables(Ctx, Arguments);

        false ->
            Next(Ctx)
    end.

-file("src/mochi/validation.gleam", 522).
-spec validate_fragment_type_condition(
    validation_context(),
    mochi@ast:fragment()
) -> validation_context().
validate_fragment_type_condition(Ctx, Fragment) ->
    Type_name = erlang:element(3, Fragment),
    Type_exists = (((Type_name =:= <<"Query"/utf8>>) orelse (Type_name =:= <<"Mutation"/utf8>>))
    orelse (Type_name =:= <<"Subscription"/utf8>>))
    orelse gleam@dict:has_key(
        erlang:element(5, erlang:element(2, Ctx)),
        Type_name
    ),
    case Type_exists of
        true ->
            Ctx;

        false ->
            add_error(
                Ctx,
                {invalid_type_condition, erlang:element(2, Fragment), Type_name}
            )
    end.

-file("src/mochi/validation.gleam", 509).
-spec validate_fragment_definitions(validation_context(), mochi@ast:document()) -> validation_context().
validate_fragment_definitions(Ctx, Document) ->
    gleam@list:fold(
        erlang:element(2, Document),
        Ctx,
        fun(Ctx@1, Def) -> case Def of
                {fragment_definition, Fragment} ->
                    validate_fragment_type_condition(Ctx@1, Fragment);

                _ ->
                    Ctx@1
            end end
    ).

-file("src/mochi/validation.gleam", 619).
-spec get_fragment_spreads(mochi@ast:selection_set()) -> list(binary()).
get_fragment_spreads(Selection_set) ->
    gleam@list:flat_map(
        erlang:element(2, Selection_set),
        fun(Sel) -> case Sel of
                {fragment_spread, Spread} ->
                    [erlang:element(2, Spread)];

                {field_selection, Field} ->
                    case erlang:element(6, Field) of
                        {some, Ss} ->
                            get_fragment_spreads(Ss);

                        none ->
                            []
                    end;

                {inline_fragment, Inline} ->
                    get_fragment_spreads(erlang:element(4, Inline))
            end end
    ).

-file("src/mochi/validation.gleam", 597).
-spec check_fragment_cycle(validation_context(), binary(), list(binary())) -> validation_context().
check_fragment_cycle(Ctx, Fragment_name, Path) ->
    case gleam@list:contains(Path, Fragment_name) of
        true ->
            add_error(Ctx, {circular_fragment_reference, Fragment_name, Path});

        false ->
            case gleam_stdlib:map_get(erlang:element(3, Ctx), Fragment_name) of
                {ok, Fragment} ->
                    New_path = [Fragment_name | Path],
                    Spread_names = get_fragment_spreads(
                        erlang:element(5, Fragment)
                    ),
                    gleam@list:fold(
                        Spread_names,
                        Ctx,
                        fun(Ctx@1, Spread_name) ->
                            check_fragment_cycle(Ctx@1, Spread_name, New_path)
                        end
                    );

                {error, _} ->
                    Ctx
            end
    end.

-file("src/mochi/validation.gleam", 587).
-spec validate_fragment_cycles(validation_context(), mochi@ast:document()) -> validation_context().
validate_fragment_cycles(Ctx, _) ->
    gleam@dict:fold(
        erlang:element(3, Ctx),
        Ctx,
        fun(Ctx@1, Name, _) -> check_fragment_cycle(Ctx@1, Name, []) end
    ).

-file("src/mochi/validation.gleam", 637).
-spec get_operations(mochi@ast:document()) -> list(mochi@ast:operation()).
get_operations(Document) ->
    gleam@list:filter_map(erlang:element(2, Document), fun(Def) -> case Def of
                {operation_definition, Operation} ->
                    {ok, Operation};

                _ ->
                    {error, nil}
            end end).

-file("src/mochi/validation.gleam", 163).
-spec validate_lone_anonymous_operation(
    validation_context(),
    mochi@ast:document()
) -> validation_context().
validate_lone_anonymous_operation(Ctx, Document) ->
    Operations = get_operations(Document),
    Has_anonymous = gleam@list:any(Operations, fun(Op) -> case Op of
                {shorthand_query, _} ->
                    true;

                {operation, _, none, _, _, _} ->
                    true;

                _ ->
                    false
            end end),
    case Has_anonymous andalso (erlang:length(Operations) > 1) of
        true ->
            add_error(Ctx, anonymous_operation_not_alone);

        false ->
            Ctx
    end.

-file("src/mochi/validation.gleam", 646).
-spec check_duplicates(
    validation_context(),
    list(binary()),
    fun((binary()) -> validation_error())
) -> validation_context().
check_duplicates(Ctx, Names, Error_fn) ->
    {_, Ctx@2} = gleam@list:fold(
        Names,
        {gleam@set:new(), Ctx},
        fun(Acc, Name) ->
            {Seen, Ctx@1} = Acc,
            case gleam@set:contains(Seen, Name) of
                true ->
                    {Seen, add_error(Ctx@1, Error_fn(Name))};

                false ->
                    {gleam@set:insert(Seen, Name), Ctx@1}
            end
        end
    ),
    Ctx@2.

-file("src/mochi/validation.gleam", 147).
-spec validate_unique_operation_names(
    validation_context(),
    mochi@ast:document()
) -> validation_context().
validate_unique_operation_names(Ctx, Document) ->
    Operations = get_operations(Document),
    Names = gleam@list:filter_map(Operations, fun(Op) -> case Op of
                {operation, _, {some, Name}, _, _, _} ->
                    {ok, Name};

                _ ->
                    {error, nil}
            end end),
    check_duplicates(
        Ctx,
        Names,
        fun(Field@0) -> {duplicate_operation_name, Field@0} end
    ).

-file("src/mochi/validation.gleam", 494).
-spec validate_unique_fragment_names(validation_context(), mochi@ast:document()) -> validation_context().
validate_unique_fragment_names(Ctx, Document) ->
    Names = gleam@list:filter_map(
        erlang:element(2, Document),
        fun(Def) -> case Def of
                {fragment_definition, Fragment} ->
                    {ok, erlang:element(2, Fragment)};

                _ ->
                    {error, nil}
            end end
    ),
    check_duplicates(
        Ctx,
        Names,
        fun(Field@0) -> {duplicate_fragment_name, Field@0} end
    ).

-file("src/mochi/validation.gleam", 667).
?DOC(" Format a validation error as a human-readable string\n").
-spec format_error(validation_error()) -> binary().
format_error(Error) ->
    case Error of
        {unknown_field, Field_name, Type_name} ->
            <<<<<<<<"Cannot query field \""/utf8, Field_name/binary>>/binary,
                        "\" on type \""/utf8>>/binary,
                    Type_name/binary>>/binary,
                "\""/utf8>>;

        {missing_required_argument, Field_name@1, Arg_name} ->
            <<<<<<<<"Field \""/utf8, Field_name@1/binary>>/binary,
                        "\" argument \""/utf8>>/binary,
                    Arg_name/binary>>/binary,
                "\" of type is required but not provided"/utf8>>;

        {unknown_argument, Field_name@2, Arg_name@1} ->
            <<<<<<<<"Unknown argument \""/utf8, Arg_name@1/binary>>/binary,
                        "\" on field \""/utf8>>/binary,
                    Field_name@2/binary>>/binary,
                "\""/utf8>>;

        {undefined_fragment, Name} ->
            <<<<"Unknown fragment \""/utf8, Name/binary>>/binary, "\""/utf8>>;

        {invalid_type_condition, Fragment_name, Type_name@1} ->
            <<<<<<<<"Fragment \""/utf8, Fragment_name/binary>>/binary,
                        "\" cannot condition on non-existent type \""/utf8>>/binary,
                    Type_name@1/binary>>/binary,
                "\""/utf8>>;

        {undefined_variable, Name@1} ->
            <<<<"Variable \"$"/utf8, Name@1/binary>>/binary,
                "\" is not defined"/utf8>>;

        {unused_variable, Name@2} ->
            <<<<"Variable \"$"/utf8, Name@2/binary>>/binary,
                "\" is never used"/utf8>>;

        {duplicate_operation_name, Name@3} ->
            <<<<"There can be only one operation named \""/utf8, Name@3/binary>>/binary,
                "\""/utf8>>;

        {duplicate_fragment_name, Name@4} ->
            <<<<"There can be only one fragment named \""/utf8, Name@4/binary>>/binary,
                "\""/utf8>>;

        anonymous_operation_not_alone ->
            <<"This anonymous operation must be the only defined operation"/utf8>>;

        {subscription_multiple_root_fields, Name@5} ->
            <<<<"Subscription "/utf8, (case Name@5 of
                        {some, N} ->
                            <<<<"\""/utf8, N/binary>>/binary, "\""/utf8>>;

                        none ->
                            <<""/utf8>>
                    end)/binary>>/binary, " must select only one top level field"/utf8>>;

        {circular_fragment_reference, Name@6, _} ->
            <<<<"Cannot spread fragment \""/utf8, Name@6/binary>>/binary,
                "\" within itself"/utf8>>;

        {directive_not_allowed, Directive_name, Location} ->
            <<<<<<"Directive \"@"/utf8, Directive_name/binary>>/binary,
                    "\" may not be used on "/utf8>>/binary,
                Location/binary>>;

        {unknown_directive, Name@7} ->
            <<<<"Unknown directive \"@"/utf8, Name@7/binary>>/binary,
                "\""/utf8>>;

        {selection_set_required, Field_name@3, Type_name@2} ->
            <<<<<<<<"Field \""/utf8, Field_name@3/binary>>/binary,
                        "\" of type \""/utf8>>/binary,
                    Type_name@2/binary>>/binary,
                "\" must have a selection of subfields"/utf8>>;

        {selection_set_not_allowed, Field_name@4, Type_name@3} ->
            <<<<<<<<"Field \""/utf8, Field_name@4/binary>>/binary,
                        "\" must not have a selection since type \""/utf8>>/binary,
                    Type_name@3/binary>>/binary,
                "\" has no subfields"/utf8>>
    end.

-file("src/mochi/validation.gleam", 730).
?DOC(" Format all validation errors as a single string\n").
-spec format_errors(list(validation_error())) -> binary().
format_errors(Errors) ->
    _pipe = Errors,
    _pipe@1 = gleam@list:map(_pipe, fun format_error/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-file("src/mochi/validation.gleam", 398).
-spec validate_field_selection_set(
    validation_context(),
    mochi@ast:field(),
    mochi@schema:field_definition()
) -> validation_context().
validate_field_selection_set(Ctx, Field, Field_def) ->
    Inner_type_name = get_base_type_name(erlang:element(4, Field_def)),
    Is_leaf = is_leaf_type(erlang:element(2, Ctx), Inner_type_name),
    case {Is_leaf, erlang:element(6, Field)} of
        {true, {some, _}} ->
            add_error(
                Ctx,
                {selection_set_not_allowed,
                    erlang:element(3, Field),
                    Inner_type_name}
            );

        {true, none} ->
            Ctx;

        {false, none} ->
            add_error(
                Ctx,
                {selection_set_required,
                    erlang:element(3, Field),
                    Inner_type_name}
            );

        {false, {some, Ss}} ->
            Inner_type = get_object_type(
                erlang:element(2, Ctx),
                Inner_type_name
            ),
            _pipe = Ctx,
            _pipe@1 = set_current_type(_pipe, Inner_type),
            validate_selection_set(_pipe@1, Ss)
    end.

-file("src/mochi/validation.gleam", 291).
-spec validate_selection_set(validation_context(), mochi@ast:selection_set()) -> validation_context().
validate_selection_set(Ctx, Selection_set) ->
    gleam@list:fold(
        erlang:element(2, Selection_set),
        Ctx,
        fun(Ctx@1, Selection) -> validate_selection(Ctx@1, Selection) end
    ).

-file("src/mochi/validation.gleam", 300).
-spec validate_selection(validation_context(), mochi@ast:selection()) -> validation_context().
validate_selection(Ctx, Selection) ->
    case Selection of
        {field_selection, Field} ->
            validate_field(Ctx, Field);

        {fragment_spread, Spread} ->
            validate_fragment_spread(Ctx, Spread);

        {inline_fragment, Inline} ->
            validate_inline_fragment(Ctx, Inline)
    end.

-file("src/mochi/validation.gleam", 311).
-spec validate_field(validation_context(), mochi@ast:field()) -> validation_context().
validate_field(Ctx, Field) ->
    skip_introspection_field(
        erlang:element(3, Field),
        Ctx,
        erlang:element(4, Field),
        fun(Ctx@1) ->
            require_current_type(
                Ctx@1,
                fun(Obj_type) ->
                    require_field_def(
                        Ctx@1,
                        Obj_type,
                        erlang:element(3, Field),
                        fun(Field_def) -> _pipe = Ctx@1,
                            _pipe@1 = validate_field_arguments(
                                _pipe,
                                Field,
                                Field_def
                            ),
                            _pipe@2 = track_argument_variables(
                                _pipe@1,
                                erlang:element(4, Field)
                            ),
                            validate_field_selection_set(
                                _pipe@2,
                                Field,
                                Field_def
                            ) end
                    )
                end
            )
        end
    ).

-file("src/mochi/validation.gleam", 195).
-spec validate_operation(validation_context(), mochi@ast:operation()) -> validation_context().
validate_operation(Ctx, Operation) ->
    case Operation of
        {shorthand_query, Selection_set} ->
            Ctx@1 = set_current_type(
                Ctx,
                erlang:element(2, erlang:element(2, Ctx))
            ),
            validate_selection_set(Ctx@1, Selection_set);

        {operation, Op_type, Op_name, Var_defs, _, Selection_set@1} ->
            Root_type = case Op_type of
                'query' ->
                    erlang:element(2, erlang:element(2, Ctx));

                mutation ->
                    erlang:element(3, erlang:element(2, Ctx));

                subscription ->
                    erlang:element(4, erlang:element(2, Ctx))
            end,
            Ctx@2 = set_current_type(Ctx, Root_type),
            Ctx@3 = track_defined_variables(Ctx@2, Var_defs),
            Ctx@4 = case Op_type of
                subscription ->
                    validate_subscription_single_root(
                        Ctx@3,
                        Selection_set@1,
                        Op_name
                    );

                _ ->
                    Ctx@3
            end,
            Ctx@5 = validate_selection_set(Ctx@4, Selection_set@1),
            validate_unused_variables(Ctx@5)
    end.

-file("src/mochi/validation.gleam", 183).
-spec validate_document_operations(validation_context(), mochi@ast:document()) -> validation_context().
validate_document_operations(Ctx, Document) ->
    gleam@list:fold(
        erlang:element(2, Document),
        Ctx,
        fun(Ctx@1, Def) -> case Def of
                {operation_definition, Operation} ->
                    validate_operation(Ctx@1, Operation);

                _ ->
                    Ctx@1
            end end
    ).

-file("src/mochi/validation.gleam", 77).
?DOC(" Validate a document against a schema\n").
-spec validate(mochi@ast:document(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error, list(validation_error())}.
validate(Document, Schema) ->
    Ctx = init_context(Schema, Document),
    Ctx@1 = validate_unique_operation_names(Ctx, Document),
    Ctx@2 = validate_lone_anonymous_operation(Ctx@1, Document),
    Ctx@3 = validate_unique_fragment_names(Ctx@2, Document),
    Ctx@4 = validate_document_operations(Ctx@3, Document),
    Ctx@5 = validate_fragment_definitions(Ctx@4, Document),
    Ctx@6 = validate_fragment_cycles(Ctx@5, Document),
    case erlang:element(6, Ctx@6) of
        [] ->
            {ok, Document};

        Errors ->
            {error, lists:reverse(Errors)}
    end.

-file("src/mochi/validation.gleam", 98).
?DOC(" Validate a single query string against a schema (convenience function)\n").
-spec validate_query(binary(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error, list(validation_error())}.
validate_query(Query, Schema) ->
    case mochi@parser:parse(Query) of
        {ok, Document} ->
            validate(Document, Schema);

        {error, _} ->
            {error, []}
    end.

-file("src/mochi/validation.gleam", 541).
-spec validate_fragment_spread(
    validation_context(),
    mochi@ast:fragment_spread_value()
) -> validation_context().
validate_fragment_spread(Ctx, Spread) ->
    case gleam_stdlib:map_get(erlang:element(3, Ctx), erlang:element(2, Spread)) of
        {ok, Fragment} ->
            case gleam@list:contains(
                erlang:element(8, Ctx),
                erlang:element(2, Spread)
            ) of
                true ->
                    Ctx;

                false ->
                    Fragment_type = get_object_type(
                        erlang:element(2, Ctx),
                        erlang:element(3, Fragment)
                    ),
                    Ctx@1 = set_current_type(Ctx, Fragment_type),
                    Ctx@2 = {validation_context,
                        erlang:element(2, Ctx@1),
                        erlang:element(3, Ctx@1),
                        erlang:element(4, Ctx@1),
                        erlang:element(5, Ctx@1),
                        erlang:element(6, Ctx@1),
                        erlang:element(7, Ctx@1),
                        [erlang:element(2, Spread) | erlang:element(8, Ctx@1)]},
                    Ctx@3 = validate_selection_set(
                        Ctx@2,
                        erlang:element(5, Fragment)
                    ),
                    {validation_context,
                        erlang:element(2, Ctx@3),
                        erlang:element(3, Ctx@3),
                        erlang:element(4, Ctx@3),
                        erlang:element(5, Ctx@3),
                        erlang:element(6, Ctx@3),
                        erlang:element(7, Ctx@3),
                        gleam@list:drop(erlang:element(8, Ctx@3), 1)}
            end;

        {error, _} ->
            add_error(Ctx, {undefined_fragment, erlang:element(2, Spread)})
    end.

-file("src/mochi/validation.gleam", 573).
-spec validate_inline_fragment(
    validation_context(),
    mochi@ast:inline_fragment_value()
) -> validation_context().
validate_inline_fragment(Ctx, Inline) ->
    case erlang:element(2, Inline) of
        {some, Type_name} ->
            Inner_type = get_object_type(erlang:element(2, Ctx), Type_name),
            Ctx@1 = set_current_type(Ctx, Inner_type),
            validate_selection_set(Ctx@1, erlang:element(4, Inline));

        none ->
            validate_selection_set(Ctx, erlang:element(4, Inline))
    end.
