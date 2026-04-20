-module(mochi@validation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/validation.gleam").
-export([format_error/1, format_errors/1, validate/2, validate_located/2, validate_query/2]).
-export_type([validation_error/0, validation_context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type validation_error() :: {parse_error, binary()} |
    {unknown_field, binary(), binary()} |
    {missing_required_argument, binary(), binary()} |
    {unknown_argument, binary(), binary()} |
    {duplicate_argument, binary(), binary()} |
    {undefined_fragment, binary()} |
    {invalid_type_condition, binary(), binary()} |
    {fragment_on_non_composite_type, binary(), binary()} |
    {fragment_spread_not_possible, binary(), binary(), binary()} |
    {unused_fragment, binary()} |
    {undefined_variable, binary()} |
    {unused_variable, binary()} |
    {duplicate_variable, binary()} |
    {variable_not_input_type, binary(), binary()} |
    {duplicate_operation_name, binary()} |
    {duplicate_fragment_name, binary()} |
    anonymous_operation_not_alone |
    {subscription_multiple_root_fields, gleam@option:option(binary())} |
    {circular_fragment_reference, binary(), list(binary())} |
    {directive_not_allowed, binary(), binary()} |
    {unknown_directive, binary()} |
    {duplicate_directive, binary()} |
    {selection_set_required, binary(), binary()} |
    {selection_set_not_allowed, binary(), binary()} |
    {fields_cannot_merge, binary(), binary()}.

-type validation_context() :: {validation_context,
        mochi@schema:schema(),
        gleam@dict:dict(binary(), mochi@ast:fragment()),
        gleam@set:set(binary()),
        gleam@set:set(binary()),
        gleam@set:set(binary()),
        list({validation_error(), gleam@option:option({integer(), integer()})}),
        gleam@option:option(mochi@schema:object_type()),
        gleam@set:set(binary()),
        gleam@option:option({integer(), integer()})}.

-file("src/mochi/validation.gleam", 173).
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

-file("src/mochi/validation.gleam", 157).
-spec init_context(mochi@schema:schema(), mochi@ast:document()) -> validation_context().
init_context(Schema, Document) ->
    Fragments = collect_fragments(Document),
    {validation_context,
        Schema,
        Fragments,
        gleam@set:new(),
        gleam@set:new(),
        gleam@set:new(),
        [],
        erlang:element(2, Schema),
        gleam@set:new(),
        none}.

-file("src/mochi/validation.gleam", 183).
-spec add_error(validation_context(), validation_error()) -> validation_context().
add_error(Ctx, Error) ->
    {validation_context,
        erlang:element(2, Ctx),
        erlang:element(3, Ctx),
        erlang:element(4, Ctx),
        erlang:element(5, Ctx),
        erlang:element(6, Ctx),
        [{Error, erlang:element(10, Ctx)} | erlang:element(7, Ctx)],
        erlang:element(8, Ctx),
        erlang:element(9, Ctx),
        erlang:element(10, Ctx)}.

-file("src/mochi/validation.gleam", 302).
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
        erlang:element(7, Ctx),
        Type_opt,
        erlang:element(9, Ctx),
        erlang:element(10, Ctx)}.

-file("src/mochi/validation.gleam", 335).
-spec get_ast_base_type_name(mochi@ast:type()) -> binary().
get_ast_base_type_name(T) ->
    case T of
        {named_type, Name} ->
            Name;

        {list_type, Inner} ->
            get_ast_base_type_name(Inner);

        {non_null_type, Inner@1} ->
            get_ast_base_type_name(Inner@1)
    end.

-file("src/mochi/validation.gleam", 343).
-spec is_input_type(mochi@schema:schema(), binary()) -> boolean().
is_input_type(Schema, Type_name) ->
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

                {ok, {input_object_type_def, _}} ->
                    true;

                _ ->
                    false
            end
    end.

-file("src/mochi/validation.gleam", 309).
-spec track_defined_variables(
    validation_context(),
    list(mochi@ast:variable_definition())
) -> validation_context().
track_defined_variables(Ctx, Var_defs) ->
    {Defined, Ctx@4} = gleam@list:fold(
        Var_defs,
        {gleam@set:new(), Ctx},
        fun(Acc, Var_def) ->
            {Seen, Ctx@1} = Acc,
            Ctx@2 = case gleam@set:contains(Seen, erlang:element(2, Var_def)) of
                true ->
                    add_error(
                        Ctx@1,
                        {duplicate_variable, erlang:element(2, Var_def)}
                    );

                false ->
                    Ctx@1
            end,
            Type_name = get_ast_base_type_name(erlang:element(3, Var_def)),
            Ctx@3 = case is_input_type(erlang:element(2, Ctx@2), Type_name) of
                true ->
                    Ctx@2;

                false ->
                    add_error(
                        Ctx@2,
                        {variable_not_input_type,
                            erlang:element(2, Var_def),
                            Type_name}
                    )
            end,
            {gleam@set:insert(Seen, erlang:element(2, Var_def)), Ctx@3}
        end
    ),
    {validation_context,
        erlang:element(2, Ctx@4),
        erlang:element(3, Ctx@4),
        Defined,
        gleam@set:new(),
        erlang:element(6, Ctx@4),
        erlang:element(7, Ctx@4),
        erlang:element(8, Ctx@4),
        erlang:element(9, Ctx@4),
        erlang:element(10, Ctx@4)}.

-file("src/mochi/validation.gleam", 356).
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

-file("src/mochi/validation.gleam", 363).
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

-file("src/mochi/validation.gleam", 441).
?DOC(" Require a current type exists in context\n").
-spec require_current_type(
    validation_context(),
    fun((mochi@schema:object_type()) -> validation_context())
) -> validation_context().
require_current_type(Ctx, Next) ->
    case erlang:element(8, Ctx) of
        {some, Obj_type} ->
            Next(Obj_type);

        none ->
            Ctx
    end.

-file("src/mochi/validation.gleam", 452).
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

-file("src/mochi/validation.gleam", 501).
-spec is_required_type(mochi@schema:field_type()) -> boolean().
is_required_type(Field_type) ->
    case Field_type of
        {non_null, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/validation.gleam", 464).
-spec validate_field_arguments(
    validation_context(),
    mochi@ast:field(),
    mochi@schema:field_definition()
) -> validation_context().
validate_field_arguments(Ctx, Field, Field_def) ->
    {Provided_args, Ctx@2} = gleam@list:fold(
        erlang:element(4, Field),
        {gleam@set:new(), Ctx},
        fun(Acc, Arg) ->
            {Seen, Ctx@1} = Acc,
            case gleam@set:contains(Seen, erlang:element(2, Arg)) of
                true ->
                    {Seen,
                        add_error(
                            Ctx@1,
                            {duplicate_argument,
                                erlang:element(3, Field),
                                erlang:element(2, Arg)}
                        )};

                false ->
                    {gleam@set:insert(Seen, erlang:element(2, Arg)), Ctx@1}
            end
        end
    ),
    Ctx@4 = gleam@list:fold(
        erlang:element(4, Field),
        Ctx@2,
        fun(Ctx@3, Arg@1) ->
            case gleam@dict:has_key(
                erlang:element(5, Field_def),
                erlang:element(2, Arg@1)
            ) of
                true ->
                    Ctx@3;

                false ->
                    add_error(
                        Ctx@3,
                        {unknown_argument,
                            erlang:element(3, Field),
                            erlang:element(2, Arg@1)}
                    )
            end
        end
    ),
    gleam@dict:fold(
        erlang:element(5, Field_def),
        Ctx@4,
        fun(Ctx@5, Arg_name, Arg_def) ->
            case is_required_type(erlang:element(4, Arg_def)) of
                true ->
                    case gleam@set:contains(Provided_args, Arg_name) of
                        true ->
                            Ctx@5;

                        false ->
                            add_error(
                                Ctx@5,
                                {missing_required_argument,
                                    erlang:element(3, Field),
                                    Arg_name}
                            )
                    end;

                false ->
                    Ctx@5
            end
        end
    ).

-file("src/mochi/validation.gleam", 534).
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

-file("src/mochi/validation.gleam", 542).
-spec is_leaf_type(mochi@schema:schema(), binary()) -> boolean().
is_leaf_type(Schema, Type_name) ->
    case {Type_name, gleam_stdlib:map_get(erlang:element(5, Schema), Type_name)} of
        {<<"String"/utf8>>, _} ->
            true;

        {<<"Int"/utf8>>, _} ->
            true;

        {<<"Float"/utf8>>, _} ->
            true;

        {<<"Boolean"/utf8>>, _} ->
            true;

        {<<"ID"/utf8>>, _} ->
            true;

        {_, {ok, {scalar_type_def, _}}} ->
            true;

        {_, {ok, {enum_type_def, _}}} ->
            true;

        {_, _} ->
            false
    end.

-file("src/mochi/validation.gleam", 551).
-spec get_object_type(mochi@schema:schema(), binary()) -> gleam@option:option(mochi@schema:object_type()).
get_object_type(Schema, Type_name) ->
    case {Type_name, gleam_stdlib:map_get(erlang:element(5, Schema), Type_name)} of
        {<<"Query"/utf8>>, _} ->
            erlang:element(2, Schema);

        {<<"Mutation"/utf8>>, _} ->
            erlang:element(3, Schema);

        {<<"Subscription"/utf8>>, _} ->
            erlang:element(4, Schema);

        {_, {ok, {object_type_def, Obj}}} ->
            {some, Obj};

        {_, _} ->
            none
    end.

-file("src/mochi/validation.gleam", 570).
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
                        erlang:element(8, Ctx),
                        erlang:element(9, Ctx),
                        erlang:element(10, Ctx)};

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

-file("src/mochi/validation.gleam", 561).
-spec track_argument_variables(validation_context(), list(mochi@ast:argument())) -> validation_context().
track_argument_variables(Ctx, Arguments) ->
    gleam@list:fold(
        Arguments,
        Ctx,
        fun(Ctx@1, Arg) ->
            track_value_variables(Ctx@1, erlang:element(3, Arg))
        end
    ).

-file("src/mochi/validation.gleam", 428).
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

-file("src/mochi/validation.gleam", 656).
-spec is_composite_type(mochi@schema:schema(), binary()) -> boolean().
is_composite_type(Schema, Type_name) ->
    case Type_name of
        <<"Query"/utf8>> ->
            true;

        <<"Mutation"/utf8>> ->
            true;

        <<"Subscription"/utf8>> ->
            true;

        _ ->
            case gleam_stdlib:map_get(erlang:element(5, Schema), Type_name) of
                {ok, {object_type_def, _}} ->
                    true;

                _ ->
                    false
            end
    end.

-file("src/mochi/validation.gleam", 768).
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

-file("src/mochi/validation.gleam", 744).
-spec check_fragment_cycle(
    validation_context(),
    binary(),
    list(binary()),
    gleam@set:set(binary())
) -> validation_context().
check_fragment_cycle(Ctx, Fragment_name, Path, Visited) ->
    case gleam@set:contains(Visited, Fragment_name) of
        true ->
            add_error(Ctx, {circular_fragment_reference, Fragment_name, Path});

        false ->
            case gleam_stdlib:map_get(erlang:element(3, Ctx), Fragment_name) of
                {ok, Fragment} ->
                    New_path = [Fragment_name | Path],
                    New_visited = gleam@set:insert(Visited, Fragment_name),
                    Spread_names = get_fragment_spreads(
                        erlang:element(5, Fragment)
                    ),
                    gleam@list:fold(
                        Spread_names,
                        Ctx,
                        fun(Ctx@1, Spread_name) ->
                            check_fragment_cycle(
                                Ctx@1,
                                Spread_name,
                                New_path,
                                New_visited
                            )
                        end
                    );

                {error, _} ->
                    Ctx
            end
    end.

-file("src/mochi/validation.gleam", 735).
-spec validate_fragment_cycles(validation_context(), mochi@ast:document()) -> validation_context().
validate_fragment_cycles(Ctx, _) ->
    gleam@dict:fold(
        erlang:element(3, Ctx),
        Ctx,
        fun(Ctx@1, Name, _) ->
            check_fragment_cycle(Ctx@1, Name, [], gleam@set:new())
        end
    ).

-file("src/mochi/validation.gleam", 786).
-spec validate_unused_fragments(validation_context()) -> validation_context().
validate_unused_fragments(Ctx) ->
    Defined_fragments = begin
        _pipe = maps:keys(erlang:element(3, Ctx)),
        gleam@set:from_list(_pipe)
    end,
    Unused = gleam@set:difference(Defined_fragments, erlang:element(6, Ctx)),
    gleam@set:fold(
        Unused,
        Ctx,
        fun(Ctx@1, Fragment_name) ->
            add_error(Ctx@1, {unused_fragment, Fragment_name})
        end
    ).

-file("src/mochi/validation.gleam", 877).
?DOC(" Validate that a directive is allowed at the given location\n").
-spec validate_directive_location(
    validation_context(),
    binary(),
    mochi@schema:directive_definition(),
    binary()
) -> validation_context().
validate_directive_location(Ctx, Directive_name, Directive_def, Location) ->
    Location_allowed = gleam@list:any(
        erlang:element(5, Directive_def),
        fun(Loc) ->
            mochi@schema:directive_location_to_string(Loc) =:= Location
        end
    ),
    case Location_allowed of
        true ->
            Ctx;

        false ->
            add_error(Ctx, {directive_not_allowed, Directive_name, Location})
    end.

-file("src/mochi/validation.gleam", 895).
?DOC(" Check if a directive is repeatable\n").
-spec is_repeatable_directive(mochi@schema:schema(), binary()) -> boolean().
is_repeatable_directive(Schema, Directive_name) ->
    case Directive_name of
        <<"skip"/utf8>> ->
            false;

        <<"include"/utf8>> ->
            false;

        <<"deprecated"/utf8>> ->
            false;

        <<"specifiedBy"/utf8>> ->
            false;

        _ ->
            case gleam_stdlib:map_get(erlang:element(6, Schema), Directive_name) of
                {ok, Directive_def} ->
                    erlang:element(6, Directive_def);

                {error, _} ->
                    false
            end
    end.

-file("src/mochi/validation.gleam", 906).
-spec validate_skip_include_args(validation_context(), mochi@ast:directive()) -> validation_context().
validate_skip_include_args(Ctx, Directive) ->
    Ctx@2 = gleam@list:fold(
        erlang:element(3, Directive),
        Ctx,
        fun(Ctx@1, Arg) -> case erlang:element(2, Arg) of
                <<"if"/utf8>> ->
                    track_value_variables(Ctx@1, erlang:element(3, Arg));

                _ ->
                    add_error(
                        Ctx@1,
                        {unknown_argument,
                            <<"@"/utf8, (erlang:element(2, Directive))/binary>>,
                            erlang:element(2, Arg)}
                    )
            end end
    ),
    case gleam@list:find(
        erlang:element(3, Directive),
        fun(A) -> erlang:element(2, A) =:= <<"if"/utf8>> end
    ) of
        {error, _} ->
            add_error(
                Ctx@2,
                {missing_required_argument,
                    <<"@"/utf8, (erlang:element(2, Directive))/binary>>,
                    <<"if"/utf8>>}
            );

        {ok, _} ->
            Ctx@2
    end.

-file("src/mochi/validation.gleam", 831).
?DOC(" Validate a single directive\n").
-spec validate_single_directive(
    validation_context(),
    mochi@ast:directive(),
    binary()
) -> validation_context().
validate_single_directive(Ctx, Directive, Location) ->
    case erlang:element(2, Directive) of
        <<"skip"/utf8>> ->
            case Location of
                <<"FIELD"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                <<"FRAGMENT_SPREAD"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                <<"INLINE_FRAGMENT"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                _ ->
                    add_error(
                        Ctx,
                        {directive_not_allowed,
                            erlang:element(2, Directive),
                            Location}
                    )
            end;

        <<"include"/utf8>> ->
            case Location of
                <<"FIELD"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                <<"FRAGMENT_SPREAD"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                <<"INLINE_FRAGMENT"/utf8>> ->
                    validate_skip_include_args(Ctx, Directive);

                _ ->
                    add_error(
                        Ctx,
                        {directive_not_allowed,
                            erlang:element(2, Directive),
                            Location}
                    )
            end;

        <<"deprecated"/utf8>> ->
            case Location of
                <<"FIELD_DEFINITION"/utf8>> ->
                    Ctx;

                <<"ARGUMENT_DEFINITION"/utf8>> ->
                    Ctx;

                <<"INPUT_FIELD_DEFINITION"/utf8>> ->
                    Ctx;

                <<"ENUM_VALUE"/utf8>> ->
                    Ctx;

                _ ->
                    add_error(
                        Ctx,
                        {directive_not_allowed,
                            erlang:element(2, Directive),
                            Location}
                    )
            end;

        <<"specifiedBy"/utf8>> ->
            case Location of
                <<"SCALAR"/utf8>> ->
                    Ctx;

                _ ->
                    add_error(
                        Ctx,
                        {directive_not_allowed,
                            erlang:element(2, Directive),
                            Location}
                    )
            end;

        _ ->
            case gleam_stdlib:map_get(
                erlang:element(6, erlang:element(2, Ctx)),
                erlang:element(2, Directive)
            ) of
                {ok, Directive_def} ->
                    validate_directive_location(
                        Ctx,
                        erlang:element(2, Directive),
                        Directive_def,
                        Location
                    );

                {error, _} ->
                    add_error(
                        Ctx,
                        {unknown_directive, erlang:element(2, Directive)}
                    )
            end
    end.

-file("src/mochi/validation.gleam", 801).
?DOC(" Validate directives on a field, fragment spread, or inline fragment\n").
-spec validate_directives(
    validation_context(),
    list(mochi@ast:directive()),
    binary()
) -> validation_context().
validate_directives(Ctx, Directives, Location) ->
    {Seen, Ctx@3} = gleam@list:fold(
        Directives,
        {gleam@set:new(), Ctx},
        fun(Acc, Directive) ->
            {Seen_set, Ctx@1} = Acc,
            Ctx@2 = validate_single_directive(Ctx@1, Directive, Location),
            case gleam@set:contains(Seen_set, erlang:element(2, Directive)) of
                true ->
                    case is_repeatable_directive(
                        erlang:element(2, Ctx@2),
                        erlang:element(2, Directive)
                    ) of
                        true ->
                            {Seen_set, Ctx@2};

                        false ->
                            {Seen_set,
                                add_error(
                                    Ctx@2,
                                    {duplicate_directive,
                                        erlang:element(2, Directive)}
                                )}
                    end;

                false ->
                    {gleam@set:insert(Seen_set, erlang:element(2, Directive)),
                        Ctx@2}
            end
        end
    ),
    _ = Seen,
    Ctx@3.

-file("src/mochi/validation.gleam", 626).
-spec validate_fragment_def(validation_context(), mochi@ast:fragment()) -> validation_context().
validate_fragment_def(Ctx, Fragment) ->
    Type_name = erlang:element(3, Fragment),
    Is_builtin_scalar = case Type_name of
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
            false
    end,
    Is_known = (((Is_builtin_scalar orelse (Type_name =:= <<"Query"/utf8>>))
    orelse (Type_name =:= <<"Mutation"/utf8>>))
    orelse (Type_name =:= <<"Subscription"/utf8>>))
    orelse gleam@dict:has_key(
        erlang:element(5, erlang:element(2, Ctx)),
        Type_name
    ),
    Ctx@1 = case Is_known of
        true ->
            Ctx;

        false ->
            add_error(
                Ctx,
                {invalid_type_condition, erlang:element(2, Fragment), Type_name}
            )
    end,
    Ctx@2 = case Is_known andalso not is_composite_type(
        erlang:element(2, Ctx@1),
        Type_name
    ) of
        true ->
            add_error(
                Ctx@1,
                {fragment_on_non_composite_type,
                    erlang:element(2, Fragment),
                    Type_name}
            );

        false ->
            Ctx@1
    end,
    validate_directives(
        Ctx@2,
        erlang:element(4, Fragment),
        <<"FRAGMENT_DEFINITION"/utf8>>
    ).

-file("src/mochi/validation.gleam", 614).
-spec validate_fragment_definitions(validation_context(), mochi@ast:document()) -> validation_context().
validate_fragment_definitions(Ctx, Document) ->
    gleam@list:fold(
        erlang:element(2, Document),
        Ctx,
        fun(Ctx@1, Def) -> case Def of
                {fragment_definition, Fragment} ->
                    validate_fragment_def(Ctx@1, Fragment);

                _ ->
                    Ctx@1
            end end
    ).

-file("src/mochi/validation.gleam", 928).
-spec get_operations(mochi@ast:document()) -> list(mochi@ast:operation()).
get_operations(Document) ->
    gleam@list:filter_map(erlang:element(2, Document), fun(Def) -> case Def of
                {operation_definition, Operation} ->
                    {ok, Operation};

                _ ->
                    {error, nil}
            end end).

-file("src/mochi/validation.gleam", 213).
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

-file("src/mochi/validation.gleam", 937).
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

-file("src/mochi/validation.gleam", 197).
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

-file("src/mochi/validation.gleam", 599).
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

-file("src/mochi/validation.gleam", 958).
?DOC(" Format a validation error as a human-readable string\n").
-spec format_error(validation_error()) -> binary().
format_error(Error) ->
    case Error of
        {parse_error, Message} ->
            <<"Parse error: "/utf8, Message/binary>>;

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

        {duplicate_argument, Field_name@3, Arg_name@2} ->
            <<<<<<<<"There can be only one argument named \""/utf8,
                            Arg_name@2/binary>>/binary,
                        "\" on field \""/utf8>>/binary,
                    Field_name@3/binary>>/binary,
                "\""/utf8>>;

        {undefined_fragment, Name} ->
            <<<<"Unknown fragment \""/utf8, Name/binary>>/binary, "\""/utf8>>;

        {invalid_type_condition, Fragment_name, Type_name@1} ->
            <<<<<<<<"Fragment \""/utf8, Fragment_name/binary>>/binary,
                        "\" cannot condition on non-existent type \""/utf8>>/binary,
                    Type_name@1/binary>>/binary,
                "\""/utf8>>;

        {fragment_on_non_composite_type, Fragment_name@1, Type_name@2} ->
            <<<<<<<<"Fragment \""/utf8, Fragment_name@1/binary>>/binary,
                        "\" cannot condition on non-composite type \""/utf8>>/binary,
                    Type_name@2/binary>>/binary,
                "\""/utf8>>;

        {fragment_spread_not_possible,
            Fragment_name@2,
            Fragment_type,
            Parent_type} ->
            <<<<<<<<<<<<"Fragment \""/utf8, Fragment_name@2/binary>>/binary,
                                "\" cannot be spread here as type \""/utf8>>/binary,
                            Fragment_type/binary>>/binary,
                        "\" never matches type \""/utf8>>/binary,
                    Parent_type/binary>>/binary,
                "\""/utf8>>;

        {unused_fragment, Name@1} ->
            <<<<"Fragment \""/utf8, Name@1/binary>>/binary,
                "\" is never used"/utf8>>;

        {undefined_variable, Name@2} ->
            <<<<"Variable \"$"/utf8, Name@2/binary>>/binary,
                "\" is not defined"/utf8>>;

        {unused_variable, Name@3} ->
            <<<<"Variable \"$"/utf8, Name@3/binary>>/binary,
                "\" is never used"/utf8>>;

        {duplicate_variable, Name@4} ->
            <<<<"There can be only one variable named \"$"/utf8, Name@4/binary>>/binary,
                "\""/utf8>>;

        {variable_not_input_type, Name@5, Type_name@3} ->
            <<<<<<<<"Variable \"$"/utf8, Name@5/binary>>/binary,
                        "\" cannot be non-input type \""/utf8>>/binary,
                    Type_name@3/binary>>/binary,
                "\""/utf8>>;

        {duplicate_operation_name, Name@6} ->
            <<<<"There can be only one operation named \""/utf8, Name@6/binary>>/binary,
                "\""/utf8>>;

        {duplicate_fragment_name, Name@7} ->
            <<<<"There can be only one fragment named \""/utf8, Name@7/binary>>/binary,
                "\""/utf8>>;

        anonymous_operation_not_alone ->
            <<"This anonymous operation must be the only defined operation"/utf8>>;

        {subscription_multiple_root_fields, Name@8} ->
            <<<<"Subscription "/utf8, (case Name@8 of
                        {some, N} ->
                            <<<<"\""/utf8, N/binary>>/binary, "\""/utf8>>;

                        none ->
                            <<""/utf8>>
                    end)/binary>>/binary, " must select only one top level field"/utf8>>;

        {circular_fragment_reference, Name@9, _} ->
            <<<<"Cannot spread fragment \""/utf8, Name@9/binary>>/binary,
                "\" within itself"/utf8>>;

        {directive_not_allowed, Directive_name, Location} ->
            <<<<<<"Directive \"@"/utf8, Directive_name/binary>>/binary,
                    "\" may not be used on "/utf8>>/binary,
                Location/binary>>;

        {unknown_directive, Name@10} ->
            <<<<"Unknown directive \"@"/utf8, Name@10/binary>>/binary,
                "\""/utf8>>;

        {selection_set_required, Field_name@4, Type_name@4} ->
            <<<<<<<<"Field \""/utf8, Field_name@4/binary>>/binary,
                        "\" of type \""/utf8>>/binary,
                    Type_name@4/binary>>/binary,
                "\" must have a selection of subfields"/utf8>>;

        {selection_set_not_allowed, Field_name@5, Type_name@5} ->
            <<<<<<<<"Field \""/utf8, Field_name@5/binary>>/binary,
                        "\" must not have a selection since type \""/utf8>>/binary,
                    Type_name@5/binary>>/binary,
                "\" has no subfields"/utf8>>;

        {duplicate_directive, Name@11} ->
            <<<<"The directive \"@"/utf8, Name@11/binary>>/binary,
                "\" can only be used once at this location"/utf8>>;

        {fields_cannot_merge, Field_name@6, Reason} ->
            <<<<<<<<"Fields \""/utf8, Field_name@6/binary>>/binary,
                        "\" conflict because "/utf8>>/binary,
                    Reason/binary>>/binary,
                ". Use different aliases on the fields to fetch both if this was intentional."/utf8>>
    end.

-file("src/mochi/validation.gleam", 1059).
?DOC(" Format all validation errors as a single string\n").
-spec format_errors(list(validation_error())) -> binary().
format_errors(Errors) ->
    _pipe = Errors,
    _pipe@1 = gleam@list:map(_pipe, fun format_error/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-file("src/mochi/validation.gleam", 1065).
-spec format_parse_error(mochi@parser:parse_error()) -> binary().
format_parse_error(Err) ->
    case Err of
        {lex_error, _} ->
            <<"Lexer error"/utf8>>;

        {unexpected_token, Expected, _, _} ->
            <<"Unexpected token, expected "/utf8, Expected/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"Unexpected end of input, expected "/utf8, Expected@1/binary>>
    end.

-file("src/mochi/validation.gleam", 508).
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
            Outer_type = erlang:element(8, Ctx),
            Inner_type = get_object_type(
                erlang:element(2, Ctx),
                Inner_type_name
            ),
            Nested_ctx = begin
                _pipe = Ctx,
                _pipe@1 = set_current_type(_pipe, Inner_type),
                validate_selection_set(_pipe@1, Ss)
            end,
            set_current_type(Nested_ctx, Outer_type)
    end.

-file("src/mochi/validation.gleam", 387).
-spec validate_selection_set(validation_context(), mochi@ast:selection_set()) -> validation_context().
validate_selection_set(Ctx, Selection_set) ->
    gleam@list:fold(
        erlang:element(2, Selection_set),
        Ctx,
        fun(Ctx@1, Selection) -> validate_selection(Ctx@1, Selection) end
    ).

-file("src/mochi/validation.gleam", 396).
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

-file("src/mochi/validation.gleam", 407).
-spec validate_field(validation_context(), mochi@ast:field()) -> validation_context().
validate_field(Ctx, Field) ->
    Ctx@1 = case erlang:element(7, Field) of
        {some, {position, Line, Column}} ->
            {validation_context,
                erlang:element(2, Ctx),
                erlang:element(3, Ctx),
                erlang:element(4, Ctx),
                erlang:element(5, Ctx),
                erlang:element(6, Ctx),
                erlang:element(7, Ctx),
                erlang:element(8, Ctx),
                erlang:element(9, Ctx),
                {some, {Line, Column}}};

        none ->
            Ctx
    end,
    Ctx@2 = validate_directives(
        Ctx@1,
        erlang:element(5, Field),
        <<"FIELD"/utf8>>
    ),
    skip_introspection_field(
        erlang:element(3, Field),
        Ctx@2,
        erlang:element(4, Field),
        fun(Ctx@3) ->
            require_current_type(
                Ctx@3,
                fun(Obj_type) ->
                    require_field_def(
                        Ctx@3,
                        Obj_type,
                        erlang:element(3, Field),
                        fun(Field_def) -> _pipe = Ctx@3,
                            _pipe@1 = validate_field_arguments(
                                _pipe,
                                Field,
                                Field_def
                            ),
                            _pipe@2 = track_argument_variables(
                                _pipe@1,
                                erlang:element(4, Field)
                            ),
                            _pipe@3 = validate_directives(
                                _pipe@2,
                                erlang:element(5, Field),
                                <<"FIELD"/utf8>>
                            ),
                            validate_field_selection_set(
                                _pipe@3,
                                Field,
                                Field_def
                            ) end
                    )
                end
            )
        end
    ).

-file("src/mochi/validation.gleam", 245).
-spec validate_operation(validation_context(), mochi@ast:operation()) -> validation_context().
validate_operation(Ctx, Operation) ->
    case Operation of
        {shorthand_query, Selection_set} ->
            Ctx@1 = set_current_type(
                Ctx,
                erlang:element(2, erlang:element(2, Ctx))
            ),
            validate_selection_set(Ctx@1, Selection_set);

        {operation, Op_type, Op_name, Var_defs, Op_directives, Selection_set@1} ->
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
            Op_location = case Op_type of
                'query' ->
                    <<"QUERY"/utf8>>;

                mutation ->
                    <<"MUTATION"/utf8>>;

                subscription ->
                    <<"SUBSCRIPTION"/utf8>>
            end,
            Ctx@4 = validate_directives(Ctx@3, Op_directives, Op_location),
            Ctx@6 = gleam@list:fold(
                Var_defs,
                Ctx@4,
                fun(Ctx@5, Var_def) ->
                    validate_directives(
                        Ctx@5,
                        erlang:element(5, Var_def),
                        <<"VARIABLE_DEFINITION"/utf8>>
                    )
                end
            ),
            Ctx@7 = case Op_type of
                subscription ->
                    validate_subscription_single_root(
                        Ctx@6,
                        Selection_set@1,
                        Op_name
                    );

                _ ->
                    Ctx@6
            end,
            Ctx@8 = validate_selection_set(Ctx@7, Selection_set@1),
            validate_unused_variables(Ctx@8)
    end.

-file("src/mochi/validation.gleam", 233).
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

-file("src/mochi/validation.gleam", 104).
-spec run_validation(mochi@ast:document(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error,
        list({validation_error(), gleam@option:option({integer(), integer()})})}.
run_validation(Document, Schema) ->
    Ctx = init_context(Schema, Document),
    Ctx@1 = validate_unique_operation_names(Ctx, Document),
    Ctx@2 = validate_lone_anonymous_operation(Ctx@1, Document),
    Ctx@3 = validate_unique_fragment_names(Ctx@2, Document),
    Ctx@4 = validate_document_operations(Ctx@3, Document),
    Ctx@5 = validate_fragment_definitions(Ctx@4, Document),
    Ctx@6 = validate_fragment_cycles(Ctx@5, Document),
    Ctx@7 = validate_unused_fragments(Ctx@6),
    case erlang:element(7, Ctx@7) of
        [] ->
            {ok, Document};

        Errors ->
            {error, lists:reverse(Errors)}
    end.

-file("src/mochi/validation.gleam", 123).
?DOC(" Validate a document against a schema\n").
-spec validate(mochi@ast:document(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error, list(validation_error())}.
validate(Document, Schema) ->
    case run_validation(Document, Schema) of
        {ok, Doc} ->
            {ok, Doc};

        {error, Located} ->
            {error,
                gleam@list:map(Located, fun(Le) -> erlang:element(1, Le) end)}
    end.

-file("src/mochi/validation.gleam", 134).
?DOC(" Like validate, but preserves source locations for each error\n").
-spec validate_located(mochi@ast:document(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error,
        list({validation_error(), gleam@option:option({integer(), integer()})})}.
validate_located(Document, Schema) ->
    run_validation(Document, Schema).

-file("src/mochi/validation.gleam", 142).
?DOC(" Validate a single query string against a schema (convenience function)\n").
-spec validate_query(binary(), mochi@schema:schema()) -> {ok,
        mochi@ast:document()} |
    {error, list(validation_error())}.
validate_query(Query, Schema) ->
    case mochi@parser:parse(Query) of
        {ok, Document} ->
            validate(Document, Schema);

        {error, Parse_error} ->
            {error, [{parse_error, format_parse_error(Parse_error)}]}
    end.

-file("src/mochi/validation.gleam", 667).
-spec validate_fragment_spread(
    validation_context(),
    mochi@ast:fragment_spread_value()
) -> validation_context().
validate_fragment_spread(Ctx, Spread) ->
    Ctx@1 = validate_directives(
        Ctx,
        erlang:element(3, Spread),
        <<"FRAGMENT_SPREAD"/utf8>>
    ),
    Fragment_result = gleam_stdlib:map_get(
        erlang:element(3, Ctx@1),
        erlang:element(2, Spread)
    ),
    Is_cycle = gleam@set:contains(
        erlang:element(9, Ctx@1),
        erlang:element(2, Spread)
    ),
    Ctx@2 = {validation_context,
        erlang:element(2, Ctx@1),
        erlang:element(3, Ctx@1),
        erlang:element(4, Ctx@1),
        erlang:element(5, Ctx@1),
        gleam@set:insert(erlang:element(6, Ctx@1), erlang:element(2, Spread)),
        erlang:element(7, Ctx@1),
        erlang:element(8, Ctx@1),
        erlang:element(9, Ctx@1),
        erlang:element(10, Ctx@1)},
    Ctx@3 = validate_directives(
        Ctx@2,
        erlang:element(3, Spread),
        <<"FRAGMENT_SPREAD"/utf8>>
    ),
    case {Fragment_result, Is_cycle} of
        {{error, _}, _} ->
            add_error(Ctx@3, {undefined_fragment, erlang:element(2, Spread)});

        {{ok, _}, true} ->
            Ctx@3;

        {{ok, Fragment}, false} ->
            Outer_type = erlang:element(8, Ctx@3),
            Fragment_type = get_object_type(
                erlang:element(2, Ctx@3),
                erlang:element(3, Fragment)
            ),
            Ctx@4 = set_current_type(Ctx@3, Fragment_type),
            Ctx@5 = {validation_context,
                erlang:element(2, Ctx@4),
                erlang:element(3, Ctx@4),
                erlang:element(4, Ctx@4),
                erlang:element(5, Ctx@4),
                erlang:element(6, Ctx@4),
                erlang:element(7, Ctx@4),
                erlang:element(8, Ctx@4),
                gleam@set:insert(
                    erlang:element(9, Ctx@4),
                    erlang:element(2, Spread)
                ),
                erlang:element(10, Ctx@4)},
            Ctx@6 = validate_selection_set(Ctx@5, erlang:element(5, Fragment)),
            Ctx@7 = {validation_context,
                erlang:element(2, Ctx@6),
                erlang:element(3, Ctx@6),
                erlang:element(4, Ctx@6),
                erlang:element(5, Ctx@6),
                erlang:element(6, Ctx@6),
                erlang:element(7, Ctx@6),
                erlang:element(8, Ctx@6),
                gleam@set:delete(
                    erlang:element(9, Ctx@6),
                    erlang:element(2, Spread)
                ),
                erlang:element(10, Ctx@6)},
            set_current_type(Ctx@7, Outer_type)
    end.

-file("src/mochi/validation.gleam", 714).
-spec validate_inline_fragment(
    validation_context(),
    mochi@ast:inline_fragment_value()
) -> validation_context().
validate_inline_fragment(Ctx, Inline) ->
    Ctx@1 = validate_directives(
        Ctx,
        erlang:element(3, Inline),
        <<"INLINE_FRAGMENT"/utf8>>
    ),
    case erlang:element(2, Inline) of
        {some, Type_name} ->
            Outer_type = erlang:element(8, Ctx@1),
            Inner_type = get_object_type(erlang:element(2, Ctx@1), Type_name),
            Nested_ctx = begin
                _pipe = Ctx@1,
                _pipe@1 = set_current_type(_pipe, Inner_type),
                validate_selection_set(_pipe@1, erlang:element(4, Inline))
            end,
            set_current_type(Nested_ctx, Outer_type);

        none ->
            validate_selection_set(Ctx@1, erlang:element(4, Inline))
    end.
