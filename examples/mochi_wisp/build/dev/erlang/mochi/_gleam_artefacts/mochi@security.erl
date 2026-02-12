-module(mochi@security).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/security.gleam").
-export([default_config/0, no_limits/0, error_message/1, analyze/1, validate/2]).
-export_type([security_config/0, security_error/0, query_analysis/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type security_config() :: {security_config,
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(integer())}.

-type security_error() :: {depth_limit_exceeded, integer(), integer()} |
    {complexity_limit_exceeded, integer(), integer()} |
    {alias_limit_exceeded, integer(), integer()} |
    {root_field_limit_exceeded, integer(), integer()}.

-type query_analysis() :: {query_analysis,
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-file("src/mochi/security.gleam", 25).
?DOC(" Default security configuration\n").
-spec default_config() -> security_config().
default_config() ->
    {security_config, {some, 10}, {some, 1000}, {some, 50}, {some, 20}}.

-file("src/mochi/security.gleam", 35).
?DOC(" Permissive configuration (no limits)\n").
-spec no_limits() -> security_config().
no_limits() ->
    {security_config, none, none, none, none}.

-file("src/mochi/security.gleam", 55).
-spec error_message(security_error()) -> binary().
error_message(Error) ->
    case Error of
        {depth_limit_exceeded, Depth, Max} ->
            <<<<<<"Query depth "/utf8,
                        (erlang:integer_to_binary(Depth))/binary>>/binary,
                    " exceeds maximum allowed depth of "/utf8>>/binary,
                (erlang:integer_to_binary(Max))/binary>>;

        {complexity_limit_exceeded, Complexity, Max@1} ->
            <<<<<<"Query complexity "/utf8,
                        (erlang:integer_to_binary(Complexity))/binary>>/binary,
                    " exceeds maximum allowed complexity of "/utf8>>/binary,
                (erlang:integer_to_binary(Max@1))/binary>>;

        {alias_limit_exceeded, Count, Max@2} ->
            <<<<<<"Query has "/utf8, (erlang:integer_to_binary(Count))/binary>>/binary,
                    " field aliases, exceeding maximum of "/utf8>>/binary,
                (erlang:integer_to_binary(Max@2))/binary>>;

        {root_field_limit_exceeded, Count@1, Max@3} ->
            <<<<<<"Query has "/utf8,
                        (erlang:integer_to_binary(Count@1))/binary>>/binary,
                    " root fields, exceeding maximum of "/utf8>>/binary,
                (erlang:integer_to_binary(Max@3))/binary>>
    end.

-file("src/mochi/security.gleam", 159).
-spec get_operations(mochi@ast:document()) -> list(mochi@ast:operation()).
get_operations(Document) ->
    gleam@list:filter_map(erlang:element(2, Document), fun(Def) -> case Def of
                {operation_definition, Op} ->
                    {ok, Op};

                _ ->
                    {error, nil}
            end end).

-file("src/mochi/security.gleam", 168).
-spec get_fragments(mochi@ast:document()) -> gleam@dict:dict(binary(), mochi@ast:fragment()).
get_fragments(Document) ->
    gleam@list:fold(
        erlang:element(2, Document),
        maps:new(),
        fun(Acc, Def) -> case Def of
                {fragment_definition, Frag} ->
                    gleam@dict:insert(Acc, erlang:element(2, Frag), Frag);

                _ ->
                    Acc
            end end
    ).

-file("src/mochi/security.gleam", 177).
-spec get_operation_selection_set(mochi@ast:operation()) -> mochi@ast:selection_set().
get_operation_selection_set(Op) ->
    case Op of
        {operation, _, _, _, _, Selection_set} ->
            Selection_set;

        {shorthand_query, Selection_set@1} ->
            Selection_set@1
    end.

-file("src/mochi/security.gleam", 184).
-spec calculate_depth(
    mochi@ast:selection_set(),
    gleam@dict:dict(binary(), mochi@ast:fragment()),
    integer()
) -> integer().
calculate_depth(Selection_set, Fragments, Current_depth) ->
    case erlang:element(2, Selection_set) of
        [] ->
            Current_depth;

        Selections ->
            gleam@list:fold(
                Selections,
                Current_depth,
                fun(Max_depth, Selection) ->
                    Child_depth = case Selection of
                        {field_selection, Field} ->
                            case erlang:element(6, Field) of
                                {some, Ss} ->
                                    calculate_depth(
                                        Ss,
                                        Fragments,
                                        Current_depth + 1
                                    );

                                none ->
                                    Current_depth
                            end;

                        {fragment_spread, Spread} ->
                            case gleam_stdlib:map_get(
                                Fragments,
                                erlang:element(2, Spread)
                            ) of
                                {ok, Frag} ->
                                    calculate_depth(
                                        erlang:element(5, Frag),
                                        Fragments,
                                        Current_depth
                                    );

                                {error, _} ->
                                    Current_depth
                            end;

                        {inline_fragment, Inline} ->
                            calculate_depth(
                                erlang:element(4, Inline),
                                Fragments,
                                Current_depth
                            )
                    end,
                    gleam@int:max(Max_depth, Child_depth)
                end
            )
    end.

-file("src/mochi/security.gleam", 248).
-spec get_list_multiplier(list(mochi@ast:argument())) -> integer().
get_list_multiplier(Arguments) ->
    gleam@list:fold(
        Arguments,
        1,
        fun(Mult, Arg) -> case erlang:element(2, Arg) of
                <<"first"/utf8>> ->
                    case erlang:element(3, Arg) of
                        {int_value, N} when N > 0 ->
                            gleam@int:max(Mult, N);

                        _ ->
                            Mult
                    end;

                <<"last"/utf8>> ->
                    case erlang:element(3, Arg) of
                        {int_value, N} when N > 0 ->
                            gleam@int:max(Mult, N);

                        _ ->
                            Mult
                    end;

                <<"limit"/utf8>> ->
                    case erlang:element(3, Arg) of
                        {int_value, N} when N > 0 ->
                            gleam@int:max(Mult, N);

                        _ ->
                            Mult
                    end;

                <<"take"/utf8>> ->
                    case erlang:element(3, Arg) of
                        {int_value, N} when N > 0 ->
                            gleam@int:max(Mult, N);

                        _ ->
                            Mult
                    end;

                _ ->
                    Mult
            end end
    ).

-file("src/mochi/security.gleam", 216).
-spec calculate_complexity(
    mochi@ast:selection_set(),
    gleam@dict:dict(binary(), mochi@ast:fragment())
) -> integer().
calculate_complexity(Selection_set, Fragments) ->
    gleam@list:fold(
        erlang:element(2, Selection_set),
        0,
        fun(Total, Selection) ->
            Field_complexity = case Selection of
                {field_selection, Field} ->
                    Base = 1,
                    Nested = case erlang:element(6, Field) of
                        {some, Ss} ->
                            calculate_complexity(Ss, Fragments);

                        none ->
                            0
                    end,
                    Multiplier = get_list_multiplier(erlang:element(4, Field)),
                    (Base + Nested) * Multiplier;

                {fragment_spread, Spread} ->
                    case gleam_stdlib:map_get(
                        Fragments,
                        erlang:element(2, Spread)
                    ) of
                        {ok, Frag} ->
                            calculate_complexity(
                                erlang:element(5, Frag),
                                Fragments
                            );

                        {error, _} ->
                            0
                    end;

                {inline_fragment, Inline} ->
                    calculate_complexity(erlang:element(4, Inline), Fragments)
            end,
            Total + Field_complexity
        end
    ).

-file("src/mochi/security.gleam", 263).
-spec count_aliases(
    mochi@ast:selection_set(),
    gleam@dict:dict(binary(), mochi@ast:fragment())
) -> integer().
count_aliases(Selection_set, Fragments) ->
    gleam@list:fold(
        erlang:element(2, Selection_set),
        0,
        fun(Count, Selection) -> case Selection of
                {field_selection, Field} ->
                    Has_alias = case erlang:element(2, Field) of
                        {some, _} ->
                            1;

                        none ->
                            0
                    end,
                    Nested = case erlang:element(6, Field) of
                        {some, Ss} ->
                            count_aliases(Ss, Fragments);

                        none ->
                            0
                    end,
                    (Count + Has_alias) + Nested;

                {fragment_spread, Spread} ->
                    case gleam_stdlib:map_get(
                        Fragments,
                        erlang:element(2, Spread)
                    ) of
                        {ok, Frag} ->
                            Count + count_aliases(
                                erlang:element(5, Frag),
                                Fragments
                            );

                        {error, _} ->
                            Count
                    end;

                {inline_fragment, Inline} ->
                    Count + count_aliases(erlang:element(4, Inline), Fragments)
            end end
    ).

-file("src/mochi/security.gleam", 293).
-spec count_selections(mochi@ast:selection_set()) -> integer().
count_selections(Selection_set) ->
    erlang:length(erlang:element(2, Selection_set)).

-file("src/mochi/security.gleam", 297).
-spec count_fields(
    mochi@ast:selection_set(),
    gleam@dict:dict(binary(), mochi@ast:fragment())
) -> integer().
count_fields(Selection_set, Fragments) ->
    gleam@list:fold(
        erlang:element(2, Selection_set),
        0,
        fun(Count, Selection) -> case Selection of
                {field_selection, Field} ->
                    Nested = case erlang:element(6, Field) of
                        {some, Ss} ->
                            count_fields(Ss, Fragments);

                        none ->
                            0
                    end,
                    (Count + 1) + Nested;

                {fragment_spread, Spread} ->
                    case gleam_stdlib:map_get(
                        Fragments,
                        erlang:element(2, Spread)
                    ) of
                        {ok, Frag} ->
                            Count + count_fields(
                                erlang:element(5, Frag),
                                Fragments
                            );

                        {error, _} ->
                            Count
                    end;

                {inline_fragment, Inline} ->
                    Count + count_fields(erlang:element(4, Inline), Fragments)
            end end
    ).

-file("src/mochi/security.gleam", 83).
?DOC(" Analyze a query document\n").
-spec analyze(mochi@ast:document()) -> query_analysis().
analyze(Document) ->
    Operations = get_operations(Document),
    Fragments = get_fragments(Document),
    {Max_depth, Total_complexity, Alias_count, Root_fields, Field_count} = gleam@list:fold(
        Operations,
        {0, 0, 0, 0, 0},
        fun(Acc, Op) ->
            {D, C, A, R, F} = Acc,
            Selection_set = get_operation_selection_set(Op),
            Op_depth = calculate_depth(Selection_set, Fragments, 1),
            Op_complexity = calculate_complexity(Selection_set, Fragments),
            Op_aliases = count_aliases(Selection_set, Fragments),
            Op_root_fields = count_selections(Selection_set),
            Op_field_count = count_fields(Selection_set, Fragments),
            {gleam@int:max(D, Op_depth),
                C + Op_complexity,
                A + Op_aliases,
                R + Op_root_fields,
                F + Op_field_count}
        end
    ),
    {query_analysis,
        Max_depth,
        Total_complexity,
        Alias_count,
        Root_fields,
        Field_count}.

-file("src/mochi/security.gleam", 116).
?DOC(" Validate a query against security configuration\n").
-spec validate(mochi@ast:document(), security_config()) -> {ok,
        query_analysis()} |
    {error, security_error()}.
validate(Document, Config) ->
    Analysis = analyze(Document),
    _pipe = case erlang:element(2, Config) of
        {some, Max} when erlang:element(2, Analysis) > Max ->
            {error, {depth_limit_exceeded, erlang:element(2, Analysis), Max}};

        _ ->
            {ok, nil}
    end,
    _pipe@1 = gleam@result:'try'(
        _pipe,
        fun(_) -> case erlang:element(3, Config) of
                {some, Max@1} when erlang:element(3, Analysis) > Max@1 ->
                    {error,
                        {complexity_limit_exceeded,
                            erlang:element(3, Analysis),
                            Max@1}};

                _ ->
                    {ok, nil}
            end end
    ),
    _pipe@2 = gleam@result:'try'(
        _pipe@1,
        fun(_) -> case erlang:element(4, Config) of
                {some, Max@2} when erlang:element(4, Analysis) > Max@2 ->
                    {error,
                        {alias_limit_exceeded,
                            erlang:element(4, Analysis),
                            Max@2}};

                _ ->
                    {ok, nil}
            end end
    ),
    _pipe@3 = gleam@result:'try'(
        _pipe@2,
        fun(_) -> case erlang:element(5, Config) of
                {some, Max@3} when erlang:element(5, Analysis) > Max@3 ->
                    {error,
                        {root_field_limit_exceeded,
                            erlang:element(5, Analysis),
                            Max@3}};

                _ ->
                    {ok, nil}
            end end
    ),
    gleam@result:map(_pipe@3, fun(_) -> Analysis end).
