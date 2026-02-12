-module(mochi@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/parser.gleam").
-export([parse/1]).
-export_type([parse_error/0, parser/0]).

-type parse_error() :: {lex_error, mochi@lexer:lexer_error()} |
    {unexpected_token, binary(), mochi@lexer:token(), mochi@lexer:position()} |
    {unexpected_e_o_f, binary()}.

-type parser() :: {parser, list(mochi@lexer:token_with_position()), integer()}.

-file("src/mochi/parser.gleam", 390).
-spec get_token_at_helper(
    list(mochi@lexer:token_with_position()),
    integer(),
    integer()
) -> {ok, mochi@lexer:token_with_position()} | {error, nil}.
get_token_at_helper(Tokens, Target_index, Current_index) ->
    case Tokens of
        [] ->
            {error, nil};

        [First | _] when Current_index =:= Target_index ->
            {ok, First};

        [_ | Rest] ->
            get_token_at_helper(Rest, Target_index, Current_index + 1)
    end.

-file("src/mochi/parser.gleam", 383).
-spec get_token_at(list(mochi@lexer:token_with_position()), integer()) -> {ok,
        mochi@lexer:token_with_position()} |
    {error, nil}.
get_token_at(Tokens, Index) ->
    get_token_at_helper(Tokens, Index, 0).

-file("src/mochi/parser.gleam", 372).
-spec peek_token(parser()) -> {ok, mochi@lexer:token_with_position()} |
    {error, nil}.
peek_token(Parser) ->
    get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)).

-file("src/mochi/parser.gleam", 376).
-spec consume_token(parser()) -> {ok,
        {mochi@lexer:token_with_position(), parser()}} |
    {error, nil}.
consume_token(Parser) ->
    case get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)) of
        {ok, Token} ->
            {ok,
                {Token,
                    {parser,
                        erlang:element(2, Parser),
                        erlang:element(3, Parser) + 1}}};

        {error, _} ->
            {error, nil}
    end.

-file("src/mochi/parser.gleam", 126).
-spec parse_operation_type(parser()) -> {ok,
        {mochi@ast:operation_type(), parser()}} |
    {error, parse_error()}.
parse_operation_type(Parser) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, 'query', _}, Parser@1}} ->
            {ok, {'query', Parser@1}};

        {ok, {{token_with_position, mutation, _}, Parser@2}} ->
            {ok, {mutation, Parser@2}};

        {ok, {{token_with_position, subscription, _}, Parser@3}} ->
            {ok, {subscription, Parser@3}};

        {ok, {{token_with_position, Token, Position}, _}} ->
            {error,
                {unexpected_token,
                    <<"query, mutation, or subscription"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation type"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 142).
-spec parse_optional_name(parser()) -> {ok,
        {gleam@option:option(binary()), parser()}} |
    {error, parse_error()}.
parse_optional_name(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, {name, Name}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"name"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    {ok, {{some, Name}, Parser@1}}
                end
            );

        _ ->
            {ok, {none, Parser}}
    end.

-file("src/mochi/parser.gleam", 360).
-spec parse_name_from_parser(parser()) -> {ok, {binary(), parser()}} |
    {error, parse_error()}.
parse_name_from_parser(Parser) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, {name, Name}, _}, Parser@1}} ->
            {ok, {Name, Parser@1}};

        {ok, {{token_with_position, Token, Position}, _}} ->
            {error, {unexpected_token, <<"name"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"name"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 402).
-spec expect_token(parser(), mochi@lexer:token(), binary()) -> {ok,
        {mochi@lexer:token_with_position(), parser()}} |
    {error, parse_error()}.
expect_token(Parser, Expected, Description) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, Token, Position}, Parser@1}} ->
            case Token =:= Expected of
                true ->
                    {ok, {{token_with_position, Token, Position}, Parser@1}};

                false ->
                    {error, {unexpected_token, Description, Token, Position}}
            end;

        {error, _} ->
            {error, {unexpected_e_o_f, Description}}
    end.

-file("src/mochi/parser.gleam", 533).
-spec parse_optional_non_null(parser(), mochi@ast:type()) -> {ok,
        {mochi@ast:type(), parser()}} |
    {error, parse_error()}.
parse_optional_non_null(Parser, Base_type) ->
    case peek_token(Parser) of
        {ok, {token_with_position, bang, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'!'"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    {ok, {{non_null_type, Base_type}, Parser@1}}
                end
            );

        _ ->
            {ok, {Base_type, Parser}}
    end.

-file("src/mochi/parser.gleam", 497).
-spec parse_type(parser()) -> {ok, {mochi@ast:type(), parser()}} |
    {error, parse_error()}.
parse_type(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_bracket, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'['"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_type(Parser@1),
                        fun(_use0@1) ->
                            {Inner_type, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@2,
                                    right_bracket,
                                    <<"']' to close list type"/utf8>>
                                ),
                                fun(_use0@2) ->
                                    {_, Parser@3} = _use0@2,
                                    gleam@result:'try'(
                                        parse_optional_non_null(
                                            Parser@3,
                                            {list_type, Inner_type}
                                        ),
                                        fun(_use0@3) ->
                                            {Final_type, Parser@4} = _use0@3,
                                            {ok, {Final_type, Parser@4}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_name_from_parser(Parser),
                fun(_use0@4) ->
                    {Name, Parser@5} = _use0@4,
                    gleam@result:'try'(
                        parse_optional_non_null(Parser@5, {named_type, Name}),
                        fun(_use0@5) ->
                            {Final_type@1, Parser@6} = _use0@5,
                            {ok, {Final_type@1, Parser@6}}
                        end
                    )
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token, <<"type name or '['"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"type"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 199).
-spec parse_fragment_spread_or_inline(parser()) -> {ok,
        {mochi@ast:selection(), parser()}} |
    {error, parse_error()}.
parse_fragment_spread_or_inline(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, spread, <<"'...' for fragment"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            case peek_token(Parser@1) of
                {ok, {token_with_position, on, _}} ->
                    gleam@result:'try'(
                        begin
                            _pipe = consume_token(Parser@1),
                            gleam@result:map_error(
                                _pipe,
                                fun(_) ->
                                    {unexpected_e_o_f, <<"'on' keyword"/utf8>>}
                                end
                            )
                        end,
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                parse_name_from_parser(Parser@2),
                                fun(_use0@2) ->
                                    {Type_name, Parser@3} = _use0@2,
                                    gleam@result:'try'(
                                        parse_selection_set(Parser@3),
                                        fun(_use0@3) ->
                                            {Selection_set, Parser@4} = _use0@3,
                                            {ok,
                                                {{inline_fragment,
                                                        {inline_fragment_value,
                                                            {some, Type_name},
                                                            [],
                                                            Selection_set}},
                                                    Parser@4}}
                                        end
                                    )
                                end
                            )
                        end
                    );

                {ok, {token_with_position, left_brace, _}} ->
                    gleam@result:'try'(
                        parse_selection_set(Parser@1),
                        fun(_use0@4) ->
                            {Selection_set@1, Parser@5} = _use0@4,
                            {ok,
                                {{inline_fragment,
                                        {inline_fragment_value,
                                            none,
                                            [],
                                            Selection_set@1}},
                                    Parser@5}}
                        end
                    );

                {ok, {token_with_position, {name, Name}, _}} ->
                    gleam@result:'try'(
                        begin
                            _pipe@1 = consume_token(Parser@1),
                            gleam@result:map_error(
                                _pipe@1,
                                fun(_) ->
                                    {unexpected_e_o_f, <<"fragment name"/utf8>>}
                                end
                            )
                        end,
                        fun(_use0@5) ->
                            {_, Parser@6} = _use0@5,
                            {ok,
                                {{fragment_spread,
                                        {fragment_spread_value, Name, []}},
                                    Parser@6}}
                        end
                    );

                {ok, {token_with_position, Token, Position}} ->
                    {error,
                        {unexpected_token,
                            <<"'on', '{', or fragment name after '...'"/utf8>>,
                            Token,
                            Position}};

                {error, _} ->
                    {error,
                        {unexpected_e_o_f,
                            <<"'on', '{', or fragment name after '...'"/utf8>>}}
            end
        end
    ).

-file("src/mochi/parser.gleam", 157).
-spec parse_selection_set(parser()) -> {ok,
        {mochi@ast:selection_set(), parser()}} |
    {error, parse_error()}.
parse_selection_set(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, left_brace, <<"'{' to start selection set"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_selections(Parser@1, []),
                fun(_use0@1) ->
                    {Selections, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(
                            Parser@2,
                            right_brace,
                            <<"'}' to end selection set"/utf8>>
                        ),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            {ok, {{selection_set, Selections}, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/parser.gleam", 174).
-spec parse_selections(parser(), list(mochi@ast:selection())) -> {ok,
        {list(mochi@ast:selection()), parser()}} |
    {error, parse_error()}.
parse_selections(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, _} ->
            gleam@result:'try'(
                parse_selection(Parser),
                fun(_use0) ->
                    {Selection, Parser@1} = _use0,
                    parse_selections(Parser@1, [Selection | Acc])
                end
            );

        {error, _} ->
            {error, {unexpected_e_o_f, <<"selection or '}'"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 189).
-spec parse_selection(parser()) -> {ok, {mochi@ast:selection(), parser()}} |
    {error, parse_error()}.
parse_selection(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, spread, _}} ->
            parse_fragment_spread_or_inline(Parser);

        _ ->
            _pipe = parse_field(Parser),
            gleam@result:map(
                _pipe,
                fun(Result) ->
                    {{field_selection, erlang:element(1, Result)},
                        erlang:element(2, Result)}
                end
            )
    end.

-file("src/mochi/parser.gleam", 64).
-spec parse_fragment_definition(parser()) -> {ok,
        {mochi@ast:fragment(), parser()}} |
    {error, parse_error()}.
parse_fragment_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, fragment, <<"'fragment' keyword"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name_from_parser(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, on, <<"'on' keyword"/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_name_from_parser(Parser@3),
                                fun(_use0@3) ->
                                    {Type_condition, Parser@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_selection_set(Parser@4),
                                        fun(_use0@4) ->
                                            {Selection_set, Parser@5} = _use0@4,
                                            {ok,
                                                {{fragment,
                                                        Name,
                                                        Type_condition,
                                                        [],
                                                        Selection_set},
                                                    Parser@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/parser.gleam", 346).
-spec parse_optional_selection_set(parser()) -> {gleam@option:option(mochi@ast:selection_set()),
    parser()}.
parse_optional_selection_set(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_brace, _}} ->
            case parse_selection_set(Parser) of
                {ok, {Ss, P}} ->
                    {{some, Ss}, P};

                {error, _} ->
                    {none, Parser}
            end;

        _ ->
            {none, Parser}
    end.

-file("src/mochi/parser.gleam", 664).
-spec parse_list_values(parser(), list(mochi@ast:value())) -> {ok,
        {list(mochi@ast:value()), parser()}} |
    {error, parse_error()}.
parse_list_values(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_bracket, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        _ ->
            gleam@result:'try'(
                parse_value(Parser),
                fun(_use0) ->
                    {Value, Parser@1} = _use0,
                    parse_list_values(Parser@1, [Value | Acc])
                end
            )
    end.

-file("src/mochi/parser.gleam", 569).
-spec parse_value(parser()) -> {ok, {mochi@ast:value(), parser()}} |
    {error, parse_error()}.
parse_value(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, dollar, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'$'"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_name_from_parser(Parser@1),
                        fun(_use0@1) ->
                            {Name, Parser@2} = _use0@1,
                            {ok, {{variable_value, Name}, Parser@2}}
                        end
                    )
                end
            );

        {ok, {token_with_position, {int_value, Value}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@1 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@1,
                        fun(_) -> {unexpected_e_o_f, <<"int value"/utf8>>} end
                    )
                end,
                fun(_use0@2) ->
                    {_, Parser@3} = _use0@2,
                    {ok, {{int_value, Value}, Parser@3}}
                end
            );

        {ok, {token_with_position, {float_value, Value@1}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@2 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@2,
                        fun(_) -> {unexpected_e_o_f, <<"float value"/utf8>>} end
                    )
                end,
                fun(_use0@3) ->
                    {_, Parser@4} = _use0@3,
                    {ok, {{float_value, Value@1}, Parser@4}}
                end
            );

        {ok, {token_with_position, {string_value, Value@2}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@3 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@3,
                        fun(_) ->
                            {unexpected_e_o_f, <<"string value"/utf8>>}
                        end
                    )
                end,
                fun(_use0@4) ->
                    {_, Parser@5} = _use0@4,
                    {ok, {{string_value, Value@2}, Parser@5}}
                end
            );

        {ok, {token_with_position, true_keyword, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@4 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@4,
                        fun(_) -> {unexpected_e_o_f, <<"true"/utf8>>} end
                    )
                end,
                fun(_use0@5) ->
                    {_, Parser@6} = _use0@5,
                    {ok, {{boolean_value, true}, Parser@6}}
                end
            );

        {ok, {token_with_position, false_keyword, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@5 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@5,
                        fun(_) -> {unexpected_e_o_f, <<"false"/utf8>>} end
                    )
                end,
                fun(_use0@6) ->
                    {_, Parser@7} = _use0@6,
                    {ok, {{boolean_value, false}, Parser@7}}
                end
            );

        {ok, {token_with_position, null_keyword, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@6 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@6,
                        fun(_) -> {unexpected_e_o_f, <<"null"/utf8>>} end
                    )
                end,
                fun(_use0@7) ->
                    {_, Parser@8} = _use0@7,
                    {ok, {null_value, Parser@8}}
                end
            );

        {ok, {token_with_position, left_bracket, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@7 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@7,
                        fun(_) -> {unexpected_e_o_f, <<"'['"/utf8>>} end
                    )
                end,
                fun(_use0@8) ->
                    {_, Parser@9} = _use0@8,
                    gleam@result:'try'(
                        parse_list_values(Parser@9, []),
                        fun(_use0@9) ->
                            {Values, Parser@10} = _use0@9,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@10,
                                    right_bracket,
                                    <<"']' to close list value"/utf8>>
                                ),
                                fun(_use0@10) ->
                                    {_, Parser@11} = _use0@10,
                                    {ok, {{list_value, Values}, Parser@11}}
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, left_brace, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@8 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@8,
                        fun(_) -> {unexpected_e_o_f, <<"'{'"/utf8>>} end
                    )
                end,
                fun(_use0@11) ->
                    {_, Parser@12} = _use0@11,
                    gleam@result:'try'(
                        parse_object_fields(Parser@12, []),
                        fun(_use0@12) ->
                            {Fields, Parser@13} = _use0@12,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@13,
                                    right_brace,
                                    <<"'}' to close object value"/utf8>>
                                ),
                                fun(_use0@13) ->
                                    {_, Parser@14} = _use0@13,
                                    {ok, {{object_value, Fields}, Parser@14}}
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, {name, Value@3}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@9 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@9,
                        fun(_) -> {unexpected_e_o_f, <<"enum value"/utf8>>} end
                    )
                end,
                fun(_use0@14) ->
                    {_, Parser@15} = _use0@14,
                    {ok, {{enum_value, Value@3}, Parser@15}}
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error, {unexpected_token, <<"value"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"value"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 549).
-spec parse_optional_default_value(parser()) -> {ok,
        {gleam@option:option(mochi@ast:value()), parser()}} |
    {error, parse_error()}.
parse_optional_default_value(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, equals, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'='"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_value(Parser@1),
                        fun(_use0@1) ->
                            {Value, Parser@2} = _use0@1,
                            {ok, {{some, Value}, Parser@2}}
                        end
                    )
                end
            );

        _ ->
            {ok, {none, Parser}}
    end.

-file("src/mochi/parser.gleam", 462).
-spec parse_variable_definition(parser()) -> {ok,
        {mochi@ast:variable_definition(), parser()}} |
    {error, parse_error()}.
parse_variable_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, dollar, <<"'$' for variable"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name_from_parser(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(
                            Parser@2,
                            colon,
                            <<"':' after variable name"/utf8>>
                        ),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_type(Parser@3),
                                fun(_use0@3) ->
                                    {Var_type, Parser@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_optional_default_value(Parser@4),
                                        fun(_use0@4) ->
                                            {Default_value, Parser@5} = _use0@4,
                                            {ok,
                                                {{variable_definition,
                                                        Name,
                                                        Var_type,
                                                        Default_value,
                                                        []},
                                                    Parser@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/parser.gleam", 445).
-spec parse_variable_definitions_list(
    parser(),
    list(mochi@ast:variable_definition())
) -> {ok, {list(mochi@ast:variable_definition()), parser()}} |
    {error, parse_error()}.
parse_variable_definitions_list(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_paren, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {token_with_position, dollar, _}} ->
            gleam@result:'try'(
                parse_variable_definition(Parser),
                fun(_use0) ->
                    {Var_def, Parser@1} = _use0,
                    parse_variable_definitions_list(Parser@1, [Var_def | Acc])
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"variable definition or ')'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"variable definition or ')'"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 422).
-spec parse_variable_definitions(parser()) -> {ok,
        {list(mochi@ast:variable_definition()), parser()}} |
    {error, parse_error()}.
parse_variable_definitions(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_paren, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'('"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_variable_definitions_list(Parser@1, []),
                        fun(_use0@1) ->
                            {Defs, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@2,
                                    right_paren,
                                    <<"')' to end variable definitions"/utf8>>
                                ),
                                fun(_use0@2) ->
                                    {_, Parser@3} = _use0@2,
                                    {ok, {Defs, Parser@3}}
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {ok, {[], Parser}}
    end.

-file("src/mochi/parser.gleam", 92).
-spec parse_operation_definition(parser()) -> {ok,
        {mochi@ast:operation(), parser()}} |
    {error, parse_error()}.
parse_operation_definition(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_brace, _}} ->
            gleam@result:'try'(
                parse_selection_set(Parser),
                fun(_use0) ->
                    {Selection_set, Parser@1} = _use0,
                    {ok, {{shorthand_query, Selection_set}, Parser@1}}
                end
            );

        {ok, {token_with_position, 'query', _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Parser@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Parser@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_selection_set(Parser@4),
                                        fun(_use0@4) ->
                                            {Selection_set@1, Parser@5} = _use0@4,
                                            {ok,
                                                {{operation,
                                                        Op_type,
                                                        Name,
                                                        Variable_defs,
                                                        [],
                                                        Selection_set@1},
                                                    Parser@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, mutation, _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Parser@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Parser@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_selection_set(Parser@4),
                                        fun(_use0@4) ->
                                            {Selection_set@1, Parser@5} = _use0@4,
                                            {ok,
                                                {{operation,
                                                        Op_type,
                                                        Name,
                                                        Variable_defs,
                                                        [],
                                                        Selection_set@1},
                                                    Parser@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, subscription, _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Parser@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Parser@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_selection_set(Parser@4),
                                        fun(_use0@4) ->
                                            {Selection_set@1, Parser@5} = _use0@4,
                                            {ok,
                                                {{operation,
                                                        Op_type,
                                                        Name,
                                                        Variable_defs,
                                                        [],
                                                        Selection_set@1},
                                                    Parser@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"operation or selection set"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation or selection set"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 49).
-spec parse_definition(parser()) -> {ok, {mochi@ast:definition(), parser()}} |
    {error, parse_error()}.
parse_definition(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, fragment, _}} ->
            _pipe = parse_fragment_definition(Parser),
            gleam@result:map(
                _pipe,
                fun(Result) ->
                    {{fragment_definition, erlang:element(1, Result)},
                        erlang:element(2, Result)}
                end
            );

        _ ->
            _pipe@1 = parse_operation_definition(Parser),
            gleam@result:map(
                _pipe@1,
                fun(Result@1) ->
                    {{operation_definition, erlang:element(1, Result@1)},
                        erlang:element(2, Result@1)}
                end
            )
    end.

-file("src/mochi/parser.gleam", 34).
-spec parse_definitions(parser(), list(mochi@ast:definition())) -> {ok,
        {list(mochi@ast:definition()), parser()}} |
    {error, parse_error()}.
parse_definitions(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, e_o_f, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, _} ->
            gleam@result:'try'(
                parse_definition(Parser),
                fun(_use0) ->
                    {Definition, Parser@1} = _use0,
                    parse_definitions(Parser@1, [Definition | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Parser}}
    end.

-file("src/mochi/parser.gleam", 29).
-spec parse_document(parser()) -> {ok, {mochi@ast:document(), parser()}} |
    {error, parse_error()}.
parse_document(Parser) ->
    gleam@result:'try'(
        parse_definitions(Parser, []),
        fun(_use0) ->
            {Definitions, Parser@1} = _use0,
            {ok, {{document, Definitions}, Parser@1}}
        end
    ).

-file("src/mochi/parser.gleam", 22).
-spec parse(binary()) -> {ok, mochi@ast:document()} | {error, parse_error()}.
parse(Input) ->
    gleam@result:'try'(
        begin
            _pipe = mochi@lexer:tokenize(Input),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {lex_error, Field@0} end
            )
        end,
        fun(Tokens) ->
            Parser = {parser, Tokens, 0},
            _pipe@1 = parse_document(Parser),
            gleam@result:map(
                _pipe@1,
                fun(Result) -> erlang:element(1, Result) end
            )
        end
    ).

-file("src/mochi/parser.gleam", 678).
-spec parse_object_fields(parser(), list(mochi@ast:object_field())) -> {ok,
        {list(mochi@ast:object_field()), parser()}} |
    {error, parse_error()}.
parse_object_fields(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_name_from_parser(Parser),
                fun(_use0) ->
                    {Name, Parser@1} = _use0,
                    gleam@result:'try'(
                        expect_token(
                            Parser@1,
                            colon,
                            <<"':' after field name"/utf8>>
                        ),
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                parse_value(Parser@2),
                                fun(_use0@2) ->
                                    {Value, Parser@3} = _use0@2,
                                    parse_object_fields(
                                        Parser@3,
                                        [{object_field, Name, Value} | Acc]
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"object field or '}'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"object field or '}'"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 726).
-spec parse_arguments_list(parser(), list(mochi@ast:argument())) -> {ok,
        {list(mochi@ast:argument()), parser()}} |
    {error, parse_error()}.
parse_arguments_list(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_paren, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_name_from_parser(Parser),
                fun(_use0) ->
                    {Name, Parser@1} = _use0,
                    gleam@result:'try'(
                        expect_token(
                            Parser@1,
                            colon,
                            <<"':' after argument name"/utf8>>
                        ),
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                parse_value(Parser@2),
                                fun(_use0@2) ->
                                    {Value, Parser@3} = _use0@2,
                                    parse_arguments_list(
                                        Parser@3,
                                        [{argument, Name, Value} | Acc]
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token, <<"argument or ')'"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"argument or ')'"/utf8>>}}
    end.

-file("src/mochi/parser.gleam", 705).
-spec parse_arguments(parser()) -> {ok, {list(mochi@ast:argument()), parser()}} |
    {error, parse_error()}.
parse_arguments(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_paren, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"'('"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_arguments_list(Parser@1, []),
                        fun(_use0@1) ->
                            {Args, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@2,
                                    right_paren,
                                    <<"')' to close arguments"/utf8>>
                                ),
                                fun(_use0@2) ->
                                    {_, Parser@3} = _use0@2,
                                    {ok, {Args, Parser@3}}
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {ok, {[], Parser}}
    end.

-file("src/mochi/parser.gleam", 329).
-spec parse_directive(parser()) -> {ok, {mochi@ast:directive(), parser()}} |
    {error, parse_error()}.
parse_directive(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, at, <<"'@' for directive"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name_from_parser(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_arguments(Parser@2),
                        fun(_use0@2) ->
                            {Arguments, Parser@3} = _use0@2,
                            {ok, {{directive, Name, Arguments}, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/parser.gleam", 316).
-spec parse_directives_list(parser(), list(mochi@ast:directive())) -> {ok,
        {list(mochi@ast:directive()), parser()}} |
    {error, parse_error()}.
parse_directives_list(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, at, _}} ->
            gleam@result:'try'(
                parse_directive(Parser),
                fun(_use0) ->
                    {Directive, Parser@1} = _use0,
                    parse_directives_list(Parser@1, [Directive | Acc])
                end
            );

        _ ->
            {ok, {lists:reverse(Acc), Parser}}
    end.

-file("src/mochi/parser.gleam", 310).
-spec parse_directives(parser()) -> {ok,
        {list(mochi@ast:directive()), parser()}} |
    {error, parse_error()}.
parse_directives(Parser) ->
    parse_directives_list(Parser, []).

-file("src/mochi/parser.gleam", 260).
-spec parse_field(parser()) -> {ok, {mochi@ast:field(), parser()}} |
    {error, parse_error()}.
parse_field(Parser) ->
    gleam@result:'try'(
        parse_name_from_parser(Parser),
        fun(_use0) ->
            {First_name, Parser@1} = _use0,
            case peek_token(Parser@1) of
                {ok, {token_with_position, colon, _}} ->
                    gleam@result:'try'(
                        begin
                            _pipe = consume_token(Parser@1),
                            gleam@result:map_error(
                                _pipe,
                                fun(_) ->
                                    {unexpected_e_o_f, <<"colon"/utf8>>}
                                end
                            )
                        end,
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                parse_name_from_parser(Parser@2),
                                fun(_use0@2) ->
                                    {Second_name, Parser@3} = _use0@2,
                                    gleam@result:'try'(
                                        parse_arguments(Parser@3),
                                        fun(_use0@3) ->
                                            {Arguments, Parser@4} = _use0@3,
                                            gleam@result:'try'(
                                                parse_directives(Parser@4),
                                                fun(_use0@4) ->
                                                    {Directives, Parser@5} = _use0@4,
                                                    {Selection_set, Parser@6} = parse_optional_selection_set(
                                                        Parser@5
                                                    ),
                                                    {ok,
                                                        {{field,
                                                                {some,
                                                                    First_name},
                                                                Second_name,
                                                                Arguments,
                                                                Directives,
                                                                Selection_set},
                                                            Parser@6}}
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    );

                _ ->
                    gleam@result:'try'(
                        parse_arguments(Parser@1),
                        fun(_use0@5) ->
                            {Arguments@1, Parser@7} = _use0@5,
                            gleam@result:'try'(
                                parse_directives(Parser@7),
                                fun(_use0@6) ->
                                    {Directives@1, Parser@8} = _use0@6,
                                    {Selection_set@1, Parser@9} = parse_optional_selection_set(
                                        Parser@8
                                    ),
                                    {ok,
                                        {{field,
                                                none,
                                                First_name,
                                                Arguments@1,
                                                Directives@1,
                                                Selection_set@1},
                                            Parser@9}}
                                end
                            )
                        end
                    )
            end
        end
    ).
