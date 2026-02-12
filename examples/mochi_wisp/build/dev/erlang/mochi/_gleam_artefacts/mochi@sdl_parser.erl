-module(mochi@sdl_parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/sdl_parser.gleam").
-export([parse_sdl/1]).
-export_type([s_d_l_parse_error/0, s_d_l_parser/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type s_d_l_parse_error() :: {s_d_l_lex_error,
        mochi@sdl_lexer:s_d_l_lexer_error()} |
    {unexpected_token,
        binary(),
        mochi@sdl_lexer:s_d_l_token(),
        mochi@sdl_lexer:position()} |
    {unexpected_e_o_f, binary()} |
    {invalid_type_definition, binary(), mochi@sdl_lexer:position()}.

-type s_d_l_parser() :: {s_d_l_parser,
        list(mochi@sdl_lexer:s_d_l_token_with_position()),
        integer()}.

-file("src/mochi/sdl_parser.gleam", 668).
-spec get_token_at_helper(
    list(mochi@sdl_lexer:s_d_l_token_with_position()),
    integer(),
    integer()
) -> {ok, mochi@sdl_lexer:s_d_l_token_with_position()} | {error, nil}.
get_token_at_helper(Tokens, Target_index, Current_index) ->
    case Tokens of
        [] ->
            {error, nil};

        [First | _] when Current_index =:= Target_index ->
            {ok, First};

        [_ | Rest] ->
            get_token_at_helper(Rest, Target_index, Current_index + 1)
    end.

-file("src/mochi/sdl_parser.gleam", 661).
-spec get_token_at(list(mochi@sdl_lexer:s_d_l_token_with_position()), integer()) -> {ok,
        mochi@sdl_lexer:s_d_l_token_with_position()} |
    {error, nil}.
get_token_at(Tokens, Target_index) ->
    get_token_at_helper(Tokens, Target_index, 0).

-file("src/mochi/sdl_parser.gleam", 632).
-spec peek_token(s_d_l_parser()) -> {ok,
        mochi@sdl_lexer:s_d_l_token_with_position()} |
    {error, nil}.
peek_token(Parser) ->
    get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)).

-file("src/mochi/sdl_parser.gleam", 636).
-spec consume_token(s_d_l_parser()) -> {ok,
        {mochi@sdl_lexer:s_d_l_token_with_position(), s_d_l_parser()}} |
    {error, nil}.
consume_token(Parser) ->
    case get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)) of
        {ok, Token} ->
            {ok,
                {Token,
                    {s_d_l_parser,
                        erlang:element(2, Parser),
                        erlang:element(3, Parser) + 1}}};

        {error, _} ->
            {error, nil}
    end.

-file("src/mochi/sdl_parser.gleam", 252).
-spec parse_name(s_d_l_parser()) -> {ok, {binary(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_name(Parser) ->
    case consume_token(Parser) of
        {ok, {{s_d_l_token_with_position, {name, Name}, _}, Parser@1}} ->
            {ok, {Name, Parser@1}};

        {ok, {{s_d_l_token_with_position, Token, Position}, _}} ->
            {error, {unexpected_token, <<"name"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"name"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 277).
-spec parse_implements_interfaces(s_d_l_parser(), list(binary())) -> {ok,
        {list(binary()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_implements_interfaces(Parser, Acc) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {Interface_name, Parser@1} = _use0,
            New_acc = [Interface_name | Acc],
            case peek_token(Parser@1) of
                {ok, {s_d_l_token_with_position, amp, _}} ->
                    gleam@result:'try'(
                        begin
                            _pipe = consume_token(Parser@1),
                            gleam@result:map_error(
                                _pipe,
                                fun(_) -> {unexpected_e_o_f, <<"&"/utf8>>} end
                            )
                        end,
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            parse_implements_interfaces(Parser@2, New_acc)
                        end
                    );

                _ ->
                    {ok, {lists:reverse(New_acc), Parser@1}}
            end
        end
    ).

-file("src/mochi/sdl_parser.gleam", 262).
-spec parse_optional_implements(s_d_l_parser()) -> {ok,
        {list(binary()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_optional_implements(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, implements, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"implements"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    parse_implements_interfaces(Parser@1, [])
                end
            );

        _ ->
            {ok, {[], Parser}}
    end.

-file("src/mochi/sdl_parser.gleam", 475).
-spec parse_value(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:s_d_l_value(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_value(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, {int_value, Value}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"int value"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    {ok, {{int_value, Value}, Parser@1}}
                end
            );

        {ok, {s_d_l_token_with_position, {float_value, Value@1}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@1 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@1,
                        fun(_) -> {unexpected_e_o_f, <<"float value"/utf8>>} end
                    )
                end,
                fun(_use0@1) ->
                    {_, Parser@2} = _use0@1,
                    {ok, {{float_value, Value@1}, Parser@2}}
                end
            );

        {ok, {s_d_l_token_with_position, {string_value, Value@2}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@2 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@2,
                        fun(_) ->
                            {unexpected_e_o_f, <<"string value"/utf8>>}
                        end
                    )
                end,
                fun(_use0@2) ->
                    {_, Parser@3} = _use0@2,
                    {ok, {{string_value, Value@2}, Parser@3}}
                end
            );

        {ok, {s_d_l_token_with_position, {boolean_value, Value@3}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@3 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@3,
                        fun(_) ->
                            {unexpected_e_o_f, <<"boolean value"/utf8>>}
                        end
                    )
                end,
                fun(_use0@3) ->
                    {_, Parser@4} = _use0@3,
                    {ok, {{boolean_value, Value@3}, Parser@4}}
                end
            );

        {ok, {s_d_l_token_with_position, {name, Name}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@4 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@4,
                        fun(_) -> {unexpected_e_o_f, <<"enum value"/utf8>>} end
                    )
                end,
                fun(_use0@4) ->
                    {_, Parser@5} = _use0@4,
                    {ok, {{enum_value, Name}, Parser@5}}
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error, {unexpected_token, <<"value"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"value"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 460).
-spec parse_optional_default_value(s_d_l_parser()) -> {ok,
        {gleam@option:option(mochi@sdl_ast:s_d_l_value()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_optional_default_value(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, equals, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"="/utf8>>} end
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

-file("src/mochi/sdl_parser.gleam", 527).
-spec parse_union_member_types_helper(s_d_l_parser(), list(binary())) -> {ok,
        {list(binary()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_union_member_types_helper(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, pipe, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"|"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_name(Parser@1),
                        fun(_use0@1) ->
                            {Member, Parser@2} = _use0@1,
                            parse_union_member_types_helper(
                                Parser@2,
                                [Member | Acc]
                            )
                        end
                    )
                end
            );

        _ ->
            {ok, {lists:reverse(Acc), Parser}}
    end.

-file("src/mochi/sdl_parser.gleam", 520).
-spec parse_union_member_types(s_d_l_parser()) -> {ok,
        {list(binary()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_union_member_types(Parser) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {First_member, Parser@1} = _use0,
            parse_union_member_types_helper(Parser@1, [First_member])
        end
    ).

-file("src/mochi/sdl_parser.gleam", 571).
-spec parse_enum_value_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:enum_value_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_enum_value_definition(Parser) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {Name, Parser@1} = _use0,
            {ok, {{enum_value_def, Name, none, []}, Parser@1}}
        end
    ).

-file("src/mochi/sdl_parser.gleam", 552).
-spec parse_enum_values(s_d_l_parser(), list(mochi@sdl_ast:enum_value_def())) -> {ok,
        {list(mochi@sdl_ast:enum_value_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_enum_values(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {s_d_l_token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_enum_value_definition(Parser),
                fun(_use0) ->
                    {Enum_value, Parser@1} = _use0,
                    parse_enum_values(Parser@1, [Enum_value | Acc])
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"enum value or '}'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"enum value or '}'"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 646).
-spec expect_token(s_d_l_parser(), mochi@sdl_lexer:s_d_l_token(), binary()) -> {ok,
        {mochi@sdl_lexer:s_d_l_token_with_position(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
expect_token(Parser, Expected, Description) ->
    case consume_token(Parser) of
        {ok, {{s_d_l_token_with_position, Token, Position}, Parser@1}} when Token =:= Expected ->
            {ok, {{s_d_l_token_with_position, Token, Position}, Parser@1}};

        {ok, {{s_d_l_token_with_position, Token@1, Position@1}, _}} ->
            {error, {unexpected_token, Description, Token@1, Position@1}};

        {error, _} ->
            {error, {unexpected_e_o_f, Description}}
    end.

-file("src/mochi/sdl_parser.gleam", 176).
?DOC(" Parse: union SearchResult = User | Post\n").
-spec parse_union_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:union_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_union_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, union, <<"union"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, equals, <<"="/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_union_member_types(Parser@3),
                                fun(_use0@3) ->
                                    {Members, Parser@4} = _use0@3,
                                    {ok,
                                        {{union_type_def,
                                                Name,
                                                none,
                                                [],
                                                Members},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 196).
?DOC(" Parse: scalar DateTime\n").
-spec parse_scalar_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:scalar_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_scalar_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, scalar, <<"scalar"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    {ok, {{scalar_type_def, Name, none, []}, Parser@2}}
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 343).
-spec parse_type(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:s_d_l_type(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_type(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, left_bracket, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"["/utf8>>} end
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
                                    <<"]"/utf8>>
                                ),
                                fun(_use0@2) ->
                                    {_, Parser@3} = _use0@2,
                                    List_type = {list_type, Inner_type},
                                    case peek_token(Parser@3) of
                                        {ok,
                                            {s_d_l_token_with_position, bang, _}} ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe@1 = consume_token(
                                                        Parser@3
                                                    ),
                                                    gleam@result:map_error(
                                                        _pipe@1,
                                                        fun(_) ->
                                                            {unexpected_e_o_f,
                                                                <<"!"/utf8>>}
                                                        end
                                                    )
                                                end,
                                                fun(_use0@3) ->
                                                    {_, Parser@4} = _use0@3,
                                                    {ok,
                                                        {{non_null_type,
                                                                List_type},
                                                            Parser@4}}
                                                end
                                            );

                                        _ ->
                                            {ok, {List_type, Parser@3}}
                                    end
                                end
                            )
                        end
                    )
                end
            );

        {ok, {s_d_l_token_with_position, {name, Name}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe@2 = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe@2,
                        fun(_) -> {unexpected_e_o_f, <<"name"/utf8>>} end
                    )
                end,
                fun(_use0@4) ->
                    {_, Parser@5} = _use0@4,
                    Named_type = {named_type, Name},
                    case peek_token(Parser@5) of
                        {ok, {s_d_l_token_with_position, bang, _}} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@3 = consume_token(Parser@5),
                                    gleam@result:map_error(
                                        _pipe@3,
                                        fun(_) ->
                                            {unexpected_e_o_f, <<"!"/utf8>>}
                                        end
                                    )
                                end,
                                fun(_use0@5) ->
                                    {_, Parser@6} = _use0@5,
                                    {ok,
                                        {{non_null_type, Named_type}, Parser@6}}
                                end
                            );

                        _ ->
                            {ok, {Named_type, Parser@5}}
                    end
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error, {unexpected_token, <<"type"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"type"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 438).
-spec parse_argument_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:argument_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_argument_definition(Parser) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {Name, Parser@1} = _use0,
            gleam@result:'try'(
                expect_token(Parser@1, colon, <<":"/utf8>>),
                fun(_use0@1) ->
                    {_, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_type(Parser@2),
                        fun(_use0@2) ->
                            {Arg_type, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_optional_default_value(Parser@3),
                                fun(_use0@3) ->
                                    {Default_value, Parser@4} = _use0@3,
                                    {ok,
                                        {{argument_def,
                                                Name,
                                                none,
                                                Arg_type,
                                                Default_value,
                                                []},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 421).
-spec parse_argument_definitions(
    s_d_l_parser(),
    list(mochi@sdl_ast:argument_def())
) -> {ok, {list(mochi@sdl_ast:argument_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_argument_definitions(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, right_paren, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {s_d_l_token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_argument_definition(Parser),
                fun(_use0) ->
                    {Argument, Parser@1} = _use0,
                    parse_argument_definitions(Parser@1, [Argument | Acc])
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"argument definition or ')'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"argument definition or ')'"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 399).
-spec parse_optional_arguments_definition(s_d_l_parser()) -> {ok,
        {list(mochi@sdl_ast:argument_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_optional_arguments_definition(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, left_paren, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"("/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    gleam@result:'try'(
                        parse_argument_definitions(Parser@1, []),
                        fun(_use0@1) ->
                            {Arguments, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                expect_token(
                                    Parser@2,
                                    right_paren,
                                    <<")"/utf8>>
                                ),
                                fun(_use0@2) ->
                                    {_, Parser@3} = _use0@2,
                                    {ok, {Arguments, Parser@3}}
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {ok, {[], Parser}}
    end.

-file("src/mochi/sdl_parser.gleam", 321).
-spec parse_field_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:field_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_field_definition(Parser) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {Name, Parser@1} = _use0,
            gleam@result:'try'(
                parse_optional_arguments_definition(Parser@1),
                fun(_use0@1) ->
                    {Arguments, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, colon, <<":"/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_type(Parser@3),
                                fun(_use0@3) ->
                                    {Field_type, Parser@4} = _use0@3,
                                    {ok,
                                        {{field_def,
                                                Name,
                                                none,
                                                Arguments,
                                                Field_type,
                                                []},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 304).
-spec parse_field_definitions(s_d_l_parser(), list(mochi@sdl_ast:field_def())) -> {ok,
        {list(mochi@sdl_ast:field_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_field_definitions(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {s_d_l_token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_field_definition(Parser),
                fun(_use0) ->
                    {Field, Parser@1} = _use0,
                    parse_field_definitions(Parser@1, [Field | Acc])
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"field definition or '}'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"field definition or '}'"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 295).
-spec parse_fields_definition(s_d_l_parser()) -> {ok,
        {list(mochi@sdl_ast:field_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_fields_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, left_brace, <<"{"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_field_definitions(Parser@1, []),
                fun(_use0@1) ->
                    {Fields, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, right_brace, <<"}"/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            {ok, {Fields, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 123).
?DOC(" Parse: type User { ... }\n").
-spec parse_object_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:object_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_object_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, type, <<"type"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_implements(Parser@2),
                        fun(_use0@2) ->
                            {Interfaces, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_fields_definition(Parser@3),
                                fun(_use0@3) ->
                                    {Fields, Parser@4} = _use0@3,
                                    {ok,
                                        {{object_type_def,
                                                Name,
                                                none,
                                                Interfaces,
                                                [],
                                                Fields},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 153).
?DOC(" Parse: interface Node { ... }\n").
-spec parse_interface_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:interface_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_interface_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, interface, <<"interface"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_fields_definition(Parser@2),
                        fun(_use0@2) ->
                            {Fields, Parser@3} = _use0@2,
                            {ok,
                                {{interface_type_def, Name, none, [], Fields},
                                    Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 543).
-spec parse_enum_values_definition(s_d_l_parser()) -> {ok,
        {list(mochi@sdl_ast:enum_value_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_enum_values_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, left_brace, <<"{"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_enum_values(Parser@1, []),
                fun(_use0@1) ->
                    {Values, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, right_brace, <<"}"/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            {ok, {Values, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 213).
?DOC(" Parse: enum Status { ACTIVE INACTIVE }\n").
-spec parse_enum_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:enum_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_enum_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, enum, <<"enum"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_enum_values_definition(Parser@2),
                        fun(_use0@2) ->
                            {Values, Parser@3} = _use0@2,
                            {ok,
                                {{enum_type_def, Name, none, [], Values},
                                    Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 608).
-spec parse_input_field_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:input_field_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_input_field_definition(Parser) ->
    gleam@result:'try'(
        parse_name(Parser),
        fun(_use0) ->
            {Name, Parser@1} = _use0,
            gleam@result:'try'(
                expect_token(Parser@1, colon, <<":"/utf8>>),
                fun(_use0@1) ->
                    {_, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_type(Parser@2),
                        fun(_use0@2) ->
                            {Field_type, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_optional_default_value(Parser@3),
                                fun(_use0@3) ->
                                    {Default_value, Parser@4} = _use0@3,
                                    {ok,
                                        {{input_field_def,
                                                Name,
                                                none,
                                                Field_type,
                                                Default_value,
                                                []},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 591).
-spec parse_input_fields(s_d_l_parser(), list(mochi@sdl_ast:input_field_def())) -> {ok,
        {list(mochi@sdl_ast:input_field_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_input_fields(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, {s_d_l_token_with_position, {name, _}, _}} ->
            gleam@result:'try'(
                parse_input_field_definition(Parser),
                fun(_use0) ->
                    {Field, Parser@1} = _use0,
                    parse_input_fields(Parser@1, [Field | Acc])
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"input field or '}'"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"input field or '}'"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 582).
-spec parse_input_fields_definition(s_d_l_parser()) -> {ok,
        {list(mochi@sdl_ast:input_field_def()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_input_fields_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, left_brace, <<"{"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_input_fields(Parser@1, []),
                fun(_use0@1) ->
                    {Fields, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(Parser@2, right_brace, <<"}"/utf8>>),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            {ok, {Fields, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 232).
?DOC(" Parse: input CreateUserInput { ... }\n").
-spec parse_input_object_type_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:input_object_type_def(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_input_object_type_definition(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, input, <<"input"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_name(Parser@1),
                fun(_use0@1) ->
                    {Name, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_input_fields_definition(Parser@2),
                        fun(_use0@2) ->
                            {Fields, Parser@3} = _use0@2,
                            {ok,
                                {{input_object_type_def, Name, none, [], Fields},
                                    Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/sdl_parser.gleam", 57).
-spec parse_type_system_definition(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:type_system_definition(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_type_system_definition(Parser) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, type, _}} ->
            _pipe = parse_object_type_definition(Parser),
            gleam@result:map(
                _pipe,
                fun(Result) ->
                    {{type_definition,
                            {object_type_definition, erlang:element(1, Result)}},
                        erlang:element(2, Result)}
                end
            );

        {ok, {s_d_l_token_with_position, interface, _}} ->
            _pipe@1 = parse_interface_type_definition(Parser),
            gleam@result:map(
                _pipe@1,
                fun(Result@1) ->
                    {{type_definition,
                            {interface_type_definition,
                                erlang:element(1, Result@1)}},
                        erlang:element(2, Result@1)}
                end
            );

        {ok, {s_d_l_token_with_position, union, _}} ->
            _pipe@2 = parse_union_type_definition(Parser),
            gleam@result:map(
                _pipe@2,
                fun(Result@2) ->
                    {{type_definition,
                            {union_type_definition, erlang:element(1, Result@2)}},
                        erlang:element(2, Result@2)}
                end
            );

        {ok, {s_d_l_token_with_position, scalar, _}} ->
            _pipe@3 = parse_scalar_type_definition(Parser),
            gleam@result:map(
                _pipe@3,
                fun(Result@3) ->
                    {{type_definition,
                            {scalar_type_definition,
                                erlang:element(1, Result@3)}},
                        erlang:element(2, Result@3)}
                end
            );

        {ok, {s_d_l_token_with_position, enum, _}} ->
            _pipe@4 = parse_enum_type_definition(Parser),
            gleam@result:map(
                _pipe@4,
                fun(Result@4) ->
                    {{type_definition,
                            {enum_type_definition, erlang:element(1, Result@4)}},
                        erlang:element(2, Result@4)}
                end
            );

        {ok, {s_d_l_token_with_position, input, _}} ->
            _pipe@5 = parse_input_object_type_definition(Parser),
            gleam@result:map(
                _pipe@5,
                fun(Result@5) ->
                    {{type_definition,
                            {input_object_type_definition,
                                erlang:element(1, Result@5)}},
                        erlang:element(2, Result@5)}
                end
            );

        {ok, {s_d_l_token_with_position, Token, Position}} ->
            {error,
                {unexpected_token, <<"type definition"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"type definition"/utf8>>}}
    end.

-file("src/mochi/sdl_parser.gleam", 40).
-spec parse_definitions(
    s_d_l_parser(),
    list(mochi@sdl_ast:type_system_definition())
) -> {ok, {list(mochi@sdl_ast:type_system_definition()), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_definitions(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {s_d_l_token_with_position, e_o_f, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, _} ->
            gleam@result:'try'(
                parse_type_system_definition(Parser),
                fun(_use0) ->
                    {Definition, Parser@1} = _use0,
                    parse_definitions(Parser@1, [Definition | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Parser}}
    end.

-file("src/mochi/sdl_parser.gleam", 33).
-spec parse_document(s_d_l_parser()) -> {ok,
        {mochi@sdl_ast:s_d_l_document(), s_d_l_parser()}} |
    {error, s_d_l_parse_error()}.
parse_document(Parser) ->
    gleam@result:'try'(
        parse_definitions(Parser, []),
        fun(_use0) ->
            {Definitions, Parser@1} = _use0,
            {ok, {{s_d_l_document, Definitions}, Parser@1}}
        end
    ).

-file("src/mochi/sdl_parser.gleam", 24).
?DOC(" Parse SDL string into an AST\n").
-spec parse_sdl(binary()) -> {ok, mochi@sdl_ast:s_d_l_document()} |
    {error, s_d_l_parse_error()}.
parse_sdl(Input) ->
    gleam@result:'try'(
        begin
            _pipe = mochi@sdl_lexer:tokenize_sdl(Input),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {s_d_l_lex_error, Field@0} end
            )
        end,
        fun(Tokens) ->
            Parser = {s_d_l_parser, Tokens, 0},
            _pipe@1 = parse_document(Parser),
            gleam@result:map(
                _pipe@1,
                fun(Result) -> erlang:element(1, Result) end
            )
        end
    ).
