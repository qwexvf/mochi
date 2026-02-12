-module(mochi_wisp@fast_parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/fast_parser.gleam").
-export([parse/1]).
-export_type([parse_error/0, token/0, position/0, token_with_pos/0, cursor/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type parse_error() :: {lex_error, binary()} |
    {unexpected_token, binary(), binary()} |
    {unexpected_e_o_f, binary()}.

-type token() :: bang |
    dollar |
    amp |
    left_paren |
    right_paren |
    spread |
    colon |
    equals |
    at |
    left_bracket |
    right_bracket |
    left_brace |
    right_brace |
    pipe |
    'query' |
    mutation |
    subscription |
    fragment_kw |
    on |
    true_kw |
    false_kw |
    null_kw |
    {name, binary()} |
    {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    e_o_f.

-type position() :: {position, integer(), integer()}.

-type token_with_pos() :: {token_with_pos, token(), position()}.

-type cursor() :: {cursor, list(token_with_pos())}.

-file("src/mochi_wisp/fast_parser.gleam", 99).
-spec fast_tokenize(binary()) -> {ok, list(token_with_pos())} |
    {error, binary()}.
fast_tokenize(Input) ->
    case mochi_fast_lexer_ffi:tokenize(Input) of
        {ok, Tokens} ->
            {ok, mochi_fast_parser_ffi:convert_tokens(Tokens)};

        {error, E} ->
            {error, mochi_fast_parser_ffi:format_error(E)}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 116).
-spec peek(cursor()) -> {ok, token_with_pos()} | {error, nil}.
peek(Cursor) ->
    case erlang:element(2, Cursor) of
        [First | _] ->
            {ok, First};

        [] ->
            {error, nil}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 123).
-spec advance(cursor()) -> {ok, {token_with_pos(), cursor()}} | {error, nil}.
advance(Cursor) ->
    case erlang:element(2, Cursor) of
        [First | Rest] ->
            {ok, {First, {cursor, Rest}}};

        [] ->
            {error, nil}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 146).
-spec tokens_match(token(), token()) -> boolean().
tokens_match(Actual, Expected) ->
    case {Actual, Expected} of
        {{name, _}, {name, _}} ->
            true;

        {{int_value, _}, {int_value, _}} ->
            true;

        {{float_value, _}, {float_value, _}} ->
            true;

        {{string_value, _}, {string_value, _}} ->
            true;

        {A, B} ->
            A =:= B
    end.

-file("src/mochi_wisp/fast_parser.gleam", 156).
-spec token_to_string(token()) -> binary().
token_to_string(Token) ->
    case Token of
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

        spread ->
            <<"..."/utf8>>;

        equals ->
            <<"="/utf8>>;

        at ->
            <<"@"/utf8>>;

        pipe ->
            <<"|"/utf8>>;

        amp ->
            <<"&"/utf8>>;

        'query' ->
            <<"query"/utf8>>;

        mutation ->
            <<"mutation"/utf8>>;

        subscription ->
            <<"subscription"/utf8>>;

        fragment_kw ->
            <<"fragment"/utf8>>;

        on ->
            <<"on"/utf8>>;

        true_kw ->
            <<"true"/utf8>>;

        false_kw ->
            <<"false"/utf8>>;

        null_kw ->
            <<"null"/utf8>>;

        {name, N} ->
            <<<<"Name("/utf8, N/binary>>/binary, ")"/utf8>>;

        {int_value, _} ->
            <<"Int"/utf8>>;

        {float_value, _} ->
            <<"Float"/utf8>>;

        {string_value, _} ->
            <<"String"/utf8>>;

        e_o_f ->
            <<"EOF"/utf8>>
    end.

-file("src/mochi_wisp/fast_parser.gleam", 130).
-spec expect(cursor(), token(), binary()) -> {ok, {token_with_pos(), cursor()}} |
    {error, parse_error()}.
expect(Cursor, Expected, Description) ->
    case advance(Cursor) of
        {ok, {Tok, New_cursor}} ->
            case tokens_match(erlang:element(2, Tok), Expected) of
                true ->
                    {ok, {Tok, New_cursor}};

                false ->
                    {error,
                        {unexpected_token,
                            Description,
                            token_to_string(erlang:element(2, Tok))}}
            end;

        {error, _} ->
            {error, {unexpected_e_o_f, Description}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 277).
-spec parse_operation_type(cursor()) -> {ok,
        {mochi@ast:operation_type(), cursor()}} |
    {error, parse_error()}.
parse_operation_type(Cursor) ->
    case advance(Cursor) of
        {ok, {{token_with_pos, 'query', _}, Cursor@1}} ->
            {ok, {'query', Cursor@1}};

        {ok, {{token_with_pos, mutation, _}, Cursor@2}} ->
            {ok, {mutation, Cursor@2}};

        {ok, {{token_with_pos, subscription, _}, Cursor@3}} ->
            {ok, {subscription, Cursor@3}};

        {ok, {{token_with_pos, Tok, _}, _}} ->
            {error,
                {unexpected_token,
                    <<"query/mutation/subscription"/utf8>>,
                    token_to_string(Tok)}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation type"/utf8>>}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 291).
-spec parse_optional_name(cursor()) -> {ok,
        {gleam@option:option(binary()), cursor()}} |
    {error, parse_error()}.
parse_optional_name(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, {name, N}, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    {ok, {{some, N}, Cursor@1}};

                {error, _} ->
                    {ok, {none, Cursor}}
            end;

        _ ->
            {ok, {none, Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 305).
-spec parse_name(cursor()) -> {ok, {binary(), cursor()}} |
    {error, parse_error()}.
parse_name(Cursor) ->
    case advance(Cursor) of
        {ok, {{token_with_pos, {name, N}, _}, Cursor@1}} ->
            {ok, {N, Cursor@1}};

        {ok, {{token_with_pos, Tok, _}, _}} ->
            {error, {unexpected_token, <<"name"/utf8>>, token_to_string(Tok)}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"name"/utf8>>}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 676).
-spec parse_non_null(cursor()) -> {ok, {boolean(), cursor()}} |
    {error, parse_error()}.
parse_non_null(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, bang, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    {ok, {true, Cursor@1}};

                {error, _} ->
                    {ok, {false, Cursor}}
            end;

        _ ->
            {ok, {false, Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 647).
-spec parse_type(cursor()) -> {ok, {mochi@ast:type(), cursor()}} |
    {error, parse_error()}.
parse_type(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, left_bracket, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    gleam@result:'try'(
                        parse_type(Cursor@1),
                        fun(_use0) ->
                            {Inner, Cursor@2} = _use0,
                            gleam@result:'try'(
                                expect(Cursor@2, right_bracket, <<"']'"/utf8>>),
                                fun(_use0@1) ->
                                    {_, Cursor@3} = _use0@1,
                                    gleam@result:'try'(
                                        parse_non_null(Cursor@3),
                                        fun(_use0@2) ->
                                            {Is_non_null, Cursor@4} = _use0@2,
                                            List_type = {list_type, Inner},
                                            case Is_non_null of
                                                true ->
                                                    {ok,
                                                        {{non_null_type,
                                                                List_type},
                                                            Cursor@4}};

                                                false ->
                                                    {ok, {List_type, Cursor@4}}
                                            end
                                        end
                                    )
                                end
                            )
                        end
                    );

                {error, _} ->
                    {error, {unexpected_e_o_f, <<"type"/utf8>>}}
            end;

        _ ->
            gleam@result:'try'(
                parse_name(Cursor),
                fun(_use0@3) ->
                    {Name, Cursor@5} = _use0@3,
                    gleam@result:'try'(
                        parse_non_null(Cursor@5),
                        fun(_use0@4) ->
                            {Is_non_null@1, Cursor@6} = _use0@4,
                            Named_type = {named_type, Name},
                            case Is_non_null@1 of
                                true ->
                                    {ok,
                                        {{non_null_type, Named_type}, Cursor@6}};

                                false ->
                                    {ok, {Named_type, Cursor@6}}
                            end
                        end
                    )
                end
            )
    end.

-file("src/mochi_wisp/fast_parser.gleam", 373).
-spec parse_inline_fragment(cursor()) -> {ok, {mochi@ast:selection(), cursor()}} |
    {error, parse_error()}.
parse_inline_fragment(Cursor) ->
    gleam@result:'try'(case peek(Cursor) of
            {ok, {token_with_pos, on, _}} ->
                case advance(Cursor) of
                    {ok, {_, Cursor@1}} ->
                        gleam@result:'try'(
                            parse_name(Cursor@1),
                            fun(_use0) ->
                                {Name, Cursor@2} = _use0,
                                {ok, {{some, Name}, Cursor@2}}
                            end
                        );

                    {error, _} ->
                        {ok, {none, Cursor}}
                end;

            _ ->
                {ok, {none, Cursor}}
        end, fun(_use0@1) ->
            {Type_condition, Cursor@3} = _use0@1,
            gleam@result:'try'(
                parse_directives(Cursor@3),
                fun(_use0@2) ->
                    {Directives, Cursor@4} = _use0@2,
                    gleam@result:'try'(
                        parse_selection_set(Cursor@4),
                        fun(_use0@3) ->
                            {Selection_set, Cursor@5} = _use0@3,
                            {ok,
                                {{inline_fragment,
                                        {inline_fragment_value,
                                            Type_condition,
                                            Directives,
                                            Selection_set}},
                                    Cursor@5}}
                        end
                    )
                end
            )
        end).

-file("src/mochi_wisp/fast_parser.gleam", 318).
-spec parse_selection_set(cursor()) -> {ok,
        {mochi@ast:selection_set(), cursor()}} |
    {error, parse_error()}.
parse_selection_set(Cursor) ->
    gleam@result:'try'(
        expect(Cursor, left_brace, <<"'{'"/utf8>>),
        fun(_use0) ->
            {_, Cursor@1} = _use0,
            gleam@result:'try'(
                parse_selections(Cursor@1, []),
                fun(_use0@1) ->
                    {Selections, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        expect(Cursor@2, right_brace, <<"'}'"/utf8>>),
                        fun(_use0@2) ->
                            {_, Cursor@3} = _use0@2,
                            {ok, {{selection_set, Selections}, Cursor@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 327).
-spec parse_selections(cursor(), list(mochi@ast:selection())) -> {ok,
        {list(mochi@ast:selection()), cursor()}} |
    {error, parse_error()}.
parse_selections(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_selection(Cursor),
                fun(_use0) ->
                    {Selection, Cursor@1} = _use0,
                    parse_selections(Cursor@1, [Selection | Acc])
                end
            );

        {error, _} ->
            {error, {unexpected_e_o_f, <<"selection or '}'"/utf8>>}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 341).
-spec parse_selection(cursor()) -> {ok, {mochi@ast:selection(), cursor()}} |
    {error, parse_error()}.
parse_selection(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, spread, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    case peek(Cursor@1) of
                        {ok, {token_with_pos, on, _}} ->
                            parse_inline_fragment(Cursor@1);

                        {ok, {token_with_pos, left_brace, _}} ->
                            parse_inline_fragment(Cursor@1);

                        {ok, {token_with_pos, {name, _}, _}} ->
                            parse_fragment_spread(Cursor@1);

                        _ ->
                            parse_inline_fragment(Cursor@1)
                    end;

                {error, _} ->
                    {error,
                        {unexpected_e_o_f,
                            <<"fragment spread or inline fragment"/utf8>>}}
            end;

        _ ->
            gleam@result:'try'(
                parse_field(Cursor),
                fun(_use0) ->
                    {Field, Cursor@2} = _use0,
                    {ok, {{field_selection, Field}, Cursor@2}}
                end
            )
    end.

-file("src/mochi_wisp/fast_parser.gleam", 437).
-spec parse_optional_selection_set(cursor()) -> {ok,
        {gleam@option:option(mochi@ast:selection_set()), cursor()}} |
    {error, parse_error()}.
parse_optional_selection_set(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, left_brace, _}} ->
            gleam@result:'try'(
                parse_selection_set(Cursor),
                fun(_use0) ->
                    {Selection_set, Cursor@1} = _use0,
                    {ok, {{some, Selection_set}, Cursor@1}}
                end
            );

        _ ->
            {ok, {none, Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 531).
-spec parse_list_values(cursor(), list(mochi@ast:value())) -> {ok,
        {list(mochi@ast:value()), cursor()}} |
    {error, parse_error()}.
parse_list_values(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, right_bracket, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_value(Cursor),
                fun(_use0) ->
                    {Value, Cursor@1} = _use0,
                    parse_list_values(Cursor@1, [Value | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 496).
-spec parse_value(cursor()) -> {ok, {mochi@ast:value(), cursor()}} |
    {error, parse_error()}.
parse_value(Cursor) ->
    case advance(Cursor) of
        {ok, {{token_with_pos, {int_value, N}, _}, Cursor@1}} ->
            {ok, {{int_value, N}, Cursor@1}};

        {ok, {{token_with_pos, {float_value, F}, _}, Cursor@2}} ->
            {ok, {{float_value, F}, Cursor@2}};

        {ok, {{token_with_pos, {string_value, S}, _}, Cursor@3}} ->
            {ok, {{string_value, S}, Cursor@3}};

        {ok, {{token_with_pos, true_kw, _}, Cursor@4}} ->
            {ok, {{boolean_value, true}, Cursor@4}};

        {ok, {{token_with_pos, false_kw, _}, Cursor@5}} ->
            {ok, {{boolean_value, false}, Cursor@5}};

        {ok, {{token_with_pos, null_kw, _}, Cursor@6}} ->
            {ok, {null_value, Cursor@6}};

        {ok, {{token_with_pos, dollar, _}, Cursor@7}} ->
            gleam@result:'try'(
                parse_name(Cursor@7),
                fun(_use0) ->
                    {Name, Cursor@8} = _use0,
                    {ok, {{variable_value, Name}, Cursor@8}}
                end
            );

        {ok, {{token_with_pos, left_bracket, _}, Cursor@9}} ->
            parse_list_value(Cursor@9);

        {ok, {{token_with_pos, left_brace, _}, Cursor@10}} ->
            parse_object_value(Cursor@10);

        {ok, {{token_with_pos, {name, N@1}, _}, Cursor@11}} ->
            {ok, {{enum_value, N@1}, Cursor@11}};

        {ok, {{token_with_pos, Tok, _}, _}} ->
            {error, {unexpected_token, <<"value"/utf8>>, token_to_string(Tok)}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"value"/utf8>>}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 525).
-spec parse_list_value(cursor()) -> {ok, {mochi@ast:value(), cursor()}} |
    {error, parse_error()}.
parse_list_value(Cursor) ->
    gleam@result:'try'(
        parse_list_values(Cursor, []),
        fun(_use0) ->
            {Values, Cursor@1} = _use0,
            gleam@result:'try'(
                expect(Cursor@1, right_bracket, <<"']'"/utf8>>),
                fun(_use0@1) ->
                    {_, Cursor@2} = _use0@1,
                    {ok, {{list_value, Values}, Cursor@2}}
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 485).
-spec parse_argument(cursor()) -> {ok, {mochi@ast:argument(), cursor()}} |
    {error, parse_error()}.
parse_argument(Cursor) ->
    gleam@result:'try'(
        parse_name(Cursor),
        fun(_use0) ->
            {Name, Cursor@1} = _use0,
            gleam@result:'try'(
                expect(Cursor@1, colon, <<"':'"/utf8>>),
                fun(_use0@1) ->
                    {_, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_value(Cursor@2),
                        fun(_use0@2) ->
                            {Value, Cursor@3} = _use0@2,
                            {ok, {{argument, Name, Value}, Cursor@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 471).
-spec parse_argument_list(cursor(), list(mochi@ast:argument())) -> {ok,
        {list(mochi@ast:argument()), cursor()}} |
    {error, parse_error()}.
parse_argument_list(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, right_paren, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_argument(Cursor),
                fun(_use0) ->
                    {Arg, Cursor@1} = _use0,
                    parse_argument_list(Cursor@1, [Arg | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 453).
-spec parse_arguments(cursor()) -> {ok, {list(mochi@ast:argument()), cursor()}} |
    {error, parse_error()}.
parse_arguments(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, left_paren, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    gleam@result:'try'(
                        parse_argument_list(Cursor@1, []),
                        fun(_use0) ->
                            {Args, Cursor@2} = _use0,
                            gleam@result:'try'(
                                expect(Cursor@2, right_paren, <<"')'"/utf8>>),
                                fun(_use0@1) ->
                                    {_, Cursor@3} = _use0@1,
                                    {ok, {Args, Cursor@3}}
                                end
                            )
                        end
                    );

                {error, _} ->
                    {ok, {[], Cursor}}
            end;

        _ ->
            {ok, {[], Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 565).
-spec parse_object_field(cursor()) -> {ok, {mochi@ast:object_field(), cursor()}} |
    {error, parse_error()}.
parse_object_field(Cursor) ->
    gleam@result:'try'(
        parse_name(Cursor),
        fun(_use0) ->
            {Name, Cursor@1} = _use0,
            gleam@result:'try'(
                expect(Cursor@1, colon, <<"':'"/utf8>>),
                fun(_use0@1) ->
                    {_, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_value(Cursor@2),
                        fun(_use0@2) ->
                            {Value, Cursor@3} = _use0@2,
                            {ok, {{object_field, Name, Value}, Cursor@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 551).
-spec parse_object_fields(cursor(), list(mochi@ast:object_field())) -> {ok,
        {list(mochi@ast:object_field()), cursor()}} |
    {error, parse_error()}.
parse_object_fields(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_object_field(Cursor),
                fun(_use0) ->
                    {Field, Cursor@1} = _use0,
                    parse_object_fields(Cursor@1, [Field | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 545).
-spec parse_object_value(cursor()) -> {ok, {mochi@ast:value(), cursor()}} |
    {error, parse_error()}.
parse_object_value(Cursor) ->
    gleam@result:'try'(
        parse_object_fields(Cursor, []),
        fun(_use0) ->
            {Fields, Cursor@1} = _use0,
            gleam@result:'try'(
                expect(Cursor@1, right_brace, <<"'}'"/utf8>>),
                fun(_use0@1) ->
                    {_, Cursor@2} = _use0@1,
                    {ok, {{object_value, Fields}, Cursor@2}}
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 630).
-spec parse_default_value(cursor()) -> {ok,
        {gleam@option:option(mochi@ast:value()), cursor()}} |
    {error, parse_error()}.
parse_default_value(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, equals, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    gleam@result:'try'(
                        parse_value(Cursor@1),
                        fun(_use0) ->
                            {Value, Cursor@2} = _use0,
                            {ok, {{some, Value}, Cursor@2}}
                        end
                    );

                {error, _} ->
                    {ok, {none, Cursor}}
            end;

        _ ->
            {ok, {none, Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 711).
-spec parse_directive(cursor()) -> {ok, {mochi@ast:directive(), cursor()}} |
    {error, parse_error()}.
parse_directive(Cursor) ->
    gleam@result:'try'(
        expect(Cursor, at, <<"'@'"/utf8>>),
        fun(_use0) ->
            {_, Cursor@1} = _use0,
            gleam@result:'try'(
                parse_name(Cursor@1),
                fun(_use0@1) ->
                    {Name, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_arguments(Cursor@2),
                        fun(_use0@2) ->
                            {Arguments, Cursor@3} = _use0@2,
                            {ok, {{directive, Name, Arguments}, Cursor@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 698).
-spec parse_directives_loop(cursor(), list(mochi@ast:directive())) -> {ok,
        {list(mochi@ast:directive()), cursor()}} |
    {error, parse_error()}.
parse_directives_loop(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, at, _}} ->
            gleam@result:'try'(
                parse_directive(Cursor),
                fun(_use0) ->
                    {Directive, Cursor@1} = _use0,
                    parse_directives_loop(Cursor@1, [Directive | Acc])
                end
            );

        _ ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 692).
-spec parse_directives(cursor()) -> {ok,
        {list(mochi@ast:directive()), cursor()}} |
    {error, parse_error()}.
parse_directives(Cursor) ->
    parse_directives_loop(Cursor, []).

-file("src/mochi_wisp/fast_parser.gleam", 224).
-spec parse_fragment_definition(cursor()) -> {ok,
        {mochi@ast:fragment(), cursor()}} |
    {error, parse_error()}.
parse_fragment_definition(Cursor) ->
    gleam@result:'try'(
        expect(Cursor, fragment_kw, <<"'fragment'"/utf8>>),
        fun(_use0) ->
            {_, Cursor@1} = _use0,
            gleam@result:'try'(
                parse_name(Cursor@1),
                fun(_use0@1) ->
                    {Name, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        expect(Cursor@2, on, <<"'on'"/utf8>>),
                        fun(_use0@2) ->
                            {_, Cursor@3} = _use0@2,
                            gleam@result:'try'(
                                parse_name(Cursor@3),
                                fun(_use0@3) ->
                                    {Type_condition, Cursor@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_directives(Cursor@4),
                                        fun(_use0@4) ->
                                            {Directives, Cursor@5} = _use0@4,
                                            gleam@result:'try'(
                                                parse_selection_set(Cursor@5),
                                                fun(_use0@5) ->
                                                    {Selection_set, Cursor@6} = _use0@5,
                                                    {ok,
                                                        {{fragment,
                                                                Name,
                                                                Type_condition,
                                                                Directives,
                                                                Selection_set},
                                                            Cursor@6}}
                                                end
                                            )
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

-file("src/mochi_wisp/fast_parser.gleam", 362).
-spec parse_fragment_spread(cursor()) -> {ok, {mochi@ast:selection(), cursor()}} |
    {error, parse_error()}.
parse_fragment_spread(Cursor) ->
    gleam@result:'try'(
        parse_name(Cursor),
        fun(_use0) ->
            {Name, Cursor@1} = _use0,
            gleam@result:'try'(
                parse_directives(Cursor@1),
                fun(_use0@1) ->
                    {Directives, Cursor@2} = _use0@1,
                    {ok,
                        {{fragment_spread,
                                {fragment_spread_value, Name, Directives}},
                            Cursor@2}}
                end
            )
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 402).
-spec parse_field(cursor()) -> {ok, {mochi@ast:field(), cursor()}} |
    {error, parse_error()}.
parse_field(Cursor) ->
    gleam@result:'try'(
        parse_name(Cursor),
        fun(_use0) ->
            {Name_or_alias, Cursor@1} = _use0,
            gleam@result:'try'(case peek(Cursor@1) of
                    {ok, {token_with_pos, colon, _}} ->
                        case advance(Cursor@1) of
                            {ok, {_, Cursor@2}} ->
                                gleam@result:'try'(
                                    parse_name(Cursor@2),
                                    fun(_use0@1) ->
                                        {Actual_name, Cursor@3} = _use0@1,
                                        {ok,
                                            {Actual_name,
                                                {some, Name_or_alias},
                                                Cursor@3}}
                                    end
                                );

                            {error, _} ->
                                {ok, {Name_or_alias, none, Cursor@1}}
                        end;

                    _ ->
                        {ok, {Name_or_alias, none, Cursor@1}}
                end, fun(_use0@2) ->
                    {Name, Alias, Cursor@4} = _use0@2,
                    gleam@result:'try'(
                        parse_arguments(Cursor@4),
                        fun(_use0@3) ->
                            {Arguments, Cursor@5} = _use0@3,
                            gleam@result:'try'(
                                parse_directives(Cursor@5),
                                fun(_use0@4) ->
                                    {Directives, Cursor@6} = _use0@4,
                                    gleam@result:'try'(
                                        parse_optional_selection_set(Cursor@6),
                                        fun(_use0@5) ->
                                            {Selection_set, Cursor@7} = _use0@5,
                                            {ok,
                                                {{field,
                                                        Alias,
                                                        Name,
                                                        Arguments,
                                                        Directives,
                                                        Selection_set},
                                                    Cursor@7}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end)
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 610).
-spec parse_variable_definition(cursor()) -> {ok,
        {mochi@ast:variable_definition(), cursor()}} |
    {error, parse_error()}.
parse_variable_definition(Cursor) ->
    gleam@result:'try'(
        expect(Cursor, dollar, <<"'$'"/utf8>>),
        fun(_use0) ->
            {_, Cursor@1} = _use0,
            gleam@result:'try'(
                parse_name(Cursor@1),
                fun(_use0@1) ->
                    {Name, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        expect(Cursor@2, colon, <<"':'"/utf8>>),
                        fun(_use0@2) ->
                            {_, Cursor@3} = _use0@2,
                            gleam@result:'try'(
                                parse_type(Cursor@3),
                                fun(_use0@3) ->
                                    {Var_type, Cursor@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_default_value(Cursor@4),
                                        fun(_use0@4) ->
                                            {Default, Cursor@5} = _use0@4,
                                            gleam@result:'try'(
                                                parse_directives(Cursor@5),
                                                fun(_use0@5) ->
                                                    {Directives, Cursor@6} = _use0@5,
                                                    {ok,
                                                        {{variable_definition,
                                                                Name,
                                                                Var_type,
                                                                Default,
                                                                Directives},
                                                            Cursor@6}}
                                                end
                                            )
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

-file("src/mochi_wisp/fast_parser.gleam", 596).
-spec parse_var_def_list(cursor(), list(mochi@ast:variable_definition())) -> {ok,
        {list(mochi@ast:variable_definition()), cursor()}} |
    {error, parse_error()}.
parse_var_def_list(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, right_paren, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_variable_definition(Cursor),
                fun(_use0) ->
                    {Def, Cursor@1} = _use0,
                    parse_var_def_list(Cursor@1, [Def | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 578).
-spec parse_variable_definitions(cursor()) -> {ok,
        {list(mochi@ast:variable_definition()), cursor()}} |
    {error, parse_error()}.
parse_variable_definitions(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, left_paren, _}} ->
            case advance(Cursor) of
                {ok, {_, Cursor@1}} ->
                    gleam@result:'try'(
                        parse_var_def_list(Cursor@1, []),
                        fun(_use0) ->
                            {Defs, Cursor@2} = _use0,
                            gleam@result:'try'(
                                expect(Cursor@2, right_paren, <<"')'"/utf8>>),
                                fun(_use0@1) ->
                                    {_, Cursor@3} = _use0@1,
                                    {ok, {Defs, Cursor@3}}
                                end
                            )
                        end
                    );

                {error, _} ->
                    {ok, {[], Cursor}}
            end;

        _ ->
            {ok, {[], Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 244).
-spec parse_operation_definition(cursor()) -> {ok,
        {mochi@ast:operation(), cursor()}} |
    {error, parse_error()}.
parse_operation_definition(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, left_brace, _}} ->
            gleam@result:'try'(
                parse_selection_set(Cursor),
                fun(_use0) ->
                    {Selection_set, Cursor@1} = _use0,
                    {ok, {{shorthand_query, Selection_set}, Cursor@1}}
                end
            );

        {ok, {token_with_pos, 'query', _}} ->
            gleam@result:'try'(
                parse_operation_type(Cursor),
                fun(_use0@1) ->
                    {Op_type, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Cursor@2),
                        fun(_use0@2) ->
                            {Name, Cursor@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Cursor@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Cursor@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_directives(Cursor@4),
                                        fun(_use0@4) ->
                                            {Directives, Cursor@5} = _use0@4,
                                            gleam@result:'try'(
                                                parse_selection_set(Cursor@5),
                                                fun(_use0@5) ->
                                                    {Selection_set@1, Cursor@6} = _use0@5,
                                                    {ok,
                                                        {{operation,
                                                                Op_type,
                                                                Name,
                                                                Variable_defs,
                                                                Directives,
                                                                Selection_set@1},
                                                            Cursor@6}}
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_pos, mutation, _}} ->
            gleam@result:'try'(
                parse_operation_type(Cursor),
                fun(_use0@1) ->
                    {Op_type, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Cursor@2),
                        fun(_use0@2) ->
                            {Name, Cursor@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Cursor@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Cursor@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_directives(Cursor@4),
                                        fun(_use0@4) ->
                                            {Directives, Cursor@5} = _use0@4,
                                            gleam@result:'try'(
                                                parse_selection_set(Cursor@5),
                                                fun(_use0@5) ->
                                                    {Selection_set@1, Cursor@6} = _use0@5,
                                                    {ok,
                                                        {{operation,
                                                                Op_type,
                                                                Name,
                                                                Variable_defs,
                                                                Directives,
                                                                Selection_set@1},
                                                            Cursor@6}}
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_pos, subscription, _}} ->
            gleam@result:'try'(
                parse_operation_type(Cursor),
                fun(_use0@1) ->
                    {Op_type, Cursor@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Cursor@2),
                        fun(_use0@2) ->
                            {Name, Cursor@3} = _use0@2,
                            gleam@result:'try'(
                                parse_variable_definitions(Cursor@3),
                                fun(_use0@3) ->
                                    {Variable_defs, Cursor@4} = _use0@3,
                                    gleam@result:'try'(
                                        parse_directives(Cursor@4),
                                        fun(_use0@4) ->
                                            {Directives, Cursor@5} = _use0@4,
                                            gleam@result:'try'(
                                                parse_selection_set(Cursor@5),
                                                fun(_use0@5) ->
                                                    {Selection_set@1, Cursor@6} = _use0@5,
                                                    {ok,
                                                        {{operation,
                                                                Op_type,
                                                                Name,
                                                                Variable_defs,
                                                                Directives,
                                                                Selection_set@1},
                                                            Cursor@6}}
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_pos, Tok, _}} ->
            {error,
                {unexpected_token,
                    <<"operation or '{'"/utf8>>,
                    token_to_string(Tok)}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation or '{'"/utf8>>}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 211).
-spec parse_definition(cursor()) -> {ok, {mochi@ast:definition(), cursor()}} |
    {error, parse_error()}.
parse_definition(Cursor) ->
    case peek(Cursor) of
        {ok, {token_with_pos, fragment_kw, _}} ->
            gleam@result:'try'(
                parse_fragment_definition(Cursor),
                fun(_use0) ->
                    {Fragment, Cursor@1} = _use0,
                    {ok, {{fragment_definition, Fragment}, Cursor@1}}
                end
            );

        _ ->
            gleam@result:'try'(
                parse_operation_definition(Cursor),
                fun(_use0@1) ->
                    {Operation, Cursor@2} = _use0@1,
                    {ok, {{operation_definition, Operation}, Cursor@2}}
                end
            )
    end.

-file("src/mochi_wisp/fast_parser.gleam", 197).
-spec parse_definitions(cursor(), list(mochi@ast:definition())) -> {ok,
        {list(mochi@ast:definition()), cursor()}} |
    {error, parse_error()}.
parse_definitions(Cursor, Acc) ->
    case peek(Cursor) of
        {ok, {token_with_pos, e_o_f, _}} ->
            {ok, {lists:reverse(Acc), Cursor}};

        {ok, _} ->
            gleam@result:'try'(
                parse_definition(Cursor),
                fun(_use0) ->
                    {Def, Cursor@1} = _use0,
                    parse_definitions(Cursor@1, [Def | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Cursor}}
    end.

-file("src/mochi_wisp/fast_parser.gleam", 192).
-spec parse_document(cursor()) -> {ok, {mochi@ast:document(), cursor()}} |
    {error, parse_error()}.
parse_document(Cursor) ->
    gleam@result:'try'(
        parse_definitions(Cursor, []),
        fun(_use0) ->
            {Definitions, Cursor@1} = _use0,
            {ok, {{document, Definitions}, Cursor@1}}
        end
    ).

-file("src/mochi_wisp/fast_parser.gleam", 82).
?DOC(" Parse GraphQL query using fast binary lexer\n").
-spec parse(binary()) -> {ok, mochi@ast:document()} | {error, parse_error()}.
parse(Input) ->
    case fast_tokenize(Input) of
        {ok, Tokens} ->
            Cursor = {cursor, Tokens},
            case parse_document(Cursor) of
                {ok, {Doc, _}} ->
                    {ok, Doc};

                {error, E} ->
                    {error, E}
            end;

        {error, Msg} ->
            {error, {lex_error, Msg}}
    end.
