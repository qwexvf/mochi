-module(glance).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glance.gleam").
-export([precedence/1, module/1]).
-export_type([definition/1, attribute/0, module_/0, function_/0, span/0, statement/0, assignment_kind/0, use_pattern/0, pattern/0, expression/0, clause/0, bit_string_segment_option/1, binary_operator/0, fn_parameter/0, function_parameter/0, assignment_name/0, import/0, constant/0, unqualified_import/0, publicity/0, type_alias/0, custom_type/0, variant/0, record_update_field/1, variant_field/0, field/1, type/0, error/0, unqualified_imports/0, pattern_constructor_arguments/0, parse_expression_unit_context/0, parsed_list/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type definition(DUG) :: {definition, list(attribute()), DUG}.

-type attribute() :: {attribute, binary(), list(expression())}.

-type module_() :: {module,
        list(definition(import())),
        list(definition(custom_type())),
        list(definition(type_alias())),
        list(definition(constant())),
        list(definition(function_()))}.

-type function_() :: {function,
        span(),
        binary(),
        publicity(),
        list(function_parameter()),
        gleam@option:option(type()),
        list(statement())}.

-type span() :: {span, integer(), integer()}.

-type statement() :: {use, span(), list(use_pattern()), expression()} |
    {assignment,
        span(),
        assignment_kind(),
        pattern(),
        gleam@option:option(type()),
        expression()} |
    {assert, span(), expression(), gleam@option:option(expression())} |
    {expression, expression()}.

-type assignment_kind() :: 'let' |
    {let_assert, gleam@option:option(expression())}.

-type use_pattern() :: {use_pattern, pattern(), gleam@option:option(type())}.

-type pattern() :: {pattern_int, span(), binary()} |
    {pattern_float, span(), binary()} |
    {pattern_string, span(), binary()} |
    {pattern_discard, span(), binary()} |
    {pattern_variable, span(), binary()} |
    {pattern_tuple, span(), list(pattern())} |
    {pattern_list, span(), list(pattern()), gleam@option:option(pattern())} |
    {pattern_assignment, span(), pattern(), binary()} |
    {pattern_concatenate,
        span(),
        binary(),
        gleam@option:option(assignment_name()),
        assignment_name()} |
    {pattern_bit_string,
        span(),
        list({pattern(), list(bit_string_segment_option(pattern()))})} |
    {pattern_variant,
        span(),
        gleam@option:option(binary()),
        binary(),
        list(field(pattern())),
        boolean()}.

-type expression() :: {int, span(), binary()} |
    {float, span(), binary()} |
    {string, span(), binary()} |
    {variable, span(), binary()} |
    {negate_int, span(), expression()} |
    {negate_bool, span(), expression()} |
    {block, span(), list(statement())} |
    {panic, span(), gleam@option:option(expression())} |
    {todo, span(), gleam@option:option(expression())} |
    {tuple, span(), list(expression())} |
    {list, span(), list(expression()), gleam@option:option(expression())} |
    {fn,
        span(),
        list(fn_parameter()),
        gleam@option:option(type()),
        list(statement())} |
    {record_update,
        span(),
        gleam@option:option(binary()),
        binary(),
        expression(),
        list(record_update_field(expression()))} |
    {field_access, span(), expression(), binary()} |
    {call, span(), expression(), list(field(expression()))} |
    {tuple_index, span(), expression(), integer()} |
    {fn_capture,
        span(),
        gleam@option:option(binary()),
        expression(),
        list(field(expression())),
        list(field(expression()))} |
    {bit_string,
        span(),
        list({expression(), list(bit_string_segment_option(expression()))})} |
    {'case', span(), list(expression()), list(clause())} |
    {binary_operator, span(), binary_operator(), expression(), expression()} |
    {echo, span(), gleam@option:option(expression())}.

-type clause() :: {clause,
        list(list(pattern())),
        gleam@option:option(expression()),
        expression()}.

-type bit_string_segment_option(DUH) :: bytes_option |
    int_option |
    float_option |
    bits_option |
    utf8_option |
    utf16_option |
    utf32_option |
    utf8_codepoint_option |
    utf16_codepoint_option |
    utf32_codepoint_option |
    signed_option |
    unsigned_option |
    big_option |
    little_option |
    native_option |
    {size_value_option, DUH} |
    {size_option, integer()} |
    {unit_option, integer()}.

-type binary_operator() :: 'and' |
    'or' |
    eq |
    not_eq |
    lt_int |
    lt_eq_int |
    lt_float |
    lt_eq_float |
    gt_eq_int |
    gt_int |
    gt_eq_float |
    gt_float |
    pipe |
    add_int |
    add_float |
    sub_int |
    sub_float |
    mult_int |
    mult_float |
    div_int |
    div_float |
    remainder_int |
    concatenate.

-type fn_parameter() :: {fn_parameter,
        assignment_name(),
        gleam@option:option(type())}.

-type function_parameter() :: {function_parameter,
        gleam@option:option(binary()),
        assignment_name(),
        gleam@option:option(type())}.

-type assignment_name() :: {named, binary()} | {discarded, binary()}.

-type import() :: {import,
        span(),
        binary(),
        gleam@option:option(assignment_name()),
        list(unqualified_import()),
        list(unqualified_import())}.

-type constant() :: {constant,
        span(),
        binary(),
        publicity(),
        gleam@option:option(type()),
        expression()}.

-type unqualified_import() :: {unqualified_import,
        binary(),
        gleam@option:option(binary())}.

-type publicity() :: public | private.

-type type_alias() :: {type_alias,
        span(),
        binary(),
        publicity(),
        list(binary()),
        type()}.

-type custom_type() :: {custom_type,
        span(),
        binary(),
        publicity(),
        boolean(),
        list(binary()),
        list(variant())}.

-type variant() :: {variant, binary(), list(variant_field()), list(attribute())}.

-type record_update_field(DUI) :: {record_update_field,
        binary(),
        gleam@option:option(DUI)}.

-type variant_field() :: {labelled_variant_field, type(), binary()} |
    {unlabelled_variant_field, type()}.

-type field(DUJ) :: {labelled_field, binary(), DUJ} |
    {shorthand_field, binary()} |
    {unlabelled_field, DUJ}.

-type type() :: {named_type,
        span(),
        binary(),
        gleam@option:option(binary()),
        list(type())} |
    {tuple_type, span(), list(type())} |
    {function_type, span(), list(type()), type()} |
    {variable_type, span(), binary()} |
    {hole_type, span(), binary()}.

-type error() :: unexpected_end_of_input |
    {unexpected_token, glexer@token:token(), glexer:position()}.

-type unqualified_imports() :: {unqualified_imports,
        list(unqualified_import()),
        list(unqualified_import()),
        integer(),
        list({glexer@token:token(), glexer:position()})}.

-type pattern_constructor_arguments() :: {pattern_constructor_arguments,
        list(field(pattern())),
        boolean(),
        integer(),
        list({glexer@token:token(), glexer:position()})}.

-type parse_expression_unit_context() :: regular_expression_unit |
    expression_unit_after_pipe.

-type parsed_list(DUK) :: {parsed_list,
        list(DUK),
        gleam@option:option(DUK),
        list({glexer@token:token(), glexer:position()}),
        integer()}.

-file("src/glance.gleam", 212).
-spec precedence(binary_operator()) -> integer().
precedence(Operator) ->
    case Operator of
        'or' ->
            1;

        'and' ->
            2;

        eq ->
            3;

        not_eq ->
            3;

        lt_int ->
            4;

        lt_eq_int ->
            4;

        lt_float ->
            4;

        lt_eq_float ->
            4;

        gt_eq_int ->
            4;

        gt_int ->
            4;

        gt_eq_float ->
            4;

        gt_float ->
            4;

        concatenate ->
            5;

        pipe ->
            6;

        add_int ->
            7;

        add_float ->
            7;

        sub_int ->
            7;

        sub_float ->
            7;

        mult_int ->
            8;

        mult_float ->
            8;

        div_int ->
            8;

        div_float ->
            8;

        remainder_int ->
            8
    end.

-file("src/glance.gleam", 391).
-spec push_variant(custom_type(), variant()) -> custom_type().
push_variant(Custom_type, Variant) ->
    {custom_type,
        erlang:element(2, Custom_type),
        erlang:element(3, Custom_type),
        erlang:element(4, Custom_type),
        erlang:element(5, Custom_type),
        erlang:element(6, Custom_type),
        [Variant | erlang:element(7, Custom_type)]}.

-file("src/glance.gleam", 395).
-spec expect(
    glexer@token:token(),
    list({glexer@token:token(), glexer:position()}),
    fun((glexer:position(), list({glexer@token:token(), glexer:position()})) -> {ok,
            DUS} |
        {error, error()})
) -> {ok, DUS} | {error, error()}.
expect(Expected, Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, Position} | Tokens@1] when Token =:= Expected ->
            Next(Position, Tokens@1);

        [{Other, Position@1} | _] ->
            {error, {unexpected_token, Other, Position@1}}
    end.

-file("src/glance.gleam", 408).
-spec expect_upper_name(
    list({glexer@token:token(), glexer:position()}),
    fun((binary(), integer(), list({glexer@token:token(), glexer:position()})) -> {ok,
            DUX} |
        {error, error()})
) -> {ok, DUX} | {error, error()}.
expect_upper_name(Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{upper_name, Name}, {position, End}} | Tokens@1] ->
            Next(Name, End, Tokens@1);

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 419).
-spec expect_name(
    list({glexer@token:token(), glexer:position()}),
    fun((binary(), list({glexer@token:token(), glexer:position()})) -> {ok, DVC} |
        {error, error()})
) -> {ok, DVC} | {error, error()}.
expect_name(Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{name, Name}, _} | Tokens@1] ->
            Next(Name, Tokens@1);

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 556).
-spec module_name(
    binary(),
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {binary(), integer(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
module_name(Name, End, Tokens) ->
    case Tokens of
        [{slash, _}, {{name, S}, {position, I}} | Tokens@1] when Name =/= <<""/utf8>> ->
            End@1 = I + erlang:byte_size(S),
            module_name(
                <<<<Name/binary, "/"/utf8>>/binary, S/binary>>,
                End@1,
                Tokens@1
            );

        [{{name, S@1}, {position, I@1}} | Tokens@2] when Name =:= <<""/utf8>> ->
            End@2 = I@1 + erlang:byte_size(S@1),
            module_name(S@1, End@2, Tokens@2);

        [] when Name =:= <<""/utf8>> ->
            {error, unexpected_end_of_input};

        [{Other, Position} | _] when Name =:= <<""/utf8>> ->
            {error, {unexpected_token, Other, Position}};

        _ ->
            {ok, {Name, End, Tokens}}
    end.

-file("src/glance.gleam", 1057).
-spec unexpected_error(list({glexer@token:token(), glexer:position()})) -> {ok,
        any()} |
    {error, error()}.
unexpected_error(Tokens) ->
    case Tokens of
        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}};

        [] ->
            {error, unexpected_end_of_input}
    end.

-file("src/glance.gleam", 1064).
-spec binary_operator(glexer@token:token()) -> {ok, binary_operator()} |
    {error, nil}.
binary_operator(Token) ->
    case Token of
        amper_amper ->
            {ok, 'and'};

        equal_equal ->
            {ok, eq};

        greater ->
            {ok, gt_int};

        greater_dot ->
            {ok, gt_float};

        greater_equal ->
            {ok, gt_eq_int};

        greater_equal_dot ->
            {ok, gt_eq_float};

        less ->
            {ok, lt_int};

        less_dot ->
            {ok, lt_float};

        less_equal ->
            {ok, lt_eq_int};

        less_equal_dot ->
            {ok, lt_eq_float};

        less_greater ->
            {ok, concatenate};

        minus ->
            {ok, sub_int};

        minus_dot ->
            {ok, sub_float};

        not_equal ->
            {ok, not_eq};

        percent ->
            {ok, remainder_int};

        v_bar_v_bar ->
            {ok, 'or'};

        pipe ->
            {ok, pipe};

        plus ->
            {ok, add_int};

        plus_dot ->
            {ok, add_float};

        slash ->
            {ok, div_int};

        slash_dot ->
            {ok, div_float};

        star ->
            {ok, mult_int};

        star_dot ->
            {ok, mult_float};

        _ ->
            {error, nil}
    end.

-file("src/glance.gleam", 1093).
-spec pop_binary_operator(list({glexer@token:token(), glexer:position()})) -> {ok,
        {binary_operator(), list({glexer@token:token(), glexer:position()})}} |
    {error, nil}.
pop_binary_operator(Tokens) ->
    case Tokens of
        [{Token, _} | Tokens@1] ->
            gleam@result:map(
                binary_operator(Token),
                fun(Op) -> {Op, Tokens@1} end
            );

        [] ->
            {error, nil}
    end.

-file("src/glance.gleam", 1137).
?DOC(" Simple-Precedence-Parser, handle seeing an operator or end\n").
-spec handle_operator(
    gleam@option:option(binary_operator()),
    list(binary_operator()),
    list(expression())
) -> {gleam@option:option(expression()),
    list(binary_operator()),
    list(expression())}.
handle_operator(Next, Operators, Values) ->
    case {Next, Operators, Values} of
        {{some, Operator}, [], _} ->
            {none, [Operator], Values};

        {{some, Next@1}, [Previous | Operators@1], [A, B | Rest_values]} ->
            case precedence(Previous) >= precedence(Next@1) of
                true ->
                    Span = {span,
                        erlang:element(2, erlang:element(2, B)),
                        erlang:element(3, erlang:element(2, A))},
                    Expression = {binary_operator, Span, Previous, B, A},
                    Values@1 = [Expression | Rest_values],
                    handle_operator({some, Next@1}, Operators@1, Values@1);

                false ->
                    {none, [Next@1, Previous | Operators@1], Values}
            end;

        {none, [Operator@1 | Operators@2], [A@1, B@1 | Values@2]} ->
            Values@3 = [{binary_operator,
                    {span,
                        erlang:element(2, erlang:element(2, B@1)),
                        erlang:element(3, erlang:element(2, A@1))},
                    Operator@1,
                    B@1,
                    A@1} |
                Values@2],
            handle_operator(none, Operators@2, Values@3);

        {none, [], [Expression@1]} ->
            {{some, Expression@1}, Operators, Values};

        {none, [], []} ->
            {none, Operators, Values};

        {_, _, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"parser bug, expression not full reduced"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"glance"/utf8>>,
                    function => <<"handle_operator"/utf8>>,
                    line => 1169})
    end.

-file("src/glance.gleam", 1178).
-spec span_from_string(integer(), binary()) -> span().
span_from_string(Start, String) ->
    {span, Start, Start + erlang:byte_size(String)}.

-file("src/glance.gleam", 1411).
-spec string_offset(integer(), binary()) -> integer().
string_offset(Start, String) ->
    Start + erlang:byte_size(String).

-file("src/glance.gleam", 430).
-spec until(
    glexer@token:token(),
    DVH,
    list({glexer@token:token(), glexer:position()}),
    fun((DVH, list({glexer@token:token(), glexer:position()})) -> {ok,
            {DVH, list({glexer@token:token(), glexer:position()})}} |
        {error, error()})
) -> {ok, {DVH, integer(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
until(Limit, Acc, Tokens, Callback) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, {position, I}} | Tokens@1] when Token =:= Limit ->
            {ok,
                {Acc, string_offset(I, glexer@token:to_source(Token)), Tokens@1}};

        [_ | _] ->
            case Callback(Acc, Tokens) of
                {ok, {Acc@1, Tokens@2}} ->
                    until(Limit, Acc@1, Tokens@2, Callback);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("src/glance.gleam", 579).
-spec optional_module_alias(
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {gleam@option:option(assignment_name()),
    integer(),
    list({glexer@token:token(), glexer:position()})}.
optional_module_alias(Tokens, End) ->
    case Tokens of
        [{as, _}, {{name, Alias}, {position, Alias_start}} | Tokens@1] ->
            {{some, {named, Alias}},
                string_offset(Alias_start, Alias),
                Tokens@1};

        [{as, _},
            {{discard_name, Alias@1}, {position, Alias_start@1}} |
            Tokens@2] ->
            {{some, {discarded, Alias@1}},
                string_offset(Alias_start@1, Alias@1) + 1,
                Tokens@2};

        _ ->
            {none, End, Tokens}
    end.

-file("src/glance.gleam", 1714).
-spec list(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {EBA, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    gleam@option:option(fun((span()) -> EBA)),
    list(EBA),
    list({glexer@token:token(), glexer:position()})
) -> {ok, parsed_list(EBA)} | {error, error()}.
list(Parser, Discard, Acc, Tokens) ->
    case Tokens of
        [{right_square, {position, End}} | Tokens@1] ->
            {ok, {parsed_list, lists:reverse(Acc), none, Tokens@1, End + 1}};

        [{comma, _}, {right_square, {position, End@1}} | Tokens@2] when Acc =/= [] ->
            {ok, {parsed_list, lists:reverse(Acc), none, Tokens@2, End@1 + 1}};

        [{dot_dot, {position, Start}},
            {right_square, {position, End@2}} = Close |
            Tokens@3] ->
            case Discard of
                none ->
                    unexpected_error([Close | Tokens@3]);

                {some, Discard@1} ->
                    Value = Discard@1({span, Start, Start + 1}),
                    Parsed_list = {parsed_list,
                        lists:reverse(Acc),
                        {some, Value},
                        Tokens@3,
                        End@2 + 1},
                    {ok, Parsed_list}
            end;

        [{dot_dot, _} | Tokens@4] ->
            gleam@result:'try'(
                Parser(Tokens@4),
                fun(_use0) ->
                    {Rest, Tokens@5} = _use0,
                    expect(
                        right_square,
                        Tokens@5,
                        fun(_use0@1, Tokens@6) ->
                            {position, End@3} = _use0@1,
                            {ok,
                                {parsed_list,
                                    lists:reverse(Acc),
                                    {some, Rest},
                                    Tokens@6,
                                    End@3 + 1}}
                        end
                    )
                end
            );

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0@2) ->
                    {Element, Tokens@7} = _use0@2,
                    Acc@1 = [Element | Acc],
                    case Tokens@7 of
                        [{right_square, {position, End@4}} | Tokens@8] ->
                            {ok,
                                {parsed_list,
                                    lists:reverse(Acc@1),
                                    none,
                                    Tokens@8,
                                    End@4 + 1}};

                        [{comma, _},
                            {right_square, {position, End@4}} |
                            Tokens@8] ->
                            {ok,
                                {parsed_list,
                                    lists:reverse(Acc@1),
                                    none,
                                    Tokens@8,
                                    End@4 + 1}};

                        [{comma, _},
                            {dot_dot, {position, Start@1}},
                            {right_square, {position, End@5}} = Close@1 |
                            Tokens@9] ->
                            case Discard of
                                none ->
                                    unexpected_error([Close@1 | Tokens@9]);

                                {some, Discard@2} ->
                                    Value@1 = Discard@2(
                                        {span, Start@1, Start@1 + 1}
                                    ),
                                    Parsed_list@1 = {parsed_list,
                                        lists:reverse(Acc@1),
                                        {some, Value@1},
                                        Tokens@9,
                                        End@5 + 1},
                                    {ok, Parsed_list@1}
                            end;

                        [{comma, _}, {dot_dot, _} | Tokens@10] ->
                            gleam@result:'try'(
                                Parser(Tokens@10),
                                fun(_use0@3) ->
                                    {Rest@1, Tokens@11} = _use0@3,
                                    expect(
                                        right_square,
                                        Tokens@11,
                                        fun(_use0@4, Tokens@12) ->
                                            {position, End@6} = _use0@4,
                                            {ok,
                                                {parsed_list,
                                                    lists:reverse(Acc@1),
                                                    {some, Rest@1},
                                                    Tokens@12,
                                                    End@6 + 1}}
                                        end
                                    )
                                end
                            );

                        [{comma, _} | Tokens@13] ->
                            list(Parser, Discard, Acc@1, Tokens@13);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 345).
-spec push_constant(module_(), list(attribute()), constant()) -> module_().
push_constant(Module, Attributes, Constant) ->
    {module,
        erlang:element(2, Module),
        erlang:element(3, Module),
        erlang:element(4, Module),
        [{definition, lists:reverse(Attributes), Constant} |
            erlang:element(5, Module)],
        erlang:element(6, Module)}.

-file("src/glance.gleam", 356).
-spec push_function(module_(), list(attribute()), function_()) -> module_().
push_function(Module, Attributes, Function) ->
    {module,
        erlang:element(2, Module),
        erlang:element(3, Module),
        erlang:element(4, Module),
        erlang:element(5, Module),
        [{definition, lists:reverse(Attributes), Function} |
            erlang:element(6, Module)]}.

-file("src/glance.gleam", 367).
-spec push_custom_type(module_(), list(attribute()), custom_type()) -> module_().
push_custom_type(Module, Attributes, Custom_type) ->
    Custom_type@1 = {custom_type,
        erlang:element(2, Custom_type),
        erlang:element(3, Custom_type),
        erlang:element(4, Custom_type),
        erlang:element(5, Custom_type),
        erlang:element(6, Custom_type),
        lists:reverse(erlang:element(7, Custom_type))},
    {module,
        erlang:element(2, Module),
        [{definition, lists:reverse(Attributes), Custom_type@1} |
            erlang:element(3, Module)],
        erlang:element(4, Module),
        erlang:element(5, Module),
        erlang:element(6, Module)}.

-file("src/glance.gleam", 380).
-spec push_type_alias(module_(), list(attribute()), type_alias()) -> module_().
push_type_alias(Module, Attributes, Type_alias) ->
    {module,
        erlang:element(2, Module),
        erlang:element(3, Module),
        [{definition, lists:reverse(Attributes), Type_alias} |
            erlang:element(4, Module)],
        erlang:element(5, Module),
        erlang:element(6, Module)}.

-file("src/glance.gleam", 618).
-spec unqualified_imports(
    list(unqualified_import()),
    list(unqualified_import()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, unqualified_imports()} | {error, error()}.
unqualified_imports(Types, Values, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_brace, {position, End}} | Tokens@1] ->
            {ok,
                {unqualified_imports,
                    lists:reverse(Types),
                    lists:reverse(Values),
                    End + 1,
                    Tokens@1}};

        [{{upper_name, Name}, _},
            {as, _},
            {{upper_name, Alias}, _},
            {comma, _} |
            Tokens@2] ->
            Import_ = {unqualified_import, Name, {some, Alias}},
            unqualified_imports(Types, [Import_ | Values], Tokens@2);

        [{{name, Name}, _}, {as, _}, {{name, Alias}, _}, {comma, _} | Tokens@2] ->
            Import_ = {unqualified_import, Name, {some, Alias}},
            unqualified_imports(Types, [Import_ | Values], Tokens@2);

        [{{upper_name, Name@1}, _},
            {as, _},
            {{upper_name, Alias@1}, _},
            {right_brace, {position, End@1}} |
            Tokens@3] ->
            Import_@1 = {unqualified_import, Name@1, {some, Alias@1}},
            {ok,
                {unqualified_imports,
                    lists:reverse(Types),
                    lists:reverse([Import_@1 | Values]),
                    End@1 + 1,
                    Tokens@3}};

        [{{name, Name@1}, _},
            {as, _},
            {{name, Alias@1}, _},
            {right_brace, {position, End@1}} |
            Tokens@3] ->
            Import_@1 = {unqualified_import, Name@1, {some, Alias@1}},
            {ok,
                {unqualified_imports,
                    lists:reverse(Types),
                    lists:reverse([Import_@1 | Values]),
                    End@1 + 1,
                    Tokens@3}};

        [{{upper_name, Name@2}, _}, {comma, _} | Tokens@4] ->
            Import_@2 = {unqualified_import, Name@2, none},
            unqualified_imports(Types, [Import_@2 | Values], Tokens@4);

        [{{name, Name@2}, _}, {comma, _} | Tokens@4] ->
            Import_@2 = {unqualified_import, Name@2, none},
            unqualified_imports(Types, [Import_@2 | Values], Tokens@4);

        [{{upper_name, Name@3}, _}, {right_brace, {position, End@2}} | Tokens@5] ->
            Import_@3 = {unqualified_import, Name@3, none},
            {ok,
                {unqualified_imports,
                    lists:reverse(Types),
                    lists:reverse([Import_@3 | Values]),
                    End@2 + 1,
                    Tokens@5}};

        [{{name, Name@3}, _}, {right_brace, {position, End@2}} | Tokens@5] ->
            Import_@3 = {unqualified_import, Name@3, none},
            {ok,
                {unqualified_imports,
                    lists:reverse(Types),
                    lists:reverse([Import_@3 | Values]),
                    End@2 + 1,
                    Tokens@5}};

        [{type, _},
            {{upper_name, Name@4}, _},
            {as, _},
            {{upper_name, Alias@2}, _},
            {comma, _} |
            Tokens@6] ->
            Import_@4 = {unqualified_import, Name@4, {some, Alias@2}},
            unqualified_imports([Import_@4 | Types], Values, Tokens@6);

        [{type, _},
            {{upper_name, Name@5}, _},
            {as, _},
            {{upper_name, Alias@3}, _},
            {right_brace, {position, End@3}} |
            Tokens@7] ->
            Import_@5 = {unqualified_import, Name@5, {some, Alias@3}},
            {ok,
                {unqualified_imports,
                    lists:reverse([Import_@5 | Types]),
                    lists:reverse(Values),
                    End@3 + 1,
                    Tokens@7}};

        [{type, _}, {{upper_name, Name@6}, _}, {comma, _} | Tokens@8] ->
            Import_@6 = {unqualified_import, Name@6, none},
            unqualified_imports([Import_@6 | Types], Values, Tokens@8);

        [{type, _},
            {{upper_name, Name@7}, _},
            {right_brace, {position, End@4}} |
            Tokens@9] ->
            Import_@7 = {unqualified_import, Name@7, none},
            {ok,
                {unqualified_imports,
                    lists:reverse([Import_@7 | Types]),
                    lists:reverse(Values),
                    End@4 + 1,
                    Tokens@9}};

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 607).
-spec optional_unqualified_imports(
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, unqualified_imports()} | {error, error()}.
optional_unqualified_imports(Tokens, End) ->
    case Tokens of
        [{dot, _}, {left_brace, _} | Tokens@1] ->
            unqualified_imports([], [], Tokens@1);

        _ ->
            {ok, {unqualified_imports, [], [], End, Tokens}}
    end.

-file("src/glance.gleam", 538).
-spec import_statement(
    module_(),
    list(attribute()),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
import_statement(Module, Attributes, Tokens, Start) ->
    gleam@result:'try'(
        module_name(<<""/utf8>>, 0, Tokens),
        fun(_use0) ->
            {Module_name, End, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_unqualified_imports(Tokens@1, End),
                fun(_use0@1) ->
                    {unqualified_imports, Ts, Vs, End@1, Tokens@2} = _use0@1,
                    {Alias, End@2, Tokens@3} = optional_module_alias(
                        Tokens@2,
                        End@1
                    ),
                    Span = {span, Start, End@2},
                    Import_ = {import, Span, Module_name, Alias, Ts, Vs},
                    Definition = {definition,
                        lists:reverse(Attributes),
                        Import_},
                    Module@1 = {module,
                        [Definition | erlang:element(2, Module)],
                        erlang:element(3, Module),
                        erlang:element(4, Module),
                        erlang:element(5, Module),
                        erlang:element(6, Module)},
                    {ok, {Module@1, Tokens@3}}
                end
            )
        end
    ).

-file("src/glance.gleam", 1342).
-spec bit_string_segment_options(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {DYX, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list(bit_string_segment_option(DYX)),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(bit_string_segment_option(DYX)),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
bit_string_segment_options(Parser, Options, Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{int, I}, Position} | Tokens@1] ->
                case gleam_stdlib:parse_int(I) of
                    {ok, I@1} ->
                        {ok, {{size_option, I@1}, Tokens@1}};

                    {error, _} ->
                        {error, {unexpected_token, {int, I}, Position}}
                end;

            [{{name, <<"size"/utf8>>}, _}, {left_paren, _} | Tokens@2] ->
                gleam@result:'try'(
                    Parser(Tokens@2),
                    fun(_use0) ->
                        {Value, Tokens@3} = _use0,
                        expect(
                            right_paren,
                            Tokens@3,
                            fun(_, Tokens@4) ->
                                {ok, {{size_value_option, Value}, Tokens@4}}
                            end
                        )
                    end
                );

            [{{name, <<"unit"/utf8>>}, Position@1},
                {left_paren, _},
                {{int, I@2}, _},
                {right_paren, _} |
                Tokens@5] ->
                case gleam_stdlib:parse_int(I@2) of
                    {ok, I@3} ->
                        {ok, {{unit_option, I@3}, Tokens@5}};

                    {error, _} ->
                        {error, {unexpected_token, {int, I@2}, Position@1}}
                end;

            [{{name, <<"bytes"/utf8>>}, _} | Tokens@6] ->
                {ok, {bytes_option, Tokens@6}};

            [{{name, <<"binary"/utf8>>}, _} | Tokens@7] ->
                {ok, {bytes_option, Tokens@7}};

            [{{name, <<"int"/utf8>>}, _} | Tokens@8] ->
                {ok, {int_option, Tokens@8}};

            [{{name, <<"float"/utf8>>}, _} | Tokens@9] ->
                {ok, {float_option, Tokens@9}};

            [{{name, <<"bits"/utf8>>}, _} | Tokens@10] ->
                {ok, {bits_option, Tokens@10}};

            [{{name, <<"bit_string"/utf8>>}, _} | Tokens@11] ->
                {ok, {bits_option, Tokens@11}};

            [{{name, <<"utf8"/utf8>>}, _} | Tokens@12] ->
                {ok, {utf8_option, Tokens@12}};

            [{{name, <<"utf16"/utf8>>}, _} | Tokens@13] ->
                {ok, {utf16_option, Tokens@13}};

            [{{name, <<"utf32"/utf8>>}, _} | Tokens@14] ->
                {ok, {utf32_option, Tokens@14}};

            [{{name, <<"utf8_codepoint"/utf8>>}, _} | Tokens@15] ->
                {ok, {utf8_codepoint_option, Tokens@15}};

            [{{name, <<"utf16_codepoint"/utf8>>}, _} | Tokens@16] ->
                {ok, {utf16_codepoint_option, Tokens@16}};

            [{{name, <<"utf32_codepoint"/utf8>>}, _} | Tokens@17] ->
                {ok, {utf32_codepoint_option, Tokens@17}};

            [{{name, <<"signed"/utf8>>}, _} | Tokens@18] ->
                {ok, {signed_option, Tokens@18}};

            [{{name, <<"unsigned"/utf8>>}, _} | Tokens@19] ->
                {ok, {unsigned_option, Tokens@19}};

            [{{name, <<"big"/utf8>>}, _} | Tokens@20] ->
                {ok, {big_option, Tokens@20}};

            [{{name, <<"little"/utf8>>}, _} | Tokens@21] ->
                {ok, {little_option, Tokens@21}};

            [{{name, <<"native"/utf8>>}, _} | Tokens@22] ->
                {ok, {native_option, Tokens@22}};

            [{Other, Position@2} | _] ->
                {error, {unexpected_token, Other, Position@2}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0@1) ->
            {Option, Tokens@23} = _use0@1,
            Options@1 = [Option | Options],
            case Tokens@23 of
                [{minus, _} | Tokens@24] ->
                    bit_string_segment_options(Parser, Options@1, Tokens@24);

                _ ->
                    {ok, {lists:reverse(Options@1), Tokens@23}}
            end
        end).

-file("src/glance.gleam", 1332).
-spec optional_bit_string_segment_options(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {DYQ, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(bit_string_segment_option(DYQ)),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_bit_string_segment_options(Parser, Tokens) ->
    case Tokens of
        [{colon, _} | Tokens@1] ->
            bit_string_segment_options(Parser, [], Tokens@1);

        _ ->
            {ok, {[], Tokens}}
    end.

-file("src/glance.gleam", 1322).
-spec bit_string_segment(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {DYJ, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {{DYJ, list(bit_string_segment_option(DYJ))},
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
bit_string_segment(Parser, Tokens) ->
    gleam@result:'try'(
        Parser(Tokens),
        fun(_use0) ->
            {Value, Tokens@1} = _use0,
            Result = optional_bit_string_segment_options(Parser, Tokens@1),
            gleam@result:'try'(
                Result,
                fun(_use0@1) ->
                    {Options, Tokens@2} = _use0@1,
                    {ok, {{Value, Options}, Tokens@2}}
                end
            )
        end
    ).

-file("src/glance.gleam", 1671).
-spec delimited(
    list(EAQ),
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {EAQ, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    glexer@token:token()
) -> {ok, {list(EAQ), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
delimited(Acc, Tokens, Parser, Delimeter) ->
    gleam@result:'try'(
        Parser(Tokens),
        fun(_use0) ->
            {T, Tokens@1} = _use0,
            Acc@1 = [T | Acc],
            case Tokens@1 of
                [{Token, _} | Tokens@2] when Token =:= Delimeter ->
                    delimited(Acc@1, Tokens@2, Parser, Delimeter);

                _ ->
                    {ok, {lists:reverse(Acc@1), Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1867).
-spec comma_delimited(
    list(EBS),
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {EBS, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    glexer@token:token()
) -> {ok,
        {list(EBS), integer(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
comma_delimited(Items, Tokens, Parser, Final) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, {position, Token_start}} | Tokens@1] when Token =:= Final ->
            {ok,
                {lists:reverse(Items),
                    string_offset(Token_start, glexer@token:to_source(Token)),
                    Tokens@1}};

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0) ->
                    {Element, Tokens@2} = _use0,
                    case Tokens@2 of
                        [{comma, _} | Tokens@3] ->
                            comma_delimited(
                                [Element | Items],
                                Tokens@3,
                                Parser,
                                Final
                            );

                        [{Token@1, {position, Token_start@1}} | Tokens@4] when Token@1 =:= Final ->
                            Offset = string_offset(
                                Token_start@1,
                                glexer@token:to_source(Token@1)
                            ),
                            {ok,
                                {lists:reverse([Element | Items]),
                                    Offset,
                                    Tokens@4}};

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 2058).
-spec name(list({glexer@token:token(), glexer:position()})) -> {ok,
        {binary(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
name(Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{name, Name}, _} | Tokens@1] ->
            {ok, {Name, Tokens@1}};

        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}}
    end.

-file("src/glance.gleam", 2113).
-spec field(
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {EDD, list({glexer@token:token(), glexer:position()})}} |
        {error, error()})
) -> {ok, {field(EDD), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
field(Tokens, Parser) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            case Tokens@1 of
                [{comma, _} | _] ->
                    {ok, {{shorthand_field, Name}, Tokens@1}};

                [{right_paren, _} | _] ->
                    {ok, {{shorthand_field, Name}, Tokens@1}};

                _ ->
                    gleam@result:'try'(
                        Parser(Tokens@1),
                        fun(_use0) ->
                            {T, Tokens@2} = _use0,
                            {ok, {{labelled_field, Name, T}, Tokens@2}}
                        end
                    )
            end;

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0@1) ->
                    {T@1, Tokens@3} = _use0@1,
                    {ok, {{unlabelled_field, T@1}, Tokens@3}}
                end
            )
    end.

-file("src/glance.gleam", 805).
-spec statement(list({glexer@token:token(), glexer:position()})) -> {ok,
        {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
statement(Tokens) ->
    case Tokens of
        [{'let', {position, Start}}, {assert, _} | Tokens@1] ->
            assignment({let_assert, none}, Tokens@1, Start);

        [{'let', {position, Start@1}} | Tokens@2] ->
            assignment('let', Tokens@2, Start@1);

        [{use, {position, Start@2}} | Tokens@3] ->
            use_(Tokens@3, Start@2);

        [{assert, {position, Start@3}} | Tokens@4] ->
            assert_(Tokens@4, Start@3);

        Tokens@5 ->
            gleam@result:'try'(
                expression(Tokens@5),
                fun(_use0) ->
                    {Expression, Tokens@6} = _use0,
                    {ok, {{expression, Expression}, Tokens@6}}
                end
            )
    end.

-file("src/glance.gleam", 819).
-spec assert_(list({glexer@token:token(), glexer:position()}), integer()) -> {ok,
        {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
assert_(Tokens, Start) ->
    gleam@result:'try'(
        expression(Tokens),
        fun(_use0) ->
            {Subject, Tokens@1} = _use0,
            case Tokens@1 of
                [{as, _} | Tokens@2] ->
                    case expression(Tokens@2) of
                        {error, Error} ->
                            {error, Error};

                        {ok, {Message, Tokens@3}} ->
                            Statement = {assert,
                                {span,
                                    Start,
                                    erlang:element(
                                        3,
                                        erlang:element(2, Message)
                                    )},
                                Subject,
                                {some, Message}},
                            {ok, {Statement, Tokens@3}}
                    end;

                _ ->
                    Statement@1 = {assert,
                        {span,
                            Start,
                            erlang:element(3, erlang:element(2, Subject))},
                        Subject,
                        none},
                    {ok, {Statement@1, Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1053).
-spec expression(list({glexer@token:token(), glexer:position()})) -> {ok,
        {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression(Tokens) ->
    expression_loop(Tokens, [], [], regular_expression_unit).

-file("src/glance.gleam", 1103).
-spec expression_loop(
    list({glexer@token:token(), glexer:position()}),
    list(binary_operator()),
    list(expression()),
    parse_expression_unit_context()
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression_loop(Tokens, Operators, Values, Context) ->
    gleam@result:'try'(
        expression_unit(Tokens, Context),
        fun(_use0) ->
            {Expression, Tokens@1} = _use0,
            case Expression of
                none ->
                    unexpected_error(Tokens@1);

                {some, E} ->
                    Values@1 = [E | Values],
                    case pop_binary_operator(Tokens@1) of
                        {ok, {Operator, Tokens@2}} ->
                            case handle_operator(
                                {some, Operator},
                                Operators,
                                Values@1
                            ) of
                                {{some, Expression@1}, _, _} ->
                                    {ok, {Expression@1, Tokens@2}};

                                {none, Operators@1, Values@2} ->
                                    expression_loop(
                                        Tokens@2,
                                        Operators@1,
                                        Values@2,
                                        case Operator of
                                            pipe ->
                                                expression_unit_after_pipe;

                                            _ ->
                                                regular_expression_unit
                                        end
                                    )
                            end;

                        _ ->
                            case erlang:element(
                                1,
                                handle_operator(none, Operators, Values@1)
                            ) of
                                none ->
                                    unexpected_error(Tokens@1);

                                {some, Expression@2} ->
                                    {ok, {Expression@2, Tokens@1}}
                            end
                    end
            end
        end
    ).

-file("src/glance.gleam", 1182).
-spec expression_unit(
    list({glexer@token:token(), glexer:position()}),
    parse_expression_unit_context()
) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression_unit(Tokens, Context) ->
    gleam@result:'try'(case Tokens of
            [{{name, Module}, {position, Start}},
                {dot, _},
                {{upper_name, Constructor}, _},
                {left_paren, _},
                {dot_dot, _} |
                Tokens@1] ->
                record_update({some, Module}, Constructor, Tokens@1, Start);

            [{{upper_name, Constructor@1}, {position, Start@1}},
                {left_paren, _},
                {dot_dot, _} |
                Tokens@2] ->
                record_update(none, Constructor@1, Tokens@2, Start@1);

            [{{upper_name, Name}, {position, Start@2}} | Tokens@3] ->
                {ok,
                    {{some, {variable, span_from_string(Start@2, Name), Name}},
                        Tokens@3}};

            [{{int, Value}, {position, Start@3}} | Tokens@4] ->
                Span = span_from_string(Start@3, Value),
                {ok, {{some, {int, Span, Value}}, Tokens@4}};

            [{{float, Value@1}, {position, Start@4}} | Tokens@5] ->
                Span@1 = span_from_string(Start@4, Value@1),
                {ok, {{some, {float, Span@1, Value@1}}, Tokens@5}};

            [{{string, Value@2}, {position, Start@5}} | Tokens@6] ->
                Span@2 = {span, Start@5, string_offset(Start@5, Value@2) + 2},
                {ok, {{some, {string, Span@2, Value@2}}, Tokens@6}};

            [{{name, Name@1}, {position, Start@6}} | Tokens@7] ->
                Span@3 = span_from_string(Start@6, Name@1),
                {ok, {{some, {variable, Span@3, Name@1}}, Tokens@7}};

            [{fn, {position, Start@7}} | Tokens@8] ->
                fn_(Tokens@8, Start@7);

            [{'case', {position, Start@8}} | Tokens@9] ->
                case_(Tokens@9, Start@8);

            [{panic, {position, Start@9}} | Tokens@10] ->
                todo_panic(
                    Tokens@10,
                    fun(Field@0, Field@1) -> {panic, Field@0, Field@1} end,
                    Start@9,
                    <<"panic"/utf8>>
                );

            [{todo, {position, Start@10}} | Tokens@11] ->
                todo_panic(
                    Tokens@11,
                    fun(Field@0, Field@1) -> {todo, Field@0, Field@1} end,
                    Start@10,
                    <<"todo"/utf8>>
                );

            [{left_square, {position, Start@11}} | Tokens@12] ->
                Result = list(fun expression/1, none, [], Tokens@12),
                gleam@result:map(
                    Result,
                    fun(_use0) ->
                        {parsed_list, Elements, Rest, Tokens@13, End} = _use0,
                        {{some, {list, {span, Start@11, End}, Elements, Rest}},
                            Tokens@13}
                    end
                );

            [{hash, {position, Start@12}}, {left_paren, _} | Tokens@14] ->
                Result@1 = comma_delimited(
                    [],
                    Tokens@14,
                    fun expression/1,
                    right_paren
                ),
                gleam@result:map(
                    Result@1,
                    fun(_use0@1) ->
                        {Expressions, End@1, Tokens@15} = _use0@1,
                        {{some, {tuple, {span, Start@12, End@1}, Expressions}},
                            Tokens@15}
                    end
                );

            [{bang, {position, Start@13}} | Tokens@16] ->
                gleam@result:map(
                    expression(Tokens@16),
                    fun(_use0@2) ->
                        {Expression, Tokens@17} = _use0@2,
                        {{some,
                                {negate_bool,
                                    {span,
                                        Start@13,
                                        erlang:element(
                                            3,
                                            erlang:element(2, Expression)
                                        )},
                                    Expression}},
                            Tokens@17}
                    end
                );

            [{minus, {position, Start@14}} | Tokens@18] ->
                gleam@result:map(
                    expression(Tokens@18),
                    fun(_use0@3) ->
                        {Expression@1, Tokens@19} = _use0@3,
                        Expression@2 = case Expression@1 of
                            {float, Location, Amount} ->
                                {float,
                                    {span,
                                        Start@14,
                                        erlang:element(3, Location)},
                                    <<"-"/utf8, Amount/binary>>};

                            _ ->
                                {negate_int,
                                    {span,
                                        Start@14,
                                        erlang:element(
                                            3,
                                            erlang:element(2, Expression@1)
                                        )},
                                    Expression@1}
                        end,
                        {{some, Expression@2}, Tokens@19}
                    end
                );

            [{left_brace, {position, Start@15}} | Tokens@20] ->
                gleam@result:map(
                    statements([], Tokens@20),
                    fun(_use0@4) ->
                        {Statements, End@2, Tokens@21} = _use0@4,
                        {{some, {block, {span, Start@15, End@2}, Statements}},
                            Tokens@21}
                    end
                );

            [{less_less, {position, Start@16}} | Tokens@22] ->
                Parser = fun(_capture) ->
                    bit_string_segment(fun expression/1, _capture)
                end,
                Result@2 = comma_delimited(
                    [],
                    Tokens@22,
                    Parser,
                    greater_greater
                ),
                gleam@result:map(
                    Result@2,
                    fun(_use0@5) ->
                        {Segments, End@3, Tokens@23} = _use0@5,
                        {{some, {bit_string, {span, Start@16, End@3}, Segments}},
                            Tokens@23}
                    end
                );

            [{echo, {position, Start@17}} | Tokens@24] ->
                case Context of
                    expression_unit_after_pipe ->
                        Span@4 = span_from_string(Start@17, <<"echo"/utf8>>),
                        {ok, {{some, {echo, Span@4, none}}, Tokens@24}};

                    regular_expression_unit ->
                        gleam@result:map(
                            expression(Tokens@24),
                            fun(Expression_and_tokens) ->
                                {Expression@3, Tokens@25} = Expression_and_tokens,
                                Span@5 = {span,
                                    Start@17,
                                    erlang:element(
                                        3,
                                        erlang:element(2, Expression@3)
                                    )},
                                Expression@4 = {echo,
                                    Span@5,
                                    {some, Expression@3}},
                                {{some, Expression@4}, Tokens@25}
                            end
                        )
                end;

            _ ->
                {ok, {none, Tokens}}
        end, fun(_use0@6) ->
            {Parsed, Tokens@26} = _use0@6,
            case Parsed of
                {some, Expression@5} ->
                    case after_expression(Expression@5, Tokens@26) of
                        {ok, {Expression@6, Tokens@27}} ->
                            {ok, {{some, Expression@6}, Tokens@27}};

                        {error, Error} ->
                            {error, Error}
                    end;

                none ->
                    {ok, {none, Tokens@26}}
            end
        end).

-file("src/glance.gleam", 1686).
-spec fn_(list({glexer@token:token(), glexer:position()}), integer()) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_(Tokens, Start) ->
    expect(
        left_paren,
        Tokens,
        fun(_, Tokens@1) ->
            Result = comma_delimited(
                [],
                Tokens@1,
                fun fn_parameter/1,
                right_paren
            ),
            gleam@result:'try'(
                Result,
                fun(_use0) ->
                    {Parameters, _, Tokens@2} = _use0,
                    gleam@result:'try'(
                        optional_return_annotation(0, Tokens@2),
                        fun(_use0@1) ->
                            {Return, _, Tokens@3} = _use0@1,
                            expect(
                                left_brace,
                                Tokens@3,
                                fun(_, Tokens@4) ->
                                    gleam@result:'try'(
                                        statements([], Tokens@4),
                                        fun(_use0@2) ->
                                            {Body, End, Tokens@5} = _use0@2,
                                            {ok,
                                                {{some,
                                                        {fn,
                                                            {span, Start, End},
                                                            Parameters,
                                                            Return,
                                                            Body}},
                                                    Tokens@5}}
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

-file("src/glance.gleam", 791).
-spec statements(
    list(statement()),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(statement()),
            integer(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
statements(Acc, Tokens) ->
    case Tokens of
        [{right_brace, {position, End}} | Tokens@1] ->
            {ok, {lists:reverse(Acc), End + 1, Tokens@1}};

        _ ->
            gleam@result:'try'(
                statement(Tokens),
                fun(_use0) ->
                    {Statement, Tokens@2} = _use0,
                    statements([Statement | Acc], Tokens@2)
                end
            )
    end.

-file("src/glance.gleam", 449).
-spec attribute(list({glexer@token:token(), glexer:position()})) -> {ok,
        {attribute(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
attribute(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{name, Name}, _} | Tokens@1] ->
                {ok, {Name, Tokens@1}};

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0) ->
            {Name@1, Tokens@2} = _use0,
            case Tokens@2 of
                [{left_paren, _} | Tokens@3] ->
                    Result = comma_delimited(
                        [],
                        Tokens@3,
                        fun expression/1,
                        right_paren
                    ),
                    gleam@result:'try'(
                        Result,
                        fun(_use0@1) ->
                            {Parameters, _, Tokens@4} = _use0@1,
                            {ok, {{attribute, Name@1, Parameters}, Tokens@4}}
                        end
                    );

                _ ->
                    {ok, {{attribute, Name@1, []}, Tokens@2}}
            end
        end).

-file("src/glance.gleam", 1301).
-spec todo_panic(
    list({glexer@token:token(), glexer:position()}),
    fun((span(), gleam@option:option(expression())) -> expression()),
    integer(),
    binary()
) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
todo_panic(Tokens, Constructor, Start, Keyword_name) ->
    case Tokens of
        [{as, _} | Tokens@1] ->
            gleam@result:'try'(
                expression(Tokens@1),
                fun(_use0) ->
                    {Reason, Tokens@2} = _use0,
                    Span = {span,
                        Start,
                        erlang:element(3, erlang:element(2, Reason))},
                    Expression = Constructor(Span, {some, Reason}),
                    {ok, {{some, Expression}, Tokens@2}}
                end
            );

        _ ->
            Span@1 = span_from_string(Start, Keyword_name),
            Expression@1 = Constructor(Span@1, none),
            {ok, {{some, Expression@1}, Tokens}}
    end.

-file("src/glance.gleam", 1594).
-spec record_update_field(list({glexer@token:token(), glexer:position()})) -> {ok,
        {record_update_field(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
record_update_field(Tokens) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            case Tokens@1 of
                [{comma, _} | _] ->
                    {ok, {{record_update_field, Name, none}, Tokens@1}};

                [{right_paren, _} | _] ->
                    {ok, {{record_update_field, Name, none}, Tokens@1}};

                _ ->
                    gleam@result:'try'(
                        expression(Tokens@1),
                        fun(_use0) ->
                            {Expression, Tokens@2} = _use0,
                            {ok,
                                {{record_update_field, Name, {some, Expression}},
                                    Tokens@2}}
                        end
                    )
            end;

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}};

        [] ->
            {error, unexpected_end_of_input}
    end.

-file("src/glance.gleam", 1568).
-spec record_update(
    gleam@option:option(binary()),
    binary(),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
record_update(Module, Constructor, Tokens, Start) ->
    gleam@result:'try'(
        expression(Tokens),
        fun(_use0) ->
            {Record, Tokens@1} = _use0,
            case Tokens@1 of
                [{right_paren, {position, End}} | Tokens@2] ->
                    Span = {span, Start, End + 1},
                    Expression = {record_update,
                        Span,
                        Module,
                        Constructor,
                        Record,
                        []},
                    {ok, {{some, Expression}, Tokens@2}};

                [{comma, _} | Tokens@3] ->
                    Result = comma_delimited(
                        [],
                        Tokens@3,
                        fun record_update_field/1,
                        right_paren
                    ),
                    gleam@result:'try'(
                        Result,
                        fun(_use0@1) ->
                            {Fields, End@1, Tokens@4} = _use0@1,
                            Span@1 = {span, Start, End@1},
                            Expression@1 = {record_update,
                                Span@1,
                                Module,
                                Constructor,
                                Record,
                                Fields},
                            {ok, {{some, Expression@1}, Tokens@4}}
                        end
                    );

                _ ->
                    {ok, {none, Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1624).
-spec case_subjects(
    list(expression()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {list(expression()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_subjects(Subjects, Tokens) ->
    gleam@result:'try'(
        expression(Tokens),
        fun(_use0) ->
            {Subject, Tokens@1} = _use0,
            Subjects@1 = [Subject | Subjects],
            case Tokens@1 of
                [{comma, _} | Tokens@2] ->
                    case_subjects(Subjects@1, Tokens@2);

                _ ->
                    {ok, {lists:reverse(Subjects@1), Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1659).
-spec optional_clause_guard(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_clause_guard(Tokens) ->
    case Tokens of
        [{'if', _} | Tokens@1] ->
            gleam@result:'try'(
                expression(Tokens@1),
                fun(_use0) ->
                    {Expression, Tokens@2} = _use0,
                    {ok, {{some, Expression}, Tokens@2}}
                end
            );

        _ ->
            {ok, {none, Tokens}}
    end.

-file("src/glance.gleam", 2084).
-spec attributes(
    list(attribute()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {list(attribute()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
attributes(Accumulated_attributes, Tokens) ->
    case Tokens of
        [{at, _} | Tokens@1] ->
            case attribute(Tokens@1) of
                {error, Error} ->
                    {error, Error};

                {ok, {Attribute, Tokens@2}} ->
                    attributes([Attribute | Accumulated_attributes], Tokens@2)
            end;

        _ ->
            {ok, {lists:reverse(Accumulated_attributes), Tokens}}
    end.

-file("src/glance.gleam", 1450).
-spec call(
    list(field(expression())),
    expression(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
call(Arguments, Function, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_paren, {position, End}} | Tokens@1] ->
            Span = {span,
                erlang:element(2, erlang:element(2, Function)),
                End + 1},
            Call = {call, Span, Function, lists:reverse(Arguments)},
            after_expression(Call, Tokens@1);

        [{{name, Label}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {comma, _},
            {right_paren, {position, End@1}} |
            Tokens@2] ->
            Span@1 = {span,
                erlang:element(2, erlang:element(2, Function)),
                End@1 + 1},
            Capture = {fn_capture,
                Span@1,
                {some, Label},
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture, Tokens@2);

        [{{name, Label}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {right_paren, {position, End@1}} |
            Tokens@2] ->
            Span@1 = {span,
                erlang:element(2, erlang:element(2, Function)),
                End@1 + 1},
            Capture = {fn_capture,
                Span@1,
                {some, Label},
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture, Tokens@2);

        [{{name, Label@1}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {comma, _} |
            Tokens@3] ->
            fn_capture(
                {some, Label@1},
                Function,
                lists:reverse(Arguments),
                [],
                Tokens@3
            );

        [{{name, Label@1}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _} |
            Tokens@3] ->
            fn_capture(
                {some, Label@1},
                Function,
                lists:reverse(Arguments),
                [],
                Tokens@3
            );

        [{{discard_name, <<""/utf8>>}, _},
            {comma, _},
            {right_paren, {position, End@2}} |
            Tokens@4] ->
            Span@2 = {span,
                erlang:element(2, erlang:element(2, Function)),
                End@2 + 1},
            Capture@1 = {fn_capture,
                Span@2,
                none,
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture@1, Tokens@4);

        [{{discard_name, <<""/utf8>>}, _},
            {right_paren, {position, End@2}} |
            Tokens@4] ->
            Span@2 = {span,
                erlang:element(2, erlang:element(2, Function)),
                End@2 + 1},
            Capture@1 = {fn_capture,
                Span@2,
                none,
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture@1, Tokens@4);

        [{{discard_name, <<""/utf8>>}, _}, {comma, _} | Tokens@5] ->
            fn_capture(none, Function, lists:reverse(Arguments), [], Tokens@5);

        [{{discard_name, <<""/utf8>>}, _} | Tokens@5] ->
            fn_capture(none, Function, lists:reverse(Arguments), [], Tokens@5);

        _ ->
            gleam@result:'try'(
                field(Tokens, fun expression/1),
                fun(_use0) ->
                    {Argument, Tokens@6} = _use0,
                    Arguments@1 = [Argument | Arguments],
                    case Tokens@6 of
                        [{comma, _} | Tokens@7] ->
                            call(Arguments@1, Function, Tokens@7);

                        [{right_paren, {position, End@3}} | Tokens@8] ->
                            Span@3 = {span,
                                erlang:element(2, erlang:element(2, Function)),
                                End@3 + 1},
                            Call@1 = {call,
                                Span@3,
                                Function,
                                lists:reverse(Arguments@1)},
                            after_expression(Call@1, Tokens@8);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 1529).
-spec fn_capture(
    gleam@option:option(binary()),
    expression(),
    list(field(expression())),
    list(field(expression())),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_capture(Label, Function, Before, After, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_paren, {position, End}} | Tokens@1] ->
            Span = {span,
                erlang:element(2, erlang:element(2, Function)),
                End + 1},
            Capture = {fn_capture,
                Span,
                Label,
                Function,
                Before,
                lists:reverse(After)},
            after_expression(Capture, Tokens@1);

        _ ->
            gleam@result:'try'(
                field(Tokens, fun expression/1),
                fun(_use0) ->
                    {Argument, Tokens@2} = _use0,
                    After@1 = [Argument | After],
                    case Tokens@2 of
                        [{comma, _} | Tokens@3] ->
                            fn_capture(
                                Label,
                                Function,
                                Before,
                                After@1,
                                Tokens@3
                            );

                        [{right_paren, {position, End@1}} | Tokens@4] ->
                            Span@1 = {span,
                                erlang:element(2, erlang:element(2, Function)),
                                End@1 + 1},
                            Call = {fn_capture,
                                Span@1,
                                Label,
                                Function,
                                Before,
                                lists:reverse(After@1)},
                            after_expression(Call, Tokens@4);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 1415).
-spec after_expression(
    expression(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
after_expression(Parsed, Tokens) ->
    case Tokens of
        [{dot, _}, {{name, Label}, {position, Label_start}} | Tokens@1] ->
            Span = {span,
                erlang:element(2, erlang:element(2, Parsed)),
                string_offset(Label_start, Label)},
            Expression = {field_access, Span, Parsed, Label},
            after_expression(Expression, Tokens@1);

        [{dot, _}, {{upper_name, Label}, {position, Label_start}} | Tokens@1] ->
            Span = {span,
                erlang:element(2, erlang:element(2, Parsed)),
                string_offset(Label_start, Label)},
            Expression = {field_access, Span, Parsed, Label},
            after_expression(Expression, Tokens@1);

        [{dot, _}, {{int, Value} = Token, Position} | Tokens@2] ->
            case gleam_stdlib:parse_int(Value) of
                {ok, I} ->
                    End = string_offset(erlang:element(2, Position), Value),
                    Span@1 = {span,
                        erlang:element(2, erlang:element(2, Parsed)),
                        End},
                    Expression@1 = {tuple_index, Span@1, Parsed, I},
                    after_expression(Expression@1, Tokens@2);

                {error, _} ->
                    {error, {unexpected_token, Token, Position}}
            end;

        [{left_paren, _} | Tokens@3] ->
            call([], Parsed, Tokens@3);

        _ ->
            {ok, {Parsed, Tokens}}
    end.

-file("src/glance.gleam", 914).
-spec pattern_constructor_arguments(
    list(field(pattern())),
    list({glexer@token:token(), glexer:position()})
) -> {ok, pattern_constructor_arguments()} | {error, error()}.
pattern_constructor_arguments(Arguments, Tokens) ->
    case Tokens of
        [{right_paren, {position, End}} | Tokens@1] ->
            {ok,
                {pattern_constructor_arguments,
                    Arguments,
                    false,
                    End + 1,
                    Tokens@1}};

        [{dot_dot, _}, {comma, _}, {right_paren, {position, End@1}} | Tokens@2] ->
            {ok,
                {pattern_constructor_arguments,
                    Arguments,
                    true,
                    End@1 + 1,
                    Tokens@2}};

        [{dot_dot, _}, {right_paren, {position, End@1}} | Tokens@2] ->
            {ok,
                {pattern_constructor_arguments,
                    Arguments,
                    true,
                    End@1 + 1,
                    Tokens@2}};

        Tokens@3 ->
            gleam@result:'try'(
                field(Tokens@3, fun pattern/1),
                fun(_use0) ->
                    {Pattern, Tokens@4} = _use0,
                    Arguments@1 = [Pattern | Arguments],
                    case Tokens@4 of
                        [{right_paren, {position, End@2}} | Tokens@5] ->
                            {ok,
                                {pattern_constructor_arguments,
                                    Arguments@1,
                                    false,
                                    End@2 + 1,
                                    Tokens@5}};

                        [{comma, _},
                            {dot_dot, _},
                            {right_paren, {position, End@3}} |
                            Tokens@6] ->
                            {ok,
                                {pattern_constructor_arguments,
                                    Arguments@1,
                                    true,
                                    End@3 + 1,
                                    Tokens@6}};

                        [{comma, _} | Tokens@7] ->
                            pattern_constructor_arguments(Arguments@1, Tokens@7);

                        [{Token, Position} | _] ->
                            {error, {unexpected_token, Token, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 947).
-spec pattern(list({glexer@token:token(), glexer:position()})) -> {ok,
        {pattern(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
pattern(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{upper_name, Name}, {position, Start}} | Tokens@1] ->
                pattern_constructor(none, Name, Tokens@1, Start, Start);

            [{{name, Module}, {position, Start@1}},
                {dot, _},
                {{upper_name, Name@1}, {position, Name_start}} |
                Tokens@2] ->
                pattern_constructor(
                    {some, Module},
                    Name@1,
                    Tokens@2,
                    Start@1,
                    Name_start
                );

            [{{string, V}, {position, Start@2}},
                {as, _},
                {{name, L}, _},
                {less_greater, _},
                {{name, R}, {position, Name_start@1}} |
                Tokens@3] ->
                Span = {span, Start@2, string_offset(Name_start@1, R)},
                Pattern = {pattern_concatenate,
                    Span,
                    V,
                    {some, {named, L}},
                    {named, R}},
                {ok, {Pattern, Tokens@3}};

            [{{string, V@1}, {position, Start@3}},
                {as, _},
                {{discard_name, L@1}, _},
                {less_greater, _},
                {{name, R@1}, {position, Name_start@2}} |
                Tokens@4] ->
                Span@1 = {span, Start@3, string_offset(Name_start@2, R@1)},
                Pattern@1 = {pattern_concatenate,
                    Span@1,
                    V@1,
                    {some, {discarded, L@1}},
                    {named, R@1}},
                {ok, {Pattern@1, Tokens@4}};

            [{{string, V@2}, {position, Start@4}},
                {less_greater, _},
                {{name, N}, {position, Name_start@3}} |
                Tokens@5] ->
                Span@2 = {span, Start@4, string_offset(Name_start@3, N)},
                Pattern@2 = {pattern_concatenate, Span@2, V@2, none, {named, N}},
                {ok, {Pattern@2, Tokens@5}};

            [{{string, V@3}, {position, Start@5}},
                {less_greater, _},
                {{discard_name, N@1}, {position, Name_start@4}} |
                Tokens@6] ->
                Span@3 = {span, Start@5, string_offset(Name_start@4, N@1) + 1},
                Pattern@3 = {pattern_concatenate,
                    Span@3,
                    V@3,
                    none,
                    {discarded, N@1}},
                {ok, {Pattern@3, Tokens@6}};

            [{{int, Value}, {position, Start@6}} | Tokens@7] ->
                {ok,
                    {{pattern_int, span_from_string(Start@6, Value), Value},
                        Tokens@7}};

            [{{float, Value@1}, {position, Start@7}} | Tokens@8] ->
                {ok,
                    {{pattern_float,
                            span_from_string(Start@7, Value@1),
                            Value@1},
                        Tokens@8}};

            [{{string, Value@2}, {position, Start@8}} | Tokens@9] ->
                {ok,
                    {{pattern_string,
                            {span, Start@8, string_offset(Start@8, Value@2) + 2},
                            Value@2},
                        Tokens@9}};

            [{{discard_name, Name@2}, {position, Start@9}} | Tokens@10] ->
                {ok,
                    {{pattern_discard,
                            {span, Start@9, string_offset(Start@9, Name@2) + 1},
                            Name@2},
                        Tokens@10}};

            [{{name, Name@3}, {position, Start@10}} | Tokens@11] ->
                {ok,
                    {{pattern_variable,
                            span_from_string(Start@10, Name@3),
                            Name@3},
                        Tokens@11}};

            [{left_square, {position, Start@11}} | Tokens@12] ->
                Result = list(
                    fun pattern/1,
                    {some,
                        fun(_capture) ->
                            {pattern_discard, _capture, <<""/utf8>>}
                        end},
                    [],
                    Tokens@12
                ),
                gleam@result:map(
                    Result,
                    fun(_use0) ->
                        {parsed_list, Elements, Rest, Tokens@13, End} = _use0,
                        {{pattern_list, {span, Start@11, End}, Elements, Rest},
                            Tokens@13}
                    end
                );

            [{hash, {position, Start@12}}, {left_paren, _} | Tokens@14] ->
                Result@1 = comma_delimited(
                    [],
                    Tokens@14,
                    fun pattern/1,
                    right_paren
                ),
                gleam@result:'try'(
                    Result@1,
                    fun(_use0@1) ->
                        {Patterns, End@1, Tokens@15} = _use0@1,
                        {ok,
                            {{pattern_tuple, {span, Start@12, End@1}, Patterns},
                                Tokens@15}}
                    end
                );

            [{less_less, {position, Start@13}} | Tokens@16] ->
                Parser = fun(_capture@1) ->
                    bit_string_segment(fun pattern/1, _capture@1)
                end,
                Result@2 = comma_delimited(
                    [],
                    Tokens@16,
                    Parser,
                    greater_greater
                ),
                gleam@result:'try'(
                    Result@2,
                    fun(_use0@2) ->
                        {Segments, End@2, Tokens@17} = _use0@2,
                        {ok,
                            {{pattern_bit_string,
                                    {span, Start@13, End@2},
                                    Segments},
                                Tokens@17}}
                    end
                );

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0@3) ->
            {Pattern@4, Tokens@18} = _use0@3,
            case Tokens@18 of
                [{as, _},
                    {{name, Name@4}, {position, Name_start@5}} |
                    Tokens@19] ->
                    Span@4 = {span,
                        erlang:element(2, erlang:element(2, Pattern@4)),
                        string_offset(Name_start@5, Name@4)},
                    Pattern@5 = {pattern_assignment, Span@4, Pattern@4, Name@4},
                    {ok, {Pattern@5, Tokens@19}};

                _ ->
                    {ok, {Pattern@4, Tokens@18}}
            end
        end).

-file("src/glance.gleam", 879).
-spec pattern_constructor(
    gleam@option:option(binary()),
    binary(),
    list({glexer@token:token(), glexer:position()}),
    integer(),
    integer()
) -> {ok, {pattern(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
pattern_constructor(Module, Constructor, Tokens, Start, Name_start) ->
    case Tokens of
        [{left_paren, _} | Tokens@1] ->
            Result = pattern_constructor_arguments([], Tokens@1),
            gleam@result:'try'(
                Result,
                fun(_use0) ->
                    {pattern_constructor_arguments,
                        Patterns,
                        Spread,
                        End,
                        Tokens@2} = _use0,
                    Arguments = lists:reverse(Patterns),
                    Pattern = {pattern_variant,
                        {span, Start, End},
                        Module,
                        Constructor,
                        Arguments,
                        Spread},
                    {ok, {Pattern, Tokens@2}}
                end
            );

        _ ->
            Span = {span, Start, string_offset(Name_start, Constructor)},
            Pattern@1 = {pattern_variant, Span, Module, Constructor, [], false},
            {ok, {Pattern@1, Tokens}}
    end.

-file("src/glance.gleam", 1649).
-spec case_clause(list({glexer@token:token(), glexer:position()})) -> {ok,
        {clause(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_clause(Tokens) ->
    Multipatterns = fun(_capture) ->
        delimited([], _capture, fun pattern/1, comma)
    end,
    Result = delimited([], Tokens, Multipatterns, v_bar),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Patterns, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_clause_guard(Tokens@1),
                fun(_use0@1) ->
                    {Guard, Tokens@2} = _use0@1,
                    expect(
                        right_arrow,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:map(
                                expression(Tokens@3),
                                fun(_use0@2) ->
                                    {Expression, Tokens@4} = _use0@2,
                                    {{clause, Patterns, Guard, Expression},
                                        Tokens@4}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1636).
-spec case_clauses(
    list(clause()),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(clause()),
            list({glexer@token:token(), glexer:position()}),
            integer()}} |
    {error, error()}.
case_clauses(Clauses, Tokens) ->
    gleam@result:'try'(
        case_clause(Tokens),
        fun(_use0) ->
            {Clause, Tokens@1} = _use0,
            Clauses@1 = [Clause | Clauses],
            case Tokens@1 of
                [{right_brace, {position, End}} | Tokens@2] ->
                    {ok, {lists:reverse(Clauses@1), Tokens@2, End + 1}};

                _ ->
                    case_clauses(Clauses@1, Tokens@1)
            end
        end
    ).

-file("src/glance.gleam", 1614).
-spec case_(list({glexer@token:token(), glexer:position()}), integer()) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_(Tokens, Start) ->
    gleam@result:'try'(
        case_subjects([], Tokens),
        fun(_use0) ->
            {Subjects, Tokens@1} = _use0,
            expect(
                left_brace,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        case_clauses([], Tokens@2),
                        fun(_use0@1) ->
                            {Clauses, Tokens@3, End} = _use0@1,
                            {ok,
                                {{some,
                                        {'case',
                                            {span, Start, End},
                                            Subjects,
                                            Clauses}},
                                    Tokens@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 2002).
-spec named_type(
    binary(),
    gleam@option:option(binary()),
    list({glexer@token:token(), glexer:position()}),
    integer(),
    integer()
) -> {ok, {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
named_type(Name, Module, Tokens, Start, Name_start) ->
    gleam@result:'try'(case Tokens of
            [{left_paren, _} | Tokens@1] ->
                comma_delimited([], Tokens@1, fun type_/1, right_paren);

            _ ->
                End = Name_start + erlang:byte_size(Name),
                {ok, {[], End, Tokens}}
        end, fun(_use0) ->
            {Parameters, End@1, Tokens@2} = _use0,
            T = {named_type, {span, Start, End@1}, Name, Module, Parameters},
            {ok, {T, Tokens@2}}
        end).

-file("src/glance.gleam", 1968).
-spec type_(list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_(Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{fn, {position, I}}, {left_paren, _} | Tokens@1] ->
            fn_type(I, Tokens@1);

        [{hash, {position, I@1}}, {left_paren, _} | Tokens@2] ->
            tuple_type(I@1, Tokens@2);

        [{{name, Module}, {position, Start}},
            {dot, _},
            {{upper_name, Name}, {position, End}} |
            Tokens@3] ->
            named_type(Name, {some, Module}, Tokens@3, Start, End);

        [{{upper_name, Name@1}, {position, Start@1}} | Tokens@4] ->
            named_type(Name@1, none, Tokens@4, Start@1, Start@1);

        [{{discard_name, Name@2}, {position, I@2}} | Tokens@5] ->
            Value = {hole_type,
                {span, I@2, string_offset(I@2, Name@2) + 1},
                Name@2},
            {ok, {Value, Tokens@5}};

        [{{name, Name@3}, {position, I@3}} | Tokens@6] ->
            Value@1 = {variable_type, span_from_string(I@3, Name@3), Name@3},
            {ok, {Value@1, Tokens@6}};

        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}}
    end.

-file("src/glance.gleam", 778).
-spec optional_return_annotation(
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {gleam@option:option(type()),
            integer(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_return_annotation(End, Tokens) ->
    case Tokens of
        [{right_arrow, _} | Tokens@1] ->
            gleam@result:'try'(
                type_(Tokens@1),
                fun(_use0) ->
                    {Return_type, Tokens@2} = _use0,
                    {ok,
                        {{some, Return_type},
                            erlang:element(3, erlang:element(2, Return_type)),
                            Tokens@2}}
                end
            );

        _ ->
            {ok, {none, End, Tokens}}
    end.

-file("src/glance.gleam", 1855).
-spec optional_type_annotation(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(type()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_type_annotation(Tokens) ->
    case Tokens of
        [{colon, _} | Tokens@1] ->
            gleam@result:map(
                type_(Tokens@1),
                fun(_use0) ->
                    {Annotation, Tokens@2} = _use0,
                    {{some, Annotation}, Tokens@2}
                end
            );

        _ ->
            {ok, {none, Tokens}}
    end.

-file("src/glance.gleam", 849).
-spec use_pattern(list({glexer@token:token(), glexer:position()})) -> {ok,
        {use_pattern(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
use_pattern(Tokens) ->
    gleam@result:'try'(
        pattern(Tokens),
        fun(_use0) ->
            {Pattern, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@1),
                fun(_use0@1) ->
                    {Annotation, Tokens@2} = _use0@1,
                    {ok, {{use_pattern, Pattern, Annotation}, Tokens@2}}
                end
            )
        end
    ).

-file("src/glance.gleam", 838).
-spec use_(list({glexer@token:token(), glexer:position()}), integer()) -> {ok,
        {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
use_(Tokens, Start) ->
    gleam@result:'try'(case Tokens of
            [{left_arrow, _} | _] ->
                {ok, {[], Tokens}};

            _ ->
                delimited([], Tokens, fun use_pattern/1, comma)
        end, fun(_use0) ->
            {Patterns, Tokens@1} = _use0,
            expect(
                left_arrow,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        expression(Tokens@2),
                        fun(_use0@1) ->
                            {Function, Tokens@3} = _use0@1,
                            {ok,
                                {{use,
                                        {span,
                                            Start,
                                            erlang:element(
                                                3,
                                                erlang:element(2, Function)
                                            )},
                                        Patterns,
                                        Function},
                                    Tokens@3}}
                        end
                    )
                end
            )
        end).

-file("src/glance.gleam", 857).
-spec assignment(
    assignment_kind(),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
assignment(Kind, Tokens, Start) ->
    gleam@result:'try'(
        pattern(Tokens),
        fun(_use0) ->
            {Pattern, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@1),
                fun(_use0@1) ->
                    {Annotation, Tokens@2} = _use0@1,
                    expect(
                        equal,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:'try'(
                                expression(Tokens@3),
                                fun(_use0@2) ->
                                    {Value, Tokens@4} = _use0@2,
                                    gleam@result:'try'(case {Kind, Tokens@4} of
                                            {{let_assert, none},
                                                [{as, _} | Tokens@5]} ->
                                                gleam@result:map(
                                                    expression(Tokens@5),
                                                    fun(_use0@3) ->
                                                        {Message, Tokens@6} = _use0@3,
                                                        {{let_assert,
                                                                {some, Message}},
                                                            Tokens@6,
                                                            erlang:element(
                                                                3,
                                                                erlang:element(
                                                                    2,
                                                                    Message
                                                                )
                                                            )}
                                                    end
                                                );

                                            {{let_assert, _}, _} ->
                                                {ok,
                                                    {Kind,
                                                        Tokens@4,
                                                        erlang:element(
                                                            3,
                                                            erlang:element(
                                                                2,
                                                                Value
                                                            )
                                                        )}};

                                            {'let', _} ->
                                                {ok,
                                                    {Kind,
                                                        Tokens@4,
                                                        erlang:element(
                                                            3,
                                                            erlang:element(
                                                                2,
                                                                Value
                                                            )
                                                        )}}
                                        end, fun(_use0@4) ->
                                            {Kind@1, Tokens@7, End} = _use0@4,
                                            Statement = {assignment,
                                                {span, Start, End},
                                                Kind@1,
                                                Pattern,
                                                Annotation,
                                                Value},
                                            {ok, {Statement, Tokens@7}}
                                        end)
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1783).
-spec fn_parameter(list({glexer@token:token(), glexer:position()})) -> {ok,
        {fn_parameter(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_parameter(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{name, Name}, _} | Tokens@1] ->
                {ok, {{named, Name}, Tokens@1}};

            [{{discard_name, Name@1}, _} | Tokens@2] ->
                {ok, {{discarded, Name@1}, Tokens@2}};

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0) ->
            {Name@2, Tokens@3} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@3),
                fun(_use0@1) ->
                    {Type_, Tokens@4} = _use0@1,
                    {ok, {{fn_parameter, Name@2, Type_}, Tokens@4}}
                end
            )
        end).

-file("src/glance.gleam", 1799).
-spec function_parameter(list({glexer@token:token(), glexer:position()})) -> {ok,
        {function_parameter(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
function_parameter(Tokens) ->
    gleam@result:'try'(case Tokens of
            [] ->
                {error, unexpected_end_of_input};

            [{{name, Label}, _}, {{discard_name, Name}, _} | Tokens@1] ->
                {ok, {{some, Label}, {discarded, Name}, Tokens@1}};

            [{{discard_name, Name@1}, _} | Tokens@2] ->
                {ok, {none, {discarded, Name@1}, Tokens@2}};

            [{{name, Label@1}, _}, {{name, Name@2}, _} | Tokens@3] ->
                {ok, {{some, Label@1}, {named, Name@2}, Tokens@3}};

            [{{name, Name@3}, _} | Tokens@4] ->
                {ok, {none, {named, Name@3}, Tokens@4}};

            [{Token, Position} | _] ->
                {error, {unexpected_token, Token, Position}}
        end, fun(_use0) ->
            {Label@2, Parameter, Tokens@5} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@5),
                fun(_use0@1) ->
                    {Type_, Tokens@6} = _use0@1,
                    {ok,
                        {{function_parameter, Label@2, Parameter, Type_},
                            Tokens@6}}
                end
            )
        end).

-file("src/glance.gleam", 747).
-spec function_definition(
    module_(),
    list(attribute()),
    publicity(),
    binary(),
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
function_definition(Module, Attributes, Publicity, Name, Start, Tokens) ->
    expect(
        left_paren,
        Tokens,
        fun(_, Tokens@1) ->
            Result = comma_delimited(
                [],
                Tokens@1,
                fun function_parameter/1,
                right_paren
            ),
            gleam@result:'try'(
                Result,
                fun(_use0) ->
                    {Parameters, End, Tokens@2} = _use0,
                    Result@1 = optional_return_annotation(End, Tokens@2),
                    gleam@result:'try'(
                        Result@1,
                        fun(_use0@1) ->
                            {Return_type, End@1, Tokens@3} = _use0@1,
                            gleam@result:'try'(case Tokens@3 of
                                    [{left_brace, _} | Tokens@4] ->
                                        statements([], Tokens@4);

                                    _ ->
                                        {ok, {[], End@1, Tokens@3}}
                                end, fun(_use0@2) ->
                                    {Body, End@2, Tokens@5} = _use0@2,
                                    Location = {span, Start, End@2},
                                    Function = {function,
                                        Location,
                                        Name,
                                        Publicity,
                                        Parameters,
                                        Return_type,
                                        Body},
                                    Module@1 = push_function(
                                        Module,
                                        Attributes,
                                        Function
                                    ),
                                    {ok, {Module@1, Tokens@5}}
                                end)
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1825).
-spec const_definition(
    module_(),
    list(attribute()),
    publicity(),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
const_definition(Module, Attributes, Publicity, Tokens, Start) ->
    expect_name(
        Tokens,
        fun(Name, Tokens@1) ->
            gleam@result:'try'(
                optional_type_annotation(Tokens@1),
                fun(_use0) ->
                    {Annotation, Tokens@2} = _use0,
                    expect(
                        equal,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:'try'(
                                expression(Tokens@3),
                                fun(_use0@1) ->
                                    {Expression, Tokens@4} = _use0@1,
                                    Constant = {constant,
                                        {span,
                                            Start,
                                            erlang:element(
                                                3,
                                                erlang:element(2, Expression)
                                            )},
                                        Name,
                                        Publicity,
                                        Annotation,
                                        Expression},
                                    Module@1 = push_constant(
                                        Module,
                                        Attributes,
                                        Constant
                                    ),
                                    {ok, {Module@1, Tokens@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1952).
-spec type_alias(
    module_(),
    list(attribute()),
    binary(),
    list(binary()),
    publicity(),
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_alias(Module, Attributes, Name, Parameters, Publicity, Start, Tokens) ->
    gleam@result:'try'(
        type_(Tokens),
        fun(_use0) ->
            {Type_, Tokens@1} = _use0,
            Span = {span, Start, erlang:element(3, erlang:element(2, Type_))},
            Alias = {type_alias, Span, Name, Publicity, Parameters, Type_},
            Module@1 = push_type_alias(Module, Attributes, Alias),
            {ok, {Module@1, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 2022).
-spec fn_type(integer(), list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_type(Start, Tokens) ->
    Result = comma_delimited([], Tokens, fun type_/1, right_paren),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Parameters, _, Tokens@1} = _use0,
            expect(
                right_arrow,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        type_(Tokens@2),
                        fun(_use0@1) ->
                            {Return, Tokens@3} = _use0@1,
                            Span = {span,
                                Start,
                                erlang:element(3, erlang:element(2, Return))},
                            {ok,
                                {{function_type, Span, Parameters, Return},
                                    Tokens@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 2031).
-spec tuple_type(integer(), list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
tuple_type(Start, Tokens) ->
    Result = comma_delimited([], Tokens, fun type_/1, right_paren),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Types, End, Tokens@1} = _use0,
            Span = {span, Start, End},
            {ok, {{tuple_type, Span, Types}, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 2100).
-spec variant_field(list({glexer@token:token(), glexer:position()})) -> {ok,
        {variant_field(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
variant_field(Tokens) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            gleam@result:'try'(
                type_(Tokens@1),
                fun(_use0) ->
                    {Type_, Tokens@2} = _use0,
                    {ok, {{labelled_variant_field, Type_, Name}, Tokens@2}}
                end
            );

        Tokens@3 ->
            gleam@result:'try'(
                type_(Tokens@3),
                fun(_use0@1) ->
                    {Type_@1, Tokens@4} = _use0@1,
                    {ok, {{unlabelled_variant_field, Type_@1}, Tokens@4}}
                end
            )
    end.

-file("src/glance.gleam", 2066).
-spec variants(custom_type(), list({glexer@token:token(), glexer:position()})) -> {ok,
        {custom_type(),
            integer(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
variants(Ct, Tokens) ->
    until(
        right_brace,
        Ct,
        Tokens,
        fun(Ct@1, Tokens@1) ->
            gleam@result:'try'(
                attributes([], Tokens@1),
                fun(_use0) ->
                    {Attributes, Tokens@2} = _use0,
                    expect_upper_name(
                        Tokens@2,
                        fun(Name, _, Tokens@3) ->
                            gleam@result:'try'(case Tokens@3 of
                                    [{left_paren, _},
                                        {right_paren, {position, I}} |
                                        Tokens@4] ->
                                        {ok, {[], I, Tokens@4}};

                                    [{left_paren, _} | Tokens@5] ->
                                        comma_delimited(
                                            [],
                                            Tokens@5,
                                            fun variant_field/1,
                                            right_paren
                                        );

                                    _ ->
                                        {ok, {[], 0, Tokens@3}}
                                end, fun(_use0@1) ->
                                    {Fields, _, Tokens@6} = _use0@1,
                                    Ct@2 = push_variant(
                                        Ct@1,
                                        {variant, Name, Fields, Attributes}
                                    ),
                                    {ok, {Ct@2, Tokens@6}}
                                end)
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 2038).
-spec custom_type(
    module_(),
    list(attribute()),
    binary(),
    list(binary()),
    publicity(),
    boolean(),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
custom_type(
    Module,
    Attributes,
    Name,
    Parameters,
    Publicity,
    Opaque_,
    Tokens,
    Start
) ->
    Ct = {custom_type, {span, 0, 0}, Name, Publicity, Opaque_, Parameters, []},
    gleam@result:'try'(
        variants(Ct, Tokens),
        fun(_use0) ->
            {Ct@1, End, Tokens@1} = _use0,
            Ct@2 = {custom_type,
                {span, Start, End},
                erlang:element(3, Ct@1),
                erlang:element(4, Ct@1),
                erlang:element(5, Ct@1),
                erlang:element(6, Ct@1),
                erlang:element(7, Ct@1)},
            Module@1 = push_custom_type(Module, Attributes, Ct@2),
            {ok, {Module@1, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 1903).
-spec type_definition(
    module_(),
    list(attribute()),
    publicity(),
    boolean(),
    list({glexer@token:token(), glexer:position()}),
    integer()
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_definition(Module, Attributes, Publicity, Opaque_, Tokens, Start) ->
    expect_upper_name(
        Tokens,
        fun(Name_value, Name_start, Tokens@1) ->
            gleam@result:'try'(case Tokens@1 of
                    [{left_paren, _} | Tokens@2] ->
                        comma_delimited([], Tokens@2, fun name/1, right_paren);

                    _ ->
                        {ok,
                            {[],
                                string_offset(Name_start, Name_value),
                                Tokens@1}}
                end, fun(_use0) ->
                    {Parameters, End, Tokens@3} = _use0,
                    case Tokens@3 of
                        [{equal, _} | Tokens@4] ->
                            type_alias(
                                Module,
                                Attributes,
                                Name_value,
                                Parameters,
                                Publicity,
                                Start,
                                Tokens@4
                            );

                        [{left_brace, _} | Tokens@5] ->
                            _pipe = Module,
                            custom_type(
                                _pipe,
                                Attributes,
                                Name_value,
                                Parameters,
                                Publicity,
                                Opaque_,
                                Tokens@5,
                                Start
                            );

                        _ ->
                            Span = {span, Start, End},
                            Ct = {custom_type,
                                Span,
                                Name_value,
                                Publicity,
                                Opaque_,
                                Parameters,
                                []},
                            Module@1 = push_custom_type(Module, Attributes, Ct),
                            {ok, {Module@1, Tokens@3}}
                    end
                end)
        end
    ).

-file("src/glance.gleam", 467).
-spec slurp(
    module_(),
    list(attribute()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, module_()} | {error, error()}.
slurp(Module, Attributes, Tokens) ->
    case Tokens of
        [{at, _} | Tokens@1] ->
            gleam@result:'try'(
                attribute(Tokens@1),
                fun(_use0) ->
                    {Attribute, Tokens@2} = _use0,
                    slurp(Module, [Attribute | Attributes], Tokens@2)
                end
            );

        [{import, {position, Start}} | Tokens@3] ->
            Result = import_statement(Module, Attributes, Tokens@3, Start),
            gleam@result:'try'(
                Result,
                fun(_use0@1) ->
                    {Module@1, Tokens@4} = _use0@1,
                    slurp(Module@1, [], Tokens@4)
                end
            );

        [{pub, {position, Start@1}}, {type, _} | Tokens@5] ->
            Result@1 = type_definition(
                Module,
                Attributes,
                public,
                false,
                Tokens@5,
                Start@1
            ),
            gleam@result:'try'(
                Result@1,
                fun(_use0@2) ->
                    {Module@2, Tokens@6} = _use0@2,
                    slurp(Module@2, [], Tokens@6)
                end
            );

        [{pub, {position, Start@2}}, {opaque, _}, {type, _} | Tokens@7] ->
            Result@2 = type_definition(
                Module,
                Attributes,
                public,
                true,
                Tokens@7,
                Start@2
            ),
            gleam@result:'try'(
                Result@2,
                fun(_use0@3) ->
                    {Module@3, Tokens@8} = _use0@3,
                    slurp(Module@3, [], Tokens@8)
                end
            );

        [{type, {position, Start@3}} | Tokens@9] ->
            Result@3 = type_definition(
                Module,
                Attributes,
                private,
                false,
                Tokens@9,
                Start@3
            ),
            gleam@result:'try'(
                Result@3,
                fun(_use0@4) ->
                    {Module@4, Tokens@10} = _use0@4,
                    slurp(Module@4, [], Tokens@10)
                end
            );

        [{pub, {position, Start@4}}, {const, _} | Tokens@11] ->
            Result@4 = const_definition(
                Module,
                Attributes,
                public,
                Tokens@11,
                Start@4
            ),
            gleam@result:'try'(
                Result@4,
                fun(_use0@5) ->
                    {Module@5, Tokens@12} = _use0@5,
                    slurp(Module@5, [], Tokens@12)
                end
            );

        [{const, {position, Start@5}} | Tokens@13] ->
            Result@5 = const_definition(
                Module,
                Attributes,
                private,
                Tokens@13,
                Start@5
            ),
            gleam@result:'try'(
                Result@5,
                fun(_use0@6) ->
                    {Module@6, Tokens@14} = _use0@6,
                    slurp(Module@6, [], Tokens@14)
                end
            );

        [{pub, Start@6}, {fn, _}, {{name, Name}, _} | Tokens@15] ->
            {position, Start@7} = Start@6,
            Result@6 = function_definition(
                Module,
                Attributes,
                public,
                Name,
                Start@7,
                Tokens@15
            ),
            gleam@result:'try'(
                Result@6,
                fun(_use0@7) ->
                    {Module@7, Tokens@16} = _use0@7,
                    slurp(Module@7, [], Tokens@16)
                end
            );

        [{fn, Start@8}, {{name, Name@1}, _} | Tokens@17] ->
            {position, Start@9} = Start@8,
            Result@7 = function_definition(
                Module,
                Attributes,
                private,
                Name@1,
                Start@9,
                Tokens@17
            ),
            gleam@result:'try'(
                Result@7,
                fun(_use0@8) ->
                    {Module@8, Tokens@18} = _use0@8,
                    slurp(Module@8, [], Tokens@18)
                end
            );

        [] ->
            {ok, Module};

        Tokens@19 ->
            unexpected_error(Tokens@19)
    end.

-file("src/glance.gleam", 337).
-spec module(binary()) -> {ok, module_()} | {error, error()}.
module(Src) ->
    _pipe = glexer:new(Src),
    _pipe@1 = glexer:discard_comments(_pipe),
    _pipe@2 = glexer:discard_whitespace(_pipe@1),
    _pipe@3 = glexer:lex(_pipe@2),
    slurp({module, [], [], [], [], []}, [], _pipe@3).
