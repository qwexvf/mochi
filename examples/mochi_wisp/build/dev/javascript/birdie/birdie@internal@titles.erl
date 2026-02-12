-module(birdie@internal@titles).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie/internal/titles.gleam").
-export([new/0, literals/1, prefixes/1, find/2, from_module/3, from_test_directory/0]).
-export_type([titles/0, test_info/0, match/0, error/0, birdie_import/0, snap_title/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque titles() :: {titles,
        gleam@dict:dict(binary(), test_info()),
        trie:trie(binary(), test_info())}.

-type test_info() :: {test_info, binary(), binary()}.

-type match() :: {literal, test_info()} | {prefix, test_info(), binary()}.

-type error() :: {cannot_find_project_root, simplifile:file_error()} |
    {cannot_read_test_directory, simplifile:file_error()} |
    {cannot_read_test_file, simplifile:file_error(), binary()} |
    {duplicate_literal_titles, binary(), test_info(), test_info()} |
    {overlapping_prefixes, binary(), test_info(), binary(), test_info()} |
    {prefix_overlapping_with_literal_title, binary(), test_info(), test_info()}.

-type birdie_import() :: {unqualified, binary()} |
    {qualified, binary(), binary()} |
    {discarded, binary()}.

-type snap_title() :: {literal_title, binary()} | {prefix_title, binary()}.

-file("src/birdie/internal/titles.gleam", 74).
?DOC(false).
-spec new() -> titles().
new() ->
    {titles, maps:new(), trie:new()}.

-file("src/birdie/internal/titles.gleam", 78).
?DOC(false).
-spec add_literal_title(titles(), binary(), test_info()) -> titles().
add_literal_title(Titles, Title, Info) ->
    Literals = erlang:element(2, Titles),
    New_literals = gleam@dict:insert(Literals, Title, Info),
    {titles, New_literals, erlang:element(3, Titles)}.

-file("src/birdie/internal/titles.gleam", 90).
?DOC(false).
-spec literals(titles()) -> gleam@dict:dict(binary(), test_info()).
literals(Titles) ->
    {titles, Literals, _} = Titles,
    Literals.

-file("src/birdie/internal/titles.gleam", 95).
?DOC(false).
-spec prefixes(titles()) -> gleam@dict:dict(binary(), test_info()).
prefixes(Titles) ->
    {titles, _, Prefixes} = Titles,
    _pipe = Prefixes,
    _pipe@1 = trie:to_list(_pipe),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Pair) ->
            {Prefix, Info} = Pair,
            {gleam@string:join(Prefix, <<""/utf8>>), Info}
        end
    ),
    maps:from_list(_pipe@2).

-file("src/birdie/internal/titles.gleam", 108).
?DOC(false).
-spec find(titles(), binary()) -> {ok, match()} | {error, nil}.
find(Titles, Title) ->
    Literal_match = gleam@result:map(
        gleam_stdlib:map_get(erlang:element(2, Titles), Title),
        fun(Field@0) -> {literal, Field@0} end
    ),
    gleam@result:lazy_or(
        Literal_match,
        fun() ->
            Letters = gleam@string:to_graphemes(Title),
            gleam@result:'try'(
                trie:subtrie(erlang:element(3, Titles), Letters),
                fun(Matching_prefixes) ->
                    case trie:to_list(Matching_prefixes) of
                        [{Prefix, Info} | _] ->
                            {ok,
                                {prefix,
                                    Info,
                                    gleam@string:join(Prefix, <<""/utf8>>)}};

                        _ ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-file("src/birdie/internal/titles.gleam", 221).
?DOC(false).
-spec imported_snap(list(glance:unqualified_import())) -> {ok, binary()} |
    {error, nil}.
imported_snap(Values) ->
    gleam@list:fold_until(Values, {error, nil}, fun(Nil, Value) -> case Value of
                {unqualified_import, <<"snap"/utf8>>, none} ->
                    {stop, {ok, <<"snap"/utf8>>}};

                {unqualified_import, <<"snap"/utf8>>, {some, Alias}} ->
                    {stop, {ok, Alias}};

                _ ->
                    {continue, Nil}
            end end).

-file("src/birdie/internal/titles.gleam", 187).
?DOC(false).
-spec birdie_import(glance:module_()) -> {ok, birdie_import()} | {error, nil}.
birdie_import(Module) ->
    gleam@list:fold_until(
        erlang:element(2, Module),
        {error, nil},
        fun(Nil, Import_) -> case erlang:element(3, Import_) of
                {import,
                    _,
                    <<"birdie"/utf8>>,
                    Birdie_alias,
                    _,
                    Unqualified_values} ->
                    case Birdie_alias of
                        {some, {discarded, _}} ->
                            case imported_snap(Unqualified_values) of
                                {ok, Snap_alias} ->
                                    {stop, {ok, {discarded, Snap_alias}}};

                                {error, _} ->
                                    {stop, {error, nil}}
                            end;

                        {some, {named, Module_name}} ->
                            case imported_snap(Unqualified_values) of
                                {ok, Snap_alias@1} ->
                                    {stop,
                                        {ok,
                                            {qualified,
                                                Module_name,
                                                Snap_alias@1}}};

                                {error, _} ->
                                    {stop, {ok, {unqualified, Module_name}}}
                            end;

                        none ->
                            case imported_snap(Unqualified_values) of
                                {ok, Snap_alias@2} ->
                                    {stop,
                                        {ok,
                                            {qualified,
                                                <<"birdie"/utf8>>,
                                                Snap_alias@2}}};

                                {error, _} ->
                                    {stop,
                                        {ok, {unqualified, <<"birdie"/utf8>>}}}
                            end
                    end;

                _ ->
                    {continue, Nil}
            end end
    ).

-file("src/birdie/internal/titles.gleam", 366).
?DOC(false).
-spec is_snap_function(glance:expression(), birdie_import()) -> boolean().
is_snap_function(Expression, Birdie_import) ->
    Is_a_call_to_snap = fun(Module, Name) -> case {Module, Birdie_import} of
            {none, {unqualified, _}} ->
                false;

            {none, {qualified, _, Snap}} ->
                Snap =:= Name;

            {none, {discarded, Snap@1}} ->
                Snap@1 =:= Name;

            {{some, Module@1}, {qualified, Birdie, Snap@2}} ->
                (<<<<Module@1/binary, "."/utf8>>/binary, Name/binary>>) =:= (<<<<Birdie/binary,
                        "."/utf8>>/binary,
                    Snap@2/binary>>);

            {{some, Module@2}, {unqualified, Birdie@1}} ->
                (<<<<Module@2/binary, "."/utf8>>/binary, Name/binary>>) =:= (<<Birdie@1/binary,
                    ".snap"/utf8>>);

            {{some, _}, {discarded, _}} ->
                false
        end end,
    case Expression of
        {variable, _, Name@1} ->
            Is_a_call_to_snap(none, Name@1);

        {field_access, _, {variable, _, Module@3}, Name@2} ->
            Is_a_call_to_snap({some, Module@3}, Name@2);

        _ ->
            false
    end.

-file("src/birdie/internal/titles.gleam", 391).
?DOC(false).
-spec expression_to_snap_title(glance:expression()) -> {ok, snap_title()} |
    {error, nil}.
expression_to_snap_title(Expression) ->
    case Expression of
        {string, _, Title} ->
            {ok, {literal_title, Title}};

        {binary_operator, _, concatenate, {string, _, Prefix}, _} ->
            {ok, {prefix_title, Prefix}};

        _ ->
            {error, nil}
    end.

-file("src/birdie/internal/titles.gleam", 231).
?DOC(false).
-spec snap_call(birdie_import(), glance:expression()) -> {ok, snap_title()} |
    {error, nil}.
snap_call(Birdie_import, Expression) ->
    case Expression of
        {call,
            _,
            Function,
            [{unlabelled_field, Title},
                {labelled_field, <<"content"/utf8>>, _, _}]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {call, _, Function, [{unlabelled_field, _}, {unlabelled_field, Title}]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {call,
            _,
            Function,
            [{unlabelled_field, _},
                {labelled_field, <<"title"/utf8>>, _, Title}]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {call,
            _,
            Function,
            [{labelled_field, <<"content"/utf8>>, _, _},
                {unlabelled_field, Title}]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {call,
            _,
            Function,
            [{labelled_field, <<"content"/utf8>>, _, _},
                {labelled_field, <<"title"/utf8>>, _, Title}]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {call, _, Function, [{labelled_field, <<"title"/utf8>>, _, Title}, _]} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {binary_operator,
            _,
            pipe,
            Title,
            {call, _, Function, [{labelled_field, <<"content"/utf8>>, _, _}]}} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {binary_operator,
            _,
            pipe,
            _,
            {call, _, Function, [{unlabelled_field, Title}]}} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {binary_operator,
            _,
            pipe,
            _,
            {call, _, Function, [{labelled_field, <<"title"/utf8>>, _, Title}]}} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {binary_operator,
            _,
            pipe,
            Title,
            {fn_capture, _, {some, <<"title"/utf8>>}, Function, _, _}} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        {binary_operator,
            _,
            pipe,
            Title,
            {fn_capture, _, _, Function, [{unlabelled_field, _}], []}} ->
            Is_snap_function = is_snap_function(Function, Birdie_import),
            gleam@bool:guard(
                not Is_snap_function,
                {error, nil},
                fun() -> expression_to_snap_title(Title) end
            );

        _ ->
            {error, nil}
    end.

-file("src/birdie/internal/titles.gleam", 568).
?DOC(false).
-spec try_or({ok, KYC} | {error, any()}, KYG, fun((KYC) -> KYG)) -> KYG.
try_or(Result, Default, Fun) ->
    case Result of
        {ok, A} ->
            Fun(A);

        {error, _} ->
            Default
    end.

-file("src/birdie/internal/titles.gleam", 575).
?DOC(false).
-spec 'try'(
    {ok, KYH} | {error, KYI},
    fun((KYI) -> KYL),
    fun((KYH) -> {ok, KYM} | {error, KYL})
) -> {ok, KYM} | {error, KYL}.
'try'(Result, Map_error, Fun) ->
    case Result of
        {ok, A} ->
            Fun(A);

        {error, E} ->
            {error, Map_error(E)}
    end.

-file("src/birdie/internal/titles.gleam", 435).
?DOC(false).
-spec try_fold_expression(
    glance:expression(),
    KXA,
    fun((KXA, glance:expression()) -> {ok, KXA} | {error, KXB})
) -> {ok, KXA} | {error, KXB}.
try_fold_expression(Expression, Acc, Fun) ->
    gleam@result:'try'(Fun(Acc, Expression), fun(Acc@1) -> case Expression of
                {int, _, _} ->
                    {ok, Acc@1};

                {float, _, _} ->
                    {ok, Acc@1};

                {string, _, _} ->
                    {ok, Acc@1};

                {variable, _, _} ->
                    {ok, Acc@1};

                {panic, _, _} ->
                    {ok, Acc@1};

                {todo, _, _} ->
                    {ok, Acc@1};

                {echo, _, none, _} ->
                    {ok, Acc@1};

                {negate_int, _, Expression@1} ->
                    try_fold_expression(Expression@1, Acc@1, Fun);

                {negate_bool, _, Expression@1} ->
                    try_fold_expression(Expression@1, Acc@1, Fun);

                {echo, _, {some, Expression@1}, _} ->
                    try_fold_expression(Expression@1, Acc@1, Fun);

                {field_access, _, Expression@1, _} ->
                    try_fold_expression(Expression@1, Acc@1, Fun);

                {tuple_index, _, Expression@1, _} ->
                    try_fold_expression(Expression@1, Acc@1, Fun);

                {block, _, Statements} ->
                    try_fold_statements(Statements, Acc@1, Fun);

                {tuple, _, Expressions} ->
                    try_fold_expressions(Expressions, Acc@1, Fun);

                {list, _, Expressions, none} ->
                    try_fold_expressions(Expressions, Acc@1, Fun);

                {list, _, Elements, {some, Rest}} ->
                    gleam@result:'try'(
                        try_fold_expressions(Elements, Acc@1, Fun),
                        fun(Acc@2) -> try_fold_expression(Rest, Acc@2, Fun) end
                    );

                {fn, _, _, _, Statements@1} ->
                    try_fold_statements(Statements@1, Acc@1, Fun);

                {record_update, _, _, _, Record, Fields} ->
                    gleam@result:'try'(
                        try_fold_expression(Record, Acc@1, Fun),
                        fun(Acc@3) ->
                            gleam@list:try_fold(
                                Fields,
                                Acc@3,
                                fun(Acc@4, Field) ->
                                    {record_update_field, _, Item} = Field,
                                    case Item of
                                        {some, Item@1} ->
                                            try_fold_expression(
                                                Item@1,
                                                Acc@4,
                                                Fun
                                            );

                                        none ->
                                            {ok, Acc@4}
                                    end
                                end
                            )
                        end
                    );

                {call, _, Function, Arguments} ->
                    gleam@result:'try'(
                        try_fold_expression(Function, Acc@1, Fun),
                        fun(Acc@5) -> try_fold_fields(Arguments, Acc@5, Fun) end
                    );

                {fn_capture,
                    _,
                    _,
                    Function@1,
                    Arguments_before,
                    Arguments_after} ->
                    gleam@result:'try'(
                        try_fold_expression(Function@1, Acc@1, Fun),
                        fun(Acc@6) ->
                            gleam@result:'try'(
                                try_fold_fields(Arguments_before, Acc@6, Fun),
                                fun(Acc@7) ->
                                    try_fold_fields(Arguments_after, Acc@7, Fun)
                                end
                            )
                        end
                    );

                {bit_string, _, Segments} ->
                    gleam@list:try_fold(
                        Segments,
                        Acc@1,
                        fun(Acc@8, _use1) ->
                            {Segment, Options} = _use1,
                            gleam@result:'try'(
                                try_fold_expression(Segment, Acc@8, Fun),
                                fun(Acc@9) ->
                                    gleam@list:try_fold(
                                        Options,
                                        Acc@9,
                                        fun(Acc@10, Option) -> case Option of
                                                {size_value_option,
                                                    Expression@2} ->
                                                    try_fold_expression(
                                                        Expression@2,
                                                        Acc@10,
                                                        Fun
                                                    );

                                                _ ->
                                                    {ok, Acc@10}
                                            end end
                                    )
                                end
                            )
                        end
                    );

                {'case', _, Subjects, Clauses} ->
                    gleam@result:'try'(
                        try_fold_expressions(Subjects, Acc@1, Fun),
                        fun(Acc@11) ->
                            try_fold_clauses(Clauses, Acc@11, Fun)
                        end
                    );

                {binary_operator, _, _, Left, Right} ->
                    gleam@result:'try'(
                        try_fold_expression(Left, Acc@1, Fun),
                        fun(Acc@12) ->
                            try_fold_expression(Right, Acc@12, Fun)
                        end
                    )
            end end).

-file("src/birdie/internal/titles.gleam", 541).
?DOC(false).
-spec try_fold_clauses(
    list(glance:clause()),
    KXP,
    fun((KXP, glance:expression()) -> {ok, KXP} | {error, KXQ})
) -> {ok, KXP} | {error, KXQ}.
try_fold_clauses(Clauses, Acc, Fun) ->
    gleam@list:try_fold(Clauses, Acc, fun(Acc@1, Clause) -> case Clause of
                {clause, _, none, Body} ->
                    try_fold_expression(Body, Acc@1, Fun);

                {clause, _, {some, Guard}, Body@1} ->
                    gleam@result:'try'(
                        try_fold_expression(Guard, Acc@1, Fun),
                        fun(Acc@2) ->
                            try_fold_expression(Body@1, Acc@2, Fun)
                        end
                    )
            end end).

-file("src/birdie/internal/titles.gleam", 408).
?DOC(false).
-spec try_fold_statements(
    list(glance:statement()),
    KWU,
    fun((KWU, glance:expression()) -> {ok, KWU} | {error, KWV})
) -> {ok, KWU} | {error, KWV}.
try_fold_statements(Statements, Acc, Fun) ->
    gleam@list:try_fold(
        Statements,
        Acc,
        fun(Acc@1, Statement) -> case Statement of
                {use, _, _, Expression} ->
                    try_fold_expression(Expression, Acc@1, Fun);

                {assert, _, Expression, none} ->
                    try_fold_expression(Expression, Acc@1, Fun);

                {assignment, _, _, _, _, Expression} ->
                    try_fold_expression(Expression, Acc@1, Fun);

                {expression, Expression} ->
                    try_fold_expression(Expression, Acc@1, Fun);

                {assert, _, Expression@1, {some, Message}} ->
                    case try_fold_expression(Expression@1, Acc@1, Fun) of
                        {ok, Acc@2} ->
                            try_fold_expression(Message, Acc@2, Fun);

                        {error, _} = E ->
                            E
                    end
            end end
    ).

-file("src/birdie/internal/titles.gleam", 141).
?DOC(false).
-spec from_module(titles(), binary(), glance:module_()) -> {ok, titles()} |
    {error, error()}.
from_module(Titles, Name, Module) ->
    try_or(
        birdie_import(Module),
        {ok, Titles},
        fun(Birdie_import) ->
            gleam@list:try_fold(
                erlang:element(6, Module),
                Titles,
                fun(Titles@1, Function) ->
                    Body = erlang:element(7, erlang:element(3, Function)),
                    try_fold_statements(
                        Body,
                        Titles@1,
                        fun(Titles@2, Expression) ->
                            case snap_call(Birdie_import, Expression) of
                                {ok, {literal_title, Title}} ->
                                    Info = {test_info,
                                        Name,
                                        erlang:element(
                                            3,
                                            erlang:element(3, Function)
                                        )},
                                    case find(Titles@2, Title) of
                                        {error, nil} ->
                                            {ok,
                                                add_literal_title(
                                                    Titles@2,
                                                    Title,
                                                    Info
                                                )};

                                        {ok, {prefix, Prefix_info, Prefix}} ->
                                            {error,
                                                {prefix_overlapping_with_literal_title,
                                                    Prefix,
                                                    Prefix_info,
                                                    Info}};

                                        {ok, {literal, Other_info}} ->
                                            {error,
                                                {duplicate_literal_titles,
                                                    Title,
                                                    Info,
                                                    Other_info}}
                                    end;

                                {ok, {prefix_title, _}} ->
                                    {ok, Titles@2};

                                {error, nil} ->
                                    {ok, Titles@2}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie/internal/titles.gleam", 124).
?DOC(false).
-spec from_test_directory() -> {ok, titles()} | {error, error()}.
from_test_directory() ->
    'try'(
        birdie@internal@project:find_root(),
        fun(Field@0) -> {cannot_find_project_root, Field@0} end,
        fun(Root) ->
            Test_directory = filepath:join(Root, <<"test"/utf8>>),
            Get_files = simplifile:get_files(Test_directory),
            'try'(
                Get_files,
                fun(Field@0) -> {cannot_read_test_directory, Field@0} end,
                fun(Files) ->
                    gleam@list:try_fold(
                        Files,
                        new(),
                        fun(Titles, File) ->
                            Is_gleam_file = filepath:extension(File) =:= {ok,
                                <<"gleam"/utf8>>},
                            gleam@bool:guard(
                                not Is_gleam_file,
                                {ok, Titles},
                                fun() ->
                                    'try'(
                                        simplifile:read(File),
                                        fun(_capture) ->
                                            {cannot_read_test_file,
                                                _capture,
                                                File}
                                        end,
                                        fun(Raw_module) ->
                                            case glance:module(Raw_module) of
                                                {ok, Module} ->
                                                    from_module(
                                                        Titles,
                                                        File,
                                                        Module
                                                    );

                                                {error, _} ->
                                                    {ok, Titles}
                                            end
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

-file("src/birdie/internal/titles.gleam", 528).
?DOC(false).
-spec try_fold_fields(
    list(glance:field(glance:expression())),
    KXI,
    fun((KXI, glance:expression()) -> {ok, KXI} | {error, KXJ})
) -> {ok, KXI} | {error, KXJ}.
try_fold_fields(Fields, Acc, Fun) ->
    gleam@list:try_fold(Fields, Acc, fun(Acc@1, Field) -> case Field of
                {labelled_field, _, _, Item} ->
                    try_fold_expression(Item, Acc@1, Fun);

                {unlabelled_field, Item} ->
                    try_fold_expression(Item, Acc@1, Fun);

                {shorthand_field, _, _} ->
                    {ok, Acc@1}
            end end).

-file("src/birdie/internal/titles.gleam", 557).
?DOC(false).
-spec try_fold_expressions(
    list(glance:expression()),
    KXW,
    fun((KXW, glance:expression()) -> {ok, KXW} | {error, KXX})
) -> {ok, KXW} | {error, KXX}.
try_fold_expressions(Expressions, Acc, Fun) ->
    gleam@list:try_fold(
        Expressions,
        Acc,
        fun(Acc@1, Expression) ->
            try_fold_expression(Expression, Acc@1, Fun)
        end
    ).
