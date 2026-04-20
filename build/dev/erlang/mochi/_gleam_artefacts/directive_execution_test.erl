-module(directive_execution_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/directive_execution_test.gleam").
-export([uppercase_directive_test/0, uppercase_directive_on_non_string_passes_through_test/0, multiply_directive_test/0, multiply_directive_with_variable_test/0, chained_directives_test/0, chained_directives_reverse_order_test/0, directive_handler_error_test/0, directive_handler_error_stops_chain_test/0, directive_without_handler_passes_through_test/0, unknown_directive_passes_through_test/0, skip_directive_still_works_test/0, include_directive_still_works_test/0, builtin_and_custom_combined_test/0]).
-export_type([message/0]).

-type message() :: {message, binary(), binary(), integer()}.

-file("test/directive_execution_test.gleam", 25).
-spec decode_message(gleam@dynamic:dynamic_()) -> {ok, message()} |
    {error, binary()}.
decode_message(_) ->
    {ok, {message, <<"1"/utf8>>, <<"hello"/utf8>>, 5}}.

-file("test/directive_execution_test.gleam", 30).
-spec try_get_string(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, binary()}.
try_get_string(Value) ->
    _pipe = gleam@dynamic@decode:run(
        Value,
        {decoder, fun gleam@dynamic@decode:decode_string/1}
    ),
    gleam@result:map_error(_pipe, fun(_) -> <<"Not a string"/utf8>> end).

-file("test/directive_execution_test.gleam", 36).
-spec try_get_int(gleam@dynamic:dynamic_()) -> {ok, integer()} |
    {error, binary()}.
try_get_int(Value) ->
    _pipe = gleam@dynamic@decode:run(
        Value,
        {decoder, fun gleam@dynamic@decode:decode_int/1}
    ),
    gleam@result:map_error(_pipe, fun(_) -> <<"Not an int"/utf8>> end).

-file("test/directive_execution_test.gleam", 45).
-spec uppercase_handler(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dynamic:dynamic_()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
uppercase_handler(_, Value) ->
    case try_get_string(Value) of
        {ok, Str} ->
            {ok, gleam_stdlib:identity(string:uppercase(Str))};

        {error, _} ->
            {ok, Value}
    end.

-file("test/directive_execution_test.gleam", 56).
-spec build_uppercase_schema() -> mochi@schema:schema().
build_uppercase_schema() ->
    Uppercase_directive = begin
        _pipe = mochi@schema:directive(<<"uppercase"/utf8>>, [field_location]),
        _pipe@1 = mochi@schema:directive_description(
            _pipe,
            <<"Transforms string to uppercase"/utf8>>
        ),
        mochi@schema:directive_handler(_pipe@1, fun uppercase_handler/2)
    end,
    Message_type = begin
        _pipe@2 = mochi@types:object(<<"Message"/utf8>>),
        _pipe@3 = mochi@types:id(
            _pipe@2,
            <<"id"/utf8>>,
            fun(M) -> erlang:element(2, M) end
        ),
        _pipe@4 = mochi@types:string(
            _pipe@3,
            <<"text"/utf8>>,
            fun(M@1) -> erlang:element(3, M@1) end
        ),
        _pipe@5 = mochi@types:int(
            _pipe@4,
            <<"count"/utf8>>,
            fun(M@2) -> erlang:element(4, M@2) end
        ),
        mochi@types:build(_pipe@5, fun decode_message/1)
    end,
    Message_query = mochi@query:'query'(
        <<"message"/utf8>>,
        {named, <<"Message"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {message, <<"1"/utf8>>, <<"hello world"/utf8>>, 5}
                )}
        end,
        fun(M@3) -> gleam_stdlib:identity(M@3) end
    ),
    _pipe@6 = mochi@query:new(),
    _pipe@7 = mochi@query:add_query(_pipe@6, Message_query),
    _pipe@8 = mochi@query:add_type(_pipe@7, Message_type),
    _pipe@9 = mochi@query:build(_pipe@8),
    mochi@schema:add_directive(_pipe@9, Uppercase_directive).

-file("test/directive_execution_test.gleam", 84).
-spec uppercase_directive_test() -> nil.
uppercase_directive_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 101).
-spec uppercase_directive_on_non_string_passes_through_test() -> nil.
uppercase_directive_on_non_string_passes_through_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        count @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 123).
-spec multiply_handler(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dynamic:dynamic_()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
multiply_handler(Args, Value) ->
    case gleam_stdlib:map_get(Args, <<"by"/utf8>>) of
        {ok, By_value} ->
            case {try_get_int(By_value), try_get_int(Value)} of
                {{ok, Multiplier}, {ok, Num}} ->
                    {ok, gleam_stdlib:identity(Num * Multiplier)};

                {_, _} ->
                    {ok, Value}
            end;

        {error, _} ->
            {error, <<"@multiply directive requires 'by' argument"/utf8>>}
    end.

-file("test/directive_execution_test.gleam", 139).
-spec build_multiply_schema() -> mochi@schema:schema().
build_multiply_schema() ->
    Multiply_directive = begin
        _pipe = mochi@schema:directive(<<"multiply"/utf8>>, [field_location]),
        _pipe@1 = mochi@schema:directive_description(
            _pipe,
            <<"Multiplies a number by the given factor"/utf8>>
        ),
        _pipe@3 = mochi@schema:directive_argument(
            _pipe@1,
            begin
                _pipe@2 = mochi@schema:arg(
                    <<"by"/utf8>>,
                    mochi@schema:non_null(mochi@schema:int_type())
                ),
                mochi@schema:arg_description(_pipe@2, <<"The multiplier"/utf8>>)
            end
        ),
        mochi@schema:directive_handler(_pipe@3, fun multiply_handler/2)
    end,
    Message_type = begin
        _pipe@4 = mochi@types:object(<<"Message"/utf8>>),
        _pipe@5 = mochi@types:id(
            _pipe@4,
            <<"id"/utf8>>,
            fun(M) -> erlang:element(2, M) end
        ),
        _pipe@6 = mochi@types:string(
            _pipe@5,
            <<"text"/utf8>>,
            fun(M@1) -> erlang:element(3, M@1) end
        ),
        _pipe@7 = mochi@types:int(
            _pipe@6,
            <<"count"/utf8>>,
            fun(M@2) -> erlang:element(4, M@2) end
        ),
        mochi@types:build(_pipe@7, fun decode_message/1)
    end,
    Message_query = mochi@query:'query'(
        <<"message"/utf8>>,
        {named, <<"Message"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {message, <<"1"/utf8>>, <<"hello"/utf8>>, 5}
                )}
        end,
        fun(M@3) -> gleam_stdlib:identity(M@3) end
    ),
    _pipe@8 = mochi@query:new(),
    _pipe@9 = mochi@query:add_query(_pipe@8, Message_query),
    _pipe@10 = mochi@query:add_type(_pipe@9, Message_type),
    _pipe@11 = mochi@query:build(_pipe@10),
    mochi@schema:add_directive(_pipe@11, Multiply_directive).

-file("test/directive_execution_test.gleam", 171).
-spec multiply_directive_test() -> nil.
multiply_directive_test() ->
    Schema_def = build_multiply_schema(),
    Query_str = <<"
    query {
      message {
        id
        count @multiply(by: 3)
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 188).
-spec multiply_directive_with_variable_test() -> nil.
multiply_directive_with_variable_test() ->
    Schema_def = build_multiply_schema(),
    Query_str = <<"
    query MultiplyQuery($factor: Int!) {
      message {
        id
        count @multiply(by: $factor)
      }
    }
    "/utf8>>,
    Vars = maps:from_list([{<<"factor"/utf8>>, gleam_stdlib:identity(10)}]),
    Result = mochi@executor:execute_query_with_variables(
        Schema_def,
        Query_str,
        Vars
    ),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 211).
-spec add_prefix_handler(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dynamic:dynamic_()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
add_prefix_handler(Args, Value) ->
    case gleam_stdlib:map_get(Args, <<"prefix"/utf8>>) of
        {ok, Prefix_value} ->
            case {try_get_string(Prefix_value), try_get_string(Value)} of
                {{ok, Prefix}, {ok, Str}} ->
                    {ok, gleam_stdlib:identity(<<Prefix/binary, Str/binary>>)};

                {_, _} ->
                    {ok, Value}
            end;

        {error, _} ->
            {error, <<"@addPrefix directive requires 'prefix' argument"/utf8>>}
    end.

-file("test/directive_execution_test.gleam", 226).
-spec build_chained_schema() -> mochi@schema:schema().
build_chained_schema() ->
    Uppercase_directive = begin
        _pipe = mochi@schema:directive(<<"uppercase"/utf8>>, [field_location]),
        mochi@schema:directive_handler(_pipe, fun uppercase_handler/2)
    end,
    Add_prefix_directive = begin
        _pipe@1 = mochi@schema:directive(<<"addPrefix"/utf8>>, [field_location]),
        _pipe@2 = mochi@schema:directive_argument(
            _pipe@1,
            mochi@schema:arg(<<"prefix"/utf8>>, mochi@schema:string_type())
        ),
        mochi@schema:directive_handler(_pipe@2, fun add_prefix_handler/2)
    end,
    Message_type = begin
        _pipe@3 = mochi@types:object(<<"Message"/utf8>>),
        _pipe@4 = mochi@types:id(
            _pipe@3,
            <<"id"/utf8>>,
            fun(M) -> erlang:element(2, M) end
        ),
        _pipe@5 = mochi@types:string(
            _pipe@4,
            <<"text"/utf8>>,
            fun(M@1) -> erlang:element(3, M@1) end
        ),
        _pipe@6 = mochi@types:int(
            _pipe@5,
            <<"count"/utf8>>,
            fun(M@2) -> erlang:element(4, M@2) end
        ),
        mochi@types:build(_pipe@6, fun decode_message/1)
    end,
    Message_query = mochi@query:'query'(
        <<"message"/utf8>>,
        {named, <<"Message"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {message, <<"1"/utf8>>, <<"hello"/utf8>>, 5}
                )}
        end,
        fun(M@3) -> gleam_stdlib:identity(M@3) end
    ),
    _pipe@7 = mochi@query:new(),
    _pipe@8 = mochi@query:add_query(_pipe@7, Message_query),
    _pipe@9 = mochi@query:add_type(_pipe@8, Message_type),
    _pipe@10 = mochi@query:build(_pipe@9),
    _pipe@11 = mochi@schema:add_directive(_pipe@10, Uppercase_directive),
    mochi@schema:add_directive(_pipe@11, Add_prefix_directive).

-file("test/directive_execution_test.gleam", 259).
-spec chained_directives_test() -> nil.
chained_directives_test() ->
    Schema_def = build_chained_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @uppercase @addPrefix(prefix: \">>>\")
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 279).
-spec chained_directives_reverse_order_test() -> nil.
chained_directives_reverse_order_test() ->
    Schema_def = build_chained_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @addPrefix(prefix: \">>>\") @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 303).
-spec failing_handler(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@dynamic:dynamic_()
) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()}.
failing_handler(_, _) ->
    {error, <<"Directive execution failed intentionally"/utf8>>}.

-file("test/directive_execution_test.gleam", 310).
-spec build_failing_directive_schema() -> mochi@schema:schema().
build_failing_directive_schema() ->
    Fail_directive = begin
        _pipe = mochi@schema:directive(<<"fail"/utf8>>, [field_location]),
        _pipe@1 = mochi@schema:directive_description(
            _pipe,
            <<"A directive that always fails"/utf8>>
        ),
        mochi@schema:directive_handler(_pipe@1, fun failing_handler/2)
    end,
    Message_type = begin
        _pipe@2 = mochi@types:object(<<"Message"/utf8>>),
        _pipe@3 = mochi@types:id(
            _pipe@2,
            <<"id"/utf8>>,
            fun(M) -> erlang:element(2, M) end
        ),
        _pipe@4 = mochi@types:string(
            _pipe@3,
            <<"text"/utf8>>,
            fun(M@1) -> erlang:element(3, M@1) end
        ),
        _pipe@5 = mochi@types:int(
            _pipe@4,
            <<"count"/utf8>>,
            fun(M@2) -> erlang:element(4, M@2) end
        ),
        mochi@types:build(_pipe@5, fun decode_message/1)
    end,
    Message_query = mochi@query:'query'(
        <<"message"/utf8>>,
        {named, <<"Message"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {message, <<"1"/utf8>>, <<"hello"/utf8>>, 5}
                )}
        end,
        fun(M@3) -> gleam_stdlib:identity(M@3) end
    ),
    _pipe@6 = mochi@query:new(),
    _pipe@7 = mochi@query:add_query(_pipe@6, Message_query),
    _pipe@8 = mochi@query:add_type(_pipe@7, Message_type),
    _pipe@9 = mochi@query:build(_pipe@8),
    mochi@schema:add_directive(_pipe@9, Fail_directive).

-file("test/directive_execution_test.gleam", 338).
-spec directive_handler_error_test() -> nil.
directive_handler_error_test() ->
    Schema_def = build_failing_directive_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @fail
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_false(gleam@list:is_empty(erlang:element(3, Result))).

-file("test/directive_execution_test.gleam", 355).
-spec directive_handler_error_stops_chain_test() -> nil.
directive_handler_error_stops_chain_test() ->
    Schema_def = begin
        _pipe = build_failing_directive_schema(),
        mochi@schema:add_directive(
            _pipe,
            begin
                _pipe@1 = mochi@schema:directive(
                    <<"uppercase"/utf8>>,
                    [field_location]
                ),
                mochi@schema:directive_handler(_pipe@1, fun uppercase_handler/2)
            end
        )
    end,
    Query_str = <<"
    query {
      message {
        id
        text @fail @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_false(gleam@list:is_empty(erlang:element(3, Result))).

-file("test/directive_execution_test.gleam", 382).
-spec build_no_handler_schema() -> mochi@schema:schema().
build_no_handler_schema() ->
    Log_directive = begin
        _pipe = mochi@schema:directive(<<"log"/utf8>>, [field_location]),
        mochi@schema:directive_description(
            _pipe,
            <<"Logs field access (no-op in tests)"/utf8>>
        )
    end,
    Message_type = begin
        _pipe@1 = mochi@types:object(<<"Message"/utf8>>),
        _pipe@2 = mochi@types:id(
            _pipe@1,
            <<"id"/utf8>>,
            fun(M) -> erlang:element(2, M) end
        ),
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"text"/utf8>>,
            fun(M@1) -> erlang:element(3, M@1) end
        ),
        _pipe@4 = mochi@types:int(
            _pipe@3,
            <<"count"/utf8>>,
            fun(M@2) -> erlang:element(4, M@2) end
        ),
        mochi@types:build(_pipe@4, fun decode_message/1)
    end,
    Message_query = mochi@query:'query'(
        <<"message"/utf8>>,
        {named, <<"Message"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {message, <<"1"/utf8>>, <<"hello"/utf8>>, 5}
                )}
        end,
        fun(M@3) -> gleam_stdlib:identity(M@3) end
    ),
    _pipe@5 = mochi@query:new(),
    _pipe@6 = mochi@query:add_query(_pipe@5, Message_query),
    _pipe@7 = mochi@query:add_type(_pipe@6, Message_type),
    _pipe@8 = mochi@query:build(_pipe@7),
    mochi@schema:add_directive(_pipe@8, Log_directive).

-file("test/directive_execution_test.gleam", 410).
-spec directive_without_handler_passes_through_test() -> nil.
directive_without_handler_passes_through_test() ->
    Schema_def = build_no_handler_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @log
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 431).
-spec unknown_directive_passes_through_test() -> nil.
unknown_directive_passes_through_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @unknownDirective
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 453).
-spec skip_directive_still_works_test() -> nil.
skip_directive_still_works_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @skip(if: true)
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 470).
-spec include_directive_still_works_test() -> nil.
include_directive_still_works_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @include(if: true) @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_execution_test.gleam", 487).
-spec builtin_and_custom_combined_test() -> nil.
builtin_and_custom_combined_test() ->
    Schema_def = build_uppercase_schema(),
    Query_str = <<"
    query {
      message {
        id
        text @skip(if: false) @uppercase
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).
