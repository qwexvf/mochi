-module(mochi@batch).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/batch.gleam").
-export([default_config/0, with_max_batch_size/2, with_continue_on_error/2, with_parallel_execution/2, request/1, request_with_variables/2, request_with_operation/2, full_request/3, execute_with_operation_name/6, execute_batch/4, parse_batch_request/1, parse_batch_requests/1]).
-export_type([batch_request/0, batch_config/0, batch_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type batch_request() :: {batch_request,
        binary(),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())),
        gleam@option:option(binary())}.

-type batch_config() :: {batch_config, integer(), boolean(), boolean()}.

-type batch_result() :: {batch_result,
        list(mochi@executor:execution_result()),
        boolean(),
        integer()}.

-file("src/mochi/batch.gleam", 66).
?DOC(" Create default batch configuration\n").
-spec default_config() -> batch_config().
default_config() ->
    {batch_config, 10, true, false}.

-file("src/mochi/batch.gleam", 75).
?DOC(" Set maximum batch size\n").
-spec with_max_batch_size(batch_config(), integer()) -> batch_config().
with_max_batch_size(Config, Size) ->
    {batch_config, Size, erlang:element(3, Config), erlang:element(4, Config)}.

-file("src/mochi/batch.gleam", 80).
?DOC(" Configure whether to continue on error\n").
-spec with_continue_on_error(batch_config(), boolean()) -> batch_config().
with_continue_on_error(Config, Continue) ->
    {batch_config,
        erlang:element(2, Config),
        Continue,
        erlang:element(4, Config)}.

-file("src/mochi/batch.gleam", 88).
?DOC(" Configure parallel execution\n").
-spec with_parallel_execution(batch_config(), boolean()) -> batch_config().
with_parallel_execution(Config, Parallel) ->
    {batch_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        Parallel}.

-file("src/mochi/batch.gleam", 100).
?DOC(" Create a batch request with just a query\n").
-spec request(binary()) -> batch_request().
request(Query) ->
    {batch_request, Query, none, none}.

-file("src/mochi/batch.gleam", 105).
?DOC(" Create a batch request with variables\n").
-spec request_with_variables(
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> batch_request().
request_with_variables(Query, Variables) ->
    {batch_request, Query, {some, Variables}, none}.

-file("src/mochi/batch.gleam", 113).
?DOC(" Create a batch request with operation name\n").
-spec request_with_operation(binary(), binary()) -> batch_request().
request_with_operation(Query, Operation_name) ->
    {batch_request, Query, none, {some, Operation_name}}.

-file("src/mochi/batch.gleam", 125).
?DOC(" Create a full batch request\n").
-spec full_request(
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> batch_request().
full_request(Query, Variables, Operation_name) ->
    {batch_request, Query, {some, Variables}, {some, Operation_name}}.

-file("src/mochi/batch.gleam", 203).
?DOC(" Get the name of an operation\n").
-spec get_operation_name(mochi@ast:operation()) -> gleam@option:option(binary()).
get_operation_name(Operation) ->
    case Operation of
        {operation, _, Name, _, _, _} ->
            Name;

        {shorthand_query, _} ->
            none
    end.

-file("src/mochi/batch.gleam", 171).
?DOC(" Find an operation in a document by name\n").
-spec find_operation(mochi@ast:document(), gleam@option:option(binary())) -> {ok,
        mochi@ast:operation()} |
    {error, binary()}.
find_operation(Document, Operation_name) ->
    Operations = begin
        _pipe = erlang:element(2, Document),
        gleam@list:filter_map(_pipe, fun(Def) -> case Def of
                    {operation_definition, Op} ->
                        {ok, Op};

                    {fragment_definition, _} ->
                        {error, nil}
                end end)
    end,
    case {Operation_name, Operations} of
        {none, [Single]} ->
            {ok, Single};

        {none, []} ->
            {error, <<"Document contains no operations"/utf8>>};

        {none, _} ->
            {error,
                <<"Document contains multiple operations, operation name is required"/utf8>>};

        {{some, Name}, Ops} ->
            _pipe@1 = Ops,
            _pipe@2 = gleam@list:find(
                _pipe@1,
                fun(Op@1) -> get_operation_name(Op@1) =:= {some, Name} end
            ),
            gleam@result:map_error(
                _pipe@2,
                fun(_) ->
                    <<<<"Operation '"/utf8, Name/binary>>/binary,
                        "' not found in document"/utf8>>
                end
            )
    end.

-file("src/mochi/batch.gleam", 211).
?DOC(" Extract all fragment definitions from a document\n").
-spec extract_fragments(mochi@ast:document()) -> list(mochi@ast:fragment()).
extract_fragments(Document) ->
    _pipe = erlang:element(2, Document),
    gleam@list:filter_map(_pipe, fun(Def) -> case Def of
                {fragment_definition, Fragment} ->
                    {ok, Fragment};

                {operation_definition, _} ->
                    {error, nil}
            end end).

-file("src/mochi/batch.gleam", 143).
?DOC(
    " Execute a query with a specific operation name selected\n"
    " This is the core function for operation selection in batched queries\n"
).
-spec execute_with_operation_name(
    mochi@schema:schema(),
    mochi@ast:document(),
    gleam@option:option(gleam@dynamic:dynamic_()),
    mochi@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    gleam@option:option(binary())
) -> mochi@executor:execution_result().
execute_with_operation_name(
    Schema_def,
    Document,
    Root_value,
    Ctx,
    Variables,
    Operation_name
) ->
    case find_operation(Document, Operation_name) of
        {ok, Operation} ->
            Fragments = extract_fragments(Document),
            New_doc = {document,
                [{operation_definition, Operation} |
                    gleam@list:map(
                        Fragments,
                        fun(F) -> {fragment_definition, F} end
                    )]},
            mochi@executor:execute(
                Schema_def,
                New_doc,
                Root_value,
                Ctx,
                Variables
            );

        {error, Msg} ->
            {execution_result, none, [{validation_error, Msg, [], none}]}
    end.

-file("src/mochi/batch.gleam", 339).
-spec has_errors(mochi@executor:execution_result()) -> boolean().
has_errors(Result) ->
    not gleam@list:is_empty(erlang:element(3, Result)).

-file("src/mochi/batch.gleam", 343).
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

-file("src/mochi/batch.gleam", 311).
-spec execute_single_request(
    mochi@schema:schema(),
    batch_request(),
    mochi@schema:execution_context()
) -> mochi@executor:execution_result().
execute_single_request(Schema_def, Req, Ctx) ->
    Variables = gleam@option:unwrap(erlang:element(3, Req), maps:new()),
    case mochi@executor:get_or_parse(Schema_def, erlang:element(2, Req)) of
        {ok, Document} ->
            execute_with_operation_name(
                Schema_def,
                Document,
                none,
                Ctx,
                Variables,
                erlang:element(4, Req)
            );

        {error, Parse_error} ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Parse error: "/utf8,
                            (format_parse_error(Parse_error))/binary>>,
                        [],
                        none}]}
    end.

-file("src/mochi/batch.gleam", 253).
-spec execute_batch_internal(
    mochi@schema:schema(),
    list(batch_request()),
    batch_config(),
    mochi@schema:execution_context()
) -> batch_result().
execute_batch_internal(Schema_def, Requests, Config, Ctx) ->
    Final_results = case erlang:element(4, Config) of
        true ->
            _pipe = Requests,
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(Req) ->
                    fun() -> execute_single_request(Schema_def, Req, Ctx) end
                end
            ),
            mochi_parallel_ffi:parallel_map(_pipe@1);

        false ->
            _pipe@2 = gleam@list:fold(
                Requests,
                {[], false},
                fun(Acc, Req@1) ->
                    {Results_acc, Has_failure} = Acc,
                    case {erlang:element(3, Config), Has_failure} of
                        {false, true} ->
                            {[{execution_result,
                                        none,
                                        [{validation_error,
                                                <<"Batch execution halted due to error"/utf8>>,
                                                [],
                                                none}]} |
                                    Results_acc],
                                true};

                        {_, _} ->
                            Result = execute_single_request(
                                Schema_def,
                                Req@1,
                                Ctx
                            ),
                            New_has_failure = Has_failure orelse has_errors(
                                Result
                            ),
                            {[Result | Results_acc], New_has_failure}
                    end
                end
            ),
            (fun(Pair) -> lists:reverse(erlang:element(1, Pair)) end)(_pipe@2)
    end,
    Failure_count = begin
        _pipe@3 = Final_results,
        gleam@list:count(_pipe@3, fun has_errors/1)
    end,
    {batch_result, Final_results, Failure_count =:= 0, Failure_count}.

-file("src/mochi/batch.gleam", 226).
?DOC(" Execute a batch of requests\n").
-spec execute_batch(
    mochi@schema:schema(),
    list(batch_request()),
    batch_config(),
    mochi@schema:execution_context()
) -> batch_result().
execute_batch(Schema_def, Requests, Config, Ctx) ->
    case erlang:length(Requests) > erlang:element(2, Config) of
        true ->
            {batch_result,
                [{execution_result,
                        none,
                        [{validation_error,
                                <<"Batch size exceeds maximum of "/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(2, Config)
                                    ))/binary>>,
                                [],
                                none}]}],
                false,
                1};

        false ->
            execute_batch_internal(Schema_def, Requests, Config, Ctx)
    end.

-file("src/mochi/batch.gleam", 431).
-spec get_list_raw(gleam@dynamic:dynamic_()) -> {ok,
        list(gleam@dynamic:dynamic_())} |
    {error, nil}.
get_list_raw(Value) ->
    _pipe = gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ),
    gleam@result:map_error(_pipe, fun(_) -> nil end).

-file("src/mochi/batch.gleam", 436).
-spec extract_string_field(gleam@dynamic:dynamic_(), binary()) -> {ok, binary()} |
    {error, nil}.
extract_string_field(Value, Field) ->
    _pipe = gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:at(
            [Field],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ),
    gleam@result:map_error(_pipe, fun(_) -> nil end).

-file("src/mochi/batch.gleam", 410).
-spec extract_query(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, binary()}.
extract_query(Value) ->
    case extract_string_field(Value, <<"query"/utf8>>) of
        {ok, Q} ->
            {ok, Q};

        {error, _} ->
            {error, <<"Missing or invalid 'query' field"/utf8>>}
    end.

-file("src/mochi/batch.gleam", 424).
-spec extract_operation_name(gleam@dynamic:dynamic_()) -> gleam@option:option(binary()).
extract_operation_name(Value) ->
    case extract_string_field(Value, <<"operationName"/utf8>>) of
        {ok, Name} ->
            {some, Name};

        {error, _} ->
            none
    end.

-file("src/mochi/batch.gleam", 441).
-spec extract_dict_field(gleam@dynamic:dynamic_(), binary()) -> {ok,
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
    {error, nil}.
extract_dict_field(Value, Field) ->
    _pipe = gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:at(
            [Field],
            gleam@dynamic@decode:dict(
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
            )
        )
    ),
    gleam@result:map_error(_pipe, fun(_) -> nil end).

-file("src/mochi/batch.gleam", 417).
-spec extract_variables(gleam@dynamic:dynamic_()) -> gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())).
extract_variables(Value) ->
    case extract_dict_field(Value, <<"variables"/utf8>>) of
        {ok, Vars} ->
            {some, Vars};

        {error, _} ->
            none
    end.

-file("src/mochi/batch.gleam", 356).
?DOC(" Parse a batch request from a dynamic value (e.g., from JSON)\n").
-spec parse_batch_request(gleam@dynamic:dynamic_()) -> {ok, batch_request()} |
    {error, binary()}.
parse_batch_request(Value) ->
    case extract_query(Value) of
        {ok, Query} ->
            Variables = extract_variables(Value),
            Operation_name = extract_operation_name(Value),
            {ok, {batch_request, Query, Variables, Operation_name}};

        {error, E} ->
            {error, E}
    end.

-file("src/mochi/batch.gleam", 372).
?DOC(" Parse multiple batch requests from a dynamic list\n").
-spec parse_batch_requests(gleam@dynamic:dynamic_()) -> {ok,
        list(batch_request())} |
    {error, binary()}.
parse_batch_requests(Value) ->
    case get_list_raw(Value) of
        {ok, Items} ->
            Results = gleam@list:map(Items, fun parse_batch_request/1),
            Errors = begin
                _pipe = Results,
                gleam@list:filter_map(_pipe, fun(R) -> case R of
                            {error, E} ->
                                {ok, E};

                            {ok, _} ->
                                {error, nil}
                        end end)
            end,
            case Errors of
                [] ->
                    {ok, gleam@list:filter_map(Results, fun(R@1) -> case R@1 of
                                    {ok, Req} ->
                                        {ok, Req};

                                    {error, _} ->
                                        {error, nil}
                                end end)};

                [First | _] ->
                    {error,
                        <<"Failed to parse batch request: "/utf8, First/binary>>}
            end;

        {error, _} ->
            case parse_batch_request(Value) of
                {ok, Req@1} ->
                    {ok, [Req@1]};

                {error, E@1} ->
                    {error, E@1}
            end
    end.
