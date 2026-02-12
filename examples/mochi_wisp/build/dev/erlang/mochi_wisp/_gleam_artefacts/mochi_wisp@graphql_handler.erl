-module(mochi_wisp@graphql_handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/graphql_handler.gleam").
-export([parse_graphql_request/1, dynamic_to_json/1, execution_result_to_json/1, handle_graphql/2, handle_graphql_quiet/2, handle_cors_preflight/0]).
-export_type([graph_q_l_request/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type graph_q_l_request() :: {graph_q_l_request,
        binary(),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())),
        gleam@option:option(binary())}.

-file("src/mochi_wisp/graphql_handler.gleam", 101).
?DOC(" Truncate a string for logging\n").
-spec truncate_string(binary(), integer()) -> binary().
truncate_string(S, Max_len) ->
    case string:length(S) > Max_len of
        true ->
            <<(gleam@string:slice(S, 0, Max_len))/binary, "..."/utf8>>;

        false ->
            S
    end.

-file("src/mochi_wisp/graphql_handler.gleam", 79).
?DOC(" Log GraphQL request details\n").
-spec log_graphql_request(binary(), graph_q_l_request()) -> nil.
log_graphql_request(Request_id, Req) ->
    Query_preview = truncate_string(erlang:element(2, Req), 100),
    logging:log(
        debug,
        <<<<<<"["/utf8, Request_id/binary>>/binary, "] Query: "/utf8>>/binary,
            Query_preview/binary>>
    ),
    case erlang:element(4, Req) of
        {some, Name} ->
            logging:log(
                debug,
                <<<<<<"["/utf8, Request_id/binary>>/binary,
                        "] Operation: "/utf8>>/binary,
                    Name/binary>>
            );

        none ->
            nil
    end,
    case erlang:element(3, Req) of
        {some, Vars} ->
            Var_count = maps:size(Vars),
            logging:log(
                debug,
                <<<<<<<<"["/utf8, Request_id/binary>>/binary,
                            "] Variables: "/utf8>>/binary,
                        (erlang:integer_to_binary(Var_count))/binary>>/binary,
                    " provided"/utf8>>
            );

        none ->
            nil
    end.

-file("src/mochi_wisp/graphql_handler.gleam", 116).
?DOC(
    " Parse a GraphQL request from JSON body\n"
    " Uses FFI because we need to preserve Dynamic values for variables\n"
).
-spec parse_graphql_request(binary()) -> {ok, graph_q_l_request()} |
    {error, binary()}.
parse_graphql_request(Body) ->
    mochi_wisp_ffi:parse_graphql_request_full(Body).

-file("src/mochi_wisp/graphql_handler.gleam", 170).
?DOC(
    " Execute a query using cached AST parsing with FAST O(n) parser\n"
    " This significantly improves performance for:\n"
    " - Repeated queries (cache hit)\n"
    " - First-time queries (O(n) binary lexer vs O(n²) string concatenation)\n"
).
-spec execute_with_cache(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> mochi@executor:execution_result().
execute_with_cache(Schema_def, Query, Variables) ->
    case mochi_wisp@query_cache:get_or_parse_fast(Query) of
        {ok, Document} ->
            Ctx = mochi@schema:execution_context(
                gleam_stdlib:identity(maps:new())
            ),
            mochi@executor:execute(Schema_def, Document, none, Ctx, Variables);

        {error, _} ->
            {execution_result,
                none,
                [{validation_error, <<"Failed to parse query"/utf8>>, []}]}
    end.

-file("src/mochi_wisp/graphql_handler.gleam", 192).
?DOC(" Log execution errors\n").
-spec log_errors(binary(), list(mochi@executor:execution_error())) -> nil.
log_errors(Request_id, Errors) ->
    case Errors of
        [] ->
            nil;

        [Error | Rest] ->
            {Msg, Path} = case Error of
                {validation_error, M, P} ->
                    {<<"ValidationError: "/utf8, M/binary>>, P};

                {resolver_error, M@1, P@1} ->
                    {<<"ResolverError: "/utf8, M@1/binary>>, P@1};

                {type_error, M@2, P@2} ->
                    {<<"TypeError: "/utf8, M@2/binary>>, P@2};

                {null_value_error, M@3, P@3} ->
                    {<<"NullValueError: "/utf8, M@3/binary>>, P@3}
            end,
            Path_str = gleam@string:join(Path, <<"."/utf8>>),
            logging:log(
                warning,
                <<<<<<<<<<"["/utf8, Request_id/binary>>/binary, "]   - "/utf8>>/binary,
                            Msg/binary>>/binary,
                        " at "/utf8>>/binary,
                    Path_str/binary>>
            ),
            log_errors(Request_id, Rest)
    end.

-file("src/mochi_wisp/graphql_handler.gleam", 214).
-spec count_list_acc(list(any()), integer()) -> integer().
count_list_acc(L, Acc) ->
    case L of
        [] ->
            Acc;

        [_ | Rest] ->
            count_list_acc(Rest, Acc + 1)
    end.

-file("src/mochi_wisp/graphql_handler.gleam", 210).
?DOC(" Count list elements without using list.length for efficiency awareness\n").
-spec count_list(list(any())) -> integer().
count_list(L) ->
    count_list_acc(L, 0).

-file("src/mochi_wisp/graphql_handler.gleam", 243).
?DOC(" Convert Dynamic value to JSON\n").
-spec dynamic_to_json(gleam@dynamic:dynamic_()) -> gleam@json:json().
dynamic_to_json(Data) ->
    mochi_wisp_ffi:dynamic_to_json(Data).

-file("src/mochi_wisp/graphql_handler.gleam", 247).
-spec encode_errors(list(mochi@executor:execution_error())) -> gleam@json:json().
encode_errors(Errors) ->
    gleam@json:array(
        Errors,
        fun(Err) ->
            {Msg, Path} = case Err of
                {validation_error, M, P} ->
                    {M, P};

                {resolver_error, M@1, P@1} ->
                    {M@1, P@1};

                {type_error, M@2, P@2} ->
                    {M@2, P@2};

                {null_value_error, M@3, P@3} ->
                    {M@3, P@3}
            end,
            case Path of
                [] ->
                    gleam@json:object(
                        [{<<"message"/utf8>>, gleam@json:string(Msg)}]
                    );

                _ ->
                    gleam@json:object(
                        [{<<"message"/utf8>>, gleam@json:string(Msg)},
                            {<<"path"/utf8>>,
                                gleam@json:array(Path, fun gleam@json:string/1)}]
                    )
            end
        end
    ).

-file("src/mochi_wisp/graphql_handler.gleam", 222).
?DOC(" Convert execution result to JSON string\n").
-spec execution_result_to_json(mochi@executor:execution_result()) -> binary().
execution_result_to_json(Result) ->
    Data_json = case erlang:element(2, Result) of
        {some, Data} ->
            dynamic_to_json(Data);

        none ->
            gleam@json:null()
    end,
    Response = case erlang:element(3, Result) of
        [] ->
            gleam@json:object([{<<"data"/utf8>>, Data_json}]);

        Errors ->
            gleam@json:object(
                [{<<"data"/utf8>>, Data_json},
                    {<<"errors"/utf8>>, encode_errors(Errors)}]
            )
    end,
    gleam@json:to_string(Response).

-file("src/mochi_wisp/graphql_handler.gleam", 291).
-spec add_cors_headers(gleam@http@response:response(wisp:body())) -> gleam@http@response:response(wisp:body()).
add_cors_headers(Response) ->
    _pipe = Response,
    _pipe@1 = fun gleam@http@response:set_header/3(
        _pipe,
        <<"Access-Control-Allow-Origin"/utf8>>,
        <<"*"/utf8>>
    ),
    _pipe@2 = fun gleam@http@response:set_header/3(
        _pipe@1,
        <<"Access-Control-Allow-Methods"/utf8>>,
        <<"POST, OPTIONS"/utf8>>
    ),
    fun gleam@http@response:set_header/3(
        _pipe@2,
        <<"Access-Control-Allow-Headers"/utf8>>,
        <<"Content-Type"/utf8>>
    ).

-file("src/mochi_wisp/graphql_handler.gleam", 124).
-spec execute_and_respond(binary(), graph_q_l_request(), mochi@schema:schema()) -> gleam@http@response:response(wisp:body()).
execute_and_respond(Request_id, Req, Schema) ->
    Variables = gleam@option:unwrap(erlang:element(3, Req), maps:new()),
    logging:log(
        debug,
        <<<<"["/utf8, Request_id/binary>>/binary, "] Executing query..."/utf8>>
    ),
    Result = execute_with_cache(Schema, erlang:element(2, Req), Variables),
    case erlang:element(3, Result) of
        [] ->
            logging:log(
                debug,
                <<<<"["/utf8, Request_id/binary>>/binary,
                    "] Query executed successfully"/utf8>>
            );

        Errors ->
            Error_count = count_list(Errors),
            logging:log(
                warning,
                <<<<<<<<"["/utf8, Request_id/binary>>/binary,
                            "] Query returned "/utf8>>/binary,
                        (erlang:integer_to_binary(Error_count))/binary>>/binary,
                    " error(s)"/utf8>>
            ),
            log_errors(Request_id, Errors)
    end,
    Json_body = execution_result_to_json(Result),
    Response_size = erlang:byte_size(Json_body),
    logging:log(
        debug,
        <<<<<<<<"["/utf8, Request_id/binary>>/binary, "] Response size: "/utf8>>/binary,
                (erlang:integer_to_binary(Response_size))/binary>>/binary,
            " bytes"/utf8>>
    ),
    _pipe = wisp:json_response(Json_body, 200),
    add_cors_headers(_pipe).

-file("src/mochi_wisp/graphql_handler.gleam", 150).
-spec execute_and_respond_quiet(graph_q_l_request(), mochi@schema:schema()) -> gleam@http@response:response(wisp:body()).
execute_and_respond_quiet(Req, Schema) ->
    Variables = gleam@option:unwrap(erlang:element(3, Req), maps:new()),
    Result = execute_with_cache(Schema, erlang:element(2, Req), Variables),
    Json_body = execution_result_to_json(Result),
    _pipe = wisp:json_response(Json_body, 200),
    add_cors_headers(_pipe).

-file("src/mochi_wisp/graphql_handler.gleam", 270).
-spec error_response(binary()) -> gleam@http@response:response(wisp:body()).
error_response(Message) ->
    Body = gleam@json:to_string(
        gleam@json:object(
            [{<<"errors"/utf8>>,
                    gleam@json:array(
                        [Message],
                        fun(M) ->
                            gleam@json:object(
                                [{<<"message"/utf8>>, gleam@json:string(M)}]
                            )
                        end
                    )}]
        )
    ),
    _pipe = wisp:json_response(Body, 400),
    add_cors_headers(_pipe).

-file("src/mochi_wisp/graphql_handler.gleam", 35).
?DOC(" Handle a GraphQL HTTP request with logging\n").
-spec handle_graphql(
    gleam@http@request:request(wisp@internal:connection()),
    mochi@schema:schema()
) -> gleam@http@response:response(wisp:body()).
handle_graphql(Req, Schema) ->
    Request_id = mochi_wisp_ffi:generate_request_id(),
    logging:log(
        info,
        <<<<"["/utf8, Request_id/binary>>/binary,
            "] GraphQL request received"/utf8>>
    ),
    wisp:require_string_body(
        Req,
        fun(Body) ->
            Body_size = erlang:byte_size(Body),
            logging:log(
                debug,
                <<<<<<<<"["/utf8, Request_id/binary>>/binary,
                            "] Request body size: "/utf8>>/binary,
                        (erlang:integer_to_binary(Body_size))/binary>>/binary,
                    " bytes"/utf8>>
            ),
            case mochi_wisp_ffi:parse_graphql_request_full(Body) of
                {ok, Graphql_req} ->
                    log_graphql_request(Request_id, Graphql_req),
                    Response = execute_and_respond(
                        Request_id,
                        Graphql_req,
                        Schema
                    ),
                    logging:log(
                        info,
                        <<<<<<"["/utf8, Request_id/binary>>/binary,
                                "] Response status: "/utf8>>/binary,
                            (erlang:integer_to_binary(
                                erlang:element(2, Response)
                            ))/binary>>
                    ),
                    Response;

                {error, Msg} ->
                    logging:log(
                        warning,
                        <<<<<<"["/utf8, Request_id/binary>>/binary,
                                "] Parse error: "/utf8>>/binary,
                            Msg/binary>>
                    ),
                    error_response(Msg)
            end
        end
    ).

-file("src/mochi_wisp/graphql_handler.gleam", 60).
?DOC(" Handle a GraphQL HTTP request without logging (for testing)\n").
-spec handle_graphql_quiet(
    gleam@http@request:request(wisp@internal:connection()),
    mochi@schema:schema()
) -> gleam@http@response:response(wisp:body()).
handle_graphql_quiet(Req, Schema) ->
    wisp:require_string_body(
        Req,
        fun(Body) -> case mochi_wisp_ffi:parse_graphql_request_full(Body) of
                {ok, Graphql_req} ->
                    execute_and_respond_quiet(Graphql_req, Schema);

                {error, Msg} ->
                    error_response(Msg)
            end end
    ).

-file("src/mochi_wisp/graphql_handler.gleam", 299).
?DOC(" Handle CORS preflight request\n").
-spec handle_cors_preflight() -> gleam@http@response:response(wisp:body()).
handle_cors_preflight() ->
    logging:log(debug, <<"CORS preflight request"/utf8>>),
    _pipe = wisp:response(204),
    _pipe@1 = add_cors_headers(_pipe),
    fun gleam@http@response:set_header/3(
        _pipe@1,
        <<"Access-Control-Max-Age"/utf8>>,
        <<"86400"/utf8>>
    ).
