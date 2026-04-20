-module(mochi@response).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/response.gleam").
-export([success/1, failure/1, partial/2, with_extensions/2, with_extension/3, with_tracing/3, to_dynamic/1, serialize/1, to_json/1, to_json_pretty/1, execution_error_to_graphql_error/1, from_execution_result/1, has_errors/1, has_data/1, error_count/1, is_success/1, is_partial/1, format/1]).
-export_type([graph_q_l_response/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type graph_q_l_response() :: {graph_q_l_response,
        gleam@option:option(gleam@dynamic:dynamic_()),
        gleam@option:option(list(mochi@error:graph_q_l_error())),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-file("src/mochi/response.gleam", 42).
?DOC(" Create a successful response with data\n").
-spec success(gleam@dynamic:dynamic_()) -> graph_q_l_response().
success(Data) ->
    {graph_q_l_response, {some, Data}, none, none}.

-file("src/mochi/response.gleam", 47).
?DOC(" Create an error-only response\n").
-spec failure(list(mochi@error:graph_q_l_error())) -> graph_q_l_response().
failure(Errors) ->
    {graph_q_l_response, none, {some, Errors}, none}.

-file("src/mochi/response.gleam", 52).
?DOC(" Create a partial response with data and errors\n").
-spec partial(gleam@dynamic:dynamic_(), list(mochi@error:graph_q_l_error())) -> graph_q_l_response().
partial(Data, Errors) ->
    {graph_q_l_response, {some, Data}, {some, Errors}, none}.

-file("src/mochi/response.gleam", 67).
?DOC(" Add extensions to a response\n").
-spec with_extensions(
    graph_q_l_response(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> graph_q_l_response().
with_extensions(Response, Extensions) ->
    {graph_q_l_response,
        erlang:element(2, Response),
        erlang:element(3, Response),
        {some, Extensions}}.

-file("src/mochi/response.gleam", 75).
?DOC(" Add a single extension value\n").
-spec with_extension(graph_q_l_response(), binary(), gleam@dynamic:dynamic_()) -> graph_q_l_response().
with_extension(Response, Key, Value) ->
    Extensions = case erlang:element(4, Response) of
        {some, Ext} ->
            gleam@dict:insert(Ext, Key, Value);

        none ->
            maps:from_list([{Key, Value}])
    end,
    {graph_q_l_response,
        erlang:element(2, Response),
        erlang:element(3, Response),
        {some, Extensions}}.

-file("src/mochi/response.gleam", 88).
?DOC(" Add tracing data to response extensions\n").
-spec with_tracing(graph_q_l_response(), integer(), integer()) -> graph_q_l_response().
with_tracing(Response, Start_time, End_time) ->
    Tracing = gleam_stdlib:identity(
        maps:from_list(
            [{<<"version"/utf8>>, gleam_stdlib:identity(1)},
                {<<"startTime"/utf8>>, gleam_stdlib:identity(Start_time)},
                {<<"endTime"/utf8>>, gleam_stdlib:identity(End_time)},
                {<<"duration"/utf8>>,
                    gleam_stdlib:identity(End_time - Start_time)}]
        )
    ),
    with_extension(Response, <<"tracing"/utf8>>, Tracing).

-file("src/mochi/response.gleam", 110).
?DOC(" Convert a GraphQLResponse to a Dynamic representation for JSON serialization\n").
-spec to_dynamic(graph_q_l_response()) -> gleam@dynamic:dynamic_().
to_dynamic(Response) ->
    Parts = [],
    Parts@1 = [{<<"data"/utf8>>, case erlang:element(2, Response) of
                {some, D} ->
                    D;

                none ->
                    gleam_stdlib:identity(nil)
            end} | Parts],
    Parts@2 = case erlang:element(3, Response) of
        {some, Errors} ->
            [{<<"errors"/utf8>>, mochi@error:errors_to_dynamic(Errors)} |
                Parts@1];

        none ->
            Parts@1
    end,
    Parts@3 = case erlang:element(4, Response) of
        {some, Ext} ->
            [{<<"extensions"/utf8>>, gleam_stdlib:identity(Ext)} | Parts@2];

        none ->
            Parts@2
    end,
    gleam_stdlib:identity(maps:from_list(Parts@3)).

-file("src/mochi/response.gleam", 139).
?DOC(
    " Serialize response to JSON string (requires external JSON encoder)\n"
    " This returns the Dynamic representation - use with a JSON library\n"
).
-spec serialize(graph_q_l_response()) -> gleam@dynamic:dynamic_().
serialize(Response) ->
    to_dynamic(Response).

-file("src/mochi/response.gleam", 144).
?DOC(" Convert a GraphQLResponse to a JSON string\n").
-spec to_json(graph_q_l_response()) -> binary().
to_json(Response) ->
    _pipe = Response,
    _pipe@1 = to_dynamic(_pipe),
    mochi@json:encode(_pipe@1).

-file("src/mochi/response.gleam", 151).
?DOC(" Convert a GraphQLResponse to a pretty-printed JSON string\n").
-spec to_json_pretty(graph_q_l_response()) -> binary().
to_json_pretty(Response) ->
    _pipe = Response,
    _pipe@1 = to_dynamic(_pipe),
    mochi@json:encode_pretty(_pipe@1, 2).

-file("src/mochi/response.gleam", 163).
?DOC(
    " Convert an ExecutionError to a GraphQLError\n"
    " Add source location to a GraphQLError if one is present\n"
).
-spec maybe_with_location(
    mochi@error:graph_q_l_error(),
    gleam@option:option({integer(), integer()})
) -> mochi@error:graph_q_l_error().
maybe_with_location(Err, Location) ->
    case Location of
        none ->
            Err;

        {some, {Line, Col}} ->
            mochi@error:with_locations(Err, [{location, Line, Col}])
    end.

-file("src/mochi/response.gleam", 174).
-spec execution_error_to_graphql_error(mochi@executor:execution_error()) -> mochi@error:graph_q_l_error().
execution_error_to_graphql_error(Err) ->
    case Err of
        {validation_error, Message, Path, Location} ->
            _pipe = mochi@error:error_at(Message, Path),
            _pipe@1 = mochi@error:with_category(
                _pipe,
                validation_error_category
            ),
            maybe_with_location(_pipe@1, Location);

        {resolver_error, Message@1, Path@1, Location@1} ->
            _pipe@2 = mochi@error:error_at(Message@1, Path@1),
            _pipe@3 = mochi@error:with_category(
                _pipe@2,
                resolver_error_category
            ),
            maybe_with_location(_pipe@3, Location@1);

        {type_error, Message@2, Path@2, Location@2} ->
            _pipe@4 = mochi@error:error_at(Message@2, Path@2),
            _pipe@5 = mochi@error:with_category(_pipe@4, type_error_category),
            maybe_with_location(_pipe@5, Location@2);

        {null_value_error, Message@3, Path@3, Location@3} ->
            _pipe@6 = mochi@error:error_at(Message@3, Path@3),
            _pipe@7 = mochi@error:with_code(
                _pipe@6,
                <<"NULL_VALUE_ERROR"/utf8>>
            ),
            _pipe@8 = mochi@error:with_category(
                _pipe@7,
                resolver_error_category
            ),
            maybe_with_location(_pipe@8, Location@3)
    end.

-file("src/mochi/response.gleam", 57).
?DOC(" Create a response from an ExecutionResult\n").
-spec from_execution_result(mochi@executor:execution_result()) -> graph_q_l_response().
from_execution_result(Result) ->
    Errors = case erlang:element(3, Result) of
        [] ->
            none;

        Errs ->
            {some, gleam@list:map(Errs, fun execution_error_to_graphql_error/1)}
    end,
    {graph_q_l_response, erlang:element(2, Result), Errors, none}.

-file("src/mochi/response.gleam", 201).
?DOC(" Check if response has errors\n").
-spec has_errors(graph_q_l_response()) -> boolean().
has_errors(Response) ->
    case erlang:element(3, Response) of
        {some, Errors} ->
            Errors /= [];

        none ->
            false
    end.

-file("src/mochi/response.gleam", 209).
?DOC(" Check if response has data\n").
-spec has_data(graph_q_l_response()) -> boolean().
has_data(Response) ->
    case erlang:element(2, Response) of
        {some, _} ->
            true;

        none ->
            false
    end.

-file("src/mochi/response.gleam", 217).
?DOC(" Get error count\n").
-spec error_count(graph_q_l_response()) -> integer().
error_count(Response) ->
    case erlang:element(3, Response) of
        {some, Errors} ->
            erlang:length(Errors);

        none ->
            0
    end.

-file("src/mochi/response.gleam", 225).
?DOC(" Check if response is successful (has data, no errors)\n").
-spec is_success(graph_q_l_response()) -> boolean().
is_success(Response) ->
    has_data(Response) andalso not has_errors(Response).

-file("src/mochi/response.gleam", 230).
?DOC(" Check if response is partial (has data and errors)\n").
-spec is_partial(graph_q_l_response()) -> boolean().
is_partial(Response) ->
    has_data(Response) andalso has_errors(Response).

-file("src/mochi/response.gleam", 239).
?DOC(" Format response for debugging/logging\n").
-spec format(graph_q_l_response()) -> binary().
format(Response) ->
    Data_str = case erlang:element(2, Response) of
        {some, _} ->
            <<"data: <present>"/utf8>>;

        none ->
            <<"data: null"/utf8>>
    end,
    Errors_str = case erlang:element(3, Response) of
        {some, Errors} ->
            Count = erlang:length(Errors),
            case Count of
                1 ->
                    <<"errors: [1 error]"/utf8>>;

                N ->
                    <<<<"errors: ["/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
                        " errors]"/utf8>>
            end;

        none ->
            <<"errors: none"/utf8>>
    end,
    Ext_str = case erlang:element(4, Response) of
        {some, _} ->
            <<"extensions: <present>"/utf8>>;

        none ->
            <<"extensions: none"/utf8>>
    end,
    <<<<<<<<<<<<"GraphQLResponse { "/utf8, Data_str/binary>>/binary, ", "/utf8>>/binary,
                    Errors_str/binary>>/binary,
                ", "/utf8>>/binary,
            Ext_str/binary>>/binary,
        " }"/utf8>>.
