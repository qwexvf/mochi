-module(mochi_wisp_ffi).
-export([parse_graphql_request_full/1, dynamic_to_json/1, get_field_safe/2, generate_request_id/0]).

%% Generate a unique request ID for log correlation
generate_request_id() ->
    %% Use a combination of timestamp and random bytes for uniqueness
    <<I:64/unsigned-integer>> = crypto:strong_rand_bytes(8),
    Id = integer_to_binary(I rem 16#FFFFFFFF, 16),
    <<"req_", Id/binary>>.

%% Parse GraphQL request from JSON body
%% Returns: {ok, GraphQLRequest} | {error, String}
parse_graphql_request_full(Body) ->
    try
        Json = json:decode(Body),
        Query = maps:get(<<"query">>, Json, undefined),
        case Query of
            undefined ->
                {error, <<"Missing required field: 'query'">>};
            Q when is_binary(Q) ->
                Variables = case maps:get(<<"variables">>, Json, null) of
                    null -> none;
                    undefined -> none;
                    V when is_map(V) ->
                        %% Convert map keys to ensure they're binaries
                        {some, convert_variables(V)};
                    _ -> none
                end,
                OpName = case maps:get(<<"operationName">>, Json, null) of
                    null -> none;
                    undefined -> none;
                    N when is_binary(N) -> {some, N};
                    _ -> none
                end,
                {ok, {graph_ql_request, Q, Variables, OpName}};
            _ ->
                {error, <<"'query' field must be a string">>}
        end
    catch
        _:_ -> {error, <<"Invalid JSON in request body">>}
    end.

%% Convert variables map to ensure proper format
convert_variables(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = if
            is_binary(K) -> K;
            is_atom(K) -> atom_to_binary(K, utf8);
            is_list(K) -> list_to_binary(K);
            true -> iolist_to_binary(io_lib:format("~p", [K]))
        end,
        maps:put(Key, V, Acc)
    end, #{}, Map).

%% Convert Dynamic value to gleam_json Json type
%% In OTP 27+, gleam_json uses the native json module and Json values are iodata
dynamic_to_json(null) -> gleam_json_ffi:null();
dynamic_to_json(nil) -> gleam_json_ffi:null();
dynamic_to_json(undefined) -> gleam_json_ffi:null();
dynamic_to_json(true) -> gleam_json_ffi:bool(true);
dynamic_to_json(false) -> gleam_json_ffi:bool(false);
dynamic_to_json(Value) when is_binary(Value) -> gleam_json_ffi:string(Value);
dynamic_to_json(Value) when is_integer(Value) -> gleam_json_ffi:int(Value);
dynamic_to_json(Value) when is_float(Value) -> gleam_json_ffi:float(Value);
dynamic_to_json(Value) when is_atom(Value) ->
    gleam_json_ffi:string(atom_to_binary(Value, utf8));
dynamic_to_json(Value) when is_list(Value) ->
    gleam_json_ffi:array([dynamic_to_json(V) || V <- Value]);
dynamic_to_json(Value) when is_map(Value) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        Key = if
            is_binary(K) -> K;
            is_atom(K) -> atom_to_binary(K, utf8);
            is_list(K) -> list_to_binary(K);
            true -> iolist_to_binary(io_lib:format("~p", [K]))
        end,
        [{Key, dynamic_to_json(V)} | Acc]
    end, [], Value),
    gleam_json_ffi:object(Pairs);
dynamic_to_json(_) -> gleam_json_ffi:null().

%% Get a field from a map safely, returning Option(Dynamic)
%% Returns: {some, Value} | none
get_field_safe(Data, Field) when is_map(Data) ->
    case maps:find(Field, Data) of
        {ok, Value} -> {some, Value};
        error -> none
    end;
get_field_safe(_, _) -> none.
