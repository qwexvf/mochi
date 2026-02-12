-module(mochi_websocket_ffi).
-export([parse_json/1, get_field/2, get_string/1, get_dict/1]).

%% Parse JSON string to Erlang term
parse_json(JsonBin) when is_binary(JsonBin) ->
    try
        case json:decode(JsonBin) of
            Map when is_map(Map) -> {ok, Map};
            Other -> {ok, Other}
        end
    catch
        _:_ -> {error, <<"Invalid JSON">>}
    end.

%% Get a field from a map
get_field(Map, Field) when is_map(Map), is_binary(Field) ->
    case maps:find(Field, Map) of
        {ok, Value} -> {ok, Value};
        error -> {error, nil}
    end;
get_field(_, _) ->
    {error, nil}.

%% Get string value
get_string(Bin) when is_binary(Bin) ->
    {ok, Bin};
get_string(_) ->
    {error, nil}.

%% Get dict/map value
get_dict(Map) when is_map(Map) ->
    {ok, Map};
get_dict(_) ->
    {error, nil}.
