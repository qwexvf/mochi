-module(mochi_json_ffi).
-export([
    is_null/1,
    is_bool/1,
    is_int/1,
    is_float/1,
    is_string/1,
    is_list/1,
    is_dict/1,
    is_option/1,
    get_bool/1,
    get_int/1,
    get_float/1,
    get_string/1,
    get_list/1,
    get_dict_entries/1,
    unwrap_option/1
]).

%% Check if value is null/nil/undefined
is_null(nil) -> true;
is_null(null) -> true;
is_null(undefined) -> true;
is_null(_) -> false.

%% Check if value is a boolean
is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.

%% Check if value is an integer
is_int(Value) when is_integer(Value) -> true;
is_int(_) -> false.

%% Check if value is a float
is_float(Value) when erlang:is_float(Value) -> true;
is_float(_) -> false.

%% Check if value is a string (binary in Erlang)
is_string(Value) when is_binary(Value) -> true;
is_string(_) -> false.

%% Check if value is a list
is_list(Value) when erlang:is_list(Value) -> true;
is_list(_) -> false.

%% Check if value is a dict/map
is_dict(Value) when is_map(Value) -> true;
is_dict(_) -> false.

%% Check if value is an Option (Some/None)
%% Gleam's Option type in Erlang
is_option({some, _}) -> true;
is_option(none) -> true;
is_option(_) -> false.

%% Get boolean value
get_bool(Value) -> Value.

%% Get integer value
get_int(Value) -> Value.

%% Get float value
get_float(Value) -> Value.

%% Get string value
get_string(Value) -> Value.

%% Get list elements (already a list in Erlang)
get_list(Value) -> Value.

%% Get dict entries as list of tuples
get_dict_entries(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Map).

%% Unwrap Option - returns inner value or nil
unwrap_option({some, Value}) -> Value;
unwrap_option(none) -> nil;
unwrap_option(_) -> nil.
