-module(mochi_ffi).
-export([dynamic_to_string/1, dynamic_to_bool/1, dict_has_key/2, try_extract_string/1, try_extract_int/1]).

dynamic_to_string(Value) when is_binary(Value) ->
    {some, Value};
dynamic_to_string(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true -> {some, unicode:characters_to_binary(Value)};
        false -> none
    end;
dynamic_to_string(_) ->
    none.

dynamic_to_bool(true) ->
    {some, true};
dynamic_to_bool(false) ->
    {some, false};
dynamic_to_bool(_) ->
    none.

dict_has_key(Value, Key) when is_map(Value) ->
    maps:is_key(Key, Value);
dict_has_key(_, _) ->
    false.

try_extract_string(Value) when is_binary(Value) ->
    {ok, Value};
try_extract_string(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true -> {ok, unicode:characters_to_binary(Value)};
        false -> {error, <<"Expected string">>}
    end;
try_extract_string(_) ->
    {error, <<"Expected string">>}.

try_extract_int(Value) when is_integer(Value) ->
    {ok, Value};
try_extract_int(_) ->
    {error, <<"Expected int">>}.
