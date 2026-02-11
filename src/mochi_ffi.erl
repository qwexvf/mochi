-module(mochi_ffi).
-export([dynamic_to_string/1, dict_has_key/2]).

dynamic_to_string(Value) when is_binary(Value) ->
    {some, Value};
dynamic_to_string(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true -> {some, unicode:characters_to_binary(Value)};
        false -> none
    end;
dynamic_to_string(_) ->
    none.

dict_has_key(Value, Key) when is_map(Value) ->
    maps:is_key(Key, Value);
dict_has_key(_, _) ->
    false.
