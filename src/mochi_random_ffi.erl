-module(mochi_random_ffi).
-export([unique_positive_int/0]).

unique_positive_int() ->
    erlang:unique_integer([positive]).
