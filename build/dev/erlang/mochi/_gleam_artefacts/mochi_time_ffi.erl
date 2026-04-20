-module(mochi_time_ffi).
-export([monotonic_time_ns/0]).

monotonic_time_ns() ->
    erlang:monotonic_time(nanosecond).
