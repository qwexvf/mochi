-module(mochi_wisp_benchmark_ffi).
-export([measure_time/1, monotonic_time_us/0]).

%% Measure execution time in microseconds
%% Returns: {TimeInMicroseconds, Result}
measure_time(Fun) ->
    {Time, Result} = timer:tc(Fun),
    {Time, Result}.

%% Get current monotonic time in microseconds
monotonic_time_us() ->
    erlang:monotonic_time(microsecond).
