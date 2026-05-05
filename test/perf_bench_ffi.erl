-module(perf_bench_ffi).
-export([concurrent_throughput/3]).

%% Spawns N worker processes, each looping over Body until Millis elapses,
%% returns the total number of body invocations across all workers.
concurrent_throughput(Workers, Millis, Body) ->
    Parent = self(),
    Deadline = erlang:monotonic_time(millisecond) + Millis,
    Pids = [spawn_link(fun() -> worker(Parent, Deadline, Body, 0) end)
            || _ <- lists:seq(1, Workers)],
    collect(Pids, 0).

worker(Parent, Deadline, Body, Count) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            Parent ! {done, self(), Count};
        false ->
            Body(),
            worker(Parent, Deadline, Body, Count + 1)
    end.

collect([], Acc) -> Acc;
collect([Pid | Rest], Acc) ->
    receive
        {done, Pid, N} -> collect(Rest, Acc + N)
    end.
