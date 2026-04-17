%% mochi_parallel_ffi.erl
%% Parallel map implementation using Erlang process spawning.
%% Used by mochi/batch.gleam to execute batch requests concurrently.
-module(mochi_parallel_ffi).
-export([parallel_map/1]).

%% Execute a list of zero-argument functions in parallel and collect results
%% in the original order. Each function is spawned as a separate Erlang process.
parallel_map(Fns) ->
    Parent = self(),
    Refs = lists:map(fun(Fn) ->
        Ref = make_ref(),
        spawn(fun() ->
            Result = Fn(),
            Parent ! {Ref, Result}
        end),
        Ref
    end, Fns),
    lists:map(fun(Ref) ->
        receive
            {Ref, Result} -> Result
        after 30000 ->
            {error, <<"parallel_map timeout">>}
        end
    end, Refs).
