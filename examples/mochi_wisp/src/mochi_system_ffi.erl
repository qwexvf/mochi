-module(mochi_system_ffi).
-export([otp_release/0, scheduler_count/0, memory_info/0, process_count/0]).

%% Get OTP release version
otp_release() ->
    list_to_binary(erlang:system_info(otp_release)).

%% Get number of schedulers (CPU cores being used)
scheduler_count() ->
    erlang:system_info(schedulers_online).

%% Get memory information
memory_info() ->
    [{total, Total}, {processes, Procs}, {system, Sys}, {atom, Atom},
     {binary, Bin}, {ets, Ets}] =
        [{K, V} || {K, V} <- erlang:memory(),
         lists:member(K, [total, processes, system, atom, binary, ets])],
    #{
        total => Total,
        processes => Procs,
        system => Sys,
        atom => Atom,
        binary => Bin,
        ets => Ets
    }.

%% Get current process count
process_count() ->
    erlang:system_info(process_count).
