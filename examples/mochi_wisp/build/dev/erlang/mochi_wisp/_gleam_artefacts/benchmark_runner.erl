-module(benchmark_runner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/benchmark_runner.gleam").
-export([main/0]).

-file("src/benchmark_runner.gleam", 6).
-spec main() -> nil.
main() ->
    mochi_wisp@benchmark:run_all().
