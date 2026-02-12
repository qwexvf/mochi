-module(realworld_benchmark_runner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/realworld_benchmark_runner.gleam").
-export([main/0]).

-file("src/realworld_benchmark_runner.gleam", 6).
-spec main() -> nil.
main() ->
    mochi_wisp@realworld_benchmark:run_all().
