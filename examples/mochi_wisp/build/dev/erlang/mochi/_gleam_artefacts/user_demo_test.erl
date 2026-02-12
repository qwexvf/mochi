-module(user_demo_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/user_demo_test.gleam").
-export([main/0]).

-file("src/user_demo_test.gleam", 3).
-spec main() -> nil.
main() ->
    mochi@user_demo:demo_user_schema().
