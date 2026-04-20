-module(global_value).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/global_value.gleam").
-export([create_with_unique_name/2]).
-export_type([node_/0, pid_/0, do_not_leak/0, header/0, get_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type node_() :: any().

-type pid_() :: any().

-type do_not_leak() :: any().

-type header() :: gleam_global_value.

-type get_error() :: invalid_stored_format | does_not_exist.

-file("src/global_value.gleam", 53).
?DOC(
    " Create a global value using a function and a string name. **The string name\n"
    " must be globally unique**, e.g. the name of the module + the name of the\n"
    " function. If you pick a name that is not unique then two global values can\n"
    " collide at runtime, which can violate the type system and cause crashes or\n"
    " worse.\n"
    "\n"
    " This is most likely useful in your test suite for shared things such as\n"
    " a database pool, assuming your test runner doesn't have a way to specify\n"
    " shared state.\n"
    "\n"
    " The value is held in persistent term storage on Erlang and in a singleton\n"
    " object on JavaScript, so accessing the value from any process is efficient,\n"
    " but you should have as few global values as possible. Ideally only have one\n"
    " per project and put all the global state in a single value.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " // test/my_app_test.gleam\n"
    " import database\n"
    " import global_value\n"
    " \n"
    " // Define the global data\n"
    " \n"
    " pub type TestGlobalData {\n"
    "   TestGlobalData(db: database.ConnectionPool)\n"
    " }\n"
    " \n"
    " // A function to get-or-create the global data.\n"
    " // In this example the first time the global data is accessed the database\n"
    " // connection is created, and then the following times the already-created\n"
    " // database connection is used.\n"
    " pub fn global_data() -> TestGlobalData {\n"
    "   global_value.create_with_unique_name(\"my_app_test.global.data\", fn() {\n"
    "     TestGlobalData(db: database.create_connection_pool())\n"
    "   })\n"
    " }\n"
    " \n"
    " // Use it in tests\n"
    " \n"
    " pub fn first_test() {\n"
    "   let globals = global_data()\n"
    "   // ... use the database connection here\n"
    " }\n"
    " \n"
    " pub fn second_test() {\n"
    "   let globals = global_data()\n"
    "   // ... use the database connection here\n"
    " }\n"
    " ```\n"
).
-spec create_with_unique_name(binary(), fun(() -> DUM)) -> DUM.
create_with_unique_name(Name, Initialiser) ->
    Name_hash = erlang:phash2(Name),
    Get_previously_stored_value = fun() ->
        case global_value_ffi:persistent_term_get(Name_hash) of
            {ok, Value} ->
                {ok, Value};

            {error, invalid_stored_format} ->
                erlang:error(#{gleam_error => panic,
                        message => <<"global_value error: the name was already taken by something else in persistent_term storage"/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"global_value"/utf8>>,
                        function => <<"create_with_unique_name"/utf8>>,
                        line => 62});

            {error, does_not_exist} ->
                {error, nil}
        end
    end,
    case Get_previously_stored_value() of
        {ok, Value@1} ->
            Value@1;

        {error, _} ->
            Me = erlang:self(),
            global:trans(
                {Name_hash, Me},
                fun() -> case Get_previously_stored_value() of
                        {ok, Value@2} ->
                            Value@2;

                        {error, _} ->
                            Value@3 = Initialiser(),
                            _ = persistent_term:put(
                                Name_hash,
                                {gleam_global_value, Value@3}
                            ),
                            Value@3
                    end end,
                [erlang:node()]
            )
    end.
