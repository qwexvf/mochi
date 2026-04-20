-module(custom_directive_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/custom_directive_test.gleam").
-export([directive_definition_test/0, directive_with_arguments_test/0, repeatable_directive_test/0, directive_handler_test/0, add_directive_to_schema_test/0, multiple_directives_test/0, skip_directive_test/0, include_directive_test/0, deprecated_directive_test/0, builtin_directives_test/0, directive_location_to_string_test/0]).

-file("test/custom_directive_test.gleam", 12).
-spec directive_definition_test() -> nil.
directive_definition_test() ->
    Auth_directive = begin
        _pipe = mochi@schema:directive(
            <<"auth"/utf8>>,
            [field_location, object_location]
        ),
        mochi@schema:directive_description(
            _pipe,
            <<"Requires authentication"/utf8>>
        )
    end,
    case erlang:element(2, Auth_directive) =:= <<"auth"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Directive name should be 'auth'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_definition_test"/utf8>>,
                    line => 19})
    end,
    case erlang:element(3, Auth_directive) of
        {some, <<"Requires authentication"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Directive description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_definition_test"/utf8>>,
                    line => 24})
    end.

-file("test/custom_directive_test.gleam", 28).
-spec directive_with_arguments_test() -> nil.
directive_with_arguments_test() ->
    Role_directive = begin
        _pipe = mochi@schema:directive(<<"hasRole"/utf8>>, [field_location]),
        mochi@schema:directive_argument(
            _pipe,
            begin
                _pipe@1 = mochi@schema:arg(
                    <<"role"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:arg_description(_pipe@1, <<"Required role"/utf8>>)
            end
        )
    end,
    case maps:size(erlang:element(4, Role_directive)) =:= 1 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 1 argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_with_arguments_test"/utf8>>,
                    line => 38})
    end,
    case gleam@dict:has_key(erlang:element(4, Role_directive), <<"role"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 'role' argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_with_arguments_test"/utf8>>,
                    line => 43})
    end.

-file("test/custom_directive_test.gleam", 47).
-spec repeatable_directive_test() -> nil.
repeatable_directive_test() ->
    Log_directive = begin
        _pipe = mochi@schema:directive(<<"log"/utf8>>, [field_location]),
        mochi@schema:directive_repeatable(_pipe)
    end,
    case erlang:element(6, Log_directive) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Directive should be repeatable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"repeatable_directive_test"/utf8>>,
                    line => 54})
    end.

-file("test/custom_directive_test.gleam", 58).
-spec directive_handler_test() -> nil.
directive_handler_test() ->
    Uppercase_directive = begin
        _pipe = mochi@schema:directive(<<"uppercase"/utf8>>, [field_location]),
        mochi@schema:directive_handler(_pipe, fun(_, Value) -> {ok, Value} end)
    end,
    case gleam@option:is_some(erlang:element(7, Uppercase_directive)) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Directive should have handler"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_handler_test"/utf8>>,
                    line => 68})
    end.

-file("test/custom_directive_test.gleam", 76).
-spec add_directive_to_schema_test() -> nil.
add_directive_to_schema_test() ->
    Auth_directive = mochi@schema:directive(<<"auth"/utf8>>, [field_location]),
    Test_schema = begin
        _pipe = mochi@schema:schema(),
        mochi@schema:add_directive(_pipe, Auth_directive)
    end,
    case gleam@dict:has_key(erlang:element(6, Test_schema), <<"auth"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have 'auth' directive"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"add_directive_to_schema_test"/utf8>>,
                    line => 85})
    end.

-file("test/custom_directive_test.gleam", 89).
-spec multiple_directives_test() -> nil.
multiple_directives_test() ->
    Auth_directive = mochi@schema:directive(<<"auth"/utf8>>, [field_location]),
    Log_directive = mochi@schema:directive(<<"log"/utf8>>, [field_location]),
    Cache_directive = mochi@schema:directive(<<"cache"/utf8>>, [field_location]),
    Test_schema = begin
        _pipe = mochi@schema:schema(),
        _pipe@1 = mochi@schema:add_directive(_pipe, Auth_directive),
        _pipe@2 = mochi@schema:add_directive(_pipe@1, Log_directive),
        mochi@schema:add_directive(_pipe@2, Cache_directive)
    end,
    case maps:size(erlang:element(6, Test_schema)) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have 3 directives"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"multiple_directives_test"/utf8>>,
                    line => 102})
    end.

-file("test/custom_directive_test.gleam", 110).
-spec skip_directive_test() -> nil.
skip_directive_test() ->
    Skip = mochi@schema:skip_directive(),
    case erlang:element(2, Skip) =:= <<"skip"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be named 'skip'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"skip_directive_test"/utf8>>,
                    line => 115})
    end,
    case gleam@dict:has_key(erlang:element(4, Skip), <<"if"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 'if' argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"skip_directive_test"/utf8>>,
                    line => 120})
    end.

-file("test/custom_directive_test.gleam", 124).
-spec include_directive_test() -> nil.
include_directive_test() ->
    Include = mochi@schema:include_directive(),
    case erlang:element(2, Include) =:= <<"include"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be named 'include'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"include_directive_test"/utf8>>,
                    line => 129})
    end,
    case gleam@dict:has_key(erlang:element(4, Include), <<"if"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 'if' argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"include_directive_test"/utf8>>,
                    line => 134})
    end.

-file("test/custom_directive_test.gleam", 138).
-spec deprecated_directive_test() -> nil.
deprecated_directive_test() ->
    Deprecated = mochi@schema:deprecated_directive(),
    case erlang:element(2, Deprecated) =:= <<"deprecated"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be named 'deprecated'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"deprecated_directive_test"/utf8>>,
                    line => 143})
    end,
    case gleam@dict:has_key(erlang:element(4, Deprecated), <<"reason"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 'reason' argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"deprecated_directive_test"/utf8>>,
                    line => 148})
    end.

-file("test/custom_directive_test.gleam", 152).
-spec builtin_directives_test() -> nil.
builtin_directives_test() ->
    Builtins = mochi@schema:builtin_directives(),
    case Builtins of
        [_, _, _] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 3 built-in directives"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"builtin_directives_test"/utf8>>,
                    line => 157})
    end.

-file("test/custom_directive_test.gleam", 167).
-spec directive_location_to_string_test() -> nil.
directive_location_to_string_test() ->
    case mochi@schema:directive_location_to_string(field_location) of
        <<"FIELD"/utf8>> ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"FieldLocation should be 'FIELD'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_location_to_string_test"/utf8>>,
                    line => 170})
    end,
    case mochi@schema:directive_location_to_string(query_location) of
        <<"QUERY"/utf8>> ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"QueryLocation should be 'QUERY'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_location_to_string_test"/utf8>>,
                    line => 175})
    end,
    case mochi@schema:directive_location_to_string(object_location) of
        <<"OBJECT"/utf8>> ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"ObjectLocation should be 'OBJECT'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_location_to_string_test"/utf8>>,
                    line => 180})
    end,
    case mochi@schema:directive_location_to_string(enum_value_location) of
        <<"ENUM_VALUE"/utf8>> ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"EnumValueLocation should be 'ENUM_VALUE'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"custom_directive_test"/utf8>>,
                    function => <<"directive_location_to_string_test"/utf8>>,
                    line => 185})
    end.
