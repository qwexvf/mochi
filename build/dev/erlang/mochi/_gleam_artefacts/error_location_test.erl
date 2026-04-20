-module(error_location_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/error_location_test.gleam").
-export([unknown_field_error_has_location_test/0, multiple_unknown_fields_all_have_locations_test/0, parse_error_has_location_test/0]).
-export_type([u/0]).

-type u() :: {u, binary()}.

-file("test/error_location_test.gleam", 12).
-spec build_schema() -> mochi@schema:schema().
build_schema() ->
    Ut = begin
        _pipe = mochi@types:object(<<"U"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {u, <<"1"/utf8>>}} end)
    end,
    Q = mochi@query:'query'(
        <<"u"/utf8>>,
        mochi@schema:named_type(<<"U"/utf8>>),
        fun(_) -> {ok, {u, <<"1"/utf8>>}} end,
        fun(V) -> gleam_stdlib:identity(V) end
    ),
    _pipe@2 = mochi@query:new(),
    _pipe@3 = mochi@query:add_query(_pipe@2, Q),
    _pipe@4 = mochi@query:add_type(_pipe@3, Ut),
    mochi@query:build(_pipe@4).

-file("test/error_location_test.gleam", 27).
-spec unknown_field_error_has_location_test() -> nil.
unknown_field_error_has_location_test() ->
    S = build_schema(),
    R = mochi@executor:execute_query(S, <<"{ u { id unknownField } }"/utf8>>),
    Has_location = gleam@list:any(erlang:element(3, R), fun(E) -> case E of
                {validation_error, _, _, {some, _}} ->
                    true;

                _ ->
                    false
            end end),
    case Has_location of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error to have location"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_location_test"/utf8>>,
                    function => <<"unknown_field_error_has_location_test"/utf8>>,
                    line => 39})
    end.

-file("test/error_location_test.gleam", 43).
-spec multiple_unknown_fields_all_have_locations_test() -> nil.
multiple_unknown_fields_all_have_locations_test() ->
    S = build_schema(),
    R = mochi@executor:execute_query(
        S,
        <<"{ u { fieldA fieldB fieldC } }"/utf8>>
    ),
    case erlang:length(erlang:element(3, R)) >= 3 of
        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected 3 validation errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_location_test"/utf8>>,
                    function => <<"multiple_unknown_fields_all_have_locations_test"/utf8>>,
                    line => 47});

        true ->
            All_located = gleam@list:all(
                erlang:element(3, R),
                fun(E) -> case E of
                        {validation_error, _, _, {some, _}} ->
                            true;

                        _ ->
                            false
                    end end
            ),
            case All_located of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected all errors to have locations"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_location_test"/utf8>>,
                            function => <<"multiple_unknown_fields_all_have_locations_test"/utf8>>,
                            line => 58})
            end
    end.

-file("test/error_location_test.gleam", 64).
-spec parse_error_has_location_test() -> nil.
parse_error_has_location_test() ->
    S = build_schema(),
    R = mochi@executor:execute_query(S, <<"{ 123 }"/utf8>>),
    case erlang:element(3, R) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected parse error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_location_test"/utf8>>,
                    function => <<"parse_error_has_location_test"/utf8>>,
                    line => 68});

        [{validation_error, _, _, Loc} | _] ->
            case Loc of
                {some, _} ->
                    nil;

                none ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Expected parse error to carry source location"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_location_test"/utf8>>,
                            function => <<"parse_error_has_location_test"/utf8>>,
                            line => 72})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected ValidationError"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_location_test"/utf8>>,
                    function => <<"parse_error_has_location_test"/utf8>>,
                    line => 75})
    end.
