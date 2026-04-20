-module(types_helpers_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/types_helpers_test.gleam").
-export([list_float_field_test/0, list_bool_field_test/0, list_id_field_test/0, non_null_list_string_field_test/0, non_null_list_int_field_test/0, non_null_list_float_field_test/0]).
-export_type([item/0]).

-type item() :: {item,
        list(binary()),
        list(float()),
        list(binary()),
        list(boolean())}.

-file("test/types_helpers_test.gleam", 13).
-spec list_float_field_test() -> nil.
list_float_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:list_float(
            _pipe,
            <<"scores"/utf8>>,
            fun(I) -> erlang:element(3, I) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"scores"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have scores field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"list_float_field_test"/utf8>>,
                    line => 21})
    end.

-file("test/types_helpers_test.gleam", 25).
-spec list_bool_field_test() -> nil.
list_bool_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:list_bool(
            _pipe,
            <<"flags"/utf8>>,
            fun(I) -> erlang:element(5, I) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"flags"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have flags field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"list_bool_field_test"/utf8>>,
                    line => 33})
    end.

-file("test/types_helpers_test.gleam", 37).
-spec list_id_field_test() -> nil.
list_id_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:list_id(
            _pipe,
            <<"ids"/utf8>>,
            fun(I) -> erlang:element(4, I) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"ids"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have ids field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"list_id_field_test"/utf8>>,
                    line => 45})
    end.

-file("test/types_helpers_test.gleam", 49).
-spec non_null_list_string_field_test() -> nil.
non_null_list_string_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:non_null_list_string(
            _pipe,
            <<"tags"/utf8>>,
            fun(I) -> erlang:element(2, I) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"tags"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have tags field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"non_null_list_string_field_test"/utf8>>,
                    line => 57})
    end.

-file("test/types_helpers_test.gleam", 61).
-spec non_null_list_int_field_test() -> nil.
non_null_list_int_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:non_null_list_int(
            _pipe,
            <<"counts"/utf8>>,
            fun(_) -> [1, 2, 3] end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"counts"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have counts field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"non_null_list_int_field_test"/utf8>>,
                    line => 69})
    end.

-file("test/types_helpers_test.gleam", 73).
-spec non_null_list_float_field_test() -> nil.
non_null_list_float_field_test() ->
    Item_type = begin
        _pipe = mochi@types:object(<<"Item"/utf8>>),
        _pipe@1 = mochi@types:non_null_list_float(
            _pipe,
            <<"scores"/utf8>>,
            fun(I) -> erlang:element(3, I) end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, {item, [], [], [], []}} end)
    end,
    case gleam@dict:has_key(erlang:element(4, Item_type), <<"scores"/utf8>>) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have scores field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"types_helpers_test"/utf8>>,
                    function => <<"non_null_list_float_field_test"/utf8>>,
                    line => 81})
    end.
