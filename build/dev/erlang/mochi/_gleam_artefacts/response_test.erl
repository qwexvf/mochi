-module(response_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/response_test.gleam").
-export([success_response_test/0, failure_response_test/0, partial_response_test/0, from_execution_result_success_test/0, from_execution_result_with_errors_test/0, from_execution_result_failure_test/0, response_with_extension_test/0, response_with_multiple_extensions_test/0, response_with_extensions_dict_test/0, response_with_tracing_test/0, to_dynamic_success_test/0, to_dynamic_failure_test/0, to_dynamic_partial_test/0, to_dynamic_with_extensions_test/0, serialize_test/0, has_errors_empty_test/0, has_errors_with_errors_test/0, has_data_with_data_test/0, has_data_without_data_test/0, error_count_zero_test/0, error_count_multiple_test/0, is_success_true_test/0, is_success_false_with_errors_test/0, is_partial_true_test/0, is_partial_false_no_errors_test/0, format_success_test/0, format_failure_test/0, format_with_extensions_test/0, convert_validation_error_test/0, convert_resolver_error_test/0, convert_type_error_test/0, error_with_location_test/0, error_without_location_test/0, resolver_error_with_location_test/0]).

-file("test/response_test.gleam", 14).
-spec success_response_test() -> nil.
success_response_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"user"/utf8>>, gleam_stdlib:identity(<<"test"/utf8>>)}]
        )
    ),
    Resp = mochi@response:success(Data),
    case mochi@response:has_data(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Success response should have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"success_response_test"/utf8>>,
                    line => 21})
    end,
    case mochi@response:has_errors(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Success response should not have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"success_response_test"/utf8>>,
                    line => 26})
    end,
    case mochi@response:is_success(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be classified as success"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"success_response_test"/utf8>>,
                    line => 31})
    end.

-file("test/response_test.gleam", 35).
-spec failure_response_test() -> nil.
failure_response_test() ->
    Errors = [mochi@error:error(<<"Something went wrong"/utf8>>)],
    Resp = mochi@response:failure(Errors),
    case mochi@response:has_data(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failure response should not have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"failure_response_test"/utf8>>,
                    line => 41})
    end,
    case mochi@response:has_errors(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Failure response should have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"failure_response_test"/utf8>>,
                    line => 46})
    end,
    case mochi@response:error_count(Resp) =:= 1 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 1 error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"failure_response_test"/utf8>>,
                    line => 51})
    end.

-file("test/response_test.gleam", 55).
-spec partial_response_test() -> nil.
partial_response_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"partial"/utf8>>, gleam_stdlib:identity(<<"data"/utf8>>)}]
        )
    ),
    Errors = [mochi@error:error(<<"Partial error"/utf8>>)],
    Resp = mochi@response:partial(Data, Errors),
    case mochi@response:has_data(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Partial response should have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"partial_response_test"/utf8>>,
                    line => 63})
    end,
    case mochi@response:has_errors(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Partial response should have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"partial_response_test"/utf8>>,
                    line => 68})
    end,
    case mochi@response:is_partial(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be classified as partial"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"partial_response_test"/utf8>>,
                    line => 73})
    end.

-file("test/response_test.gleam", 81).
-spec from_execution_result_success_test() -> nil.
from_execution_result_success_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"result"/utf8>>, gleam_stdlib:identity(<<"ok"/utf8>>)}]
        )
    ),
    Exec_result = {execution_result, {some, Data}, []},
    Resp = mochi@response:from_execution_result(Exec_result),
    case mochi@response:is_success(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be success"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"from_execution_result_success_test"/utf8>>,
                    line => 90})
    end.

-file("test/response_test.gleam", 94).
-spec from_execution_result_with_errors_test() -> nil.
from_execution_result_with_errors_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"partial"/utf8>>, gleam_stdlib:identity(<<"data"/utf8>>)}]
        )
    ),
    Errors = [{validation_error,
            <<"Field not found"/utf8>>,
            [<<"user"/utf8>>, <<"email"/utf8>>],
            none},
        {resolver_error,
            <<"Database error"/utf8>>,
            [<<"query"/utf8>>, <<"users"/utf8>>],
            none}],
    Exec_result = {execution_result, {some, Data}, Errors},
    Resp = mochi@response:from_execution_result(Exec_result),
    case mochi@response:is_partial(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be partial"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"from_execution_result_with_errors_test"/utf8>>,
                    line => 111})
    end,
    case mochi@response:error_count(Resp) =:= 2 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 2 errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"from_execution_result_with_errors_test"/utf8>>,
                    line => 116})
    end.

-file("test/response_test.gleam", 120).
-spec from_execution_result_failure_test() -> nil.
from_execution_result_failure_test() ->
    Errors = [{type_error, <<"Type mismatch"/utf8>>, [<<"field"/utf8>>], none}],
    Exec_result = {execution_result, none, Errors},
    Resp = mochi@response:from_execution_result(Exec_result),
    case mochi@response:has_data(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"from_execution_result_failure_test"/utf8>>,
                    line => 128})
    end,
    case mochi@response:has_errors(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"from_execution_result_failure_test"/utf8>>,
                    line => 133})
    end.

-file("test/response_test.gleam", 141).
-spec response_with_extension_test() -> nil.
response_with_extension_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Resp = begin
        _pipe = mochi@response:success(Data),
        mochi@response:with_extension(
            _pipe,
            <<"requestId"/utf8>>,
            gleam_stdlib:identity(<<"req-123"/utf8>>)
        )
    end,
    case erlang:element(4, Resp) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"requestId"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have requestId extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"response_with_extension_test"/utf8>>,
                            line => 151})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"response_with_extension_test"/utf8>>,
                    line => 154})
    end.

-file("test/response_test.gleam", 158).
-spec response_with_multiple_extensions_test() -> nil.
response_with_multiple_extensions_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Resp = begin
        _pipe = mochi@response:success(Data),
        _pipe@1 = mochi@response:with_extension(
            _pipe,
            <<"key1"/utf8>>,
            gleam_stdlib:identity(<<"value1"/utf8>>)
        ),
        _pipe@2 = mochi@response:with_extension(
            _pipe@1,
            <<"key2"/utf8>>,
            gleam_stdlib:identity(<<"value2"/utf8>>)
        ),
        mochi@response:with_extension(
            _pipe@2,
            <<"key3"/utf8>>,
            gleam_stdlib:identity(<<"value3"/utf8>>)
        )
    end,
    case erlang:element(4, Resp) of
        {some, Ext} ->
            case maps:size(Ext) =:= 3 of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 3 extensions"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"response_with_multiple_extensions_test"/utf8>>,
                            line => 170})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"response_with_multiple_extensions_test"/utf8>>,
                    line => 173})
    end.

-file("test/response_test.gleam", 177).
-spec response_with_extensions_dict_test() -> nil.
response_with_extensions_dict_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Extensions = maps:from_list(
        [{<<"a"/utf8>>, gleam_stdlib:identity(1)},
            {<<"b"/utf8>>, gleam_stdlib:identity(2)}]
    ),
    Resp = begin
        _pipe = mochi@response:success(Data),
        mochi@response:with_extensions(_pipe, Extensions)
    end,
    case erlang:element(4, Resp) of
        {some, Ext} ->
            case maps:size(Ext) =:= 2 of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 2 extensions"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"response_with_extensions_dict_test"/utf8>>,
                            line => 193})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"response_with_extensions_dict_test"/utf8>>,
                    line => 196})
    end.

-file("test/response_test.gleam", 200).
-spec response_with_tracing_test() -> nil.
response_with_tracing_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Resp = begin
        _pipe = mochi@response:success(Data),
        mochi@response:with_tracing(_pipe, 1000, 1500)
    end,
    case erlang:element(4, Resp) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"tracing"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have tracing extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"response_with_tracing_test"/utf8>>,
                            line => 210})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"response_with_tracing_test"/utf8>>,
                    line => 213})
    end.

-file("test/response_test.gleam", 221).
-spec to_dynamic_success_test() -> nil.
to_dynamic_success_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list(
            [{<<"user"/utf8>>, gleam_stdlib:identity(<<"john"/utf8>>)}]
        )
    ),
    Resp = mochi@response:success(Data),
    _ = mochi@response:to_dynamic(Resp),
    nil.

-file("test/response_test.gleam", 232).
-spec to_dynamic_failure_test() -> nil.
to_dynamic_failure_test() ->
    Errors = [mochi@error:error(<<"Error 1"/utf8>>),
        begin
            _pipe = mochi@error:error(<<"Error 2"/utf8>>),
            mochi@error:at_location(_pipe, 5, 10)
        end],
    Resp = mochi@response:failure(Errors),
    _ = mochi@response:to_dynamic(Resp),
    nil.

-file("test/response_test.gleam", 245).
-spec to_dynamic_partial_test() -> nil.
to_dynamic_partial_test() ->
    Data = gleam_stdlib:identity(
        maps:from_list([{<<"partial"/utf8>>, gleam_stdlib:identity(true)}])
    ),
    Errors = [mochi@error:validation_error(
            <<"Warning"/utf8>>,
            [<<"field"/utf8>>]
        )],
    Resp = mochi@response:partial(Data, Errors),
    _ = mochi@response:to_dynamic(Resp),
    nil.

-file("test/response_test.gleam", 256).
-spec to_dynamic_with_extensions_test() -> nil.
to_dynamic_with_extensions_test() ->
    Data = gleam_stdlib:identity(<<"data"/utf8>>),
    Resp = begin
        _pipe = mochi@response:success(Data),
        mochi@response:with_extension(
            _pipe,
            <<"custom"/utf8>>,
            gleam_stdlib:identity(<<"value"/utf8>>)
        )
    end,
    _ = mochi@response:to_dynamic(Resp),
    nil.

-file("test/response_test.gleam", 267).
-spec serialize_test() -> nil.
serialize_test() ->
    Data = gleam_stdlib:identity(<<"test"/utf8>>),
    Resp = mochi@response:success(Data),
    _ = mochi@response:serialize(Resp),
    nil.

-file("test/response_test.gleam", 281).
-spec has_errors_empty_test() -> nil.
has_errors_empty_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"test"/utf8>>)),
    case mochi@response:has_errors(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"has_errors_empty_test"/utf8>>,
                    line => 286})
    end.

-file("test/response_test.gleam", 290).
-spec has_errors_with_errors_test() -> nil.
has_errors_with_errors_test() ->
    Resp = mochi@response:failure([mochi@error:error(<<"error"/utf8>>)]),
    case mochi@response:has_errors(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have errors"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"has_errors_with_errors_test"/utf8>>,
                    line => 295})
    end.

-file("test/response_test.gleam", 299).
-spec has_data_with_data_test() -> nil.
has_data_with_data_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"data"/utf8>>)),
    case mochi@response:has_data(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"has_data_with_data_test"/utf8>>,
                    line => 304})
    end.

-file("test/response_test.gleam", 308).
-spec has_data_without_data_test() -> nil.
has_data_without_data_test() ->
    Resp = mochi@response:failure([mochi@error:error(<<"error"/utf8>>)]),
    case mochi@response:has_data(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not have data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"has_data_without_data_test"/utf8>>,
                    line => 313})
    end.

-file("test/response_test.gleam", 317).
-spec error_count_zero_test() -> nil.
error_count_zero_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"test"/utf8>>)),
    case mochi@response:error_count(Resp) =:= 0 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Error count should be 0"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"error_count_zero_test"/utf8>>,
                    line => 322})
    end.

-file("test/response_test.gleam", 326).
-spec error_count_multiple_test() -> nil.
error_count_multiple_test() ->
    Errors = [mochi@error:error(<<"Error 1"/utf8>>),
        mochi@error:error(<<"Error 2"/utf8>>),
        mochi@error:error(<<"Error 3"/utf8>>)],
    Resp = mochi@response:failure(Errors),
    case mochi@response:error_count(Resp) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Error count should be 3"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"error_count_multiple_test"/utf8>>,
                    line => 336})
    end.

-file("test/response_test.gleam", 340).
-spec is_success_true_test() -> nil.
is_success_true_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"test"/utf8>>)),
    case mochi@response:is_success(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be success"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"is_success_true_test"/utf8>>,
                    line => 345})
    end.

-file("test/response_test.gleam", 349).
-spec is_success_false_with_errors_test() -> nil.
is_success_false_with_errors_test() ->
    Resp = mochi@response:failure([mochi@error:error(<<"error"/utf8>>)]),
    case mochi@response:is_success(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not be success"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"is_success_false_with_errors_test"/utf8>>,
                    line => 354})
    end.

-file("test/response_test.gleam", 358).
-spec is_partial_true_test() -> nil.
is_partial_true_test() ->
    Resp = mochi@response:partial(
        gleam_stdlib:identity(<<"data"/utf8>>),
        [mochi@error:error(<<"warning"/utf8>>)]
    ),
    case mochi@response:is_partial(Resp) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be partial"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"is_partial_true_test"/utf8>>,
                    line => 364})
    end.

-file("test/response_test.gleam", 368).
-spec is_partial_false_no_errors_test() -> nil.
is_partial_false_no_errors_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"data"/utf8>>)),
    case mochi@response:is_partial(Resp) of
        false ->
            nil;

        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not be partial"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"is_partial_false_no_errors_test"/utf8>>,
                    line => 373})
    end.

-file("test/response_test.gleam", 381).
-spec format_success_test() -> nil.
format_success_test() ->
    Resp = mochi@response:success(gleam_stdlib:identity(<<"test"/utf8>>)),
    Formatted = mochi@response:format(Resp),
    case Formatted of
        <<""/utf8>> ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Format should produce non-empty string"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"format_success_test"/utf8>>,
                    line => 387});

        _ ->
            nil
    end.

-file("test/response_test.gleam", 392).
-spec format_failure_test() -> nil.
format_failure_test() ->
    Resp = mochi@response:failure([mochi@error:error(<<"error"/utf8>>)]),
    Formatted = mochi@response:format(Resp),
    case Formatted of
        <<""/utf8>> ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Format should produce non-empty string"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"format_failure_test"/utf8>>,
                    line => 397});

        _ ->
            nil
    end.

-file("test/response_test.gleam", 402).
-spec format_with_extensions_test() -> nil.
format_with_extensions_test() ->
    Resp = begin
        _pipe = mochi@response:success(gleam_stdlib:identity(<<"test"/utf8>>)),
        mochi@response:with_extension(
            _pipe,
            <<"key"/utf8>>,
            gleam_stdlib:identity(<<"value"/utf8>>)
        )
    end,
    Formatted = mochi@response:format(Resp),
    case Formatted of
        <<""/utf8>> ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Format should produce non-empty string"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"format_with_extensions_test"/utf8>>,
                    line => 410});

        _ ->
            nil
    end.

-file("test/response_test.gleam", 419).
-spec convert_validation_error_test() -> nil.
convert_validation_error_test() ->
    Exec_err = {validation_error,
        <<"Field not found"/utf8>>,
        [<<"user"/utf8>>, <<"email"/utf8>>],
        none},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(2, Gql_err) =:= <<"Field not found"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be preserved"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"convert_validation_error_test"/utf8>>,
                    line => 430})
    end,
    case erlang:element(5, Gql_err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"category"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have category extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"convert_validation_error_test"/utf8>>,
                            line => 437})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"convert_validation_error_test"/utf8>>,
                    line => 440})
    end.

-file("test/response_test.gleam", 444).
-spec convert_resolver_error_test() -> nil.
convert_resolver_error_test() ->
    Exec_err = {resolver_error,
        <<"Database error"/utf8>>,
        [<<"query"/utf8>>, <<"users"/utf8>>],
        none},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(2, Gql_err) =:= <<"Database error"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be preserved"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"convert_resolver_error_test"/utf8>>,
                    line => 451})
    end.

-file("test/response_test.gleam", 455).
-spec convert_type_error_test() -> nil.
convert_type_error_test() ->
    Exec_err = {type_error, <<"Type mismatch"/utf8>>, [<<"field"/utf8>>], none},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(2, Gql_err) =:= <<"Type mismatch"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be preserved"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"convert_type_error_test"/utf8>>,
                    line => 461})
    end.

-file("test/response_test.gleam", 469).
-spec error_with_location_test() -> nil.
error_with_location_test() ->
    Exec_err = {validation_error,
        <<"Field not found"/utf8>>,
        [<<"user"/utf8>>, <<"name"/utf8>>],
        {some, {5, 10}}},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(3, Gql_err) of
        {some, [{location, Line, Col}]} ->
            case (Line =:= 5) andalso (Col =:= 10) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Location should be line 5, column 10"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"error_with_location_test"/utf8>>,
                            line => 483})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have exactly one location"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"error_with_location_test"/utf8>>,
                    line => 486})
    end.

-file("test/response_test.gleam", 490).
-spec error_without_location_test() -> nil.
error_without_location_test() ->
    Exec_err = {validation_error,
        <<"Field not found"/utf8>>,
        [<<"user"/utf8>>],
        none},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(3, Gql_err) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should not have locations when None"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"error_without_location_test"/utf8>>,
                    line => 498})
    end.

-file("test/response_test.gleam", 502).
-spec resolver_error_with_location_test() -> nil.
resolver_error_with_location_test() ->
    Exec_err = {resolver_error,
        <<"Database error"/utf8>>,
        [<<"query"/utf8>>, <<"users"/utf8>>],
        {some, {3, 5}}},
    Gql_err = mochi@response:execution_error_to_graphql_error(Exec_err),
    case erlang:element(3, Gql_err) of
        {some, [{location, Line, Col}]} ->
            case (Line =:= 3) andalso (Col =:= 5) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Location should be line 3, column 5"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"response_test"/utf8>>,
                            function => <<"resolver_error_with_location_test"/utf8>>,
                            line => 515})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have exactly one location"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"response_test"/utf8>>,
                    function => <<"resolver_error_with_location_test"/utf8>>,
                    line => 518})
    end.
