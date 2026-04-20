-module(batch_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/batch_test.gleam").
-export([default_config_test/0, config_with_max_batch_size_test/0, config_with_continue_on_error_test/0, config_with_parallel_execution_test/0, request_basic_test/0, request_with_variables_test/0, request_with_operation_test/0, execute_single_request_test/0, execute_multiple_requests_test/0, execute_batch_exceeds_max_size_test/0, execute_with_parse_error_test/0, execute_batch_parallel_test/0, stop_on_error_test/0, execute_with_operation_name_test/0]).

-file("test/batch_test.gleam", 14).
-spec create_test_schema() -> mochi@schema:schema().
create_test_schema() ->
    Resolver = fun(_) -> {ok, gleam_stdlib:identity(<<"hello"/utf8>>)} end,
    Query_field = begin
        _pipe = mochi@schema:field_def(
            <<"hello"/utf8>>,
            {named, <<"String"/utf8>>}
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    Query_type = begin
        _pipe@1 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(_pipe@1, Query_field)
    end,
    _pipe@2 = mochi@schema:schema(),
    mochi@schema:'query'(_pipe@2, Query_type).

-file("test/batch_test.gleam", 28).
-spec create_test_context() -> mochi@schema:execution_context().
create_test_context() ->
    mochi@schema:execution_context(gleam_stdlib:identity(maps:new())).

-file("test/batch_test.gleam", 36).
-spec default_config_test() -> nil.
default_config_test() ->
    Config = mochi@batch:default_config(),
    gleeunit@should:equal(erlang:element(2, Config), 10),
    gleeunit@should:equal(erlang:element(3, Config), true),
    gleeunit@should:equal(erlang:element(4, Config), false).

-file("test/batch_test.gleam", 43).
-spec config_with_max_batch_size_test() -> nil.
config_with_max_batch_size_test() ->
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_max_batch_size(_pipe, 5)
    end,
    gleeunit@should:equal(erlang:element(2, Config), 5).

-file("test/batch_test.gleam", 48).
-spec config_with_continue_on_error_test() -> nil.
config_with_continue_on_error_test() ->
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_continue_on_error(_pipe, false)
    end,
    gleeunit@should:equal(erlang:element(3, Config), false).

-file("test/batch_test.gleam", 53).
-spec config_with_parallel_execution_test() -> nil.
config_with_parallel_execution_test() ->
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_parallel_execution(_pipe, true)
    end,
    gleeunit@should:equal(erlang:element(4, Config), true).

-file("test/batch_test.gleam", 62).
-spec request_basic_test() -> nil.
request_basic_test() ->
    Req = mochi@batch:request(<<"{ hello }"/utf8>>),
    gleeunit@should:equal(erlang:element(2, Req), <<"{ hello }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Req), none),
    gleeunit@should:equal(erlang:element(4, Req), none).

-file("test/batch_test.gleam", 69).
-spec request_with_variables_test() -> nil.
request_with_variables_test() ->
    Vars = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"123"/utf8>>)}]
    ),
    Req = mochi@batch:request_with_variables(<<"{ hello }"/utf8>>, Vars),
    gleeunit@should:equal(erlang:element(2, Req), <<"{ hello }"/utf8>>),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(3, Req))).

-file("test/batch_test.gleam", 76).
-spec request_with_operation_test() -> nil.
request_with_operation_test() ->
    Req = mochi@batch:request_with_operation(
        <<"query MyOp { hello }"/utf8>>,
        <<"MyOp"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(4, Req), {some, <<"MyOp"/utf8>>}).

-file("test/batch_test.gleam", 85).
-spec execute_single_request_test() -> nil.
execute_single_request_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = mochi@batch:default_config(),
    Requests = [mochi@batch:request(<<"{ hello }"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:equal(erlang:length(erlang:element(2, Result)), 1),
    gleeunit@should:equal(erlang:element(4, Result), 0),
    gleeunit@should:be_true(erlang:element(3, Result)).

-file("test/batch_test.gleam", 98).
-spec execute_multiple_requests_test() -> nil.
execute_multiple_requests_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = mochi@batch:default_config(),
    Requests = [mochi@batch:request(<<"{ hello }"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:equal(erlang:length(erlang:element(2, Result)), 2),
    gleeunit@should:equal(erlang:element(4, Result), 0),
    gleeunit@should:be_true(erlang:element(3, Result)).

-file("test/batch_test.gleam", 111).
-spec execute_batch_exceeds_max_size_test() -> nil.
execute_batch_exceeds_max_size_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_max_batch_size(_pipe, 2)
    end,
    Requests = [mochi@batch:request(<<"{ hello }"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:be_false(erlang:element(3, Result)),
    gleeunit@should:equal(erlang:element(4, Result), 1).

-file("test/batch_test.gleam", 127).
-spec execute_with_parse_error_test() -> nil.
execute_with_parse_error_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = mochi@batch:default_config(),
    Requests = [mochi@batch:request(<<"INVALID QUERY !!!"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:equal(erlang:element(4, Result), 1),
    gleeunit@should:be_false(erlang:element(3, Result)).

-file("test/batch_test.gleam", 139).
-spec execute_batch_parallel_test() -> nil.
execute_batch_parallel_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_parallel_execution(_pipe, true)
    end,
    Requests = [mochi@batch:request(<<"{ hello }"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:equal(erlang:length(erlang:element(2, Result)), 2),
    gleeunit@should:equal(erlang:element(4, Result), 0),
    gleeunit@should:be_true(erlang:element(3, Result)).

-file("test/batch_test.gleam", 153).
-spec stop_on_error_test() -> nil.
stop_on_error_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Config = begin
        _pipe = mochi@batch:default_config(),
        mochi@batch:with_continue_on_error(_pipe, false)
    end,
    Requests = [mochi@batch:request(<<"INVALID"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>),
        mochi@batch:request(<<"{ hello }"/utf8>>)],
    Result = mochi@batch:execute_batch(Schema_def, Requests, Config, Ctx),
    gleeunit@should:be_false(erlang:element(3, Result)),
    gleeunit@should:be_true(erlang:element(4, Result) > 0).

-file("test/batch_test.gleam", 202).
-spec mochi_batch_parse_for_test(binary()) -> {ok, mochi@ast:document()} |
    {error, mochi@parser:parse_error()}.
mochi_batch_parse_for_test(Query) ->
    mochi@parser:parse(Query).

-file("test/batch_test.gleam", 176).
-spec execute_with_operation_name_test() -> nil.
execute_with_operation_name_test() ->
    Schema_def = create_test_schema(),
    Ctx = create_test_context(),
    Query = <<"query GetHello { hello }"/utf8>>,
    Doc_result = mochi_batch_parse_for_test(Query),
    case Doc_result of
        {ok, Document} ->
            Result = mochi@batch:execute_with_operation_name(
                Schema_def,
                Document,
                none,
                Ctx,
                maps:new(),
                {some, <<"GetHello"/utf8>>}
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            nil
    end.
