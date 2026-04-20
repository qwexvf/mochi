-module(telemetry_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/telemetry_test.gleam").
-export([disabled_config_test/0, with_handler_config_test/0, with_field_tracking_test/0, without_field_tracking_test/0, new_context_test/0, from_option_some_test/0, from_option_none_test/0, disabled_config_does_not_emit_test/0, enabled_config_emits_events_test/0, emit_if_present_none_test/0, emit_if_present_some_test/0, record_parse_start_test/0, record_parse_end_test/0, record_operation_start_test/0, record_operation_end_test/0, record_field_start_no_tracking_test/0, record_field_start_with_tracking_test/0, build_metrics_summary_empty_test/0, format_duration_nanoseconds_test/0, format_duration_microseconds_test/0, format_duration_milliseconds_test/0, format_duration_seconds_test/0, to_schema_fn_disabled_test/0, to_schema_fn_emits_operation_start_test/0, to_schema_fn_emits_field_events_test/0]).

-file("test/telemetry_test.gleam", 12).
-spec disabled_config_test() -> nil.
disabled_config_test() ->
    Config = mochi@telemetry:disabled(),
    gleeunit@should:equal(erlang:element(2, Config), false),
    gleeunit@should:equal(erlang:element(3, Config), false),
    gleeunit@should:equal(erlang:element(4, Config), false).

-file("test/telemetry_test.gleam", 19).
-spec with_handler_config_test() -> nil.
with_handler_config_test() ->
    Handler = fun(_) -> nil end,
    Config = mochi@telemetry:with_handler(Handler),
    gleeunit@should:equal(erlang:element(2, Config), true).

-file("test/telemetry_test.gleam", 25).
-spec with_field_tracking_test() -> nil.
with_field_tracking_test() ->
    Config = begin
        _pipe = mochi@telemetry:with_handler(fun(_) -> nil end),
        mochi@telemetry:with_field_tracking(_pipe)
    end,
    gleeunit@should:equal(erlang:element(3, Config), true).

-file("test/telemetry_test.gleam", 31).
-spec without_field_tracking_test() -> nil.
without_field_tracking_test() ->
    Config = begin
        _pipe = mochi@telemetry:with_handler(fun(_) -> nil end),
        _pipe@1 = mochi@telemetry:with_field_tracking(_pipe),
        mochi@telemetry:without_field_tracking(_pipe@1)
    end,
    gleeunit@should:equal(erlang:element(3, Config), false).

-file("test/telemetry_test.gleam", 43).
-spec new_context_test() -> nil.
new_context_test() ->
    Config = mochi@telemetry:disabled(),
    Ctx = mochi@telemetry:new_context(Config),
    gleeunit@should:equal(erlang:element(3, Ctx), []),
    gleeunit@should:equal(erlang:element(4, Ctx), none).

-file("test/telemetry_test.gleam", 50).
-spec from_option_some_test() -> nil.
from_option_some_test() ->
    Config = mochi@telemetry:disabled(),
    Result = mochi@telemetry:from_option({some, Config}),
    gleeunit@should:be_true(gleam@option:is_some(Result)).

-file("test/telemetry_test.gleam", 56).
-spec from_option_none_test() -> nil.
from_option_none_test() ->
    Result = mochi@telemetry:from_option(none),
    gleeunit@should:equal(Result, none).

-file("test/telemetry_test.gleam", 65).
-spec disabled_config_does_not_emit_test() -> nil.
disabled_config_does_not_emit_test() ->
    Config = mochi@telemetry:disabled(),
    Ctx = mochi@telemetry:new_context(Config),
    New_ctx = mochi@telemetry:emit(Ctx, {parse_start, 0}),
    gleeunit@should:equal(erlang:length(erlang:element(3, New_ctx)), 0).

-file("test/telemetry_test.gleam", 73).
-spec enabled_config_emits_events_test() -> nil.
enabled_config_emits_events_test() ->
    Events_ref = gleam@list:new(),
    _ = Events_ref,
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = mochi@telemetry:new_context(Config),
    New_ctx = mochi@telemetry:emit(Ctx, {parse_start, 0}),
    gleeunit@should:equal(erlang:length(erlang:element(3, New_ctx)), 1).

-file("test/telemetry_test.gleam", 84).
-spec emit_if_present_none_test() -> nil.
emit_if_present_none_test() ->
    Result = mochi@telemetry:emit_if_present(none, {parse_start, 0}),
    gleeunit@should:equal(Result, none).

-file("test/telemetry_test.gleam", 89).
-spec emit_if_present_some_test() -> nil.
emit_if_present_some_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = mochi@telemetry:new_context(Config),
    Result = mochi@telemetry:emit_if_present({some, Ctx}, {parse_start, 0}),
    gleeunit@should:be_true(gleam@option:is_some(Result)).

-file("test/telemetry_test.gleam", 100).
-spec record_parse_start_test() -> nil.
record_parse_start_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = begin
        _pipe = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_parse_start(_pipe)
    end,
    gleeunit@should:equal(erlang:length(erlang:element(3, Ctx)), 1),
    case gleam@list:first(erlang:element(3, Ctx)) of
        {ok, {parse_start, _}} ->
            nil;

        _ ->
            gleeunit@should:fail()
    end.

-file("test/telemetry_test.gleam", 110).
-spec record_parse_end_test() -> nil.
record_parse_end_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = begin
        _pipe = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_parse_end(_pipe, true)
    end,
    gleeunit@should:equal(erlang:length(erlang:element(3, Ctx)), 1),
    case gleam@list:first(erlang:element(3, Ctx)) of
        {ok, {parse_end, _, true}} ->
            nil;

        _ ->
            gleeunit@should:fail()
    end.

-file("test/telemetry_test.gleam", 120).
-spec record_operation_start_test() -> nil.
record_operation_start_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = begin
        _pipe = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_operation_start(
            _pipe,
            {some, <<"TestOp"/utf8>>},
            'query'
        )
    end,
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(4, Ctx))).

-file("test/telemetry_test.gleam", 128).
-spec record_operation_end_test() -> nil.
record_operation_end_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = begin
        _pipe = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_operation_end(
            _pipe,
            {some, <<"TestOp"/utf8>>},
            true,
            0
        )
    end,
    case gleam@list:first(erlang:element(3, Ctx)) of
        {ok, {operation_end, _, {some, <<"TestOp"/utf8>>}, true, 0}} ->
            nil;

        _ ->
            gleeunit@should:fail()
    end.

-file("test/telemetry_test.gleam", 139).
-spec record_field_start_no_tracking_test() -> nil.
record_field_start_no_tracking_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = begin
        _pipe = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_field_start(
            _pipe,
            <<"name"/utf8>>,
            <<"User"/utf8>>,
            [<<"user"/utf8>>, <<"name"/utf8>>]
        )
    end,
    gleeunit@should:equal(erlang:length(erlang:element(3, Ctx)), 0).

-file("test/telemetry_test.gleam", 149).
-spec record_field_start_with_tracking_test() -> nil.
record_field_start_with_tracking_test() ->
    Config = begin
        _pipe = mochi@telemetry:with_handler(fun(_) -> nil end),
        mochi@telemetry:with_field_tracking(_pipe)
    end,
    Ctx = begin
        _pipe@1 = mochi@telemetry:new_context(Config),
        mochi@telemetry:record_field_start(
            _pipe@1,
            <<"name"/utf8>>,
            <<"User"/utf8>>,
            [<<"user"/utf8>>, <<"name"/utf8>>]
        )
    end,
    gleeunit@should:equal(erlang:length(erlang:element(3, Ctx)), 1).

-file("test/telemetry_test.gleam", 162).
-spec build_metrics_summary_empty_test() -> nil.
build_metrics_summary_empty_test() ->
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Ctx = mochi@telemetry:new_context(Config),
    Summary = mochi@telemetry:build_metrics_summary(Ctx),
    gleeunit@should:equal(erlang:element(5, Summary), 0),
    gleeunit@should:equal(erlang:element(6, Summary), 0).

-file("test/telemetry_test.gleam", 170).
-spec format_duration_nanoseconds_test() -> nil.
format_duration_nanoseconds_test() ->
    gleeunit@should:equal(
        mochi@telemetry:format_duration(500),
        <<"500ns"/utf8>>
    ).

-file("test/telemetry_test.gleam", 174).
-spec format_duration_microseconds_test() -> nil.
format_duration_microseconds_test() ->
    gleeunit@should:equal(mochi@telemetry:format_duration(5000), <<"5us"/utf8>>).

-file("test/telemetry_test.gleam", 178).
-spec format_duration_milliseconds_test() -> nil.
format_duration_milliseconds_test() ->
    gleeunit@should:equal(
        mochi@telemetry:format_duration(5000000),
        <<"5ms"/utf8>>
    ).

-file("test/telemetry_test.gleam", 182).
-spec format_duration_seconds_test() -> nil.
format_duration_seconds_test() ->
    gleeunit@should:equal(
        mochi@telemetry:format_duration(2000000000),
        <<"2s"/utf8>>
    ).

-file("test/telemetry_test.gleam", 190).
-spec to_schema_fn_disabled_test() -> nil.
to_schema_fn_disabled_test() ->
    Config = mochi@telemetry:disabled(),
    Fn_ = mochi@telemetry:to_schema_fn(Config),
    Fn_({schema_operation_start, none}),
    nil.

-file("test/telemetry_test.gleam", 198).
-spec to_schema_fn_emits_operation_start_test() -> nil.
to_schema_fn_emits_operation_start_test() ->
    Received = [],
    _ = Received,
    Config = mochi@telemetry:with_handler(fun(_) -> nil end),
    Fn_ = mochi@telemetry:to_schema_fn(Config),
    Fn_({schema_operation_start, {some, <<"Query"/utf8>>}}),
    Fn_({schema_operation_end, {some, <<"Query"/utf8>>}, true, 0}),
    nil.

-file("test/telemetry_test.gleam", 209).
-spec to_schema_fn_emits_field_events_test() -> nil.
to_schema_fn_emits_field_events_test() ->
    Config = begin
        _pipe = mochi@telemetry:with_handler(fun(_) -> nil end),
        mochi@telemetry:with_field_tracking(_pipe)
    end,
    Fn_ = mochi@telemetry:to_schema_fn(Config),
    Fn_(
        {schema_field_start,
            <<"name"/utf8>>,
            <<"User"/utf8>>,
            [<<"user"/utf8>>, <<"name"/utf8>>]}
    ),
    Fn_(
        {schema_field_end,
            <<"name"/utf8>>,
            <<"User"/utf8>>,
            [<<"user"/utf8>>, <<"name"/utf8>>],
            true,
            1000}
    ),
    nil.
