-record(telemetry_context, {
    config :: mochi@telemetry:telemetry_config(),
    events :: list(mochi@telemetry:telemetry_event()),
    operation_start :: gleam@option:option(integer()),
    field_timings :: gleam@dict:dict(binary(), {integer(), integer()}),
    current_path :: list(binary())
}).
