-record(operation_start, {
    timestamp :: integer(),
    operation_name :: gleam@option:option(binary()),
    operation_type :: mochi@telemetry:operation_type()
}).
