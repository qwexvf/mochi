-record(schema_operation_end, {
    operation_name :: gleam@option:option(binary()),
    success :: boolean(),
    error_count :: integer()
}).
