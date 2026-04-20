-record(operation_end, {
    timestamp :: integer(),
    operation_name :: gleam@option:option(binary()),
    success :: boolean(),
    error_count :: integer()
}).
