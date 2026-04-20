-record(metrics_summary, {
    total_duration_ns :: integer(),
    parse_duration_ns :: integer(),
    validation_duration_ns :: integer(),
    field_count :: integer(),
    error_count :: integer(),
    slowest_fields :: list({binary(), integer()})
}).
