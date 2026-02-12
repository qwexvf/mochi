-record(metrics_state, {
    total_requests :: integer(),
    successful_requests :: integer(),
    failed_requests :: integer(),
    total_duration_us :: integer(),
    max_duration_us :: integer(),
    min_duration_us :: integer()
}).
