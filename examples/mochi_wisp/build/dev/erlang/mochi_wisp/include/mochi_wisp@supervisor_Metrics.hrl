-record(metrics, {
    total_requests :: integer(),
    successful_requests :: integer(),
    failed_requests :: integer(),
    total_duration_us :: integer(),
    avg_duration_us :: float(),
    max_duration_us :: integer(),
    min_duration_us :: integer()
}).
