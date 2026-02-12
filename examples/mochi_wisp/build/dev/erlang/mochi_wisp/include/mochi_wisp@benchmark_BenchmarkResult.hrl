-record(benchmark_result, {
    name :: binary(),
    iterations :: integer(),
    total_us :: integer(),
    avg_us :: float(),
    min_us :: integer(),
    max_us :: integer(),
    ops_per_sec :: float()
}).
