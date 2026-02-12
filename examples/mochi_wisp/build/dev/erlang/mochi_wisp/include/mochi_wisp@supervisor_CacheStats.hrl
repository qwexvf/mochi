-record(cache_stats, {
    hits :: integer(),
    misses :: integer(),
    size :: integer(),
    hit_rate :: float()
}).
