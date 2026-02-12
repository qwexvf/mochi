-record(app_state, {
    cache_manager :: gleam@erlang@process:subject(mochi_wisp@supervisor:cache_message()),
    metrics_collector :: gleam@erlang@process:subject(mochi_wisp@supervisor:metrics_message())
}).
