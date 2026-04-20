-record(batch_result, {
    results :: list(mochi@executor:execution_result()),
    all_succeeded :: boolean(),
    failure_count :: integer()
}).
