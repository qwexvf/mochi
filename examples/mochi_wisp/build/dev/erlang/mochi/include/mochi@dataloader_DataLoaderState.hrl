-record(data_loader_state, {
    batch_load_fn :: fun((list(any())) -> {ok,
            list({ok, any()} | {error, binary()})} |
        {error, binary()}),
    options :: mochi@dataloader:data_loader_options(),
    cache :: gleam@dict:dict(any(), {ok, any()} | {error, binary()}),
    pending_batch :: list(any()),
    batch_scheduled :: boolean()
}).
