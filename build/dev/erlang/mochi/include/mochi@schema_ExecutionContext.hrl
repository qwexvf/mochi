-record(execution_context, {
    user_context :: gleam@dynamic:dynamic_(),
    data_loaders :: gleam@dict:dict(binary(), mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())),
    middleware_fn :: gleam@option:option(fun((binary(), mochi@schema:field_definition(), mochi@schema:resolver_info(), fun((mochi@schema:resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})),
    telemetry :: gleam@option:option(mochi@schema:telemetry_context()),
    telemetry_fn :: gleam@option:option(fun((mochi@schema:schema_event()) -> nil))
}).
