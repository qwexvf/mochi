-record(connection_state, {
    schema :: mochi@schema:schema(),
    pubsub :: mochi@subscription:pub_sub(),
    execution_context :: mochi@schema:execution_context(),
    active_subscriptions :: gleam@dict:dict(binary(), binary()),
    acknowledged :: boolean(),
    connection_params :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
}).
