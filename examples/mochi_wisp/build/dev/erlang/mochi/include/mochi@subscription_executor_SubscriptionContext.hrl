-record(subscription_context, {
    schema :: mochi@schema:schema(),
    pubsub :: mochi@subscription:pub_sub(),
    execution_context :: mochi@schema:execution_context(),
    variable_values :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
}).
