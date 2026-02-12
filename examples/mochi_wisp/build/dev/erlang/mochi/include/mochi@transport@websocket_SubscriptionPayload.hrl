-record(subscription_payload, {
    'query' :: binary(),
    variables :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())),
    operation_name :: gleam@option:option(binary()),
    extensions :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
}).
