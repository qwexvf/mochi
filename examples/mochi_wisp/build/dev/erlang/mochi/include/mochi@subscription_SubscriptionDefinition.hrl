-record(subscription_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: mochi@schema:field_type(),
    arguments :: gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    topic_resolver :: fun((mochi@schema:resolver_info()) -> {ok, binary()} |
        {error, binary()}),
    event_transformer :: fun((any()) -> gleam@dynamic:dynamic_()),
    filter :: gleam@option:option(fun((any(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> boolean()))
}).
