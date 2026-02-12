-record(directive_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    arguments :: gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    locations :: list(mochi@schema:directive_location()),
    is_repeatable :: boolean(),
    handler :: gleam@option:option(fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()}))
}).
