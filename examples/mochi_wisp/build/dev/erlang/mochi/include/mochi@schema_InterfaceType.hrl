-record(interface_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: gleam@dict:dict(binary(), mochi@schema:field_definition()),
    resolve_type :: gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok,
            binary()} |
        {error, binary()}))
}).
