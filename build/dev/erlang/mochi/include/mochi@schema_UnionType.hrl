-record(union_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    types :: list(mochi@schema:object_type()),
    resolve_type :: gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok,
            binary()} |
        {error, binary()}))
}).
