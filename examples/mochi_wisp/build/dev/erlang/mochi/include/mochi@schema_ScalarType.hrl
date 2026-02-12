-record(scalar_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    serialize :: fun((gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()}),
    parse_value :: fun((gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()}),
    parse_literal :: fun((gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
}).
