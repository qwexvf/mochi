-record(field_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: mochi@schema:field_type(),
    arguments :: gleam@dict:dict(binary(), mochi@schema:argument_definition()),
    resolver :: gleam@option:option(fun((mochi@schema:resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})),
    is_deprecated :: boolean(),
    deprecation_reason :: gleam@option:option(binary())
}).
