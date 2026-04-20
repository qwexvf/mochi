-record(type_field_with_args, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: mochi@schema:field_type(),
    args :: list(mochi@schema:argument_definition()),
    resolver :: fun((any(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), mochi@schema:execution_context()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
}).
