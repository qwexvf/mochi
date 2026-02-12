-record(field_spec, {
    name :: binary(),
    field_type :: mochi@schema:field_type(),
    description :: binary(),
    extractor :: fun((gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
}).
