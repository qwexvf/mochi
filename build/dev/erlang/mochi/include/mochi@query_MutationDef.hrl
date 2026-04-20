-record(mutation_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    args_decoder :: fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
            any()} |
        {error, binary()}),
    resolver :: fun((any(), mochi@schema:execution_context()) -> {ok, any()} |
        {error, binary()}),
    result_encoder :: fun((any()) -> gleam@dynamic:dynamic_()),
    arg_definitions :: list(mochi@query:arg_def()),
    return_type :: mochi@schema:field_type()
}).
