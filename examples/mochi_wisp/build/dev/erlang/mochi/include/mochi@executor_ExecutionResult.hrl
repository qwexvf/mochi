-record(execution_result, {
    data :: gleam@option:option(gleam@dynamic:dynamic_()),
    errors :: list(mochi@executor:execution_error())
}).
