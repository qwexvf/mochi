-record(query_execution_context, {
    schema :: mochi@schema:schema(),
    root_value :: gleam@option:option(gleam@dynamic:dynamic_()),
    execution_context :: mochi@schema:execution_context(),
    variable_values :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    fragments :: gleam@dict:dict(binary(), mochi@ast:fragment())
}).
