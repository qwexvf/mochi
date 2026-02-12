-record(operation, {
    operation_type :: mochi@ast:operation_type(),
    name :: gleam@option:option(binary()),
    variable_definitions :: list(mochi@ast:variable_definition()),
    directives :: list(mochi@ast:directive()),
    selection_set :: mochi@ast:selection_set()
}).
