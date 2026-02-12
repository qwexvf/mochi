-record(variable_definition, {
    variable :: binary(),
    type_ :: mochi@ast:type(),
    default_value :: gleam@option:option(mochi@ast:value()),
    directives :: list(mochi@ast:directive())
}).
