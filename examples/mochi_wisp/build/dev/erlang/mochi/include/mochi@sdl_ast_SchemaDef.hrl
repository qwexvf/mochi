-record(schema_def, {
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage()),
    'query' :: gleam@option:option(binary()),
    mutation :: gleam@option:option(binary()),
    subscription :: gleam@option:option(binary())
}).
