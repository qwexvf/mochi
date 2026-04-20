-record(enum_value_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage())
}).
