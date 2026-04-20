-record(enum_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage()),
    values :: list(mochi@sdl_ast:enum_value_def())
}).
