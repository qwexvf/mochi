-record(interface_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage()),
    fields :: list(mochi@sdl_ast:field_def())
}).
