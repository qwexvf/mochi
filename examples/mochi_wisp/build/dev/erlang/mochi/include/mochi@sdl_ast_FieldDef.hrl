-record(field_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    arguments :: list(mochi@sdl_ast:argument_def()),
    field_type :: mochi@sdl_ast:s_d_l_type(),
    directives :: list(mochi@sdl_ast:directive_usage())
}).
