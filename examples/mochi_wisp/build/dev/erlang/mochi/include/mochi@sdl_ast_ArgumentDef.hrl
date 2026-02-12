-record(argument_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    arg_type :: mochi@sdl_ast:s_d_l_type(),
    default_value :: gleam@option:option(mochi@sdl_ast:s_d_l_value()),
    directives :: list(mochi@sdl_ast:directive_usage())
}).
