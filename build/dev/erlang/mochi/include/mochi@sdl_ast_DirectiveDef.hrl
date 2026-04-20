-record(directive_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    locations :: list(mochi@sdl_ast:directive_location()),
    arguments :: list(mochi@sdl_ast:argument_def())
}).
