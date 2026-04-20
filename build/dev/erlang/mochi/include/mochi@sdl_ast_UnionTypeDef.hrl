-record(union_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage()),
    member_types :: list(binary())
}).
