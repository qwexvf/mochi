-record(input_object_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(mochi@sdl_ast:directive_usage()),
    fields :: list(mochi@sdl_ast:input_field_def())
}).
