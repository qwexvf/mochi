-record(input_builder, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: list(mochi@types:input_field())
}).
