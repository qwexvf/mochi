-record(type_builder, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: list(mochi@types:type_field(any()))
}).
