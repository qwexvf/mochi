-record(enum_builder, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    values :: list(mochi@types:enum_value())
}).
