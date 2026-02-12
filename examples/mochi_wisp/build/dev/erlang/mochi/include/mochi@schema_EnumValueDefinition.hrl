-record(enum_value_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    value :: gleam@dynamic:dynamic_(),
    is_deprecated :: boolean(),
    deprecation_reason :: gleam@option:option(binary())
}).
