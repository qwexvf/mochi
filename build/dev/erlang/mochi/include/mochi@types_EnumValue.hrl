-record(enum_value, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    is_deprecated :: boolean(),
    deprecation_reason :: gleam@option:option(binary())
}).
