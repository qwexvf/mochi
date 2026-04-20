-record(arg_def, {
    name :: binary(),
    arg_type :: mochi@schema:field_type(),
    description :: gleam@option:option(binary()),
    default_value :: gleam@option:option(gleam@dynamic:dynamic_())
}).
