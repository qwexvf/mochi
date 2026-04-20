-record(input_field_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: mochi@schema:field_type(),
    default_value :: gleam@option:option(gleam@dynamic:dynamic_())
}).
