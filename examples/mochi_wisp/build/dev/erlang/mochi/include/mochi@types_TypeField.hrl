-record(type_field, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: mochi@schema:field_type(),
    extractor :: fun((any()) -> gleam@dynamic:dynamic_())
}).
