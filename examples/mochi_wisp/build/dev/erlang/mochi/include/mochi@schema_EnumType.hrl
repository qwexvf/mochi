-record(enum_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    values :: gleam@dict:dict(binary(), mochi@schema:enum_value_definition())
}).
