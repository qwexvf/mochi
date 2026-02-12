-record(object_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: gleam@dict:dict(binary(), mochi@schema:field_definition()),
    interfaces :: list(mochi@schema:interface_type())
}).
