-record(named_type, {
    location :: glance:span(),
    name :: binary(),
    module :: gleam@option:option(binary()),
    parameters :: list(glance:type())
}).
