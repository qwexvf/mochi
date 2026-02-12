-record(assert, {
    location :: glance:span(),
    expression :: glance:expression(),
    message :: gleam@option:option(glance:expression())
}).
