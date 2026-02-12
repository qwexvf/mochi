-record(panic, {
    location :: glance:span(),
    message :: gleam@option:option(glance:expression())
}).
