-record(list, {
    location :: glance:span(),
    elements :: list(glance:expression()),
    rest :: gleam@option:option(glance:expression())
}).
