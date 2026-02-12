-record(pattern_list, {
    location :: glance:span(),
    elements :: list(glance:pattern()),
    tail :: gleam@option:option(glance:pattern())
}).
