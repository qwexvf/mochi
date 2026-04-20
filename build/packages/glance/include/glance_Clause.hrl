-record(clause, {
    patterns :: list(list(glance:pattern())),
    guard :: gleam@option:option(glance:expression()),
    body :: glance:expression()
}).
