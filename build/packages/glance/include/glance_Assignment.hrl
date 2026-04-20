-record(assignment, {
    location :: glance:span(),
    kind :: glance:assignment_kind(),
    pattern :: glance:pattern(),
    annotation :: gleam@option:option(glance:type()),
    value :: glance:expression()
}).
