-record(constant, {
    location :: glance:span(),
    name :: binary(),
    publicity :: glance:publicity(),
    annotation :: gleam@option:option(glance:type()),
    value :: glance:expression()
}).
