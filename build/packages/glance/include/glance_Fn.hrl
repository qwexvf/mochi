-record(fn, {
    location :: glance:span(),
    arguments :: list(glance:fn_parameter()),
    return_annotation :: gleam@option:option(glance:type()),
    body :: list(glance:statement())
}).
