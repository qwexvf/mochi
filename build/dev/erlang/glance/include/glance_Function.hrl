-record(function, {
    location :: glance:span(),
    name :: binary(),
    publicity :: glance:publicity(),
    parameters :: list(glance:function_parameter()),
    return :: gleam@option:option(glance:type()),
    body :: list(glance:statement())
}).
