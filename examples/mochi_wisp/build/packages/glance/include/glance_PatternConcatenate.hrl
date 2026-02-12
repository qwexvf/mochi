-record(pattern_concatenate, {
    location :: glance:span(),
    prefix :: binary(),
    prefix_name :: gleam@option:option(glance:assignment_name()),
    rest_name :: glance:assignment_name()
}).
