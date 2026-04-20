-record(import, {
    location :: glance:span(),
    module :: binary(),
    alias :: gleam@option:option(glance:assignment_name()),
    unqualified_types :: list(glance:unqualified_import()),
    unqualified_values :: list(glance:unqualified_import())
}).
