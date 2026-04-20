-record(resolver_context, {
    field_name :: binary(),
    parent_type :: binary(),
    path :: list(binary()),
    info :: mochi@schema:resolver_info()
}).
