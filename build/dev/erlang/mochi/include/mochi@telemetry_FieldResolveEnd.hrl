-record(field_resolve_end, {
    timestamp :: integer(),
    field_name :: binary(),
    parent_type :: binary(),
    path :: list(binary()),
    success :: boolean(),
    duration_ns :: integer()
}).
