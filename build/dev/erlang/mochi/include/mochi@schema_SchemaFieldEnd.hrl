-record(schema_field_end, {
    field_name :: binary(),
    parent_type :: binary(),
    path :: list(binary()),
    success :: boolean(),
    duration_ns :: integer()
}).
