-record(custom_type, {
    location :: glance:span(),
    name :: binary(),
    publicity :: glance:publicity(),
    opaque_ :: boolean(),
    parameters :: list(binary()),
    variants :: list(glance:variant())
}).
