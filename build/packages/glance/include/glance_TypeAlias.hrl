-record(type_alias, {
    location :: glance:span(),
    name :: binary(),
    publicity :: glance:publicity(),
    parameters :: list(binary()),
    aliased :: glance:type()
}).
