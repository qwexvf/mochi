-record(call, {
    location :: glance:span(),
    function :: glance:expression(),
    arguments :: list(glance:field(glance:expression()))
}).
