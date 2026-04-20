-record(bit_string, {
    location :: glance:span(),
    segments :: list({glance:expression(),
        list(glance:bit_string_segment_option(glance:expression()))})
}).
