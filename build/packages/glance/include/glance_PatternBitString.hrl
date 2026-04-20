-record(pattern_bit_string, {
    location :: glance:span(),
    segments :: list({glance:pattern(),
        list(glance:bit_string_segment_option(glance:pattern()))})
}).
