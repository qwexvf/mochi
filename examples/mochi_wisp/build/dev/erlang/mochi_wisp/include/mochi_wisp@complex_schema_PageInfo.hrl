-record(page_info, {
    has_next_page :: boolean(),
    has_previous_page :: boolean(),
    start_cursor :: gleam@option:option(binary()),
    end_cursor :: gleam@option:option(binary())
}).
