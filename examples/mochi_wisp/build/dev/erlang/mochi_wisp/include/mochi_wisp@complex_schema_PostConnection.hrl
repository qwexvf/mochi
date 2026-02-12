-record(post_connection, {
    edges :: list(mochi_wisp@complex_schema:post_edge()),
    page_info :: mochi_wisp@complex_schema:page_info(),
    total_count :: integer()
}).
