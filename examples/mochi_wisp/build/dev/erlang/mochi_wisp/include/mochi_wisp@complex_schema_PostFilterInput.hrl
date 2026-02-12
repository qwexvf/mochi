-record(post_filter_input, {
    status :: gleam@option:option(mochi_wisp@complex_schema:post_status()),
    author_id :: gleam@option:option(binary()),
    tags :: gleam@option:option(list(binary())),
    search :: gleam@option:option(binary())
}).
