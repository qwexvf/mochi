-record(create_post_input, {
    title :: binary(),
    content :: binary(),
    excerpt :: gleam@option:option(binary()),
    tags :: gleam@option:option(list(binary())),
    status :: gleam@option:option(mochi_wisp@complex_schema:post_status())
}).
