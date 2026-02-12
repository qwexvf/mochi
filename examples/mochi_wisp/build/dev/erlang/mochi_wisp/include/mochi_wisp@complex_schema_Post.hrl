-record(post, {
    id :: binary(),
    title :: binary(),
    content :: binary(),
    excerpt :: gleam@option:option(binary()),
    author_id :: binary(),
    status :: mochi_wisp@complex_schema:post_status(),
    tags :: list(binary()),
    view_count :: integer(),
    created_at :: binary(),
    updated_at :: binary()
}).
