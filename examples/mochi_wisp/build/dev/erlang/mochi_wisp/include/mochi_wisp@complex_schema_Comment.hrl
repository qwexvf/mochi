-record(comment, {
    id :: binary(),
    content :: binary(),
    author_id :: binary(),
    post_id :: binary(),
    parent_id :: gleam@option:option(binary()),
    created_at :: binary()
}).
