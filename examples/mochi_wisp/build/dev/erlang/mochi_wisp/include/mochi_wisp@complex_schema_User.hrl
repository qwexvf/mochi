-record(user, {
    id :: binary(),
    username :: binary(),
    email :: binary(),
    display_name :: binary(),
    bio :: gleam@option:option(binary()),
    role :: mochi_wisp@complex_schema:role(),
    created_at :: binary(),
    updated_at :: binary()
}).
