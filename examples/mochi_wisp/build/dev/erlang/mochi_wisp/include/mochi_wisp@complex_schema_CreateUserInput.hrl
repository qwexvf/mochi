-record(create_user_input, {
    username :: binary(),
    email :: binary(),
    display_name :: binary(),
    bio :: gleam@option:option(binary()),
    role :: gleam@option:option(mochi_wisp@complex_schema:role())
}).
