-record(update_user_input, {
    display_name :: gleam@option:option(binary()),
    bio :: gleam@option:option(binary()),
    role :: gleam@option:option(mochi_wisp@complex_schema:role())
}).
