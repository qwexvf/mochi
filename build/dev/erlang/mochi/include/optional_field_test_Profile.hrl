-record(profile, {
    id :: binary(),
    name :: gleam@option:option(binary()),
    age :: gleam@option:option(integer()),
    score :: gleam@option:option(float()),
    active :: gleam@option:option(boolean())
}).
