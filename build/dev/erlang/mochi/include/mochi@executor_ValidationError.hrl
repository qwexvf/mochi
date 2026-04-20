-record(validation_error, {
    message :: binary(),
    path :: list(binary()),
    location :: gleam@option:option({integer(), integer()})
}).
