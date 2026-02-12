-record(field, {
    alias :: gleam@option:option(binary()),
    name :: binary(),
    arguments :: list(mochi@ast:argument()),
    directives :: list(mochi@ast:directive()),
    selection_set :: gleam@option:option(mochi@ast:selection_set())
}).
