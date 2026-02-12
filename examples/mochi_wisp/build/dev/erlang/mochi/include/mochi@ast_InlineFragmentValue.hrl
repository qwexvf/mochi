-record(inline_fragment_value, {
    type_condition :: gleam@option:option(binary()),
    directives :: list(mochi@ast:directive()),
    selection_set :: mochi@ast:selection_set()
}).
