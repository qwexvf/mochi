-record(fragment, {
    name :: binary(),
    type_condition :: binary(),
    directives :: list(mochi@ast:directive()),
    selection_set :: mochi@ast:selection_set()
}).
