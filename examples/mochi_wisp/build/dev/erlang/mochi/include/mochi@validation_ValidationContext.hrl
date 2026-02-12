-record(validation_context, {
    schema :: mochi@schema:schema(),
    fragments :: gleam@dict:dict(binary(), mochi@ast:fragment()),
    defined_variables :: gleam@set:set(binary()),
    used_variables :: gleam@set:set(binary()),
    errors :: list(mochi@validation:validation_error()),
    current_type :: gleam@option:option(mochi@schema:object_type()),
    fragment_spread_path :: list(binary())
}).
