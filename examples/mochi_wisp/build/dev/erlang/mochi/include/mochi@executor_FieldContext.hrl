-record(field_context, {
    parent_value :: gleam@option:option(gleam@dynamic:dynamic_()),
    field_name :: binary(),
    field_args :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    path :: list(binary())
}).
