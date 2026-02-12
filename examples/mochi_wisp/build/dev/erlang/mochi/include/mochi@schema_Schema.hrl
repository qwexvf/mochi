-record(schema, {
    'query' :: gleam@option:option(mochi@schema:object_type()),
    mutation :: gleam@option:option(mochi@schema:object_type()),
    subscription :: gleam@option:option(mochi@schema:object_type()),
    types :: gleam@dict:dict(binary(), mochi@schema:type_definition()),
    directives :: gleam@dict:dict(binary(), mochi@schema:directive_definition())
}).
