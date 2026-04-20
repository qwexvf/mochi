-record(middleware_def, {
    name :: binary(),
    priority :: integer(),
    field_filter :: gleam@option:option(mochi@middleware:field_filter()),
    middleware :: fun((mochi@middleware:resolution(), fun((mochi@middleware:resolution()) -> mochi@middleware:resolution())) -> mochi@middleware:resolution())
}).
