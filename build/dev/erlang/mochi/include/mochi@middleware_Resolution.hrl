-record(resolution, {
    value :: gleam@option:option(gleam@dynamic:dynamic_()),
    error :: gleam@option:option(binary()),
    field_name :: binary(),
    parent_type :: binary(),
    context :: mochi@middleware:resolver_context(),
    private :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
}).
