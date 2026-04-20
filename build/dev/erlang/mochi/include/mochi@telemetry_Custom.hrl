-record(custom, {
    timestamp :: integer(),
    name :: binary(),
    data :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
}).
