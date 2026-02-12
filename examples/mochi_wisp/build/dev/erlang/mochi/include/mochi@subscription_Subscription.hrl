-record(subscription, {
    id :: binary(),
    topic :: binary(),
    field_name :: binary(),
    arguments :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    callback :: fun((gleam@dynamic:dynamic_()) -> nil)
}).
