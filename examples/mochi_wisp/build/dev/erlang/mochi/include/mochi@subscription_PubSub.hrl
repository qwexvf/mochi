-record(pub_sub, {
    subscriptions :: gleam@dict:dict(binary(), mochi@subscription:subscription()),
    topics :: gleam@dict:dict(binary(), list(binary())),
    next_id :: integer()
}).
