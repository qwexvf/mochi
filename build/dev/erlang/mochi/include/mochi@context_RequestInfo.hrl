-record(request_info, {
    headers :: gleam@dict:dict(binary(), binary()),
    method :: binary(),
    path :: binary()
}).
