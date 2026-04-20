-record(graph_q_l_error, {
    message :: binary(),
    locations :: gleam@option:option(list(mochi@error:location())),
    path :: gleam@option:option(list(mochi@error:path_segment())),
    extensions :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
}).
