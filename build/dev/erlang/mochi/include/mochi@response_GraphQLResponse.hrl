-record(graph_q_l_response, {
    data :: gleam@option:option(gleam@dynamic:dynamic_()),
    errors :: gleam@option:option(list(mochi@error:graph_q_l_error())),
    extensions :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
}).
