-record(graph_q_l_request, {
    'query' :: binary(),
    variables :: gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())),
    operation_name :: gleam@option:option(binary())
}).
