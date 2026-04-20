-record(context_pipeline, {
    builders :: list(fun((mochi@context:request_info(), gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
            gleam@dict:dict(binary(), gleam@dynamic:dynamic_())} |
        {error, binary()}))
}).
