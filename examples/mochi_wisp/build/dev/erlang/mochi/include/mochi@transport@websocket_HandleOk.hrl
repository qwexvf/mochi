-record(handle_ok, {
    state :: mochi@transport@websocket:connection_state(),
    response :: gleam@option:option(mochi@transport@websocket:server_message())
}).
