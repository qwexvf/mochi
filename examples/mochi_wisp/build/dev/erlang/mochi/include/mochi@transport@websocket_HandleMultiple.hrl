-record(handle_multiple, {
    state :: mochi@transport@websocket:connection_state(),
    responses :: list(mochi@transport@websocket:server_message())
}).
