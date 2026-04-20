-record(mutation_payload, {
    successful :: boolean(),
    result :: gleam@option:option(any()),
    messages :: list(mochi@payload:validation_message())
}).
