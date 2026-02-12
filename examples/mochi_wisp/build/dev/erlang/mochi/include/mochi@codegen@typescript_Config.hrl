-record(config, {
    use_exports :: boolean(),
    use_interfaces :: boolean(),
    readonly_properties :: boolean(),
    include_helpers :: boolean(),
    header :: gleam@option:option(binary())
}).
