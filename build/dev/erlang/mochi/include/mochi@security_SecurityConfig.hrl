-record(security_config, {
    max_depth :: gleam@option:option(integer()),
    max_complexity :: gleam@option:option(integer()),
    max_aliases :: gleam@option:option(integer()),
    max_root_fields :: gleam@option:option(integer())
}).
