-record(lexer, {
    original_source :: binary(),
    source :: binary(),
    byte_offset :: integer(),
    preserve_whitespace :: boolean(),
    preserve_comments :: boolean(),
    mode :: glexer:lexer_mode(),
    newlines :: splitter:splitter()
}).
