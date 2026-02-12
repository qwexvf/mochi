-record(unexpected_token, {
    expected :: binary(),
    got :: mochi@sdl_lexer:s_d_l_token(),
    position :: mochi@sdl_lexer:position()
}).
