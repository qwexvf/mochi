-record(unexpected_token, {
    expected :: binary(),
    got :: mochi@lexer:token(),
    position :: mochi@lexer:position()
}).
