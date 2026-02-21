{application, glexer, [
    {vsn, "2.2.1"},
    {applications, [gleam_stdlib]},
    {description, "A lexer for Gleam source code"},
    {modules, [glexer,
               glexer@token,
               glexer_ffi]},
    {registered, []}
]}.
