{application, term_size, [
    {vsn, "1.0.1"},
    {applications, [gleam_stdlib]},
    {description, "Retrieve the terminal's size in rows and columns on all targets"},
    {modules, [term_size,
               term_size_ffi]},
    {registered, []}
]}.
