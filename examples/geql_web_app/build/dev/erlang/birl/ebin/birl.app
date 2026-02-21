{application, birl, [
    {vsn, "1.8.0"},
    {applications, [gleam_regexp,
                    gleam_stdlib,
                    ranger]},
    {description, "Date / Time handling for Gleam"},
    {modules, [birl,
               birl@duration,
               birl@interval,
               birl@zones,
               birl_ffi]},
    {registered, []}
]}.
