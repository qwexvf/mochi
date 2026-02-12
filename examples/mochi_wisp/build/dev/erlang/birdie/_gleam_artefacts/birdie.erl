-module(birdie).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birdie.gleam").
-export([snap/2, main/0]).
-export_type([error/0, new/0, accepted/0, snapshot/1, outcome/0, info_line/0, split/0, answer/0, review_mode/0, review_choice/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type error() :: snapshot_with_empty_title |
    {cannot_create_snapshots_folder, simplifile:file_error()} |
    {cannot_read_accepted_snapshot, simplifile:file_error(), binary()} |
    {cannot_read_new_snapshot, simplifile:file_error(), binary()} |
    {cannot_save_new_snapshot, simplifile:file_error(), binary(), binary()} |
    {cannot_read_snapshots, simplifile:file_error(), binary()} |
    {cannot_reject_snapshot, simplifile:file_error(), binary()} |
    {cannot_accept_snapshot, simplifile:file_error(), binary()} |
    cannot_read_user_input |
    {corrupted_snapshot, binary()} |
    {cannot_find_project_root, simplifile:file_error()} |
    {cannot_create_referenced_file, binary(), simplifile:file_error()} |
    {cannot_read_referenced_file, binary(), simplifile:file_error()} |
    {cannot_mark_snapshot_as_referenced, simplifile:file_error()} |
    {stale_snapshots_found, list(binary())} |
    {cannot_delete_stale_snapshot, simplifile:file_error()} |
    missing_referenced_file |
    {cannot_get_titles, birdie@internal@titles:error()} |
    {cannot_figure_out_project_name, simplifile:file_error()}.

-type new() :: any().

-type accepted() :: any().

-type snapshot(LJO) :: {snapshot,
        binary(),
        binary(),
        gleam@option:option(birdie@internal@titles:test_info())} |
    {gleam_phantom, LJO}.

-type outcome() :: {new_snapshot_created, snapshot(new()), binary()} |
    {different, snapshot(accepted()), snapshot(new())} |
    same.

-type info_line() :: {info_line_with_title, binary(), split(), binary()} |
    {info_line_with_no_title, binary(), split()}.

-type split() :: do_not_split | split_words | truncate.

-type answer() :: yes | no.

-type review_mode() :: show_diff | hide_diff.

-type review_choice() :: accept_snapshot |
    reject_snapshot |
    skip_snapshot |
    toggle_diff_view.

-file("src/birdie.gleam", 120).
-spec get_temp_directory() -> binary().
get_temp_directory() ->
    Temp = begin
        gleam@result:lazy_or(
            envoy_ffi:get(<<"TMPDIR"/utf8>>),
            fun() ->
                gleam@result:lazy_or(
                    envoy_ffi:get(<<"TEMP"/utf8>>),
                    fun() -> envoy_ffi:get(<<"TMP"/utf8>>) end
                )
            end
        )
    end,
    case Temp of
        {ok, Temp@1} ->
            Temp@1;

        {error, _} ->
            case birdie_ffi:is_windows() of
                true ->
                    <<"C:\\TMP"/utf8>>;

                false ->
                    <<"/tmp"/utf8>>
            end
    end.

-file("src/birdie.gleam", 112).
-spec referenced_file_path() -> {ok, binary()} | {error, error()}.
referenced_file_path() ->
    gleam@result:'try'(
        begin
            _pipe = birdie@internal@project:name(),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {cannot_figure_out_project_name, Field@0} end
            )
        end,
        fun(Name) ->
            {ok,
                filepath:join(
                    get_temp_directory(),
                    <<Name/binary, "_referenced.txt"/utf8>>
                )}
        end
    ).

-file("src/birdie.gleam", 94).
?DOC(
    " Returns the path to the referenced file, initialising it to be empty only\n"
    " the first time this function is called.\n"
).
-spec global_referenced_file() -> {ok, binary()} | {error, error()}.
global_referenced_file() ->
    global_value:create_with_unique_name(
        <<"birdie.referenced_file"/utf8>>,
        fun() ->
            gleam@result:'try'(
                referenced_file_path(),
                fun(Referenced_file) ->
                    case simplifile:create_file(Referenced_file) of
                        {ok, _} ->
                            {ok, Referenced_file};

                        {error, eexist} ->
                            _pipe = simplifile:write(
                                Referenced_file,
                                <<""/utf8>>
                            ),
                            _pipe@1 = gleam@result:replace(
                                _pipe,
                                Referenced_file
                            ),
                            gleam@result:map_error(
                                _pipe@1,
                                fun(_capture) ->
                                    {cannot_create_referenced_file,
                                        Referenced_file,
                                        _capture}
                                end
                            );

                        {error, Reason} ->
                            {error,
                                {cannot_create_referenced_file,
                                    Referenced_file,
                                    Reason}}
                    end
                end
            )
        end
    ).

-file("src/birdie.gleam", 144).
?DOC(
    " Finds the snapshots folder at the root of the project the command is run\n"
    " into. If it's not present the folder is created automatically.\n"
).
-spec snapshot_folder() -> {ok, binary()} | {error, error()}.
snapshot_folder() ->
    global_value:create_with_unique_name(
        <<"birdie.snapshot_folder"/utf8>>,
        fun() ->
            Result = gleam@result:map_error(
                birdie@internal@project:find_root(),
                fun(Field@0) -> {cannot_find_project_root, Field@0} end
            ),
            gleam@result:'try'(
                Result,
                fun(Project_root) ->
                    Snapshots_folder = filepath:join(
                        Project_root,
                        <<"birdie_snapshots"/utf8>>
                    ),
                    case simplifile_erl:create_directory(Snapshots_folder) of
                        {ok, nil} ->
                            {ok, Snapshots_folder};

                        {error, eexist} ->
                            {ok, Snapshots_folder};

                        {error, Error} ->
                            {error, {cannot_create_snapshots_folder, Error}}
                    end
                end
            )
        end
    ).

-file("src/birdie.gleam", 276).
-spec validate_snapshot_title(binary()) -> {ok, nil} | {error, error()}.
validate_snapshot_title(Title) ->
    case gleam@string:trim(Title) of
        <<""/utf8>> ->
            {error, snapshot_with_empty_title};

        _ ->
            {ok, nil}
    end.

-file("src/birdie.gleam", 285).
-spec to_diff_lines(snapshot(accepted()), snapshot(new())) -> list(birdie@internal@diff:diff_line()).
to_diff_lines(Accepted, New) ->
    {snapshot, _, Accepted_content, _} = Accepted,
    {snapshot, _, New_content, _} = New,
    birdie@internal@diff:histogram(Accepted_content, New_content).

-file("src/birdie.gleam", 296).
-spec split_n(binary(), integer(), binary()) -> {ok, {list(binary()), binary()}} |
    {error, nil}.
split_n(String, N, Separator) ->
    case N =< 0 of
        true ->
            {ok, {[], String}};

        false ->
            gleam@result:'try'(
                gleam@string:split_once(String, Separator),
                fun(_use0) ->
                    {Line, Rest} = _use0,
                    gleam@result:'try'(
                        split_n(Rest, N - 1, Separator),
                        fun(_use0@1) ->
                            {Lines, Rest@1} = _use0@1,
                            {ok, {[Line | Lines], Rest@1}}
                        end
                    )
                end
            )
    end.

-file("src/birdie.gleam", 374).
-spec trim_end_once(binary(), binary()) -> binary().
trim_end_once(String, Substring) ->
    case gleam_stdlib:string_ends_with(String, Substring) of
        true ->
            gleam@string:drop_end(String, string:length(Substring));

        false ->
            String
    end.

-file("src/birdie.gleam", 366).
?DOC(
    " Birdie started adding newlines to the end of files starting from `1.4.0`,\n"
    " so if we're reading a snapshot created from `1.4.0` onwards then we want to\n"
    " make sure to remove that newline!\n"
).
-spec trim_content(binary(), binary()) -> binary().
trim_content(Content, Version) ->
    Version@2 = case birdie@internal@version:parse(Version) of
        {ok, Version@1} -> Version@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"corrupt birdie version"/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birdie"/utf8>>,
                        function => <<"trim_content"/utf8>>,
                        line => 367,
                        value => _assert_fail,
                        start => 11561,
                        'end' => 11608,
                        pattern_start => 11572,
                        pattern_end => 11583})
    end,
    case birdie@internal@version:compare(
        Version@2,
        birdie@internal@version:new(1, 4, 0)
    ) of
        gt ->
            trim_end_once(Content, <<"\n"/utf8>>);

        eq ->
            trim_end_once(Content, <<"\n"/utf8>>);

        lt ->
            Content
    end.

-file("src/birdie.gleam", 311).
-spec deserialise(binary()) -> {ok, snapshot(any())} | {error, nil}.
deserialise(Raw) ->
    case split_n(Raw, 4, <<"\n"/utf8>>) of
        {ok,
            {[<<"---"/utf8>>,
                    <<"version: "/utf8, Version/binary>>,
                    <<"title: "/utf8, Title/binary>>,
                    <<"---"/utf8>>],
                Content}} ->
            {ok,
                {snapshot,
                    gleam@string:trim(Title),
                    trim_content(Content, Version),
                    none}};

        {ok,
            {[<<"---\r"/utf8>>,
                    <<"version: "/utf8, Version/binary>>,
                    <<"title: "/utf8, Title/binary>>,
                    <<"---\r"/utf8>>],
                Content}} ->
            {ok,
                {snapshot,
                    gleam@string:trim(Title),
                    trim_content(Content, Version),
                    none}};

        {ok, _} ->
            case split_n(Raw, 6, <<"\n"/utf8>>) of
                {ok,
                    {[<<"---"/utf8>>,
                            <<"version: "/utf8, Version@1/binary>>,
                            <<"title: "/utf8, Title@1/binary>>,
                            <<"file: "/utf8, File/binary>>,
                            <<"test_name: "/utf8, Test_name/binary>>,
                            <<"---"/utf8>>],
                        Content@1}} ->
                    {ok,
                        {snapshot,
                            gleam@string:trim(Title@1),
                            trim_content(Content@1, Version@1),
                            {some,
                                {test_info,
                                    gleam@string:trim(File),
                                    gleam@string:trim(Test_name)}}}};

                {ok,
                    {[<<"---\r"/utf8>>,
                            <<"version: "/utf8, Version@1/binary>>,
                            <<"title: "/utf8, Title@1/binary>>,
                            <<"file: "/utf8, File/binary>>,
                            <<"test_name: "/utf8, Test_name/binary>>,
                            <<"---\r"/utf8>>],
                        Content@1}} ->
                    {ok,
                        {snapshot,
                            gleam@string:trim(Title@1),
                            trim_content(Content@1, Version@1),
                            {some,
                                {test_info,
                                    gleam@string:trim(File),
                                    gleam@string:trim(Test_name)}}}};

                {ok, _} ->
                    {error, nil};

                {error, _} ->
                    {error, nil}
            end;

        {error, _} ->
            case split_n(Raw, 6, <<"\n"/utf8>>) of
                {ok,
                    {[<<"---"/utf8>>,
                            <<"version: "/utf8, Version@1/binary>>,
                            <<"title: "/utf8, Title@1/binary>>,
                            <<"file: "/utf8, File/binary>>,
                            <<"test_name: "/utf8, Test_name/binary>>,
                            <<"---"/utf8>>],
                        Content@1}} ->
                    {ok,
                        {snapshot,
                            gleam@string:trim(Title@1),
                            trim_content(Content@1, Version@1),
                            {some,
                                {test_info,
                                    gleam@string:trim(File),
                                    gleam@string:trim(Test_name)}}}};

                {ok,
                    {[<<"---\r"/utf8>>,
                            <<"version: "/utf8, Version@1/binary>>,
                            <<"title: "/utf8, Title@1/binary>>,
                            <<"file: "/utf8, File/binary>>,
                            <<"test_name: "/utf8, Test_name/binary>>,
                            <<"---\r"/utf8>>],
                        Content@1}} ->
                    {ok,
                        {snapshot,
                            gleam@string:trim(Title@1),
                            trim_content(Content@1, Version@1),
                            {some,
                                {test_info,
                                    gleam@string:trim(File),
                                    gleam@string:trim(Test_name)}}}};

                {ok, _} ->
                    {error, nil};

                {error, _} ->
                    {error, nil}
            end
    end.

-file("src/birdie.gleam", 435).
?DOC(" Read an accepted snapshot which might be missing.\n").
-spec read_accepted(binary()) -> {ok, gleam@option:option(snapshot(accepted()))} |
    {error, error()}.
read_accepted(Source) ->
    case simplifile:read(Source) of
        {ok, Content} ->
            case deserialise(Content) of
                {ok, Snapshot} ->
                    {ok, {some, Snapshot}};

                {error, nil} ->
                    {error, {corrupted_snapshot, Source}}
            end;

        {error, enoent} ->
            {ok, none};

        {error, Reason} ->
            {error, {cannot_read_accepted_snapshot, Reason, Source}}
    end.

-file("src/birdie.gleam", 455).
?DOC(
    " Read a new snapshot.\n"
    "\n"
    " > ℹ️ Notice the different return type compared to `read_accepted`: when we\n"
    " > try to read a new snapshot we are sure it's there (because we've listed\n"
    " > the directory or something else) so if it's not present that's an error\n"
    " > and we don't return an `Ok(None)`.\n"
).
-spec read_new(binary()) -> {ok, snapshot(new())} | {error, error()}.
read_new(Source) ->
    case simplifile:read(Source) of
        {ok, Content} ->
            gleam@result:replace_error(
                deserialise(Content),
                {corrupted_snapshot, Source}
            );

        {error, Reason} ->
            {error, {cannot_read_new_snapshot, Reason, Source}}
    end.

-file("src/birdie.gleam", 557).
-spec reject_snapshot(binary()) -> {ok, nil} | {error, error()}.
reject_snapshot(New_snapshot_path) ->
    _pipe = simplifile_erl:delete(New_snapshot_path),
    gleam@result:map_error(
        _pipe,
        fun(_capture) ->
            {cannot_reject_snapshot, _capture, New_snapshot_path}
        end
    ).

-file("src/birdie.gleam", 567).
?DOC(
    " Turns a snapshot's title into a file name stripping it of all dangerous\n"
    " characters (or at least those I could think ok 😁).\n"
).
-spec file_name(binary()) -> binary().
file_name(Title) ->
    _pipe = gleam@string:replace(Title, <<"/"/utf8>>, <<" "/utf8>>),
    _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<" "/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\n"/utf8>>, <<" "/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\t"/utf8>>, <<" "/utf8>>),
    _pipe@4 = gleam@string:replace(_pipe@3, <<"\r"/utf8>>, <<" "/utf8>>),
    _pipe@5 = gleam@string:replace(_pipe@4, <<"."/utf8>>, <<" "/utf8>>),
    _pipe@6 = gleam@string:replace(_pipe@5, <<":"/utf8>>, <<" "/utf8>>),
    justin:snake_case(_pipe@6).

-file("src/birdie.gleam", 752).
-spec to_function_name(binary(), binary()) -> binary().
to_function_name(File, Function_name) ->
    Module_name = case File of
        <<"./test/"/utf8, Rest/binary>> ->
            filepath:strip_extension(Rest);

        _ ->
            filepath:strip_extension(File)
    end,
    <<<<<<Module_name/binary, ".{"/utf8>>/binary, Function_name/binary>>/binary,
        "}"/utf8>>.

-file("src/birdie.gleam", 594).
-spec explain(error()) -> binary().
explain(Error) ->
    Heading = fun(Reason) ->
        <<<<"["/utf8,
                (gleam_community@ansi:bold(gleam@string:inspect(Reason)))/binary>>/binary,
            "] "/utf8>>
    end,
    Message = case Error of
        snapshot_with_empty_title ->
            <<"a snapshot cannot have the empty string as a title."/utf8>>;

        {cannot_create_snapshots_folder, Reason@1} ->
            <<(Heading(Reason@1))/binary,
                "I couldn't create the snapshots folder."/utf8>>;

        {cannot_read_accepted_snapshot, Reason@2, Source} ->
            <<<<(Heading(Reason@2))/binary,
                    "I couldn't read the accepted snapshot from "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, Source/binary>>/binary, "\"."/utf8>>
                ))/binary>>;

        {cannot_read_new_snapshot, Reason@3, Source@1} ->
            <<<<(Heading(Reason@3))/binary,
                    "I couldn't read the new snapshot from "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, Source@1/binary>>/binary, "\"."/utf8>>
                ))/binary>>;

        {cannot_save_new_snapshot, Reason@4, Title, Destination} ->
            <<<<<<<<(Heading(Reason@4))/binary,
                            "I couldn't save the snapshot "/utf8>>/binary,
                        (gleam_community@ansi:italic(
                            <<<<"\""/utf8, Title/binary>>/binary, "\" "/utf8>>
                        ))/binary>>/binary,
                    "to "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, Destination/binary>>/binary, "\"."/utf8>>
                ))/binary>>;

        {cannot_read_snapshots, Reason@5, _} ->
            <<(Heading(Reason@5))/binary,
                "I couldn't read the snapshots folder's contents."/utf8>>;

        {cannot_reject_snapshot, Reason@6, Snapshot} ->
            <<<<(Heading(Reason@6))/binary,
                    "I couldn't reject the snapshot "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, Snapshot/binary>>/binary, "\"."/utf8>>
                ))/binary>>;

        {cannot_accept_snapshot, Reason@7, Snapshot@1} ->
            <<<<(Heading(Reason@7))/binary,
                    "I couldn't accept the snapshot "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, Snapshot@1/binary>>/binary, "\"."/utf8>>
                ))/binary>>;

        cannot_read_user_input ->
            <<"I couldn't read the user input."/utf8>>;

        {corrupted_snapshot, Source@2} ->
            <<<<<<<<"It looks like "/utf8,
                            (gleam_community@ansi:italic(
                                <<<<"\""/utf8, Source@2/binary>>/binary,
                                    "\"\n"/utf8>>
                            ))/binary>>/binary,
                        "is not a valid snapshot.\n"/utf8>>/binary,
                    "This might happen when someone modifies its content.\n"/utf8>>/binary,
                "Try deleting the snapshot and recreating it."/utf8>>;

        {cannot_create_referenced_file, File, eacces} ->
            <<<<<<<<<<"I don't have the required permission to create the file used to track\n"/utf8,
                                ((<<<<"stale snapshots at: `"/utf8,
                                        File/binary>>/binary,
                                    "`.\n"/utf8>>))/binary>>/binary,
                            "This usually happens when the current user doesn't have a write\n"/utf8>>/binary,
                        "permission for the system's temporary directory.\n\n"/utf8>>/binary,
                    "Hint: you can set the $TEMP environment variable to make me use a\n"/utf8>>/binary,
                "different directory to write the reference file in."/utf8>>;

        {cannot_read_referenced_file, File@1, eacces} ->
            <<<<<<<<<<"I don't have the required permission to read the file used to track\n"/utf8,
                                ((<<<<"stale snapshots at: `"/utf8,
                                        File@1/binary>>/binary,
                                    "`.\n"/utf8>>))/binary>>/binary,
                            "This usually happens when the current user doesn't have a write\n"/utf8>>/binary,
                        "permission for the system's temporary directory.\n\n"/utf8>>/binary,
                    "Hint: you can set the $TEMP environment variable to make me use a\n"/utf8>>/binary,
                "different directory to write the reference file in."/utf8>>;

        {cannot_create_referenced_file, _, Reason@8} ->
            <<(Heading(Reason@8))/binary,
                "I couldn't create the file used to track stale snapshots."/utf8>>;

        {cannot_read_referenced_file, _, Reason@9} ->
            <<(Heading(Reason@9))/binary,
                "I couldn't read the file used to track stale snapshots."/utf8>>;

        {cannot_mark_snapshot_as_referenced, Reason@10} ->
            <<(Heading(Reason@10))/binary,
                "I couldn't write to the file used to track stale snapshots."/utf8>>;

        {cannot_find_project_root, Reason@11} ->
            <<<<(Heading(Reason@11))/binary,
                    "I couldn't locate the project's root where the snapshot's"/utf8>>/binary,
                " folder should be."/utf8>>;

        {cannot_get_titles, {cannot_find_project_root, Reason@11}} ->
            <<<<(Heading(Reason@11))/binary,
                    "I couldn't locate the project's root where the snapshot's"/utf8>>/binary,
                " folder should be."/utf8>>;

        missing_referenced_file ->
            <<<<"I couldn't find any information about stale snapshots.\n"/utf8,
                    "Remember you have to run `gleam test` first, so I can find any stale "/utf8>>/binary,
                "snapshot."/utf8>>;

        {stale_snapshots_found, Stale_snapshots} ->
            Titles = begin
                _pipe = gleam@list:map(
                    Stale_snapshots,
                    fun(Snapshot@2) ->
                        <<"  - "/utf8,
                            (filepath:strip_extension(Snapshot@2))/binary>>
                    end
                ),
                gleam@string:join(_pipe, <<"\n"/utf8>>)
            end,
            <<<<<<<<<<"I found the following stale snapshots:\n\n"/utf8,
                                Titles/binary>>/binary,
                            "\n\n"/utf8>>/binary,
                        "These snapshots were not referenced by any snapshot test during the "/utf8>>/binary,
                    "last `gleam test`\n"/utf8>>/binary,
                "Hint: run `gleam run -m birdie stale delete` to delete them"/utf8>>;

        {cannot_delete_stale_snapshot, Reason@12} ->
            <<(Heading(Reason@12))/binary,
                "I couldn't delete one of the stale snapshots."/utf8>>;

        {cannot_get_titles, {cannot_read_test_directory, Reason@13}} ->
            <<(Heading(Reason@13))/binary,
                "I couldn't list the contents of the test folder."/utf8>>;

        {cannot_figure_out_project_name, Reason@14} ->
            <<(Heading(Reason@14))/binary,
                "I couldn't figure out the current project's name."/utf8>>;

        {cannot_get_titles, {cannot_read_test_file, Reason@15, File@2}} ->
            <<<<(Heading(Reason@15))/binary,
                    "I couldn't read the test file "/utf8>>/binary,
                (gleam_community@ansi:italic(
                    <<<<"\""/utf8, File@2/binary>>/binary, "\"\n"/utf8>>
                ))/binary>>;

        {cannot_get_titles,
            {duplicate_literal_titles,
                Title@1,
                {test_info, One_file, One_test_name},
                {test_info, Other_file, Other_test_name}}} ->
            Same_file = One_file =:= Other_file,
            Same_function = One_test_name =:= Other_test_name,
            Location = case {Same_file, Same_function} of
                {true, true} ->
                    <<"Both tests are defined in:\n\n  "/utf8,
                        (gleam_community@ansi:italic(
                            to_function_name(One_file, One_test_name)
                        ))/binary>>;

                {_, _} ->
                    <<<<<<"One test is defined in:\n\n  "/utf8,
                                (gleam_community@ansi:italic(
                                    to_function_name(One_file, One_test_name)
                                ))/binary>>/binary,
                            "\n\nWhile the other is defined in:\n\n  "/utf8>>/binary,
                        (gleam_community@ansi:italic(
                            to_function_name(Other_file, Other_test_name)
                        ))/binary>>
            end,
            <<<<<<<<<<"it looks like there's some snapshot tests sharing the same title:

  "/utf8,
                                (gleam_community@ansi:italic(
                                    <<<<"\""/utf8, Title@1/binary>>/binary,
                                        "\""/utf8>>
                                ))/binary>>/binary,
                            "

Snapshot titles "/utf8>>/binary,
                        (gleam_community@ansi:bold(<<"must be unique"/utf8>>))/binary>>/binary,
                    " or you would run into strange diffs
when reviewing them, try changing one of those.
"/utf8>>/binary,
                Location/binary>>;

        {cannot_get_titles, {overlapping_prefixes, _, _, _, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Prefixes are not implemented yet"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"explain"/utf8>>,
                    line => 743});

        {cannot_get_titles, {prefix_overlapping_with_literal_title, _, _, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Prefixes are not implemented yet"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"explain"/utf8>>,
                    line => 746})
    end,
    Message.

-file("src/birdie.gleam", 772).
-spec snapshot_default_lines(snapshot(any())) -> list(info_line()).
snapshot_default_lines(Snapshot) ->
    {snapshot, Title, _, Info} = Snapshot,
    case Info of
        none ->
            [{info_line_with_title, Title, split_words, <<"title"/utf8>>}];

        {some, {test_info, File, Test_name}} ->
            [{info_line_with_title, Title, split_words, <<"title"/utf8>>},
                {info_line_with_title, File, truncate, <<"file"/utf8>>},
                {info_line_with_title, Test_name, truncate, <<"name"/utf8>>}]
    end.

-file("src/birdie.gleam", 851).
-spec count_digits_loop(integer(), integer()) -> integer().
count_digits_loop(Number, Digits) ->
    case Number < 10 of
        true ->
            1 + Digits;

        false ->
            count_digits_loop(Number div 10, 1 + Digits)
    end.

-file("src/birdie.gleam", 847).
-spec count_digits(integer()) -> integer().
count_digits(Number) ->
    count_digits_loop(gleam@int:absolute_value(Number), 0).

-file("src/birdie.gleam", 919).
-spec pretty_diff_line(
    birdie@internal@diff:diff_line(),
    integer(),
    fun((binary()) -> binary())
) -> binary().
pretty_diff_line(Diff_line, Padding, Shared_line_style) ->
    {diff_line, Number, Line, Kind} = Diff_line,
    {Pretty_number, Pretty_line, Separator} = case Kind of
        shared ->
            {begin
                    _pipe = erlang:integer_to_binary(Number),
                    _pipe@1 = gleam@string:pad_start(
                        _pipe,
                        Padding - 1,
                        <<" "/utf8>>
                    ),
                    gleam_community@ansi:dim(_pipe@1)
                end,
                Shared_line_style(Line),
                <<" │ "/utf8>>};

        new ->
            {begin
                    _pipe@2 = erlang:integer_to_binary(Number),
                    _pipe@3 = gleam@string:pad_start(
                        _pipe@2,
                        Padding - 1,
                        <<" "/utf8>>
                    ),
                    _pipe@4 = gleam_community@ansi:green(_pipe@3),
                    gleam_community@ansi:bold(_pipe@4)
                end,
                gleam_community@ansi:green(Line),
                gleam_community@ansi:green(<<" + "/utf8>>)};

        old ->
            Number@1 = begin
                _pipe@5 = (<<" "/utf8,
                    (erlang:integer_to_binary(Number))/binary>>),
                gleam@string:pad_end(_pipe@5, Padding - 1, <<" "/utf8>>)
            end,
            {gleam_community@ansi:red(Number@1),
                gleam_community@ansi:red(Line),
                gleam_community@ansi:red(<<" - "/utf8>>)}
    end,
    <<<<Pretty_number/binary, Separator/binary>>/binary, Pretty_line/binary>>.

-file("src/birdie.gleam", 957).
-spec truncate(binary(), integer()) -> binary().
truncate(String, Max_length) ->
    case string:length(String) > Max_length of
        false ->
            String;

        true ->
            _pipe = gleam@string:to_graphemes(String),
            _pipe@1 = gleam@list:take(_pipe, Max_length - 3),
            _pipe@2 = gleam@string:join(_pipe@1, <<""/utf8>>),
            gleam@string:append(_pipe@2, <<"..."/utf8>>)
    end.

-file("src/birdie.gleam", 975).
-spec do_to_lines(
    list(binary()),
    binary(),
    integer(),
    list(binary()),
    integer()
) -> list(binary()).
do_to_lines(Lines, Line, Line_length, Words, Max_length) ->
    case Words of
        [] ->
            case Line =:= <<""/utf8>> of
                true ->
                    lists:reverse(Lines);

                false ->
                    lists:reverse([Line | Lines])
            end;

        [Word | Rest] ->
            Word_length = string:length(Word),
            New_line_length = (Word_length + Line_length) + 1,
            case New_line_length > Max_length of
                true ->
                    do_to_lines(
                        [Line | Lines],
                        <<""/utf8>>,
                        0,
                        Words,
                        Max_length
                    );

                false ->
                    New_line = case Line of
                        <<""/utf8>> ->
                            Word;

                        _ ->
                            <<<<Line/binary, " "/utf8>>/binary, Word/binary>>
                    end,
                    do_to_lines(
                        Lines,
                        New_line,
                        New_line_length,
                        Rest,
                        Max_length
                    )
            end
    end.

-file("src/birdie.gleam", 968).
-spec to_lines(binary(), integer()) -> list(binary()).
to_lines(String, Max_length) ->
    gleam@list:flat_map(
        gleam@string:split(String, <<"\n"/utf8>>),
        fun(Line) ->
            Words = gleam@string:split(Line, <<" "/utf8>>),
            do_to_lines([], <<""/utf8>>, 0, Words, Max_length)
        end
    ).

-file("src/birdie.gleam", 896).
-spec pretty_info_line(info_line(), integer()) -> binary().
pretty_info_line(Line, Width) ->
    {Prefix, Prefix_length} = case Line of
        {info_line_with_no_title, _, _} ->
            {<<"  "/utf8>>, 2};

        {info_line_with_title, _, _, Title} ->
            {<<"  "/utf8,
                    (gleam_community@ansi:blue(<<Title/binary, ": "/utf8>>))/binary>>,
                string:length(Title) + 4}
    end,
    case erlang:element(3, Line) of
        truncate ->
            <<Prefix/binary,
                (truncate(erlang:element(2, Line), Width - Prefix_length))/binary>>;

        do_not_split ->
            <<Prefix/binary, (erlang:element(2, Line))/binary>>;

        split_words ->
            case to_lines(erlang:element(2, Line), Width - Prefix_length) of
                [] ->
                    Prefix;

                [Line@1 | Lines] ->
                    gleam@list:fold(
                        Lines,
                        <<Prefix/binary, Line@1/binary>>,
                        fun(Acc, Line@2) ->
                            <<<<<<Acc/binary, "\n"/utf8>>/binary,
                                    (gleam@string:repeat(
                                        <<" "/utf8>>,
                                        Prefix_length
                                    ))/binary>>/binary,
                                Line@2/binary>>
                        end
                    )
            end
    end.

-file("src/birdie.gleam", 1266).
-spec toggle_mode(review_mode()) -> review_mode().
toggle_mode(Mode) ->
    case Mode of
        show_diff ->
            hide_diff;

        hide_diff ->
            show_diff
    end.

-file("src/birdie.gleam", 1439).
-spec terminal_width() -> integer().
terminal_width() ->
    case term_size_ffi:terminal_size() of
        {ok, {_, Columns}} ->
            Columns;

        {error, _} ->
            80
    end.

-file("src/birdie.gleam", 858).
-spec pretty_box(
    binary(),
    list(birdie@internal@diff:diff_line()),
    list(info_line()),
    fun((binary()) -> binary())
) -> binary().
pretty_box(Title, Content_lines, Info_lines, Shared_line_style) ->
    Width = terminal_width(),
    Lines_count = erlang:length(Content_lines) + 1,
    Padding = (count_digits(Lines_count) * 2) + 5,
    Title_length = string:length(Title),
    Title_line_right = gleam@string:repeat(
        <<"─"/utf8>>,
        (Width - 5) - Title_length
    ),
    Title_line = <<<<<<"── "/utf8, Title/binary>>/binary, " ─"/utf8>>/binary,
        Title_line_right/binary>>,
    Info_lines@1 = begin
        _pipe = gleam@list:map(
            Info_lines,
            fun(_capture) -> pretty_info_line(_capture, Width) end
        ),
        gleam@string:join(_pipe, <<"\n"/utf8>>)
    end,
    Content = begin
        _pipe@1 = gleam@list:map(
            Content_lines,
            fun(_capture@1) ->
                pretty_diff_line(_capture@1, Padding, Shared_line_style)
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    Left_padding_line = gleam@string:repeat(<<"─"/utf8>>, Padding),
    Right_padding_line = gleam@string:repeat(
        <<"─"/utf8>>,
        (Width - Padding) - 1
    ),
    Open_line = <<<<Left_padding_line/binary, "┬"/utf8>>/binary,
        Right_padding_line/binary>>,
    Closed_line = <<<<Left_padding_line/binary, "┴"/utf8>>/binary,
        Right_padding_line/binary>>,
    _pipe@2 = [Title_line,
        <<""/utf8>>,
        Info_lines@1,
        <<""/utf8>>,
        Open_line,
        Content,
        Closed_line],
    gleam@string:join(_pipe@2, <<"\n"/utf8>>).

-file("src/birdie.gleam", 784).
-spec new_snapshot_box(snapshot(new()), list(info_line())) -> binary().
new_snapshot_box(Snapshot, Additional_info_lines) ->
    {snapshot, _, Content, _} = Snapshot,
    Content@1 = begin
        _pipe = gleam@string:split(Content, <<"\n"/utf8>>),
        gleam@list:index_map(
            _pipe,
            fun(Line, I) -> {diff_line, I + 1, Line, new} end
        )
    end,
    pretty_box(
        <<"new snapshot"/utf8>>,
        Content@1,
        lists:append([snapshot_default_lines(Snapshot), Additional_info_lines]),
        fun(Shared_line) -> Shared_line end
    ).

-file("src/birdie.gleam", 804).
-spec diff_snapshot_box(
    snapshot(accepted()),
    snapshot(new()),
    list(info_line())
) -> binary().
diff_snapshot_box(Accepted, New, Additional_info_lines) ->
    pretty_box(
        <<"mismatched snapshots"/utf8>>,
        to_diff_lines(Accepted, New),
        begin
            _pipe = [snapshot_default_lines(Accepted),
                Additional_info_lines,
                [{info_line_with_no_title, <<""/utf8>>, do_not_split},
                    {info_line_with_no_title,
                        gleam_community@ansi:red(<<"- old snapshot"/utf8>>),
                        do_not_split},
                    {info_line_with_no_title,
                        gleam_community@ansi:green(<<"+ new snapshot"/utf8>>),
                        do_not_split}]],
            lists:append(_pipe)
        end,
        fun(Shared_line) -> gleam_community@ansi:dim(Shared_line) end
    ).

-file("src/birdie.gleam", 826).
-spec regular_snapshot_box(snapshot(new()), list(info_line())) -> binary().
regular_snapshot_box(New, Additional_info_lines) ->
    {snapshot, _, Content, _} = New,
    Content@1 = begin
        _pipe = gleam@string:split(Content, <<"\n"/utf8>>),
        gleam@list:index_map(
            _pipe,
            fun(Line, I) -> {diff_line, I + 1, Line, shared} end
        )
    end,
    pretty_box(
        <<"mismatched snapshots"/utf8>>,
        Content@1,
        begin
            _pipe@1 = [snapshot_default_lines(New), Additional_info_lines],
            lists:append(_pipe@1)
        end,
        fun(Shared_line) -> Shared_line end
    ).

-file("src/birdie.gleam", 1451).
?DOC(
    " Replaces the first occurrence of an element in the list with the given\n"
    " replacement.\n"
).
-spec replace_first(list(LMY), LMY, LMY) -> list(LMY).
replace_first(List, Item, Replacement) ->
    case List of
        [] ->
            [];

        [First | Rest] when First =:= Item ->
            [Replacement | Rest];

        [First@1 | Rest@1] ->
            [First@1 | replace_first(Rest@1, Item, Replacement)]
    end.

-file("src/birdie.gleam", 1461).
?DOC(" Clear the screen.\n").
-spec clear() -> nil.
clear() ->
    gleam_stdlib:print(<<"\x{1b}c"/utf8>>),
    gleam_stdlib:print(<<"\x{1b}[H\x{1b}[J"/utf8>>).

-file("src/birdie.gleam", 1468).
?DOC(" Move the cursor up a given number of lines.\n").
-spec cursor_up(integer()) -> nil.
cursor_up(N) ->
    gleam_stdlib:print(
        <<<<"\x{1b}["/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
            "A"/utf8>>
    ).

-file("src/birdie.gleam", 1474).
?DOC(" Clear the line the cursor is currently on.\n").
-spec clear_line() -> nil.
clear_line() ->
    gleam_stdlib:print(<<"\x{1b}[2K"/utf8>>).

-file("src/birdie.gleam", 1426).
-spec report_status({ok, nil} | {error, error()}) -> nil.
report_status(Result) ->
    case Result of
        {ok, nil} ->
            gleam_stdlib:println(gleam_community@ansi:green(<<"🐦‍⬛ Done!"/utf8>>)),
            erlang:halt(0);

        {error, Error} ->
            gleam_stdlib:println_error(
                <<(gleam_community@ansi:red(<<"Error: "/utf8>>))/binary,
                    (explain(Error))/binary>>
            ),
            erlang:halt(1)
    end.

-file("src/birdie.gleam", 1081).
-spec ask_yes_or_no(binary()) -> answer().
ask_yes_or_no(Prompt) ->
    case birdie_ffi:get_line(<<Prompt/binary, " [Y/n] "/utf8>>) of
        {error, _} ->
            no;

        {ok, Line} ->
            case begin
                _pipe = string:lowercase(Line),
                gleam@string:trim(_pipe)
            end of
                <<"yes"/utf8>> ->
                    yes;

                <<"y"/utf8>> ->
                    yes;

                <<""/utf8>> ->
                    yes;

                _ ->
                    no
            end
    end.

-file("src/birdie.gleam", 1286).
?DOC(
    " Asks the user to make a choice: it first prints a reminder of the options\n"
    " and waits for the user to choose one.\n"
    " Will prompt again if the choice is not amongst the possible options.\n"
).
-spec ask_choice(review_mode()) -> {ok, review_choice()} | {error, error()}.
ask_choice(Mode) ->
    Diff_message = case Mode of
        hide_diff ->
            <<" show diff  "/utf8>>;

        show_diff ->
            <<" hide diff  "/utf8>>
    end,
    gleam_stdlib:println(
        <<<<<<((<<<<(gleam_community@ansi:bold(
                                gleam_community@ansi:green(<<"  a"/utf8>>)
                            ))/binary,
                            " accept     "/utf8>>/binary,
                        (gleam_community@ansi:dim(
                            <<"accept the new snapshot\n"/utf8>>
                        ))/binary>>))/binary,
                    ((<<<<(gleam_community@ansi:bold(
                                gleam_community@ansi:red(<<"  r"/utf8>>)
                            ))/binary,
                            " reject     "/utf8>>/binary,
                        (gleam_community@ansi:dim(
                            <<"reject the new snapshot\n"/utf8>>
                        ))/binary>>))/binary>>/binary,
                ((<<<<(gleam_community@ansi:bold(
                            gleam_community@ansi:yellow(<<"  s"/utf8>>)
                        ))/binary,
                        " skip       "/utf8>>/binary,
                    (gleam_community@ansi:dim(
                        <<"skip the snapshot for now\n"/utf8>>
                    ))/binary>>))/binary>>/binary,
            ((<<<<(gleam_community@ansi:bold(
                        gleam_community@ansi:cyan(<<"  d"/utf8>>)
                    ))/binary,
                    Diff_message/binary>>/binary,
                (gleam_community@ansi:dim(<<"toggle snapshot diff\n"/utf8>>))/binary>>))/binary>>
    ),
    clear_line(),
    case gleam@result:map(
        birdie_ffi:get_line(<<"> "/utf8>>),
        fun gleam@string:trim/1
    ) of
        {ok, <<"a"/utf8>>} ->
            {ok, accept_snapshot};

        {ok, <<"r"/utf8>>} ->
            {ok, reject_snapshot};

        {ok, <<"s"/utf8>>} ->
            {ok, skip_snapshot};

        {ok, <<"d"/utf8>>} ->
            {ok, toggle_diff_view};

        {ok, _} ->
            cursor_up(6),
            ask_choice(Mode);

        {error, _} ->
            {error, cannot_read_user_input}
    end.

-file("src/birdie.gleam", 381).
-spec serialise(snapshot(new())) -> binary().
serialise(Snapshot) ->
    {snapshot, Title, Content, Info} = Snapshot,
    Info_lines = case Info of
        none ->
            [];

        {some, {test_info, File, Test_name}} ->
            [<<"file: "/utf8, File/binary>>,
                <<"test_name: "/utf8, Test_name/binary>>]
    end,
    _pipe = [[<<"---"/utf8>>,
            <<"version: "/utf8, "1.5.4"/utf8>>,
            <<"title: "/utf8,
                (gleam@string:replace(Title, <<"\n"/utf8>>, <<"\\n"/utf8>>))/binary>>],
        Info_lines,
        [<<"---"/utf8>>, Content]],
    _pipe@1 = lists:append(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<"\n"/utf8>>),
    gleam@string:append(_pipe@2, <<"\n"/utf8>>).

-file("src/birdie.gleam", 415).
?DOC(" Save a new snapshot to a given path.\n").
-spec save(snapshot(new()), binary()) -> {ok, nil} | {error, error()}.
save(Snapshot, Destination) ->
    case gleam_stdlib:string_ends_with(Destination, <<".new"/utf8>>) of
        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Looks like I've messed up something, all new snapshots should have the `.new` extension"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"save"/utf8>>,
                    line => 421});

        true ->
            _pipe = simplifile:write(Destination, serialise(Snapshot)),
            gleam@result:map_error(
                _pipe,
                fun(_capture) ->
                    {cannot_save_new_snapshot,
                        _capture,
                        erlang:element(2, Snapshot),
                        Destination}
                end
            )
    end.

-file("src/birdie.gleam", 486).
?DOC(
    " List all the accepted snapshots in a folder. Every file is automatically\n"
    " prepended with the folder so you get the full path of each file.\n"
).
-spec list_accepted_snapshots(binary()) -> {ok, list(binary())} |
    {error, error()}.
list_accepted_snapshots(Folder) ->
    case simplifile_erl:read_directory(Folder) of
        {error, Reason} ->
            {error, {cannot_read_snapshots, Reason, Folder}};

        {ok, Files} ->
            {ok,
                begin
                    gleam@list:filter_map(
                        Files,
                        fun(File) -> case filepath:extension(File) of
                                {ok, Extension} when Extension =:= <<"accepted"/utf8>> ->
                                    {ok, filepath:join(Folder, File)};

                                _ ->
                                    {error, nil}
                            end end
                    )
                end}
    end.

-file("src/birdie.gleam", 587).
?DOC(
    " Turns a new snapshot path into the path of the corresponding accepted\n"
    " snapshot.\n"
).
-spec to_accepted_path(binary()) -> binary().
to_accepted_path(File) ->
    <<<<(filepath:strip_extension(File))/binary, "."/utf8>>/binary,
        "accepted"/utf8>>.

-file("src/birdie.gleam", 503).
-spec accept_snapshot(binary(), birdie@internal@titles:titles()) -> {ok, nil} |
    {error, error()}.
accept_snapshot(New_snapshot_path, Titles) ->
    gleam@result:'try'(
        read_new(New_snapshot_path),
        fun(Snapshot) ->
            {snapshot, Title, Content, _} = Snapshot,
            Accepted_snapshot_path = to_accepted_path(New_snapshot_path),
            gleam@result:'try'(
                referenced_file_path(),
                fun(Referenced_file) ->
                    gleam@result:'try'(
                        case simplifile_erl:is_file(Referenced_file) of
                            {ok, _} ->
                                {ok, nil};

                            {error, _} ->
                                _pipe = simplifile:create_file(Referenced_file),
                                gleam@result:map_error(
                                    _pipe,
                                    fun(_capture) ->
                                        {cannot_create_referenced_file,
                                            Referenced_file,
                                            _capture}
                                    end
                                )
                        end,
                        fun(_) ->
                            gleam@result:'try'(
                                begin
                                    _pipe@1 = simplifile:append(
                                        Referenced_file,
                                        <<(filepath:base_name(
                                                Accepted_snapshot_path
                                            ))/binary,
                                            "\n"/utf8>>
                                    ),
                                    gleam@result:map_error(
                                        _pipe@1,
                                        fun(Field@0) -> {cannot_mark_snapshot_as_referenced, Field@0} end
                                    )
                                end,
                                fun(_) ->
                                    case birdie@internal@titles:find(
                                        Titles,
                                        Title
                                    ) of
                                        {ok, {literal, Info}} ->
                                            Delete_new_snapshot = begin
                                                _pipe@2 = simplifile_erl:delete(
                                                    New_snapshot_path
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@2,
                                                    fun(_capture@1) ->
                                                        {cannot_accept_snapshot,
                                                            _capture@1,
                                                            New_snapshot_path}
                                                    end
                                                )
                                            end,
                                            gleam@result:'try'(
                                                Delete_new_snapshot,
                                                fun(_) ->
                                                    _pipe@3 = {snapshot,
                                                        Title,
                                                        Content,
                                                        {some, Info}},
                                                    _pipe@4 = serialise(_pipe@3),
                                                    _pipe@5 = simplifile:write(
                                                        Accepted_snapshot_path,
                                                        _pipe@4
                                                    ),
                                                    gleam@result:map_error(
                                                        _pipe@5,
                                                        fun(_capture@2) ->
                                                            {cannot_accept_snapshot,
                                                                _capture@2,
                                                                Accepted_snapshot_path}
                                                        end
                                                    )
                                                end
                                            );

                                        {ok, {prefix, Info, _}} ->
                                            Delete_new_snapshot = begin
                                                _pipe@2 = simplifile_erl:delete(
                                                    New_snapshot_path
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@2,
                                                    fun(_capture@1) ->
                                                        {cannot_accept_snapshot,
                                                            _capture@1,
                                                            New_snapshot_path}
                                                    end
                                                )
                                            end,
                                            gleam@result:'try'(
                                                Delete_new_snapshot,
                                                fun(_) ->
                                                    _pipe@3 = {snapshot,
                                                        Title,
                                                        Content,
                                                        {some, Info}},
                                                    _pipe@4 = serialise(_pipe@3),
                                                    _pipe@5 = simplifile:write(
                                                        Accepted_snapshot_path,
                                                        _pipe@4
                                                    ),
                                                    gleam@result:map_error(
                                                        _pipe@5,
                                                        fun(_capture@2) ->
                                                            {cannot_accept_snapshot,
                                                                _capture@2,
                                                                Accepted_snapshot_path}
                                                        end
                                                    )
                                                end
                                            );

                                        {error, _} ->
                                            _pipe@6 = simplifile_erl:rename_file(
                                                New_snapshot_path,
                                                Accepted_snapshot_path
                                            ),
                                            gleam@result:map_error(
                                                _pipe@6,
                                                fun(_capture@3) ->
                                                    {cannot_accept_snapshot,
                                                        _capture@3,
                                                        New_snapshot_path}
                                                end
                                            )
                                    end
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 1134).
-spec update_accepted_snapshots(binary(), birdie@internal@titles:titles()) -> {ok,
        nil} |
    {error, error()}.
update_accepted_snapshots(Snapshots_folder, Titles) ->
    gleam@result:'try'(
        list_accepted_snapshots(Snapshots_folder),
        fun(Accepted_snapshots) ->
            gleam@list:try_each(
                Accepted_snapshots,
                fun(Accepted_snapshot) ->
                    gleam@result:'try'(
                        read_accepted(Accepted_snapshot),
                        fun(Snapshot) -> case Snapshot of
                                none ->
                                    {ok, nil};

                                {some, {snapshot, Title, _, Info} = Snapshot@1} ->
                                    case {birdie@internal@titles:find(
                                            Titles,
                                            Title
                                        ),
                                        Info} of
                                        {{ok, Match}, {some, Existing_info}} when erlang:element(
                                            2,
                                            Match
                                        ) =/= Existing_info ->
                                            _pipe = {snapshot,
                                                erlang:element(2, Snapshot@1),
                                                erlang:element(3, Snapshot@1),
                                                {some, erlang:element(2, Match)}},
                                            _pipe@1 = serialise(_pipe),
                                            _pipe@2 = simplifile:write(
                                                Accepted_snapshot,
                                                _pipe@1
                                            ),
                                            gleam@result:map_error(
                                                _pipe@2,
                                                fun(_capture) ->
                                                    {cannot_accept_snapshot,
                                                        _capture,
                                                        Accepted_snapshot}
                                                end
                                            );

                                        {{ok, Match@1}, none} ->
                                            _pipe@3 = {snapshot,
                                                erlang:element(2, Snapshot@1),
                                                erlang:element(3, Snapshot@1),
                                                {some,
                                                    erlang:element(2, Match@1)}},
                                            _pipe@4 = serialise(_pipe@3),
                                            _pipe@5 = simplifile:write(
                                                Accepted_snapshot,
                                                _pipe@4
                                            ),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun(_capture@1) ->
                                                    {cannot_accept_snapshot,
                                                        _capture@1,
                                                        Accepted_snapshot}
                                                end
                                            );

                                        {_, _} ->
                                            {ok, nil}
                                    end
                            end end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 1193).
?DOC(" Reviews all the new snapshots one by one.\n").
-spec review_loop(
    list(binary()),
    birdie@internal@titles:titles(),
    integer(),
    integer(),
    review_mode()
) -> {ok, nil} | {error, error()}.
review_loop(New_snapshot_paths, Titles, Current, Out_of, Mode) ->
    case New_snapshot_paths of
        [] ->
            {ok, nil};

        [New_snapshot_path | Rest] ->
            clear(),
            gleam@result:'try'(
                read_new(New_snapshot_path),
                fun(New_snapshot) ->
                    New_snapshot_info = case birdie@internal@titles:find(
                        Titles,
                        erlang:element(2, New_snapshot)
                    ) of
                        {ok, {prefix, Info, _}} ->
                            {some, Info};

                        {ok, {literal, Info}} ->
                            {some, Info};

                        {error, _} ->
                            none
                    end,
                    New_snapshot@1 = {snapshot,
                        erlang:element(2, New_snapshot),
                        erlang:element(3, New_snapshot),
                        New_snapshot_info},
                    Accepted_snapshot_path = to_accepted_path(New_snapshot_path),
                    gleam@result:'try'(
                        read_accepted(Accepted_snapshot_path),
                        fun(Accepted_snapshot) ->
                            Progress = <<<<<<(gleam_community@ansi:dim(
                                            <<"Reviewing "/utf8>>
                                        ))/binary,
                                        (gleam_community@ansi:bold(
                                            gleam_community@ansi:yellow(
                                                rank:ordinalise(Current)
                                            )
                                        ))/binary>>/binary,
                                    (gleam_community@ansi:dim(
                                        <<" out of "/utf8>>
                                    ))/binary>>/binary,
                                (gleam_community@ansi:bold(
                                    gleam_community@ansi:yellow(
                                        erlang:integer_to_binary(Out_of)
                                    )
                                ))/binary>>,
                            Box = case {Accepted_snapshot, Mode} of
                                {none, _} ->
                                    new_snapshot_box(New_snapshot@1, []);

                                {{some, Accepted_snapshot@1}, show_diff} ->
                                    diff_snapshot_box(
                                        Accepted_snapshot@1,
                                        New_snapshot@1,
                                        []
                                    );

                                {{some, _}, hide_diff} ->
                                    regular_snapshot_box(New_snapshot@1, [])
                            end,
                            gleam_stdlib:println(
                                <<<<<<Progress/binary, "\n\n"/utf8>>/binary,
                                        Box/binary>>/binary,
                                    "\n"/utf8>>
                            ),
                            gleam@result:'try'(
                                ask_choice(Mode),
                                fun(Choice) -> case Choice of
                                        accept_snapshot ->
                                            gleam@result:'try'(
                                                accept_snapshot(
                                                    New_snapshot_path,
                                                    Titles
                                                ),
                                                fun(_) ->
                                                    review_loop(
                                                        Rest,
                                                        Titles,
                                                        Current + 1,
                                                        Out_of,
                                                        Mode
                                                    )
                                                end
                                            );

                                        reject_snapshot ->
                                            gleam@result:'try'(
                                                reject_snapshot(
                                                    New_snapshot_path
                                                ),
                                                fun(_) ->
                                                    review_loop(
                                                        Rest,
                                                        Titles,
                                                        Current + 1,
                                                        Out_of,
                                                        Mode
                                                    )
                                                end
                                            );

                                        skip_snapshot ->
                                            review_loop(
                                                Rest,
                                                Titles,
                                                Current + 1,
                                                Out_of,
                                                Mode
                                            );

                                        toggle_diff_view ->
                                            Mode@1 = toggle_mode(Mode),
                                            review_loop(
                                                New_snapshot_paths,
                                                Titles,
                                                Current,
                                                Out_of,
                                                Mode@1
                                            )
                                    end end
                            )
                        end
                    )
                end
            )
    end.

-file("src/birdie.gleam", 1370).
-spec stale_snapshots_file_names() -> {ok, list(binary())} | {error, error()}.
stale_snapshots_file_names() ->
    gleam@result:'try'(
        snapshot_folder(),
        fun(Snapshots_folder) ->
            gleam@result:'try'(
                referenced_file_path(),
                fun(Referenced_file) -> case simplifile:read(Referenced_file) of
                        {error, enoent} ->
                            {error, missing_referenced_file};

                        {error, Reason} ->
                            {error,
                                {cannot_read_referenced_file,
                                    Referenced_file,
                                    Reason}};

                        {ok, Non_stale_snapshots} ->
                            Existing_accepted_snapshots = begin
                                _pipe = simplifile:get_files(Snapshots_folder),
                                _pipe@1 = gleam@result:unwrap(_pipe, []),
                                gleam@list:fold(
                                    _pipe@1,
                                    gleam@set:new(),
                                    fun(Files, File) ->
                                        case filepath:extension(File) =:= {ok,
                                            <<"accepted"/utf8>>} of
                                            true ->
                                                gleam@set:insert(
                                                    Files,
                                                    filepath:base_name(File)
                                                );

                                            false ->
                                                Files
                                        end
                                    end
                                )
                            end,
                            Non_stale_snapshots@1 = gleam@string:split(
                                Non_stale_snapshots,
                                <<"\n"/utf8>>
                            ),
                            _pipe@2 = Existing_accepted_snapshots,
                            _pipe@3 = gleam@set:drop(
                                _pipe@2,
                                Non_stale_snapshots@1
                            ),
                            _pipe@4 = gleam@set:to_list(_pipe@3),
                            {ok, _pipe@4}
                    end end
            )
        end
    ).

-file("src/birdie.gleam", 1405).
-spec check_stale() -> {ok, nil} | {error, error()}.
check_stale() ->
    gleam_stdlib:println(<<"Checking stale snapshots..."/utf8>>),
    gleam@result:'try'(
        stale_snapshots_file_names(),
        fun(Stale_snapshots) -> case Stale_snapshots of
                [] ->
                    {ok, nil};

                [_ | _] ->
                    {error, {stale_snapshots_found, Stale_snapshots}}
            end end
    ).

-file("src/birdie.gleam", 1414).
-spec delete_stale() -> {ok, nil} | {error, error()}.
delete_stale() ->
    gleam_stdlib:println(<<"Checking stale snapshots..."/utf8>>),
    gleam@result:'try'(
        snapshot_folder(),
        fun(Snapshots_folder) ->
            gleam@result:'try'(
                stale_snapshots_file_names(),
                fun(Stale_snapshots) ->
                    _pipe@1 = gleam@list:try_each(
                        Stale_snapshots,
                        fun(Stale_snapshot) ->
                            _pipe = filepath:join(
                                Snapshots_folder,
                                Stale_snapshot
                            ),
                            simplifile_erl:delete(_pipe)
                        end
                    ),
                    gleam@result:map_error(
                        _pipe@1,
                        fun(_capture) ->
                            {cannot_delete_stale_snapshot, _capture}
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 466).
?DOC(
    " List all the new snapshots in a folder. Every file is automatically\n"
    " prepended with the folder so you get the full path of each file.\n"
).
-spec list_new_snapshots(binary()) -> {ok, list(binary())} | {error, error()}.
list_new_snapshots(Folder) ->
    case simplifile_erl:read_directory(Folder) of
        {error, Reason} ->
            {error, {cannot_read_snapshots, Reason, Folder}};

        {ok, Files} ->
            {ok,
                begin
                    gleam@list:filter_map(
                        Files,
                        fun(File) -> case filepath:extension(File) of
                                {ok, Extension} when Extension =:= <<"new"/utf8>> ->
                                    {ok, filepath:join(Folder, File)};

                                _ ->
                                    {error, nil}
                            end end
                    )
                end}
    end.

-file("src/birdie.gleam", 580).
?DOC(" Returns the path where a new snapshot should be saved.\n").
-spec new_destination(snapshot(new()), binary()) -> binary().
new_destination(Snapshot, Folder) ->
    <<<<(filepath:join(Folder, file_name(erlang:element(2, Snapshot))))/binary,
            "."/utf8>>/binary,
        "new"/utf8>>.

-file("src/birdie.gleam", 209).
-spec do_snap(binary(), binary()) -> {ok, outcome()} | {error, error()}.
do_snap(Content, Title) ->
    gleam@result:'try'(
        validate_snapshot_title(Title),
        fun(_) ->
            gleam@result:'try'(
                snapshot_folder(),
                fun(Folder) ->
                    New = {snapshot, Title, Content, none},
                    New_snapshot_path = new_destination(New, Folder),
                    Accepted_snapshot_path = to_accepted_path(New_snapshot_path),
                    gleam@result:'try'(
                        read_accepted(Accepted_snapshot_path),
                        fun(Accepted) -> case Accepted of
                                none ->
                                    gleam@result:'try'(
                                        save(New, New_snapshot_path),
                                        fun(_) ->
                                            {ok,
                                                {new_snapshot_created,
                                                    New,
                                                    New_snapshot_path}}
                                        end
                                    );

                                {some, Accepted@1} ->
                                    gleam@result:'try'(
                                        global_referenced_file(),
                                        fun(Referenced_file) ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = simplifile:append(
                                                        Referenced_file,
                                                        <<(filepath:base_name(
                                                                Accepted_snapshot_path
                                                            ))/binary,
                                                            "\n"/utf8>>
                                                    ),
                                                    gleam@result:map_error(
                                                        _pipe,
                                                        fun(Field@0) -> {cannot_mark_snapshot_as_referenced, Field@0} end
                                                    )
                                                end,
                                                fun(_) ->
                                                    case erlang:element(
                                                        3,
                                                        Accepted@1
                                                    )
                                                    =:= erlang:element(3, New) of
                                                        true ->
                                                            _ = simplifile_erl:delete(
                                                                New_snapshot_path
                                                            ),
                                                            {ok, same};

                                                        false ->
                                                            gleam@result:'try'(
                                                                save(
                                                                    New,
                                                                    New_snapshot_path
                                                                ),
                                                                fun(_) ->
                                                                    {ok,
                                                                        {different,
                                                                            Accepted@1,
                                                                            New}}
                                                                end
                                                            )
                                                    end
                                                end
                                            )
                                        end
                                    )
                            end end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 174).
?DOC(
    " Performs a snapshot test with the given title, saving the content to a new\n"
    " snapshot file. All your snapshots will be stored in a folder called\n"
    " `birdie_snapshots` in the project's root.\n"
    "\n"
    " The test will fail if there already is an accepted snapshot with the same\n"
    " title and a different content.\n"
    " The test will also fail if there's no accepted snapshot with the same title\n"
    " to make sure you will review new snapshots as well.\n"
    "\n"
    " > 🚨 A snapshot is saved to a file named after its title, so all titles\n"
    " > should be unique! Otherwise you'd end up comparing unrelated snapshots.\n"
    "\n"
    " > 🐦‍⬛ To review all your snapshots interactively you can run\n"
    " > `gleam run -m birdie`.\n"
    " >\n"
    " > To get an help text and all the available options you can run\n"
    " > `gleam run -m birdie help`.\n"
).
-spec snap(binary(), binary()) -> nil.
snap(Content, Title) ->
    case do_snap(Content, Title) of
        {ok, same} ->
            nil;

        {ok, {new_snapshot_created, Snapshot, _}} ->
            Hint_message = gleam_community@ansi:yellow(
                <<"run `gleam run -m birdie` to review the snapshots"/utf8>>
            ),
            Hint = {info_line_with_title,
                Hint_message,
                do_not_split,
                <<"hint"/utf8>>},
            Box = new_snapshot_box(Snapshot, [Hint]),
            gleam_stdlib:println_error(
                <<<<"\n\n"/utf8, Box/binary>>/binary, "\n"/utf8>>
            ),
            erlang:error(#{gleam_error => panic,
                    message => <<"Birdie snapshot test failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"snap"/utf8>>,
                    line => 184});

        {ok, {different, Accepted, New}} ->
            Hint_message@1 = gleam_community@ansi:yellow(
                <<"run `gleam run -m birdie` to review the snapshots"/utf8>>
            ),
            Hint@1 = {info_line_with_title,
                Hint_message@1,
                do_not_split,
                <<"hint"/utf8>>},
            Box@1 = diff_snapshot_box(Accepted, New, [Hint@1]),
            gleam_stdlib:println_error(
                <<<<"\n\n"/utf8, Box@1/binary>>/binary, "\n"/utf8>>
            ),
            erlang:error(#{gleam_error => panic,
                    message => <<"Birdie snapshot test failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"snap"/utf8>>,
                    line => 193});

        {error, Error} ->
            Panic_message = <<"Birdie snapshot test failed\n"/utf8,
                (explain(Error))/binary>>,
            erlang:error(#{gleam_error => panic,
                    message => Panic_message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"birdie"/utf8>>,
                    function => <<"snap"/utf8>>,
                    line => 198})
    end.

-file("src/birdie.gleam", 1162).
-spec do_review(binary(), birdie@internal@titles:titles()) -> {ok, nil} |
    {error, error()}.
do_review(Snapshots_folder, Titles) ->
    gleam@result:'try'(
        list_new_snapshots(Snapshots_folder),
        fun(New_snapshots) -> case erlang:length(New_snapshots) of
                0 ->
                    gleam_stdlib:println(<<"No new snapshots to review."/utf8>>),
                    {ok, nil};

                N ->
                    Result = review_loop(New_snapshots, Titles, 1, N, show_diff),
                    clear(),
                    gleam@result:'try'(
                        Result,
                        fun(_) ->
                            gleam_stdlib:println(case N of
                                    1 ->
                                        <<"Reviewed one snapshot"/utf8>>;

                                    N@1 ->
                                        <<<<"Reviewed "/utf8,
                                                (erlang:integer_to_binary(N@1))/binary>>/binary,
                                            " snapshots"/utf8>>
                                end),
                            {ok, nil}
                        end
                    )
            end end
    ).

-file("src/birdie.gleam", 1121).
-spec review() -> {ok, nil} | {error, error()}.
review() ->
    gleam@result:'try'(
        snapshot_folder(),
        fun(Snapshots_folder) ->
            Get_titles = birdie@internal@titles:from_test_directory(),
            gleam@result:'try'(
                gleam@result:map_error(
                    Get_titles,
                    fun(Field@0) -> {cannot_get_titles, Field@0} end
                ),
                fun(Titles) ->
                    gleam@result:'try'(
                        update_accepted_snapshots(Snapshots_folder, Titles),
                        fun(_) ->
                            gleam@result:'try'(
                                do_review(Snapshots_folder, Titles),
                                fun(_) -> {ok, nil} end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 1334).
-spec accept_all() -> {ok, nil} | {error, error()}.
accept_all() ->
    gleam_stdlib:println(<<"Looking for new snapshots..."/utf8>>),
    gleam@result:'try'(
        snapshot_folder(),
        fun(Snapshots_folder) ->
            gleam@result:'try'(
                list_new_snapshots(Snapshots_folder),
                fun(New_snapshots) ->
                    Get_titles = birdie@internal@titles:from_test_directory(),
                    gleam@result:'try'(
                        gleam@result:map_error(
                            Get_titles,
                            fun(Field@0) -> {cannot_get_titles, Field@0} end
                        ),
                        fun(Titles) ->
                            gleam@result:'try'(
                                update_accepted_snapshots(
                                    Snapshots_folder,
                                    Titles
                                ),
                                fun(_) ->
                                    case erlang:length(New_snapshots) of
                                        0 ->
                                            gleam_stdlib:println(
                                                <<"No new snapshots to accept."/utf8>>
                                            );

                                        1 ->
                                            gleam_stdlib:println(
                                                <<"Accepting one new snapshot."/utf8>>
                                            );

                                        N ->
                                            gleam_stdlib:println(
                                                <<<<"Accepting "/utf8,
                                                        (erlang:integer_to_binary(
                                                            N
                                                        ))/binary>>/binary,
                                                    " new snapshots."/utf8>>
                                            )
                                    end,
                                    gleam@list:try_each(
                                        New_snapshots,
                                        fun(_capture) ->
                                            accept_snapshot(_capture, Titles)
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 1352).
-spec reject_all() -> {ok, nil} | {error, error()}.
reject_all() ->
    gleam_stdlib:println(<<"Looking for new snapshots..."/utf8>>),
    gleam@result:'try'(
        snapshot_folder(),
        fun(Snapshots_folder) ->
            gleam@result:'try'(
                list_new_snapshots(Snapshots_folder),
                fun(New_snapshots) ->
                    Get_titles = birdie@internal@titles:from_test_directory(),
                    gleam@result:'try'(
                        gleam@result:map_error(
                            Get_titles,
                            fun(Field@0) -> {cannot_get_titles, Field@0} end
                        ),
                        fun(Titles) ->
                            gleam@result:'try'(
                                update_accepted_snapshots(
                                    Snapshots_folder,
                                    Titles
                                ),
                                fun(_) ->
                                    case erlang:length(New_snapshots) of
                                        0 ->
                                            gleam_stdlib:println(
                                                <<"No new snapshots to reject."/utf8>>
                                            );

                                        1 ->
                                            gleam_stdlib:println(
                                                <<"Rejecting one new snapshot."/utf8>>
                                            );

                                        N ->
                                            gleam_stdlib:println(
                                                <<<<"Rejecting "/utf8,
                                                        (erlang:integer_to_binary(
                                                            N
                                                        ))/binary>>/binary,
                                                    " new snapshots."/utf8>>
                                            )
                                    end,
                                    gleam@list:try_each(
                                        New_snapshots,
                                        fun reject_snapshot/1
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/birdie.gleam", 1097).
-spec run_command(birdie@internal@cli:command()) -> nil.
run_command(Command) ->
    case Command of
        review ->
            report_status(review());

        accept ->
            report_status(accept_all());

        reject ->
            report_status(reject_all());

        {stale, check_stale} ->
            report_status(check_stale());

        {stale, delete_stale} ->
            report_status(delete_stale());

        help ->
            gleam_stdlib:println(
                birdie@internal@cli:help_text(
                    <<"1.5.4"/utf8>>,
                    help,
                    full_command
                )
            );

        {with_help_option, Command@1, Explained} ->
            gleam_stdlib:println(
                birdie@internal@cli:help_text(
                    <<"1.5.4"/utf8>>,
                    Command@1,
                    Explained
                )
            )
    end.

-file("src/birdie.gleam", 1026).
-spec parse_and_run(list(binary())) -> nil.
parse_and_run(Args) ->
    case birdie@internal@cli:parse(Args) of
        {ok, Command} ->
            run_command(Command);

        {error, {unknown_option, Command@1, Option}} ->
            _pipe = birdie@internal@cli:unknown_option_error(
                <<"1.5.4"/utf8>>,
                Command@1,
                Option
            ),
            gleam_stdlib:println(_pipe),
            erlang:halt(1);

        {error, {unknown_subcommand, Command@2, Subcommand}} ->
            _pipe@1 = birdie@internal@cli:unknown_subcommand_error(
                <<"1.5.4"/utf8>>,
                Command@2,
                Subcommand
            ),
            gleam_stdlib:println(_pipe@1),
            erlang:halt(1);

        {error, {missing_subcommand, Command@3}} ->
            _pipe@2 = birdie@internal@cli:missing_subcommand_error(
                <<"1.5.4"/utf8>>,
                Command@3
            ),
            gleam_stdlib:println(_pipe@2),
            erlang:halt(1);

        {error, {unexpected_argument, Command@4, Argument}} ->
            _pipe@3 = birdie@internal@cli:unexpected_argument_error(
                <<"1.5.4"/utf8>>,
                Command@4,
                Argument
            ),
            gleam_stdlib:println(_pipe@3),
            erlang:halt(1);

        {error, {unknown_command, Command@5}} ->
            case birdie@internal@cli:similar_command(Command@5) of
                {error, nil} ->
                    _pipe@4 = birdie@internal@cli:unknown_command_error(
                        Command@5,
                        true
                    ),
                    gleam_stdlib:println(_pipe@4),
                    erlang:halt(1);

                {ok, New_command} ->
                    _pipe@5 = birdie@internal@cli:unknown_command_error(
                        Command@5,
                        false
                    ),
                    gleam_stdlib:println(_pipe@5),
                    Prompt = <<<<"I think you misspelled `"/utf8,
                            New_command/binary>>/binary,
                        "`, would you like me to run it instead?"/utf8>>,
                    case ask_yes_or_no(Prompt) of
                        no ->
                            gleam_stdlib:println(
                                <<"\n"/utf8,
                                    (birdie@internal@cli:main_help_text())/binary>>
                            ),
                            erlang:halt(1);

                        yes ->
                            _pipe@6 = replace_first(
                                Args,
                                Command@5,
                                New_command
                            ),
                            parse_and_run(_pipe@6)
                    end
            end
    end.

-file("src/birdie.gleam", 1022).
?DOC(
    " Reviews the snapshots in the project's folder.\n"
    " This function will behave differently depending on the command line\n"
    " arguments provided to the program.\n"
    " To have a look at all the available options you can run\n"
    " `gleam run -m birdie help`.\n"
    "\n"
    " > 🐦‍⬛ The recommended workflow is to first run your gleeunit tests with\n"
    " > `gleam test` and then review any new/failing snapshot manually running\n"
    " > `gleam run -m birdie`.\n"
    " >\n"
    " > And don't forget to commit your snapshots! Those should be treated as code\n"
    " > and checked with the vcs you're using.\n"
).
-spec main() -> nil.
main() ->
    parse_and_run(erlang:element(4, argv:load())).
