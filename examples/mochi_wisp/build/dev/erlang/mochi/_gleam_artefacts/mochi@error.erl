-module(mochi@error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/error.gleam").
-export([error/1, error_with_path/2, error_at/2, at_location/3, with_locations/2, with_path/2, with_extension/3, with_extensions/2, with_code/2, to_dynamic/1, errors_to_dynamic/1, format/1, with_category/2, validation_error/2, resolver_error/2, type_error/2, authentication_error/1, authorization_error/2, user_input_error/3, internal_error/1, path_from_strings/1, append_index/2, append_field/2]).
-export_type([location/0, path_segment/0, graph_q_l_error/0, error_category/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type location() :: {location, integer(), integer()}.

-type path_segment() :: {field_segment, binary()} | {index_segment, integer()}.

-type graph_q_l_error() :: {graph_q_l_error,
        binary(),
        gleam@option:option(list(location())),
        gleam@option:option(list(path_segment())),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-type error_category() :: validation_error_category |
    resolver_error_category |
    type_error_category |
    authentication_error_category |
    authorization_error_category |
    internal_error_category |
    user_input_error_category.

-file("src/mochi/error.gleam", 63).
?DOC(" Create a simple error with just a message\n").
-spec error(binary()) -> graph_q_l_error().
error(Message) ->
    {graph_q_l_error, Message, none, none, none}.

-file("src/mochi/error.gleam", 68).
?DOC(" Create an error with a path\n").
-spec error_with_path(binary(), list(path_segment())) -> graph_q_l_error().
error_with_path(Message, Path) ->
    {graph_q_l_error, Message, none, {some, Path}, none}.

-file("src/mochi/error.gleam", 78).
?DOC(" Create an error from string path segments (convenience)\n").
-spec error_at(binary(), list(binary())) -> graph_q_l_error().
error_at(Message, Path) ->
    {graph_q_l_error,
        Message,
        none,
        {some,
            gleam@list:map(Path, fun(Field@0) -> {field_segment, Field@0} end)},
        none}.

-file("src/mochi/error.gleam", 88).
?DOC(" Add a location to an error\n").
-spec at_location(graph_q_l_error(), integer(), integer()) -> graph_q_l_error().
at_location(Err, Line, Column) ->
    New_loc = {location, Line, Column},
    Locations = case erlang:element(3, Err) of
        {some, Locs} ->
            {some, lists:append(Locs, [New_loc])};

        none ->
            {some, [New_loc]}
    end,
    {graph_q_l_error,
        erlang:element(2, Err),
        Locations,
        erlang:element(4, Err),
        erlang:element(5, Err)}.

-file("src/mochi/error.gleam", 98).
?DOC(" Add locations to an error\n").
-spec with_locations(graph_q_l_error(), list(location())) -> graph_q_l_error().
with_locations(Err, Locations) ->
    {graph_q_l_error,
        erlang:element(2, Err),
        {some, Locations},
        erlang:element(4, Err),
        erlang:element(5, Err)}.

-file("src/mochi/error.gleam", 106).
?DOC(" Set the path on an error\n").
-spec with_path(graph_q_l_error(), list(path_segment())) -> graph_q_l_error().
with_path(Err, Path) ->
    {graph_q_l_error,
        erlang:element(2, Err),
        erlang:element(3, Err),
        {some, Path},
        erlang:element(5, Err)}.

-file("src/mochi/error.gleam", 111).
?DOC(" Add an extension to an error\n").
-spec with_extension(graph_q_l_error(), binary(), gleam@dynamic:dynamic_()) -> graph_q_l_error().
with_extension(Err, Key, Value) ->
    Extensions = case erlang:element(5, Err) of
        {some, Ext} ->
            gleam@dict:insert(Ext, Key, Value);

        none ->
            maps:from_list([{Key, Value}])
    end,
    {graph_q_l_error,
        erlang:element(2, Err),
        erlang:element(3, Err),
        erlang:element(4, Err),
        {some, Extensions}}.

-file("src/mochi/error.gleam", 124).
?DOC(" Set multiple extensions on an error\n").
-spec with_extensions(
    graph_q_l_error(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> graph_q_l_error().
with_extensions(Err, Extensions) ->
    {graph_q_l_error,
        erlang:element(2, Err),
        erlang:element(3, Err),
        erlang:element(4, Err),
        {some, Extensions}}.

-file("src/mochi/error.gleam", 141).
?DOC(" Set error code in extensions\n").
-spec with_code(graph_q_l_error(), binary()) -> graph_q_l_error().
with_code(Err, Code) ->
    with_extension(Err, <<"code"/utf8>>, gleam_stdlib:identity(Code)).

-file("src/mochi/error.gleam", 255).
-spec path_segment_to_dynamic(path_segment()) -> gleam@dynamic:dynamic_().
path_segment_to_dynamic(Segment) ->
    case Segment of
        {field_segment, Name} ->
            gleam_stdlib:identity(Name);

        {index_segment, Index} ->
            gleam_stdlib:identity(Index)
    end.

-file("src/mochi/error.gleam", 205).
?DOC(" Convert a GraphQLError to a Dynamic representation for JSON serialization\n").
-spec to_dynamic(graph_q_l_error()) -> gleam@dynamic:dynamic_().
to_dynamic(Err) ->
    Base = [{<<"message"/utf8>>, gleam_stdlib:identity(erlang:element(2, Err))}],
    With_locations = case erlang:element(3, Err) of
        {some, Locs} ->
            [{<<"locations"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@list:map(
                            Locs,
                            fun(Loc) ->
                                gleam_stdlib:identity(
                                    maps:from_list(
                                        [{<<"line"/utf8>>,
                                                gleam_stdlib:identity(
                                                    erlang:element(2, Loc)
                                                )},
                                            {<<"column"/utf8>>,
                                                gleam_stdlib:identity(
                                                    erlang:element(3, Loc)
                                                )}]
                                    )
                                )
                            end
                        )
                    )} |
                Base];

        none ->
            Base
    end,
    With_path = case erlang:element(4, Err) of
        {some, Path_segments} ->
            [{<<"path"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@list:map(
                            Path_segments,
                            fun path_segment_to_dynamic/1
                        )
                    )} |
                With_locations];

        none ->
            With_locations
    end,
    Final = case erlang:element(5, Err) of
        {some, Ext} ->
            [{<<"extensions"/utf8>>, gleam_stdlib:identity(Ext)} | With_path];

        none ->
            With_path
    end,
    gleam_stdlib:identity(maps:from_list(Final)).

-file("src/mochi/error.gleam", 251).
?DOC(" Convert multiple errors to a list\n").
-spec errors_to_dynamic(list(graph_q_l_error())) -> gleam@dynamic:dynamic_().
errors_to_dynamic(Errors) ->
    gleam_stdlib:identity(gleam@list:map(Errors, fun to_dynamic/1)).

-file("src/mochi/error.gleam", 283).
-spec format_path(list(path_segment())) -> binary().
format_path(Segments) ->
    _pipe = Segments,
    _pipe@1 = gleam@list:map(_pipe, fun(S) -> case S of
                {field_segment, Name} ->
                    Name;

                {index_segment, Index} ->
                    <<<<"["/utf8, (erlang:integer_to_binary(Index))/binary>>/binary,
                        "]"/utf8>>
            end end),
    gleam@string:join(_pipe@1, <<"."/utf8>>).

-file("src/mochi/error.gleam", 294).
-spec format_locations(list(location())) -> binary().
format_locations(Locations) ->
    Loc_strs = gleam@list:map(
        Locations,
        fun(Loc) ->
            <<<<<<<<"("/utf8,
                            (erlang:integer_to_binary(erlang:element(2, Loc)))/binary>>/binary,
                        ":"/utf8>>/binary,
                    (erlang:integer_to_binary(erlang:element(3, Loc)))/binary>>/binary,
                ")"/utf8>>
        end
    ),
    case Loc_strs of
        [] ->
            <<""/utf8>>;

        _ ->
            <<<<"["/utf8, (gleam@string:join(Loc_strs, <<", "/utf8>>))/binary>>/binary,
                "]"/utf8>>
    end.

-file("src/mochi/error.gleam", 267).
?DOC(" Format an error as a human-readable string\n").
-spec format(graph_q_l_error()) -> binary().
format(Err) ->
    Msg = erlang:element(2, Err),
    With_path = case erlang:element(4, Err) of
        {some, Path_segments} ->
            <<<<Msg/binary, " at "/utf8>>/binary,
                (format_path(Path_segments))/binary>>;

        none ->
            Msg
    end,
    With_locations = case erlang:element(3, Err) of
        {some, Locs} ->
            <<<<With_path/binary, " "/utf8>>/binary,
                (format_locations(Locs))/binary>>;

        none ->
            With_path
    end,
    With_locations.

-file("src/mochi/error.gleam", 309).
-spec category_to_string(error_category()) -> binary().
category_to_string(Category) ->
    case Category of
        validation_error_category ->
            <<"VALIDATION"/utf8>>;

        resolver_error_category ->
            <<"RESOLVER"/utf8>>;

        type_error_category ->
            <<"TYPE"/utf8>>;

        authentication_error_category ->
            <<"AUTHENTICATION"/utf8>>;

        authorization_error_category ->
            <<"AUTHORIZATION"/utf8>>;

        internal_error_category ->
            <<"INTERNAL"/utf8>>;

        user_input_error_category ->
            <<"USER_INPUT"/utf8>>
    end.

-file("src/mochi/error.gleam", 132).
?DOC(" Set error category in extensions\n").
-spec with_category(graph_q_l_error(), error_category()) -> graph_q_l_error().
with_category(Err, Category) ->
    with_extension(
        Err,
        <<"category"/utf8>>,
        gleam_stdlib:identity(category_to_string(Category))
    ).

-file("src/mochi/error.gleam", 150).
?DOC(" Create a validation error\n").
-spec validation_error(binary(), list(binary())) -> graph_q_l_error().
validation_error(Message, Path) ->
    _pipe = error_at(Message, Path),
    with_category(_pipe, validation_error_category).

-file("src/mochi/error.gleam", 156).
?DOC(" Create a resolver error\n").
-spec resolver_error(binary(), list(binary())) -> graph_q_l_error().
resolver_error(Message, Path) ->
    _pipe = error_at(Message, Path),
    with_category(_pipe, resolver_error_category).

-file("src/mochi/error.gleam", 162).
?DOC(" Create a type error\n").
-spec type_error(binary(), list(binary())) -> graph_q_l_error().
type_error(Message, Path) ->
    _pipe = error_at(Message, Path),
    with_category(_pipe, type_error_category).

-file("src/mochi/error.gleam", 168).
?DOC(" Create an authentication error\n").
-spec authentication_error(binary()) -> graph_q_l_error().
authentication_error(Message) ->
    _pipe = error(Message),
    _pipe@1 = with_category(_pipe, authentication_error_category),
    with_code(_pipe@1, <<"UNAUTHENTICATED"/utf8>>).

-file("src/mochi/error.gleam", 175).
?DOC(" Create an authorization error\n").
-spec authorization_error(binary(), list(binary())) -> graph_q_l_error().
authorization_error(Message, Path) ->
    _pipe = error_at(Message, Path),
    _pipe@1 = with_category(_pipe, authorization_error_category),
    with_code(_pipe@1, <<"FORBIDDEN"/utf8>>).

-file("src/mochi/error.gleam", 182).
?DOC(" Create a user input error\n").
-spec user_input_error(binary(), binary(), list(binary())) -> graph_q_l_error().
user_input_error(Message, Field, Path) ->
    _pipe = error_at(Message, Path),
    _pipe@1 = with_category(_pipe, user_input_error_category),
    _pipe@2 = with_code(_pipe@1, <<"BAD_USER_INPUT"/utf8>>),
    with_extension(_pipe@2, <<"field"/utf8>>, gleam_stdlib:identity(Field)).

-file("src/mochi/error.gleam", 194).
?DOC(" Create an internal server error\n").
-spec internal_error(binary()) -> graph_q_l_error().
internal_error(Message) ->
    _pipe = error(Message),
    _pipe@1 = with_category(_pipe, internal_error_category),
    with_code(_pipe@1, <<"INTERNAL_SERVER_ERROR"/utf8>>).

-file("src/mochi/error.gleam", 322).
?DOC(" Convert string path to PathSegment list\n").
-spec path_from_strings(list(binary())) -> list(path_segment()).
path_from_strings(Path) ->
    gleam@list:map(Path, fun(Field@0) -> {field_segment, Field@0} end).

-file("src/mochi/error.gleam", 327).
?DOC(" Add index segment to path\n").
-spec append_index(list(path_segment()), integer()) -> list(path_segment()).
append_index(Path, Index) ->
    lists:append(Path, [{index_segment, Index}]).

-file("src/mochi/error.gleam", 332).
?DOC(" Add field segment to path\n").
-spec append_field(list(path_segment()), binary()) -> list(path_segment()).
append_field(Path, Field) ->
    lists:append(Path, [{field_segment, Field}]).
