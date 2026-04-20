-module(error_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/error_test.gleam").
-export([simple_error_test/0, error_with_path_test/0, error_at_test/0, error_with_location_test/0, error_with_multiple_locations_test/0, error_with_locations_list_test/0, error_with_extension_test/0, error_with_multiple_extensions_test/0, error_with_extensions_dict_test/0, error_with_category_test/0, error_with_code_test/0, validation_error_test/0, resolver_error_test/0, type_error_test/0, authentication_error_test/0, authorization_error_test/0, user_input_error_test/0, internal_error_test/0, error_to_dynamic_basic_test/0, error_to_dynamic_full_test/0, errors_to_dynamic_test/0, format_simple_error_test/0, format_error_with_path_test/0, format_error_with_index_path_test/0, format_error_with_location_test/0, path_from_strings_test/0, append_index_test/0, append_field_test/0, error_location_from_execution_test/0]).
-export_type([test_person/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type test_person() :: {test_person, binary(), binary()}.

-file("test/error_test.gleam", 17).
-spec simple_error_test() -> nil.
simple_error_test() ->
    Err = mochi@error:error(<<"Something went wrong"/utf8>>),
    case erlang:element(2, Err) =:= <<"Something went wrong"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be 'Something went wrong'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"simple_error_test"/utf8>>,
                    line => 22})
    end,
    case erlang:element(3, Err) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Locations should be None"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"simple_error_test"/utf8>>,
                    line => 27})
    end,
    case erlang:element(4, Err) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Path should be None"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"simple_error_test"/utf8>>,
                    line => 32})
    end,
    case erlang:element(5, Err) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Extensions should be None"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"simple_error_test"/utf8>>,
                    line => 37})
    end.

-file("test/error_test.gleam", 41).
-spec error_with_path_test() -> nil.
error_with_path_test() ->
    Err = mochi@error:error_with_path(
        <<"Field not found"/utf8>>,
        [{field_segment, <<"user"/utf8>>}, {field_segment, <<"email"/utf8>>}]
    ),
    case erlang:element(4, Err) of
        {some,
            [{field_segment, <<"user"/utf8>>},
                {field_segment, <<"email"/utf8>>}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Path should be [user, email]"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_path_test"/utf8>>,
                    line => 50})
    end.

-file("test/error_test.gleam", 54).
-spec error_at_test() -> nil.
error_at_test() ->
    Err = mochi@error:error_at(
        <<"Invalid field"/utf8>>,
        [<<"query"/utf8>>, <<"users"/utf8>>, <<"name"/utf8>>]
    ),
    case erlang:element(4, Err) of
        {some,
            [{field_segment, <<"query"/utf8>>},
                {field_segment, <<"users"/utf8>>},
                {field_segment, <<"name"/utf8>>}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Path should be [query, users, name]"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_at_test"/utf8>>,
                    line => 63})
    end.

-file("test/error_test.gleam", 71).
-spec error_with_location_test() -> nil.
error_with_location_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Syntax error"/utf8>>),
        mochi@error:at_location(_pipe, 10, 5)
    end,
    case erlang:element(3, Err) of
        {some, [{location, 10, 5}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have one location at line 10, column 5"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_location_test"/utf8>>,
                    line => 78})
    end.

-file("test/error_test.gleam", 82).
-spec error_with_multiple_locations_test() -> nil.
error_with_multiple_locations_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Multiple errors"/utf8>>),
        _pipe@1 = mochi@error:at_location(_pipe, 10, 5),
        mochi@error:at_location(_pipe@1, 15, 3)
    end,
    case erlang:element(3, Err) of
        {some, [{location, 10, 5}, {location, 15, 3}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have two locations"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_multiple_locations_test"/utf8>>,
                    line => 90})
    end.

-file("test/error_test.gleam", 94).
-spec error_with_locations_list_test() -> nil.
error_with_locations_list_test() ->
    Locs = [{location, 1, 1}, {location, 2, 2}, {location, 3, 3}],
    Err = begin
        _pipe = mochi@error:error(<<"Test"/utf8>>),
        mochi@error:with_locations(_pipe, Locs)
    end,
    case erlang:element(3, Err) of
        {some, Locations} ->
            case Locations of
                [{location, 1, 1}, {location, 2, 2}, {location, 3, 3}] ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have three locations"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_locations_list_test"/utf8>>,
                            line => 105})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have locations"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_locations_list_test"/utf8>>,
                    line => 108})
    end.

-file("test/error_test.gleam", 116).
-spec error_with_extension_test() -> nil.
error_with_extension_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Test error"/utf8>>),
        mochi@error:with_extension(
            _pipe,
            <<"code"/utf8>>,
            gleam_stdlib:identity(<<"SOME_CODE"/utf8>>)
        )
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"code"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'code' extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_extension_test"/utf8>>,
                            line => 125})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_extension_test"/utf8>>,
                    line => 128})
    end.

-file("test/error_test.gleam", 132).
-spec error_with_multiple_extensions_test() -> nil.
error_with_multiple_extensions_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Test error"/utf8>>),
        _pipe@1 = mochi@error:with_extension(
            _pipe,
            <<"code"/utf8>>,
            gleam_stdlib:identity(<<"ERROR_CODE"/utf8>>)
        ),
        _pipe@2 = mochi@error:with_extension(
            _pipe@1,
            <<"timestamp"/utf8>>,
            gleam_stdlib:identity(1234567890)
        ),
        mochi@error:with_extension(
            _pipe@2,
            <<"requestId"/utf8>>,
            gleam_stdlib:identity(<<"req-123"/utf8>>)
        )
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case maps:size(Ext) =:= 3 of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 3 extensions"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_multiple_extensions_test"/utf8>>,
                            line => 143})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_multiple_extensions_test"/utf8>>,
                    line => 146})
    end.

-file("test/error_test.gleam", 150).
-spec error_with_extensions_dict_test() -> nil.
error_with_extensions_dict_test() ->
    Extensions = maps:from_list(
        [{<<"key1"/utf8>>, gleam_stdlib:identity(<<"value1"/utf8>>)},
            {<<"key2"/utf8>>, gleam_stdlib:identity(42)}]
    ),
    Err = begin
        _pipe = mochi@error:error(<<"Test"/utf8>>),
        mochi@error:with_extensions(_pipe, Extensions)
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case maps:size(Ext) =:= 2 of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 2 extensions"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_extensions_dict_test"/utf8>>,
                            line => 165})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_extensions_dict_test"/utf8>>,
                    line => 168})
    end.

-file("test/error_test.gleam", 172).
-spec error_with_category_test() -> nil.
error_with_category_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Test"/utf8>>),
        mochi@error:with_category(_pipe, validation_error_category)
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"category"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'category' extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_category_test"/utf8>>,
                            line => 181})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_category_test"/utf8>>,
                    line => 184})
    end.

-file("test/error_test.gleam", 188).
-spec error_with_code_test() -> nil.
error_with_code_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Test"/utf8>>),
        mochi@error:with_code(_pipe, <<"CUSTOM_ERROR_CODE"/utf8>>)
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"code"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'code' extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_with_code_test"/utf8>>,
                            line => 197})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_with_code_test"/utf8>>,
                    line => 200})
    end.

-file("test/error_test.gleam", 208).
-spec validation_error_test() -> nil.
validation_error_test() ->
    Err = mochi@error:validation_error(
        <<"Field not found"/utf8>>,
        [<<"user"/utf8>>, <<"email"/utf8>>]
    ),
    case erlang:element(2, Err) =:= <<"Field not found"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be 'Field not found'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"validation_error_test"/utf8>>,
                    line => 213})
    end,
    case erlang:element(4, Err) of
        {some,
            [{field_segment, <<"user"/utf8>>},
                {field_segment, <<"email"/utf8>>}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Path should be [user, email]"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"validation_error_test"/utf8>>,
                    line => 218})
    end,
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"category"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have category extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"validation_error_test"/utf8>>,
                            line => 225})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"validation_error_test"/utf8>>,
                    line => 228})
    end.

-file("test/error_test.gleam", 232).
-spec resolver_error_test() -> nil.
resolver_error_test() ->
    Err = mochi@error:resolver_error(
        <<"Database error"/utf8>>,
        [<<"query"/utf8>>, <<"users"/utf8>>]
    ),
    case erlang:element(2, Err) =:= <<"Database error"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be 'Database error'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"resolver_error_test"/utf8>>,
                    line => 237})
    end.

-file("test/error_test.gleam", 241).
-spec type_error_test() -> nil.
type_error_test() ->
    Err = mochi@error:type_error(
        <<"Expected String, got Int"/utf8>>,
        [<<"user"/utf8>>, <<"age"/utf8>>]
    ),
    case erlang:element(2, Err) =:= <<"Expected String, got Int"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Message should be 'Expected String, got Int'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"type_error_test"/utf8>>,
                    line => 246})
    end.

-file("test/error_test.gleam", 250).
-spec authentication_error_test() -> nil.
authentication_error_test() ->
    Err = mochi@error:authentication_error(<<"Not authenticated"/utf8>>),
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"code"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'code' extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"authentication_error_test"/utf8>>,
                            line => 257})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"authentication_error_test"/utf8>>,
                    line => 260})
    end.

-file("test/error_test.gleam", 264).
-spec authorization_error_test() -> nil.
authorization_error_test() ->
    Err = mochi@error:authorization_error(
        <<"Access denied to admin field"/utf8>>,
        [<<"user"/utf8>>, <<"role"/utf8>>]
    ),
    case erlang:element(4, Err) of
        {some,
            [{field_segment, <<"user"/utf8>>}, {field_segment, <<"role"/utf8>>}]} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have path [user, role]"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"authorization_error_test"/utf8>>,
                    line => 270})
    end.

-file("test/error_test.gleam", 274).
-spec user_input_error_test() -> nil.
user_input_error_test() ->
    Err = mochi@error:user_input_error(
        <<"Invalid email format"/utf8>>,
        <<"email"/utf8>>,
        [<<"input"/utf8>>]
    ),
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"field"/utf8>>) andalso gleam@dict:has_key(
                Ext,
                <<"code"/utf8>>
            ) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'field' and 'code' extensions"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"user_input_error_test"/utf8>>,
                            line => 281})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"user_input_error_test"/utf8>>,
                    line => 284})
    end.

-file("test/error_test.gleam", 288).
-spec internal_error_test() -> nil.
internal_error_test() ->
    Err = mochi@error:internal_error(<<"Unexpected server error"/utf8>>),
    case erlang:element(5, Err) of
        {some, Ext} ->
            case gleam@dict:has_key(Ext, <<"code"/utf8>>) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have 'code' extension"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"internal_error_test"/utf8>>,
                            line => 295})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have extensions"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"internal_error_test"/utf8>>,
                    line => 298})
    end.

-file("test/error_test.gleam", 306).
-spec error_to_dynamic_basic_test() -> nil.
error_to_dynamic_basic_test() ->
    Err = mochi@error:error(<<"Test message"/utf8>>),
    _ = mochi@error:to_dynamic(Err),
    nil.

-file("test/error_test.gleam", 315).
-spec error_to_dynamic_full_test() -> nil.
error_to_dynamic_full_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Complete error"/utf8>>),
        _pipe@1 = mochi@error:at_location(_pipe, 10, 5),
        _pipe@2 = mochi@error:with_path(
            _pipe@1,
            [{field_segment, <<"query"/utf8>>},
                {field_segment, <<"users"/utf8>>},
                {index_segment, 0},
                {field_segment, <<"name"/utf8>>}]
        ),
        _pipe@3 = mochi@error:with_extension(
            _pipe@2,
            <<"code"/utf8>>,
            gleam_stdlib:identity(<<"FULL_ERROR"/utf8>>)
        ),
        mochi@error:with_extension(
            _pipe@3,
            <<"timestamp"/utf8>>,
            gleam_stdlib:identity(1234567890)
        )
    end,
    _ = mochi@error:to_dynamic(Err),
    nil.

-file("test/error_test.gleam", 334).
-spec errors_to_dynamic_test() -> nil.
errors_to_dynamic_test() ->
    Errors = [mochi@error:error(<<"Error 1"/utf8>>),
        mochi@error:error(<<"Error 2"/utf8>>),
        mochi@error:error(<<"Error 3"/utf8>>)],
    _ = mochi@error:errors_to_dynamic(Errors),
    nil.

-file("test/error_test.gleam", 351).
-spec format_simple_error_test() -> nil.
format_simple_error_test() ->
    Err = mochi@error:error(<<"Simple error message"/utf8>>),
    Formatted = mochi@error:format(Err),
    case Formatted =:= <<"Simple error message"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Format should be 'Simple error message'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"format_simple_error_test"/utf8>>,
                    line => 357})
    end.

-file("test/error_test.gleam", 361).
-spec format_error_with_path_test() -> nil.
format_error_with_path_test() ->
    Err = mochi@error:error_at(
        <<"Field error"/utf8>>,
        [<<"user"/utf8>>, <<"email"/utf8>>]
    ),
    Formatted = mochi@error:format(Err),
    case Formatted =:= <<"Field error at user.email"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Format should be 'Field error at user.email', got: "/utf8,
                        Formatted/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"format_error_with_path_test"/utf8>>,
                    line => 368})
    end.

-file("test/error_test.gleam", 374).
-spec format_error_with_index_path_test() -> nil.
format_error_with_index_path_test() ->
    Err = mochi@error:error_with_path(
        <<"Array error"/utf8>>,
        [{field_segment, <<"users"/utf8>>},
            {index_segment, 5},
            {field_segment, <<"name"/utf8>>}]
    ),
    Formatted = mochi@error:format(Err),
    case Formatted =:= <<"Array error at users.[5].name"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Format should be 'Array error at users.[5].name', got: "/utf8,
                        Formatted/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"format_error_with_index_path_test"/utf8>>,
                    line => 386})
    end.

-file("test/error_test.gleam", 392).
-spec format_error_with_location_test() -> nil.
format_error_with_location_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Located error"/utf8>>),
        mochi@error:at_location(_pipe, 10, 5)
    end,
    Formatted = mochi@error:format(Err),
    case Formatted =:= <<"Located error [(10:5)]"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Format should be 'Located error [(10:5)]', got: "/utf8,
                        Formatted/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"format_error_with_location_test"/utf8>>,
                    line => 401})
    end.

-file("test/error_test.gleam", 411).
-spec path_from_strings_test() -> nil.
path_from_strings_test() ->
    Path = mochi@error:path_from_strings(
        [<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>]
    ),
    case Path of
        [{field_segment, <<"a"/utf8>>},
            {field_segment, <<"b"/utf8>>},
            {field_segment, <<"c"/utf8>>}] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should convert strings to field segments"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"path_from_strings_test"/utf8>>,
                    line => 417})
    end.

-file("test/error_test.gleam", 421).
-spec append_index_test() -> nil.
append_index_test() ->
    Path = [{field_segment, <<"users"/utf8>>}],
    New_path = mochi@error:append_index(Path, 5),
    case New_path of
        [{field_segment, <<"users"/utf8>>}, {index_segment, 5}] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should append index segment"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"append_index_test"/utf8>>,
                    line => 427})
    end.

-file("test/error_test.gleam", 431).
-spec append_field_test() -> nil.
append_field_test() ->
    Path = [{field_segment, <<"query"/utf8>>}],
    New_path = mochi@error:append_field(Path, <<"users"/utf8>>),
    case New_path of
        [{field_segment, <<"query"/utf8>>}, {field_segment, <<"users"/utf8>>}] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should append field segment"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"append_field_test"/utf8>>,
                    line => 437})
    end.

-file("test/error_test.gleam", 449).
-spec decode_person(gleam@dynamic:dynamic_()) -> {ok, test_person()} |
    {error, binary()}.
decode_person(_) ->
    {ok, {test_person, <<"1"/utf8>>, <<"Test"/utf8>>}}.

-file("test/error_test.gleam", 453).
-spec create_person_schema() -> mochi@schema:schema().
create_person_schema() ->
    Person_type = begin
        _pipe = mochi@types:object(<<"Person"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(P) -> erlang:element(2, P) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(P@1) -> erlang:element(3, P@1) end
        ),
        mochi@types:build(_pipe@2, fun decode_person/1)
    end,
    Person_query = mochi@query:'query'(
        <<"person"/utf8>>,
        mochi@schema:named_type(<<"Person"/utf8>>),
        fun(_) -> {ok, {test_person, <<"1"/utf8>>, <<"Test"/utf8>>}} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Person_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, Person_type),
    mochi@query:build(_pipe@5).

-file("test/error_test.gleam", 511).
-spec int_to_string(integer()) -> binary().
int_to_string(I) ->
    case I of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        2 ->
            <<"2"/utf8>>;

        3 ->
            <<"3"/utf8>>;

        4 ->
            <<"4"/utf8>>;

        5 ->
            <<"5"/utf8>>;

        _ ->
            <<"?"/utf8>>
    end.

-file("test/error_test.gleam", 475).
?DOC(" Test that querying a non-existent field produces an error with source location\n").
-spec error_location_from_execution_test() -> nil.
error_location_from_execution_test() ->
    Test_schema = create_person_schema(),
    Query_str = <<"{\n  person {\n    nonexistent\n  }\n}"/utf8>>,
    Result = mochi@executor:execute_query(Test_schema, Query_str),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have validation error for nonexistent field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"error_test"/utf8>>,
                    function => <<"error_location_from_execution_test"/utf8>>,
                    line => 485});

        [Err | _] ->
            Gql_err = mochi@response:execution_error_to_graphql_error(Err),
            case erlang:element(3, Gql_err) of
                {some, [{location, Line, _}]} ->
                    case Line =:= 3 of
                        true ->
                            nil;

                        false ->
                            erlang:error(#{gleam_error => panic,
                                    message => (<<"Error location should be line 3, got line "/utf8,
                                        (begin
                                            _pipe = Line,
                                            int_to_string(_pipe)
                                        end)/binary>>),
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"error_test"/utf8>>,
                                    function => <<"error_location_from_execution_test"/utf8>>,
                                    line => 497})
                    end;

                {some, _} ->
                    nil;

                none ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Error should have source location"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"error_test"/utf8>>,
                            function => <<"error_location_from_execution_test"/utf8>>,
                            line => 505})
            end
    end.
