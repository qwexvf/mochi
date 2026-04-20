-module(scalars_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/scalars_test.gleam").
-export([date_time_valid_iso8601_test/0, date_time_valid_with_offset_test/0, date_time_invalid_test/0, date_time_empty_string_test/0, date_valid_test/0, date_invalid_format_test/0, date_invalid_short_test/0, json_any_value_test/0, json_int_value_test/0, email_valid_test/0, email_missing_at_test/0, email_missing_dot_test/0, email_multiple_at_test/0, url_valid_https_test/0, url_valid_http_test/0, url_invalid_ftp_test/0, url_invalid_no_scheme_test/0, uuid_valid_test/0, uuid_invalid_length_test/0, uuid_invalid_no_dashes_test/0]).

-file("test/scalars_test.gleam", 4).
-spec date_time_valid_iso8601_test() -> nil.
date_time_valid_iso8601_test() ->
    Scalar = mochi@scalars:date_time(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"2024-01-15T10:30:00Z"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid datetime: "/utf8,
                        E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_time_valid_iso8601_test"/utf8>>,
                    line => 8})
    end.

-file("test/scalars_test.gleam", 12).
-spec date_time_valid_with_offset_test() -> nil.
date_time_valid_with_offset_test() ->
    Scalar = mochi@scalars:date_time(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"2024-01-15T10:30:00+05:30"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid datetime with offset: "/utf8,
                        E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_time_valid_with_offset_test"/utf8>>,
                    line => 16})
    end.

-file("test/scalars_test.gleam", 20).
-spec date_time_invalid_test() -> nil.
date_time_invalid_test() ->
    Scalar = mochi@scalars:date_time(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"not-a-date"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject invalid datetime"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_time_invalid_test"/utf8>>,
                    line => 23});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 28).
-spec date_time_empty_string_test() -> nil.
date_time_empty_string_test() ->
    Scalar = mochi@scalars:date_time(),
    case (erlang:element(5, Scalar))(gleam_stdlib:identity(<<""/utf8>>)) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject empty string"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_time_empty_string_test"/utf8>>,
                    line => 31});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 36).
-spec date_valid_test() -> nil.
date_valid_test() ->
    Scalar = mochi@scalars:date(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"2024-01-15"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid date: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_valid_test"/utf8>>,
                    line => 40})
    end.

-file("test/scalars_test.gleam", 44).
-spec date_invalid_format_test() -> nil.
date_invalid_format_test() ->
    Scalar = mochi@scalars:date(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"2024/01/15"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject slash-delimited date"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_invalid_format_test"/utf8>>,
                    line => 47});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 52).
-spec date_invalid_short_test() -> nil.
date_invalid_short_test() ->
    Scalar = mochi@scalars:date(),
    case (erlang:element(5, Scalar))(gleam_stdlib:identity(<<"2024-1-5"/utf8>>)) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject short date"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"date_invalid_short_test"/utf8>>,
                    line => 55});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 60).
-spec json_any_value_test() -> nil.
json_any_value_test() ->
    Scalar = mochi@scalars:json(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"any string"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"JSON should accept any value: "/utf8,
                        E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"json_any_value_test"/utf8>>,
                    line => 64})
    end.

-file("test/scalars_test.gleam", 68).
-spec json_int_value_test() -> nil.
json_int_value_test() ->
    Scalar = mochi@scalars:json(),
    case (erlang:element(5, Scalar))(gleam_stdlib:identity(42)) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"JSON should accept int: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"json_int_value_test"/utf8>>,
                    line => 72})
    end.

-file("test/scalars_test.gleam", 76).
-spec email_valid_test() -> nil.
email_valid_test() ->
    Scalar = mochi@scalars:email(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"user@example.com"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid email: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"email_valid_test"/utf8>>,
                    line => 80})
    end.

-file("test/scalars_test.gleam", 84).
-spec email_missing_at_test() -> nil.
email_missing_at_test() ->
    Scalar = mochi@scalars:email(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"userexample.com"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject email without @"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"email_missing_at_test"/utf8>>,
                    line => 87});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 92).
-spec email_missing_dot_test() -> nil.
email_missing_dot_test() ->
    Scalar = mochi@scalars:email(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"user@example"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject email without dot in domain"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"email_missing_dot_test"/utf8>>,
                    line => 95});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 100).
-spec email_multiple_at_test() -> nil.
email_multiple_at_test() ->
    Scalar = mochi@scalars:email(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"user@@example.com"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject email with multiple @"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"email_multiple_at_test"/utf8>>,
                    line => 103});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 108).
-spec url_valid_https_test() -> nil.
url_valid_https_test() ->
    Scalar = mochi@scalars:url(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"https://example.com"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid https URL: "/utf8,
                        E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"url_valid_https_test"/utf8>>,
                    line => 112})
    end.

-file("test/scalars_test.gleam", 116).
-spec url_valid_http_test() -> nil.
url_valid_http_test() ->
    Scalar = mochi@scalars:url(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"http://example.com/path?q=1"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid http URL: "/utf8,
                        E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"url_valid_http_test"/utf8>>,
                    line => 120})
    end.

-file("test/scalars_test.gleam", 124).
-spec url_invalid_ftp_test() -> nil.
url_invalid_ftp_test() ->
    Scalar = mochi@scalars:url(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"ftp://example.com"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject ftp URL"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"url_invalid_ftp_test"/utf8>>,
                    line => 127});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 132).
-spec url_invalid_no_scheme_test() -> nil.
url_invalid_no_scheme_test() ->
    Scalar = mochi@scalars:url(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"example.com"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject URL without scheme"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"url_invalid_no_scheme_test"/utf8>>,
                    line => 135});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 140).
-spec uuid_valid_test() -> nil.
uuid_valid_test() ->
    Scalar = mochi@scalars:uuid(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"550e8400-e29b-41d4-a716-446655440000"/utf8>>)
    ) of
        {ok, _} ->
            nil;

        {error, E} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Should parse valid UUID: "/utf8, E/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"uuid_valid_test"/utf8>>,
                    line => 146})
    end.

-file("test/scalars_test.gleam", 150).
-spec uuid_invalid_length_test() -> nil.
uuid_invalid_length_test() ->
    Scalar = mochi@scalars:uuid(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"550e8400-e29b-41d4-a716"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject short UUID"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"uuid_invalid_length_test"/utf8>>,
                    line => 153});

        {error, _} ->
            nil
    end.

-file("test/scalars_test.gleam", 158).
-spec uuid_invalid_no_dashes_test() -> nil.
uuid_invalid_no_dashes_test() ->
    Scalar = mochi@scalars:uuid(),
    case (erlang:element(5, Scalar))(
        gleam_stdlib:identity(<<"550e8400e29b41d4a716446655440000"/utf8>>)
    ) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should reject UUID without dashes"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"scalars_test"/utf8>>,
                    function => <<"uuid_invalid_no_dashes_test"/utf8>>,
                    line => 163});

        {error, _} ->
            nil
    end.
