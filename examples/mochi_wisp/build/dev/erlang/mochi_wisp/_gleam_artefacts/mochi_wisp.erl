-module(mochi_wisp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp.gleam").
-export([get_app_state/0, main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi_wisp.gleam", 130).
?DOC(" Get the app state from persistent_term\n").
-spec get_app_state() -> mochi_wisp@supervisor:app_state().
get_app_state() ->
    mochi_app_state_ffi:get_app_state().

-file("src/mochi_wisp.gleam", 136).
-spec common_queries() -> list(binary()).
common_queries() ->
    [<<"{ users { id name } }"/utf8>>,
        <<"{ users { id name email } }"/utf8>>,
        <<"{ users { id name email role } }"/utf8>>,
        <<"{ user(id: \"1\") { id name } }"/utf8>>,
        <<"{ user(id: \"1\") { id name email role } }"/utf8>>,
        <<"{ __schema { types { name } } }"/utf8>>,
        <<"{ __type(name: \"User\") { name fields { name type { name } } } }"/utf8>>].

-file("src/mochi_wisp.gleam", 71).
-spec start_http_server(mochi_wisp@supervisor:app_state()) -> nil.
start_http_server(App_state) ->
    Handler = fun mochi_wisp@router:handle_request/1,
    Secret_key_base = wisp:random_string(64),
    logging:log(info, <<"Configuring HTTP server..."/utf8>>),
    case begin
        _pipe = wisp@wisp_mist:handler(Handler, Secret_key_base),
        _pipe@1 = mist:new(_pipe),
        _pipe@2 = mist:port(_pipe@1, 8000),
        _pipe@3 = mist:bind(_pipe@2, <<"0.0.0.0"/utf8>>),
        mist:start(_pipe@3)
    end of
        {ok, _} -> nil;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"mochi_wisp"/utf8>>,
                        function => <<"start_http_server"/utf8>>,
                        line => 81,
                        value => _assert_fail,
                        start => 2615,
                        'end' => 2766,
                        pattern_start => 2626,
                        pattern_end => 2631})
    end,
    logging:log(
        info,
        <<"Server started successfully on port "/utf8,
            (erlang:integer_to_binary(8000))/binary>>
    ),
    logging:log(info, <<""/utf8>>),
    logging:log(info, <<"Endpoints:"/utf8>>),
    logging:log(info, <<"  POST /graphql     - Execute GraphQL queries"/utf8>>),
    logging:log(info, <<"  OPTIONS /graphql  - CORS preflight"/utf8>>),
    logging:log(info, <<"  GET /health       - Health check"/utf8>>),
    logging:log(info, <<""/utf8>>),
    logging:log(info, <<"Example query:"/utf8>>),
    logging:log(
        info,
        <<<<"  curl -X POST http://localhost:"/utf8,
                (erlang:integer_to_binary(8000))/binary>>/binary,
            "/graphql \\"/utf8>>
    ),
    logging:log(info, <<"    -H \"Content-Type: application/json\" \\"/utf8>>),
    logging:log(
        info,
        <<"    -d '{\"query\": \"{ users { id name email } }\"}'"/utf8>>
    ),
    logging:log(info, <<""/utf8>>),
    logging:log(info, <<"Set DEBUG=1 for verbose logging"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"GraphQL server running at http://localhost:"/utf8,
                (erlang:integer_to_binary(8000))/binary>>/binary,
            "/graphql"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Stats = mochi_wisp@supervisor:get_cache_stats(App_state),
    logging:log(
        info,
        <<<<"Cache stats: "/utf8,
                (erlang:integer_to_binary(erlang:element(4, Stats)))/binary>>/binary,
            " queries cached"/utf8>>
    ),
    gleam_erlang_ffi:sleep_forever().

-file("src/mochi_wisp.gleam", 29).
-spec main() -> nil.
main() ->
    wisp:configure_logger(),
    logging:set_level(info),
    logging:log(info, <<"==========================================="/utf8>>),
    logging:log(info, <<"  Mochi GraphQL Server (OTP Edition)"/utf8>>),
    logging:log(info, <<"==========================================="/utf8>>),
    logging:log(info, <<""/utf8>>),
    logging:log(info, <<"BEAM VM Features Enabled:"/utf8>>),
    logging:log(info, <<"  - OTP Actors for cache & metrics"/utf8>>),
    logging:log(info, <<"  - ETS tables with read/write concurrency"/utf8>>),
    logging:log(info, <<"  - persistent_term for schema caching"/utf8>>),
    logging:log(info, <<"  - Process isolation for fault tolerance"/utf8>>),
    logging:log(info, <<""/utf8>>),
    logging:log(info, <<"Initializing OTP application..."/utf8>>),
    App_result = mochi_wisp@supervisor:init_app(),
    case App_result of
        {ok, App_state} ->
            mochi_app_state_ffi:set_app_state(App_state),
            logging:log(info, <<"Building and caching GraphQL schema..."/utf8>>),
            mochi_wisp@router:init_schema(),
            logging:log(info, <<"Warming up query cache..."/utf8>>),
            mochi_wisp@supervisor:warm_cache(App_state, common_queries()),
            start_http_server(App_state);

        {error, Msg} ->
            logging:log(
                error,
                <<"Failed to initialize application: "/utf8, Msg/binary>>
            ),
            erlang:error(#{gleam_error => panic,
                    message => <<"Application initialization failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi_wisp"/utf8>>,
                    function => <<"main"/utf8>>,
                    line => 66})
    end.
