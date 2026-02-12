-module(mochi_wisp@router).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/router.gleam").
-export([init_schema/0, handle_request/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi_wisp/router.gleam", 26).
?DOC(" Initialize the schema cache - call once at startup\n").
-spec init_schema() -> nil.
init_schema() ->
    Graphql_schema = mochi_wisp@schema:build_schema(),
    mochi_router_ffi:set_schema(Graphql_schema),
    logging:log(info, <<"GraphQL schema initialized and cached"/utf8>>).

-file("src/mochi_wisp/router.gleam", 192).
-spec calculate_hit_rate(integer(), integer()) -> float().
calculate_hit_rate(Hits, Misses) ->
    Total = Hits + Misses,
    case Total of
        0 ->
            +0.0;

        _ ->
            (case erlang:float(Total) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Hits) / Gleam@denominator
            end) * 100.0
    end.

-file("src/mochi_wisp/router.gleam", 155).
?DOC(" Handle metrics endpoint\n").
-spec handle_metrics() -> gleam@http@response:response(wisp:body()).
handle_metrics() ->
    Cache_stats = mochi_query_cache_ffi:stats(),
    Body = gleam@json:to_string(
        gleam@json:object(
            [{<<"cache"/utf8>>,
                    gleam@json:object(
                        [{<<"queries_cached"/utf8>>,
                                gleam@json:int(erlang:element(4, Cache_stats))},
                            {<<"cache_hits"/utf8>>,
                                gleam@json:int(erlang:element(2, Cache_stats))},
                            {<<"cache_misses"/utf8>>,
                                gleam@json:int(erlang:element(3, Cache_stats))},
                            {<<"hit_rate_percent"/utf8>>,
                                gleam@json:float(
                                    calculate_hit_rate(
                                        erlang:element(2, Cache_stats),
                                        erlang:element(3, Cache_stats)
                                    )
                                )}]
                    )}]
        )
    ),
    wisp:json_response(Body, 200).

-file("src/mochi_wisp/router.gleam", 174).
?DOC(" Handle cache stats endpoint\n").
-spec handle_cache_stats() -> gleam@http@response:response(wisp:body()).
handle_cache_stats() ->
    Stats = mochi_query_cache_ffi:stats(),
    Total = erlang:element(2, Stats) + erlang:element(3, Stats),
    Body = gleam@json:to_string(
        gleam@json:object(
            [{<<"hits"/utf8>>, gleam@json:int(erlang:element(2, Stats))},
                {<<"misses"/utf8>>, gleam@json:int(erlang:element(3, Stats))},
                {<<"size"/utf8>>, gleam@json:int(erlang:element(4, Stats))},
                {<<"total_requests"/utf8>>, gleam@json:int(Total)},
                {<<"hit_rate_percent"/utf8>>,
                    gleam@json:float(
                        calculate_hit_rate(
                            erlang:element(2, Stats),
                            erlang:element(3, Stats)
                        )
                    )}]
        )
    ),
    wisp:json_response(Body, 200).

-file("src/mochi_wisp/router.gleam", 131).
?DOC(" Handle detailed health check with system info\n").
-spec handle_health_detailed() -> gleam@http@response:response(wisp:body()).
handle_health_detailed() ->
    Cache_stats = mochi_query_cache_ffi:stats(),
    Body = gleam@json:to_string(
        gleam@json:object(
            [{<<"status"/utf8>>, gleam@json:string(<<"healthy"/utf8>>)},
                {<<"cache"/utf8>>,
                    gleam@json:object(
                        [{<<"hits"/utf8>>,
                                gleam@json:int(erlang:element(2, Cache_stats))},
                            {<<"misses"/utf8>>,
                                gleam@json:int(erlang:element(3, Cache_stats))},
                            {<<"size"/utf8>>,
                                gleam@json:int(erlang:element(4, Cache_stats))},
                            {<<"hit_rate"/utf8>>,
                                gleam@json:float(
                                    calculate_hit_rate(
                                        erlang:element(2, Cache_stats),
                                        erlang:element(3, Cache_stats)
                                    )
                                )}]
                    )},
                {<<"vm"/utf8>>,
                    gleam@json:object(
                        [{<<"otp_release"/utf8>>,
                                gleam@json:string(
                                    mochi_system_ffi:otp_release()
                                )},
                            {<<"schedulers"/utf8>>,
                                gleam@json:int(
                                    mochi_system_ffi:scheduler_count()
                                )}]
                    )}]
        )
    ),
    wisp:json_response(Body, 200).

-file("src/mochi_wisp/router.gleam", 208).
-spec handle_graphql_route(
    gleam@http@request:request(wisp@internal:connection()),
    mochi@schema:schema()
) -> gleam@http@response:response(wisp:body()).
handle_graphql_route(Req, Gql_schema) ->
    case erlang:element(2, Req) of
        post ->
            mochi_wisp@graphql_handler:handle_graphql_quiet(Req, Gql_schema);

        options ->
            mochi_wisp@graphql_handler:handle_cors_preflight();

        _ ->
            logging:log(
                warning,
                <<"Method not allowed: "/utf8,
                    (gleam@http:method_to_string(erlang:element(2, Req)))/binary>>
            ),
            wisp:method_not_allowed([post, options])
    end.

-file("src/mochi_wisp/router.gleam", 49).
?DOC(" Route the request to the appropriate handler\n").
-spec route_request(
    gleam@http@request:request(wisp@internal:connection()),
    mochi@schema:schema()
) -> gleam@http@response:response(wisp:body()).
route_request(Req, Graphql_schema) ->
    case fun gleam@http@request:path_segments/1(Req) of
        [<<"graphql"/utf8>>] ->
            handle_graphql_route(Req, Graphql_schema);

        [<<"health"/utf8>>] ->
            logging:log(debug, <<"Health check OK"/utf8>>),
            wisp:ok();

        [<<"health"/utf8>>, <<"detailed"/utf8>>] ->
            handle_health_detailed();

        [<<"metrics"/utf8>>] ->
            handle_metrics();

        [<<"cache"/utf8>>, <<"clear"/utf8>>] ->
            case erlang:element(2, Req) of
                post ->
                    mochi_query_cache_ffi:clear(),
                    logging:log(info, <<"Cache cleared via API"/utf8>>),
                    wisp:json_response(
                        <<"{\"status\": \"cleared\"}"/utf8>>,
                        200
                    );

                _ ->
                    wisp:method_not_allowed([post])
            end;

        [<<"cache"/utf8>>, <<"stats"/utf8>>] ->
            handle_cache_stats();

        [<<"graphiql"/utf8>>] ->
            wisp:html_response(
                mochi@playground:graphiql(<<"/graphql"/utf8>>),
                200
            );

        [<<"playground"/utf8>>] ->
            wisp:html_response(
                mochi@playground:playground(<<"/graphql"/utf8>>),
                200
            );

        [<<"sandbox"/utf8>>] ->
            wisp:html_response(
                mochi@playground:apollo_sandbox(<<"/graphql"/utf8>>),
                200
            );

        [<<"explorer"/utf8>>] ->
            wisp:html_response(
                mochi@playground:simple_explorer(<<"/graphql"/utf8>>),
                200
            );

        [] ->
            wisp:html_response(
                <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<!DOCTYPE html><html><head><title>Mochi GraphQL</title>
        <style>
          body { font-family: -apple-system, sans-serif; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
          h1 { color: #333; } h2 { color: #666; margin-top: 2rem; }
          a { color: #e94560; } code { background: #f4f4f4; padding: 0.2rem 0.4rem; border-radius: 3px; }
          pre { background: #1a1a2e; color: #eee; padding: 1rem; border-radius: 5px; overflow-x: auto; }
          .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; }
          .card { border: 1px solid #ddd; padding: 1rem; border-radius: 5px; }
          .card h3 { margin: 0 0 0.5rem; }
        </style></head><body>"/utf8,
                                                                                                    "<h1>🍡 Mochi GraphQL Server</h1>"/utf8>>/binary,
                                                                                                "<p>Code-first GraphQL for Gleam, powered by the BEAM VM.</p>"/utf8>>/binary,
                                                                                            "<h2>Interactive Playgrounds</h2>"/utf8>>/binary,
                                                                                        "<div class=\"grid\">"/utf8>>/binary,
                                                                                    "<div class=\"card\"><h3><a href=\"/graphiql\">GraphiQL</a></h3><p>The classic GraphQL IDE</p></div>"/utf8>>/binary,
                                                                                "<div class=\"card\"><h3><a href=\"/playground\">Playground</a></h3><p>GraphQL Playground (legacy)</p></div>"/utf8>>/binary,
                                                                            "<div class=\"card\"><h3><a href=\"/sandbox\">Apollo Sandbox</a></h3><p>Modern Apollo explorer</p></div>"/utf8>>/binary,
                                                                        "<div class=\"card\"><h3><a href=\"/explorer\">Simple Explorer</a></h3><p>Lightweight, no deps</p></div>"/utf8>>/binary,
                                                                    "</div>"/utf8>>/binary,
                                                                "<h2>API Endpoints</h2>"/utf8>>/binary,
                                                            "<ul>"/utf8>>/binary,
                                                        "<li><code>POST /graphql</code> - Execute GraphQL queries</li>"/utf8>>/binary,
                                                    "<li><code>GET /health</code> - Health check</li>"/utf8>>/binary,
                                                "<li><code>GET /metrics</code> - Performance metrics</li>"/utf8>>/binary,
                                            "<li><code>GET /cache/stats</code> - Cache statistics</li>"/utf8>>/binary,
                                        "</ul>"/utf8>>/binary,
                                    "<h2>Example Query</h2>"/utf8>>/binary,
                                "<pre>curl -X POST http://localhost:8000/graphql \\\n"/utf8>>/binary,
                            "  -H \"Content-Type: application/json\" \\\n"/utf8>>/binary,
                        "  -d '{\"query\": \"{ users { id name email } }\"}'</pre>"/utf8>>/binary,
                    "</body></html>"/utf8>>,
                200
            );

        _ ->
            logging:log(warning, <<"404 Not Found"/utf8>>),
            wisp:not_found()
    end.

-file("src/mochi_wisp/router.gleam", 33).
?DOC(" Main request handler with request logging\n").
-spec handle_request(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_request(Req) ->
    Method = gleam@http:method_to_string(erlang:element(2, Req)),
    Path = <<"/"/utf8,
        (gleam@string:join(
            fun gleam@http@request:path_segments/1(Req),
            <<"/"/utf8>>
        ))/binary>>,
    logging:log(info, <<<<Method/binary, " "/utf8>>/binary, Path/binary>>),
    Graphql_schema = mochi_router_ffi:get_schema(),
    Response = route_request(Req, Graphql_schema),
    Response.
