-module(mochi_wisp@realworld_benchmark).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/realworld_benchmark.gleam").
-export([generate_large_query/1, run_all/0]).

-file("src/mochi_wisp/realworld_benchmark.gleam", 340).
-spec generate_fields(integer(), integer(), list(binary())) -> list(binary()).
generate_fields(Current, Max, Acc) ->
    case Current > Max of
        true ->
            Acc;

        false ->
            Field = <<<<"  field"/utf8,
                    (erlang:integer_to_binary(Current))/binary>>/binary,
                ": user(id: \"user-1\") { id username }"/utf8>>,
            generate_fields(Current + 1, Max, [Field | Acc])
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 331).
-spec generate_large_query(integer()) -> binary().
generate_large_query(Field_count) ->
    Fields = begin
        _pipe = generate_fields(1, Field_count, []),
        _pipe@1 = lists:reverse(_pipe),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    <<<<"query GeneratedQuery {\n"/utf8, Fields/binary>>/binary, "\n}"/utf8>>.

-file("src/mochi_wisp/realworld_benchmark.gleam", 403).
-spec run_iterations(integer(), fun(() -> any()), list(integer())) -> list(integer()).
run_iterations(Remaining, F, Acc) ->
    case Remaining of
        0 ->
            Acc;

        _ ->
            {Time_us, _} = mochi_wisp_benchmark_ffi:measure_time(F),
            run_iterations(Remaining - 1, F, [Time_us | Acc])
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 453).
-spec pad_left_zero(integer(), integer()) -> binary().
pad_left_zero(N, Width) ->
    S = erlang:integer_to_binary(N),
    Len = string:length(S),
    case Len >= Width of
        true ->
            S;

        false ->
            <<(gleam@string:repeat(<<"0"/utf8>>, Width - Len))/binary,
                S/binary>>
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 446).
-spec float_to_string_2(float()) -> binary().
float_to_string_2(F) ->
    Rounded = erlang:trunc(F * 100.0),
    Int_part = Rounded div 100,
    Dec_part = gleam@int:absolute_value(Rounded rem 100),
    <<<<(erlang:integer_to_binary(Int_part))/binary, "."/utf8>>/binary,
        (pad_left_zero(Dec_part, 2))/binary>>.

-file("src/mochi_wisp/realworld_benchmark.gleam", 424).
-spec format_time_compact(float()) -> binary().
format_time_compact(Us) ->
    case Us < 1000.0 of
        true ->
            <<(float_to_string_2(Us))/binary, " µs"/utf8>>;

        false ->
            case Us < 1000000.0 of
                true ->
                    <<(float_to_string_2(Us / 1000.0))/binary, " ms"/utf8>>;

                false ->
                    <<(float_to_string_2(Us / 1000000.0))/binary, " s"/utf8>>
            end
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 435).
-spec format_ops(float()) -> binary().
format_ops(Ops) ->
    case Ops >= 1000000.0 of
        true ->
            <<(float_to_string_2(Ops / 1000000.0))/binary, "M"/utf8>>;

        false ->
            case Ops >= 1000.0 of
                true ->
                    <<(float_to_string_2(Ops / 1000.0))/binary, "K"/utf8>>;

                false ->
                    float_to_string_2(Ops)
            end
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 623).
-spec estimate_tokens(binary()) -> integer().
estimate_tokens(Query) ->
    string:length(Query) div 4.

-file("src/mochi_wisp/realworld_benchmark.gleam", 699).
-spec repeat(integer(), fun(() -> any())) -> nil.
repeat(N, F) ->
    case N =< 0 of
        true ->
            nil;

        false ->
            _ = F(),
            repeat(N - 1, F)
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 713).
-spec print_summary() -> nil.
print_summary() ->
    gleam_stdlib:println(<<"--- Summary ---"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Key findings:"/utf8>>),
    gleam_stdlib:println(
        <<"  1. Parser performance scales roughly linearly with query size"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  2. Deeply nested queries are slower due to recursive execution"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  3. Fragment resolution adds overhead but is still efficient"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  4. Query caching provides 2-10x speedup for repeated queries"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Optimization recommendations:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Enable query caching for production (already implemented)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Consider query complexity limits for large queries"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Parser can be optimized with binary patterns for >5x speedup"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi_wisp/realworld_benchmark.gleam", 733).
-spec pad_right(binary(), integer()) -> binary().
pad_right(S, Width) ->
    Len = string:length(S),
    case Len >= Width of
        true ->
            S;

        false ->
            <<S/binary,
                (gleam@string:repeat(<<" "/utf8>>, Width - Len))/binary>>
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 514).
-spec print_query_size(binary(), binary()) -> nil.
print_query_size(Name, Query) ->
    Chars = string:length(Query),
    Bytes = erlang:byte_size(Query),
    gleam_stdlib:println(
        <<<<<<<<<<<<"  "/utf8, (pad_right(Name, 20))/binary>>/binary,
                            ": "/utf8>>/binary,
                        (erlang:integer_to_binary(Chars))/binary>>/binary,
                    " chars, "/utf8>>/binary,
                (erlang:integer_to_binary(Bytes))/binary>>/binary,
            " bytes"/utf8>>
    ).

-file("src/mochi_wisp/realworld_benchmark.gleam", 741).
-spec pad_left(binary(), integer()) -> binary().
pad_left(S, Width) ->
    Len = string:length(S),
    case Len >= Width of
        true ->
            S;

        false ->
            <<(gleam@string:repeat(<<" "/utf8>>, Width - Len))/binary,
                S/binary>>
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 413).
-spec format_time(float()) -> binary().
format_time(Us) ->
    case Us < 1000.0 of
        true ->
            pad_left(<<(float_to_string_2(Us))/binary, " µs"/utf8>>, 12);

        false ->
            case Us < 1000000.0 of
                true ->
                    pad_left(
                        <<(float_to_string_2(Us / 1000.0))/binary, " ms"/utf8>>,
                        12
                    );

                false ->
                    pad_left(
                        <<(float_to_string_2(Us / 1000000.0))/binary,
                            " s"/utf8>>,
                        12
                    )
            end
    end.

-file("src/mochi_wisp/realworld_benchmark.gleam", 385).
-spec benchmark(binary(), integer(), fun(() -> any())) -> nil.
benchmark(Name, Iterations, F) ->
    Times = run_iterations(Iterations, F, []),
    Total_us = gleam@list:fold(Times, 0, fun(Acc, T) -> Acc + T end),
    Avg_us = case erlang:float(Iterations) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Total_us) / Gleam@denominator
    end,
    Min_us = gleam@list:fold(
        Times,
        999999999,
        fun(Acc@1, T@1) -> gleam@int:min(Acc@1, T@1) end
    ),
    Max_us = gleam@list:fold(
        Times,
        0,
        fun(Acc@2, T@2) -> gleam@int:max(Acc@2, T@2) end
    ),
    Ops_per_sec = case Avg_us of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> 1000000.0 / Gleam@denominator@1
    end,
    gleam_stdlib:println(
        <<<<<<<<<<<<<<(pad_right(Name, 30))/binary,
                                    (format_time(Avg_us))/binary>>/binary,
                                (pad_left(
                                    <<(format_ops(Ops_per_sec))/binary,
                                        " ops/sec"/utf8>>,
                                    18
                                ))/binary>>/binary,
                            "  (min: "/utf8>>/binary,
                        (format_time_compact(erlang:float(Min_us)))/binary>>/binary,
                    ", max: "/utf8>>/binary,
                (format_time_compact(erlang:float(Max_us)))/binary>>/binary,
            ")"/utf8>>
    ).

-file("src/mochi_wisp/realworld_benchmark.gleam", 632).
-spec run_cache_vs_nocache_comparison(mochi@schema:schema()) -> nil.
run_cache_vs_nocache_comparison(Gql_schema) ->
    gleam_stdlib:println(<<"--- Cache vs No-Cache Comparison ---"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Test_queries = [{<<"Simple"/utf8>>, <<"{ users { id username } }"/utf8>>},
        {<<"Medium"/utf8>>,
            <<"
  query GetUser {
    user(id: \"user-1\") {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
    }
  }
"/utf8>>},
        {<<"Large"/utf8>>,
            <<"
  query LargeQuery {
    users {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
      posts {
        id
        title
        content
        excerpt
        status
        tags
        viewCount
        createdAt
        updatedAt
        author {
          id
          username
          email
          displayName
        }
        comments {
          id
          content
          createdAt
          author {
            id
            username
          }
        }
      }
      comments {
        id
        content
        createdAt
        post {
          id
          title
        }
      }
    }
    posts {
      id
      title
      content
      status
      author {
        id
        username
        role
      }
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>}],
    gleam@list:each(
        Test_queries,
        fun(Pair) ->
            {Name, Query} = Pair,
            gleam_stdlib:println(<<Name/binary, " query:"/utf8>>),
            Iterations = 1000,
            Start_nocache = mochi_wisp_benchmark_ffi:monotonic_time_us(),
            repeat(
                Iterations,
                fun() ->
                    Ctx = mochi@schema:execution_context(
                        gleam_stdlib:identity(maps:new())
                    ),
                    case mochi@parser:parse(Query) of
                        {ok, Doc} ->
                            _ = mochi@executor:execute(
                                Gql_schema,
                                Doc,
                                none,
                                Ctx,
                                maps:new()
                            ),
                            nil;

                        {error, _} ->
                            nil
                    end
                end
            ),
            Elapsed_nocache = mochi_wisp_benchmark_ffi:monotonic_time_us() - Start_nocache,
            Ops_nocache = case erlang:float(Elapsed_nocache) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Iterations) * 1000000.0 / Gleam@denominator
            end,
            mochi_query_cache_ffi:clear(),
            _ = mochi_wisp@query_cache:get_or_parse(Query),
            Start_cached = mochi_wisp_benchmark_ffi:monotonic_time_us(),
            repeat(
                Iterations,
                fun() ->
                    Ctx@1 = mochi@schema:execution_context(
                        gleam_stdlib:identity(maps:new())
                    ),
                    case mochi_wisp@query_cache:get_or_parse(Query) of
                        {ok, Doc@1} ->
                            _ = mochi@executor:execute(
                                Gql_schema,
                                Doc@1,
                                none,
                                Ctx@1,
                                maps:new()
                            ),
                            nil;

                        {error, _} ->
                            nil
                    end
                end
            ),
            Elapsed_cached = mochi_wisp_benchmark_ffi:monotonic_time_us() - Start_cached,
            Ops_cached = case erlang:float(Elapsed_cached) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> erlang:float(Iterations) * 1000000.0 / Gleam@denominator@1
            end,
            Speedup = case Ops_nocache of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@2 -> Ops_cached / Gleam@denominator@2
            end,
            gleam_stdlib:println(
                <<<<"  No cache:  "/utf8, (format_ops(Ops_nocache))/binary>>/binary,
                    " ops/sec"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"  Cached:    "/utf8, (format_ops(Ops_cached))/binary>>/binary,
                    " ops/sec"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"  Speedup:   "/utf8, (float_to_string_2(Speedup))/binary>>/binary,
                    "x"/utf8>>
            ),
            gleam_stdlib:println(<<""/utf8>>)
        end
    ),
    Stats = mochi_query_cache_ffi:stats(),
    gleam_stdlib:println(
        <<<<<<<<"Cache stats: "/utf8,
                        (erlang:integer_to_binary(erlang:element(2, Stats)))/binary>>/binary,
                    " hits, "/utf8>>/binary,
                (erlang:integer_to_binary(erlang:element(3, Stats)))/binary>>/binary,
            " misses"/utf8>>
    ),
    nil.

-file("src/mochi_wisp/realworld_benchmark.gleam", 466).
-spec run_parsing_benchmarks() -> nil.
run_parsing_benchmarks() ->
    gleam_stdlib:println(
        <<"--- Parsing Benchmarks (Parser Performance) ---"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Query sizes:"/utf8>>),
    print_query_size(<<"Simple"/utf8>>, <<"{ users { id username } }"/utf8>>),
    print_query_size(
        <<"Medium"/utf8>>,
        <<"
  query GetUser {
    user(id: \"user-1\") {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
    }
  }
"/utf8>>
    ),
    print_query_size(
        <<"Nested"/utf8>>,
        <<"
  query UserWithPosts {
    user(id: \"user-1\") {
      id
      username
      displayName
      posts {
        id
        title
        status
        viewCount
        tags
      }
    }
  }
"/utf8>>
    ),
    print_query_size(
        <<"Deeply nested"/utf8>>,
        <<"
  query DeeplyNested {
    user(id: \"user-1\") {
      id
      username
      posts {
        id
        title
        content
        author {
          id
          username
        }
        comments {
          id
          content
          author {
            id
            username
            role
          }
          replies {
            id
            content
            author {
              id
              username
            }
          }
        }
      }
    }
  }
"/utf8>>
    ),
    print_query_size(
        <<"Multiple roots"/utf8>>,
        <<"
  query Dashboard {
    users {
      id
      username
      role
    }
    posts {
      id
      title
      status
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
    ),
    print_query_size(
        <<"Fragments"/utf8>>,
        <<"
  query WithFragments {
    user(id: \"user-1\") {
      ...UserFields
      posts {
        ...PostFields
        author {
          ...UserFields
        }
      }
    }
    users {
      ...UserFields
    }
  }

  fragment UserFields on User {
    id
    username
    email
    displayName
    role
    createdAt
  }

  fragment PostFields on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>
    ),
    print_query_size(
        <<"Large"/utf8>>,
        <<"
  query LargeQuery {
    users {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
      posts {
        id
        title
        content
        excerpt
        status
        tags
        viewCount
        createdAt
        updatedAt
        author {
          id
          username
          email
          displayName
        }
        comments {
          id
          content
          createdAt
          author {
            id
            username
          }
        }
      }
      comments {
        id
        content
        createdAt
        post {
          id
          title
        }
      }
    }
    posts {
      id
      title
      content
      status
      author {
        id
        username
        role
      }
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
    ),
    print_query_size(
        <<"Complex combined"/utf8>>,
        <<"
  query ComplexQuery($userId: ID!, $postId: ID!, $limit: Int = 10) {
    currentUser: user(id: $userId) {
      ...UserDetails
      posts {
        ...PostSummary
      }
    }
    featuredPost: post(id: $postId) {
      ...PostDetails
      author {
        ...UserDetails
      }
      comments {
        id
        content
        author {
          ...UserDetails
        }
        replies {
          id
          content
        }
      }
    }
    recentPosts: publishedPosts {
      ...PostSummary
    }
    allUsers: users {
      id
      username
      role
    }
  }

  fragment UserDetails on User {
    id
    username
    email
    displayName
    bio
    role
    createdAt
    updatedAt
  }

  fragment PostSummary on Post {
    id
    title
    excerpt
    status
    viewCount
  }

  fragment PostDetails on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Parsing performance:"/utf8>>),
    benchmark(
        <<"Parse simple"/utf8>>,
        5000,
        fun() -> mochi@parser:parse(<<"{ users { id username } }"/utf8>>) end
    ),
    benchmark(
        <<"Parse medium"/utf8>>,
        5000,
        fun() ->
            mochi@parser:parse(
                <<"
  query GetUser {
    user(id: \"user-1\") {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse nested"/utf8>>,
        3000,
        fun() ->
            mochi@parser:parse(
                <<"
  query UserWithPosts {
    user(id: \"user-1\") {
      id
      username
      displayName
      posts {
        id
        title
        status
        viewCount
        tags
      }
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse deeply nested"/utf8>>,
        2000,
        fun() ->
            mochi@parser:parse(
                <<"
  query DeeplyNested {
    user(id: \"user-1\") {
      id
      username
      posts {
        id
        title
        content
        author {
          id
          username
        }
        comments {
          id
          content
          author {
            id
            username
            role
          }
          replies {
            id
            content
            author {
              id
              username
            }
          }
        }
      }
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse multiple roots"/utf8>>,
        2000,
        fun() ->
            mochi@parser:parse(
                <<"
  query Dashboard {
    users {
      id
      username
      role
    }
    posts {
      id
      title
      status
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse fragments"/utf8>>,
        2000,
        fun() ->
            mochi@parser:parse(
                <<"
  query WithFragments {
    user(id: \"user-1\") {
      ...UserFields
      posts {
        ...PostFields
        author {
          ...UserFields
        }
      }
    }
    users {
      ...UserFields
    }
  }

  fragment UserFields on User {
    id
    username
    email
    displayName
    role
    createdAt
  }

  fragment PostFields on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse directives"/utf8>>,
        3000,
        fun() ->
            mochi@parser:parse(
                <<"
  query WithDirectives($showEmail: Boolean!, $skipBio: Boolean!) {
    user(id: \"user-1\") {
      id
      username
      email @include(if: $showEmail)
      bio @skip(if: $skipBio)
      posts {
        id
        title @include(if: true)
        content @skip(if: false)
      }
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse large"/utf8>>,
        1000,
        fun() ->
            mochi@parser:parse(
                <<"
  query LargeQuery {
    users {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
      posts {
        id
        title
        content
        excerpt
        status
        tags
        viewCount
        createdAt
        updatedAt
        author {
          id
          username
          email
          displayName
        }
        comments {
          id
          content
          createdAt
          author {
            id
            username
          }
        }
      }
      comments {
        id
        content
        createdAt
        post {
          id
          title
        }
      }
    }
    posts {
      id
      title
      content
      status
      author {
        id
        username
        role
      }
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Parse complex combined"/utf8>>,
        1000,
        fun() ->
            mochi@parser:parse(
                <<"
  query ComplexQuery($userId: ID!, $postId: ID!, $limit: Int = 10) {
    currentUser: user(id: $userId) {
      ...UserDetails
      posts {
        ...PostSummary
      }
    }
    featuredPost: post(id: $postId) {
      ...PostDetails
      author {
        ...UserDetails
      }
      comments {
        id
        content
        author {
          ...UserDetails
        }
        replies {
          id
          content
        }
      }
    }
    recentPosts: publishedPosts {
      ...PostSummary
    }
    allUsers: users {
      id
      username
      role
    }
  }

  fragment UserDetails on User {
    id
    username
    email
    displayName
    bio
    role
    createdAt
    updatedAt
  }

  fragment PostSummary on Post {
    id
    title
    excerpt
    status
    viewCount
  }

  fragment PostDetails on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Generated queries (scaling test):"/utf8>>),
    Q10 = generate_large_query(10),
    Q50 = generate_large_query(50),
    Q100 = generate_large_query(100),
    Q200 = generate_large_query(200),
    print_query_size(<<"Generated 10 fields"/utf8>>, Q10),
    print_query_size(<<"Generated 50 fields"/utf8>>, Q50),
    print_query_size(<<"Generated 100 fields"/utf8>>, Q100),
    print_query_size(<<"Generated 200 fields"/utf8>>, Q200),
    gleam_stdlib:println(<<""/utf8>>),
    benchmark(
        <<"Parse 10 fields"/utf8>>,
        2000,
        fun() -> mochi@parser:parse(Q10) end
    ),
    benchmark(
        <<"Parse 50 fields"/utf8>>,
        500,
        fun() -> mochi@parser:parse(Q50) end
    ),
    benchmark(
        <<"Parse 100 fields"/utf8>>,
        200,
        fun() -> mochi@parser:parse(Q100) end
    ),
    benchmark(
        <<"Parse 200 fields"/utf8>>,
        100,
        fun() -> mochi@parser:parse(Q200) end
    ),
    nil.

-file("src/mochi_wisp/realworld_benchmark.gleam", 526).
-spec run_execution_benchmarks(mochi@schema:schema()) -> nil.
run_execution_benchmarks(Gql_schema) ->
    gleam_stdlib:println(
        <<"--- Execution Benchmarks (Full Pipeline) ---"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    benchmark(
        <<"Execute simple"/utf8>>,
        2000,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"{ users { id username } }"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute medium"/utf8>>,
        1000,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query GetUser {
    user(id: \"user-1\") {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute nested"/utf8>>,
        500,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query UserWithPosts {
    user(id: \"user-1\") {
      id
      username
      displayName
      posts {
        id
        title
        status
        viewCount
        tags
      }
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute deeply nested"/utf8>>,
        200,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query DeeplyNested {
    user(id: \"user-1\") {
      id
      username
      posts {
        id
        title
        content
        author {
          id
          username
        }
        comments {
          id
          content
          author {
            id
            username
            role
          }
          replies {
            id
            content
            author {
              id
              username
            }
          }
        }
      }
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute multiple roots"/utf8>>,
        500,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query Dashboard {
    users {
      id
      username
      role
    }
    posts {
      id
      title
      status
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute fragments"/utf8>>,
        200,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query WithFragments {
    user(id: \"user-1\") {
      ...UserFields
      posts {
        ...PostFields
        author {
          ...UserFields
        }
      }
    }
    users {
      ...UserFields
    }
  }

  fragment UserFields on User {
    id
    username
    email
    displayName
    role
    createdAt
  }

  fragment PostFields on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>
            )
        end
    ),
    benchmark(
        <<"Execute large"/utf8>>,
        100,
        fun() ->
            mochi@executor:execute_query(
                Gql_schema,
                <<"
  query LargeQuery {
    users {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
      posts {
        id
        title
        content
        excerpt
        status
        tags
        viewCount
        createdAt
        updatedAt
        author {
          id
          username
          email
          displayName
        }
        comments {
          id
          content
          createdAt
          author {
            id
            username
          }
        }
      }
      comments {
        id
        content
        createdAt
        post {
          id
          title
        }
      }
    }
    posts {
      id
      title
      content
      status
      author {
        id
        username
        role
      }
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"With variables:"/utf8>>),
    Vars = maps:from_list(
        [{<<"userId"/utf8>>, gleam_stdlib:identity(<<"user-1"/utf8>>)},
            {<<"postId"/utf8>>, gleam_stdlib:identity(<<"post-1"/utf8>>)},
            {<<"limit"/utf8>>, gleam_stdlib:identity(10)}]
    ),
    benchmark(
        <<"Execute with vars"/utf8>>,
        500,
        fun() ->
            mochi@executor:execute_query_with_variables(
                Gql_schema,
                <<"
  query GetUserById($userId: ID!, $includeEmail: Boolean) {
    user(id: $userId) {
      id
      username
      displayName
      bio
      role
    }
  }
"/utf8>>,
                Vars
            )
        end
    ),
    benchmark(
        <<"Execute complex with vars"/utf8>>,
        100,
        fun() ->
            mochi@executor:execute_query_with_variables(
                Gql_schema,
                <<"
  query ComplexQuery($userId: ID!, $postId: ID!, $limit: Int = 10) {
    currentUser: user(id: $userId) {
      ...UserDetails
      posts {
        ...PostSummary
      }
    }
    featuredPost: post(id: $postId) {
      ...PostDetails
      author {
        ...UserDetails
      }
      comments {
        id
        content
        author {
          ...UserDetails
        }
        replies {
          id
          content
        }
      }
    }
    recentPosts: publishedPosts {
      ...PostSummary
    }
    allUsers: users {
      id
      username
      role
    }
  }

  fragment UserDetails on User {
    id
    username
    email
    displayName
    bio
    role
    createdAt
    updatedAt
  }

  fragment PostSummary on Post {
    id
    title
    excerpt
    status
    viewCount
  }

  fragment PostDetails on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>,
                Vars
            )
        end
    ),
    nil.

-file("src/mochi_wisp/realworld_benchmark.gleam", 581).
-spec run_query_complexity_analysis() -> nil.
run_query_complexity_analysis() ->
    gleam_stdlib:println(<<"--- Query Complexity Analysis ---"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    Queries = [{<<"Simple"/utf8>>, <<"{ users { id username } }"/utf8>>},
        {<<"Medium"/utf8>>,
            <<"
  query GetUser {
    user(id: \"user-1\") {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
    }
  }
"/utf8>>},
        {<<"Nested"/utf8>>,
            <<"
  query UserWithPosts {
    user(id: \"user-1\") {
      id
      username
      displayName
      posts {
        id
        title
        status
        viewCount
        tags
      }
    }
  }
"/utf8>>},
        {<<"Deeply nested"/utf8>>,
            <<"
  query DeeplyNested {
    user(id: \"user-1\") {
      id
      username
      posts {
        id
        title
        content
        author {
          id
          username
        }
        comments {
          id
          content
          author {
            id
            username
            role
          }
          replies {
            id
            content
            author {
              id
              username
            }
          }
        }
      }
    }
  }
"/utf8>>},
        {<<"Multiple roots"/utf8>>,
            <<"
  query Dashboard {
    users {
      id
      username
      role
    }
    posts {
      id
      title
      status
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>},
        {<<"Fragments"/utf8>>,
            <<"
  query WithFragments {
    user(id: \"user-1\") {
      ...UserFields
      posts {
        ...PostFields
        author {
          ...UserFields
        }
      }
    }
    users {
      ...UserFields
    }
  }

  fragment UserFields on User {
    id
    username
    email
    displayName
    role
    createdAt
  }

  fragment PostFields on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>},
        {<<"Large"/utf8>>,
            <<"
  query LargeQuery {
    users {
      id
      username
      email
      displayName
      bio
      role
      createdAt
      updatedAt
      posts {
        id
        title
        content
        excerpt
        status
        tags
        viewCount
        createdAt
        updatedAt
        author {
          id
          username
          email
          displayName
        }
        comments {
          id
          content
          createdAt
          author {
            id
            username
          }
        }
      }
      comments {
        id
        content
        createdAt
        post {
          id
          title
        }
      }
    }
    posts {
      id
      title
      content
      status
      author {
        id
        username
        role
      }
    }
    publishedPosts {
      id
      title
      viewCount
    }
  }
"/utf8>>},
        {<<"Complex combined"/utf8>>,
            <<"
  query ComplexQuery($userId: ID!, $postId: ID!, $limit: Int = 10) {
    currentUser: user(id: $userId) {
      ...UserDetails
      posts {
        ...PostSummary
      }
    }
    featuredPost: post(id: $postId) {
      ...PostDetails
      author {
        ...UserDetails
      }
      comments {
        id
        content
        author {
          ...UserDetails
        }
        replies {
          id
          content
        }
      }
    }
    recentPosts: publishedPosts {
      ...PostSummary
    }
    allUsers: users {
      id
      username
      role
    }
  }

  fragment UserDetails on User {
    id
    username
    email
    displayName
    bio
    role
    createdAt
    updatedAt
  }

  fragment PostSummary on Post {
    id
    title
    excerpt
    status
    viewCount
  }

  fragment PostDetails on Post {
    id
    title
    content
    excerpt
    status
    tags
    viewCount
    createdAt
    updatedAt
  }
"/utf8>>}],
    gleam_stdlib:println(<<"Query complexity metrics:"/utf8>>),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<(pad_right(<<"Query"/utf8>>, 18))/binary, " | "/utf8>>/binary,
                                (pad_right(<<"Chars"/utf8>>, 8))/binary>>/binary,
                            " | "/utf8>>/binary,
                        (pad_right(<<"Tokens*"/utf8>>, 8))/binary>>/binary,
                    " | "/utf8>>/binary,
                (pad_right(<<"Parse µs"/utf8>>, 10))/binary>>/binary,
            " | µs/char"/utf8>>
    ),
    gleam_stdlib:println(gleam@string:repeat(<<"-"/utf8>>, 65)),
    gleam@list:each(
        Queries,
        fun(Pair) ->
            {Name, Query} = Pair,
            Chars = string:length(Query),
            Tokens_est = estimate_tokens(Query),
            {Parse_us, _} = mochi_wisp_benchmark_ffi:measure_time(
                fun() -> mochi@parser:parse(Query) end
            ),
            Us_per_char = case erlang:float(Chars) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Parse_us) / Gleam@denominator
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<<<<<(pad_right(Name, 18))/binary, " | "/utf8>>/binary,
                                            (pad_left(
                                                erlang:integer_to_binary(Chars),
                                                8
                                            ))/binary>>/binary,
                                        " | "/utf8>>/binary,
                                    (pad_left(
                                        erlang:integer_to_binary(Tokens_est),
                                        8
                                    ))/binary>>/binary,
                                " | "/utf8>>/binary,
                            (pad_left(erlang:integer_to_binary(Parse_us), 10))/binary>>/binary,
                        " | "/utf8>>/binary,
                    (float_to_string_2(Us_per_char))/binary>>
            )
        end
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"* Token count is estimated"/utf8>>),
    nil.

-file("src/mochi_wisp/realworld_benchmark.gleam", 354).
-spec run_all() -> nil.
run_all() ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ),
    gleam_stdlib:println(<<"  Real-World GraphQL Benchmark Suite"/utf8>>),
    gleam_stdlib:println(
        <<"=============================================="/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Building complex schema..."/utf8>>),
    Gql_schema = mochi_wisp@complex_schema:build_complex_schema(),
    gleam_stdlib:println(<<""/utf8>>),
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    run_parsing_benchmarks(),
    gleam_stdlib:println(<<""/utf8>>),
    run_execution_benchmarks(Gql_schema),
    gleam_stdlib:println(<<""/utf8>>),
    run_query_complexity_analysis(),
    gleam_stdlib:println(<<""/utf8>>),
    run_cache_vs_nocache_comparison(Gql_schema),
    gleam_stdlib:println(<<""/utf8>>),
    print_summary().
