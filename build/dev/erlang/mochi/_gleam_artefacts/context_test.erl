-module(context_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/context_test.gleam").
-export([request_info_basic_test/0, get_header_found_test/0, get_header_case_insensitive_test/0, get_header_not_found_test/0, get_authorization_test/0, get_bearer_token_test/0, get_bearer_token_lowercase_test/0, get_bearer_token_no_auth_test/0, get_bearer_token_wrong_scheme_test/0, new_pipeline_test/0, add_builder_test/0, pipeline_runs_in_order_test/0, pipeline_stops_on_error_test/0, try_build_context_returns_initial_on_error_test/0, to_dynamic_test/0, bearer_token_builder_test/0, headers_builder_test/0, require_bearer_token_present_test/0, require_bearer_token_missing_test/0, require_bearer_token_wrong_scheme_test/0, add_to_context_or_success_test/0, add_to_context_or_fallback_test/0, request_metadata_builder_test/0, pipeline_with_multiple_builders_test/0, pipeline_require_bearer_blocks_without_token_test/0]).

-file("test/context_test.gleam", 12).
-spec request_info_basic_test() -> nil.
request_info_basic_test() ->
    Headers = maps:from_list(
        [{<<"content-type"/utf8>>, <<"application/json"/utf8>>}]
    ),
    Info = mochi@context:request_info(
        Headers,
        <<"POST"/utf8>>,
        <<"/graphql"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Info), <<"POST"/utf8>>),
    gleeunit@should:equal(erlang:element(4, Info), <<"/graphql"/utf8>>).

-file("test/context_test.gleam", 19).
-spec get_header_found_test() -> nil.
get_header_found_test() ->
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer token123"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:get_header(Info, <<"authorization"/utf8>>),
    gleeunit@should:equal(Result, {ok, <<"Bearer token123"/utf8>>}).

-file("test/context_test.gleam", 26).
-spec get_header_case_insensitive_test() -> nil.
get_header_case_insensitive_test() ->
    Headers = maps:from_list(
        [{<<"Content-Type"/utf8>>, <<"application/json"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:get_header(Info, <<"content-type"/utf8>>),
    gleeunit@should:equal(Result, {ok, <<"application/json"/utf8>>}).

-file("test/context_test.gleam", 34).
-spec get_header_not_found_test() -> nil.
get_header_not_found_test() ->
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:get_header(Info, <<"missing-header"/utf8>>),
    gleeunit@should:equal(Result, {error, nil}).

-file("test/context_test.gleam", 40).
-spec get_authorization_test() -> nil.
get_authorization_test() ->
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer mytoken"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"POST"/utf8>>, <<"/"/utf8>>),
    gleeunit@should:equal(
        mochi@context:get_authorization(Info),
        {ok, <<"Bearer mytoken"/utf8>>}
    ).

-file("test/context_test.gleam", 46).
-spec get_bearer_token_test() -> nil.
get_bearer_token_test() ->
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer secret-token"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"POST"/utf8>>, <<"/"/utf8>>),
    gleeunit@should:equal(
        mochi@context:get_bearer_token(Info),
        {ok, <<"secret-token"/utf8>>}
    ).

-file("test/context_test.gleam", 52).
-spec get_bearer_token_lowercase_test() -> nil.
get_bearer_token_lowercase_test() ->
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"bearer token-lowercase"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"POST"/utf8>>, <<"/"/utf8>>),
    gleeunit@should:equal(
        mochi@context:get_bearer_token(Info),
        {ok, <<"token-lowercase"/utf8>>}
    ).

-file("test/context_test.gleam", 58).
-spec get_bearer_token_no_auth_test() -> nil.
get_bearer_token_no_auth_test() ->
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    gleeunit@should:equal(mochi@context:get_bearer_token(Info), {error, nil}).

-file("test/context_test.gleam", 63).
-spec get_bearer_token_wrong_scheme_test() -> nil.
get_bearer_token_wrong_scheme_test() ->
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Basic abc123"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"POST"/utf8>>, <<"/"/utf8>>),
    gleeunit@should:equal(mochi@context:get_bearer_token(Info), {error, nil}).

-file("test/context_test.gleam", 73).
-spec new_pipeline_test() -> nil.
new_pipeline_test() ->
    Pipeline = mochi@context:new_pipeline(),
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    gleeunit@should:equal(Result, {ok, maps:new()}).

-file("test/context_test.gleam", 80).
-spec add_builder_test() -> nil.
add_builder_test() ->
    Builder = fun(_, Ctx) ->
        {ok,
            gleam@dict:insert(
                Ctx,
                <<"key"/utf8>>,
                gleam_stdlib:identity(<<"value"/utf8>>)
            )}
    end,
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        mochi@context:add_builder(_pipe, Builder)
    end,
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    case Result of
        {ok, Ctx@1} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx@1, <<"key"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 94).
-spec pipeline_runs_in_order_test() -> nil.
pipeline_runs_in_order_test() ->
    Builder1 = fun(_, Ctx) ->
        {ok, gleam@dict:insert(Ctx, <<"step"/utf8>>, gleam_stdlib:identity(1))}
    end,
    Builder2 = fun(_, Ctx@1) ->
        {ok,
            gleam@dict:insert(Ctx@1, <<"step"/utf8>>, gleam_stdlib:identity(2))}
    end,
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        _pipe@1 = mochi@context:add_builder(_pipe, Builder1),
        mochi@context:add_builder(_pipe@1, Builder2)
    end,
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    case Result of
        {ok, _} ->
            nil;

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 115).
-spec pipeline_stops_on_error_test() -> nil.
pipeline_stops_on_error_test() ->
    Failing_builder = fun(_, _) -> {error, <<"Auth failed"/utf8>>} end,
    Should_not_run = fun(_, Ctx) ->
        {ok,
            gleam@dict:insert(Ctx, <<"ran"/utf8>>, gleam_stdlib:identity(true))}
    end,
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        _pipe@1 = mochi@context:add_builder(_pipe, Failing_builder),
        mochi@context:add_builder(_pipe@1, Should_not_run)
    end,
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    gleeunit@should:equal(Result, {error, <<"Auth failed"/utf8>>}).

-file("test/context_test.gleam", 135).
-spec try_build_context_returns_initial_on_error_test() -> nil.
try_build_context_returns_initial_on_error_test() ->
    Failing_builder = fun(_, _) -> {error, <<"Fail"/utf8>>} end,
    Initial = maps:from_list(
        [{<<"default"/utf8>>, gleam_stdlib:identity(<<"value"/utf8>>)}]
    ),
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        mochi@context:add_builder(_pipe, Failing_builder)
    end,
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:try_build_context(Pipeline, Info, Initial),
    gleeunit@should:equal(Result, Initial).

-file("test/context_test.gleam", 150).
-spec to_dynamic_test() -> nil.
to_dynamic_test() ->
    Ctx = maps:from_list(
        [{<<"user_id"/utf8>>, gleam_stdlib:identity(<<"123"/utf8>>)}]
    ),
    Dyn = mochi@context:to_dynamic(Ctx),
    gleeunit@should:not_equal(Dyn, gleam_stdlib:identity(none)).

-file("test/context_test.gleam", 161).
-spec bearer_token_builder_test() -> nil.
bearer_token_builder_test() ->
    Builder = mochi@context:bearer_token_builder(),
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer mytoken"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = Builder(Info, maps:new()),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"token"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 173).
-spec headers_builder_test() -> nil.
headers_builder_test() ->
    Builder = mochi@context:headers_builder(),
    Headers = maps:from_list(
        [{<<"content-type"/utf8>>, <<"application/json"/utf8>>},
            {<<"x-request-id"/utf8>>, <<"abc123"/utf8>>}]
    ),
    Info = mochi@context:request_info(
        Headers,
        <<"POST"/utf8>>,
        <<"/graphql"/utf8>>
    ),
    Result = Builder(Info, maps:new()),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"headers"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 193).
-spec require_bearer_token_present_test() -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
require_bearer_token_present_test() ->
    Builder = mochi@context:require_bearer_token(),
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer secret123"/utf8>>}]
    ),
    Info = mochi@context:request_info(
        Headers,
        <<"POST"/utf8>>,
        <<"/graphql"/utf8>>
    ),
    Result = Builder(Info, maps:new()),
    gleeunit@should:be_ok(Result).

-file("test/context_test.gleam", 201).
-spec require_bearer_token_missing_test() -> binary().
require_bearer_token_missing_test() ->
    Builder = mochi@context:require_bearer_token(),
    Info = mochi@context:request_info(
        maps:new(),
        <<"POST"/utf8>>,
        <<"/graphql"/utf8>>
    ),
    Result = Builder(Info, maps:new()),
    gleeunit@should:be_error(Result).

-file("test/context_test.gleam", 208).
-spec require_bearer_token_wrong_scheme_test() -> binary().
require_bearer_token_wrong_scheme_test() ->
    Builder = mochi@context:require_bearer_token(),
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Basic dXNlcjpwYXNz"/utf8>>}]
    ),
    Info = mochi@context:request_info(
        Headers,
        <<"POST"/utf8>>,
        <<"/graphql"/utf8>>
    ),
    Result = Builder(Info, maps:new()),
    gleeunit@should:be_error(Result).

-file("test/context_test.gleam", 220).
-spec add_to_context_or_success_test() -> nil.
add_to_context_or_success_test() ->
    Builder = mochi@context:add_to_context_or(
        <<"custom"/utf8>>,
        fun(Info) -> case mochi@context:get_header(Info, <<"x-custom"/utf8>>) of
                {ok, V} ->
                    {ok, gleam_stdlib:identity(V)};

                {error, _} ->
                    {error, <<"no header"/utf8>>}
            end end,
        gleam_stdlib:identity(<<"default"/utf8>>)
    ),
    Headers = maps:from_list([{<<"x-custom"/utf8>>, <<"my-value"/utf8>>}]),
    Info@1 = mochi@context:request_info(Headers, <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = Builder(Info@1, maps:new()),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"custom"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 242).
-spec add_to_context_or_fallback_test() -> nil.
add_to_context_or_fallback_test() ->
    Builder = mochi@context:add_to_context_or(
        <<"custom"/utf8>>,
        fun(_) -> {error, <<"not found"/utf8>>} end,
        gleam_stdlib:identity(<<"default"/utf8>>)
    ),
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = Builder(Info, maps:new()),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"custom"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 264).
-spec request_metadata_builder_test() -> nil.
request_metadata_builder_test() ->
    Builder = mochi@context:request_metadata_builder(),
    Info = mochi@context:request_info(
        maps:new(),
        <<"DELETE"/utf8>>,
        <<"/api/users/1"/utf8>>
    ),
    Result = Builder(Info, maps:new()),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"request"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 278).
-spec pipeline_with_multiple_builders_test() -> nil.
pipeline_with_multiple_builders_test() ->
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        _pipe@1 = mochi@context:add_builder(
            _pipe,
            mochi@context:headers_builder()
        ),
        _pipe@2 = mochi@context:add_builder(
            _pipe@1,
            mochi@context:bearer_token_builder()
        ),
        mochi@context:add_builder(
            _pipe@2,
            mochi@context:request_metadata_builder()
        )
    end,
    Headers = maps:from_list(
        [{<<"authorization"/utf8>>, <<"Bearer tok"/utf8>>}]
    ),
    Info = mochi@context:request_info(Headers, <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    case Result of
        {ok, Ctx} ->
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"headers"/utf8>>)),
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"token"/utf8>>)),
            gleeunit@should:be_true(gleam@dict:has_key(Ctx, <<"request"/utf8>>));

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/context_test.gleam", 298).
-spec pipeline_require_bearer_blocks_without_token_test() -> binary().
pipeline_require_bearer_blocks_without_token_test() ->
    Pipeline = begin
        _pipe = mochi@context:new_pipeline(),
        _pipe@1 = mochi@context:add_builder(
            _pipe,
            mochi@context:require_bearer_token()
        ),
        mochi@context:add_builder(_pipe@1, mochi@context:headers_builder())
    end,
    Info = mochi@context:request_info(maps:new(), <<"GET"/utf8>>, <<"/"/utf8>>),
    Result = mochi@context:build_context(Pipeline, Info, maps:new()),
    gleeunit@should:be_error(Result).
