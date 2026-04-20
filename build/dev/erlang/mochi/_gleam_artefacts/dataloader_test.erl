-module(dataloader_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/dataloader_test.gleam").
-export([default_options_test/0, new_creates_loader_test/0, new_with_options_test/0, load_single_value_test/0, load_not_found_test/0, load_many_test/0, load_many_with_errors_test/0, load_many_empty_list_test/0, cache_returns_cached_value_test/0, clear_cache_test/0, clear_key_test/0, prime_adds_to_cache_test/0, prime_error_adds_error_to_cache_test/0, int_loader_success_test/0, int_loader_not_found_test/0, string_loader_success_test/0, string_loader_not_found_test/0, int_batch_loader_test/0, string_batch_loader_test/0, batch_loader_error_test/0, int_loader_result_success_test/0, int_loader_result_not_found_test/0, string_loader_result_success_test/0, string_loader_result_not_found_test/0, int_key_test/0, string_key_test/0, duplicate_keys_deduplicated_test/0, cache_disabled_test/0, load_many_makes_single_batch_call_test/0, load_many_skips_cached_keys_test/0, load_many_duplicate_keys_test/0, load_many_respects_max_batch_size_test/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("test/dataloader_test.gleam", 13).
?DOC(" Simple in-memory \"database\" for testing\n").
-spec find_by_id(integer()) -> {ok, binary()} | {error, binary()}.
find_by_id(Id) ->
    case Id of
        1 ->
            {ok, <<"Item 1"/utf8>>};

        2 ->
            {ok, <<"Item 2"/utf8>>};

        3 ->
            {ok, <<"Item 3"/utf8>>};

        _ ->
            {error, <<"Not found"/utf8>>}
    end.

-file("test/dataloader_test.gleam", 22).
-spec find_by_key(binary()) -> {ok, binary()} | {error, binary()}.
find_by_key(Key) ->
    case Key of
        <<"a"/utf8>> ->
            {ok, <<"Value A"/utf8>>};

        <<"b"/utf8>> ->
            {ok, <<"Value B"/utf8>>};

        <<"c"/utf8>> ->
            {ok, <<"Value C"/utf8>>};

        _ ->
            {error, <<"Not found"/utf8>>}
    end.

-file("test/dataloader_test.gleam", 31).
-spec to_dynamic(any()) -> gleam@dynamic:dynamic_().
to_dynamic(Value) ->
    gleam_stdlib:identity(Value).

-file("test/dataloader_test.gleam", 39).
-spec default_options_test() -> nil.
default_options_test() ->
    Opts = mochi@dataloader:default_options(),
    gleeunit@should:equal(erlang:element(2, Opts), 100),
    gleeunit@should:equal(erlang:element(3, Opts), true).

-file("test/dataloader_test.gleam", 49).
-spec new_creates_loader_test() -> nil.
new_creates_loader_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(K) -> {ok, K * 2} end)} end
    ),
    {_, Result} = mochi@dataloader:load(Loader, 5),
    gleeunit@should:equal(Result, {ok, 10}).

-file("test/dataloader_test.gleam", 58).
-spec new_with_options_test() -> nil.
new_with_options_test() ->
    Opts = {data_loader_options, 50, false},
    Loader = mochi@dataloader:new_with_options(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(K) -> {ok, K} end)} end,
        Opts
    ),
    {_, Result} = mochi@dataloader:load(Loader, 42),
    gleeunit@should:equal(Result, {ok, 42}).

-file("test/dataloader_test.gleam", 74).
-spec load_single_value_test() -> nil.
load_single_value_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(Id) -> case find_by_id(Id) of
                            {ok, V} ->
                                {ok, V};

                            {error, E} ->
                                {error, E}
                        end end)} end
    ),
    {_, Result} = mochi@dataloader:load(Loader, 1),
    gleeunit@should:equal(Result, {ok, <<"Item 1"/utf8>>}).

-file("test/dataloader_test.gleam", 90).
-spec load_not_found_test() -> nil.
load_not_found_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(Id) -> case find_by_id(Id) of
                            {ok, V} ->
                                {ok, V};

                            {error, E} ->
                                {error, E}
                        end end)} end
    ),
    {_, Result} = mochi@dataloader:load(Loader, 999),
    gleeunit@should:equal(Result, {error, <<"Not found"/utf8>>}).

-file("test/dataloader_test.gleam", 110).
-spec load_many_test() -> nil.
load_many_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(Id) -> case find_by_id(Id) of
                            {ok, V} ->
                                {ok, V};

                            {error, E} ->
                                {error, E}
                        end end)} end
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, [1, 2, 3]),
    gleeunit@should:equal(
        Results,
        [{ok, <<"Item 1"/utf8>>},
            {ok, <<"Item 2"/utf8>>},
            {ok, <<"Item 3"/utf8>>}]
    ).

-file("test/dataloader_test.gleam", 126).
-spec load_many_with_errors_test() -> nil.
load_many_with_errors_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(Id) -> case find_by_id(Id) of
                            {ok, V} ->
                                {ok, V};

                            {error, E} ->
                                {error, E}
                        end end)} end
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, [1, 999, 2]),
    gleeunit@should:equal(
        Results,
        [{ok, <<"Item 1"/utf8>>},
            {error, <<"Not found"/utf8>>},
            {ok, <<"Item 2"/utf8>>}]
    ).

-file("test/dataloader_test.gleam", 142).
-spec load_many_empty_list_test() -> nil.
load_many_empty_list_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(K) -> {ok, K} end)} end
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, []),
    gleeunit@should:equal(Results, []).

-file("test/dataloader_test.gleam", 153).
-spec cache_returns_cached_value_test() -> nil.
cache_returns_cached_value_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok, gleam@list:map(Keys, fun(Id) -> {ok, Id * 10} end)}
        end
    ),
    {Loader2, Result1} = mochi@dataloader:load(Loader, 5),
    gleeunit@should:equal(Result1, {ok, 50}),
    {_, Result2} = mochi@dataloader:load(Loader2, 5),
    gleeunit@should:equal(Result2, {ok, 50}).

-file("test/dataloader_test.gleam", 169).
-spec clear_cache_test() -> nil.
clear_cache_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok, gleam@list:map(Keys, fun(Id) -> {ok, Id * 10} end)}
        end
    ),
    {Loader2, _} = mochi@dataloader:load(Loader, 5),
    Loader3 = mochi@dataloader:clear_cache(Loader2),
    {_, Result2} = mochi@dataloader:load(Loader3, 5),
    gleeunit@should:equal(Result2, {ok, 50}).

-file("test/dataloader_test.gleam", 183).
-spec clear_key_test() -> nil.
clear_key_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok, gleam@list:map(Keys, fun(Id) -> {ok, Id * 10} end)}
        end
    ),
    {Loader2, _} = mochi@dataloader:load(Loader, 5),
    {Loader3, _} = mochi@dataloader:load(Loader2, 6),
    Loader4 = mochi@dataloader:clear_key(Loader3, 5),
    {_, Result} = mochi@dataloader:load(Loader4, 6),
    gleeunit@should:equal(Result, {ok, 60}).

-file("test/dataloader_test.gleam", 202).
-spec prime_adds_to_cache_test() -> nil.
prime_adds_to_cache_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok,
                gleam@list:map(
                    Keys,
                    fun(_) -> {error, <<"Should not be called"/utf8>>} end
                )}
        end
    ),
    Loader2 = mochi@dataloader:prime(Loader, 42, <<"Primed Value"/utf8>>),
    {_, Result} = mochi@dataloader:load(Loader2, 42),
    gleeunit@should:equal(Result, {ok, <<"Primed Value"/utf8>>}).

-file("test/dataloader_test.gleam", 213).
-spec prime_error_adds_error_to_cache_test() -> nil.
prime_error_adds_error_to_cache_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok,
                gleam@list:map(
                    Keys,
                    fun(_) -> {ok, <<"Should not be called"/utf8>>} end
                )}
        end
    ),
    Loader2 = mochi@dataloader:prime_error(Loader, 42, <<"Primed Error"/utf8>>),
    {_, Result} = mochi@dataloader:load(Loader2, 42),
    gleeunit@should:equal(Result, {error, <<"Primed Error"/utf8>>}).

-file("test/dataloader_test.gleam", 228).
-spec int_loader_success_test() -> gleam@dynamic:dynamic_().
int_loader_success_test() ->
    Loader = mochi@dataloader:int_loader(fun(Id) -> case find_by_id(Id) of
                {ok, V} ->
                    {ok, to_dynamic(V)};

                {error, E} ->
                    {error, E}
            end end),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(1)),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 241).
-spec int_loader_not_found_test() -> binary().
int_loader_not_found_test() ->
    Loader = mochi@dataloader:int_loader(fun(Id) -> case find_by_id(Id) of
                {ok, V} ->
                    {ok, to_dynamic(V)};

                {error, E} ->
                    {error, E}
            end end),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(999)),
    gleeunit@should:be_error(Result).

-file("test/dataloader_test.gleam", 258).
-spec string_loader_success_test() -> gleam@dynamic:dynamic_().
string_loader_success_test() ->
    Loader = mochi@dataloader:string_loader(fun(Key) -> case find_by_key(Key) of
                {ok, V} ->
                    {ok, to_dynamic(V)};

                {error, E} ->
                    {error, E}
            end end),
    {_, Result} = mochi@dataloader:load(
        Loader,
        mochi@dataloader:string_key(<<"a"/utf8>>)
    ),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 271).
-spec string_loader_not_found_test() -> binary().
string_loader_not_found_test() ->
    Loader = mochi@dataloader:string_loader(fun(Key) -> case find_by_key(Key) of
                {ok, V} ->
                    {ok, to_dynamic(V)};

                {error, E} ->
                    {error, E}
            end end),
    {_, Result} = mochi@dataloader:load(
        Loader,
        mochi@dataloader:string_key(<<"zzz"/utf8>>)
    ),
    gleeunit@should:be_error(Result).

-file("test/dataloader_test.gleam", 288).
-spec int_batch_loader_test() -> gleam@dynamic:dynamic_().
int_batch_loader_test() ->
    Loader = mochi@dataloader:int_batch_loader(
        fun(Ids) ->
            {ok, gleam@list:map(Ids, fun(Id) -> to_dynamic(Id * 100) end)}
        end
    ),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(5)),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 298).
-spec string_batch_loader_test() -> gleam@dynamic:dynamic_().
string_batch_loader_test() ->
    Loader = mochi@dataloader:string_batch_loader(
        fun(Keys) ->
            {ok,
                gleam@list:map(
                    Keys,
                    fun(K) -> to_dynamic(<<"Value: "/utf8, K/binary>>) end
                )}
        end
    ),
    {_, Result} = mochi@dataloader:load(
        Loader,
        mochi@dataloader:string_key(<<"test"/utf8>>)
    ),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 309).
-spec batch_loader_error_test() -> binary().
batch_loader_error_test() ->
    Loader = mochi@dataloader:int_batch_loader(
        fun(_) -> {error, <<"Database connection failed"/utf8>>} end
    ),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(1)),
    gleeunit@should:be_error(Result).

-file("test/dataloader_test.gleam", 323).
-spec int_loader_result_success_test() -> gleam@dynamic:dynamic_().
int_loader_result_success_test() ->
    Loader = mochi@dataloader:int_loader_result(
        fun find_by_id/1,
        fun to_dynamic/1,
        <<"Item not found"/utf8>>
    ),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(1)),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 331).
-spec int_loader_result_not_found_test() -> nil.
int_loader_result_not_found_test() ->
    Loader = mochi@dataloader:int_loader_result(
        fun find_by_id/1,
        fun to_dynamic/1,
        <<"Item not found"/utf8>>
    ),
    {_, Result} = mochi@dataloader:load(Loader, mochi@dataloader:int_key(999)),
    gleeunit@should:equal(Result, {error, <<"Item not found"/utf8>>}).

-file("test/dataloader_test.gleam", 339).
-spec string_loader_result_success_test() -> gleam@dynamic:dynamic_().
string_loader_result_success_test() ->
    Loader = mochi@dataloader:string_loader_result(
        fun find_by_key/1,
        fun to_dynamic/1,
        <<"Key not found"/utf8>>
    ),
    {_, Result} = mochi@dataloader:load(
        Loader,
        mochi@dataloader:string_key(<<"a"/utf8>>)
    ),
    gleeunit@should:be_ok(Result).

-file("test/dataloader_test.gleam", 347).
-spec string_loader_result_not_found_test() -> nil.
string_loader_result_not_found_test() ->
    Loader = mochi@dataloader:string_loader_result(
        fun find_by_key/1,
        fun to_dynamic/1,
        <<"Key not found"/utf8>>
    ),
    {_, Result} = mochi@dataloader:load(
        Loader,
        mochi@dataloader:string_key(<<"zzz"/utf8>>)
    ),
    gleeunit@should:equal(Result, {error, <<"Key not found"/utf8>>}).

-file("test/dataloader_test.gleam", 359).
-spec int_key_test() -> nil.
int_key_test() ->
    _ = mochi@dataloader:int_key(42),
    gleeunit@should:be_true(true).

-file("test/dataloader_test.gleam", 365).
-spec string_key_test() -> nil.
string_key_test() ->
    _ = mochi@dataloader:string_key(<<"hello"/utf8>>),
    gleeunit@should:be_true(true).

-file("test/dataloader_test.gleam", 375).
-spec duplicate_keys_deduplicated_test() -> nil.
duplicate_keys_deduplicated_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            {ok, gleam@list:map(Keys, fun(Id) -> {ok, Id * 10} end)}
        end
    ),
    {Loader2, Result1} = mochi@dataloader:load(Loader, 5),
    {_, Result2} = mochi@dataloader:load(Loader2, 5),
    gleeunit@should:equal(Result1, {ok, 50}),
    gleeunit@should:equal(Result2, {ok, 50}).

-file("test/dataloader_test.gleam", 388).
-spec cache_disabled_test() -> nil.
cache_disabled_test() ->
    Opts = {data_loader_options, 100, false},
    Loader = mochi@dataloader:new_with_options(
        fun(Keys) -> {ok, gleam@list:map(Keys, fun(K) -> {ok, K * 10} end)} end,
        Opts
    ),
    {Loader2, Result1} = mochi@dataloader:load(Loader, 5),
    {_, Result2} = mochi@dataloader:load(Loader2, 5),
    gleeunit@should:equal(Result1, {ok, 50}),
    gleeunit@should:equal(Result2, {ok, 50}).

-file("test/dataloader_test.gleam", 407).
-spec load_many_makes_single_batch_call_test() -> nil.
load_many_makes_single_batch_call_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            gleeunit@should:equal(Keys, [1, 2, 3]),
            {ok, gleam@list:map(Keys, fun(K) -> {ok, K * 10} end)}
        end
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, [1, 2, 3]),
    gleeunit@should:equal(Results, [{ok, 10}, {ok, 20}, {ok, 30}]).

-file("test/dataloader_test.gleam", 418).
-spec load_many_skips_cached_keys_test() -> nil.
load_many_skips_cached_keys_test() ->
    Loader = mochi@dataloader:new_with_options(
        fun(Keys) ->
            gleeunit@should:be_false(gleam@list:contains(Keys, 2)),
            {ok, gleam@list:map(Keys, fun(K) -> {ok, K * 10} end)}
        end,
        {data_loader_options, 100, true}
    ),
    Loader2 = mochi@dataloader:prime(Loader, 2, 20),
    {_, Results} = mochi@dataloader:load_many(Loader2, [1, 2, 3]),
    gleeunit@should:equal(Results, [{ok, 10}, {ok, 20}, {ok, 30}]).

-file("test/dataloader_test.gleam", 433).
-spec load_many_duplicate_keys_test() -> nil.
load_many_duplicate_keys_test() ->
    Loader = mochi@dataloader:new(
        fun(Keys) ->
            gleeunit@should:equal(Keys, [1, 2]),
            {ok, gleam@list:map(Keys, fun(K) -> {ok, K * 10} end)}
        end
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, [1, 1, 2]),
    gleeunit@should:equal(Results, [{ok, 10}, {ok, 10}, {ok, 20}]).

-file("test/dataloader_test.gleam", 445).
-spec load_many_respects_max_batch_size_test() -> nil.
load_many_respects_max_batch_size_test() ->
    Call_count = gleam_stdlib:identity(0),
    _ = Call_count,
    Opts = {data_loader_options, 2, true},
    Loader = mochi@dataloader:new_with_options(
        fun(Keys) ->
            gleeunit@should:be_true(erlang:length(Keys) =< 2),
            {ok, gleam@list:map(Keys, fun(K) -> {ok, K} end)}
        end,
        Opts
    ),
    {_, Results} = mochi@dataloader:load_many(Loader, [1, 2, 3, 4]),
    gleeunit@should:equal(Results, [{ok, 1}, {ok, 2}, {ok, 3}, {ok, 4}]).
