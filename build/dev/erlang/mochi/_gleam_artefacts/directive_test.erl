-module(directive_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/directive_test.gleam").
-export([skip_directive_true_test/0, skip_directive_false_test/0, skip_directive_with_variable_true_test/0, skip_directive_with_variable_false_test/0, include_directive_true_test/0, include_directive_false_test/0, include_directive_with_variable_true_test/0, include_directive_with_variable_false_test/0, multiple_directives_on_field_test/0, no_directives_test/0, skip_true_field_absent_test/0, skip_false_field_present_test/0, include_true_field_present_test/0, include_false_field_absent_test/0, skip_variable_true_field_absent_test/0, include_variable_false_field_absent_test/0]).
-export_type([user/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type user() :: {user, binary(), binary(), boolean()}.

-file("test/directive_test.gleam", 43).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>, true}}.

-file("test/directive_test.gleam", 21).
-spec build_test_schema() -> mochi@schema:schema().
build_test_schema() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(3, U@1) end
        ),
        _pipe@3 = mochi@types:bool(
            _pipe@2,
            <<"active"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"user"/utf8>>,
        {named, <<"User"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    {user, <<"1"/utf8>>, <<"Alice"/utf8>>, true}
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/directive_test.gleam", 47).
-spec execute_query(mochi@schema:schema(), binary()) -> mochi@executor:execution_result().
execute_query(Schema_def, Query_str) ->
    mochi@executor:execute_query(Schema_def, Query_str).

-file("test/directive_test.gleam", 54).
-spec execute_query_with_vars(
    mochi@schema:schema(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> mochi@executor:execution_result().
execute_query_with_vars(Schema_def, Query_str, Vars) ->
    mochi@executor:execute_query_with_variables(Schema_def, Query_str, Vars).

-file("test/directive_test.gleam", 66).
-spec skip_directive_true_test() -> nil.
skip_directive_true_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name @skip(if: true)
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 83).
-spec skip_directive_false_test() -> nil.
skip_directive_false_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name @skip(if: false)
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 100).
-spec skip_directive_with_variable_true_test() -> nil.
skip_directive_with_variable_true_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser($skipName: Boolean!) {
      user {
        id
        name @skip(if: $skipName)
      }
    }
    "/utf8>>,
    Vars = maps:from_list([{<<"skipName"/utf8>>, gleam_stdlib:identity(true)}]),
    Result = execute_query_with_vars(Schema_def, Query_str, Vars),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 118).
-spec skip_directive_with_variable_false_test() -> nil.
skip_directive_with_variable_false_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser($skipName: Boolean!) {
      user {
        id
        name @skip(if: $skipName)
      }
    }
    "/utf8>>,
    Vars = maps:from_list([{<<"skipName"/utf8>>, gleam_stdlib:identity(false)}]),
    Result = execute_query_with_vars(Schema_def, Query_str, Vars),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 140).
-spec include_directive_true_test() -> nil.
include_directive_true_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name @include(if: true)
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 157).
-spec include_directive_false_test() -> nil.
include_directive_false_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name @include(if: false)
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 174).
-spec include_directive_with_variable_true_test() -> nil.
include_directive_with_variable_true_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser($includeName: Boolean!) {
      user {
        id
        name @include(if: $includeName)
      }
    }
    "/utf8>>,
    Vars = maps:from_list(
        [{<<"includeName"/utf8>>, gleam_stdlib:identity(true)}]
    ),
    Result = execute_query_with_vars(Schema_def, Query_str, Vars),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 192).
-spec include_directive_with_variable_false_test() -> nil.
include_directive_with_variable_false_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser($includeName: Boolean!) {
      user {
        id
        name @include(if: $includeName)
      }
    }
    "/utf8>>,
    Vars = maps:from_list(
        [{<<"includeName"/utf8>>, gleam_stdlib:identity(false)}]
    ),
    Result = execute_query_with_vars(Schema_def, Query_str, Vars),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 214).
-spec multiple_directives_on_field_test() -> nil.
multiple_directives_on_field_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name @skip(if: false) @include(if: true)
        active
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 232).
-spec no_directives_test() -> nil.
no_directives_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name
        active
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/directive_test.gleam", 266).
-spec get_nested(gleam@dynamic:dynamic_(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, nil}.
get_nested(Data, Path) ->
    case Path of
        [] ->
            {ok, Data};

        [Key | Rest] ->
            Direct = begin
                _pipe = gleam@dynamic@decode:run(
                    Data,
                    gleam@dynamic@decode:at(
                        [Key],
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ),
                gleam@result:map_error(_pipe, fun(_) -> nil end)
            end,
            case Direct of
                {ok, V} ->
                    get_nested(V, Rest);

                {error, _} ->
                    case gleam@dynamic@decode:run(
                        Data,
                        gleam@dynamic@decode:list(
                            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                        )
                    ) of
                        {ok, Items} ->
                            gleam@list:find_map(
                                Items,
                                fun(Item) ->
                                    case gleam@dynamic@decode:run(
                                        Item,
                                        gleam@dynamic@decode:at(
                                            [Key],
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_dynamic/1}
                                        )
                                    ) of
                                        {ok, V@1} ->
                                            _pipe@1 = get_nested(V@1, Rest),
                                            gleam@result:map_error(
                                                _pipe@1,
                                                fun(_) -> nil end
                                            );

                                        {error, _} ->
                                            {error, nil}
                                    end
                                end
                            );

                        {error, _} ->
                            {error, nil}
                    end
            end
    end.

-file("test/directive_test.gleam", 256).
?DOC(
    " Extract a string field from a nested path in execution result data.\n"
    " The executor returns data as a list of field dicts; we search through them.\n"
).
-spec get_field_from_result(mochi@executor:execution_result(), list(binary())) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, nil}.
get_field_from_result(Result, Path) ->
    case erlang:element(2, Result) of
        none ->
            {error, nil};

        {some, Data} ->
            get_nested(Data, Path)
    end.

-file("test/directive_test.gleam", 298).
-spec skip_true_field_absent_test() -> nil.
skip_true_field_absent_test() ->
    Schema_def = build_test_schema(),
    Result = execute_query(
        Schema_def,
        <<"query { user { id name @skip(if: true) } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_error(Name_field).

-file("test/directive_test.gleam", 309).
-spec skip_false_field_present_test() -> gleam@dynamic:dynamic_().
skip_false_field_present_test() ->
    Schema_def = build_test_schema(),
    Result = execute_query(
        Schema_def,
        <<"query { user { id name @skip(if: false) } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_ok(Name_field).

-file("test/directive_test.gleam", 319).
-spec include_true_field_present_test() -> gleam@dynamic:dynamic_().
include_true_field_present_test() ->
    Schema_def = build_test_schema(),
    Result = execute_query(
        Schema_def,
        <<"query { user { id name @include(if: true) } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_ok(Name_field).

-file("test/directive_test.gleam", 329).
-spec include_false_field_absent_test() -> nil.
include_false_field_absent_test() ->
    Schema_def = build_test_schema(),
    Result = execute_query(
        Schema_def,
        <<"query { user { id name @include(if: false) } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_error(Name_field).

-file("test/directive_test.gleam", 339).
-spec skip_variable_true_field_absent_test() -> nil.
skip_variable_true_field_absent_test() ->
    Schema_def = build_test_schema(),
    Vars = maps:from_list([{<<"skip"/utf8>>, gleam_stdlib:identity(true)}]),
    Result = execute_query_with_vars(
        Schema_def,
        <<"query Q($skip: Boolean!) { user { id name @skip(if: $skip) } }"/utf8>>,
        Vars
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_error(Name_field).

-file("test/directive_test.gleam", 354).
-spec include_variable_false_field_absent_test() -> nil.
include_variable_false_field_absent_test() ->
    Schema_def = build_test_schema(),
    Vars = maps:from_list([{<<"inc"/utf8>>, gleam_stdlib:identity(false)}]),
    Result = execute_query_with_vars(
        Schema_def,
        <<"query Q($inc: Boolean!) { user { id name @include(if: $inc) } }"/utf8>>,
        Vars
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    Name_field = get_field_from_result(
        Result,
        [<<"user"/utf8>>, <<"name"/utf8>>]
    ),
    gleeunit@should:be_error(Name_field).
