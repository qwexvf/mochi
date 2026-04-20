-module(fragment_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/fragment_test.gleam").
-export([fragment_spread_basic_test/0, fragment_spread_with_all_fields_test/0, fragment_spread_mixed_with_fields_test/0, undefined_fragment_error_test/0, inline_fragment_with_type_condition_test/0, inline_fragment_without_type_condition_test/0, inline_fragment_type_mismatch_test/0, nested_fragments_test/0, multiple_fragments_test/0, duplicate_field_merged_test/0, duplicate_field_with_same_alias_test/0, field_from_fragment_deduplication_test/0, execute_with_operation_name_test/0, execute_with_operation_name_second_operation_test/0, execute_with_operation_name_not_found_test/0, execute_anonymous_when_alone_test/0, inline_fragment_without_type_multiple_fields_test/0, inline_fragment_without_type_with_directive_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), binary()}.

-file("test/fragment_test.gleam", 44).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>}}.

-file("test/fragment_test.gleam", 20).
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
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"email"/utf8>>,
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
                    {user,
                        <<"1"/utf8>>,
                        <<"Alice"/utf8>>,
                        <<"alice@example.com"/utf8>>}
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/fragment_test.gleam", 48).
-spec execute_query(mochi@schema:schema(), binary()) -> mochi@executor:execution_result().
execute_query(Schema_def, Query_str) ->
    mochi@executor:execute_query(Schema_def, Query_str).

-file("test/fragment_test.gleam", 59).
-spec fragment_spread_basic_test() -> nil.
fragment_spread_basic_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ...UserFields
      }
    }

    fragment UserFields on User {
      id
      name
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 80).
-spec fragment_spread_with_all_fields_test() -> nil.
fragment_spread_with_all_fields_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ...AllUserFields
      }
    }

    fragment AllUserFields on User {
      id
      name
      email
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 102).
-spec fragment_spread_mixed_with_fields_test() -> nil.
fragment_spread_mixed_with_fields_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        ...NameFragment
      }
    }

    fragment NameFragment on User {
      name
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 123).
-spec undefined_fragment_error_test() -> nil.
undefined_fragment_error_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ...UndefinedFragment
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_false(gleam@list:is_empty(erlang:element(3, Result))).

-file("test/fragment_test.gleam", 142).
-spec inline_fragment_with_type_condition_test() -> nil.
inline_fragment_with_type_condition_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        ... on User {
          name
          email
        }
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 162).
-spec inline_fragment_without_type_condition_test() -> nil.
inline_fragment_without_type_condition_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        ... {
          name
        }
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 181).
-spec inline_fragment_type_mismatch_test() -> nil.
inline_fragment_type_mismatch_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        ... on OtherType {
          name
        }
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))).

-file("test/fragment_test.gleam", 205).
-spec nested_fragments_test() -> nil.
nested_fragments_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ...UserBasic
      }
    }

    fragment UserBasic on User {
      id
      ...UserName
    }

    fragment UserName on User {
      name
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 230).
-spec multiple_fragments_test() -> nil.
multiple_fragments_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ...IdFragment
        ...NameFragment
        ...EmailFragment
      }
    }

    fragment IdFragment on User {
      id
    }

    fragment NameFragment on User {
      name
    }

    fragment EmailFragment on User {
      email
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 264).
-spec duplicate_field_merged_test() -> nil.
duplicate_field_merged_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        id
        name
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 283).
-spec duplicate_field_with_same_alias_test() -> nil.
duplicate_field_with_same_alias_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        userId: id
        userId: id
        name
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 302).
-spec field_from_fragment_deduplication_test() -> nil.
field_from_fragment_deduplication_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        name
        ...UserFields
      }
    }

    fragment UserFields on User {
      id
      name
      email
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 331).
-spec execute_with_operation_name_test() -> nil.
execute_with_operation_name_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser {
      user {
        id
        name
      }
    }

    query GetUserEmail {
      user {
        id
        email
      }
    }
    "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Document} ->
            Ctx = mochi@schema:execution_context(
                gleam_stdlib:identity(maps:new())
            ),
            Result = mochi@executor:execute_with_operation_name(
                Schema_def,
                Document,
                none,
                Ctx,
                maps:new(),
                {some, <<"GetUser"/utf8>>}
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"fragment_test"/utf8>>,
                    function => <<"execute_with_operation_name_test"/utf8>>,
                    line => 367})
    end.

-file("test/fragment_test.gleam", 371).
-spec execute_with_operation_name_second_operation_test() -> nil.
execute_with_operation_name_second_operation_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser {
      user {
        id
        name
      }
    }

    query GetUserEmail {
      user {
        id
        email
      }
    }
    "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Document} ->
            Ctx = mochi@schema:execution_context(
                gleam_stdlib:identity(maps:new())
            ),
            Result = mochi@executor:execute_with_operation_name(
                Schema_def,
                Document,
                none,
                Ctx,
                maps:new(),
                {some, <<"GetUserEmail"/utf8>>}
            ),
            gleeunit@should:be_true(
                gleam@option:is_some(erlang:element(2, Result))
            ),
            gleeunit@should:equal(erlang:element(3, Result), []);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"fragment_test"/utf8>>,
                    function => <<"execute_with_operation_name_second_operation_test"/utf8>>,
                    line => 407})
    end.

-file("test/fragment_test.gleam", 411).
-spec execute_with_operation_name_not_found_test() -> nil.
execute_with_operation_name_not_found_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query GetUser {
      user {
        id
        name
      }
    }
    "/utf8>>,
    case mochi@parser:parse(Query_str) of
        {ok, Document} ->
            Ctx = mochi@schema:execution_context(
                gleam_stdlib:identity(maps:new())
            ),
            Result = mochi@executor:execute_with_operation_name(
                Schema_def,
                Document,
                none,
                Ctx,
                maps:new(),
                {some, <<"NonExistent"/utf8>>}
            ),
            gleeunit@should:be_false(
                gleam@list:is_empty(erlang:element(3, Result))
            );

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"fragment_test"/utf8>>,
                    function => <<"execute_with_operation_name_not_found_test"/utf8>>,
                    line => 440})
    end.

-file("test/fragment_test.gleam", 444).
-spec execute_anonymous_when_alone_test() -> nil.
execute_anonymous_when_alone_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    {
      user {
        id
        name
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 466).
-spec inline_fragment_without_type_multiple_fields_test() -> nil.
inline_fragment_without_type_multiple_fields_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        ... {
          id
          name
          email
        }
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/fragment_test.gleam", 487).
-spec inline_fragment_without_type_with_directive_test() -> nil.
inline_fragment_without_type_with_directive_test() ->
    Schema_def = build_test_schema(),
    Query_str = <<"
    query {
      user {
        id
        ... @skip(if: false) {
          name
        }
      }
    }
    "/utf8>>,
    Result = execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).
