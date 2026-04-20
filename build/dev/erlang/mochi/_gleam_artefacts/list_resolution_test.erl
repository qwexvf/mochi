-module(list_resolution_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/list_resolution_test.gleam").
-export([list_of_objects_with_scalar_fields_test/0, list_with_partial_field_selection_test/0, list_with_single_field_selection_test/0, list_of_objects_with_nested_object_fields_test/0, list_of_objects_nested_with_all_fields_test/0, list_with_arguments_test/0, list_with_default_arguments_test/0, empty_list_handling_test/0, non_null_list_type_test/0, multiple_list_fields_in_query_test/0, aliased_list_field_test/0, list_with_typename_test/0, list_of_objects_with_list_fields_test/0, deeply_nested_list_test/0, deeply_nested_list_partial_selection_test/0, single_item_list_test/0]).
-export_type([user/0, post/0, comment/0, user_with_tags/0, team/0]).

-type user() :: {user, binary(), binary(), integer()}.

-type post() :: {post, binary(), binary(), binary()}.

-type comment() :: {comment, binary(), binary()}.

-type user_with_tags() :: {user_with_tags, binary(), binary(), list(binary())}.

-type team() :: {team, binary(), binary()}.

-file("test/list_resolution_test.gleam", 38).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Test User"/utf8>>, 25}}.

-file("test/list_resolution_test.gleam", 42).
-spec decode_post(gleam@dynamic:dynamic_()) -> {ok, post()} | {error, binary()}.
decode_post(_) ->
    {ok, {post, <<"1"/utf8>>, <<"Test Post"/utf8>>, <<"1"/utf8>>}}.

-file("test/list_resolution_test.gleam", 46).
-spec decode_comment(gleam@dynamic:dynamic_()) -> {ok, comment()} |
    {error, binary()}.
decode_comment(_) ->
    {ok, {comment, <<"1"/utf8>>, <<"Test Comment"/utf8>>}}.

-file("test/list_resolution_test.gleam", 50).
-spec decode_user_with_tags(gleam@dynamic:dynamic_()) -> {ok, user_with_tags()} |
    {error, binary()}.
decode_user_with_tags(_) ->
    {ok,
        {user_with_tags,
            <<"1"/utf8>>,
            <<"Test User"/utf8>>,
            [<<"tag1"/utf8>>, <<"tag2"/utf8>>]}}.

-file("test/list_resolution_test.gleam", 58).
-spec build_users_list_schema() -> mochi@schema:schema().
build_users_list_schema() ->
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
        _pipe@3 = mochi@types:int(
            _pipe@2,
            <<"age"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)},
                                {<<"age"/utf8>>, gleam_stdlib:identity(30)}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"2"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Bob"/utf8>>)},
                                {<<"age"/utf8>>, gleam_stdlib:identity(25)}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"3"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Charlie"/utf8>>)},
                                {<<"age"/utf8>>, gleam_stdlib:identity(35)}]
                        )]
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/list_resolution_test.gleam", 101).
-spec list_of_objects_with_scalar_fields_test() -> nil.
list_of_objects_with_scalar_fields_test() ->
    Schema_def = build_users_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
        age
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 121).
-spec list_with_partial_field_selection_test() -> nil.
list_with_partial_field_selection_test() ->
    Schema_def = build_users_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 139).
-spec list_with_single_field_selection_test() -> nil.
list_with_single_field_selection_test() ->
    Schema_def = build_users_list_schema(),
    Query_str = <<"
    query {
      users {
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 160).
-spec build_posts_with_author_schema() -> mochi@schema:schema().
build_posts_with_author_schema() ->
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
        _pipe@3 = mochi@types:int(
            _pipe@2,
            <<"age"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    Post_type = begin
        _pipe@4 = mochi@types:object(<<"Post"/utf8>>),
        _pipe@5 = mochi@types:id(
            _pipe@4,
            <<"id"/utf8>>,
            fun(P) -> erlang:element(2, P) end
        ),
        _pipe@6 = mochi@types:string(
            _pipe@5,
            <<"title"/utf8>>,
            fun(P@1) -> erlang:element(3, P@1) end
        ),
        _pipe@7 = mochi@types:object_field(
            _pipe@6,
            <<"author"/utf8>>,
            <<"User"/utf8>>,
            fun(_) ->
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)},
                            {<<"name"/utf8>>,
                                gleam_stdlib:identity(<<"Author Name"/utf8>>)},
                            {<<"age"/utf8>>, gleam_stdlib:identity(30)}]
                    )
                )
            end
        ),
        mochi@types:build(_pipe@7, fun decode_post/1)
    end,
    Posts_query = mochi@query:'query'(
        <<"posts"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Post"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"post1"/utf8>>)},
                                {<<"title"/utf8>>,
                                    gleam_stdlib:identity(<<"First Post"/utf8>>)},
                                {<<"author"/utf8>>,
                                    gleam_stdlib:identity(
                                        maps:from_list(
                                            [{<<"id"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        <<"1"/utf8>>
                                                    )},
                                                {<<"name"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        <<"Alice"/utf8>>
                                                    )},
                                                {<<"age"/utf8>>,
                                                    gleam_stdlib:identity(30)}]
                                        )
                                    )}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"post2"/utf8>>)},
                                {<<"title"/utf8>>,
                                    gleam_stdlib:identity(
                                        <<"Second Post"/utf8>>
                                    )},
                                {<<"author"/utf8>>,
                                    gleam_stdlib:identity(
                                        maps:from_list(
                                            [{<<"id"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        <<"2"/utf8>>
                                                    )},
                                                {<<"name"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        <<"Bob"/utf8>>
                                                    )},
                                                {<<"age"/utf8>>,
                                                    gleam_stdlib:identity(25)}]
                                        )
                                    )}]
                        )]
                )}
        end,
        fun(P@2) -> gleam_stdlib:identity(P@2) end
    ),
    _pipe@8 = mochi@query:new(),
    _pipe@9 = mochi@query:add_query(_pipe@8, Posts_query),
    _pipe@10 = mochi@query:add_type(_pipe@9, User_type),
    _pipe@11 = mochi@query:add_type(_pipe@10, Post_type),
    mochi@query:build(_pipe@11).

-file("test/list_resolution_test.gleam", 231).
-spec list_of_objects_with_nested_object_fields_test() -> nil.
list_of_objects_with_nested_object_fields_test() ->
    Schema_def = build_posts_with_author_schema(),
    Query_str = <<"
    query {
      posts {
        id
        title
        author {
          id
          name
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 253).
-spec list_of_objects_nested_with_all_fields_test() -> nil.
list_of_objects_nested_with_all_fields_test() ->
    Schema_def = build_posts_with_author_schema(),
    Query_str = <<"
    query {
      posts {
        id
        title
        author {
          id
          name
          age
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 280).
-spec build_users_with_filter_schema() -> mochi@schema:schema().
build_users_with_filter_schema() ->
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
        _pipe@3 = mochi@types:int(
            _pipe@2,
            <<"age"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    Users_query = mochi@query:query_with_args(
        <<"users"/utf8>>,
        [mochi@query:arg(<<"minAge"/utf8>>, mochi@schema:int_type())],
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, 0} end,
        fun(_, _) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)},
                                {<<"age"/utf8>>, gleam_stdlib:identity(30)}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"2"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Bob"/utf8>>)},
                                {<<"age"/utf8>>, gleam_stdlib:identity(25)}]
                        )]
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/list_resolution_test.gleam", 323).
-spec list_with_arguments_test() -> nil.
list_with_arguments_test() ->
    Schema_def = build_users_with_filter_schema(),
    Query_str = <<"
    query {
      users(minAge: 28) {
        id
        name
        age
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 342).
-spec list_with_default_arguments_test() -> nil.
list_with_default_arguments_test() ->
    Schema_def = build_users_with_filter_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 364).
-spec build_empty_list_schema() -> mochi@schema:schema().
build_empty_list_schema() ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, gleam_stdlib:identity([])} end,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Users_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    mochi@query:build(_pipe@5).

-file("test/list_resolution_test.gleam", 388).
-spec empty_list_handling_test() -> nil.
empty_list_handling_test() ->
    Schema_def = build_empty_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 410).
-spec build_non_null_list_schema() -> mochi@schema:schema().
build_non_null_list_schema() ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:non_null(
            mochi@schema:list_type(
                mochi@schema:non_null(mochi@schema:named_type(<<"User"/utf8>>))
            )
        ),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)}]
                        )]
                )}
        end,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Users_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    mochi@query:build(_pipe@5).

-file("test/list_resolution_test.gleam", 442).
-spec non_null_list_type_test() -> nil.
non_null_list_type_test() ->
    Schema_def = build_non_null_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 464).
-spec build_multi_list_schema() -> mochi@schema:schema().
build_multi_list_schema() ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    Comment_type = begin
        _pipe@3 = mochi@types:object(<<"Comment"/utf8>>),
        _pipe@4 = mochi@types:id(
            _pipe@3,
            <<"id"/utf8>>,
            fun(C) -> erlang:element(2, C) end
        ),
        _pipe@5 = mochi@types:string(
            _pipe@4,
            <<"body"/utf8>>,
            fun(C@1) -> erlang:element(3, C@1) end
        ),
        mochi@types:build(_pipe@5, fun decode_comment/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)}]
                        )]
                )}
        end,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    Comments_query = mochi@query:'query'(
        <<"comments"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Comment"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"c1"/utf8>>)},
                                {<<"body"/utf8>>,
                                    gleam_stdlib:identity(
                                        <<"Great post!"/utf8>>
                                    )}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"c2"/utf8>>)},
                                {<<"body"/utf8>>,
                                    gleam_stdlib:identity(
                                        <<"Thanks for sharing"/utf8>>
                                    )}]
                        )]
                )}
        end,
        fun(C@2) -> gleam_stdlib:identity(C@2) end
    ),
    _pipe@6 = mochi@query:new(),
    _pipe@7 = mochi@query:add_query(_pipe@6, Users_query),
    _pipe@8 = mochi@query:add_query(_pipe@7, Comments_query),
    _pipe@9 = mochi@query:add_type(_pipe@8, User_type),
    _pipe@10 = mochi@query:add_type(_pipe@9, Comment_type),
    mochi@query:build(_pipe@10).

-file("test/list_resolution_test.gleam", 523).
-spec multiple_list_fields_in_query_test() -> nil.
multiple_list_fields_in_query_test() ->
    Schema_def = build_multi_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
      comments {
        id
        body
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 549).
-spec aliased_list_field_test() -> nil.
aliased_list_field_test() ->
    Schema_def = build_users_list_schema(),
    Query_str = <<"
    query {
      allUsers: users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 571).
-spec list_with_typename_test() -> nil.
list_with_typename_test() ->
    Schema_def = build_users_list_schema(),
    Query_str = <<"
    query {
      users {
        __typename
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 594).
-spec build_user_with_tags_schema() -> mochi@schema:schema().
build_user_with_tags_schema() ->
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
        _pipe@3 = mochi@types:list_string(
            _pipe@2,
            <<"tags"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user_with_tags/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)},
                                {<<"tags"/utf8>>,
                                    gleam_stdlib:identity(
                                        [<<"admin"/utf8>>, <<"developer"/utf8>>]
                                    )}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"2"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Bob"/utf8>>)},
                                {<<"tags"/utf8>>,
                                    gleam_stdlib:identity([<<"user"/utf8>>])}]
                        )]
                )}
        end,
        fun(U@3) -> gleam_stdlib:identity(U@3) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/list_resolution_test.gleam", 631).
-spec list_of_objects_with_list_fields_test() -> nil.
list_of_objects_with_list_fields_test() ->
    Schema_def = build_user_with_tags_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
        tags
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 658).
-spec decode_team(gleam@dynamic:dynamic_()) -> {ok, team()} | {error, binary()}.
decode_team(_) ->
    {ok, {team, <<"1"/utf8>>, <<"Test Team"/utf8>>}}.

-file("test/list_resolution_test.gleam", 662).
-spec build_nested_list_schema() -> mochi@schema:schema().
build_nested_list_schema() ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    Team_type = begin
        _pipe@3 = mochi@types:object(<<"Team"/utf8>>),
        _pipe@4 = mochi@types:id(
            _pipe@3,
            <<"id"/utf8>>,
            fun(T) -> erlang:element(2, T) end
        ),
        _pipe@5 = mochi@types:string(
            _pipe@4,
            <<"name"/utf8>>,
            fun(T@1) -> erlang:element(3, T@1) end
        ),
        _pipe@6 = mochi@types:list_object(
            _pipe@5,
            <<"members"/utf8>>,
            <<"User"/utf8>>,
            fun(_) ->
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Alice"/utf8>>)}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"2"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Bob"/utf8>>)}]
                        )]
                )
            end
        ),
        mochi@types:build(_pipe@6, fun decode_team/1)
    end,
    Teams_query = mochi@query:'query'(
        <<"teams"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Team"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"team1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(
                                        <<"Engineering"/utf8>>
                                    )},
                                {<<"members"/utf8>>,
                                    gleam_stdlib:identity(
                                        [maps:from_list(
                                                [{<<"id"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"1"/utf8>>
                                                        )},
                                                    {<<"name"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"Alice"/utf8>>
                                                        )}]
                                            )]
                                    )}]
                        ),
                        maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"team2"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Design"/utf8>>)},
                                {<<"members"/utf8>>,
                                    gleam_stdlib:identity(
                                        [maps:from_list(
                                                [{<<"id"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"2"/utf8>>
                                                        )},
                                                    {<<"name"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"Bob"/utf8>>
                                                        )}]
                                            ),
                                            maps:from_list(
                                                [{<<"id"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"3"/utf8>>
                                                        )},
                                                    {<<"name"/utf8>>,
                                                        gleam_stdlib:identity(
                                                            <<"Charlie"/utf8>>
                                                        )}]
                                            )]
                                    )}]
                        )]
                )}
        end,
        fun(T@2) -> gleam_stdlib:identity(T@2) end
    ),
    _pipe@7 = mochi@query:new(),
    _pipe@8 = mochi@query:add_query(_pipe@7, Teams_query),
    _pipe@9 = mochi@query:add_type(_pipe@8, User_type),
    _pipe@10 = mochi@query:add_type(_pipe@9, Team_type),
    mochi@query:build(_pipe@10).

-file("test/list_resolution_test.gleam", 737).
-spec deeply_nested_list_test() -> nil.
deeply_nested_list_test() ->
    Schema_def = build_nested_list_schema(),
    Query_str = <<"
    query {
      teams {
        id
        name
        members {
          id
          name
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 759).
-spec deeply_nested_list_partial_selection_test() -> nil.
deeply_nested_list_partial_selection_test() ->
    Schema_def = build_nested_list_schema(),
    Query_str = <<"
    query {
      teams {
        name
        members {
          name
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/list_resolution_test.gleam", 783).
-spec build_single_item_list_schema() -> mochi@schema:schema().
build_single_item_list_schema() ->
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
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    [maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(<<"1"/utf8>>)},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(<<"Only User"/utf8>>)}]
                        )]
                )}
        end,
        fun(U@2) -> gleam_stdlib:identity(U@2) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Users_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    mochi@query:build(_pipe@5).

-file("test/list_resolution_test.gleam", 813).
-spec single_item_list_test() -> nil.
single_item_list_test() ->
    Schema_def = build_single_item_list_schema(),
    Query_str = <<"
    query {
      users {
        id
        name
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).
