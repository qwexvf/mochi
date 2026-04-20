-module(union_interface_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/union_interface_test.gleam").
-export([inline_fragment_on_interface_test/0, inline_fragment_on_union_test/0, typename_on_union_test/0, inline_fragment_same_type_test/0, inline_fragment_no_type_condition_test/0, inline_fragment_type_mismatch_test/0]).
-export_type([cat/0, dog/0, user/0]).

-type cat() :: {cat, binary(), binary(), boolean()}.

-type dog() :: {dog, binary(), binary(), boolean()}.

-type user() :: {user, binary(), binary(), binary()}.

-file("test/union_interface_test.gleam", 35).
-spec decode_dict_has_key(gleam@dynamic:dynamic_(), binary()) -> boolean().
decode_dict_has_key(Value, Key) ->
    case gleam@dynamic@decode:run(
        Value,
        gleam@dynamic@decode:at(
            [Key],
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-file("test/union_interface_test.gleam", 27).
-spec resolve_animal_type(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, binary()}.
resolve_animal_type(Value) ->
    case decode_dict_has_key(Value, <<"meows"/utf8>>) of
        true ->
            {ok, <<"Cat"/utf8>>};

        false ->
            {ok, <<"Dog"/utf8>>}
    end.

-file("test/union_interface_test.gleam", 42).
-spec build_interface_schema() -> mochi@schema:schema().
build_interface_schema() ->
    Animal_interface = begin
        _pipe = mochi@schema:interface(<<"Animal"/utf8>>),
        _pipe@1 = mochi@schema:interface_description(
            _pipe,
            <<"An animal in the system"/utf8>>
        ),
        _pipe@2 = mochi@schema:interface_field(
            _pipe@1,
            mochi@schema:field_def(<<"id"/utf8>>, mochi@schema:id_type())
        ),
        _pipe@3 = mochi@schema:interface_field(
            _pipe@2,
            mochi@schema:field_def(<<"name"/utf8>>, mochi@schema:string_type())
        ),
        mochi@schema:interface_resolve_type(_pipe@3, fun resolve_animal_type/1)
    end,
    Cat_type = begin
        _pipe@4 = mochi@types:object(<<"Cat"/utf8>>),
        _pipe@5 = mochi@types:id(
            _pipe@4,
            <<"id"/utf8>>,
            fun(C) -> erlang:element(2, C) end
        ),
        _pipe@6 = mochi@types:string(
            _pipe@5,
            <<"name"/utf8>>,
            fun(C@1) -> erlang:element(3, C@1) end
        ),
        _pipe@7 = mochi@types:bool(
            _pipe@6,
            <<"meows"/utf8>>,
            fun(C@2) -> erlang:element(4, C@2) end
        ),
        _pipe@8 = mochi@types:build(
            _pipe@7,
            fun(_) -> {ok, {cat, <<"1"/utf8>>, <<"Whiskers"/utf8>>, true}} end
        ),
        (fun(Obj) ->
            {object_type,
                erlang:element(2, Obj),
                erlang:element(3, Obj),
                erlang:element(4, Obj),
                [Animal_interface]}
        end)(_pipe@8)
    end,
    Dog_type = begin
        _pipe@9 = mochi@types:object(<<"Dog"/utf8>>),
        _pipe@10 = mochi@types:id(
            _pipe@9,
            <<"id"/utf8>>,
            fun(D) -> erlang:element(2, D) end
        ),
        _pipe@11 = mochi@types:string(
            _pipe@10,
            <<"name"/utf8>>,
            fun(D@1) -> erlang:element(3, D@1) end
        ),
        _pipe@12 = mochi@types:bool(
            _pipe@11,
            <<"barks"/utf8>>,
            fun(D@2) -> erlang:element(4, D@2) end
        ),
        _pipe@13 = mochi@types:build(
            _pipe@12,
            fun(_) -> {ok, {dog, <<"2"/utf8>>, <<"Rex"/utf8>>, true}} end
        ),
        (fun(Obj@1) ->
            {object_type,
                erlang:element(2, Obj@1),
                erlang:element(3, Obj@1),
                erlang:element(4, Obj@1),
                [Animal_interface]}
        end)(_pipe@13)
    end,
    Animal_query = mochi@query:'query'(
        <<"animal"/utf8>>,
        {named, <<"Animal"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"1"/utf8>>)},
                            {<<"name"/utf8>>,
                                gleam_stdlib:identity(<<"Whiskers"/utf8>>)},
                            {<<"meows"/utf8>>, gleam_stdlib:identity(true)}]
                    )
                )}
        end,
        fun(A) -> gleam_stdlib:identity(A) end
    ),
    _pipe@14 = mochi@query:new(),
    _pipe@15 = mochi@query:add_query(_pipe@14, Animal_query),
    _pipe@16 = mochi@query:add_type(_pipe@15, Cat_type),
    _pipe@17 = mochi@query:add_type(_pipe@16, Dog_type),
    _pipe@18 = mochi@query:add_interface(_pipe@17, Animal_interface),
    mochi@query:build(_pipe@18).

-file("test/union_interface_test.gleam", 97).
-spec inline_fragment_on_interface_test() -> nil.
inline_fragment_on_interface_test() ->
    Schema_def = build_interface_schema(),
    Query_str = <<"
    query {
      animal {
        id
        name
        ... on Cat {
          meows
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/union_interface_test.gleam", 121).
-spec build_union_schema() -> mochi@schema:schema().
build_union_schema() ->
    Cat_type = begin
        _pipe = mochi@types:object(<<"Cat"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(C) -> erlang:element(2, C) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(C@1) -> erlang:element(3, C@1) end
        ),
        _pipe@3 = mochi@types:bool(
            _pipe@2,
            <<"meows"/utf8>>,
            fun(C@2) -> erlang:element(4, C@2) end
        ),
        mochi@types:build(
            _pipe@3,
            fun(_) -> {ok, {cat, <<"1"/utf8>>, <<"Whiskers"/utf8>>, true}} end
        )
    end,
    Dog_type = begin
        _pipe@4 = mochi@types:object(<<"Dog"/utf8>>),
        _pipe@5 = mochi@types:id(
            _pipe@4,
            <<"id"/utf8>>,
            fun(D) -> erlang:element(2, D) end
        ),
        _pipe@6 = mochi@types:string(
            _pipe@5,
            <<"name"/utf8>>,
            fun(D@1) -> erlang:element(3, D@1) end
        ),
        _pipe@7 = mochi@types:bool(
            _pipe@6,
            <<"barks"/utf8>>,
            fun(D@2) -> erlang:element(4, D@2) end
        ),
        mochi@types:build(
            _pipe@7,
            fun(_) -> {ok, {dog, <<"2"/utf8>>, <<"Rex"/utf8>>, true}} end
        )
    end,
    Pet_union = begin
        _pipe@8 = mochi@schema:union(<<"Pet"/utf8>>),
        _pipe@9 = mochi@schema:union_description(
            _pipe@8,
            <<"A pet in the system"/utf8>>
        ),
        _pipe@10 = mochi@schema:union_member(_pipe@9, Cat_type),
        _pipe@11 = mochi@schema:union_member(_pipe@10, Dog_type),
        mochi@schema:union_resolve_type(_pipe@11, fun resolve_animal_type/1)
    end,
    Pet_query = mochi@query:'query'(
        <<"pet"/utf8>>,
        {named, <<"Pet"/utf8>>},
        fun(_) ->
            {ok,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"2"/utf8>>)},
                            {<<"name"/utf8>>,
                                gleam_stdlib:identity(<<"Rex"/utf8>>)},
                            {<<"barks"/utf8>>, gleam_stdlib:identity(true)}]
                    )
                )}
        end,
        fun(P) -> gleam_stdlib:identity(P) end
    ),
    _pipe@12 = mochi@query:new(),
    _pipe@13 = mochi@query:add_query(_pipe@12, Pet_query),
    _pipe@14 = mochi@query:add_type(_pipe@13, Cat_type),
    _pipe@15 = mochi@query:add_type(_pipe@14, Dog_type),
    _pipe@16 = mochi@query:add_union(_pipe@15, Pet_union),
    mochi@query:build(_pipe@16).

-file("test/union_interface_test.gleam", 174).
-spec inline_fragment_on_union_test() -> nil.
inline_fragment_on_union_test() ->
    Schema_def = build_union_schema(),
    Query_str = <<"
    query {
      pet {
        ... on Dog {
          id
          name
          barks
        }
        ... on Cat {
          id
          name
          meows
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/union_interface_test.gleam", 199).
-spec typename_on_union_test() -> nil.
typename_on_union_test() ->
    Schema_def = build_union_schema(),
    Query_str = <<"
    query {
      pet {
        __typename
        ... on Dog {
          name
        }
      }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/union_interface_test.gleam", 226).
-spec build_simple_schema() -> mochi@schema:schema().
build_simple_schema() ->
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
        mochi@types:build(
            _pipe@3,
            fun(_) ->
                {ok,
                    {user,
                        <<"1"/utf8>>,
                        <<"Alice"/utf8>>,
                        <<"alice@example.com"/utf8>>}}
            end
        )
    end,
    User_query = mochi@query:'query'(
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
    _pipe@5 = mochi@query:add_query(_pipe@4, User_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/union_interface_test.gleam", 250).
-spec inline_fragment_same_type_test() -> nil.
inline_fragment_same_type_test() ->
    Schema_def = build_simple_schema(),
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
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/union_interface_test.gleam", 270).
-spec inline_fragment_no_type_condition_test() -> nil.
inline_fragment_no_type_condition_test() ->
    Schema_def = build_simple_schema(),
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
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/union_interface_test.gleam", 289).
-spec inline_fragment_type_mismatch_test() -> nil.
inline_fragment_type_mismatch_test() ->
    Schema_def = build_simple_schema(),
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
    Result = mochi@executor:execute_query(Schema_def, Query_str),
    gleeunit@should:be_true(gleam@option:is_some(erlang:element(2, Result))).
