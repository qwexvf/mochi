-module(context_isolation_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/context_isolation_test.gleam").
-export([named_fragment_on_same_type_followed_by_sibling_test/0, named_fragment_with_nested_object_followed_by_sibling_test/0, multiple_named_fragments_on_same_type_test/0, named_fragment_interspersed_with_siblings_test/0, fragment_then_nested_object_then_sibling_test/0, nested_fragment_spread_inside_another_fragment_test/0, fragment_used_in_multiple_queries_test/0, inline_fragment_on_same_type_followed_by_sibling_test/0, multiple_inline_fragments_on_same_type_test/0, inline_fragment_without_type_condition_followed_by_sibling_test/0, inline_fragment_with_nested_object_followed_by_sibling_test/0, inline_fragment_then_nested_object_then_sibling_test/0, alternating_inline_fragments_and_plain_fields_test/0, inline_fragment_different_type_sibling_score_not_rejected_test/0, inline_fragment_different_type_sibling_email_not_rejected_test/0, inline_fragment_different_type_multiple_siblings_not_rejected_test/0, two_inline_fragments_different_types_siblings_after_both_test/0, fragment_spread_different_type_sibling_score_not_rejected_test/0, fragment_spread_different_type_sibling_email_not_rejected_test/0, fragment_spread_different_type_multiple_siblings_not_rejected_test/0, mixed_fragment_and_inline_different_types_siblings_not_rejected_test/0, invalid_field_after_valid_fragment_still_rejected_test/0, invalid_field_after_inline_fragment_still_rejected_test/0, invalid_field_inside_fragment_still_rejected_test/0, invalid_field_inside_inline_fragment_still_rejected_test/0]).
-export_type([profile/0, user/0, address/0, post/0]).

-type profile() :: {profile, binary(), binary()}.

-type user() :: {user, binary(), binary(), binary(), integer(), profile()}.

-type address() :: {address, binary(), binary(), binary()}.

-type post() :: {post, binary(), binary(), binary(), binary()}.

-file("test/context_isolation_test.gleam", 33).
-spec profile_type() -> mochi@schema:object_type().
profile_type() ->
    _pipe = mochi@types:object(<<"Profile"/utf8>>),
    _pipe@1 = mochi@types:string(
        _pipe,
        <<"bio"/utf8>>,
        fun(P) -> erlang:element(2, P) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"website"/utf8>>,
        fun(P@1) -> erlang:element(3, P@1) end
    ),
    mochi@types:build(
        _pipe@2,
        fun(_) -> {ok, {profile, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/context_isolation_test.gleam", 40).
-spec user_type() -> mochi@schema:object_type().
user_type() ->
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
    _pipe@4 = mochi@types:int(
        _pipe@3,
        <<"score"/utf8>>,
        fun(U@3) -> erlang:element(5, U@3) end
    ),
    _pipe@5 = mochi@types:object_field(
        _pipe@4,
        <<"profile"/utf8>>,
        <<"Profile"/utf8>>,
        fun(U@4) -> gleam_stdlib:identity(erlang:element(6, U@4)) end
    ),
    mochi@types:build(
        _pipe@5,
        fun(_) ->
            {ok,
                {user,
                    <<""/utf8>>,
                    <<""/utf8>>,
                    <<""/utf8>>,
                    0,
                    {profile, <<""/utf8>>, <<""/utf8>>}}}
        end
    ).

-file("test/context_isolation_test.gleam", 52).
-spec address_type() -> mochi@schema:object_type().
address_type() ->
    _pipe = mochi@types:object(<<"Address"/utf8>>),
    _pipe@1 = mochi@types:string(
        _pipe,
        <<"street"/utf8>>,
        fun(A) -> erlang:element(2, A) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"city"/utf8>>,
        fun(A@1) -> erlang:element(3, A@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"zip"/utf8>>,
        fun(A@2) -> erlang:element(4, A@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) -> {ok, {address, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/context_isolation_test.gleam", 60).
-spec post_type() -> mochi@schema:object_type().
post_type() ->
    _pipe = mochi@types:object(<<"Post"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(P) -> erlang:element(2, P) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"title"/utf8>>,
        fun(P@1) -> erlang:element(3, P@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"body"/utf8>>,
        fun(P@2) -> erlang:element(4, P@2) end
    ),
    _pipe@4 = mochi@types:id(
        _pipe@3,
        <<"authorId"/utf8>>,
        fun(P@3) -> erlang:element(5, P@3) end
    ),
    mochi@types:build(
        _pipe@4,
        fun(_) ->
            {ok, {post, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}}
        end
    ).

-file("test/context_isolation_test.gleam", 69).
-spec build_schema() -> mochi@schema:schema().
build_schema() ->
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Alice"/utf8>>,
                    <<"alice@example.com"/utf8>>,
                    42,
                    {profile, <<"dev"/utf8>>, <<"example.com"/utf8>>}}}
        end,
        fun gleam_stdlib:identity/1
    ),
    Post_query = mochi@query:'query'(
        <<"post"/utf8>>,
        mochi@schema:named_type(<<"Post"/utf8>>),
        fun(_) ->
            {ok,
                {post,
                    <<"1"/utf8>>,
                    <<"Hello"/utf8>>,
                    <<"World"/utf8>>,
                    <<"1"/utf8>>}}
        end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, User_query),
    _pipe@2 = mochi@query:add_query(_pipe@1, Post_query),
    _pipe@3 = mochi@query:add_type(_pipe@2, user_type()),
    _pipe@4 = mochi@query:add_type(_pipe@3, address_type()),
    _pipe@5 = mochi@query:add_type(_pipe@4, profile_type()),
    _pipe@6 = mochi@query:add_type(_pipe@5, post_type()),
    mochi@query:build(_pipe@6).

-file("test/context_isolation_test.gleam", 106).
-spec assert_valid(binary()) -> nil.
assert_valid(Q) ->
    S = build_schema(),
    case mochi@validation:validate_query(Q, S) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected valid query but got errors: "/utf8,
                        (gleam@string:join(
                            gleam@list:map(
                                Errors,
                                fun mochi@validation:format_error/1
                            ),
                            <<", "/utf8>>
                        ))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"context_isolation_test"/utf8>>,
                    function => <<"assert_valid"/utf8>>,
                    line => 111})
    end.

-file("test/context_isolation_test.gleam", 118).
-spec assert_errors_do_not_contain(binary(), binary()) -> nil.
assert_errors_do_not_contain(Q, Forbidden) ->
    S = build_schema(),
    case mochi@validation:validate_query(Q, S) of
        {ok, _} ->
            nil;

        {error, Errors} ->
            Messages = gleam@list:map(
                Errors,
                fun mochi@validation:format_error/1
            ),
            case gleam@list:any(
                Messages,
                fun(M) -> gleam_stdlib:contains_string(M, Forbidden) end
            ) of
                true ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<<<<<"False positive: error message contained forbidden string '"/utf8,
                                        Forbidden/binary>>/binary,
                                    "'. Errors: "/utf8>>/binary,
                                (gleam@string:join(Messages, <<", "/utf8>>))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"context_isolation_test"/utf8>>,
                            function => <<"assert_errors_do_not_contain"/utf8>>,
                            line => 126});

                false ->
                    nil
            end
    end.

-file("test/context_isolation_test.gleam", 138).
-spec assert_has_error_for(binary(), binary()) -> nil.
assert_has_error_for(Q, Expected) ->
    S = build_schema(),
    case mochi@validation:validate_query(Q, S) of
        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<<<"Expected validation error containing '"/utf8,
                            Expected/binary>>/binary,
                        "' but got none"/utf8>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"context_isolation_test"/utf8>>,
                    function => <<"assert_has_error_for"/utf8>>,
                    line => 142});

        {error, Errors} ->
            Messages = gleam@list:map(
                Errors,
                fun mochi@validation:format_error/1
            ),
            case gleam@list:any(
                Messages,
                fun(M) -> gleam_stdlib:contains_string(M, Expected) end
            ) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => (<<<<<<"Expected error containing '"/utf8,
                                        Expected/binary>>/binary,
                                    "' but got: "/utf8>>/binary,
                                (gleam@string:join(Messages, <<", "/utf8>>))/binary>>),
                            file => <<?FILEPATH/utf8>>,
                            module => <<"context_isolation_test"/utf8>>,
                            function => <<"assert_has_error_for"/utf8>>,
                            line => 150})
            end
    end.

-file("test/context_isolation_test.gleam", 163).
-spec named_fragment_on_same_type_followed_by_sibling_test() -> nil.
named_fragment_on_same_type_followed_by_sibling_test() ->
    assert_valid(
        <<"{ user { id ...UserBasics score email } }
     fragment UserBasics on User { name }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 170).
-spec named_fragment_with_nested_object_followed_by_sibling_test() -> nil.
named_fragment_with_nested_object_followed_by_sibling_test() ->
    assert_valid(
        <<"{ user { id ...UserWithProfile score } }
     fragment UserWithProfile on User { name profile { bio website } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 177).
-spec multiple_named_fragments_on_same_type_test() -> nil.
multiple_named_fragments_on_same_type_test() ->
    assert_valid(
        <<"{ user { ...UserName ...UserContact score } }
     fragment UserName on User { id name }
     fragment UserContact on User { email }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 185).
-spec named_fragment_interspersed_with_siblings_test() -> nil.
named_fragment_interspersed_with_siblings_test() ->
    assert_valid(
        <<"{ user { id ...UserName email ...UserScore name } }
     fragment UserName on User { name }
     fragment UserScore on User { score }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 193).
-spec fragment_then_nested_object_then_sibling_test() -> nil.
fragment_then_nested_object_then_sibling_test() ->
    assert_valid(
        <<"{ user { ...UserName profile { bio } score } }
     fragment UserName on User { id name }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 200).
-spec nested_fragment_spread_inside_another_fragment_test() -> nil.
nested_fragment_spread_inside_another_fragment_test() ->
    assert_valid(
        <<"{ user { ...UserFull score } }
     fragment UserFull on User { id ...UserName email }
     fragment UserName on User { name }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 208).
-spec fragment_used_in_multiple_queries_test() -> nil.
fragment_used_in_multiple_queries_test() ->
    assert_valid(
        <<"{ user { ...CoreFields score } post { id title body authorId } }
     fragment CoreFields on User { id name email }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 217).
-spec inline_fragment_on_same_type_followed_by_sibling_test() -> nil.
inline_fragment_on_same_type_followed_by_sibling_test() ->
    assert_valid(<<"{ user { id ... on User { name email } score } }"/utf8>>).

-file("test/context_isolation_test.gleam", 221).
-spec multiple_inline_fragments_on_same_type_test() -> nil.
multiple_inline_fragments_on_same_type_test() ->
    assert_valid(
        <<"{ user { ... on User { id name } ... on User { email } score } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 227).
-spec inline_fragment_without_type_condition_followed_by_sibling_test() -> nil.
inline_fragment_without_type_condition_followed_by_sibling_test() ->
    assert_valid(
        <<"{ user { id ... { name email } score active: score } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 231).
-spec inline_fragment_with_nested_object_followed_by_sibling_test() -> nil.
inline_fragment_with_nested_object_followed_by_sibling_test() ->
    assert_valid(
        <<"{ user { id ... on User { profile { bio } name } score email } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 237).
-spec inline_fragment_then_nested_object_then_sibling_test() -> nil.
inline_fragment_then_nested_object_then_sibling_test() ->
    assert_valid(
        <<"{ user { ... on User { id } profile { bio website } score } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 241).
-spec alternating_inline_fragments_and_plain_fields_test() -> nil.
alternating_inline_fragments_and_plain_fields_test() ->
    assert_valid(
        <<"{ user { id ... on User { name } score ... on User { email } } }"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 258).
-spec inline_fragment_different_type_sibling_score_not_rejected_test() -> nil.
inline_fragment_different_type_sibling_score_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { id ... on Address { city } score } }"/utf8>>,
        <<"score"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 265).
-spec inline_fragment_different_type_sibling_email_not_rejected_test() -> nil.
inline_fragment_different_type_sibling_email_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ... on Address { street } email name } }"/utf8>>,
        <<"email"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 272).
-spec inline_fragment_different_type_multiple_siblings_not_rejected_test() -> nil.
inline_fragment_different_type_multiple_siblings_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ... on Post { title body } id name score email } }"/utf8>>,
        <<"score"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 279).
-spec two_inline_fragments_different_types_siblings_after_both_test() -> nil.
two_inline_fragments_different_types_siblings_after_both_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ... on Address { city } ... on Post { title } id name } }"/utf8>>,
        <<"name"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 292).
-spec fragment_spread_different_type_sibling_score_not_rejected_test() -> nil.
fragment_spread_different_type_sibling_score_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { id ...AddressFields score } }
     fragment AddressFields on Address { city zip }"/utf8>>,
        <<"score"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 300).
-spec fragment_spread_different_type_sibling_email_not_rejected_test() -> nil.
fragment_spread_different_type_sibling_email_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ...PostFields email name score } }
     fragment PostFields on Post { title body }"/utf8>>,
        <<"email"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 308).
-spec fragment_spread_different_type_multiple_siblings_not_rejected_test() -> nil.
fragment_spread_different_type_multiple_siblings_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ...AddressFields id name email score } }
     fragment AddressFields on Address { street city zip }"/utf8>>,
        <<"name"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 316).
-spec mixed_fragment_and_inline_different_types_siblings_not_rejected_test() -> nil.
mixed_fragment_and_inline_different_types_siblings_not_rejected_test() ->
    assert_errors_do_not_contain(
        <<"{ user { ...AddressFields ... on Post { title } id score } }
     fragment AddressFields on Address { city }"/utf8>>,
        <<"score"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 328).
-spec invalid_field_after_valid_fragment_still_rejected_test() -> nil.
invalid_field_after_valid_fragment_still_rejected_test() ->
    assert_has_error_for(
        <<"{ user { ...UserBasics nonExistent } }
     fragment UserBasics on User { id name }"/utf8>>,
        <<"nonExistent"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 336).
-spec invalid_field_after_inline_fragment_still_rejected_test() -> nil.
invalid_field_after_inline_fragment_still_rejected_test() ->
    assert_has_error_for(
        <<"{ user { ... on User { name } bogusField } }"/utf8>>,
        <<"bogusField"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 343).
-spec invalid_field_inside_fragment_still_rejected_test() -> nil.
invalid_field_inside_fragment_still_rejected_test() ->
    assert_has_error_for(
        <<"{ user { ...BadFragment score } }
     fragment BadFragment on User { id nonExistentInUser }"/utf8>>,
        <<"nonExistentInUser"/utf8>>
    ).

-file("test/context_isolation_test.gleam", 351).
-spec invalid_field_inside_inline_fragment_still_rejected_test() -> nil.
invalid_field_inside_inline_fragment_still_rejected_test() ->
    assert_has_error_for(
        <<"{ user { id ... on User { name bogusInUser } score } }"/utf8>>,
        <<"bogusInUser"/utf8>>
    ).
