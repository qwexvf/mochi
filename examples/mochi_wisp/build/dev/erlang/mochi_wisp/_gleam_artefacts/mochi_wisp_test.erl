-module(mochi_wisp_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/mochi_wisp_test.gleam").
-export([main/0, parse_simple_query_test/0, parse_query_with_variables_test/0, parse_query_with_operation_name_test/0, parse_query_without_optional_fields_test/0, parse_invalid_json_test/0, parse_missing_query_field_test/0, parse_null_query_field_test/0, parse_empty_body_test/0, parse_array_instead_of_object_test/0, minimal_schema_test/0, scalar_types_schema_test/0, users_list_schema_test/0, users_object_schema_test/0, wisp_schema_users_query_test/0, wisp_schema_users_with_role_test/0, wisp_schema_user_by_id_test/0, wisp_schema_user_not_found_test/0, wisp_schema_user_all_fields_test/0, wisp_schema_user_id_1_details_test/0, wisp_schema_user_id_3_details_test/0, sample_users_count_test/0, sample_users_first_is_alice_test/0, find_user_success_test/0, find_user_bob_test/0, find_user_charlie_test/0, find_user_not_found_test/0, role_to_string_admin_test/0, role_to_string_member_test/0, role_to_string_guest_test/0, string_to_role_admin_test/0, string_to_role_member_test/0, string_to_role_guest_test/0, string_to_role_invalid_test/0, string_to_role_lowercase_invalid_test/0, execution_result_to_json_success_test/0, execution_result_to_json_with_errors_test/0, execution_result_to_json_with_path_test/0, execution_result_to_json_null_data_test/0, decode_user_by_id_args_success_test/0, decode_user_by_id_args_missing_test/0, decode_user_by_id_args_empty_id_test/0, role_enum_name_test/0, role_enum_values_count_test/0, role_enum_has_admin_test/0, role_enum_has_member_test/0, role_enum_has_guest_test/0, user_type_name_test/0, user_type_has_id_field_test/0, user_type_has_name_field_test/0, user_type_has_email_field_test/0, user_type_has_role_field_test/0, user_type_field_count_test/0, user_to_dynamic_test/0, users_encoder_test/0, user_encoder_single_test/0, multiple_fields_query_test/0, nested_selection_test/0, benchmark_measure_time_test/0, benchmark_monotonic_time_test/0, query_only_id_field_test/0, query_only_name_field_test/0, query_only_email_field_test/0, query_only_role_field_test/0, query_cache_init_test/0, query_cache_get_or_parse_test/0, query_cache_stats_test/0, query_cache_size_test/0, query_cache_clear_test/0, query_cache_invalid_query_test/0]).

-file("test/mochi_wisp_test.gleam", 14).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/mochi_wisp_test.gleam", 22).
-spec parse_simple_query_test() -> nil.
parse_simple_query_test() ->
    Body = <<"{\"query\": \"{ hello }\"}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_true(case Result of
            {ok, Req} ->
                erlang:element(2, Req) =:= <<"{ hello }"/utf8>>;

            {error, _} ->
                false
        end).

-file("test/mochi_wisp_test.gleam", 32).
-spec parse_query_with_variables_test() -> nil.
parse_query_with_variables_test() ->
    Body = <<"{\"query\": \"query GetUser($id: ID!) { user(id: $id) { name } }\", \"variables\": {\"id\": \"123\"}}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Req} ->
            gleeunit@should:equal(
                erlang:element(2, Req),
                <<"query GetUser($id: ID!) { user(id: $id) { name } }"/utf8>>
            ),
            gleeunit@should:be_true(case erlang:element(3, Req) of
                    {some, _} ->
                        true;

                    none ->
                        false
                end);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 53).
-spec parse_query_with_operation_name_test() -> nil.
parse_query_with_operation_name_test() ->
    Body = <<"{\"query\": \"query GetUsers { users { id } }\", \"operationName\": \"GetUsers\"}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Req} ->
            gleeunit@should:equal(
                erlang:element(4, Req),
                {some, <<"GetUsers"/utf8>>}
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 67).
-spec parse_query_without_optional_fields_test() -> nil.
parse_query_without_optional_fields_test() ->
    Body = <<"{\"query\": \"{ users { id } }\"}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Req} ->
            gleeunit@should:equal(erlang:element(3, Req), none),
            gleeunit@should:equal(erlang:element(4, Req), none);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 81).
-spec parse_invalid_json_test() -> binary().
parse_invalid_json_test() ->
    Body = <<"{ not valid json }"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 88).
-spec parse_missing_query_field_test() -> binary().
parse_missing_query_field_test() ->
    Body = <<"{\"variables\": {}}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 95).
-spec parse_null_query_field_test() -> binary().
parse_null_query_field_test() ->
    Body = <<"{\"query\": null}"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 102).
-spec parse_empty_body_test() -> binary().
parse_empty_body_test() ->
    Body = <<""/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 109).
-spec parse_array_instead_of_object_test() -> binary().
parse_array_instead_of_object_test() ->
    Body = <<"[{\"query\": \"{ hello }\"}]"/utf8>>,
    Result = mochi_wisp_ffi:parse_graphql_request_full(Body),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 120).
-spec minimal_schema_test() -> nil.
minimal_schema_test() ->
    Hello_query = mochi@query:'query'(
        <<"hello"/utf8>>,
        {non_null, {named, <<"String"/utf8>>}},
        fun(_) -> {ok, <<"world"/utf8>>} end,
        fun(S) -> gleam_stdlib:identity(S) end
    ),
    S@1 = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(_pipe, Hello_query),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(S@1, <<"{ hello }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 139).
-spec scalar_types_schema_test() -> nil.
scalar_types_schema_test() ->
    Scalars_query = mochi@query:'query'(
        <<"scalars"/utf8>>,
        {non_null, {named, <<"String"/utf8>>}},
        fun(_) -> {ok, <<"test"/utf8>>} end,
        fun(S) -> gleam_stdlib:identity(S) end
    ),
    S@1 = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(_pipe, Scalars_query),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(S@1, <<"{ scalars }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 157).
-spec users_list_schema_test() -> nil.
users_list_schema_test() ->
    Users_query = mochi@query:'query'(
        <<"userNames"/utf8>>,
        {non_null, {list, {named, <<"String"/utf8>>}}},
        fun(_) ->
            {ok, [<<"Alice"/utf8>>, <<"Bob"/utf8>>, <<"Charlie"/utf8>>]}
        end,
        fun(Names) -> gleam_stdlib:identity(Names) end
    ),
    S = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(_pipe, Users_query),
        mochi@query:build(_pipe@1)
    end,
    Result = mochi@executor:execute_query(S, <<"{ userNames }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 176).
-spec users_object_schema_test() -> nil.
users_object_schema_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(1, U) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(2, U@1) end
        ),
        mochi@types:build(
            _pipe@2,
            fun(_) -> {ok, {<<"1"/utf8>>, <<"Alice"/utf8>>}} end
        )
    end,
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        {non_null, {list, {named, <<"User"/utf8>>}}},
        fun(_) ->
            {ok,
                [{<<"1"/utf8>>, <<"Alice"/utf8>>},
                    {<<"2"/utf8>>, <<"Bob"/utf8>>}]}
        end,
        fun(Users) ->
            gleam_stdlib:identity(
                begin
                    _pipe@3 = Users,
                    gleam@list:map(
                        _pipe@3,
                        fun(U@2) ->
                            maps:from_list(
                                [{<<"id"/utf8>>,
                                        gleam_stdlib:identity(
                                            erlang:element(1, U@2)
                                        )},
                                    {<<"name"/utf8>>,
                                        gleam_stdlib:identity(
                                            erlang:element(2, U@2)
                                        )}]
                            )
                        end
                    )
                end
            )
        end
    ),
    S = begin
        _pipe@4 = mochi@query:new(),
        _pipe@5 = mochi@query:add_query(_pipe@4, Users_query),
        _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
        mochi@query:build(_pipe@6)
    end,
    Result = mochi@executor:execute_query(S, <<"{ users { id name } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 216).
-spec wisp_schema_users_query_test() -> nil.
wisp_schema_users_query_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ users { id name email } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 222).
-spec wisp_schema_users_with_role_test() -> nil.
wisp_schema_users_with_role_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ users { id name email role } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 228).
-spec wisp_schema_user_by_id_test() -> nil.
wisp_schema_user_by_id_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ user(id: \"1\") { id name } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 234).
-spec wisp_schema_user_not_found_test() -> nil.
wisp_schema_user_not_found_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ user(id: \"nonexistent\") { id name } }"/utf8>>
    ),
    gleeunit@should:be_true(erlang:element(3, Result) /= []).

-file("test/mochi_wisp_test.gleam", 242).
-spec wisp_schema_user_all_fields_test() -> nil.
wisp_schema_user_all_fields_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ user(id: \"2\") { id name email role } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 249).
-spec wisp_schema_user_id_1_details_test() -> nil.
wisp_schema_user_id_1_details_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ user(id: \"1\") { id name email role } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 256).
-spec wisp_schema_user_id_3_details_test() -> nil.
wisp_schema_user_id_3_details_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ user(id: \"3\") { id name email role } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 267).
-spec sample_users_count_test() -> nil.
sample_users_count_test() ->
    Users = mochi_wisp@schema:sample_users(),
    gleeunit@should:equal(erlang:length(Users), 3).

-file("test/mochi_wisp_test.gleam", 272).
-spec sample_users_first_is_alice_test() -> nil.
sample_users_first_is_alice_test() ->
    Users = mochi_wisp@schema:sample_users(),
    case gleam@list:first(Users) of
        {ok, User} ->
            gleeunit@should:equal(erlang:element(3, User), <<"Alice"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 280).
-spec find_user_success_test() -> nil.
find_user_success_test() ->
    Result = mochi_wisp@schema:find_user_by_id(<<"1"/utf8>>),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, User} ->
            gleeunit@should:equal(erlang:element(3, User), <<"Alice"/utf8>>),
            gleeunit@should:equal(
                erlang:element(4, User),
                <<"alice@example.com"/utf8>>
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 292).
-spec find_user_bob_test() -> nil.
find_user_bob_test() ->
    Result = mochi_wisp@schema:find_user_by_id(<<"2"/utf8>>),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, User} ->
            gleeunit@should:equal(erlang:element(3, User), <<"Bob"/utf8>>),
            gleeunit@should:equal(erlang:element(5, User), member);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 304).
-spec find_user_charlie_test() -> nil.
find_user_charlie_test() ->
    Result = mochi_wisp@schema:find_user_by_id(<<"3"/utf8>>),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, User} ->
            gleeunit@should:equal(erlang:element(3, User), <<"Charlie"/utf8>>),
            gleeunit@should:equal(erlang:element(5, User), guest);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 316).
-spec find_user_not_found_test() -> binary().
find_user_not_found_test() ->
    Result = mochi_wisp@schema:find_user_by_id(<<"999"/utf8>>),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 321).
-spec role_to_string_admin_test() -> nil.
role_to_string_admin_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:role_to_string(admin),
        <<"ADMIN"/utf8>>
    ).

-file("test/mochi_wisp_test.gleam", 325).
-spec role_to_string_member_test() -> nil.
role_to_string_member_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:role_to_string(member),
        <<"MEMBER"/utf8>>
    ).

-file("test/mochi_wisp_test.gleam", 329).
-spec role_to_string_guest_test() -> nil.
role_to_string_guest_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:role_to_string(guest),
        <<"GUEST"/utf8>>
    ).

-file("test/mochi_wisp_test.gleam", 333).
-spec string_to_role_admin_test() -> nil.
string_to_role_admin_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:string_to_role(<<"ADMIN"/utf8>>),
        {ok, admin}
    ).

-file("test/mochi_wisp_test.gleam", 337).
-spec string_to_role_member_test() -> nil.
string_to_role_member_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:string_to_role(<<"MEMBER"/utf8>>),
        {ok, member}
    ).

-file("test/mochi_wisp_test.gleam", 341).
-spec string_to_role_guest_test() -> nil.
string_to_role_guest_test() ->
    gleeunit@should:equal(
        mochi_wisp@schema:string_to_role(<<"GUEST"/utf8>>),
        {ok, guest}
    ).

-file("test/mochi_wisp_test.gleam", 345).
-spec string_to_role_invalid_test() -> binary().
string_to_role_invalid_test() ->
    gleeunit@should:be_error(
        mochi_wisp@schema:string_to_role(<<"INVALID"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 349).
-spec string_to_role_lowercase_invalid_test() -> binary().
string_to_role_lowercase_invalid_test() ->
    gleeunit@should:be_error(mochi_wisp@schema:string_to_role(<<"admin"/utf8>>)).

-file("test/mochi_wisp_test.gleam", 357).
-spec execution_result_to_json_success_test() -> nil.
execution_result_to_json_success_test() ->
    Result = {execution_result,
        {some, gleam_stdlib:identity(<<"test data"/utf8>>)},
        []},
    Json_str = mochi_wisp@graphql_handler:execution_result_to_json(Result),
    gleeunit@should:be_true(
        gleam_stdlib:contains_string(Json_str, <<"data"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 368).
-spec execution_result_to_json_with_errors_test() -> nil.
execution_result_to_json_with_errors_test() ->
    Result = {execution_result,
        none,
        [{validation_error, <<"Test error"/utf8>>, [<<"field"/utf8>>]}]},
    Json_str = mochi_wisp@graphql_handler:execution_result_to_json(Result),
    gleeunit@should:be_true(
        gleam_stdlib:contains_string(Json_str, <<"errors"/utf8>>)
    ),
    gleeunit@should:be_true(
        gleam_stdlib:contains_string(Json_str, <<"Test error"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 379).
-spec execution_result_to_json_with_path_test() -> nil.
execution_result_to_json_with_path_test() ->
    Result = {execution_result,
        none,
        [{resolver_error,
                <<"Resolver failed"/utf8>>,
                [<<"query"/utf8>>, <<"user"/utf8>>, <<"name"/utf8>>]}]},
    Json_str = mochi_wisp@graphql_handler:execution_result_to_json(Result),
    gleeunit@should:be_true(
        gleam_stdlib:contains_string(Json_str, <<"path"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 388).
-spec execution_result_to_json_null_data_test() -> nil.
execution_result_to_json_null_data_test() ->
    Result = {execution_result, none, []},
    Json_str = mochi_wisp@graphql_handler:execution_result_to_json(Result),
    gleeunit@should:be_true(
        gleam_stdlib:contains_string(Json_str, <<"null"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 398).
-spec decode_user_by_id_args_success_test() -> nil.
decode_user_by_id_args_success_test() ->
    Args = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"123"/utf8>>)}]
    ),
    Result = mochi_wisp@schema:decode_user_by_id_args(Args),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Args_decoded} ->
            gleeunit@should:equal(
                erlang:element(2, Args_decoded),
                <<"123"/utf8>>
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 408).
-spec decode_user_by_id_args_missing_test() -> binary().
decode_user_by_id_args_missing_test() ->
    Args = maps:new(),
    Result = mochi_wisp@schema:decode_user_by_id_args(Args),
    gleeunit@should:be_error(Result).

-file("test/mochi_wisp_test.gleam", 414).
-spec decode_user_by_id_args_empty_id_test() -> nil.
decode_user_by_id_args_empty_id_test() ->
    Args = maps:from_list([{<<"id"/utf8>>, gleam_stdlib:identity(<<""/utf8>>)}]),
    Result = mochi_wisp@schema:decode_user_by_id_args(Args),
    gleeunit@should:be_ok(Result),
    case Result of
        {ok, Args_decoded} ->
            gleeunit@should:equal(erlang:element(2, Args_decoded), <<""/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 428).
-spec role_enum_name_test() -> nil.
role_enum_name_test() ->
    Role_enum = mochi_wisp@schema:role_enum(),
    gleeunit@should:equal(erlang:element(2, Role_enum), <<"Role"/utf8>>).

-file("test/mochi_wisp_test.gleam", 433).
-spec role_enum_values_count_test() -> nil.
role_enum_values_count_test() ->
    Role_enum = mochi_wisp@schema:role_enum(),
    gleeunit@should:equal(maps:size(erlang:element(4, Role_enum)), 3).

-file("test/mochi_wisp_test.gleam", 438).
-spec role_enum_has_admin_test() -> nil.
role_enum_has_admin_test() ->
    Role_enum = mochi_wisp@schema:role_enum(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, Role_enum), <<"ADMIN"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 443).
-spec role_enum_has_member_test() -> nil.
role_enum_has_member_test() ->
    Role_enum = mochi_wisp@schema:role_enum(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, Role_enum), <<"MEMBER"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 448).
-spec role_enum_has_guest_test() -> nil.
role_enum_has_guest_test() ->
    Role_enum = mochi_wisp@schema:role_enum(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, Role_enum), <<"GUEST"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 457).
-spec user_type_name_test() -> nil.
user_type_name_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:equal(erlang:element(2, User_t), <<"User"/utf8>>).

-file("test/mochi_wisp_test.gleam", 462).
-spec user_type_has_id_field_test() -> nil.
user_type_has_id_field_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, User_t), <<"id"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 467).
-spec user_type_has_name_field_test() -> nil.
user_type_has_name_field_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, User_t), <<"name"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 472).
-spec user_type_has_email_field_test() -> nil.
user_type_has_email_field_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, User_t), <<"email"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 477).
-spec user_type_has_role_field_test() -> nil.
user_type_has_role_field_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, User_t), <<"role"/utf8>>)
    ).

-file("test/mochi_wisp_test.gleam", 482).
-spec user_type_field_count_test() -> nil.
user_type_field_count_test() ->
    User_t = mochi_wisp@schema:user_type(),
    gleeunit@should:equal(maps:size(erlang:element(4, User_t)), 4).

-file("test/mochi_wisp_test.gleam", 491).
-spec user_to_dynamic_test() -> nil.
user_to_dynamic_test() ->
    User = {user,
        <<"1"/utf8>>,
        <<"Test User"/utf8>>,
        <<"test@example.com"/utf8>>,
        admin},
    _ = mochi_wisp@schema:user_to_dynamic(User),
    gleeunit@should:be_true(true).

-file("test/mochi_wisp_test.gleam", 504).
-spec users_encoder_test() -> nil.
users_encoder_test() ->
    Users = [{user,
            <<"1"/utf8>>,
            <<"Alice"/utf8>>,
            <<"alice@example.com"/utf8>>,
            admin},
        {user, <<"2"/utf8>>, <<"Bob"/utf8>>, <<"bob@example.com"/utf8>>, member}],
    _ = mochi_wisp@schema:users_encoder(Users),
    gleeunit@should:be_true(true).

-file("test/mochi_wisp_test.gleam", 524).
-spec user_encoder_single_test() -> nil.
user_encoder_single_test() ->
    User = {user,
        <<"1"/utf8>>,
        <<"Test"/utf8>>,
        <<"test@test.com"/utf8>>,
        guest},
    _ = mochi_wisp@schema:user_encoder(User),
    gleeunit@should:be_true(true).

-file("test/mochi_wisp_test.gleam", 540).
-spec multiple_fields_query_test() -> nil.
multiple_fields_query_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(
        S,
        <<"{ users { id } user(id: \"1\") { name } }"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 551).
-spec nested_selection_test() -> nil.
nested_selection_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(S, <<"{ users { id name } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(case erlang:element(2, Result) of
            {some, _} ->
                true;

            none ->
                false
        end).

-file("test/mochi_wisp_test.gleam", 567).
-spec benchmark_measure_time_test() -> nil.
benchmark_measure_time_test() ->
    {Time_us, Result} = mochi_wisp_benchmark_ffi:measure_time(
        fun() -> 1 + 1 end
    ),
    gleeunit@should:equal(Result, 2),
    gleeunit@should:be_true(Time_us >= 0).

-file("test/mochi_wisp_test.gleam", 575).
-spec benchmark_monotonic_time_test() -> nil.
benchmark_monotonic_time_test() ->
    T1 = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    T2 = mochi_wisp_benchmark_ffi:monotonic_time_us(),
    gleeunit@should:be_true(T2 >= T1).

-file("test/mochi_wisp_test.gleam", 586).
-spec query_only_id_field_test() -> nil.
query_only_id_field_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(S, <<"{ users { id } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 592).
-spec query_only_name_field_test() -> nil.
query_only_name_field_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(S, <<"{ users { name } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 598).
-spec query_only_email_field_test() -> nil.
query_only_email_field_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(S, <<"{ users { email } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 604).
-spec query_only_role_field_test() -> nil.
query_only_role_field_test() ->
    S = mochi_wisp@schema:build_schema(),
    Result = mochi@executor:execute_query(S, <<"{ users { role } }"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/mochi_wisp_test.gleam", 616).
-spec query_cache_init_test() -> nil.
query_cache_init_test() ->
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:init(),
    gleeunit@should:be_true(true).

-file("test/mochi_wisp_test.gleam", 624).
-spec query_cache_get_or_parse_test() -> nil.
query_cache_get_or_parse_test() ->
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Result1 = mochi_wisp@query_cache:get_or_parse(<<"{ hello }"/utf8>>),
    gleeunit@should:be_ok(Result1),
    Result2 = mochi_wisp@query_cache:get_or_parse(<<"{ hello }"/utf8>>),
    gleeunit@should:be_ok(Result2),
    case {Result1, Result2} of
        {{ok, Doc1}, {ok, Doc2}} ->
            gleeunit@should:equal(
                erlang:length(erlang:element(2, Doc1)),
                erlang:length(erlang:element(2, Doc2))
            );

        {_, _} ->
            gleeunit@should:fail()
    end.

-file("test/mochi_wisp_test.gleam", 645).
-spec query_cache_stats_test() -> nil.
query_cache_stats_test() ->
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Stats1 = mochi_query_cache_ffi:stats(),
    gleeunit@should:equal(erlang:element(2, Stats1), 0),
    gleeunit@should:equal(erlang:element(3, Stats1), 0),
    gleeunit@should:equal(erlang:element(4, Stats1), 0),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ users { id } }"/utf8>>),
    Stats2 = mochi_query_cache_ffi:stats(),
    gleeunit@should:equal(erlang:element(3, Stats2), 1),
    gleeunit@should:equal(erlang:element(2, Stats2), 0),
    gleeunit@should:equal(erlang:element(4, Stats2), 1),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ users { id } }"/utf8>>),
    Stats3 = mochi_query_cache_ffi:stats(),
    gleeunit@should:equal(erlang:element(2, Stats3), 1),
    gleeunit@should:equal(erlang:element(3, Stats3), 1),
    gleeunit@should:equal(erlang:element(4, Stats3), 1).

-file("test/mochi_wisp_test.gleam", 670).
-spec query_cache_size_test() -> nil.
query_cache_size_test() ->
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 0),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ a }"/utf8>>),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 1),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ b }"/utf8>>),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 2),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ a }"/utf8>>),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 2).

-file("test/mochi_wisp_test.gleam", 687).
-spec query_cache_clear_test() -> nil.
query_cache_clear_test() ->
    mochi_query_cache_ffi:init(),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ x }"/utf8>>),
    _ = mochi_wisp@query_cache:get_or_parse(<<"{ y }"/utf8>>),
    gleeunit@should:be_true(mochi_query_cache_ffi:size() > 0),
    mochi_query_cache_ffi:clear(),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 0),
    Stats = mochi_query_cache_ffi:stats(),
    gleeunit@should:equal(erlang:element(2, Stats), 0),
    gleeunit@should:equal(erlang:element(3, Stats), 0).

-file("test/mochi_wisp_test.gleam", 702).
-spec query_cache_invalid_query_test() -> nil.
query_cache_invalid_query_test() ->
    mochi_query_cache_ffi:init(),
    mochi_query_cache_ffi:clear(),
    Result = mochi_wisp@query_cache:get_or_parse(<<"{ invalid query !!!"/utf8>>),
    gleeunit@should:be_error(Result),
    gleeunit@should:equal(mochi_query_cache_ffi:size(), 0).
