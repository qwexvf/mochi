-module(payload_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/payload_test.gleam").
-export([ok_sets_successful_true_test/0, ok_wraps_result_in_some_test/0, ok_has_empty_messages_test/0, error_sets_successful_false_test/0, error_result_is_none_test/0, error_preserves_messages_test/0, error_with_empty_messages_test/0, message_has_no_field_test/0, message_has_no_code_test/0, message_sets_message_test/0, message_for_sets_field_test/0, message_for_sets_message_test/0, message_for_has_no_code_test/0, with_code_sets_code_test/0, with_code_preserves_field_and_message_test/0, with_code_on_top_level_message_test/0, validation_message_type_name_test/0, validation_message_type_has_field_field_test/0, validation_message_type_has_message_field_test/0, validation_message_type_has_code_field_test/0, validation_message_type_message_is_non_null_test/0, validation_message_type_field_is_nullable_test/0, payload_type_name_test/0, payload_type_has_successful_field_test/0, payload_type_successful_is_non_null_bool_test/0, payload_type_has_messages_field_test/0, payload_type_has_result_field_test/0, payload_type_result_points_to_result_type_test/0, payload_types_returns_both_types_test/0, vm_to_dynamic_message_field_test/0, vm_to_dynamic_field_present_test/0, vm_to_dynamic_code_present_test/0, to_dynamic_successful_true_test/0, to_dynamic_successful_false_test/0, to_dynamic_result_present_on_success_test/0, to_dynamic_messages_list_test/0, e2e_successful_mutation_test/0, e2e_successful_payload_fields_test/0, e2e_failed_mutation_test/0, e2e_failed_payload_fields_test/0, e2e_failed_result_is_null_test/0, e2e_multiple_messages_test/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), binary()}.

-file("test/payload_test.gleam", 24).
-spec user_to_dynamic(user()) -> gleam@dynamic:dynamic_().
user_to_dynamic(U) ->
    mochi@types:record(
        [mochi@types:field(<<"id"/utf8>>, erlang:element(2, U)),
            mochi@types:field(<<"name"/utf8>>, erlang:element(3, U)),
            mochi@types:field(<<"email"/utf8>>, erlang:element(4, U))]
    ).

-file("test/payload_test.gleam", 32).
-spec default_ctx() -> mochi@schema:execution_context().
default_ctx() ->
    mochi@schema:execution_context(gleam_stdlib:identity(maps:new())).

-file("test/payload_test.gleam", 36).
-spec build_mutation_schema(
    fun((binary(), mochi@schema:execution_context()) -> mochi@payload:mutation_payload(user()))
) -> mochi@schema:schema().
build_mutation_schema(Resolver) ->
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
            fun(_) -> {ok, {user, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}} end
        )
    end,
    {Create_user_payload, Vm_type} = mochi@payload:payload_types(
        <<"CreateUser"/utf8>>,
        <<"User"/utf8>>
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
    _pipe@6 = mochi@query:add_type(_pipe@5, Vm_type),
    _pipe@7 = mochi@query:add_type(_pipe@6, Create_user_payload),
    _pipe@8 = mochi@query:add_mutation(
        _pipe@7,
        mochi@query:mutation(
            <<"createUser"/utf8>>,
            [mochi@query:arg(
                    <<"name"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                )],
            mochi@schema:named_type(<<"CreateUserPayload"/utf8>>),
            fun(Args) -> mochi@query:get_string(Args, <<"name"/utf8>>) end,
            fun(Name, Ctx) -> {ok, Resolver(Name, Ctx)} end,
            fun(P) -> mochi@payload:to_dynamic(P, fun user_to_dynamic/1) end
        )
    ),
    mochi@query:build(_pipe@8).

-file("test/payload_test.gleam", 70).
-spec ok_sets_successful_true_test() -> nil.
ok_sets_successful_true_test() ->
    P = mochi@payload:ok(
        {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>}
    ),
    gleeunit@should:equal(erlang:element(2, P), true).

-file("test/payload_test.gleam", 75).
-spec ok_wraps_result_in_some_test() -> nil.
ok_wraps_result_in_some_test() ->
    User = {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>},
    P = mochi@payload:ok(User),
    gleeunit@should:equal(erlang:element(3, P), {some, User}).

-file("test/payload_test.gleam", 81).
-spec ok_has_empty_messages_test() -> nil.
ok_has_empty_messages_test() ->
    P = mochi@payload:ok(
        {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>}
    ),
    gleeunit@should:equal(erlang:element(4, P), []).

-file("test/payload_test.gleam", 90).
-spec error_sets_successful_false_test() -> nil.
error_sets_successful_false_test() ->
    P = mochi@payload:error(
        [mochi@payload:message(<<"Something went wrong"/utf8>>)]
    ),
    gleeunit@should:equal(erlang:element(2, P), false).

-file("test/payload_test.gleam", 96).
-spec error_result_is_none_test() -> nil.
error_result_is_none_test() ->
    P = mochi@payload:error(
        [mochi@payload:message(<<"Something went wrong"/utf8>>)]
    ),
    gleeunit@should:equal(erlang:element(3, P), none).

-file("test/payload_test.gleam", 102).
-spec error_preserves_messages_test() -> nil.
error_preserves_messages_test() ->
    Msgs = [mochi@payload:message(<<"name is required"/utf8>>),
        mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>)],
    P = mochi@payload:error(Msgs),
    gleeunit@should:equal(erlang:length(erlang:element(4, P)), 2).

-file("test/payload_test.gleam", 111).
-spec error_with_empty_messages_test() -> nil.
error_with_empty_messages_test() ->
    P = mochi@payload:error([]),
    gleeunit@should:equal(erlang:element(2, P), false),
    gleeunit@should:equal(erlang:element(4, P), []).

-file("test/payload_test.gleam", 121).
-spec message_has_no_field_test() -> nil.
message_has_no_field_test() ->
    Vm = mochi@payload:message(<<"Something failed"/utf8>>),
    gleeunit@should:equal(erlang:element(2, Vm), none).

-file("test/payload_test.gleam", 126).
-spec message_has_no_code_test() -> nil.
message_has_no_code_test() ->
    Vm = mochi@payload:message(<<"Something failed"/utf8>>),
    gleeunit@should:equal(erlang:element(4, Vm), none).

-file("test/payload_test.gleam", 131).
-spec message_sets_message_test() -> nil.
message_sets_message_test() ->
    Vm = mochi@payload:message(<<"Something failed"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Vm), <<"Something failed"/utf8>>).

-file("test/payload_test.gleam", 140).
-spec message_for_sets_field_test() -> nil.
message_for_sets_field_test() ->
    Vm = mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>),
    gleeunit@should:equal(erlang:element(2, Vm), {some, <<"email"/utf8>>}).

-file("test/payload_test.gleam", 145).
-spec message_for_sets_message_test() -> nil.
message_for_sets_message_test() ->
    Vm = mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>),
    gleeunit@should:equal(erlang:element(3, Vm), <<"is invalid"/utf8>>).

-file("test/payload_test.gleam", 150).
-spec message_for_has_no_code_test() -> nil.
message_for_has_no_code_test() ->
    Vm = mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>),
    gleeunit@should:equal(erlang:element(4, Vm), none).

-file("test/payload_test.gleam", 159).
-spec with_code_sets_code_test() -> nil.
with_code_sets_code_test() ->
    Vm = begin
        _pipe = mochi@payload:message_for(
            <<"email"/utf8>>,
            <<"has already been taken"/utf8>>
        ),
        mochi@payload:with_code(_pipe, <<"already_taken"/utf8>>)
    end,
    gleeunit@should:equal(
        erlang:element(4, Vm),
        {some, <<"already_taken"/utf8>>}
    ).

-file("test/payload_test.gleam", 166).
-spec with_code_preserves_field_and_message_test() -> nil.
with_code_preserves_field_and_message_test() ->
    Vm = begin
        _pipe = mochi@payload:message_for(
            <<"email"/utf8>>,
            <<"has already been taken"/utf8>>
        ),
        mochi@payload:with_code(_pipe, <<"already_taken"/utf8>>)
    end,
    gleeunit@should:equal(erlang:element(2, Vm), {some, <<"email"/utf8>>}),
    gleeunit@should:equal(
        erlang:element(3, Vm),
        <<"has already been taken"/utf8>>
    ).

-file("test/payload_test.gleam", 174).
-spec with_code_on_top_level_message_test() -> nil.
with_code_on_top_level_message_test() ->
    Vm = begin
        _pipe = mochi@payload:message(<<"server error"/utf8>>),
        mochi@payload:with_code(_pipe, <<"internal_error"/utf8>>)
    end,
    gleeunit@should:equal(erlang:element(2, Vm), none),
    gleeunit@should:equal(
        erlang:element(4, Vm),
        {some, <<"internal_error"/utf8>>}
    ).

-file("test/payload_test.gleam", 186).
-spec validation_message_type_name_test() -> nil.
validation_message_type_name_test() ->
    T = mochi@payload:validation_message_type(),
    gleeunit@should:equal(erlang:element(2, T), <<"ValidationMessage"/utf8>>).

-file("test/payload_test.gleam", 191).
-spec validation_message_type_has_field_field_test() -> nil.
validation_message_type_has_field_field_test() ->
    T = mochi@payload:validation_message_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"field"/utf8>>)
    ).

-file("test/payload_test.gleam", 196).
-spec validation_message_type_has_message_field_test() -> nil.
validation_message_type_has_message_field_test() ->
    T = mochi@payload:validation_message_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"message"/utf8>>)
    ).

-file("test/payload_test.gleam", 201).
-spec validation_message_type_has_code_field_test() -> nil.
validation_message_type_has_code_field_test() ->
    T = mochi@payload:validation_message_type(),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"code"/utf8>>)
    ).

-file("test/payload_test.gleam", 206).
-spec validation_message_type_message_is_non_null_test() -> nil.
validation_message_type_message_is_non_null_test() ->
    T = mochi@payload:validation_message_type(),
    case gleam_stdlib:map_get(erlang:element(4, T), <<"message"/utf8>>) of
        {ok, F} ->
            case erlang:element(4, F) of
                {non_null, {named, <<"String"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"message field should be NonNull(String)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"validation_message_type_message_is_non_null_test"/utf8>>,
                            line => 212})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"ValidationMessage should have 'message' field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"validation_message_type_message_is_non_null_test"/utf8>>,
                    line => 214})
    end.

-file("test/payload_test.gleam", 218).
-spec validation_message_type_field_is_nullable_test() -> nil.
validation_message_type_field_is_nullable_test() ->
    T = mochi@payload:validation_message_type(),
    case gleam_stdlib:map_get(erlang:element(4, T), <<"field"/utf8>>) of
        {ok, F} ->
            case erlang:element(4, F) of
                {named, <<"String"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"field field should be nullable String"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"validation_message_type_field_is_nullable_test"/utf8>>,
                            line => 224})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"ValidationMessage should have 'field' field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"validation_message_type_field_is_nullable_test"/utf8>>,
                    line => 226})
    end.

-file("test/payload_test.gleam", 234).
-spec payload_type_name_test() -> nil.
payload_type_name_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    gleeunit@should:equal(erlang:element(2, T), <<"CreateUserPayload"/utf8>>).

-file("test/payload_test.gleam", 239).
-spec payload_type_has_successful_field_test() -> nil.
payload_type_has_successful_field_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"successful"/utf8>>)
    ).

-file("test/payload_test.gleam", 244).
-spec payload_type_successful_is_non_null_bool_test() -> nil.
payload_type_successful_is_non_null_bool_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    case gleam_stdlib:map_get(erlang:element(4, T), <<"successful"/utf8>>) of
        {ok, F} ->
            case erlang:element(4, F) of
                {non_null, {named, <<"Boolean"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"successful should be NonNull(Boolean)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"payload_type_successful_is_non_null_bool_test"/utf8>>,
                            line => 250})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"payload should have 'successful' field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"payload_type_successful_is_non_null_bool_test"/utf8>>,
                    line => 252})
    end.

-file("test/payload_test.gleam", 256).
-spec payload_type_has_messages_field_test() -> nil.
payload_type_has_messages_field_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"messages"/utf8>>)
    ).

-file("test/payload_test.gleam", 261).
-spec payload_type_has_result_field_test() -> nil.
payload_type_has_result_field_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    gleeunit@should:be_true(
        gleam@dict:has_key(erlang:element(4, T), <<"result"/utf8>>)
    ).

-file("test/payload_test.gleam", 266).
-spec payload_type_result_points_to_result_type_test() -> nil.
payload_type_result_points_to_result_type_test() ->
    T = mochi@payload:payload_type(<<"CreateUser"/utf8>>, <<"User"/utf8>>),
    case gleam_stdlib:map_get(erlang:element(4, T), <<"result"/utf8>>) of
        {ok, F} ->
            case erlang:element(4, F) of
                {named, <<"User"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"result field should be Named('User')"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"payload_type_result_points_to_result_type_test"/utf8>>,
                            line => 272})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"payload should have 'result' field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"payload_type_result_points_to_result_type_test"/utf8>>,
                    line => 274})
    end.

-file("test/payload_test.gleam", 282).
-spec payload_types_returns_both_types_test() -> nil.
payload_types_returns_both_types_test() ->
    {P, Vm} = mochi@payload:payload_types(
        <<"UpdateUser"/utf8>>,
        <<"User"/utf8>>
    ),
    gleeunit@should:equal(erlang:element(2, P), <<"UpdateUserPayload"/utf8>>),
    gleeunit@should:equal(erlang:element(2, Vm), <<"ValidationMessage"/utf8>>).

-file("test/payload_test.gleam", 292).
-spec vm_to_dynamic_message_field_test() -> nil.
vm_to_dynamic_message_field_test() ->
    Vm = mochi@payload:message(<<"name is required"/utf8>>),
    Dyn = mochi@payload:validation_message_to_dynamic(Vm),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"message"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ) of
        {ok, <<"name is required"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"message should be 'name is required'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"vm_to_dynamic_message_field_test"/utf8>>,
                    line => 297})
    end.

-file("test/payload_test.gleam", 301).
-spec vm_to_dynamic_field_present_test() -> nil.
vm_to_dynamic_field_present_test() ->
    Vm = mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>),
    Dyn = mochi@payload:validation_message_to_dynamic(Vm),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"field"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ) of
        {ok, <<"email"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"field should be 'email'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"vm_to_dynamic_field_present_test"/utf8>>,
                    line => 306})
    end.

-file("test/payload_test.gleam", 310).
-spec vm_to_dynamic_code_present_test() -> nil.
vm_to_dynamic_code_present_test() ->
    Vm = begin
        _pipe = mochi@payload:message_for(<<"email"/utf8>>, <<"taken"/utf8>>),
        mochi@payload:with_code(_pipe, <<"already_taken"/utf8>>)
    end,
    Dyn = mochi@payload:validation_message_to_dynamic(Vm),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"code"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ) of
        {ok, <<"already_taken"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"code should be 'already_taken'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"vm_to_dynamic_code_present_test"/utf8>>,
                    line => 317})
    end.

-file("test/payload_test.gleam", 325).
-spec to_dynamic_successful_true_test() -> nil.
to_dynamic_successful_true_test() ->
    User = {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>},
    P = mochi@payload:ok(User),
    Dyn = mochi@payload:to_dynamic(P, fun user_to_dynamic/1),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"successful"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_bool/1}
        )
    ) of
        {ok, true} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"successful should be True"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"to_dynamic_successful_true_test"/utf8>>,
                    line => 331})
    end.

-file("test/payload_test.gleam", 335).
-spec to_dynamic_successful_false_test() -> nil.
to_dynamic_successful_false_test() ->
    P = mochi@payload:error([mochi@payload:message(<<"failed"/utf8>>)]),
    Dyn = mochi@payload:to_dynamic(P, fun user_to_dynamic/1),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"successful"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_bool/1}
        )
    ) of
        {ok, false} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"successful should be False"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"to_dynamic_successful_false_test"/utf8>>,
                    line => 341})
    end.

-file("test/payload_test.gleam", 345).
-spec to_dynamic_result_present_on_success_test() -> nil.
to_dynamic_result_present_on_success_test() ->
    User = {user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>},
    P = mochi@payload:ok(User),
    Dyn = mochi@payload:to_dynamic(P, fun user_to_dynamic/1),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"result"/utf8>>, <<"name"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ) of
        {ok, <<"Alice"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"result.name should be 'Alice'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"to_dynamic_result_present_on_success_test"/utf8>>,
                    line => 351})
    end.

-file("test/payload_test.gleam", 355).
-spec to_dynamic_messages_list_test() -> nil.
to_dynamic_messages_list_test() ->
    Msgs = [mochi@payload:message(<<"name is required"/utf8>>),
        mochi@payload:message_for(<<"email"/utf8>>, <<"is invalid"/utf8>>)],
    P = mochi@payload:error(Msgs),
    Dyn = mochi@payload:to_dynamic(P, fun user_to_dynamic/1),
    case gleam@dynamic@decode:run(
        Dyn,
        gleam@dynamic@decode:at(
            [<<"messages"/utf8>>],
            gleam@dynamic@decode:list(
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
            )
        )
    ) of
        {ok, Ms} ->
            gleeunit@should:equal(erlang:length(Ms), 2);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"messages should be a list"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"to_dynamic_messages_list_test"/utf8>>,
                    line => 364})
    end.

-file("test/payload_test.gleam", 372).
-spec e2e_successful_mutation_test() -> nil.
e2e_successful_mutation_test() ->
    Schema = build_mutation_schema(
        fun(Name, _) ->
            mochi@payload:ok(
                {user,
                    <<"new-1"/utf8>>,
                    Name,
                    <<Name/binary, "@example.com"/utf8>>}
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"Bob\") { successful messages { field message code } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe = erlang:element(2, Result),
            gleam@option:is_some(_pipe)
        end
    ).

-file("test/payload_test.gleam", 390).
-spec e2e_successful_payload_fields_test() -> nil.
e2e_successful_payload_fields_test() ->
    Schema = build_mutation_schema(
        fun(Name, _) ->
            mochi@payload:ok(
                {user,
                    <<"new-1"/utf8>>,
                    Name,
                    <<Name/binary, "@example.com"/utf8>>}
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"Bob\") { successful result { name } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    case erlang:element(2, Result) of
        {some, Data} ->
            case gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [<<"createUser"/utf8>>, <<"successful"/utf8>>],
                    {decoder, fun gleam@dynamic@decode:decode_bool/1}
                )
            ) of
                {ok, true} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"successful should be True"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"e2e_successful_payload_fields_test"/utf8>>,
                            line => 412})
            end,
            case gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [<<"createUser"/utf8>>, <<"result"/utf8>>, <<"name"/utf8>>],
                    {decoder, fun gleam@dynamic@decode:decode_string/1}
                )
            ) of
                {ok, <<"Bob"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"result.name should be 'Bob'"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"e2e_successful_payload_fields_test"/utf8>>,
                            line => 421})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"e2e_successful_payload_fields_test"/utf8>>,
                    line => 424})
    end.

-file("test/payload_test.gleam", 432).
-spec e2e_failed_mutation_test() -> nil.
e2e_failed_mutation_test() ->
    Schema = build_mutation_schema(
        fun(_, _) ->
            mochi@payload:error(
                [begin
                        _pipe = mochi@payload:message_for(
                            <<"name"/utf8>>,
                            <<"has already been taken"/utf8>>
                        ),
                        mochi@payload:with_code(_pipe, <<"already_taken"/utf8>>)
                    end]
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"Alice\") { successful messages { field message code } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []),
    gleeunit@should:be_true(
        begin
            _pipe@1 = erlang:element(2, Result),
            gleam@option:is_some(_pipe@1)
        end
    ).

-file("test/payload_test.gleam", 454).
-spec e2e_failed_payload_fields_test() -> nil.
e2e_failed_payload_fields_test() ->
    Schema = build_mutation_schema(
        fun(_, _) ->
            mochi@payload:error(
                [begin
                        _pipe = mochi@payload:message_for(
                            <<"name"/utf8>>,
                            <<"has already been taken"/utf8>>
                        ),
                        mochi@payload:with_code(_pipe, <<"already_taken"/utf8>>)
                    end]
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"Alice\") { successful messages { field message code } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    case erlang:element(2, Result) of
        {some, Data} ->
            case gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [<<"createUser"/utf8>>, <<"successful"/utf8>>],
                    {decoder, fun gleam@dynamic@decode:decode_bool/1}
                )
            ) of
                {ok, false} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"successful should be False"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"e2e_failed_payload_fields_test"/utf8>>,
                            line => 477})
            end,
            case gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [<<"createUser"/utf8>>, <<"messages"/utf8>>],
                    gleam@dynamic@decode:list(
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                )
            ) of
                {ok, [Msg]} ->
                    case gleam@dynamic@decode:run(
                        Msg,
                        gleam@dynamic@decode:at(
                            [<<"field"/utf8>>],
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        )
                    ) of
                        {ok, <<"name"/utf8>>} ->
                            nil;

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"message field should be 'name'"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"payload_test"/utf8>>,
                                    function => <<"e2e_failed_payload_fields_test"/utf8>>,
                                    line => 488})
                    end,
                    case gleam@dynamic@decode:run(
                        Msg,
                        gleam@dynamic@decode:at(
                            [<<"code"/utf8>>],
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        )
                    ) of
                        {ok, <<"already_taken"/utf8>>} ->
                            nil;

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"message code should be 'already_taken'"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"payload_test"/utf8>>,
                                    function => <<"e2e_failed_payload_fields_test"/utf8>>,
                                    line => 492})
                    end;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"messages should have exactly 1 item"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"e2e_failed_payload_fields_test"/utf8>>,
                            line => 495})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"e2e_failed_payload_fields_test"/utf8>>,
                    line => 498})
    end.

-file("test/payload_test.gleam", 502).
-spec e2e_failed_result_is_null_test() -> nil.
e2e_failed_result_is_null_test() ->
    Schema = build_mutation_schema(
        fun(_, _) ->
            mochi@payload:error(
                [mochi@payload:message(<<"validation failed"/utf8>>)]
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"X\") { successful result { id } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    gleeunit@should:equal(erlang:element(3, Result), []).

-file("test/payload_test.gleam", 519).
-spec e2e_multiple_messages_test() -> nil.
e2e_multiple_messages_test() ->
    Schema = build_mutation_schema(
        fun(_, _) ->
            mochi@payload:error(
                [mochi@payload:message_for(
                        <<"name"/utf8>>,
                        <<"is too short"/utf8>>
                    ),
                    mochi@payload:message_for(
                        <<"email"/utf8>>,
                        <<"is invalid"/utf8>>
                    ),
                    mochi@payload:message(<<"general failure"/utf8>>)]
            )
        end
    ),
    Result = mochi@executor:execute_query_with_context(
        Schema,
        <<"mutation { createUser(name: \"X\") { successful messages { field message } } }"/utf8>>,
        maps:new(),
        default_ctx()
    ),
    case erlang:element(2, Result) of
        {some, Data} ->
            case gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [<<"createUser"/utf8>>, <<"messages"/utf8>>],
                    gleam@dynamic@decode:list(
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                )
            ) of
                {ok, Msgs} ->
                    gleeunit@should:equal(erlang:length(Msgs), 3);

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"messages should be a list"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"payload_test"/utf8>>,
                            function => <<"e2e_multiple_messages_test"/utf8>>,
                            line => 546})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"payload_test"/utf8>>,
                    function => <<"e2e_multiple_messages_test"/utf8>>,
                    line => 549})
    end.
