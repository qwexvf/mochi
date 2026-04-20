-module(code_first_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/code_first_test.gleam").
-export([types_object_builder_test/0, types_with_description_test/0, types_string_with_desc_test/0, types_int_field_test/0, types_float_field_test/0, types_bool_field_test/0, types_list_string_test/0, types_enum_builder_test/0, query_simple_test/0, query_with_description_test/0, query_with_args_test/0, mutation_test/0, mutation_with_description_test/0, arg_with_description_test/0, schema_builder_test/0, schema_with_mutation_test/0, schema_multiple_queries_test/0, field_deprecation_test/0, field_deprecation_no_reason_test/0, enum_value_deprecation_test/0, field_deduplication_test/0, aliased_duplicate_field_test/0, get_string_or_present_test/0, get_string_or_missing_test/0, get_int_or_present_test/0, get_int_or_missing_test/0, get_float_or_present_test/0, get_float_or_missing_test/0, get_bool_or_present_test/0, get_bool_or_missing_test/0, get_id_or_present_test/0, get_id_or_missing_test/0, arg_with_default_test/0, arg_with_default_desc_test/0, non_null_string_test/0, non_null_string_with_desc_test/0, non_null_int_test/0, non_null_float_test/0, non_null_bool_test/0, input_builder_basic_test/0, input_builder_all_types_test/0, input_builder_optional_fields_test/0, input_builder_custom_field_test/0, input_builder_with_default_test/0, schema_builder_with_input_test/0, field_with_args_test/0, enum_mapping_test/0, enum_mapping_with_desc_test/0, enum_from_mappings_test/0, enum_from_mappings_with_desc_test/0, build_with_encoder_returns_type_and_encoder_test/0, encoder_generates_function_from_builder_test/0, encoder_with_all_field_types_test/0, encoder_ignores_fields_with_args_test/0]).
-export_type([user/0, post/0, test_status/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type user() :: {user, binary(), binary(), binary(), integer()}.

-type post() :: {post, binary(), binary(), binary()}.

-type test_status() :: active | inactive | pending.

-file("test/code_first_test.gleam", 26).
-spec decode_user(gleam@dynamic:dynamic_()) -> {ok, user()} | {error, binary()}.
decode_user(_) ->
    {ok, {user, <<"1"/utf8>>, <<"Test"/utf8>>, <<"test@example.com"/utf8>>, 25}}.

-file("test/code_first_test.gleam", 35).
-spec types_object_builder_test() -> nil.
types_object_builder_test() ->
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
            fun(U@2) -> erlang:element(5, U@2) end
        ),
        mochi@types:build(_pipe@3, fun decode_user/1)
    end,
    case erlang:element(2, User_type) =:= <<"User"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Type name should be 'User'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_object_builder_test"/utf8>>,
                    line => 45})
    end,
    case maps:size(erlang:element(4, User_type)) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 3 fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_object_builder_test"/utf8>>,
                    line => 50})
    end.

-file("test/code_first_test.gleam", 54).
-spec types_with_description_test() -> nil.
types_with_description_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:description(
            _pipe,
            <<"A user in the system"/utf8>>
        ),
        _pipe@2 = mochi@types:id(
            _pipe@1,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    case erlang:element(3, User_type) of
        {some, <<"A user in the system"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_with_description_test"/utf8>>,
                    line => 63})
    end.

-file("test/code_first_test.gleam", 67).
-spec types_string_with_desc_test() -> nil.
types_string_with_desc_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:string_with_desc(
            _pipe,
            <<"name"/utf8>>,
            <<"The user's name"/utf8>>,
            fun(U) -> erlang:element(3, U) end
        ),
        mochi@types:build(_pipe@1, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"name"/utf8>>) of
        {ok, Field} ->
            case erlang:element(3, Field) of
                {some, <<"The user's name"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field description should be set"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"types_string_with_desc_test"/utf8>>,
                            line => 77})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'name' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_string_with_desc_test"/utf8>>,
                    line => 79})
    end.

-file("test/code_first_test.gleam", 83).
-spec types_int_field_test() -> nil.
types_int_field_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:int(
            _pipe,
            <<"age"/utf8>>,
            fun(U) -> erlang:element(5, U) end
        ),
        mochi@types:build(_pipe@1, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"age"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {named, <<"Int"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be Int"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"types_int_field_test"/utf8>>,
                            line => 93})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'age' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_int_field_test"/utf8>>,
                    line => 95})
    end.

-file("test/code_first_test.gleam", 99).
-spec types_float_field_test() -> nil.
types_float_field_test() ->
    Builder = begin
        _pipe = mochi@types:object(<<"Stats"/utf8>>),
        _pipe@1 = mochi@types:float(_pipe, <<"score"/utf8>>, fun(_) -> 3.14 end),
        mochi@types:build(_pipe@1, fun(_) -> {ok, nil} end)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Builder), <<"score"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {named, <<"Float"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be Float"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"types_float_field_test"/utf8>>,
                            line => 109})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'score' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_float_field_test"/utf8>>,
                    line => 111})
    end.

-file("test/code_first_test.gleam", 115).
-spec types_bool_field_test() -> nil.
types_bool_field_test() ->
    Builder = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:bool(_pipe, <<"active"/utf8>>, fun(_) -> true end),
        mochi@types:build(_pipe@1, fun(_) -> {ok, nil} end)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Builder), <<"active"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {named, <<"Boolean"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be Boolean"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"types_bool_field_test"/utf8>>,
                            line => 125})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'active' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_bool_field_test"/utf8>>,
                    line => 127})
    end.

-file("test/code_first_test.gleam", 131).
-spec types_list_string_test() -> nil.
types_list_string_test() ->
    Builder = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:list_string(
            _pipe,
            <<"tags"/utf8>>,
            fun(_) -> [<<"a"/utf8>>, <<"b"/utf8>>] end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, nil} end)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Builder), <<"tags"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {list, {named, <<"String"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be List(String)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"types_list_string_test"/utf8>>,
                            line => 141})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'tags' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_list_string_test"/utf8>>,
                    line => 143})
    end.

-file("test/code_first_test.gleam", 147).
-spec types_enum_builder_test() -> nil.
types_enum_builder_test() ->
    Role_enum = begin
        _pipe = mochi@types:enum_type(<<"Role"/utf8>>),
        _pipe@1 = mochi@types:enum_description(_pipe, <<"User roles"/utf8>>),
        _pipe@2 = mochi@types:value(_pipe@1, <<"ADMIN"/utf8>>),
        _pipe@3 = mochi@types:value(_pipe@2, <<"USER"/utf8>>),
        _pipe@4 = mochi@types:value_with_desc(
            _pipe@3,
            <<"GUEST"/utf8>>,
            <<"Limited access"/utf8>>
        ),
        mochi@types:build_enum(_pipe@4)
    end,
    case erlang:element(2, Role_enum) =:= <<"Role"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Enum name should be 'Role'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_enum_builder_test"/utf8>>,
                    line => 158})
    end,
    case maps:size(erlang:element(4, Role_enum)) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 3 enum values"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_enum_builder_test"/utf8>>,
                    line => 163})
    end,
    case erlang:element(3, Role_enum) of
        {some, <<"User roles"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Enum description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"types_enum_builder_test"/utf8>>,
                    line => 168})
    end.

-file("test/code_first_test.gleam", 176).
-spec query_simple_test() -> nil.
query_simple_test() ->
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, []} end,
        fun(_) -> gleam_stdlib:identity([]) end
    ),
    case erlang:element(2, Users_query) =:= <<"users"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query name should be 'users'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"query_simple_test"/utf8>>,
                    line => 187})
    end.

-file("test/code_first_test.gleam", 191).
-spec query_with_description_test() -> nil.
query_with_description_test() ->
    Users_query = begin
        _pipe = mochi@query:'query'(
            <<"users"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
            fun(_) -> {ok, []} end,
            fun(_) -> gleam_stdlib:identity([]) end
        ),
        mochi@query:query_description(_pipe, <<"Get all users"/utf8>>)
    end,
    case erlang:element(3, Users_query) of
        {some, <<"Get all users"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"query_with_description_test"/utf8>>,
                    line => 203})
    end.

-file("test/code_first_test.gleam", 207).
-spec query_with_args_test() -> nil.
query_with_args_test() ->
    User_query = mochi@query:query_with_args(
        <<"user"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(Args) -> case gleam_stdlib:map_get(Args, <<"id"/utf8>>) of
                {ok, _} ->
                    {ok, <<"1"/utf8>>};

                {error, _} ->
                    {error, <<"Missing id"/utf8>>}
            end end,
        fun(_, _) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Test"/utf8>>,
                    <<"test@example.com"/utf8>>,
                    25}}
        end,
        fun(U) -> gleam_stdlib:identity(U) end
    ),
    case erlang:element(2, User_query) =:= <<"user"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Query name should be 'user'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"query_with_args_test"/utf8>>,
                    line => 225})
    end,
    case erlang:element(7, User_query) of
        [Arg] ->
            case erlang:element(2, Arg) =:= <<"id"/utf8>> of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Arg name should be 'id'"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"query_with_args_test"/utf8>>,
                            line => 232})
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 1 argument"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"query_with_args_test"/utf8>>,
                    line => 234})
    end.

-file("test/code_first_test.gleam", 238).
-spec mutation_test() -> nil.
mutation_test() ->
    Create_user = mochi@query:mutation(
        <<"createUser"/utf8>>,
        [mochi@query:arg(
                <<"name"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) -> {ok, <<"Test"/utf8>>} end,
        fun(_, _) ->
            {ok,
                {user,
                    <<"new"/utf8>>,
                    <<"Test"/utf8>>,
                    <<"test@example.com"/utf8>>,
                    0}}
        end,
        fun(U) -> gleam_stdlib:identity(U) end
    ),
    case erlang:element(2, Create_user) =:= <<"createUser"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation name should be 'createUser'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"mutation_test"/utf8>>,
                    line => 251})
    end.

-file("test/code_first_test.gleam", 255).
-spec mutation_with_description_test() -> nil.
mutation_with_description_test() ->
    Create_user = begin
        _pipe = mochi@query:mutation(
            <<"createUser"/utf8>>,
            [],
            mochi@schema:named_type(<<"User"/utf8>>),
            fun(_) -> {ok, nil} end,
            fun(_, _) ->
                {ok,
                    {user,
                        <<"new"/utf8>>,
                        <<"Test"/utf8>>,
                        <<"test@example.com"/utf8>>,
                        0}}
            end,
            fun(U) -> gleam_stdlib:identity(U) end
        ),
        mochi@query:mutation_description(_pipe, <<"Create a new user"/utf8>>)
    end,
    case erlang:element(3, Create_user) of
        {some, <<"Create a new user"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mutation description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"mutation_with_description_test"/utf8>>,
                    line => 269})
    end.

-file("test/code_first_test.gleam", 273).
-spec arg_with_description_test() -> nil.
arg_with_description_test() ->
    Arg = mochi@query:arg_with_desc(
        <<"id"/utf8>>,
        mochi@schema:id_type(),
        <<"The user ID"/utf8>>
    ),
    case (erlang:element(2, Arg) =:= <<"id"/utf8>>) andalso (erlang:element(
        4,
        Arg
    )
    =:= {some, <<"The user ID"/utf8>>}) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Arg should have name and description"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"arg_with_description_test"/utf8>>,
                    line => 278})
    end.

-file("test/code_first_test.gleam", 286).
-spec schema_builder_test() -> nil.
schema_builder_test() ->
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
        fun(_) -> {ok, []} end,
        fun(_) -> gleam_stdlib:identity([]) end
    ),
    Built_schema = begin
        _pipe@3 = mochi@query:new(),
        _pipe@4 = mochi@query:add_query(_pipe@3, Users_query),
        _pipe@5 = mochi@query:add_type(_pipe@4, User_type),
        mochi@query:build(_pipe@5)
    end,
    case erlang:element(2, Built_schema) of
        {some, Q} ->
            case gleam_stdlib:map_get(erlang:element(4, Q), <<"users"/utf8>>) of
                {ok, _} ->
                    nil;

                {error, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Query should have 'users' field"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"schema_builder_test"/utf8>>,
                            line => 311})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have query type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"schema_builder_test"/utf8>>,
                    line => 313})
    end.

-file("test/code_first_test.gleam", 317).
-spec schema_with_mutation_test() -> nil.
schema_with_mutation_test() ->
    Create_user = mochi@query:mutation(
        <<"createUser"/utf8>>,
        [],
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) -> {ok, nil} end,
        fun(_, _) ->
            {ok,
                {user,
                    <<"new"/utf8>>,
                    <<"Test"/utf8>>,
                    <<"test@example.com"/utf8>>,
                    0}}
        end,
        fun(U) -> gleam_stdlib:identity(U) end
    ),
    Built_schema = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_mutation(_pipe, Create_user),
        mochi@query:build(_pipe@1)
    end,
    case erlang:element(3, Built_schema) of
        {some, M} ->
            case gleam_stdlib:map_get(
                erlang:element(4, M),
                <<"createUser"/utf8>>
            ) of
                {ok, _} ->
                    nil;

                {error, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Mutation should have 'createUser' field"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"schema_with_mutation_test"/utf8>>,
                            line => 337})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have mutation type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"schema_with_mutation_test"/utf8>>,
                    line => 339})
    end.

-file("test/code_first_test.gleam", 343).
-spec schema_multiple_queries_test() -> nil.
schema_multiple_queries_test() ->
    Users_query = mochi@query:'query'(
        <<"users"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
        fun(_) -> {ok, []} end,
        fun gleam_stdlib:identity/1
    ),
    Posts_query = mochi@query:'query'(
        <<"posts"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Post"/utf8>>)),
        fun(_) -> {ok, []} end,
        fun gleam_stdlib:identity/1
    ),
    Built_schema = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_query(_pipe, Users_query),
        _pipe@2 = mochi@query:add_query(_pipe@1, Posts_query),
        mochi@query:build(_pipe@2)
    end,
    case erlang:element(2, Built_schema) of
        {some, Q} ->
            case maps:size(erlang:element(4, Q)) >= 2 of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Should have at least 2 query fields"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"schema_multiple_queries_test"/utf8>>,
                            line => 370})
            end;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have query type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"schema_multiple_queries_test"/utf8>>,
                    line => 373})
    end.

-file("test/code_first_test.gleam", 381).
-spec field_deprecation_test() -> nil.
field_deprecation_test() ->
    Field = begin
        _pipe = mochi@schema:field_def(
            <<"oldField"/utf8>>,
            mochi@schema:string_type()
        ),
        mochi@schema:deprecate(_pipe, <<"Use newField instead"/utf8>>)
    end,
    case erlang:element(7, Field) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field should be deprecated"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_deprecation_test"/utf8>>,
                    line => 388})
    end,
    case erlang:element(8, Field) of
        {some, <<"Use newField instead"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Deprecation reason should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_deprecation_test"/utf8>>,
                    line => 393})
    end.

-file("test/code_first_test.gleam", 397).
-spec field_deprecation_no_reason_test() -> nil.
field_deprecation_no_reason_test() ->
    Field = begin
        _pipe = mochi@schema:field_def(
            <<"oldField"/utf8>>,
            mochi@schema:string_type()
        ),
        mochi@schema:deprecate_field(_pipe)
    end,
    case erlang:element(7, Field) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field should be deprecated"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_deprecation_no_reason_test"/utf8>>,
                    line => 404})
    end,
    case erlang:element(8, Field) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Deprecation reason should not be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_deprecation_no_reason_test"/utf8>>,
                    line => 409})
    end.

-file("test/code_first_test.gleam", 413).
-spec enum_value_deprecation_test() -> nil.
enum_value_deprecation_test() ->
    Role_enum = begin
        _pipe = mochi@types:enum_type(<<"Status"/utf8>>),
        _pipe@1 = mochi@types:value(_pipe, <<"ACTIVE"/utf8>>),
        _pipe@2 = mochi@types:deprecated_value(_pipe@1, <<"PENDING"/utf8>>),
        _pipe@3 = mochi@types:deprecated_value_with_reason(
            _pipe@2,
            <<"LEGACY"/utf8>>,
            <<"Use ARCHIVED instead"/utf8>>
        ),
        mochi@types:build_enum(_pipe@3)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Role_enum), <<"ACTIVE"/utf8>>) of
        {ok, V} ->
            case erlang:element(5, V) of
                false ->
                    nil;

                true ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"ACTIVE should not be deprecated"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"enum_value_deprecation_test"/utf8>>,
                            line => 425})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"ACTIVE should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_value_deprecation_test"/utf8>>,
                    line => 427})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Role_enum), <<"PENDING"/utf8>>) of
        {ok, V@1} ->
            case erlang:element(5, V@1) andalso (erlang:element(6, V@1) =:= none) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"PENDING should be deprecated without reason"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"enum_value_deprecation_test"/utf8>>,
                            line => 434})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"PENDING should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_value_deprecation_test"/utf8>>,
                    line => 436})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Role_enum), <<"LEGACY"/utf8>>) of
        {ok, V@2} ->
            case erlang:element(5, V@2) andalso (erlang:element(6, V@2) =:= {some,
                <<"Use ARCHIVED instead"/utf8>>}) of
                true ->
                    nil;

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"LEGACY should be deprecated with reason"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"enum_value_deprecation_test"/utf8>>,
                            line => 445})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"LEGACY should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_value_deprecation_test"/utf8>>,
                    line => 447})
    end.

-file("test/code_first_test.gleam", 455).
-spec build_exec_schema() -> mochi@schema:schema().
build_exec_schema() ->
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
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        {named, <<"User"/utf8>>},
        fun(_) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Alice"/utf8>>,
                    <<"alice@example.com"/utf8>>,
                    30}}
        end,
        fun gleam_stdlib:identity/1
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_query(_pipe@4, User_query),
    _pipe@6 = mochi@query:add_type(_pipe@5, User_type),
    mochi@query:build(_pipe@6).

-file("test/code_first_test.gleam", 479).
?DOC(
    " A field appearing twice in the same selection set should appear once in the\n"
    " response, not duplicated.\n"
).
-spec field_deduplication_test() -> nil.
field_deduplication_test() ->
    Schema_def = build_exec_schema(),
    Result = mochi@executor:execute_query(
        Schema_def,
        <<"{ user { id name name } }"/utf8>>
    ),
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_deduplication_test"/utf8>>,
                    line => 484});

        {some, Data} ->
            User_result = begin
                _pipe = gleam@dynamic@decode:run(
                    Data,
                    gleam@dynamic@decode:at(
                        [<<"user"/utf8>>],
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ),
                gleam@result:lazy_unwrap(
                    _pipe,
                    fun() ->
                        case gleam@dynamic@decode:run(
                            Data,
                            gleam@dynamic@decode:list(
                                {decoder,
                                    fun gleam@dynamic@decode:decode_dynamic/1}
                            )
                        ) of
                            {ok, [First | _]} ->
                                _pipe@1 = gleam@dynamic@decode:run(
                                    First,
                                    gleam@dynamic@decode:at(
                                        [<<"user"/utf8>>],
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_dynamic/1}
                                    )
                                ),
                                gleam@result:unwrap(
                                    _pipe@1,
                                    gleam_stdlib:identity(nil)
                                );

                            _ ->
                                gleam_stdlib:identity(nil)
                        end
                    end
                )
            end,
            Name = begin
                _pipe@2 = gleam@dynamic@decode:run(
                    User_result,
                    gleam@dynamic@decode:at(
                        [<<"name"/utf8>>],
                        {decoder, fun gleam@dynamic@decode:decode_string/1}
                    )
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            case Name of
                <<""/utf8>> ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"name should be present when requested twice"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"field_deduplication_test"/utf8>>,
                            line => 503});

                _ ->
                    nil
            end
    end.

-file("test/code_first_test.gleam", 511).
?DOC(" Requesting the same field with an alias produces separate keys.\n").
-spec aliased_duplicate_field_test() -> nil.
aliased_duplicate_field_test() ->
    Schema_def = build_exec_schema(),
    Result = mochi@executor:execute_query(
        Schema_def,
        <<"{ user { firstName: name lastName: name } }"/utf8>>
    ),
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"aliased_duplicate_field_test"/utf8>>,
                    line => 519});

        {some, _} ->
            case erlang:element(3, Result) of
                [] ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Unexpected errors with aliased duplicate fields"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"aliased_duplicate_field_test"/utf8>>,
                            line => 524})
            end
    end.

-file("test/code_first_test.gleam", 534).
-spec get_string_or_present_test() -> nil.
get_string_or_present_test() ->
    Args = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"Alice"/utf8>>)}]
    ),
    Result = mochi@query:get_string_or(
        Args,
        <<"name"/utf8>>,
        <<"Default"/utf8>>
    ),
    case Result =:= <<"Alice"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return present string value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_string_or_present_test"/utf8>>,
                    line => 539})
    end.

-file("test/code_first_test.gleam", 543).
-spec get_string_or_missing_test() -> nil.
get_string_or_missing_test() ->
    Args = maps:new(),
    Result = mochi@query:get_string_or(
        Args,
        <<"name"/utf8>>,
        <<"Default"/utf8>>
    ),
    case Result =:= <<"Default"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return default when missing"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_string_or_missing_test"/utf8>>,
                    line => 548})
    end.

-file("test/code_first_test.gleam", 552).
-spec get_int_or_present_test() -> nil.
get_int_or_present_test() ->
    Args = maps:from_list([{<<"limit"/utf8>>, gleam_stdlib:identity(25)}]),
    Result = mochi@query:get_int_or(Args, <<"limit"/utf8>>, 10),
    case Result =:= 25 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return present int value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_int_or_present_test"/utf8>>,
                    line => 557})
    end.

-file("test/code_first_test.gleam", 561).
-spec get_int_or_missing_test() -> nil.
get_int_or_missing_test() ->
    Args = maps:new(),
    Result = mochi@query:get_int_or(Args, <<"limit"/utf8>>, 10),
    case Result =:= 10 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return default when missing"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_int_or_missing_test"/utf8>>,
                    line => 566})
    end.

-file("test/code_first_test.gleam", 570).
-spec get_float_or_present_test() -> nil.
get_float_or_present_test() ->
    Args = maps:from_list([{<<"price"/utf8>>, gleam_stdlib:identity(19.99)}]),
    Result = mochi@query:get_float_or(Args, <<"price"/utf8>>, +0.0),
    case Result =:= 19.99 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return present float value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_float_or_present_test"/utf8>>,
                    line => 575})
    end.

-file("test/code_first_test.gleam", 579).
-spec get_float_or_missing_test() -> nil.
get_float_or_missing_test() ->
    Args = maps:new(),
    Result = mochi@query:get_float_or(Args, <<"price"/utf8>>, 9.99),
    case Result =:= 9.99 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return default when missing"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_float_or_missing_test"/utf8>>,
                    line => 584})
    end.

-file("test/code_first_test.gleam", 588).
-spec get_bool_or_present_test() -> nil.
get_bool_or_present_test() ->
    Args = maps:from_list([{<<"active"/utf8>>, gleam_stdlib:identity(false)}]),
    Result = mochi@query:get_bool_or(Args, <<"active"/utf8>>, true),
    case Result =:= false of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return present bool value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_bool_or_present_test"/utf8>>,
                    line => 593})
    end.

-file("test/code_first_test.gleam", 597).
-spec get_bool_or_missing_test() -> nil.
get_bool_or_missing_test() ->
    Args = maps:new(),
    Result = mochi@query:get_bool_or(Args, <<"active"/utf8>>, true),
    case Result =:= true of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return default when missing"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_bool_or_missing_test"/utf8>>,
                    line => 602})
    end.

-file("test/code_first_test.gleam", 606).
-spec get_id_or_present_test() -> nil.
get_id_or_present_test() ->
    Args = maps:from_list(
        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"user-123"/utf8>>)}]
    ),
    Result = mochi@query:get_id_or(Args, <<"id"/utf8>>, <<"default-id"/utf8>>),
    case Result =:= <<"user-123"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return present id value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_id_or_present_test"/utf8>>,
                    line => 611})
    end.

-file("test/code_first_test.gleam", 615).
-spec get_id_or_missing_test() -> nil.
get_id_or_missing_test() ->
    Args = maps:new(),
    Result = mochi@query:get_id_or(Args, <<"id"/utf8>>, <<"default-id"/utf8>>),
    case Result =:= <<"default-id"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should return default when missing"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"get_id_or_missing_test"/utf8>>,
                    line => 620})
    end.

-file("test/code_first_test.gleam", 628).
-spec arg_with_default_test() -> nil.
arg_with_default_test() ->
    Arg = mochi@query:arg_with_default(
        <<"limit"/utf8>>,
        mochi@schema:int_type(),
        gleam_stdlib:identity(10)
    ),
    case erlang:element(2, Arg) =:= <<"limit"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Arg name should be 'limit'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"arg_with_default_test"/utf8>>,
                    line => 633})
    end,
    case erlang:element(5, Arg) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Arg should have default value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"arg_with_default_test"/utf8>>,
                    line => 637})
    end.

-file("test/code_first_test.gleam", 641).
-spec arg_with_default_desc_test() -> nil.
arg_with_default_desc_test() ->
    Arg = mochi@query:arg_with_default_desc(
        <<"limit"/utf8>>,
        mochi@schema:int_type(),
        gleam_stdlib:identity(10),
        <<"Maximum items to return"/utf8>>
    ),
    case (erlang:element(2, Arg) =:= <<"limit"/utf8>>) andalso (erlang:element(
        4,
        Arg
    )
    =:= {some, <<"Maximum items to return"/utf8>>}) of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Arg should have name and description"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"arg_with_default_desc_test"/utf8>>,
                    line => 653})
    end,
    case erlang:element(5, Arg) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Arg should have default value"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"arg_with_default_desc_test"/utf8>>,
                    line => 657})
    end.

-file("test/code_first_test.gleam", 665).
-spec non_null_string_test() -> nil.
non_null_string_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:non_null_string(
            _pipe,
            <<"name"/utf8>>,
            fun(U) -> erlang:element(3, U) end
        ),
        mochi@types:build(_pipe@1, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"name"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"String"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be NonNull(String)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_string_test"/utf8>>,
                            line => 675})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'name' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"non_null_string_test"/utf8>>,
                    line => 677})
    end.

-file("test/code_first_test.gleam", 681).
-spec non_null_string_with_desc_test() -> nil.
non_null_string_with_desc_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:non_null_string_with_desc(
            _pipe,
            <<"name"/utf8>>,
            <<"User's full name"/utf8>>,
            fun(U) -> erlang:element(3, U) end
        ),
        mochi@types:build(_pipe@1, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"name"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"String"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be NonNull(String)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_string_with_desc_test"/utf8>>,
                            line => 693})
            end,
            case erlang:element(3, Field) of
                {some, <<"User's full name"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field description should be set"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_string_with_desc_test"/utf8>>,
                            line => 697})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'name' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"non_null_string_with_desc_test"/utf8>>,
                    line => 700})
    end.

-file("test/code_first_test.gleam", 704).
-spec non_null_int_test() -> nil.
non_null_int_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:non_null_int(
            _pipe,
            <<"age"/utf8>>,
            fun(U) -> erlang:element(5, U) end
        ),
        mochi@types:build(_pipe@1, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"age"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"Int"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be NonNull(Int)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_int_test"/utf8>>,
                            line => 714})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'age' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"non_null_int_test"/utf8>>,
                    line => 716})
    end.

-file("test/code_first_test.gleam", 720).
-spec non_null_float_test() -> nil.
non_null_float_test() ->
    Stats_type = begin
        _pipe = mochi@types:object(<<"Stats"/utf8>>),
        _pipe@1 = mochi@types:non_null_float(
            _pipe,
            <<"score"/utf8>>,
            fun(_) -> 3.14 end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, nil} end)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Stats_type), <<"score"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"Float"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be NonNull(Float)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_float_test"/utf8>>,
                            line => 730})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'score' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"non_null_float_test"/utf8>>,
                    line => 732})
    end.

-file("test/code_first_test.gleam", 736).
-spec non_null_bool_test() -> nil.
non_null_bool_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:non_null_bool(
            _pipe,
            <<"active"/utf8>>,
            fun(_) -> true end
        ),
        mochi@types:build(_pipe@1, fun(_) -> {ok, nil} end)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"active"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"Boolean"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be NonNull(Boolean)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"non_null_bool_test"/utf8>>,
                            line => 746})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'active' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"non_null_bool_test"/utf8>>,
                    line => 748})
    end.

-file("test/code_first_test.gleam", 756).
-spec input_builder_basic_test() -> nil.
input_builder_basic_test() ->
    Input = begin
        _pipe = mochi@types:input(<<"CreateUserInput"/utf8>>),
        _pipe@1 = mochi@types:input_description(
            _pipe,
            <<"Input for creating a new user"/utf8>>
        ),
        _pipe@2 = mochi@types:input_string(
            _pipe@1,
            <<"name"/utf8>>,
            <<"User's full name"/utf8>>
        ),
        _pipe@3 = mochi@types:input_string(
            _pipe@2,
            <<"email"/utf8>>,
            <<"User's email address"/utf8>>
        ),
        mochi@types:build_input(_pipe@3)
    end,
    case erlang:element(2, Input) =:= <<"CreateUserInput"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Input name should be 'CreateUserInput'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_basic_test"/utf8>>,
                    line => 766})
    end,
    case erlang:element(3, Input) of
        {some, <<"Input for creating a new user"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Input description should be set"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_basic_test"/utf8>>,
                    line => 770})
    end,
    case maps:size(erlang:element(4, Input)) =:= 2 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Input should have 2 fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_basic_test"/utf8>>,
                    line => 774})
    end.

-file("test/code_first_test.gleam", 778).
-spec input_builder_all_types_test() -> nil.
input_builder_all_types_test() ->
    Input = begin
        _pipe = mochi@types:input(<<"CompleteInput"/utf8>>),
        _pipe@1 = mochi@types:input_string(
            _pipe,
            <<"name"/utf8>>,
            <<"Name"/utf8>>
        ),
        _pipe@2 = mochi@types:input_int(
            _pipe@1,
            <<"count"/utf8>>,
            <<"Count"/utf8>>
        ),
        _pipe@3 = mochi@types:input_float(
            _pipe@2,
            <<"price"/utf8>>,
            <<"Price"/utf8>>
        ),
        _pipe@4 = mochi@types:input_bool(
            _pipe@3,
            <<"active"/utf8>>,
            <<"Active"/utf8>>
        ),
        _pipe@5 = mochi@types:input_id(
            _pipe@4,
            <<"userId"/utf8>>,
            <<"User ID"/utf8>>
        ),
        mochi@types:build_input(_pipe@5)
    end,
    case maps:size(erlang:element(4, Input)) =:= 5 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Input should have 5 fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_all_types_test"/utf8>>,
                    line => 790})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"name"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {non_null, {named, <<"String"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"name should be NonNull(String)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_all_types_test"/utf8>>,
                            line => 798})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"name field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_all_types_test"/utf8>>,
                    line => 800})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"count"/utf8>>) of
        {ok, Field@1} ->
            case erlang:element(4, Field@1) of
                {non_null, {named, <<"Int"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"count should be NonNull(Int)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_all_types_test"/utf8>>,
                            line => 807})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"count field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_all_types_test"/utf8>>,
                    line => 809})
    end.

-file("test/code_first_test.gleam", 813).
-spec input_builder_optional_fields_test() -> nil.
input_builder_optional_fields_test() ->
    Input = begin
        _pipe = mochi@types:input(<<"SearchInput"/utf8>>),
        _pipe@1 = mochi@types:input_optional_string(
            _pipe,
            <<"query"/utf8>>,
            <<"Search query"/utf8>>
        ),
        _pipe@2 = mochi@types:input_optional_int(
            _pipe@1,
            <<"limit"/utf8>>,
            <<"Max results"/utf8>>
        ),
        _pipe@3 = mochi@types:input_optional_float(
            _pipe@2,
            <<"minPrice"/utf8>>,
            <<"Minimum price"/utf8>>
        ),
        _pipe@4 = mochi@types:input_optional_bool(
            _pipe@3,
            <<"includeInactive"/utf8>>,
            <<"Include inactive"/utf8>>
        ),
        mochi@types:build_input(_pipe@4)
    end,
    case maps:size(erlang:element(4, Input)) =:= 4 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Input should have 4 fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_optional_fields_test"/utf8>>,
                    line => 824})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"query"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {named, <<"String"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"query should be String (nullable)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_optional_fields_test"/utf8>>,
                            line => 832})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"query field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_optional_fields_test"/utf8>>,
                    line => 834})
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"limit"/utf8>>) of
        {ok, Field@1} ->
            case erlang:element(4, Field@1) of
                {named, <<"Int"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"limit should be Int (nullable)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_optional_fields_test"/utf8>>,
                            line => 841})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"limit field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_optional_fields_test"/utf8>>,
                    line => 843})
    end.

-file("test/code_first_test.gleam", 847).
-spec input_builder_custom_field_test() -> nil.
input_builder_custom_field_test() ->
    Input = begin
        _pipe = mochi@types:input(<<"FilterInput"/utf8>>),
        _pipe@1 = mochi@types:input_field(
            _pipe,
            <<"status"/utf8>>,
            mochi@schema:named_type(<<"Status"/utf8>>),
            <<"Filter status"/utf8>>
        ),
        mochi@types:build_input(_pipe@1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"status"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {named, <<"Status"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"status should be Named(Status)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_custom_field_test"/utf8>>,
                            line => 857})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"status field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_custom_field_test"/utf8>>,
                    line => 859})
    end.

-file("test/code_first_test.gleam", 863).
-spec input_builder_with_default_test() -> nil.
input_builder_with_default_test() ->
    Input = begin
        _pipe = mochi@types:input(<<"PaginationInput"/utf8>>),
        _pipe@1 = mochi@types:input_field_with_default(
            _pipe,
            <<"limit"/utf8>>,
            mochi@schema:int_type(),
            gleam_stdlib:identity(10),
            <<"Items per page"/utf8>>
        ),
        mochi@types:build_input(_pipe@1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, Input), <<"limit"/utf8>>) of
        {ok, Field} ->
            case erlang:element(5, Field) of
                {some, _} ->
                    nil;

                none ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"limit should have default value"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"input_builder_with_default_test"/utf8>>,
                            line => 878})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"limit field should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"input_builder_with_default_test"/utf8>>,
                    line => 880})
    end.

-file("test/code_first_test.gleam", 884).
-spec schema_builder_with_input_test() -> nil.
schema_builder_with_input_test() ->
    Create_user_input = begin
        _pipe = mochi@types:input(<<"CreateUserInput"/utf8>>),
        _pipe@1 = mochi@types:input_string(
            _pipe,
            <<"name"/utf8>>,
            <<"User name"/utf8>>
        ),
        mochi@types:build_input(_pipe@1)
    end,
    Built_schema = begin
        _pipe@2 = mochi@query:new(),
        _pipe@3 = mochi@query:add_input(_pipe@2, Create_user_input),
        mochi@query:build(_pipe@3)
    end,
    case gleam_stdlib:map_get(
        erlang:element(5, Built_schema),
        <<"CreateUserInput"/utf8>>
    ) of
        {ok, {input_object_type_def, _}} ->
            nil;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Type should be InputObjectTypeDef"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"schema_builder_with_input_test"/utf8>>,
                    line => 897});

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"CreateUserInput should be in schema types"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"schema_builder_with_input_test"/utf8>>,
                    line => 898})
    end.

-file("test/code_first_test.gleam", 906).
-spec field_with_args_test() -> nil.
field_with_args_test() ->
    User_type = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        _pipe@2 = mochi@types:field_with_args(
            _pipe@1,
            <<"posts"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"Post"/utf8>>)),
            [mochi@schema:arg(<<"limit"/utf8>>, mochi@schema:int_type())],
            <<"User's posts"/utf8>>,
            fun(_, _, _) -> {ok, gleam_stdlib:identity([])} end
        ),
        mochi@types:build(_pipe@2, fun decode_user/1)
    end,
    case gleam_stdlib:map_get(erlang:element(4, User_type), <<"posts"/utf8>>) of
        {ok, Field} ->
            case erlang:element(4, Field) of
                {list, {named, <<"Post"/utf8>>}} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field type should be List(Post)"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"field_with_args_test"/utf8>>,
                            line => 924})
            end,
            case erlang:element(3, Field) of
                {some, <<"User's posts"/utf8>>} ->
                    nil;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field description should be set"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"field_with_args_test"/utf8>>,
                            line => 929})
            end,
            case gleam_stdlib:map_get(
                erlang:element(5, Field),
                <<"limit"/utf8>>
            ) of
                {ok, _} ->
                    nil;

                {error, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Field should have 'limit' argument"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"code_first_test"/utf8>>,
                            function => <<"field_with_args_test"/utf8>>,
                            line => 934})
            end;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Field 'posts' should exist"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"field_with_args_test"/utf8>>,
                    line => 937})
    end.

-file("test/code_first_test.gleam", 951).
-spec enum_mapping_test() -> nil.
enum_mapping_test() ->
    Mapping = mochi@types:enum_mapping(active, <<"ACTIVE"/utf8>>),
    case erlang:element(3, Mapping) =:= <<"ACTIVE"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mapping graphql_name should be 'ACTIVE'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_mapping_test"/utf8>>,
                    line => 955})
    end,
    case erlang:element(4, Mapping) of
        none ->
            nil;

        {some, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mapping should not have description"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_mapping_test"/utf8>>,
                    line => 959})
    end.

-file("test/code_first_test.gleam", 963).
-spec enum_mapping_with_desc_test() -> nil.
enum_mapping_with_desc_test() ->
    Mapping = mochi@types:enum_mapping_with_desc(
        pending,
        <<"PENDING"/utf8>>,
        <<"Awaiting approval"/utf8>>
    ),
    case erlang:element(3, Mapping) =:= <<"PENDING"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mapping graphql_name should be 'PENDING'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_mapping_with_desc_test"/utf8>>,
                    line => 968})
    end,
    case erlang:element(4, Mapping) of
        {some, <<"Awaiting approval"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Mapping should have description"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_mapping_with_desc_test"/utf8>>,
                    line => 972})
    end.

-file("test/code_first_test.gleam", 976).
-spec enum_from_mappings_test() -> nil.
enum_from_mappings_test() ->
    {Enum_type, To_graphql, From_graphql} = mochi@types:enum_from_mappings(
        <<"Status"/utf8>>,
        [mochi@types:enum_mapping(active, <<"ACTIVE"/utf8>>),
            mochi@types:enum_mapping(inactive, <<"INACTIVE"/utf8>>),
            mochi@types:enum_mapping_with_desc(
                pending,
                <<"PENDING"/utf8>>,
                <<"Awaiting approval"/utf8>>
            )]
    ),
    case erlang:element(2, Enum_type) =:= <<"Status"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Enum name should be 'Status'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 987})
    end,
    case maps:size(erlang:element(4, Enum_type)) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Enum should have 3 values"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 991})
    end,
    case To_graphql(active) =:= <<"ACTIVE"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Active should convert to 'ACTIVE'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 997})
    end,
    case To_graphql(inactive) =:= <<"INACTIVE"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Inactive should convert to 'INACTIVE'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 1001})
    end,
    case From_graphql(<<"ACTIVE"/utf8>>) of
        {ok, active} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"'ACTIVE' should convert to Active"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 1007})
    end,
    case From_graphql(<<"INACTIVE"/utf8>>) of
        {ok, inactive} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"'INACTIVE' should convert to Inactive"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 1011})
    end,
    case From_graphql(<<"INVALID"/utf8>>) of
        {error, _} ->
            nil;

        {ok, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"'INVALID' should return Error"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_test"/utf8>>,
                    line => 1015})
    end.

-file("test/code_first_test.gleam", 1019).
-spec enum_from_mappings_with_desc_test() -> nil.
enum_from_mappings_with_desc_test() ->
    {Enum_type, _, _} = mochi@types:enum_from_mappings_with_desc(
        <<"Priority"/utf8>>,
        <<"Task priority levels"/utf8>>,
        [mochi@types:enum_mapping(active, <<"HIGH"/utf8>>)]
    ),
    case erlang:element(3, Enum_type) of
        {some, <<"Task priority levels"/utf8>>} ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Enum should have description"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"enum_from_mappings_with_desc_test"/utf8>>,
                    line => 1027})
    end.

-file("test/code_first_test.gleam", 1035).
-spec build_with_encoder_returns_type_and_encoder_test() -> nil.
build_with_encoder_returns_type_and_encoder_test() ->
    {User_type, User_encoder} = begin
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
            fun(U@2) -> erlang:element(5, U@2) end
        ),
        mochi@types:build_with_encoder(_pipe@3, fun decode_user/1)
    end,
    case erlang:element(2, User_type) =:= <<"User"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Type name should be 'User'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"build_with_encoder_returns_type_and_encoder_test"/utf8>>,
                    line => 1047})
    end,
    case maps:size(erlang:element(4, User_type)) =:= 3 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have 3 fields"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"build_with_encoder_returns_type_and_encoder_test"/utf8>>,
                    line => 1051})
    end,
    Test_user = {user,
        <<"42"/utf8>>,
        <<"Bob"/utf8>>,
        <<"bob@example.com"/utf8>>,
        28},
    Encoded = User_encoder(Test_user),
    Id_result = begin
        _pipe@4 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"id"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@4, <<""/utf8>>)
    end,
    case Id_result =:= <<"42"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Encoded 'id' should be '42'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"build_with_encoder_returns_type_and_encoder_test"/utf8>>,
                    line => 1064})
    end,
    Name_result = begin
        _pipe@5 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"name"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@5, <<""/utf8>>)
    end,
    case Name_result =:= <<"Bob"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Encoded 'name' should be 'Bob'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"build_with_encoder_returns_type_and_encoder_test"/utf8>>,
                    line => 1072})
    end,
    Age_result = begin
        _pipe@6 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"age"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_int/1}
            )
        ),
        gleam@result:unwrap(_pipe@6, 0)
    end,
    case Age_result =:= 28 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Encoded 'age' should be 28"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"build_with_encoder_returns_type_and_encoder_test"/utf8>>,
                    line => 1080})
    end.

-file("test/code_first_test.gleam", 1084).
-spec encoder_generates_function_from_builder_test() -> nil.
encoder_generates_function_from_builder_test() ->
    User_builder = begin
        _pipe = mochi@types:object(<<"User"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(U) -> erlang:element(2, U) end
        ),
        mochi@types:string(
            _pipe@1,
            <<"email"/utf8>>,
            fun(U@1) -> erlang:element(4, U@1) end
        )
    end,
    User_encoder = mochi@types:encoder(User_builder),
    Test_user = {user,
        <<"99"/utf8>>,
        <<"Test"/utf8>>,
        <<"test@test.com"/utf8>>,
        30},
    Encoded = User_encoder(Test_user),
    Id_result = begin
        _pipe@2 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"id"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@2, <<""/utf8>>)
    end,
    case Id_result =:= <<"99"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Encoded 'id' should be '99'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_generates_function_from_builder_test"/utf8>>,
                    line => 1103})
    end,
    Email_result = begin
        _pipe@3 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"email"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@3, <<""/utf8>>)
    end,
    case Email_result =:= <<"test@test.com"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Encoded 'email' should be 'test@test.com'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_generates_function_from_builder_test"/utf8>>,
                    line => 1111})
    end.

-file("test/code_first_test.gleam", 1115).
-spec encoder_with_all_field_types_test() -> nil.
encoder_with_all_field_types_test() ->
    {_, Encoder_fn} = begin
        _pipe = mochi@types:object(<<"Mixed"/utf8>>),
        _pipe@1 = mochi@types:string(
            _pipe,
            <<"str"/utf8>>,
            fun(_) -> <<"hello"/utf8>> end
        ),
        _pipe@2 = mochi@types:int(_pipe@1, <<"num"/utf8>>, fun(_) -> 42 end),
        _pipe@3 = mochi@types:float(_pipe@2, <<"dec"/utf8>>, fun(_) -> 3.14 end),
        _pipe@4 = mochi@types:bool(_pipe@3, <<"flag"/utf8>>, fun(_) -> true end),
        mochi@types:build_with_encoder(_pipe@4, fun(_) -> {ok, nil} end)
    end,
    Encoded = Encoder_fn(nil),
    Str_result = begin
        _pipe@5 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"str"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@5, <<""/utf8>>)
    end,
    case Str_result =:= <<"hello"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"String field should be 'hello'"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_with_all_field_types_test"/utf8>>,
                    line => 1133})
    end,
    Num_result = begin
        _pipe@6 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"num"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_int/1}
            )
        ),
        gleam@result:unwrap(_pipe@6, 0)
    end,
    case Num_result =:= 42 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Int field should be 42"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_with_all_field_types_test"/utf8>>,
                    line => 1142})
    end,
    Dec_result = begin
        _pipe@7 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"dec"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_float/1}
            )
        ),
        gleam@result:unwrap(_pipe@7, +0.0)
    end,
    case Dec_result =:= 3.14 of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Float field should be 3.14"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_with_all_field_types_test"/utf8>>,
                    line => 1151})
    end,
    Flag_result = begin
        _pipe@8 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"flag"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_bool/1}
            )
        ),
        gleam@result:unwrap(_pipe@8, false)
    end,
    case Flag_result =:= true of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Bool field should be True"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_with_all_field_types_test"/utf8>>,
                    line => 1160})
    end.

-file("test/code_first_test.gleam", 1164).
-spec encoder_ignores_fields_with_args_test() -> nil.
encoder_ignores_fields_with_args_test() ->
    {_, Encoder_fn} = begin
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
        _pipe@3 = mochi@types:field_with_args(
            _pipe@2,
            <<"posts"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"Post"/utf8>>)),
            [mochi@schema:arg(<<"limit"/utf8>>, mochi@schema:int_type())],
            <<"User's posts"/utf8>>,
            fun(_, _, _) -> {ok, gleam_stdlib:identity([])} end
        ),
        mochi@types:build_with_encoder(_pipe@3, fun decode_user/1)
    end,
    Test_user = {user,
        <<"1"/utf8>>,
        <<"Alice"/utf8>>,
        <<"alice@test.com"/utf8>>,
        25},
    Encoded = Encoder_fn(Test_user),
    Id_result = begin
        _pipe@4 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"id"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@4, <<""/utf8>>)
    end,
    case Id_result =:= <<"1"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"id should be present"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_ignores_fields_with_args_test"/utf8>>,
                    line => 1189})
    end,
    Name_result = begin
        _pipe@5 = gleam@dynamic@decode:run(
            Encoded,
            gleam@dynamic@decode:at(
                [<<"name"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            )
        ),
        gleam@result:unwrap(_pipe@5, <<""/utf8>>)
    end,
    case Name_result =:= <<"Alice"/utf8>> of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => <<"name should be present"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"code_first_test"/utf8>>,
                    function => <<"encoder_ignores_fields_with_args_test"/utf8>>,
                    line => 1197})
    end,
    nil.
