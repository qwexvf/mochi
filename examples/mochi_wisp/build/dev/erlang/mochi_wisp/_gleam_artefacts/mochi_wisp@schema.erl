-module(mochi_wisp@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/schema.gleam").
-export([sample_users/0, find_user_by_id/1, role_to_string/1, string_to_role/1, role_enum/0, user_type/0, user_to_dynamic/1, users_encoder/1, user_encoder/1, users_resolver/1, user_resolver/2, decode_user_by_id_args/1, build_schema/0]).
-export_type([user/0, role/0, user_by_id_args/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type user() :: {user, binary(), binary(), binary(), role()}.

-type role() :: admin | member | guest.

-type user_by_id_args() :: {user_by_id_args, binary()}.

-file("src/mochi_wisp/schema.gleam", 31).
-spec sample_users() -> list(user()).
sample_users() ->
    [{user, <<"1"/utf8>>, <<"Alice"/utf8>>, <<"alice@example.com"/utf8>>, admin},
        {user, <<"2"/utf8>>, <<"Bob"/utf8>>, <<"bob@example.com"/utf8>>, member},
        {user,
            <<"3"/utf8>>,
            <<"Charlie"/utf8>>,
            <<"charlie@example.com"/utf8>>,
            guest}].

-file("src/mochi_wisp/schema.gleam", 45).
-spec result_from_option({ok, BQM} | {error, nil}, binary()) -> {ok, BQM} |
    {error, binary()}.
result_from_option(Opt, Error) ->
    case Opt of
        {ok, Value} ->
            {ok, Value};

        {error, _} ->
            {error, Error}
    end.

-file("src/mochi_wisp/schema.gleam", 39).
-spec find_user_by_id(binary()) -> {ok, user()} | {error, binary()}.
find_user_by_id(Id) ->
    _pipe = sample_users(),
    _pipe@1 = gleam@list:find(_pipe, fun(U) -> erlang:element(2, U) =:= Id end),
    result_from_option(_pipe@1, <<"User not found: "/utf8, Id/binary>>).

-file("src/mochi_wisp/schema.gleam", 56).
-spec role_to_string(role()) -> binary().
role_to_string(Role) ->
    case Role of
        admin ->
            <<"ADMIN"/utf8>>;

        member ->
            <<"MEMBER"/utf8>>;

        guest ->
            <<"GUEST"/utf8>>
    end.

-file("src/mochi_wisp/schema.gleam", 64).
-spec string_to_role(binary()) -> {ok, role()} | {error, binary()}.
string_to_role(S) ->
    case S of
        <<"ADMIN"/utf8>> ->
            {ok, admin};

        <<"MEMBER"/utf8>> ->
            {ok, member};

        <<"GUEST"/utf8>> ->
            {ok, guest};

        _ ->
            {error, <<"Unknown role: "/utf8, S/binary>>}
    end.

-file("src/mochi_wisp/schema.gleam", 77).
-spec role_enum() -> mochi@schema:enum_type().
role_enum() ->
    _pipe = mochi@types:enum_type(<<"Role"/utf8>>),
    _pipe@1 = mochi@types:enum_description(
        _pipe,
        <<"User role in the system"/utf8>>
    ),
    _pipe@2 = mochi@types:value_with_desc(
        _pipe@1,
        <<"ADMIN"/utf8>>,
        <<"Administrator with full access"/utf8>>
    ),
    _pipe@3 = mochi@types:value_with_desc(
        _pipe@2,
        <<"MEMBER"/utf8>>,
        <<"Regular member"/utf8>>
    ),
    _pipe@4 = mochi@types:value_with_desc(
        _pipe@3,
        <<"GUEST"/utf8>>,
        <<"Guest with limited access"/utf8>>
    ),
    mochi@types:build_enum(_pipe@4).

-file("src/mochi_wisp/schema.gleam", 126).
?DOC(
    " Extract a field from Dynamic parent value\n"
    " Uses FFI because we need to access dynamic map fields\n"
).
-spec extract_field(gleam@option:option(gleam@dynamic:dynamic_()), binary()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
extract_field(Parent, Field) ->
    case Parent of
        {some, P} ->
            case mochi_wisp_ffi:get_field_safe(P, Field) of
                {some, Value} ->
                    {ok, Value};

                none ->
                    {error, <<"Field not found: "/utf8, Field/binary>>}
            end;

        none ->
            {error, <<"No parent value"/utf8>>}
    end.

-file("src/mochi_wisp/schema.gleam", 96).
-spec id_field() -> mochi@schema:field_definition().
id_field() ->
    _pipe = mochi@schema:field_def(
        <<"id"/utf8>>,
        mochi@schema:non_null(mochi@schema:id_type())
    ),
    mochi@schema:resolver(
        _pipe,
        fun(Info) -> extract_field(erlang:element(2, Info), <<"id"/utf8>>) end
    ).

-file("src/mochi_wisp/schema.gleam", 103).
-spec name_field() -> mochi@schema:field_definition().
name_field() ->
    _pipe = mochi@schema:field_def(
        <<"name"/utf8>>,
        mochi@schema:non_null(mochi@schema:string_type())
    ),
    mochi@schema:resolver(
        _pipe,
        fun(Info) -> extract_field(erlang:element(2, Info), <<"name"/utf8>>) end
    ).

-file("src/mochi_wisp/schema.gleam", 110).
-spec email_field() -> mochi@schema:field_definition().
email_field() ->
    _pipe = mochi@schema:field_def(
        <<"email"/utf8>>,
        mochi@schema:non_null(mochi@schema:string_type())
    ),
    mochi@schema:resolver(
        _pipe,
        fun(Info) ->
            extract_field(erlang:element(2, Info), <<"email"/utf8>>)
        end
    ).

-file("src/mochi_wisp/schema.gleam", 117).
-spec role_field() -> mochi@schema:field_definition().
role_field() ->
    _pipe = mochi@schema:field_def(
        <<"role"/utf8>>,
        mochi@schema:non_null({named, <<"Role"/utf8>>})
    ),
    mochi@schema:resolver(
        _pipe,
        fun(Info) -> extract_field(erlang:element(2, Info), <<"role"/utf8>>) end
    ).

-file("src/mochi_wisp/schema.gleam", 87).
-spec user_type() -> mochi@schema:object_type().
user_type() ->
    _pipe = mochi@schema:object(<<"User"/utf8>>),
    _pipe@1 = mochi@schema:description(_pipe, <<"A user in the system"/utf8>>),
    _pipe@2 = mochi@schema:field(_pipe@1, id_field()),
    _pipe@3 = mochi@schema:field(_pipe@2, name_field()),
    _pipe@4 = mochi@schema:field(_pipe@3, email_field()),
    mochi@schema:field(_pipe@4, role_field()).

-file("src/mochi_wisp/schema.gleam", 150).
-spec user_to_dynamic(user()) -> gleam@dynamic:dynamic_().
user_to_dynamic(User) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(erlang:element(2, User))},
                {<<"name"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, User))},
                {<<"email"/utf8>>,
                    gleam_stdlib:identity(erlang:element(4, User))},
                {<<"role"/utf8>>,
                    gleam_stdlib:identity(
                        role_to_string(erlang:element(5, User))
                    )}]
        )
    ).

-file("src/mochi_wisp/schema.gleam", 161).
-spec users_encoder(list(user())) -> gleam@dynamic:dynamic_().
users_encoder(Users) ->
    gleam_stdlib:identity(gleam@list:map(Users, fun user_to_dynamic/1)).

-file("src/mochi_wisp/schema.gleam", 165).
-spec user_encoder(user()) -> gleam@dynamic:dynamic_().
user_encoder(User) ->
    user_to_dynamic(User).

-file("src/mochi_wisp/schema.gleam", 173).
-spec users_resolver(mochi@schema:execution_context()) -> {ok, list(user())} |
    {error, binary()}.
users_resolver(_) ->
    {ok, sample_users()}.

-file("src/mochi_wisp/schema.gleam", 179).
-spec user_resolver(user_by_id_args(), mochi@schema:execution_context()) -> {ok,
        user()} |
    {error, binary()}.
user_resolver(Args, _) ->
    find_user_by_id(erlang:element(2, Args)).

-file("src/mochi_wisp/schema.gleam", 194).
-spec decode_user_by_id_args(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> {ok, user_by_id_args()} | {error, binary()}.
decode_user_by_id_args(Args) ->
    case gleam_stdlib:map_get(Args, <<"id"/utf8>>) of
        {ok, Id_dyn} ->
            case gleam@dynamic@decode:run(
                Id_dyn,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, Id} ->
                    {ok, {user_by_id_args, Id}};

                {error, _} ->
                    {error, <<"Invalid id argument: expected string"/utf8>>}
            end;

        {error, _} ->
            {error, <<"Missing required argument: id"/utf8>>}
    end.

-file("src/mochi_wisp/schema.gleam", 212).
-spec build_schema() -> mochi@schema:schema().
build_schema() ->
    User_t = user_type(),
    Users_query = begin
        _pipe = mochi@query:'query'(
            <<"users"/utf8>>,
            {non_null, {list, {named, <<"User"/utf8>>}}},
            fun users_resolver/1,
            fun users_encoder/1
        ),
        mochi@query:query_description(_pipe, <<"Get all users"/utf8>>)
    end,
    User_query = begin
        _pipe@1 = mochi@query:query_with_args(
            <<"user"/utf8>>,
            [mochi@query:arg_with_desc(
                    <<"id"/utf8>>,
                    {non_null, {named, <<"ID"/utf8>>}},
                    <<"The user ID"/utf8>>
                )],
            {named, <<"User"/utf8>>},
            fun decode_user_by_id_args/1,
            fun user_resolver/2,
            fun user_encoder/1
        ),
        mochi@query:query_description(_pipe@1, <<"Get a user by ID"/utf8>>)
    end,
    _pipe@2 = mochi@query:new(),
    _pipe@3 = mochi@query:add_query(_pipe@2, Users_query),
    _pipe@4 = mochi@query:add_query(_pipe@3, User_query),
    _pipe@5 = mochi@query:add_type(_pipe@4, User_t),
    _pipe@6 = mochi@query:add_enum(_pipe@5, role_enum()),
    mochi@query:build(_pipe@6).
