-module(mochi_wisp@complex_schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi_wisp/complex_schema.gleam").
-export([sample_users/0, sample_posts/0, sample_comments/0, find_user_by_id/1, find_post_by_id/1, find_comment_by_id/1, find_posts_by_author/1, find_comments_by_post/1, find_comments_by_author/1, find_replies/1, role_to_string/1, status_to_string/1, user_to_dynamic/1, post_to_dynamic/1, comment_to_dynamic/1, role_enum/0, post_status_enum/0, user_type/0, post_type/0, comment_type/0, users_resolver/1, posts_resolver/1, published_posts_resolver/1, decode_id_args/1, user_by_id_resolver/2, post_by_id_resolver/2, build_complex_schema/0]).
-export_type([user/0, role/0, post/0, post_status/0, comment/0, page_info/0, post_connection/0, post_edge/0, create_user_input/0, update_user_input/0, create_post_input/0, post_filter_input/0, user_by_id_args/0]).

-type user() :: {user,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        role(),
        binary(),
        binary()}.

-type role() :: admin | moderator | member | guest.

-type post() :: {post,
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary(),
        post_status(),
        list(binary()),
        integer(),
        binary(),
        binary()}.

-type post_status() :: draft | published | archived.

-type comment() :: {comment,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary()}.

-type page_info() :: {page_info,
        boolean(),
        boolean(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type post_connection() :: {post_connection,
        list(post_edge()),
        page_info(),
        integer()}.

-type post_edge() :: {post_edge, post(), binary()}.

-type create_user_input() :: {create_user_input,
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(role())}.

-type update_user_input() :: {update_user_input,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(role())}.

-type create_post_input() :: {create_post_input,
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(list(binary())),
        gleam@option:option(post_status())}.

-type post_filter_input() :: {post_filter_input,
        gleam@option:option(post_status()),
        gleam@option:option(binary()),
        gleam@option:option(list(binary())),
        gleam@option:option(binary())}.

-type user_by_id_args() :: {user_by_id_args, binary()}.

-file("src/mochi_wisp/complex_schema.gleam", 129).
-spec sample_users() -> list(user()).
sample_users() ->
    [{user,
            <<"user-1"/utf8>>,
            <<"alice"/utf8>>,
            <<"alice@example.com"/utf8>>,
            <<"Alice Johnson"/utf8>>,
            {some, <<"Software engineer passionate about Gleam"/utf8>>},
            admin,
            <<"2024-01-15T10:30:00Z"/utf8>>,
            <<"2024-03-20T14:45:00Z"/utf8>>},
        {user,
            <<"user-2"/utf8>>,
            <<"bob"/utf8>>,
            <<"bob@example.com"/utf8>>,
            <<"Bob Smith"/utf8>>,
            none,
            moderator,
            <<"2024-02-01T08:00:00Z"/utf8>>,
            <<"2024-02-01T08:00:00Z"/utf8>>},
        {user,
            <<"user-3"/utf8>>,
            <<"charlie"/utf8>>,
            <<"charlie@example.com"/utf8>>,
            <<"Charlie Brown"/utf8>>,
            {some, <<"GraphQL enthusiast"/utf8>>},
            member,
            <<"2024-02-15T12:00:00Z"/utf8>>,
            <<"2024-03-01T09:30:00Z"/utf8>>},
        {user,
            <<"user-4"/utf8>>,
            <<"diana"/utf8>>,
            <<"diana@example.com"/utf8>>,
            <<"Diana Prince"/utf8>>,
            {some, <<"Full-stack developer"/utf8>>},
            member,
            <<"2024-03-01T16:20:00Z"/utf8>>,
            <<"2024-03-15T11:00:00Z"/utf8>>},
        {user,
            <<"user-5"/utf8>>,
            <<"guest"/utf8>>,
            <<"guest@example.com"/utf8>>,
            <<"Guest User"/utf8>>,
            none,
            guest,
            <<"2024-03-10T00:00:00Z"/utf8>>,
            <<"2024-03-10T00:00:00Z"/utf8>>}].

-file("src/mochi_wisp/complex_schema.gleam", 184).
-spec sample_posts() -> list(post()).
sample_posts() ->
    [{post,
            <<"post-1"/utf8>>,
            <<"Getting Started with Gleam"/utf8>>,
            <<"Gleam is a friendly language for building type-safe systems..."/utf8>>,
            {some, <<"An introduction to the Gleam programming language"/utf8>>},
            <<"user-1"/utf8>>,
            published,
            [<<"gleam"/utf8>>, <<"tutorial"/utf8>>, <<"beginners"/utf8>>],
            1542,
            <<"2024-02-01T10:00:00Z"/utf8>>,
            <<"2024-02-15T14:30:00Z"/utf8>>},
        {post,
            <<"post-2"/utf8>>,
            <<"Building GraphQL APIs with Mochi"/utf8>>,
            <<"Mochi is a code-first GraphQL library for Gleam..."/utf8>>,
            {some, <<"Learn how to build GraphQL APIs using Mochi"/utf8>>},
            <<"user-1"/utf8>>,
            published,
            [<<"gleam"/utf8>>,
                <<"graphql"/utf8>>,
                <<"mochi"/utf8>>,
                <<"api"/utf8>>],
            892,
            <<"2024-02-20T08:00:00Z"/utf8>>,
            <<"2024-03-01T16:00:00Z"/utf8>>},
        {post,
            <<"post-3"/utf8>>,
            <<"Advanced Type Safety Patterns"/utf8>>,
            <<"Exploring advanced patterns for type-safe programming..."/utf8>>,
            none,
            <<"user-3"/utf8>>,
            draft,
            [<<"types"/utf8>>, <<"patterns"/utf8>>, <<"advanced"/utf8>>],
            0,
            <<"2024-03-15T11:00:00Z"/utf8>>,
            <<"2024-03-15T11:00:00Z"/utf8>>},
        {post,
            <<"post-4"/utf8>>,
            <<"Performance Optimization Techniques"/utf8>>,
            <<"How to optimize your Gleam applications for performance..."/utf8>>,
            {some, <<"Tips and tricks for high-performance Gleam code"/utf8>>},
            <<"user-2"/utf8>>,
            published,
            [<<"performance"/utf8>>, <<"optimization"/utf8>>, <<"gleam"/utf8>>],
            456,
            <<"2024-03-10T09:00:00Z"/utf8>>,
            <<"2024-03-12T15:20:00Z"/utf8>>},
        {post,
            <<"post-5"/utf8>>,
            <<"Archived Post Example"/utf8>>,
            <<"This post has been archived..."/utf8>>,
            none,
            <<"user-1"/utf8>>,
            archived,
            [<<"archive"/utf8>>],
            123,
            <<"2024-01-01T00:00:00Z"/utf8>>,
            <<"2024-02-01T00:00:00Z"/utf8>>}].

-file("src/mochi_wisp/complex_schema.gleam", 249).
-spec sample_comments() -> list(comment()).
sample_comments() ->
    [{comment,
            <<"comment-1"/utf8>>,
            <<"Great article! Very helpful for beginners."/utf8>>,
            <<"user-2"/utf8>>,
            <<"post-1"/utf8>>,
            none,
            <<"2024-02-02T14:30:00Z"/utf8>>},
        {comment,
            <<"comment-2"/utf8>>,
            <<"Thanks for the feedback!"/utf8>>,
            <<"user-1"/utf8>>,
            <<"post-1"/utf8>>,
            {some, <<"comment-1"/utf8>>},
            <<"2024-02-02T15:00:00Z"/utf8>>},
        {comment,
            <<"comment-3"/utf8>>,
            <<"Could you elaborate on the error handling section?"/utf8>>,
            <<"user-3"/utf8>>,
            <<"post-1"/utf8>>,
            none,
            <<"2024-02-03T09:00:00Z"/utf8>>},
        {comment,
            <<"comment-4"/utf8>>,
            <<"Mochi looks amazing! Can't wait to try it."/utf8>>,
            <<"user-4"/utf8>>,
            <<"post-2"/utf8>>,
            none,
            <<"2024-02-21T10:00:00Z"/utf8>>},
        {comment,
            <<"comment-5"/utf8>>,
            <<"I've been using it in production, works great!"/utf8>>,
            <<"user-2"/utf8>>,
            <<"post-2"/utf8>>,
            {some, <<"comment-4"/utf8>>},
            <<"2024-02-22T08:30:00Z"/utf8>>}].

-file("src/mochi_wisp/complex_schema.gleam", 298).
-spec find_user_by_id(binary()) -> gleam@option:option(user()).
find_user_by_id(Id) ->
    _pipe = gleam@list:find(
        sample_users(),
        fun(U) -> erlang:element(2, U) =:= Id end
    ),
    gleam@option:from_result(_pipe).

-file("src/mochi_wisp/complex_schema.gleam", 303).
-spec find_post_by_id(binary()) -> gleam@option:option(post()).
find_post_by_id(Id) ->
    _pipe = gleam@list:find(
        sample_posts(),
        fun(P) -> erlang:element(2, P) =:= Id end
    ),
    gleam@option:from_result(_pipe).

-file("src/mochi_wisp/complex_schema.gleam", 308).
-spec find_comment_by_id(binary()) -> gleam@option:option(comment()).
find_comment_by_id(Id) ->
    _pipe = gleam@list:find(
        sample_comments(),
        fun(C) -> erlang:element(2, C) =:= Id end
    ),
    gleam@option:from_result(_pipe).

-file("src/mochi_wisp/complex_schema.gleam", 313).
-spec find_posts_by_author(binary()) -> list(post()).
find_posts_by_author(Author_id) ->
    gleam@list:filter(
        sample_posts(),
        fun(P) -> erlang:element(6, P) =:= Author_id end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 317).
-spec find_comments_by_post(binary()) -> list(comment()).
find_comments_by_post(Post_id) ->
    gleam@list:filter(
        sample_comments(),
        fun(C) -> erlang:element(5, C) =:= Post_id end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 321).
-spec find_comments_by_author(binary()) -> list(comment()).
find_comments_by_author(Author_id) ->
    gleam@list:filter(
        sample_comments(),
        fun(C) -> erlang:element(4, C) =:= Author_id end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 325).
-spec find_replies(binary()) -> list(comment()).
find_replies(Parent_id) ->
    gleam@list:filter(
        sample_comments(),
        fun(C) -> erlang:element(6, C) =:= {some, Parent_id} end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 333).
-spec role_to_string(role()) -> binary().
role_to_string(Role) ->
    case Role of
        admin ->
            <<"ADMIN"/utf8>>;

        moderator ->
            <<"MODERATOR"/utf8>>;

        member ->
            <<"MEMBER"/utf8>>;

        guest ->
            <<"GUEST"/utf8>>
    end.

-file("src/mochi_wisp/complex_schema.gleam", 342).
-spec status_to_string(post_status()) -> binary().
status_to_string(Status) ->
    case Status of
        draft ->
            <<"DRAFT"/utf8>>;

        published ->
            <<"PUBLISHED"/utf8>>;

        archived ->
            <<"ARCHIVED"/utf8>>
    end.

-file("src/mochi_wisp/complex_schema.gleam", 399).
-spec option_to_dynamic(
    gleam@option:option(BPS),
    fun((BPS) -> gleam@dynamic:dynamic_())
) -> gleam@dynamic:dynamic_().
option_to_dynamic(Opt, Encoder) ->
    case Opt of
        {some, V} ->
            Encoder(V);

        none ->
            gleam_stdlib:identity(nil)
    end.

-file("src/mochi_wisp/complex_schema.gleam", 354).
-spec user_to_dynamic(user()) -> gleam@dynamic:dynamic_().
user_to_dynamic(User) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(erlang:element(2, User))},
                {<<"username"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, User))},
                {<<"email"/utf8>>,
                    gleam_stdlib:identity(erlang:element(4, User))},
                {<<"displayName"/utf8>>,
                    gleam_stdlib:identity(erlang:element(5, User))},
                {<<"bio"/utf8>>,
                    option_to_dynamic(
                        erlang:element(6, User),
                        fun gleam_stdlib:identity/1
                    )},
                {<<"role"/utf8>>,
                    gleam_stdlib:identity(
                        role_to_string(erlang:element(7, User))
                    )},
                {<<"createdAt"/utf8>>,
                    gleam_stdlib:identity(erlang:element(8, User))},
                {<<"updatedAt"/utf8>>,
                    gleam_stdlib:identity(erlang:element(9, User))}]
        )
    ).

-file("src/mochi_wisp/complex_schema.gleam", 369).
-spec post_to_dynamic(post()) -> gleam@dynamic:dynamic_().
post_to_dynamic(Post) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(erlang:element(2, Post))},
                {<<"title"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, Post))},
                {<<"content"/utf8>>,
                    gleam_stdlib:identity(erlang:element(4, Post))},
                {<<"excerpt"/utf8>>,
                    option_to_dynamic(
                        erlang:element(5, Post),
                        fun gleam_stdlib:identity/1
                    )},
                {<<"authorId"/utf8>>,
                    gleam_stdlib:identity(erlang:element(6, Post))},
                {<<"status"/utf8>>,
                    gleam_stdlib:identity(
                        status_to_string(erlang:element(7, Post))
                    )},
                {<<"tags"/utf8>>,
                    gleam_stdlib:identity(erlang:element(8, Post))},
                {<<"viewCount"/utf8>>,
                    gleam_stdlib:identity(erlang:element(9, Post))},
                {<<"createdAt"/utf8>>,
                    gleam_stdlib:identity(erlang:element(10, Post))},
                {<<"updatedAt"/utf8>>,
                    gleam_stdlib:identity(erlang:element(11, Post))}]
        )
    ).

-file("src/mochi_wisp/complex_schema.gleam", 386).
-spec comment_to_dynamic(comment()) -> gleam@dynamic:dynamic_().
comment_to_dynamic(Comment) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"id"/utf8>>, gleam_stdlib:identity(erlang:element(2, Comment))},
                {<<"content"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, Comment))},
                {<<"authorId"/utf8>>,
                    gleam_stdlib:identity(erlang:element(4, Comment))},
                {<<"postId"/utf8>>,
                    gleam_stdlib:identity(erlang:element(5, Comment))},
                {<<"parentId"/utf8>>,
                    option_to_dynamic(
                        erlang:element(6, Comment),
                        fun gleam_stdlib:identity/1
                    )},
                {<<"createdAt"/utf8>>,
                    gleam_stdlib:identity(erlang:element(7, Comment))}]
        )
    ).

-file("src/mochi_wisp/complex_schema.gleam", 410).
-spec role_enum() -> mochi@schema:enum_type().
role_enum() ->
    _pipe = mochi@types:enum_type(<<"Role"/utf8>>),
    _pipe@1 = mochi@types:enum_description(
        _pipe,
        <<"User role determining access levels"/utf8>>
    ),
    _pipe@2 = mochi@types:value_with_desc(
        _pipe@1,
        <<"ADMIN"/utf8>>,
        <<"Full administrative access"/utf8>>
    ),
    _pipe@3 = mochi@types:value_with_desc(
        _pipe@2,
        <<"MODERATOR"/utf8>>,
        <<"Content moderation access"/utf8>>
    ),
    _pipe@4 = mochi@types:value_with_desc(
        _pipe@3,
        <<"MEMBER"/utf8>>,
        <<"Standard member access"/utf8>>
    ),
    _pipe@5 = mochi@types:value_with_desc(
        _pipe@4,
        <<"GUEST"/utf8>>,
        <<"Limited guest access"/utf8>>
    ),
    mochi@types:build_enum(_pipe@5).

-file("src/mochi_wisp/complex_schema.gleam", 420).
-spec post_status_enum() -> mochi@schema:enum_type().
post_status_enum() ->
    _pipe = mochi@types:enum_type(<<"PostStatus"/utf8>>),
    _pipe@1 = mochi@types:enum_description(
        _pipe,
        <<"Publication status of a post"/utf8>>
    ),
    _pipe@2 = mochi@types:value_with_desc(
        _pipe@1,
        <<"DRAFT"/utf8>>,
        <<"Not yet published"/utf8>>
    ),
    _pipe@3 = mochi@types:value_with_desc(
        _pipe@2,
        <<"PUBLISHED"/utf8>>,
        <<"Publicly visible"/utf8>>
    ),
    _pipe@4 = mochi@types:value_with_desc(
        _pipe@3,
        <<"ARCHIVED"/utf8>>,
        <<"No longer active"/utf8>>
    ),
    mochi@types:build_enum(_pipe@4).

-file("src/mochi_wisp/complex_schema.gleam", 579).
-spec field_resolver(binary()) -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
field_resolver(Field_name) ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, Field_name) of
                    {some, Value} ->
                        {ok, Value};

                    none ->
                        {error, <<"Field not found: "/utf8, Field_name/binary>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 592).
-spec user_posts_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
user_posts_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"id"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, User_id} ->
                                Posts = find_posts_by_author(User_id),
                                {ok,
                                    gleam_stdlib:identity(
                                        gleam@list:map(
                                            Posts,
                                            fun post_to_dynamic/1
                                        )
                                    )};

                            {error, _} ->
                                {error, <<"Invalid user id"/utf8>>}
                        end;

                    none ->
                        {error, <<"User id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 612).
-spec user_comments_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
user_comments_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"id"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, User_id} ->
                                Comments = find_comments_by_author(User_id),
                                {ok,
                                    gleam_stdlib:identity(
                                        gleam@list:map(
                                            Comments,
                                            fun comment_to_dynamic/1
                                        )
                                    )};

                            {error, _} ->
                                {error, <<"Invalid user id"/utf8>>}
                        end;

                    none ->
                        {error, <<"User id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 429).
-spec user_type() -> mochi@schema:object_type().
user_type() ->
    _pipe = mochi@schema:object(<<"User"/utf8>>),
    _pipe@1 = mochi@schema:description(
        _pipe,
        <<"A user account in the system"/utf8>>
    ),
    _pipe@3 = mochi@schema:field(
        _pipe@1,
        begin
            _pipe@2 = mochi@schema:field_def(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            ),
            mochi@schema:resolver(_pipe@2, field_resolver(<<"id"/utf8>>))
        end
    ),
    _pipe@5 = mochi@schema:field(
        _pipe@3,
        begin
            _pipe@4 = mochi@schema:field_def(
                <<"username"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@4, field_resolver(<<"username"/utf8>>))
        end
    ),
    _pipe@7 = mochi@schema:field(
        _pipe@5,
        begin
            _pipe@6 = mochi@schema:field_def(
                <<"email"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@6, field_resolver(<<"email"/utf8>>))
        end
    ),
    _pipe@9 = mochi@schema:field(
        _pipe@7,
        begin
            _pipe@8 = mochi@schema:field_def(
                <<"displayName"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(
                _pipe@8,
                field_resolver(<<"displayName"/utf8>>)
            )
        end
    ),
    _pipe@11 = mochi@schema:field(
        _pipe@9,
        begin
            _pipe@10 = mochi@schema:field_def(
                <<"bio"/utf8>>,
                mochi@schema:string_type()
            ),
            mochi@schema:resolver(_pipe@10, field_resolver(<<"bio"/utf8>>))
        end
    ),
    _pipe@13 = mochi@schema:field(
        _pipe@11,
        begin
            _pipe@12 = mochi@schema:field_def(
                <<"role"/utf8>>,
                mochi@schema:non_null({named, <<"Role"/utf8>>})
            ),
            mochi@schema:resolver(_pipe@12, field_resolver(<<"role"/utf8>>))
        end
    ),
    _pipe@15 = mochi@schema:field(
        _pipe@13,
        begin
            _pipe@14 = mochi@schema:field_def(
                <<"createdAt"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(
                _pipe@14,
                field_resolver(<<"createdAt"/utf8>>)
            )
        end
    ),
    _pipe@17 = mochi@schema:field(
        _pipe@15,
        begin
            _pipe@16 = mochi@schema:field_def(
                <<"updatedAt"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(
                _pipe@16,
                field_resolver(<<"updatedAt"/utf8>>)
            )
        end
    ),
    _pipe@19 = mochi@schema:field(
        _pipe@17,
        begin
            _pipe@18 = mochi@schema:field_def(
                <<"posts"/utf8>>,
                mochi@schema:non_null(
                    {list, mochi@schema:non_null({named, <<"Post"/utf8>>})}
                )
            ),
            mochi@schema:resolver(_pipe@18, user_posts_resolver())
        end
    ),
    mochi@schema:field(
        _pipe@19,
        begin
            _pipe@20 = mochi@schema:field_def(
                <<"comments"/utf8>>,
                mochi@schema:non_null(
                    {list, mochi@schema:non_null({named, <<"Comment"/utf8>>})}
                )
            ),
            mochi@schema:resolver(_pipe@20, user_comments_resolver())
        end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 632).
-spec post_author_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
post_author_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"authorId"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, Author_id} ->
                                case find_user_by_id(Author_id) of
                                    {some, User} ->
                                        {ok, user_to_dynamic(User)};

                                    none ->
                                        {error,
                                            <<"Author not found: "/utf8,
                                                Author_id/binary>>}
                                end;

                            {error, _} ->
                                {error, <<"Invalid author id"/utf8>>}
                        end;

                    none ->
                        {error, <<"Author id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 653).
-spec post_comments_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
post_comments_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"id"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, Post_id} ->
                                Comments = find_comments_by_post(Post_id),
                                {ok,
                                    gleam_stdlib:identity(
                                        gleam@list:map(
                                            Comments,
                                            fun comment_to_dynamic/1
                                        )
                                    )};

                            {error, _} ->
                                {error, <<"Invalid post id"/utf8>>}
                        end;

                    none ->
                        {error, <<"Post id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 480).
-spec post_type() -> mochi@schema:object_type().
post_type() ->
    _pipe = mochi@schema:object(<<"Post"/utf8>>),
    _pipe@1 = mochi@schema:description(_pipe, <<"A blog post"/utf8>>),
    _pipe@3 = mochi@schema:field(
        _pipe@1,
        begin
            _pipe@2 = mochi@schema:field_def(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            ),
            mochi@schema:resolver(_pipe@2, field_resolver(<<"id"/utf8>>))
        end
    ),
    _pipe@5 = mochi@schema:field(
        _pipe@3,
        begin
            _pipe@4 = mochi@schema:field_def(
                <<"title"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@4, field_resolver(<<"title"/utf8>>))
        end
    ),
    _pipe@7 = mochi@schema:field(
        _pipe@5,
        begin
            _pipe@6 = mochi@schema:field_def(
                <<"content"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@6, field_resolver(<<"content"/utf8>>))
        end
    ),
    _pipe@9 = mochi@schema:field(
        _pipe@7,
        begin
            _pipe@8 = mochi@schema:field_def(
                <<"excerpt"/utf8>>,
                mochi@schema:string_type()
            ),
            mochi@schema:resolver(_pipe@8, field_resolver(<<"excerpt"/utf8>>))
        end
    ),
    _pipe@11 = mochi@schema:field(
        _pipe@9,
        begin
            _pipe@10 = mochi@schema:field_def(
                <<"status"/utf8>>,
                mochi@schema:non_null({named, <<"PostStatus"/utf8>>})
            ),
            mochi@schema:resolver(_pipe@10, field_resolver(<<"status"/utf8>>))
        end
    ),
    _pipe@13 = mochi@schema:field(
        _pipe@11,
        begin
            _pipe@12 = mochi@schema:field_def(
                <<"tags"/utf8>>,
                mochi@schema:non_null(
                    {list, mochi@schema:non_null(mochi@schema:string_type())}
                )
            ),
            mochi@schema:resolver(_pipe@12, field_resolver(<<"tags"/utf8>>))
        end
    ),
    _pipe@15 = mochi@schema:field(
        _pipe@13,
        begin
            _pipe@14 = mochi@schema:field_def(
                <<"viewCount"/utf8>>,
                mochi@schema:non_null(mochi@schema:int_type())
            ),
            mochi@schema:resolver(
                _pipe@14,
                field_resolver(<<"viewCount"/utf8>>)
            )
        end
    ),
    _pipe@17 = mochi@schema:field(
        _pipe@15,
        begin
            _pipe@16 = mochi@schema:field_def(
                <<"createdAt"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(
                _pipe@16,
                field_resolver(<<"createdAt"/utf8>>)
            )
        end
    ),
    _pipe@19 = mochi@schema:field(
        _pipe@17,
        begin
            _pipe@18 = mochi@schema:field_def(
                <<"updatedAt"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(
                _pipe@18,
                field_resolver(<<"updatedAt"/utf8>>)
            )
        end
    ),
    _pipe@21 = mochi@schema:field(
        _pipe@19,
        begin
            _pipe@20 = mochi@schema:field_def(
                <<"author"/utf8>>,
                mochi@schema:non_null({named, <<"User"/utf8>>})
            ),
            mochi@schema:resolver(_pipe@20, post_author_resolver())
        end
    ),
    mochi@schema:field(
        _pipe@21,
        begin
            _pipe@22 = mochi@schema:field_def(
                <<"comments"/utf8>>,
                mochi@schema:non_null(
                    {list, mochi@schema:non_null({named, <<"Comment"/utf8>>})}
                )
            ),
            mochi@schema:resolver(_pipe@22, post_comments_resolver())
        end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 673).
-spec comment_author_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
comment_author_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"authorId"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, Author_id} ->
                                case find_user_by_id(Author_id) of
                                    {some, User} ->
                                        {ok, user_to_dynamic(User)};

                                    none ->
                                        {error,
                                            <<"Author not found: "/utf8,
                                                Author_id/binary>>}
                                end;

                            {error, _} ->
                                {error, <<"Invalid author id"/utf8>>}
                        end;

                    none ->
                        {error, <<"Author id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 694).
-spec comment_post_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
comment_post_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"postId"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, Post_id} ->
                                case find_post_by_id(Post_id) of
                                    {some, Post} ->
                                        {ok, post_to_dynamic(Post)};

                                    none ->
                                        {error,
                                            <<"Post not found: "/utf8,
                                                Post_id/binary>>}
                                end;

                            {error, _} ->
                                {error, <<"Invalid post id"/utf8>>}
                        end;

                    none ->
                        {error, <<"Post id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 715).
-spec comment_parent_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
comment_parent_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"parentId"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            gleam@dynamic@decode:optional(
                                {decoder,
                                    fun gleam@dynamic@decode:decode_string/1}
                            )
                        ) of
                            {ok, {some, Parent_id}} ->
                                case find_comment_by_id(Parent_id) of
                                    {some, Comment} ->
                                        {ok, comment_to_dynamic(Comment)};

                                    none ->
                                        {ok, gleam_stdlib:identity(nil)}
                                end;

                            {ok, none} ->
                                {ok, gleam_stdlib:identity(nil)};

                            {error, _} ->
                                {ok, gleam_stdlib:identity(nil)}
                        end;

                    none ->
                        {ok, gleam_stdlib:identity(nil)}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 737).
-spec comment_replies_resolver() -> fun((mochi@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
comment_replies_resolver() ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case mochi_wisp_ffi:get_field_safe(Parent, <<"id"/utf8>>) of
                    {some, Id_dyn} ->
                        case gleam@dynamic@decode:run(
                            Id_dyn,
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ) of
                            {ok, Comment_id} ->
                                Replies = find_replies(Comment_id),
                                {ok,
                                    gleam_stdlib:identity(
                                        gleam@list:map(
                                            Replies,
                                            fun comment_to_dynamic/1
                                        )
                                    )};

                            {error, _} ->
                                {error, <<"Invalid comment id"/utf8>>}
                        end;

                    none ->
                        {error, <<"Comment id not found"/utf8>>}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end.

-file("src/mochi_wisp/complex_schema.gleam", 535).
-spec comment_type() -> mochi@schema:object_type().
comment_type() ->
    _pipe = mochi@schema:object(<<"Comment"/utf8>>),
    _pipe@1 = mochi@schema:description(_pipe, <<"A comment on a post"/utf8>>),
    _pipe@3 = mochi@schema:field(
        _pipe@1,
        begin
            _pipe@2 = mochi@schema:field_def(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            ),
            mochi@schema:resolver(_pipe@2, field_resolver(<<"id"/utf8>>))
        end
    ),
    _pipe@5 = mochi@schema:field(
        _pipe@3,
        begin
            _pipe@4 = mochi@schema:field_def(
                <<"content"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@4, field_resolver(<<"content"/utf8>>))
        end
    ),
    _pipe@7 = mochi@schema:field(
        _pipe@5,
        begin
            _pipe@6 = mochi@schema:field_def(
                <<"createdAt"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@schema:resolver(_pipe@6, field_resolver(<<"createdAt"/utf8>>))
        end
    ),
    _pipe@9 = mochi@schema:field(
        _pipe@7,
        begin
            _pipe@8 = mochi@schema:field_def(
                <<"author"/utf8>>,
                mochi@schema:non_null({named, <<"User"/utf8>>})
            ),
            mochi@schema:resolver(_pipe@8, comment_author_resolver())
        end
    ),
    _pipe@11 = mochi@schema:field(
        _pipe@9,
        begin
            _pipe@10 = mochi@schema:field_def(
                <<"post"/utf8>>,
                mochi@schema:non_null({named, <<"Post"/utf8>>})
            ),
            mochi@schema:resolver(_pipe@10, comment_post_resolver())
        end
    ),
    _pipe@13 = mochi@schema:field(
        _pipe@11,
        begin
            _pipe@12 = mochi@schema:field_def(
                <<"parent"/utf8>>,
                {named, <<"Comment"/utf8>>}
            ),
            mochi@schema:resolver(_pipe@12, comment_parent_resolver())
        end
    ),
    mochi@schema:field(
        _pipe@13,
        begin
            _pipe@14 = mochi@schema:field_def(
                <<"replies"/utf8>>,
                mochi@schema:non_null(
                    {list, mochi@schema:non_null({named, <<"Comment"/utf8>>})}
                )
            ),
            mochi@schema:resolver(_pipe@14, comment_replies_resolver())
        end
    ).

-file("src/mochi_wisp/complex_schema.gleam", 761).
-spec users_resolver(mochi@schema:execution_context()) -> {ok, list(user())} |
    {error, binary()}.
users_resolver(_) ->
    {ok, sample_users()}.

-file("src/mochi_wisp/complex_schema.gleam", 765).
-spec posts_resolver(mochi@schema:execution_context()) -> {ok, list(post())} |
    {error, binary()}.
posts_resolver(_) ->
    {ok, sample_posts()}.

-file("src/mochi_wisp/complex_schema.gleam", 769).
-spec published_posts_resolver(mochi@schema:execution_context()) -> {ok,
        list(post())} |
    {error, binary()}.
published_posts_resolver(_) ->
    {ok,
        gleam@list:filter(
            sample_posts(),
            fun(P) -> erlang:element(7, P) =:= published end
        )}.

-file("src/mochi_wisp/complex_schema.gleam", 777).
-spec decode_id_args(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok,
        user_by_id_args()} |
    {error, binary()}.
decode_id_args(Args) ->
    case gleam_stdlib:map_get(Args, <<"id"/utf8>>) of
        {ok, Id_dyn} ->
            case gleam@dynamic@decode:run(
                Id_dyn,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, Id} ->
                    {ok, {user_by_id_args, Id}};

                {error, _} ->
                    {error, <<"Invalid id"/utf8>>}
            end;

        {error, _} ->
            {error, <<"Missing id"/utf8>>}
    end.

-file("src/mochi_wisp/complex_schema.gleam", 788).
-spec user_by_id_resolver(user_by_id_args(), mochi@schema:execution_context()) -> {ok,
        user()} |
    {error, binary()}.
user_by_id_resolver(Args, _) ->
    case find_user_by_id(erlang:element(2, Args)) of
        {some, User} ->
            {ok, User};

        none ->
            {error, <<"User not found"/utf8>>}
    end.

-file("src/mochi_wisp/complex_schema.gleam", 798).
-spec post_by_id_resolver(user_by_id_args(), mochi@schema:execution_context()) -> {ok,
        post()} |
    {error, binary()}.
post_by_id_resolver(Args, _) ->
    case find_post_by_id(erlang:element(2, Args)) of
        {some, Post} ->
            {ok, Post};

        none ->
            {error, <<"Post not found"/utf8>>}
    end.

-file("src/mochi_wisp/complex_schema.gleam", 812).
-spec build_complex_schema() -> mochi@schema:schema().
build_complex_schema() ->
    Users_q = begin
        _pipe = mochi@query:'query'(
            <<"users"/utf8>>,
            mochi@schema:non_null(
                {list, mochi@schema:non_null({named, <<"User"/utf8>>})}
            ),
            fun users_resolver/1,
            fun(Users) ->
                gleam_stdlib:identity(
                    gleam@list:map(Users, fun user_to_dynamic/1)
                )
            end
        ),
        mochi@query:query_description(_pipe, <<"Get all users"/utf8>>)
    end,
    User_q = begin
        _pipe@1 = mochi@query:query_with_args(
            <<"user"/utf8>>,
            [mochi@query:arg(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                )],
            {named, <<"User"/utf8>>},
            fun decode_id_args/1,
            fun user_by_id_resolver/2,
            fun user_to_dynamic/1
        ),
        mochi@query:query_description(_pipe@1, <<"Get user by ID"/utf8>>)
    end,
    Posts_q = begin
        _pipe@2 = mochi@query:'query'(
            <<"posts"/utf8>>,
            mochi@schema:non_null(
                {list, mochi@schema:non_null({named, <<"Post"/utf8>>})}
            ),
            fun posts_resolver/1,
            fun(Posts) ->
                gleam_stdlib:identity(
                    gleam@list:map(Posts, fun post_to_dynamic/1)
                )
            end
        ),
        mochi@query:query_description(_pipe@2, <<"Get all posts"/utf8>>)
    end,
    Post_q = begin
        _pipe@3 = mochi@query:query_with_args(
            <<"post"/utf8>>,
            [mochi@query:arg(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                )],
            {named, <<"Post"/utf8>>},
            fun decode_id_args/1,
            fun post_by_id_resolver/2,
            fun post_to_dynamic/1
        ),
        mochi@query:query_description(_pipe@3, <<"Get post by ID"/utf8>>)
    end,
    Published_posts_q = begin
        _pipe@4 = mochi@query:'query'(
            <<"publishedPosts"/utf8>>,
            mochi@schema:non_null(
                {list, mochi@schema:non_null({named, <<"Post"/utf8>>})}
            ),
            fun published_posts_resolver/1,
            fun(Posts@1) ->
                gleam_stdlib:identity(
                    gleam@list:map(Posts@1, fun post_to_dynamic/1)
                )
            end
        ),
        mochi@query:query_description(
            _pipe@4,
            <<"Get all published posts"/utf8>>
        )
    end,
    _pipe@5 = mochi@query:new(),
    _pipe@6 = mochi@query:add_query(_pipe@5, Users_q),
    _pipe@7 = mochi@query:add_query(_pipe@6, User_q),
    _pipe@8 = mochi@query:add_query(_pipe@7, Posts_q),
    _pipe@9 = mochi@query:add_query(_pipe@8, Post_q),
    _pipe@10 = mochi@query:add_query(_pipe@9, Published_posts_q),
    _pipe@11 = mochi@query:add_type(_pipe@10, user_type()),
    _pipe@12 = mochi@query:add_type(_pipe@11, post_type()),
    _pipe@13 = mochi@query:add_type(_pipe@12, comment_type()),
    _pipe@14 = mochi@query:add_enum(_pipe@13, role_enum()),
    _pipe@15 = mochi@query:add_enum(_pipe@14, post_status_enum()),
    mochi@query:build(_pipe@15).
