-module(bookstore_example_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/bookstore_example_test.gleam").
-export([bookstore_list_books_test/0, bookstore_get_book_by_id_test/0, bookstore_list_authors_test/0, bookstore_get_author_by_id_test/0, bookstore_add_book_mutation_test/0, bookstore_add_review_mutation_test/0, bookstore_cross_domain_query_test/0]).
-export_type([book/0, author/0, review/0]).

-type book() :: {book, binary(), binary(), binary()}.

-type author() :: {author, binary(), binary()}.

-type review() :: {review, binary(), binary(), integer(), binary()}.

-file("test/bookstore_example_test.gleam", 27).
-spec all_books() -> list(book()).
all_books() ->
    [{book, <<"1"/utf8>>, <<"Norwegian Wood"/utf8>>, <<"1"/utf8>>},
        {book, <<"2"/utf8>>, <<"Kafka on the Shore"/utf8>>, <<"1"/utf8>>},
        {book, <<"3"/utf8>>, <<"Kitchen"/utf8>>, <<"2"/utf8>>}].

-file("test/bookstore_example_test.gleam", 35).
-spec all_authors() -> list(author()).
all_authors() ->
    [{author, <<"1"/utf8>>, <<"Haruki Murakami"/utf8>>},
        {author, <<"2"/utf8>>, <<"Banana Yoshimoto"/utf8>>}].

-file("test/bookstore_example_test.gleam", 41).
-spec book_schema() -> mochi@query:schema_builder().
book_schema() ->
    Book_type = begin
        _pipe = mochi@types:object(<<"Book"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(B) -> erlang:element(2, B) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"title"/utf8>>,
            fun(B@1) -> erlang:element(3, B@1) end
        ),
        mochi@types:build(
            _pipe@2,
            fun(_) ->
                {ok,
                    {book,
                        <<"1"/utf8>>,
                        <<"Norwegian Wood"/utf8>>,
                        <<"1"/utf8>>}}
            end
        )
    end,
    Books_query = mochi@query:'query'(
        <<"books"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Book"/utf8>>)),
        fun(_) -> {ok, all_books()} end,
        fun(Books) -> gleam_stdlib:identity(Books) end
    ),
    Book_query = mochi@query:query_with_args(
        <<"book"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"Book"/utf8>>),
        fun(Args) -> mochi@query:get_id(Args, <<"id"/utf8>>) end,
        fun(Id, _) ->
            case gleam@list:find(
                all_books(),
                fun(B@2) -> erlang:element(2, B@2) =:= Id end
            ) of
                {ok, B@3} ->
                    {ok, B@3};

                {error, _} ->
                    {error, <<"Not found"/utf8>>}
            end
        end,
        fun(B@4) -> gleam_stdlib:identity(B@4) end
    ),
    Add_book = mochi@query:mutation(
        <<"addBook"/utf8>>,
        [mochi@query:arg(
                <<"title"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            ),
            mochi@query:arg(
                <<"authorId"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:non_null(mochi@schema:named_type(<<"Book"/utf8>>)),
        fun(Args@1) ->
            case {mochi@query:get_string(Args@1, <<"title"/utf8>>),
                mochi@query:get_id(Args@1, <<"authorId"/utf8>>)} of
                {{ok, T}, {ok, A}} ->
                    {ok, {T, A}};

                {_, _} ->
                    {error, <<"Missing args"/utf8>>}
            end
        end,
        fun(Input, _) ->
            {Title, Author_id} = Input,
            {ok, {book, <<"new-1"/utf8>>, Title, Author_id}}
        end,
        fun(B@5) -> gleam_stdlib:identity(B@5) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Books_query),
    _pipe@5 = mochi@query:add_query(_pipe@4, Book_query),
    _pipe@6 = mochi@query:add_mutation(_pipe@5, Add_book),
    mochi@query:add_type(_pipe@6, Book_type).

-file("test/bookstore_example_test.gleam", 99).
-spec author_schema() -> mochi@query:schema_builder().
author_schema() ->
    Author_type = begin
        _pipe = mochi@types:object(<<"Author"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(A) -> erlang:element(2, A) end
        ),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(A@1) -> erlang:element(3, A@1) end
        ),
        mochi@types:build(
            _pipe@2,
            fun(_) ->
                {ok, {author, <<"1"/utf8>>, <<"Haruki Murakami"/utf8>>}}
            end
        )
    end,
    Authors_query = mochi@query:'query'(
        <<"authors"/utf8>>,
        mochi@schema:list_type(mochi@schema:named_type(<<"Author"/utf8>>)),
        fun(_) -> {ok, all_authors()} end,
        fun(A@2) -> gleam_stdlib:identity(A@2) end
    ),
    Author_query = mochi@query:query_with_args(
        <<"author"/utf8>>,
        [mochi@query:arg(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )],
        mochi@schema:named_type(<<"Author"/utf8>>),
        fun(Args) -> mochi@query:get_id(Args, <<"id"/utf8>>) end,
        fun(Id, _) ->
            case gleam@list:find(
                all_authors(),
                fun(A@3) -> erlang:element(2, A@3) =:= Id end
            ) of
                {ok, A@4} ->
                    {ok, A@4};

                {error, _} ->
                    {error, <<"Not found"/utf8>>}
            end
        end,
        fun(A@5) -> gleam_stdlib:identity(A@5) end
    ),
    _pipe@3 = mochi@query:new(),
    _pipe@4 = mochi@query:add_query(_pipe@3, Authors_query),
    _pipe@5 = mochi@query:add_query(_pipe@4, Author_query),
    mochi@query:add_type(_pipe@5, Author_type).

-file("test/bookstore_example_test.gleam", 135).
-spec review_schema() -> mochi@query:schema_builder().
review_schema() ->
    Review_type = begin
        _pipe = mochi@types:object(<<"Review"/utf8>>),
        _pipe@1 = mochi@types:id(
            _pipe,
            <<"id"/utf8>>,
            fun(R) -> erlang:element(2, R) end
        ),
        _pipe@2 = mochi@types:int(
            _pipe@1,
            <<"rating"/utf8>>,
            fun(R@1) -> erlang:element(4, R@1) end
        ),
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"comment"/utf8>>,
            fun(R@2) -> erlang:element(5, R@2) end
        ),
        mochi@types:build(
            _pipe@3,
            fun(_) ->
                {ok, {review, <<"1"/utf8>>, <<"1"/utf8>>, 5, <<"Great"/utf8>>}}
            end
        )
    end,
    Add_review = mochi@query:mutation(
        <<"addReview"/utf8>>,
        [mochi@query:arg(
                <<"bookId"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            ),
            mochi@query:arg(
                <<"rating"/utf8>>,
                mochi@schema:non_null(mochi@schema:int_type())
            ),
            mochi@query:arg(<<"comment"/utf8>>, mochi@schema:string_type())],
        mochi@schema:non_null(mochi@schema:named_type(<<"Review"/utf8>>)),
        fun(Args) ->
            case {mochi@query:get_id(Args, <<"bookId"/utf8>>),
                mochi@query:get_int(Args, <<"rating"/utf8>>)} of
                {{ok, Bid}, {ok, R@3}} ->
                    C = case mochi@query:get_optional_string(
                        Args,
                        <<"comment"/utf8>>
                    ) of
                        {some, V} ->
                            V;

                        none ->
                            <<""/utf8>>
                    end,
                    {ok, {Bid, R@3, C}};

                {_, _} ->
                    {error, <<"Missing args"/utf8>>}
            end
        end,
        fun(Input, _) ->
            {_, Rating, Comment} = Input,
            {ok, {review, <<"new-1"/utf8>>, <<"1"/utf8>>, Rating, Comment}}
        end,
        fun(R@4) -> gleam_stdlib:identity(R@4) end
    ),
    _pipe@4 = mochi@query:new(),
    _pipe@5 = mochi@query:add_mutation(_pipe@4, Add_review),
    mochi@query:add_type(_pipe@5, Review_type).

-file("test/bookstore_example_test.gleam", 176).
-spec create_schema() -> mochi@schema:schema().
create_schema() ->
    _pipe = book_schema(),
    _pipe@1 = mochi@query:merge(_pipe, author_schema()),
    _pipe@2 = mochi@query:merge(_pipe@1, review_schema()),
    mochi@query:build(_pipe@2).

-file("test/bookstore_example_test.gleam", 185).
-spec bookstore_list_books_test() -> nil.
bookstore_list_books_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ books { id title } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"books query should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_list_books_test"/utf8>>,
                    line => 191})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"books query should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_list_books_test"/utf8>>,
                    line => 195})
    end.

-file("test/bookstore_example_test.gleam", 199).
-spec bookstore_get_book_by_id_test() -> nil.
bookstore_get_book_by_id_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ book(id: \"2\") { id title } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"book(id) query should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_get_book_by_id_test"/utf8>>,
                    line => 206})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"book(id) query should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_get_book_by_id_test"/utf8>>,
                    line => 210})
    end.

-file("test/bookstore_example_test.gleam", 214).
-spec bookstore_list_authors_test() -> nil.
bookstore_list_authors_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ authors { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"authors query should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_list_authors_test"/utf8>>,
                    line => 220})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"authors query should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_list_authors_test"/utf8>>,
                    line => 224})
    end.

-file("test/bookstore_example_test.gleam", 228).
-spec bookstore_get_author_by_id_test() -> nil.
bookstore_get_author_by_id_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ author(id: \"1\") { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"author(id) query should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_get_author_by_id_test"/utf8>>,
                    line => 235})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"author(id) query should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_get_author_by_id_test"/utf8>>,
                    line => 239})
    end.

-file("test/bookstore_example_test.gleam", 243).
-spec bookstore_add_book_mutation_test() -> nil.
bookstore_add_book_mutation_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"mutation { addBook(title: \"1Q84\", authorId: \"1\") { id title } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"addBook mutation should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_add_book_mutation_test"/utf8>>,
                    line => 253})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"addBook mutation should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_add_book_mutation_test"/utf8>>,
                    line => 257})
    end.

-file("test/bookstore_example_test.gleam", 261).
-spec bookstore_add_review_mutation_test() -> nil.
bookstore_add_review_mutation_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"mutation { addReview(bookId: \"1\", rating: 5, comment: \"Great!\") { id rating comment } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"addReview mutation should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_add_review_mutation_test"/utf8>>,
                    line => 271})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"addReview mutation should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_add_review_mutation_test"/utf8>>,
                    line => 275})
    end.

-file("test/bookstore_example_test.gleam", 279).
-spec bookstore_cross_domain_query_test() -> nil.
bookstore_cross_domain_query_test() ->
    Schema = create_schema(),
    Result = mochi@executor:execute_query(
        Schema,
        <<"{ books { id title } authors { id name } }"/utf8>>
    ),
    case erlang:element(3, Result) of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Cross-domain query should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_cross_domain_query_test"/utf8>>,
                    line => 288})
    end,
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Cross-domain query should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"bookstore_example_test"/utf8>>,
                    function => <<"bookstore_cross_domain_query_test"/utf8>>,
                    line => 292})
    end.
