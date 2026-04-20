-module(mochi_snapshot_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/mochi_snapshot_test.gleam").
-export([main/0, basic_sdl_parsing_test/0, enum_type_parsing_test/0, union_type_parsing_test/0, input_type_parsing_test/0, complex_schema_parsing_test/0, user_schema_structure_test/0, field_type_variations_test/0, sdl_parsing_error_test/0, malformed_union_error_test/0, json_object_encoding_test/0, json_nested_encoding_test/0, error_with_extensions_test/0, error_with_path_and_location_test/0, response_success_test/0, response_with_errors_test/0]).

-file("test/mochi_snapshot_test.gleam", 26).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/mochi_snapshot_test.gleam", 35).
-spec basic_sdl_parsing_test() -> nil.
basic_sdl_parsing_test() ->
    Sdl = <<"
    scalar DateTime

    type User {
      id: ID!
      name: String
      email: String!
    }"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(
        _pipe@1,
        <<"Basic SDL parsing with scalar and object types"/utf8>>
    ).

-file("test/mochi_snapshot_test.gleam", 52).
-spec enum_type_parsing_test() -> nil.
enum_type_parsing_test() ->
    Sdl = <<"
    enum UserRole {
      ADMIN
      USER
      MODERATOR
    }"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"Enum type SDL parsing"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 67).
-spec union_type_parsing_test() -> nil.
union_type_parsing_test() ->
    Sdl = <<"
    union SearchResult = User | Post | Comment"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"Union type SDL parsing"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 78).
-spec input_type_parsing_test() -> nil.
input_type_parsing_test() ->
    Sdl = <<"
    input CreateUserInput {
      name: String!
      email: String!
      age: Int
    }"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"Input type SDL parsing"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 93).
-spec complex_schema_parsing_test() -> nil.
complex_schema_parsing_test() ->
    Sdl = <<"
    type Query {
      user: User
      posts: [Post!]!
    }

    type User {
      id: ID!
      name: String!
      posts: [Post!]!
    }

    type Post {
      id: ID!
      title: String!
      author: User!
    }"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"Complex schema with relationships"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 123).
-spec user_schema_structure_test() -> nil.
user_schema_structure_test() ->
    User_type = begin
        _pipe = mochi@schema:object(<<"User"/utf8>>),
        _pipe@1 = mochi@schema:description(
            _pipe,
            <<"A user in the system"/utf8>>
        ),
        _pipe@3 = mochi@schema:field(
            _pipe@1,
            begin
                _pipe@2 = mochi@schema:field_def(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                ),
                mochi@schema:field_description(
                    _pipe@2,
                    <<"Unique identifier"/utf8>>
                )
            end
        ),
        _pipe@5 = mochi@schema:field(
            _pipe@3,
            begin
                _pipe@4 = mochi@schema:field_def(
                    <<"name"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:field_description(
                    _pipe@4,
                    <<"User's full name"/utf8>>
                )
            end
        ),
        mochi@schema:field(
            _pipe@5,
            begin
                _pipe@6 = mochi@schema:field_def(
                    <<"email"/utf8>>,
                    mochi@schema:non_null(mochi@schema:string_type())
                ),
                mochi@schema:field_description(
                    _pipe@6,
                    <<"User's email address"/utf8>>
                )
            end
        )
    end,
    Query_type = begin
        _pipe@7 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@7,
            begin
                _pipe@8 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                mochi@schema:argument(
                    _pipe@8,
                    begin
                        _pipe@9 = mochi@schema:arg(
                            <<"id"/utf8>>,
                            mochi@schema:non_null(mochi@schema:id_type())
                        ),
                        mochi@schema:arg_description(
                            _pipe@9,
                            <<"User ID"/utf8>>
                        )
                    end
                )
            end
        )
    end,
    Complete_schema = begin
        _pipe@10 = mochi@schema:schema(),
        _pipe@11 = mochi@schema:'query'(_pipe@10, Query_type),
        mochi@schema:add_type(_pipe@11, {object_type_def, User_type})
    end,
    _pipe@12 = Complete_schema,
    _pipe@13 = gleam@string:inspect(_pipe@12),
    birdie:snap(_pipe@13, <<"Complete User schema structure"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 161).
-spec field_type_variations_test() -> nil.
field_type_variations_test() ->
    Test_type = begin
        _pipe = mochi@schema:object(<<"TestType"/utf8>>),
        _pipe@1 = mochi@schema:field(
            _pipe,
            mochi@schema:field_def(
                <<"nullable_string"/utf8>>,
                mochi@schema:string_type()
            )
        ),
        _pipe@2 = mochi@schema:field(
            _pipe@1,
            mochi@schema:field_def(
                <<"non_null_string"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )
        ),
        _pipe@3 = mochi@schema:field(
            _pipe@2,
            mochi@schema:field_def(
                <<"list_of_strings"/utf8>>,
                mochi@schema:list_type(mochi@schema:string_type())
            )
        ),
        mochi@schema:field(
            _pipe@3,
            mochi@schema:field_def(
                <<"non_null_list_of_non_null_strings"/utf8>>,
                mochi@schema:non_null(
                    mochi@schema:list_type(
                        mochi@schema:non_null(mochi@schema:string_type())
                    )
                )
            )
        )
    end,
    _pipe@4 = Test_type,
    _pipe@5 = gleam@string:inspect(_pipe@4),
    birdie:snap(
        _pipe@5,
        <<"Field type variations (nullable, non-null, lists)"/utf8>>
    ).

-file("test/mochi_snapshot_test.gleam", 188).
-spec sdl_parsing_error_test() -> nil.
sdl_parsing_error_test() ->
    Invalid_sdl = <<"
    type User {
      id: ID!
      name: String
      # Missing closing brace"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Invalid_sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"SDL parsing error handling"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 202).
-spec malformed_union_error_test() -> nil.
malformed_union_error_test() ->
    Invalid_sdl = <<"union SearchResult = | Post"/utf8>>,
    _pipe = mochi@sdl_parser:parse_sdl(Invalid_sdl),
    _pipe@1 = gleam@string:inspect(_pipe),
    birdie:snap(_pipe@1, <<"Malformed union type error"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 215).
-spec json_object_encoding_test() -> nil.
json_object_encoding_test() ->
    Data = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"Alice"/utf8>>)},
            {<<"age"/utf8>>, gleam_stdlib:identity(30)},
            {<<"active"/utf8>>, gleam_stdlib:identity(true)}]
    ),
    _pipe = Data,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = mochi@json:encode_pretty(_pipe@1, 2),
    birdie:snap(_pipe@2, <<"JSON object encoding with mixed types"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 230).
-spec json_nested_encoding_test() -> nil.
json_nested_encoding_test() ->
    Inner = maps:from_list(
        [{<<"city"/utf8>>, gleam_stdlib:identity(<<"Tokyo"/utf8>>)},
            {<<"country"/utf8>>, gleam_stdlib:identity(<<"Japan"/utf8>>)}]
    ),
    Data = maps:from_list(
        [{<<"user"/utf8>>, gleam_stdlib:identity(<<"Bob"/utf8>>)},
            {<<"address"/utf8>>, gleam_stdlib:identity(Inner)},
            {<<"tags"/utf8>>,
                gleam_stdlib:identity(
                    [<<"dev"/utf8>>, <<"gleam"/utf8>>, <<"graphql"/utf8>>]
                )}]
    ),
    _pipe = Data,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = mochi@json:encode_pretty(_pipe@1, 2),
    birdie:snap(_pipe@2, <<"JSON nested object and array encoding"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 255).
-spec error_with_extensions_test() -> nil.
error_with_extensions_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Authentication required"/utf8>>),
        _pipe@1 = mochi@error:with_code(_pipe, <<"UNAUTHENTICATED"/utf8>>),
        _pipe@2 = mochi@error:with_category(
            _pipe@1,
            authentication_error_category
        ),
        mochi@error:with_extension(
            _pipe@2,
            <<"retryAfter"/utf8>>,
            gleam_stdlib:identity(60)
        )
    end,
    _pipe@3 = Err,
    _pipe@4 = mochi@error:to_dynamic(_pipe@3),
    _pipe@5 = mochi@json:encode_pretty(_pipe@4, 2),
    birdie:snap(_pipe@5, <<"GraphQL error with extensions"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 269).
-spec error_with_path_and_location_test() -> nil.
error_with_path_and_location_test() ->
    Err = begin
        _pipe = mochi@error:error(<<"Field 'email' is not valid"/utf8>>),
        _pipe@1 = mochi@error:at_location(_pipe, 10, 15),
        _pipe@2 = mochi@error:with_path(
            _pipe@1,
            [{field_segment, <<"query"/utf8>>},
                {field_segment, <<"users"/utf8>>},
                {index_segment, 0},
                {field_segment, <<"email"/utf8>>}]
        ),
        mochi@error:with_code(_pipe@2, <<"VALIDATION_ERROR"/utf8>>)
    end,
    _pipe@3 = Err,
    _pipe@4 = mochi@error:to_dynamic(_pipe@3),
    _pipe@5 = mochi@json:encode_pretty(_pipe@4, 2),
    birdie:snap(_pipe@5, <<"GraphQL error with path and location"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 292).
-spec response_success_test() -> nil.
response_success_test() ->
    Data = maps:from_list(
        [{<<"user"/utf8>>,
                gleam_stdlib:identity(
                    maps:from_list(
                        [{<<"id"/utf8>>, gleam_stdlib:identity(<<"123"/utf8>>)},
                            {<<"name"/utf8>>,
                                gleam_stdlib:identity(<<"Alice"/utf8>>)}]
                    )
                )}]
    ),
    Resp = begin
        _pipe = mochi@response:success(gleam_stdlib:identity(Data)),
        mochi@response:with_extension(
            _pipe,
            <<"requestId"/utf8>>,
            gleam_stdlib:identity(<<"req-abc-123"/utf8>>)
        )
    end,
    _pipe@1 = Resp,
    _pipe@2 = mochi@response:to_json_pretty(_pipe@1),
    birdie:snap(_pipe@2, <<"GraphQL success response with extensions"/utf8>>).

-file("test/mochi_snapshot_test.gleam", 316).
-spec response_with_errors_test() -> nil.
response_with_errors_test() ->
    Data = maps:from_list([{<<"user"/utf8>>, gleam_stdlib:identity(nil)}]),
    Errors = [begin
            _pipe = mochi@error:error(<<"User not found"/utf8>>),
            _pipe@1 = mochi@error:with_path(
                _pipe,
                [{field_segment, <<"query"/utf8>>},
                    {field_segment, <<"user"/utf8>>}]
            ),
            mochi@error:with_code(_pipe@1, <<"NOT_FOUND"/utf8>>)
        end],
    Resp = mochi@response:partial(gleam_stdlib:identity(Data), Errors),
    _pipe@2 = Resp,
    _pipe@3 = mochi@response:to_json_pretty(_pipe@2),
    birdie:snap(_pipe@3, <<"GraphQL partial response with errors"/utf8>>).
