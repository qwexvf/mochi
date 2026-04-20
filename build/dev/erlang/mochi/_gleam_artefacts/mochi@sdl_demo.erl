-module(mochi@sdl_demo).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/sdl_demo.gleam").
-export([demo_sdl_parsing/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi/sdl_demo.gleam", 259).
-spec int_to_string(integer()) -> binary().
int_to_string(Value) ->
    case Value of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        2 ->
            <<"2"/utf8>>;

        3 ->
            <<"3"/utf8>>;

        4 ->
            <<"4"/utf8>>;

        5 ->
            <<"5"/utf8>>;

        6 ->
            <<"6"/utf8>>;

        7 ->
            <<"7"/utf8>>;

        8 ->
            <<"8"/utf8>>;

        9 ->
            <<"9"/utf8>>;

        _ ->
            <<"many"/utf8>>
    end.

-file("src/mochi/sdl_demo.gleam", 249).
-spec count_definitions(list(mochi@sdl_ast:type_system_definition()), integer()) -> binary().
count_definitions(Definitions, Acc) ->
    case Definitions of
        [] ->
            int_to_string(Acc);

        [_ | Rest] ->
            count_definitions(Rest, Acc + 1)
    end.

-file("src/mochi/sdl_demo.gleam", 300).
-spec get_type_kind(mochi@sdl_ast:type_def()) -> binary().
get_type_kind(Type_def) ->
    case Type_def of
        {object_type_definition, _} ->
            <<"type"/utf8>>;

        {interface_type_definition, _} ->
            <<"interface"/utf8>>;

        {union_type_definition, _} ->
            <<"union"/utf8>>;

        {scalar_type_definition, _} ->
            <<"scalar"/utf8>>;

        {enum_type_definition, _} ->
            <<"enum"/utf8>>;

        {input_object_type_definition, _} ->
            <<"input"/utf8>>
    end.

-file("src/mochi/sdl_demo.gleam", 288).
-spec print_definition(mochi@sdl_ast:type_system_definition(), integer()) -> nil.
print_definition(Definition, Index) ->
    Index_str = int_to_string(Index),
    case Definition of
        {type_definition, Type_def} ->
            Type_name = mochi@sdl_ast:get_type_name(Type_def),
            Type_kind = get_type_kind(Type_def),
            gleam_stdlib:println(
                <<<<<<<<<<"  "/utf8, Index_str/binary>>/binary, ". "/utf8>>/binary,
                            Type_kind/binary>>/binary,
                        " "/utf8>>/binary,
                    Type_name/binary>>
            );

        _ ->
            gleam_stdlib:println(
                <<<<"  "/utf8, Index_str/binary>>/binary,
                    ". Other definition"/utf8>>
            )
    end.

-file("src/mochi/sdl_demo.gleam", 275).
-spec print_definition_list(
    list(mochi@sdl_ast:type_system_definition()),
    integer()
) -> nil.
print_definition_list(Definitions, Index) ->
    case Definitions of
        [] ->
            nil;

        [Def | Rest] ->
            print_definition(Def, Index),
            print_definition_list(Rest, Index + 1)
    end.

-file("src/mochi/sdl_demo.gleam", 236).
-spec print_document_summary(mochi@sdl_ast:s_d_l_document()) -> nil.
print_document_summary(Document) ->
    gleam_stdlib:println(<<"Document contains:"/utf8>>),
    Count = count_definitions(erlang:element(2, Document), 0),
    gleam_stdlib:println(
        <<<<"  - "/utf8, Count/binary>>/binary, " type definitions"/utf8>>
    ),
    case erlang:element(2, Document) of
        [] ->
            gleam_stdlib:println(<<"  (no definitions)"/utf8>>);

        Definitions ->
            print_definition_list(Definitions, 1)
    end.

-file("src/mochi/sdl_demo.gleam", 341).
-spec format_lex_error(mochi@sdl_lexer:s_d_l_lexer_error()) -> binary().
format_lex_error(Error) ->
    case Error of
        {unexpected_character, Char, _} ->
            <<"Unexpected character: "/utf8, Char/binary>>;

        {invalid_number, Value, _} ->
            <<"Invalid number: "/utf8, Value/binary>>;

        {unterminated_string, _} ->
            <<"Unterminated string"/utf8>>;

        {unterminated_description, _} ->
            <<"Unterminated description"/utf8>>
    end.

-file("src/mochi/sdl_demo.gleam", 350).
-spec format_token(mochi@sdl_lexer:s_d_l_token()) -> binary().
format_token(Token) ->
    case Token of
        {name, Name} ->
            <<<<"Name("/utf8, Name/binary>>/binary, ")"/utf8>>;

        type ->
            <<"type"/utf8>>;

        interface ->
            <<"interface"/utf8>>;

        union ->
            <<"union"/utf8>>;

        scalar ->
            <<"scalar"/utf8>>;

        enum ->
            <<"enum"/utf8>>;

        input ->
            <<"input"/utf8>>;

        left_brace ->
            <<"{"/utf8>>;

        right_brace ->
            <<"}"/utf8>>;

        left_paren ->
            <<"("/utf8>>;

        right_paren ->
            <<")"/utf8>>;

        left_bracket ->
            <<"["/utf8>>;

        right_bracket ->
            <<"]"/utf8>>;

        colon ->
            <<":"/utf8>>;

        bang ->
            <<"!"/utf8>>;

        equals ->
            <<"="/utf8>>;

        pipe ->
            <<"|"/utf8>>;

        amp ->
            <<"&"/utf8>>;

        e_o_f ->
            <<"EOF"/utf8>>;

        _ ->
            <<"Token"/utf8>>
    end.

-file("src/mochi/sdl_demo.gleam", 311).
-spec print_parse_error(mochi@sdl_parser:s_d_l_parse_error()) -> nil.
print_parse_error(Error) ->
    case Error of
        {s_d_l_lex_error, Lex_error} ->
            gleam_stdlib:println(
                <<"  Lexer error: "/utf8, (format_lex_error(Lex_error))/binary>>
            );

        {unexpected_token, Expected, Got, Position} ->
            gleam_stdlib:println(<<"  Expected: "/utf8, Expected/binary>>),
            gleam_stdlib:println(<<"  Got: "/utf8, (format_token(Got))/binary>>),
            gleam_stdlib:println(
                <<<<<<"  At: line "/utf8,
                            (int_to_string(erlang:element(2, Position)))/binary>>/binary,
                        ", column "/utf8>>/binary,
                    (int_to_string(erlang:element(3, Position)))/binary>>
            );

        {unexpected_e_o_f, Expected@1} ->
            gleam_stdlib:println(
                <<"  Unexpected end of file, expected: "/utf8,
                    Expected@1/binary>>
            );

        {invalid_type_definition, Message, Position@1} ->
            gleam_stdlib:println(
                <<"  Invalid type definition: "/utf8, Message/binary>>
            ),
            gleam_stdlib:println(
                <<<<<<"  At: line "/utf8,
                            (int_to_string(erlang:element(2, Position@1)))/binary>>/binary,
                        ", column "/utf8>>/binary,
                    (int_to_string(erlang:element(3, Position@1)))/binary>>
            )
    end.

-file("src/mochi/sdl_demo.gleam", 33).
-spec demo_basic_types() -> nil.
demo_basic_types() ->
    gleam_stdlib:println(<<"📝 Test 1: Basic Types"/utf8>>),
    gleam_stdlib:println(<<"======================"/utf8>>),
    Schema_sdl = <<"
    scalar DateTime
    
    type User {
      id: ID!
      name: String
      email: String!
    }
    
    interface Node {
      id: ID!
    }
  "/utf8>>,
    gleam_stdlib:println(<<"Input SDL:"/utf8>>),
    gleam_stdlib:println(Schema_sdl),
    case mochi@sdl_parser:parse_sdl(Schema_sdl) of
        {ok, Document} ->
            gleam_stdlib:println(<<"✅ Parsing successful!"/utf8>>),
            print_document_summary(Document);

        {error, Error} ->
            gleam_stdlib:println(<<"❌ Parsing failed:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi/sdl_demo.gleam", 69).
-spec demo_complex_object() -> nil.
demo_complex_object() ->
    gleam_stdlib:println(<<"📝 Test 2: Complex Object with Arguments"/utf8>>),
    gleam_stdlib:println(<<"========================================"/utf8>>),
    Schema_sdl = <<"
    type Query {
      user(id: ID!): User
      posts(limit: Int = 10, offset: Int = 0): [Post!]!
      search(query: String!, type: SearchType = USER): SearchResult
    }
    
    type Post {
      id: ID!
      title: String!
      content: String
      author: User!
    }
  "/utf8>>,
    gleam_stdlib:println(<<"Input SDL:"/utf8>>),
    gleam_stdlib:println(Schema_sdl),
    case mochi@sdl_parser:parse_sdl(Schema_sdl) of
        {ok, Document} ->
            gleam_stdlib:println(<<"✅ Complex parsing successful!"/utf8>>),
            print_document_summary(Document);

        {error, Error} ->
            gleam_stdlib:println(<<"❌ Complex parsing failed:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi/sdl_demo.gleam", 106).
-spec demo_union_type() -> nil.
demo_union_type() ->
    gleam_stdlib:println(<<"📝 Test 3: Union Types"/utf8>>),
    gleam_stdlib:println(<<"======================"/utf8>>),
    Schema_sdl = <<"
    union SearchResult = User | Post | Comment
    
    union MediaItem = Photo | Video | Audio
  "/utf8>>,
    gleam_stdlib:println(<<"Input SDL:"/utf8>>),
    gleam_stdlib:println(Schema_sdl),
    case mochi@sdl_parser:parse_sdl(Schema_sdl) of
        {ok, Document} ->
            gleam_stdlib:println(<<"✅ Union parsing successful!"/utf8>>),
            print_document_summary(Document);

        {error, Error} ->
            gleam_stdlib:println(<<"❌ Union parsing failed:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi/sdl_demo.gleam", 134).
-spec demo_enum_type() -> nil.
demo_enum_type() ->
    gleam_stdlib:println(<<"📝 Test 4: Enum Types"/utf8>>),
    gleam_stdlib:println(<<"====================="/utf8>>),
    Schema_sdl = <<"
    enum Status {
      ACTIVE
      INACTIVE
      PENDING
    }
    
    enum UserRole {
      ADMIN
      USER
      MODERATOR
    }
  "/utf8>>,
    gleam_stdlib:println(<<"Input SDL:"/utf8>>),
    gleam_stdlib:println(Schema_sdl),
    case mochi@sdl_parser:parse_sdl(Schema_sdl) of
        {ok, Document} ->
            gleam_stdlib:println(<<"✅ Enum parsing successful!"/utf8>>),
            print_document_summary(Document);

        {error, Error} ->
            gleam_stdlib:println(<<"❌ Enum parsing failed:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi/sdl_demo.gleam", 170).
-spec demo_input_type() -> nil.
demo_input_type() ->
    gleam_stdlib:println(<<"📝 Test 5: Input Types"/utf8>>),
    gleam_stdlib:println(<<"======================"/utf8>>),
    Schema_sdl = <<"
    input CreateUserInput {
      name: String!
      email: String!
      age: Int = 18
      role: UserRole = USER
    }
    
    input UpdateUserInput {
      id: ID!
      name: String
      email: String
    }
  "/utf8>>,
    gleam_stdlib:println(<<"Input SDL:"/utf8>>),
    gleam_stdlib:println(Schema_sdl),
    case mochi@sdl_parser:parse_sdl(Schema_sdl) of
        {ok, Document} ->
            gleam_stdlib:println(<<"✅ Input parsing successful!"/utf8>>),
            print_document_summary(Document);

        {error, Error} ->
            gleam_stdlib:println(<<"❌ Input parsing failed:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>).

-file("src/mochi/sdl_demo.gleam", 207).
-spec demo_error_handling() -> nil.
demo_error_handling() ->
    gleam_stdlib:println(<<"📝 Test 6: Error Handling"/utf8>>),
    gleam_stdlib:println(<<"========================="/utf8>>),
    Invalid_sdl = <<"
    type User {
      id: ID!
      name: String
      # Missing closing brace intentionally
  "/utf8>>,
    gleam_stdlib:println(<<"Invalid SDL (missing closing brace):"/utf8>>),
    gleam_stdlib:println(Invalid_sdl),
    case mochi@sdl_parser:parse_sdl(Invalid_sdl) of
        {ok, _} ->
            gleam_stdlib:println(<<"❌ Should have failed but didn't!"/utf8>>);

        {error, Error} ->
            gleam_stdlib:println(<<"✅ Correctly caught error:"/utf8>>),
            print_parse_error(Error)
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🎯 SDL Demo Complete!"/utf8>>).

-file("src/mochi/sdl_demo.gleam", 10).
?DOC(" Comprehensive SDL demo showing parsing and error handling\n").
-spec demo_sdl_parsing() -> nil.
demo_sdl_parsing() ->
    gleam_stdlib:println(<<"=== GeQL SDL Complete Demo ==="/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    demo_basic_types(),
    demo_complex_object(),
    demo_union_type(),
    demo_enum_type(),
    demo_input_type(),
    demo_error_handling().
