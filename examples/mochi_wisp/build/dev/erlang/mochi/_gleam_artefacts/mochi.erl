-module(mochi).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi.gleam").
-export([parse/1, execute/2, new_schema/0, main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi.gleam", 51).
?DOC(" Parse a GraphQL query string\n").
-spec parse(binary()) -> {ok, mochi@ast:document()} |
    {error, mochi@parser:parse_error()}.
parse(Query_string) ->
    mochi@parser:parse(Query_string).

-file("src/mochi.gleam", 56).
?DOC(" Execute a GraphQL query against a schema\n").
-spec execute(mochi@schema:schema(), binary()) -> mochi@executor:execution_result().
execute(Schema, Query_string) ->
    mochi@executor:execute_query(Schema, Query_string).

-file("src/mochi.gleam", 64).
?DOC(" Create a new schema builder\n").
-spec new_schema() -> mochi@query:schema_builder().
new_schema() ->
    mochi@query:new().

-file("src/mochi.gleam", 120).
-spec create_demo_schema() -> mochi@schema:schema().
create_demo_schema() ->
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
                mochi@schema:resolver(
                    _pipe@2,
                    fun(_) -> {error, <<"Demo"/utf8>>} end
                )
            end
        ),
        mochi@schema:field(
            _pipe@3,
            begin
                _pipe@4 = mochi@schema:field_def(
                    <<"name"/utf8>>,
                    mochi@schema:string_type()
                ),
                mochi@schema:resolver(
                    _pipe@4,
                    fun(_) -> {error, <<"Demo"/utf8>>} end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@5 = mochi@schema:object(<<"Query"/utf8>>),
        mochi@schema:field(
            _pipe@5,
            begin
                _pipe@6 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                mochi@schema:resolver(
                    _pipe@6,
                    fun(_) -> {error, <<"Demo"/utf8>>} end
                )
            end
        )
    end,
    _pipe@7 = mochi@schema:schema(),
    _pipe@8 = mochi@schema:'query'(_pipe@7, Query_type),
    mochi@schema:add_type(_pipe@8, {object_type_def, User_type}).

-file("src/mochi.gleam", 72).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"🍡 mochi - Code First GraphQL for Gleam"/utf8>>),
    gleam_stdlib:println(<<"========================================"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"📦 Code First API Example:"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  // Define your Gleam type"/utf8>>),
    gleam_stdlib:println(<<"  pub type User {"/utf8>>),
    gleam_stdlib:println(
        <<"    User(id: String, name: String, age: Int)"/utf8>>
    ),
    gleam_stdlib:println(<<"  }"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  // Build GraphQL type with type-safe extractors"/utf8>>
    ),
    gleam_stdlib:println(<<"  let user_type = types.object(\"User\")"/utf8>>),
    gleam_stdlib:println(
        <<"    |> types.id(\"id\", fn(u: User) { u.id })"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    |> types.string(\"name\", fn(u: User) { u.name })"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    |> types.int(\"age\", fn(u: User) { u.age })"/utf8>>
    ),
    gleam_stdlib:println(<<"    |> types.build(decode_user)"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    Query = <<"{ user { id name } }"/utf8>>,
    case mochi@parser:parse(Query) of
        {ok, _} ->
            gleam_stdlib:println(<<"✅ GraphQL Parser: Working!"/utf8>>);

        {error, _} ->
            gleam_stdlib:println(<<"❌ Parser error"/utf8>>)
    end,
    _ = create_demo_schema(),
    gleam_stdlib:println(<<"✅ Schema Builder: Working!"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"📚 Available Modules:"/utf8>>),
    gleam_stdlib:println(
        <<"  - mochi/types   : Type builders (object, string, int, etc.)"/utf8>>
    ),
    gleam_stdlib:println(<<"  - mochi/query   : Query/Mutation builders"/utf8>>),
    gleam_stdlib:println(<<"  - mochi/schema  : Low-level schema types"/utf8>>),
    gleam_stdlib:println(<<"  - mochi/parser  : GraphQL query parser"/utf8>>),
    gleam_stdlib:println(<<"  - mochi/executor: Query execution engine"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🎉 Ready to build type-safe GraphQL APIs!"/utf8>>).
