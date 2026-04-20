-module(introspection_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/introspection_test.gleam").
-export([parse_typename_query_test/0, parse_schema_introspection_test/0, parse_type_introspection_test/0, parse_full_introspection_query_test/0, execute_typename_test/0, execute_schema_introspection_test/0, execute_type_introspection_test/0, execute_type_introspection_with_variable_test/0, schema_has_user_type_test/0, schema_has_role_enum_test/0, schema_query_type_test/0, enum_type_kind_test/0, field_type_kind_for_enum_test/0, schema_directives_not_empty_test/0, schema_directives_include_skip_test/0, schema_directive_locations_test/0, interface_possible_types_test/0, scalar_field_type_kind_test/0, input_field_default_value_test/0, deep_oftype_traversal_test/0, schema_selection_set_test/0, type_nested_fields_traversal_test/0, full_introspection_deep_nesting_test/0, include_deprecated_argument_accepted_test/0, introspection_fields_without_include_deprecated_test/0]).
-export_type([test_user/0, item/0, dog/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type test_user() :: {test_user, binary(), binary(), binary()}.

-type item() :: {item, binary(), binary()}.

-type dog() :: {dog, binary()}.

-file("test/introspection_test.gleam", 19).
-spec assert_true(boolean(), binary()) -> nil.
assert_true(Condition, Message) ->
    case Condition of
        true ->
            nil;

        false ->
            erlang:error(#{gleam_error => panic,
                    message => Message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"assert_true"/utf8>>,
                    line => 22})
    end.

-file("test/introspection_test.gleam", 26).
-spec assert_ok({ok, OAK} | {error, any()}, binary()) -> OAK.
assert_ok(Res, Message) ->
    case Res of
        {ok, V} ->
            V;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => Message,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"assert_ok"/utf8>>,
                    line => 29})
    end.

-file("test/introspection_test.gleam", 33).
-spec parse_ok(binary()) -> nil.
parse_ok(Query_str) ->
    case mochi@parser:parse(Query_str) of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Parse should succeed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"parse_ok"/utf8>>,
                    line => 36})
    end.

-file("test/introspection_test.gleam", 48).
-spec decode_test_user(gleam@dynamic:dynamic_()) -> {ok, test_user()} |
    {error, binary()}.
decode_test_user(_) ->
    {ok,
        {test_user,
            <<"1"/utf8>>,
            <<"Test User"/utf8>>,
            <<"test@example.com"/utf8>>}}.

-file("test/introspection_test.gleam", 52).
-spec create_test_schema() -> mochi@schema:schema().
create_test_schema() ->
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
        _pipe@3 = mochi@types:string(
            _pipe@2,
            <<"name"/utf8>>,
            fun(U@1) -> erlang:element(3, U@1) end
        ),
        _pipe@4 = mochi@types:string(
            _pipe@3,
            <<"email"/utf8>>,
            fun(U@2) -> erlang:element(4, U@2) end
        ),
        mochi@types:build(_pipe@4, fun decode_test_user/1)
    end,
    Role_enum = begin
        _pipe@5 = mochi@types:enum_type(<<"Role"/utf8>>),
        _pipe@6 = mochi@types:enum_description(_pipe@5, <<"User roles"/utf8>>),
        _pipe@7 = mochi@types:value(_pipe@6, <<"ADMIN"/utf8>>),
        _pipe@8 = mochi@types:value(_pipe@7, <<"USER"/utf8>>),
        _pipe@9 = mochi@types:value(_pipe@8, <<"GUEST"/utf8>>),
        mochi@types:build_enum(_pipe@9)
    end,
    Users_query = begin
        _pipe@10 = mochi@query:'query'(
            <<"users"/utf8>>,
            mochi@schema:list_type(mochi@schema:named_type(<<"User"/utf8>>)),
            fun(_) ->
                {ok,
                    [{test_user,
                            <<"1"/utf8>>,
                            <<"Test"/utf8>>,
                            <<"test@example.com"/utf8>>}]}
            end,
            fun gleam_stdlib:identity/1
        ),
        mochi@query:query_description(_pipe@10, <<"Get all users"/utf8>>)
    end,
    User_query = begin
        _pipe@13 = mochi@query:query_with_args(
            <<"user"/utf8>>,
            [mochi@query:arg(
                    <<"id"/utf8>>,
                    mochi@schema:non_null(mochi@schema:id_type())
                )],
            mochi@schema:named_type(<<"User"/utf8>>),
            fun(Args) -> _pipe@11 = gleam_stdlib:map_get(Args, <<"id"/utf8>>),
                _pipe@12 = gleam@result:map(
                    _pipe@11,
                    fun(_) -> <<"1"/utf8>> end
                ),
                gleam@result:map_error(
                    _pipe@12,
                    fun(_) -> <<"Missing id"/utf8>> end
                ) end,
            fun(Id, _) ->
                {ok,
                    {test_user,
                        Id,
                        <<"User "/utf8, Id/binary>>,
                        <<"user@example.com"/utf8>>}}
            end,
            fun gleam_stdlib:identity/1
        ),
        mochi@query:query_description(_pipe@13, <<"Get a user by ID"/utf8>>)
    end,
    _pipe@14 = mochi@query:new(),
    _pipe@15 = mochi@query:add_query(_pipe@14, Users_query),
    _pipe@16 = mochi@query:add_query(_pipe@15, User_query),
    _pipe@17 = mochi@query:add_type(_pipe@16, User_type),
    _pipe@18 = mochi@query:add_enum(_pipe@17, Role_enum),
    mochi@query:build(_pipe@18).

-file("test/introspection_test.gleam", 105).
-spec parse_typename_query_test() -> nil.
parse_typename_query_test() ->
    parse_ok(<<"{ user(id: \"1\") { __typename name } }"/utf8>>).

-file("test/introspection_test.gleam", 109).
-spec parse_schema_introspection_test() -> nil.
parse_schema_introspection_test() ->
    parse_ok(<<"{ __schema { queryType { name } } }"/utf8>>).

-file("test/introspection_test.gleam", 113).
-spec parse_type_introspection_test() -> nil.
parse_type_introspection_test() ->
    parse_ok(<<"{ __type(name: \"User\") { name kind } }"/utf8>>).

-file("test/introspection_test.gleam", 117).
-spec parse_full_introspection_query_test() -> nil.
parse_full_introspection_query_test() ->
    Query_str = <<"
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          kind
          name
          description
          fields {
            name
            description
            args {
              name
              description
              type { kind name ofType { kind name } }
            }
            type { kind name ofType { kind name } }
          }
          enumValues { name description }
        }
        directives { name description }
      }
    }
  "/utf8>>,
    parse_ok(Query_str).

-file("test/introspection_test.gleam", 152).
-spec execute_typename_test() -> nil.
execute_typename_test() ->
    Test_schema = create_test_schema(),
    _ = mochi@executor:execute_query(Test_schema, <<"{ __typename }"/utf8>>),
    nil.

-file("test/introspection_test.gleam", 158).
-spec execute_schema_introspection_test() -> nil.
execute_schema_introspection_test() ->
    Test_schema = create_test_schema(),
    _ = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { queryType { name } } }"/utf8>>
    ),
    nil.

-file("test/introspection_test.gleam", 165).
-spec execute_type_introspection_test() -> nil.
execute_type_introspection_test() ->
    Test_schema = create_test_schema(),
    _ = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"User\") { name kind } }"/utf8>>
    ),
    nil.

-file("test/introspection_test.gleam", 175).
-spec execute_type_introspection_with_variable_test() -> nil.
execute_type_introspection_with_variable_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"query GetType($name: String!) { __type(name: $name) { name kind } }"/utf8>>,
    Variables = maps:from_list(
        [{<<"name"/utf8>>, gleam_stdlib:identity(<<"User"/utf8>>)}]
    ),
    _ = mochi@executor:execute_query_with_variables(
        Test_schema,
        Query_str,
        Variables
    ),
    nil.

-file("test/introspection_test.gleam", 189).
-spec schema_has_user_type_test() -> nil.
schema_has_user_type_test() ->
    Test_schema = create_test_schema(),
    User_type = assert_ok(
        gleam_stdlib:map_get(erlang:element(5, Test_schema), <<"User"/utf8>>),
        <<"Schema should have User type"/utf8>>
    ),
    case User_type of
        {object_type_def, Obj} ->
            assert_true(
                erlang:element(2, Obj) =:= <<"User"/utf8>>,
                <<"Type name should be User"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be ObjectTypeDef"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"schema_has_user_type_test"/utf8>>,
                    line => 201})
    end.

-file("test/introspection_test.gleam", 205).
-spec schema_has_role_enum_test() -> nil.
schema_has_role_enum_test() ->
    Test_schema = create_test_schema(),
    Role_type = assert_ok(
        gleam_stdlib:map_get(erlang:element(5, Test_schema), <<"Role"/utf8>>),
        <<"Schema should have Role enum"/utf8>>
    ),
    case Role_type of
        {enum_type_def, Enum} ->
            assert_true(
                erlang:element(2, Enum) =:= <<"Role"/utf8>>,
                <<"Enum name should be Role"/utf8>>
            ),
            assert_true(
                maps:size(erlang:element(4, Enum)) =:= 3,
                <<"Role enum should have 3 values"/utf8>>
            );

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should be EnumTypeDef"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"schema_has_role_enum_test"/utf8>>,
                    line => 219})
    end.

-file("test/introspection_test.gleam", 223).
-spec schema_query_type_test() -> nil.
schema_query_type_test() ->
    Test_schema = create_test_schema(),
    case erlang:element(2, Test_schema) of
        {some, Query_type} ->
            assert_true(
                erlang:element(2, Query_type) =:= <<"Query"/utf8>>,
                <<"Query type name should be Query"/utf8>>
            ),
            _ = assert_ok(
                gleam_stdlib:map_get(
                    erlang:element(4, Query_type),
                    <<"users"/utf8>>
                ),
                <<"Should have users field"/utf8>>
            ),
            _ = assert_ok(
                gleam_stdlib:map_get(
                    erlang:element(4, Query_type),
                    <<"user"/utf8>>
                ),
                <<"Should have user field"/utf8>>
            ),
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Schema should have query type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"schema_query_type_test"/utf8>>,
                    line => 238})
    end.

-file("test/introspection_test.gleam", 246).
-spec get_string_field(gleam@dynamic:dynamic_(), binary()) -> binary().
get_string_field(D, Key) ->
    _pipe = gleam@dynamic@decode:run(
        D,
        gleam@dynamic@decode:at(
            [Key],
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        )
    ),
    gleam@result:unwrap(_pipe, <<""/utf8>>).

-file("test/introspection_test.gleam", 251).
-spec get_list_field(gleam@dynamic:dynamic_(), binary()) -> list(gleam@dynamic:dynamic_()).
get_list_field(D, Key) ->
    _pipe = gleam@dynamic@decode:run(
        D,
        gleam@dynamic@decode:at(
            [Key],
            gleam@dynamic@decode:list(
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
            )
        )
    ),
    gleam@result:unwrap(_pipe, []).

-file("test/introspection_test.gleam", 256).
-spec get_data(mochi@executor:execution_result()) -> gleam@dynamic:dynamic_().
get_data(Result) ->
    case erlang:element(2, Result) of
        {some, D} ->
            D;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data but got None"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"get_data"/utf8>>,
                    line => 259})
    end.

-file("test/introspection_test.gleam", 265).
?DOC(
    " The executor returns data as a list of field dicts merged together.\n"
    " This helper finds a field by name within that list.\n"
).
-spec find_field(gleam@dynamic:dynamic_(), binary()) -> gleam@dynamic:dynamic_().
find_field(Data, Name) ->
    case gleam@dynamic@decode:run(
        Data,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ) of
        {ok, Items} ->
            _pipe = gleam@list:find_map(
                Items,
                fun(Item) ->
                    gleam@dynamic@decode:run(
                        Item,
                        gleam@dynamic@decode:at(
                            [Name],
                            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                        )
                    )
                end
            ),
            gleam@result:unwrap(_pipe, gleam_stdlib:identity(nil));

        {error, _} ->
            _pipe@1 = gleam@dynamic@decode:run(
                Data,
                gleam@dynamic@decode:at(
                    [Name],
                    {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                )
            ),
            gleam@result:unwrap(_pipe@1, gleam_stdlib:identity(nil))
    end.

-file("test/introspection_test.gleam", 280).
-spec enum_type_kind_test() -> nil.
enum_type_kind_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"Role\") { kind name } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    assert_true(
        get_string_field(Type_obj, <<"kind"/utf8>>) =:= <<"ENUM"/utf8>>,
        <<"Role enum should have kind ENUM, not OBJECT"/utf8>>
    ),
    assert_true(
        get_string_field(Type_obj, <<"name"/utf8>>) =:= <<"Role"/utf8>>,
        <<"Type name should be Role"/utf8>>
    ).

-file("test/introspection_test.gleam", 304).
-spec field_type_kind_for_enum_test() -> nil.
field_type_kind_for_enum_test() ->
    Role_enum = begin
        _pipe = mochi@types:enum_type(<<"Status"/utf8>>),
        _pipe@1 = mochi@types:value(_pipe, <<"ACTIVE"/utf8>>),
        _pipe@2 = mochi@types:value(_pipe@1, <<"INACTIVE"/utf8>>),
        mochi@types:build_enum(_pipe@2)
    end,
    Item_type = begin
        _pipe@3 = mochi@types:object(<<"Item"/utf8>>),
        _pipe@4 = mochi@types:id(
            _pipe@3,
            <<"id"/utf8>>,
            fun(I) -> erlang:element(2, I) end
        ),
        _pipe@5 = mochi@types:string(
            _pipe@4,
            <<"status"/utf8>>,
            fun(I@1) -> erlang:element(3, I@1) end
        ),
        mochi@types:build(
            _pipe@5,
            fun(_) -> {ok, {item, <<"1"/utf8>>, <<"ACTIVE"/utf8>>}} end
        )
    end,
    Test_schema = begin
        _pipe@6 = mochi@query:new(),
        _pipe@7 = mochi@query:add_query(
            _pipe@6,
            mochi@query:'query'(
                <<"item"/utf8>>,
                mochi@schema:named_type(<<"Item"/utf8>>),
                fun(_) -> {ok, {item, <<"1"/utf8>>, <<"ACTIVE"/utf8>>}} end,
                fun gleam_stdlib:identity/1
            )
        ),
        _pipe@8 = mochi@query:add_type(_pipe@7, Item_type),
        _pipe@9 = mochi@query:add_enum(_pipe@8, Role_enum),
        mochi@query:build(_pipe@9)
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"Status\") { kind name } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    assert_true(
        get_string_field(Type_obj, <<"kind"/utf8>>) =:= <<"ENUM"/utf8>>,
        <<"Status should have kind ENUM"/utf8>>
    ).

-file("test/introspection_test.gleam", 344).
-spec schema_directives_not_empty_test() -> nil.
schema_directives_not_empty_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { directives { name } } }"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Directives = get_list_field(Schema_obj, <<"directives"/utf8>>),
    assert_true(
        Directives /= [],
        <<"Schema should expose at least the built-in directives"/utf8>>
    ).

-file("test/introspection_test.gleam", 357).
-spec schema_directives_include_skip_test() -> nil.
schema_directives_include_skip_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { directives { name } } }"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Directives = get_list_field(Schema_obj, <<"directives"/utf8>>),
    Names = gleam@list:map(
        Directives,
        fun(D) -> get_string_field(D, <<"name"/utf8>>) end
    ),
    assert_true(
        gleam@list:contains(Names, <<"skip"/utf8>>),
        <<"@skip directive should be in schema directives"/utf8>>
    ),
    assert_true(
        gleam@list:contains(Names, <<"include"/utf8>>),
        <<"@include directive should be in schema directives"/utf8>>
    ),
    assert_true(
        gleam@list:contains(Names, <<"deprecated"/utf8>>),
        <<"@deprecated directive should be in schema directives"/utf8>>
    ).

-file("test/introspection_test.gleam", 380).
-spec schema_directive_locations_test() -> nil.
schema_directive_locations_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { directives { name locations } } }"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Directives = get_list_field(Schema_obj, <<"directives"/utf8>>),
    Skip_dir = gleam@list:find(
        Directives,
        fun(D) -> get_string_field(D, <<"name"/utf8>>) =:= <<"skip"/utf8>> end
    ),
    case Skip_dir of
        {ok, D@1} ->
            Locations = get_list_field(D@1, <<"locations"/utf8>>),
            assert_true(Locations /= [], <<"@skip should have locations"/utf8>>);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected @skip directive"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"schema_directive_locations_test"/utf8>>,
                    line => 397})
    end.

-file("test/introspection_test.gleam", 406).
-spec interface_possible_types_test() -> nil.
interface_possible_types_test() ->
    Animal_interface = begin
        _pipe = mochi@schema:interface(<<"Animal"/utf8>>),
        mochi@schema:interface_field(
            _pipe,
            mochi@schema:field_def(<<"name"/utf8>>, mochi@schema:string_type())
        )
    end,
    Dog_type = begin
        _pipe@1 = mochi@types:object(<<"Dog2"/utf8>>),
        _pipe@2 = mochi@types:string(
            _pipe@1,
            <<"name"/utf8>>,
            fun(D) -> erlang:element(2, D) end
        ),
        _pipe@3 = mochi@types:build(
            _pipe@2,
            fun(_) -> {ok, {dog, <<"Rex"/utf8>>}} end
        ),
        (fun(Obj) ->
            {object_type,
                erlang:element(2, Obj),
                erlang:element(3, Obj),
                erlang:element(4, Obj),
                [Animal_interface]}
        end)(_pipe@3)
    end,
    Test_schema = begin
        _pipe@4 = mochi@query:new(),
        _pipe@5 = mochi@query:add_query(
            _pipe@4,
            mochi@query:'query'(
                <<"dog"/utf8>>,
                mochi@schema:named_type(<<"Dog2"/utf8>>),
                fun(_) -> {ok, {dog, <<"Rex"/utf8>>}} end,
                fun gleam_stdlib:identity/1
            )
        ),
        _pipe@6 = mochi@query:add_type(_pipe@5, Dog_type),
        _pipe@7 = mochi@query:add_interface(_pipe@6, Animal_interface),
        mochi@query:build(_pipe@7)
    end,
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"Animal\") { kind possibleTypes { name kind } } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    assert_true(
        get_string_field(Type_obj, <<"kind"/utf8>>) =:= <<"INTERFACE"/utf8>>,
        <<"Animal should be kind INTERFACE"/utf8>>
    ),
    Possible = get_list_field(Type_obj, <<"possibleTypes"/utf8>>),
    assert_true(Possible /= [], <<"Interface should have possibleTypes"/utf8>>),
    Names = gleam@list:map(
        Possible,
        fun(T) -> get_string_field(T, <<"name"/utf8>>) end
    ),
    assert_true(
        gleam@list:contains(Names, <<"Dog2"/utf8>>),
        <<"Dog2 should be in possibleTypes for Animal interface"/utf8>>
    ).

-file("test/introspection_test.gleam", 451).
-spec scalar_field_type_kind_test() -> nil.
scalar_field_type_kind_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"User\") { fields { name type { kind name } } } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    Fields = get_list_field(Type_obj, <<"fields"/utf8>>),
    Id_field = gleam@list:find(
        Fields,
        fun(F) -> get_string_field(F, <<"name"/utf8>>) =:= <<"id"/utf8>> end
    ),
    case Id_field of
        {ok, F@1} ->
            Field_type = begin
                _pipe = gleam@dynamic@decode:run(
                    F@1,
                    gleam@dynamic@decode:at(
                        [<<"type"/utf8>>],
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ),
                gleam@result:unwrap(_pipe, gleam_stdlib:identity(nil))
            end,
            Of_type = begin
                _pipe@1 = gleam@dynamic@decode:run(
                    Field_type,
                    gleam@dynamic@decode:at(
                        [<<"ofType"/utf8>>],
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ),
                gleam@result:unwrap(_pipe@1, gleam_stdlib:identity(nil))
            end,
            assert_true(
                get_string_field(Of_type, <<"kind"/utf8>>) =:= <<"SCALAR"/utf8>>,
                <<"ID type should be SCALAR kind"/utf8>>
            );

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected id field on User"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"scalar_field_type_kind_test"/utf8>>,
                    line => 478})
    end.

-file("test/introspection_test.gleam", 483).
-spec input_field_default_value_test() -> nil.
input_field_default_value_test() ->
    Filter_input = {input_object_type,
        <<"FilterInput"/utf8>>,
        none,
        maps:from_list(
            [{<<"limit"/utf8>>,
                    {input_field_definition,
                        <<"limit"/utf8>>,
                        none,
                        {named, <<"Int"/utf8>>},
                        {some, gleam_stdlib:identity(10)}}}]
        )},
    Base = create_test_schema(),
    Test_schema = {schema,
        erlang:element(2, Base),
        erlang:element(3, Base),
        erlang:element(4, Base),
        gleam@dict:insert(
            erlang:element(5, Base),
            <<"FilterInput"/utf8>>,
            {input_object_type_def, Filter_input}
        ),
        erlang:element(6, Base),
        erlang:element(7, Base)},
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"FilterInput\") { inputFields { name defaultValue } } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    Input_fields = get_list_field(Type_obj, <<"inputFields"/utf8>>),
    Limit_field = gleam@list:find(
        Input_fields,
        fun(F) -> get_string_field(F, <<"name"/utf8>>) =:= <<"limit"/utf8>> end
    ),
    case Limit_field of
        {ok, F@1} ->
            Dv = get_string_field(F@1, <<"defaultValue"/utf8>>),
            assert_true(
                Dv =:= <<"10"/utf8>>,
                <<"defaultValue for limit should be \"10\""/utf8>>
            );

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected limit input field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"input_field_default_value_test"/utf8>>,
                    line => 527})
    end.

-file("test/introspection_test.gleam", 535).
-spec get_nested_field(gleam@dynamic:dynamic_(), list(binary())) -> gleam@dynamic:dynamic_().
get_nested_field(D, Keys) ->
    case Keys of
        [] ->
            D;

        [Key | Rest] ->
            Nested = begin
                _pipe = gleam@dynamic@decode:run(
                    D,
                    gleam@dynamic@decode:at(
                        [Key],
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ),
                gleam@result:unwrap(_pipe, gleam_stdlib:identity(nil))
            end,
            get_nested_field(Nested, Rest)
    end.

-file("test/introspection_test.gleam", 548).
-spec deep_oftype_traversal_test() -> nil.
deep_oftype_traversal_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { queryType { fields { name type { kind name ofType { kind name } } } } } }"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Query_type = get_nested_field(Schema_obj, [<<"queryType"/utf8>>]),
    Fields = get_list_field(Query_type, <<"fields"/utf8>>),
    Users_field = gleam@list:find(
        Fields,
        fun(F) -> get_string_field(F, <<"name"/utf8>>) =:= <<"users"/utf8>> end
    ),
    case Users_field of
        {ok, F@1} ->
            Field_type = get_nested_field(F@1, [<<"type"/utf8>>]),
            assert_true(
                get_string_field(Field_type, <<"kind"/utf8>>) =:= <<"LIST"/utf8>>,
                <<"users field type should be LIST at top level"/utf8>>
            ),
            Of_type = get_nested_field(Field_type, [<<"ofType"/utf8>>]),
            assert_true(
                get_string_field(Of_type, <<"kind"/utf8>>) =:= <<"OBJECT"/utf8>>,
                <<"ofType should be OBJECT"/utf8>>
            ),
            assert_true(
                get_string_field(Of_type, <<"name"/utf8>>) =:= <<"User"/utf8>>,
                <<"ofType name should be User"/utf8>>
            );

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected users field"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"deep_oftype_traversal_test"/utf8>>,
                    line => 584})
    end.

-file("test/introspection_test.gleam", 589).
-spec schema_selection_set_test() -> nil.
schema_selection_set_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __schema { queryType { name } } }"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Query_type = get_nested_field(Schema_obj, [<<"queryType"/utf8>>]),
    assert_true(
        get_string_field(Query_type, <<"name"/utf8>>) =:= <<"Query"/utf8>>,
        <<"queryType.name should be Query"/utf8>>
    ).

-file("test/introspection_test.gleam", 606).
-spec type_nested_fields_traversal_test() -> nil.
type_nested_fields_traversal_test() ->
    Test_schema = create_test_schema(),
    Result = mochi@executor:execute_query(
        Test_schema,
        <<"{ __type(name: \"User\") { fields { name type { kind name ofType { kind name } } } } }"/utf8>>
    ),
    Data = get_data(Result),
    Type_obj = find_field(Data, <<"__type"/utf8>>),
    Fields = get_list_field(Type_obj, <<"fields"/utf8>>),
    Id_field = gleam@list:find(
        Fields,
        fun(F) -> get_string_field(F, <<"name"/utf8>>) =:= <<"id"/utf8>> end
    ),
    case Id_field of
        {ok, F@1} ->
            Field_type = get_nested_field(F@1, [<<"type"/utf8>>]),
            assert_true(
                get_string_field(Field_type, <<"kind"/utf8>>) =:= <<"NON_NULL"/utf8>>,
                <<"id field type should be NON_NULL"/utf8>>
            ),
            Of_type = get_nested_field(Field_type, [<<"ofType"/utf8>>]),
            assert_true(
                get_string_field(Of_type, <<"kind"/utf8>>) =:= <<"SCALAR"/utf8>>,
                <<"ofType.kind for id field should be SCALAR"/utf8>>
            ),
            assert_true(
                get_string_field(Of_type, <<"name"/utf8>>) =:= <<"ID"/utf8>>,
                <<"ofType.name for id field should be ID"/utf8>>
            );

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected id field on User type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"type_nested_fields_traversal_test"/utf8>>,
                    line => 640})
    end.

-file("test/introspection_test.gleam", 645).
-spec full_introspection_deep_nesting_test() -> nil.
full_introspection_deep_nesting_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"
    {
      __schema {
        types {
          kind
          name
          fields {
            name
            type {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  "/utf8>>,
    Result = mochi@executor:execute_query(Test_schema, Query_str),
    assert_true(
        erlang:element(3, Result) =:= [],
        <<"Full introspection should have no errors"/utf8>>
    ),
    Data = get_data(Result),
    Schema_obj = find_field(Data, <<"__schema"/utf8>>),
    Types_list = get_list_field(Schema_obj, <<"types"/utf8>>),
    Query_type = gleam@list:find(
        Types_list,
        fun(T) -> get_string_field(T, <<"kind"/utf8>>) =:= <<"OBJECT"/utf8>> end
    ),
    case Query_type of
        {ok, _} ->
            nil;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Should have at least one OBJECT type"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"full_introspection_deep_nesting_test"/utf8>>,
                    line => 687})
    end.

-file("test/introspection_test.gleam", 695).
-spec include_deprecated_argument_accepted_test() -> nil.
include_deprecated_argument_accepted_test() ->
    Test_schema = create_test_schema(),
    Query_false = <<"{ __type(name: \"User\") { fields(includeDeprecated: false) { name } } }"/utf8>>,
    Result_false = mochi@executor:execute_query(Test_schema, Query_false),
    case erlang:element(2, Result_false) of
        {some, _} ->
            nil;

        none ->
            nil
    end.

-file("test/introspection_test.gleam", 713).
-spec introspection_fields_without_include_deprecated_test() -> nil.
introspection_fields_without_include_deprecated_test() ->
    Test_schema = create_test_schema(),
    Query_str = <<"{ __type(name: \"User\") { fields { name } } }"/utf8>>,
    Result = mochi@executor:execute_query(Test_schema, Query_str),
    assert_true(
        erlang:element(3, Result) =:= [],
        <<"Query should execute without errors"/utf8>>
    ),
    case erlang:element(2, Result) of
        {some, _} ->
            nil;

        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Fields introspection should return data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"introspection_test"/utf8>>,
                    function => <<"introspection_fields_without_include_deprecated_test"/utf8>>,
                    line => 723})
    end.
