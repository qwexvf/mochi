-module(mochi@schema_printer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/schema_printer.gleam").
-export([print_type_definition/1, print_schema/1, demo_schema_printing/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi/schema_printer.gleam", 234).
?DOC(" Print field type (handles NonNull, List, Named)\n").
-spec print_field_type(mochi@schema:field_type()) -> binary().
print_field_type(Field_type) ->
    case Field_type of
        {named, Name} ->
            Name;

        {non_null, Inner} ->
            <<(print_field_type(Inner))/binary, "!"/utf8>>;

        {list, Inner@1} ->
            <<<<"["/utf8, (print_field_type(Inner@1))/binary>>/binary,
                "]"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 210).
?DOC(" Print argument definition\n").
-spec print_argument_definition(mochi@schema:argument_definition()) -> binary().
print_argument_definition(Arg) ->
    Base = <<<<(erlang:element(2, Arg))/binary, ": "/utf8>>/binary,
        (print_field_type(erlang:element(4, Arg)))/binary>>,
    case erlang:element(5, Arg) of
        {some, _} ->
            <<<<Base/binary, " = "/utf8>>/binary, "defaultValue"/utf8>>;

        none ->
            Base
    end.

-file("src/mochi/schema_printer.gleam", 243).
?DOC(" Print schema definition if it has custom root types\n").
-spec print_schema_definition(mochi@schema:schema()) -> binary().
print_schema_definition(Schema_def) ->
    Parts = [],
    Parts@1 = case erlang:element(2, Schema_def) of
        {some, Query} when erlang:element(2, Query) =/= <<"Query"/utf8>> ->
            [<<"query: "/utf8, (erlang:element(2, Query))/binary>> | Parts];

        _ ->
            Parts
    end,
    Parts@2 = case erlang:element(3, Schema_def) of
        {some, Mutation} when erlang:element(2, Mutation) =/= <<"Mutation"/utf8>> ->
            [<<"mutation: "/utf8, (erlang:element(2, Mutation))/binary>> |
                Parts@1];

        _ ->
            Parts@1
    end,
    Parts@3 = case erlang:element(4, Schema_def) of
        {some, Subscription} when erlang:element(2, Subscription) =/= <<"Subscription"/utf8>> ->
            [<<"subscription: "/utf8, (erlang:element(2, Subscription))/binary>> |
                Parts@2];

        _ ->
            Parts@2
    end,
    case Parts@3 of
        [] ->
            <<""/utf8>>;

        _ ->
            <<<<"schema {\n  "/utf8,
                    (gleam@string:join(lists:reverse(Parts@3), <<"\n  "/utf8>>))/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 274).
?DOC(" Check if schema has custom root type names\n").
-spec has_custom_root_types(mochi@schema:schema()) -> boolean().
has_custom_root_types(Schema_def) ->
    Query_custom = case erlang:element(2, Schema_def) of
        {some, Query} ->
            erlang:element(2, Query) /= <<"Query"/utf8>>;

        none ->
            false
    end,
    Mutation_custom = case erlang:element(3, Schema_def) of
        {some, Mutation} ->
            erlang:element(2, Mutation) /= <<"Mutation"/utf8>>;

        none ->
            false
    end,
    Subscription_custom = case erlang:element(4, Schema_def) of
        {some, Subscription} ->
            erlang:element(2, Subscription) /= <<"Subscription"/utf8>>;

        none ->
            false
    end,
    (Query_custom orelse Mutation_custom) orelse Subscription_custom.

-file("src/mochi/schema_printer.gleam", 294).
?DOC(" Print description as a comment\n").
-spec print_description(binary()) -> binary().
print_description(Desc) ->
    <<<<"\"\"\""/utf8, Desc/binary>>/binary, "\"\"\""/utf8>>.

-file("src/mochi/schema_printer.gleam", 73).
?DOC(" Print scalar type: scalar DateTime\n").
-spec print_scalar_type(mochi@schema:scalar_type()) -> binary().
print_scalar_type(Scalar) ->
    Result = <<"scalar "/utf8, (erlang:element(2, Scalar))/binary>>,
    case erlang:element(3, Scalar) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Result/binary>>;

        none ->
            Result
    end.

-file("src/mochi/schema_printer.gleam", 122).
?DOC(" Print union type: union SearchResult = User | Post\n").
-spec print_union_type(mochi@schema:union_type()) -> binary().
print_union_type(Union) ->
    Header = <<"union "/utf8, (erlang:element(2, Union))/binary>>,
    Header@1 = case erlang:element(3, Union) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Header/binary>>;

        none ->
            Header
    end,
    Members = begin
        _pipe = gleam@list:map(
            erlang:element(4, Union),
            fun(T) -> erlang:element(2, T) end
        ),
        gleam@string:join(_pipe, <<" | "/utf8>>)
    end,
    case Members of
        <<""/utf8>> ->
            Header@1;

        _ ->
            <<<<Header@1/binary, " = "/utf8>>/binary, Members/binary>>
    end.

-file("src/mochi/schema_printer.gleam", 299).
?DOC(" Print description inline\n").
-spec print_description_inline(binary()) -> binary().
print_description_inline(Desc) ->
    <<"# "/utf8, Desc/binary>>.

-file("src/mochi/schema_printer.gleam", 161).
?DOC(" Print field definition within a type\n").
-spec print_field_definition(mochi@schema:field_definition(), binary()) -> binary().
print_field_definition(Field, Indent) ->
    Base = <<Indent/binary, (erlang:element(2, Field))/binary>>,
    Base@1 = case maps:size(erlang:element(5, Field)) of
        0 ->
            Base;

        _ ->
            Args = begin
                _pipe = maps:to_list(erlang:element(5, Field)),
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(Entry) ->
                        print_argument_definition(erlang:element(2, Entry))
                    end
                ),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end,
            <<<<<<Base/binary, "("/utf8>>/binary, Args/binary>>/binary,
                ")"/utf8>>
    end,
    Base@2 = <<<<Base@1/binary, ": "/utf8>>/binary,
        (print_field_type(erlang:element(4, Field)))/binary>>,
    case erlang:element(3, Field) of
        {some, Desc} ->
            <<<<(print_description_inline(Desc))/binary, "\n"/utf8>>/binary,
                Base@2/binary>>;

        none ->
            Base@2
    end.

-file("src/mochi/schema_printer.gleam", 45).
?DOC(" Print object type: type User { ... }\n").
-spec print_object_type(mochi@schema:object_type()) -> binary().
print_object_type(Obj) ->
    Header = <<"type "/utf8, (erlang:element(2, Obj))/binary>>,
    Header@1 = case erlang:element(3, Obj) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Header/binary>>;

        none ->
            Header
    end,
    Header@2 = case erlang:length(erlang:element(5, Obj)) of
        0 ->
            Header@1;

        _ ->
            <<<<Header@1/binary, " implements "/utf8>>/binary,
                (gleam@string:join(
                    gleam@list:map(
                        erlang:element(5, Obj),
                        fun(I) -> erlang:element(2, I) end
                    ),
                    <<" & "/utf8>>
                ))/binary>>
    end,
    Fields = begin
        _pipe = maps:to_list(erlang:element(4, Obj)),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) ->
                print_field_definition(erlang:element(2, Entry), <<"  "/utf8>>)
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    case Fields of
        <<""/utf8>> ->
            Header@2;

        _ ->
            <<<<<<Header@2/binary, " {\n"/utf8>>/binary, Fields/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 102).
?DOC(" Print interface type: interface Node { ... }\n").
-spec print_interface_type(mochi@schema:interface_type()) -> binary().
print_interface_type(Interface) ->
    Header = <<"interface "/utf8, (erlang:element(2, Interface))/binary>>,
    Header@1 = case erlang:element(3, Interface) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Header/binary>>;

        none ->
            Header
    end,
    Fields = begin
        _pipe = maps:to_list(erlang:element(4, Interface)),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) ->
                print_field_definition(erlang:element(2, Entry), <<"  "/utf8>>)
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    case Fields of
        <<""/utf8>> ->
            Header@1;

        _ ->
            <<<<<<Header@1/binary, " {\n"/utf8>>/binary, Fields/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 190).
?DOC(" Print input field definition\n").
-spec print_input_field_definition(
    mochi@schema:input_field_definition(),
    binary()
) -> binary().
print_input_field_definition(Field, Indent) ->
    Base = <<<<<<Indent/binary, (erlang:element(2, Field))/binary>>/binary,
            ": "/utf8>>/binary,
        (print_field_type(erlang:element(4, Field)))/binary>>,
    Base@1 = case erlang:element(5, Field) of
        {some, _} ->
            <<<<Base/binary, " = "/utf8>>/binary, "defaultValue"/utf8>>;

        none ->
            Base
    end,
    case erlang:element(3, Field) of
        {some, Desc} ->
            <<<<(print_description_inline(Desc))/binary, "\n"/utf8>>/binary,
                Base@1/binary>>;

        none ->
            Base@1
    end.

-file("src/mochi/schema_printer.gleam", 141).
?DOC(" Print input object type: input CreateUserInput { ... }\n").
-spec print_input_object_type(mochi@schema:input_object_type()) -> binary().
print_input_object_type(Input) ->
    Header = <<"input "/utf8, (erlang:element(2, Input))/binary>>,
    Header@1 = case erlang:element(3, Input) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Header/binary>>;

        none ->
            Header
    end,
    Fields = begin
        _pipe = maps:to_list(erlang:element(4, Input)),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) ->
                print_input_field_definition(
                    erlang:element(2, Entry),
                    <<"  "/utf8>>
                )
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    case Fields of
        <<""/utf8>> ->
            Header@1;

        _ ->
            <<<<<<Header@1/binary, " {\n"/utf8>>/binary, Fields/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 221).
?DOC(" Print enum value\n").
-spec print_enum_value(mochi@schema:enum_value_definition(), binary()) -> binary().
print_enum_value(Enum_value, Indent) ->
    Base = <<Indent/binary, (erlang:element(2, Enum_value))/binary>>,
    case erlang:element(3, Enum_value) of
        {some, Desc} ->
            <<<<(print_description_inline(Desc))/binary, "\n"/utf8>>/binary,
                Base/binary>>;

        none ->
            Base
    end.

-file("src/mochi/schema_printer.gleam", 82).
?DOC(" Print enum type: enum Status { ACTIVE INACTIVE }\n").
-spec print_enum_type(mochi@schema:enum_type()) -> binary().
print_enum_type(Enum) ->
    Header = <<"enum "/utf8, (erlang:element(2, Enum))/binary>>,
    Header@1 = case erlang:element(3, Enum) of
        {some, Desc} ->
            <<<<(print_description(Desc))/binary, "\n"/utf8>>/binary,
                Header/binary>>;

        none ->
            Header
    end,
    Values = begin
        _pipe = maps:to_list(erlang:element(4, Enum)),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) ->
                print_enum_value(erlang:element(2, Entry), <<"  "/utf8>>)
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    case Values of
        <<""/utf8>> ->
            Header@1;

        _ ->
            <<<<<<Header@1/binary, " {\n"/utf8>>/binary, Values/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/mochi/schema_printer.gleam", 33).
?DOC(" Print individual type definition\n").
-spec print_type_definition(mochi@schema:type_definition()) -> binary().
print_type_definition(Type_def) ->
    case Type_def of
        {object_type_def, Obj} ->
            print_object_type(Obj);

        {scalar_type_def, Scalar} ->
            print_scalar_type(Scalar);

        {enum_type_def, Enum} ->
            print_enum_type(Enum);

        {interface_type_def, Interface} ->
            print_interface_type(Interface);

        {union_type_def, Union} ->
            print_union_type(Union);

        {input_object_type_def, Input} ->
            print_input_object_type(Input)
    end.

-file("src/mochi/schema_printer.gleam", 12).
?DOC(" Print a schema to SDL format\n").
-spec print_schema(mochi@schema:schema()) -> binary().
print_schema(Schema_def) ->
    Parts = [],
    Parts@1 = case has_custom_root_types(Schema_def) of
        true ->
            [print_schema_definition(Schema_def) | Parts];

        false ->
            Parts
    end,
    Type_parts = begin
        _pipe = maps:to_list(erlang:element(5, Schema_def)),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) -> print_type_definition(erlang:element(2, Entry)) end
        ),
        gleam@list:filter(_pipe@1, fun(S) -> S /= <<""/utf8>> end)
    end,
    All_parts = lists:append(Parts@1, Type_parts),
    gleam@string:join(All_parts, <<"\n\n"/utf8>>).

-file("src/mochi/schema_printer.gleam", 304).
?DOC(" Demo function to show schema printing capabilities\n").
-spec demo_schema_printing() -> nil.
demo_schema_printing() ->
    gleam_stdlib:println(<<"=== GeQL Schema Printer Demo ==="/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
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
                    mochi@schema:string_type()
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
    Post_type = begin
        _pipe@7 = mochi@schema:object(<<"Post"/utf8>>),
        _pipe@8 = mochi@schema:description(_pipe@7, <<"A blog post"/utf8>>),
        _pipe@9 = mochi@schema:field(
            _pipe@8,
            mochi@schema:field_def(
                <<"id"/utf8>>,
                mochi@schema:non_null(mochi@schema:id_type())
            )
        ),
        _pipe@10 = mochi@schema:field(
            _pipe@9,
            mochi@schema:field_def(
                <<"title"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )
        ),
        mochi@schema:field(
            _pipe@10,
            mochi@schema:field_def(
                <<"content"/utf8>>,
                mochi@schema:string_type()
            )
        )
    end,
    Query_type = begin
        _pipe@11 = mochi@schema:object(<<"Query"/utf8>>),
        _pipe@13 = mochi@schema:field(
            _pipe@11,
            begin
                _pipe@12 = mochi@schema:field_def(
                    <<"user"/utf8>>,
                    mochi@schema:named_type(<<"User"/utf8>>)
                ),
                mochi@schema:argument(
                    _pipe@12,
                    mochi@schema:arg(
                        <<"id"/utf8>>,
                        mochi@schema:non_null(mochi@schema:id_type())
                    )
                )
            end
        ),
        mochi@schema:field(
            _pipe@13,
            mochi@schema:field_def(
                <<"posts"/utf8>>,
                mochi@schema:list_type(
                    mochi@schema:non_null(
                        mochi@schema:named_type(<<"Post"/utf8>>)
                    )
                )
            )
        )
    end,
    Complete_schema = begin
        _pipe@14 = mochi@schema:schema(),
        _pipe@15 = mochi@schema:'query'(_pipe@14, Query_type),
        _pipe@16 = mochi@schema:add_type(_pipe@15, {object_type_def, User_type}),
        mochi@schema:add_type(_pipe@16, {object_type_def, Post_type})
    end,
    gleam_stdlib:println(<<"📄 Generated SDL:"/utf8>>),
    gleam_stdlib:println(<<"================="/utf8>>),
    gleam_stdlib:println(print_schema(Complete_schema)),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"✨ Schema printing complete!"/utf8>>).
