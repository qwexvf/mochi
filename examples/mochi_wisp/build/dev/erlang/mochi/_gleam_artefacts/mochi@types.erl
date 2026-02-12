-module(mochi@types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/types.gleam").
-export([to_dynamic/1, object/1, description/2, string/3, string_with_desc/4, id/3, int/3, int_with_desc/4, float/3, bool/3, optional_string/3, optional_int/3, list_string/3, list_int/3, object_field/4, list_object/4, non_null_field/4, build/2, enum_type/1, enum_description/2, value/2, value_with_desc/3, deprecated_value/2, deprecated_value_with_reason/3, build_enum/1]).
-export_type([type_builder/1, type_field/1, enum_builder/0, enum_value/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type type_builder(ACAX) :: {type_builder,
        binary(),
        gleam@option:option(binary()),
        list(type_field(ACAX))}.

-type type_field(ACAY) :: {type_field,
        binary(),
        gleam@option:option(binary()),
        mochi@schema:field_type(),
        fun((ACAY) -> gleam@dynamic:dynamic_())}.

-type enum_builder() :: {enum_builder,
        binary(),
        gleam@option:option(binary()),
        list(enum_value())}.

-type enum_value() :: {enum_value,
        binary(),
        gleam@option:option(binary()),
        boolean(),
        gleam@option:option(binary())}.

-file("src/mochi/types.gleam", 21).
?DOC(
    " Convert any Gleam value to Dynamic\n"
    " This uses unsafe_coerce under the hood\n"
).
-spec to_dynamic(any()) -> gleam@dynamic:dynamic_().
to_dynamic(Value) ->
    gleam_stdlib:identity(Value).

-file("src/mochi/types.gleam", 47).
?DOC(" Create a new type builder\n").
-spec object(binary()) -> type_builder(any()).
object(Name) ->
    {type_builder, Name, none, []}.

-file("src/mochi/types.gleam", 52).
?DOC(" Add description to type\n").
-spec description(type_builder(ACBC), binary()) -> type_builder(ACBC).
description(Builder, Desc) ->
    {type_builder,
        erlang:element(2, Builder),
        {some, Desc},
        erlang:element(4, Builder)}.

-file("src/mochi/types.gleam", 57).
?DOC(" Add a string field\n").
-spec string(type_builder(ACBF), binary(), fun((ACBF) -> binary())) -> type_builder(ACBF).
string(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:string_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 73).
?DOC(" Add a string field with description\n").
-spec string_with_desc(
    type_builder(ACBI),
    binary(),
    binary(),
    fun((ACBI) -> binary())
) -> type_builder(ACBI).
string_with_desc(Builder, Name, Desc, Extractor) ->
    Field = {type_field,
        Name,
        {some, Desc},
        mochi@schema:string_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 90).
?DOC(" Add an ID field\n").
-spec id(type_builder(ACBL), binary(), fun((ACBL) -> binary())) -> type_builder(ACBL).
id(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:id_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 106).
?DOC(" Add an int field\n").
-spec int(type_builder(ACBO), binary(), fun((ACBO) -> integer())) -> type_builder(ACBO).
int(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:int_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 122).
?DOC(" Add an int field with description\n").
-spec int_with_desc(
    type_builder(ACBR),
    binary(),
    binary(),
    fun((ACBR) -> integer())
) -> type_builder(ACBR).
int_with_desc(Builder, Name, Desc, Extractor) ->
    Field = {type_field,
        Name,
        {some, Desc},
        mochi@schema:int_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 139).
?DOC(" Add a float field\n").
-spec float(type_builder(ACBU), binary(), fun((ACBU) -> float())) -> type_builder(ACBU).
float(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:float_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 155).
?DOC(" Add a boolean field\n").
-spec bool(type_builder(ACBX), binary(), fun((ACBX) -> boolean())) -> type_builder(ACBX).
bool(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:boolean_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 171).
?DOC(" Add an optional string field\n").
-spec optional_string(
    type_builder(ACCA),
    binary(),
    fun((ACCA) -> gleam@option:option(binary()))
) -> type_builder(ACCA).
optional_string(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:string_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 187).
?DOC(" Add an optional int field\n").
-spec optional_int(
    type_builder(ACCE),
    binary(),
    fun((ACCE) -> gleam@option:option(integer()))
) -> type_builder(ACCE).
optional_int(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:int_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 203).
?DOC(" Add a list of strings field\n").
-spec list_string(type_builder(ACCI), binary(), fun((ACCI) -> list(binary()))) -> type_builder(ACCI).
list_string(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:string_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 219).
?DOC(" Add a list of ints field\n").
-spec list_int(type_builder(ACCM), binary(), fun((ACCM) -> list(integer()))) -> type_builder(ACCM).
list_int(Builder, Name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:int_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 235).
?DOC(" Add a related object field\n").
-spec object_field(
    type_builder(ACCQ),
    binary(),
    binary(),
    fun((ACCQ) -> gleam@dynamic:dynamic_())
) -> type_builder(ACCQ).
object_field(Builder, Name, Type_name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:named_type(Type_name),
        Extractor},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 252).
?DOC(" Add a list of related objects field\n").
-spec list_object(
    type_builder(ACCT),
    binary(),
    binary(),
    fun((ACCT) -> gleam@dynamic:dynamic_())
) -> type_builder(ACCT).
list_object(Builder, Name, Type_name, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:named_type(Type_name)),
        Extractor},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 269).
?DOC(" Add a non-null field\n").
-spec non_null_field(
    type_builder(ACCW),
    binary(),
    mochi@schema:field_type(),
    fun((ACCW) -> gleam@dynamic:dynamic_())
) -> type_builder(ACCW).
non_null_field(Builder, Name, Field_type, Extractor) ->
    Field = {type_field,
        Name,
        none,
        mochi@schema:non_null(Field_type),
        Extractor},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 309).
-spec to_field_def(
    type_field(ACDD),
    fun((gleam@dynamic:dynamic_()) -> {ok, ACDD} | {error, binary()})
) -> mochi@schema:field_definition().
to_field_def(F, Decoder) ->
    Resolver = fun(Info) -> case erlang:element(2, Info) of
            {some, Parent_dyn} ->
                case Decoder(Parent_dyn) of
                    {ok, Parent} ->
                        {ok, (erlang:element(5, F))(Parent)};

                    {error, E} ->
                        {error, E}
                end;

            none ->
                {error, <<"No parent value"/utf8>>}
        end end,
    Base = begin
        _pipe = mochi@schema:field_def(
            erlang:element(2, F),
            erlang:element(4, F)
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    case erlang:element(3, F) of
        {some, Desc} ->
            mochi@schema:field_description(Base, Desc);

        none ->
            Base
    end.

-file("src/mochi/types.gleam", 290).
?DOC(" Build the TypeBuilder into an ObjectType with a decoder\n").
-spec build(
    type_builder(ACCZ),
    fun((gleam@dynamic:dynamic_()) -> {ok, ACCZ} | {error, binary()})
) -> mochi@schema:object_type().
build(Builder, Decoder) ->
    Schema_fields = gleam@list:map(
        lists:reverse(erlang:element(4, Builder)),
        fun(F) -> to_field_def(F, Decoder) end
    ),
    Base_obj = mochi@schema:object(erlang:element(2, Builder)),
    With_desc = case erlang:element(3, Builder) of
        {some, D} ->
            mochi@schema:description(Base_obj, D);

        none ->
            Base_obj
    end,
    gleam@list:fold(
        Schema_fields,
        With_desc,
        fun(Obj, Field) -> mochi@schema:field(Obj, Field) end
    ).

-file("src/mochi/types.gleam", 358).
?DOC(" Create a new enum builder\n").
-spec enum_type(binary()) -> enum_builder().
enum_type(Name) ->
    {enum_builder, Name, none, []}.

-file("src/mochi/types.gleam", 363).
?DOC(" Add description to enum\n").
-spec enum_description(enum_builder(), binary()) -> enum_builder().
enum_description(Builder, Desc) ->
    {enum_builder,
        erlang:element(2, Builder),
        {some, Desc},
        erlang:element(4, Builder)}.

-file("src/mochi/types.gleam", 368).
?DOC(" Add an enum value\n").
-spec value(enum_builder(), binary()) -> enum_builder().
value(Builder, Name) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, false, none} | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 376).
?DOC(" Add an enum value with description\n").
-spec value_with_desc(enum_builder(), binary(), binary()) -> enum_builder().
value_with_desc(Builder, Name, Desc) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, {some, Desc}, false, none} |
            erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 388).
?DOC(" Add a deprecated enum value\n").
-spec deprecated_value(enum_builder(), binary()) -> enum_builder().
deprecated_value(Builder, Name) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, true, none} | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 396).
?DOC(" Add a deprecated enum value with reason\n").
-spec deprecated_value_with_reason(enum_builder(), binary(), binary()) -> enum_builder().
deprecated_value_with_reason(Builder, Name, Reason) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, true, {some, Reason}} |
            erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 408).
?DOC(" Build the enum type\n").
-spec build_enum(enum_builder()) -> mochi@schema:enum_type().
build_enum(Builder) ->
    Enum_values = gleam@list:fold(
        lists:reverse(erlang:element(4, Builder)),
        maps:new(),
        fun(Acc, V) ->
            Value_def = {enum_value_definition,
                erlang:element(2, V),
                erlang:element(3, V),
                gleam_stdlib:identity(erlang:element(2, V)),
                erlang:element(4, V),
                erlang:element(5, V)},
            gleam@dict:insert(Acc, erlang:element(2, V), Value_def)
        end
    ),
    {enum_type,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        Enum_values}.
