-module(mochi@types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/types.gleam").
-export([to_dynamic/1, record/1, option/1, field/2, object/1, description/2, string/3, string_with_desc/4, id/3, int/3, int_with_desc/4, float/3, bool/3, optional_string/3, optional_int/3, optional_float/3, optional_bool/3, nullable/1, list_string/3, list_int/3, list_float/3, list_bool/3, list_id/3, non_null_list_string/3, non_null_list_int/3, non_null_list_float/3, object_field/4, list_object/4, non_null_field/4, non_null_string/3, non_null_string_with_desc/4, non_null_int/3, non_null_int_with_desc/4, non_null_float/3, non_null_float_with_desc/4, non_null_bool/3, non_null_bool_with_desc/4, field_with_args/6, build_direct/1, encoder/1, build/2, build_with_encoder/2, enum_type/1, enum_description/2, value/2, value_with_desc/3, deprecated_value/2, deprecated_value_with_reason/3, build_enum/1, enum_mapping/2, enum_mapping_with_desc/3, enum_from_mappings/2, enum_from_mappings_with_desc/3, input/1, input_description/2, input_field/4, input_string/3, input_int/3, input_float/3, input_bool/3, input_id/3, input_optional_string/3, input_optional_int/3, input_optional_float/3, input_optional_bool/3, input_field_with_default/5, build_input/1]).
-export_type([type_builder/1, type_field/1, enum_builder/0, enum_value/0, enum_mapping/1, input_builder/0, input_field/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Type definition helpers for Code First GraphQL.\n"
    "\n"
    " This module provides builders for creating GraphQL types from Gleam types\n"
    " with minimal boilerplate, plus helpers for Dynamic conversion.\n"
    "\n"
    " ## Object Type Builder\n"
    "\n"
    " ```gleam\n"
    " let user_type = types.object(\"User\")\n"
    "   |> types.description(\"A user in the system\")\n"
    "   |> types.id(\"id\", fn(u: User) { u.id })\n"
    "   |> types.string(\"name\", fn(u: User) { u.name })\n"
    "   |> types.int(\"age\", fn(u: User) { u.age })\n"
    "   |> types.build(decode_user)\n"
    " ```\n"
    "\n"
    " ## Enum Builder\n"
    "\n"
    " ```gleam\n"
    " let role_enum = types.enum_type(\"Role\")\n"
    "   |> types.value(\"ADMIN\")\n"
    "   |> types.value(\"USER\")\n"
    "   |> types.deprecated_value_with_reason(\"GUEST\", \"Use USER instead\")\n"
    "   |> types.build_enum\n"
    " ```\n"
    "\n"
    " ## Dynamic Conversion Helpers\n"
    "\n"
    " Use these when building DataLoader encoders or custom resolvers:\n"
    "\n"
    " ```gleam\n"
    " fn user_to_dynamic(u: User) -> Dynamic {\n"
    "   types.record([\n"
    "     types.field(\"id\", u.id),\n"
    "     types.field(\"name\", u.name),\n"
    "     #(\"profile\", profile_to_dynamic(u.profile)),\n"
    "     #(\"age\", types.option(u.age)),  // Option(Int) -> null if None\n"
    "   ])\n"
    " }\n"
    " ```\n"
).

-type type_builder(BMC) :: {type_builder,
        binary(),
        gleam@option:option(binary()),
        list(type_field(BMC))}.

-type type_field(BMD) :: {type_field,
        binary(),
        gleam@option:option(binary()),
        mochi@schema:field_type(),
        fun((BMD) -> gleam@dynamic:dynamic_())} |
    {type_field_with_args,
        binary(),
        gleam@option:option(binary()),
        mochi@schema:field_type(),
        list(mochi@schema:argument_definition()),
        fun((BMD, gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), mochi@schema:execution_context()) -> {ok,
                gleam@dynamic:dynamic_()} |
            {error, binary()})}.

-type enum_builder() :: {enum_builder,
        binary(),
        gleam@option:option(binary()),
        list(enum_value())}.

-type enum_value() :: {enum_value,
        binary(),
        gleam@option:option(binary()),
        boolean(),
        gleam@option:option(binary())}.

-type enum_mapping(BME) :: {enum_mapping,
        BME,
        binary(),
        gleam@option:option(binary())}.

-type input_builder() :: {input_builder,
        binary(),
        gleam@option:option(binary()),
        list(input_field())}.

-type input_field() :: {input_field,
        binary(),
        gleam@option:option(binary()),
        mochi@schema:field_type(),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-file("src/mochi/types.gleam", 60).
?DOC(
    " Convert any Gleam value to Dynamic\n"
    " This uses unsafe_coerce under the hood\n"
).
-spec to_dynamic(any()) -> gleam@dynamic:dynamic_().
to_dynamic(Value) ->
    gleam_stdlib:identity(Value).

-file("src/mochi/types.gleam", 80).
?DOC(
    " Build a Dynamic dict from a list of field tuples\n"
    "\n"
    " This is a convenience helper for creating DataLoader encoders.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " fn user_to_dynamic(u: User) -> Dynamic {\n"
    "   types.record([\n"
    "     #(\"id\", types.to_dynamic(u.id)),\n"
    "     #(\"name\", types.to_dynamic(u.name)),\n"
    "     #(\"email\", types.to_dynamic(u.email)),\n"
    "   ])\n"
    " }\n"
    " ```\n"
).
-spec record(list({binary(), gleam@dynamic:dynamic_()})) -> gleam@dynamic:dynamic_().
record(Fields) ->
    gleam_stdlib:identity(maps:from_list(Fields)).

-file("src/mochi/types.gleam", 91).
?DOC(
    " Convert an Option to Dynamic (None becomes Nil/null)\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " #(\"age\", types.option(user.age))\n"
    " ```\n"
).
-spec option(gleam@option:option(any())) -> gleam@dynamic:dynamic_().
option(Opt) ->
    case Opt of
        {some, V} ->
            gleam_stdlib:identity(V);

        none ->
            gleam_stdlib:identity(nil)
    end.

-file("src/mochi/types.gleam", 108).
?DOC(
    " Shorthand: wrap a value in to_dynamic with a field name\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " types.record([\n"
    "   types.field(\"id\", user.id),\n"
    "   types.field(\"name\", user.name),\n"
    " ])\n"
    " ```\n"
).
-spec field(binary(), any()) -> {binary(), gleam@dynamic:dynamic_()}.
field(Name, Value) ->
    {Name, gleam_stdlib:identity(Value)}.

-file("src/mochi/types.gleam", 146).
?DOC(" Create a new type builder\n").
-spec object(binary()) -> type_builder(any()).
object(Name) ->
    {type_builder, Name, none, []}.

-file("src/mochi/types.gleam", 151).
?DOC(" Add description to type\n").
-spec description(type_builder(BMN), binary()) -> type_builder(BMN).
description(Builder, Desc) ->
    {type_builder,
        erlang:element(2, Builder),
        {some, Desc},
        erlang:element(4, Builder)}.

-file("src/mochi/types.gleam", 155).
-spec add_field(
    type_builder(BMQ),
    binary(),
    gleam@option:option(binary()),
    mochi@schema:field_type(),
    fun((BMQ) -> gleam@dynamic:dynamic_())
) -> type_builder(BMQ).
add_field(Builder, Name, Description, Field_type, Extractor) ->
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{type_field, Name, Description, Field_type, Extractor} |
            erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 174).
?DOC(" Add a string field\n").
-spec string(type_builder(BMU), binary(), fun((BMU) -> binary())) -> type_builder(BMU).
string(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:string_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 185).
?DOC(" Add a string field with description\n").
-spec string_with_desc(
    type_builder(BMX),
    binary(),
    binary(),
    fun((BMX) -> binary())
) -> type_builder(BMX).
string_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:string_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 197).
?DOC(" Add an ID field\n").
-spec id(type_builder(BNA), binary(), fun((BNA) -> binary())) -> type_builder(BNA).
id(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:id_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 208).
?DOC(" Add an int field\n").
-spec int(type_builder(BND), binary(), fun((BND) -> integer())) -> type_builder(BND).
int(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:int_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 219).
?DOC(" Add an int field with description\n").
-spec int_with_desc(
    type_builder(BNG),
    binary(),
    binary(),
    fun((BNG) -> integer())
) -> type_builder(BNG).
int_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:int_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 231).
?DOC(" Add a float field\n").
-spec float(type_builder(BNJ), binary(), fun((BNJ) -> float())) -> type_builder(BNJ).
float(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:float_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 242).
?DOC(" Add a boolean field\n").
-spec bool(type_builder(BNM), binary(), fun((BNM) -> boolean())) -> type_builder(BNM).
bool(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:boolean_type(),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 254).
?DOC(
    " Add an optional string field. `None` becomes JSON `null`; `Some(v)` becomes\n"
    " the string value. Use this for nullable GraphQL fields.\n"
).
-spec optional_string(
    type_builder(BNP),
    binary(),
    fun((BNP) -> gleam@option:option(binary()))
) -> type_builder(BNP).
optional_string(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:string_type(),
        fun(A) -> option(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 266).
?DOC(
    " Add an optional int field. `None` becomes JSON `null`; `Some(v)` becomes\n"
    " the int value.\n"
).
-spec optional_int(
    type_builder(BNT),
    binary(),
    fun((BNT) -> gleam@option:option(integer()))
) -> type_builder(BNT).
optional_int(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:int_type(),
        fun(A) -> option(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 278).
?DOC(
    " Add an optional float field. `None` becomes JSON `null`; `Some(v)` becomes\n"
    " the float value.\n"
).
-spec optional_float(
    type_builder(BNX),
    binary(),
    fun((BNX) -> gleam@option:option(float()))
) -> type_builder(BNX).
optional_float(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:float_type(),
        fun(A) -> option(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 290).
?DOC(
    " Add an optional bool field. `None` becomes JSON `null`; `Some(v)` becomes\n"
    " the bool value.\n"
).
-spec optional_bool(
    type_builder(BOB),
    binary(),
    fun((BOB) -> gleam@option:option(boolean()))
) -> type_builder(BOB).
optional_bool(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:boolean_type(),
        fun(A) -> option(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 321).
?DOC(
    " A decoder that returns `None` when the value is nil/null and otherwise runs\n"
    " the inner decoder.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " use email <- decode.optional_field(\"email\", None, types.nullable(decode.string))\n"
    " ```\n"
).
-spec nullable(gleam@dynamic@decode:decoder(BOF)) -> gleam@dynamic@decode:decoder(gleam@option:option(BOF)).
nullable(Inner) ->
    gleam@dynamic@decode:optional(Inner).

-file("src/mochi/types.gleam", 328).
?DOC(" Add a list of strings field\n").
-spec list_string(type_builder(BOJ), binary(), fun((BOJ) -> list(binary()))) -> type_builder(BOJ).
list_string(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:string_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 339).
?DOC(" Add a list of ints field\n").
-spec list_int(type_builder(BON), binary(), fun((BON) -> list(integer()))) -> type_builder(BON).
list_int(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:int_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 349).
-spec list_float(type_builder(BOR), binary(), fun((BOR) -> list(float()))) -> type_builder(BOR).
list_float(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:float_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 359).
-spec list_bool(type_builder(BOV), binary(), fun((BOV) -> list(boolean()))) -> type_builder(BOV).
list_bool(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:boolean_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 369).
-spec list_id(type_builder(BOZ), binary(), fun((BOZ) -> list(binary()))) -> type_builder(BOZ).
list_id(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:id_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 379).
-spec non_null_list_string(
    type_builder(BPD),
    binary(),
    fun((BPD) -> list(binary()))
) -> type_builder(BPD).
non_null_list_string(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(
            mochi@schema:list_type(mochi@schema:string_type())
        ),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 393).
-spec non_null_list_int(
    type_builder(BPH),
    binary(),
    fun((BPH) -> list(integer()))
) -> type_builder(BPH).
non_null_list_int(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:list_type(mochi@schema:int_type())),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 407).
-spec non_null_list_float(
    type_builder(BPL),
    binary(),
    fun((BPL) -> list(float()))
) -> type_builder(BPL).
non_null_list_float(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:list_type(mochi@schema:float_type())),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 422).
?DOC(" Add a related object field\n").
-spec object_field(
    type_builder(BPP),
    binary(),
    binary(),
    fun((BPP) -> gleam@dynamic:dynamic_())
) -> type_builder(BPP).
object_field(Builder, Name, Type_name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:named_type(Type_name),
        Extractor
    ).

-file("src/mochi/types.gleam", 432).
?DOC(" Add a list of related objects field\n").
-spec list_object(
    type_builder(BPS),
    binary(),
    binary(),
    fun((BPS) -> gleam@dynamic:dynamic_())
) -> type_builder(BPS).
list_object(Builder, Name, Type_name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:list_type(mochi@schema:named_type(Type_name)),
        Extractor
    ).

-file("src/mochi/types.gleam", 448).
?DOC(" Add a non-null field\n").
-spec non_null_field(
    type_builder(BPV),
    binary(),
    mochi@schema:field_type(),
    fun((BPV) -> gleam@dynamic:dynamic_())
) -> type_builder(BPV).
non_null_field(Builder, Name, Field_type, Extractor) ->
    add_field(Builder, Name, none, mochi@schema:non_null(Field_type), Extractor).

-file("src/mochi/types.gleam", 462).
?DOC(" Add a non-null string field\n").
-spec non_null_string(type_builder(BPY), binary(), fun((BPY) -> binary())) -> type_builder(BPY).
non_null_string(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:string_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 473).
?DOC(" Add a non-null string field with description\n").
-spec non_null_string_with_desc(
    type_builder(BQB),
    binary(),
    binary(),
    fun((BQB) -> binary())
) -> type_builder(BQB).
non_null_string_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:non_null(mochi@schema:string_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 489).
?DOC(" Add a non-null int field\n").
-spec non_null_int(type_builder(BQE), binary(), fun((BQE) -> integer())) -> type_builder(BQE).
non_null_int(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:int_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 500).
?DOC(" Add a non-null int field with description\n").
-spec non_null_int_with_desc(
    type_builder(BQH),
    binary(),
    binary(),
    fun((BQH) -> integer())
) -> type_builder(BQH).
non_null_int_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:non_null(mochi@schema:int_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 516).
?DOC(" Add a non-null float field\n").
-spec non_null_float(type_builder(BQK), binary(), fun((BQK) -> float())) -> type_builder(BQK).
non_null_float(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:float_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 527).
?DOC(" Add a non-null float field with description\n").
-spec non_null_float_with_desc(
    type_builder(BQN),
    binary(),
    binary(),
    fun((BQN) -> float())
) -> type_builder(BQN).
non_null_float_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:non_null(mochi@schema:float_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 543).
?DOC(" Add a non-null bool field\n").
-spec non_null_bool(type_builder(BQQ), binary(), fun((BQQ) -> boolean())) -> type_builder(BQQ).
non_null_bool(Builder, Name, Extractor) ->
    add_field(
        Builder,
        Name,
        none,
        mochi@schema:non_null(mochi@schema:boolean_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 554).
?DOC(" Add a non-null bool field with description\n").
-spec non_null_bool_with_desc(
    type_builder(BQT),
    binary(),
    binary(),
    fun((BQT) -> boolean())
) -> type_builder(BQT).
non_null_bool_with_desc(Builder, Name, Desc, Extractor) ->
    add_field(
        Builder,
        Name,
        {some, Desc},
        mochi@schema:non_null(mochi@schema:boolean_type()),
        fun(A) -> gleam_stdlib:identity(Extractor(A)) end
    ).

-file("src/mochi/types.gleam", 588).
?DOC(
    " Add a field with arguments and custom resolver\n"
    "\n"
    " ```gleam\n"
    " types.object(\"User\")\n"
    "   |> types.field_with_args(\n"
    "     name: \"posts\",\n"
    "     returns: schema.list_type(schema.named_type(\"Post\")),\n"
    "     args: [schema.arg(\"limit\", schema.int_type())],\n"
    "     desc: \"User's posts with optional limit\",\n"
    "     resolve: fn(user, args, ctx) {\n"
    "       let limit = query.get_optional_int(args, \"limit\")\n"
    "       get_user_posts(user.id, limit)\n"
    "     },\n"
    "   )\n"
    " ```\n"
).
-spec field_with_args(
    type_builder(BQW),
    binary(),
    mochi@schema:field_type(),
    list(mochi@schema:argument_definition()),
    binary(),
    fun((BQW, gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), mochi@schema:execution_context()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> type_builder(BQW).
field_with_args(Builder, Name, Field_type, Args, Description, Resolver) ->
    Field = {type_field_with_args,
        Name,
        {some, Description},
        Field_type,
        Args,
        Resolver},
    {type_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 713).
-spec to_field_def_direct(type_field(any())) -> mochi@schema:field_definition().
to_field_def_direct(F) ->
    case F of
        {type_field, Name, Description, Field_type, Extractor} ->
            Resolver = fun(Info) -> case erlang:element(2, Info) of
                    {some, Parent_dyn} ->
                        {ok, Extractor(gleam_stdlib:identity(Parent_dyn))};

                    none ->
                        {error, <<"No parent value"/utf8>>}
                end end,
            Base = begin
                _pipe = mochi@schema:field_def(Name, Field_type),
                mochi@schema:resolver(_pipe, Resolver)
            end,
            case Description of
                {some, Desc} ->
                    mochi@schema:field_description(Base, Desc);

                none ->
                    Base
            end;

        {type_field_with_args,
            Name@1,
            Description@1,
            Field_type@1,
            Args,
            Field_resolver} ->
            Resolver@1 = fun(Info@1) -> case erlang:element(2, Info@1) of
                    {some, Parent_dyn@1} ->
                        Field_resolver(
                            gleam_stdlib:identity(Parent_dyn@1),
                            erlang:element(3, Info@1),
                            erlang:element(4, Info@1)
                        );

                    none ->
                        {error, <<"No parent value"/utf8>>}
                end end,
            Base@1 = begin
                _pipe@1 = mochi@schema:field_def(Name@1, Field_type@1),
                mochi@schema:resolver(_pipe@1, Resolver@1)
            end,
            With_args = gleam@list:fold(
                Args,
                Base@1,
                fun(Field, Arg) -> mochi@schema:argument(Field, Arg) end
            ),
            case Description@1 of
                {some, Desc@1} ->
                    mochi@schema:field_description(With_args, Desc@1);

                none ->
                    With_args
            end
    end.

-file("src/mochi/types.gleam", 694).
?DOC(
    " Generate an encoder function from a TypeBuilder\n"
    "\n"
    " The encoder uses the same extractors defined for each field,\n"
    " so you don't need to write the same field mappings twice.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let user_builder = types.object(\"User\")\n"
    "   |> types.id(\"id\", fn(u: User) { u.id })\n"
    "   |> types.string(\"name\", fn(u: User) { u.name })\n"
    "\n"
    " let user_type = types.build(user_builder, decode_user)\n"
    " let user_encoder = types.encoder(user_builder)\n"
    "\n"
    " // user_encoder(User(\"1\", \"Alice\")) produces:\n"
    " // {\"id\": \"1\", \"name\": \"Alice\"}\n"
    " ```\n"
    " Build the TypeBuilder into an ObjectType using identity coerce instead of a\n"
    " decoder. This eliminates the encode/decode roundtrip — no Dict is built per\n"
    " object and no Dict lookups are done per field. The encoder is just\n"
    " `to_dynamic` (a BEAM no-op).\n"
    "\n"
    " Safe as long as the resolver returns values of type `a` and the extractors\n"
    " are defined for the same type `a`. Do not use when resolvers return\n"
    " pre-encoded Dicts.\n"
).
-spec build_direct(type_builder(BRM)) -> {mochi@schema:object_type(),
    fun((BRM) -> gleam@dynamic:dynamic_())}.
build_direct(Builder) ->
    Schema_fields = gleam@list:map(
        lists:reverse(erlang:element(4, Builder)),
        fun(F) -> to_field_def_direct(F) end
    ),
    Base_obj = mochi@schema:object(erlang:element(2, Builder)),
    With_desc = case erlang:element(3, Builder) of
        {some, D} ->
            mochi@schema:description(Base_obj, D);

        none ->
            Base_obj
    end,
    Object_type = gleam@list:fold(
        Schema_fields,
        With_desc,
        fun(Obj, Field) -> mochi@schema:field(Obj, Field) end
    ),
    {Object_type, fun gleam_stdlib:identity/1}.

-file("src/mochi/types.gleam", 756).
-spec encoder(type_builder(BRQ)) -> fun((BRQ) -> gleam@dynamic:dynamic_()).
encoder(Builder) ->
    fun(Value) ->
        Field_pairs = gleam@list:filter_map(
            erlang:element(4, Builder),
            fun(F) -> case F of
                    {type_field, Name, _, _, Extractor} ->
                        {ok, {Name, Extractor(Value)}};

                    {type_field_with_args, _, _, _, _, _} ->
                        {error, nil}
                end end
        ),
        gleam_stdlib:identity(maps:from_list(Field_pairs))
    end.

-file("src/mochi/types.gleam", 771).
-spec to_field_def(
    type_field(BRS),
    fun((gleam@dynamic:dynamic_()) -> {ok, BRS} | {error, binary()})
) -> mochi@schema:field_definition().
to_field_def(F, Decoder) ->
    case F of
        {type_field, Name, Description, Field_type, Extractor} ->
            Resolver = fun(Info) -> case erlang:element(2, Info) of
                    {some, Parent_dyn} ->
                        gleam@result:map(Decoder(Parent_dyn), Extractor);

                    none ->
                        {error, <<"No parent value"/utf8>>}
                end end,
            Base = begin
                _pipe = mochi@schema:field_def(Name, Field_type),
                mochi@schema:resolver(_pipe, Resolver)
            end,
            case Description of
                {some, Desc} ->
                    mochi@schema:field_description(Base, Desc);

                none ->
                    Base
            end;

        {type_field_with_args,
            Name@1,
            Description@1,
            Field_type@1,
            Args,
            Field_resolver} ->
            Resolver@1 = fun(Info@1) -> case erlang:element(2, Info@1) of
                    {some, Parent_dyn@1} ->
                        gleam@result:'try'(
                            Decoder(Parent_dyn@1),
                            fun(Parent) ->
                                Field_resolver(
                                    Parent,
                                    erlang:element(3, Info@1),
                                    erlang:element(4, Info@1)
                                )
                            end
                        );

                    none ->
                        {error, <<"No parent value"/utf8>>}
                end end,
            Base@1 = begin
                _pipe@1 = mochi@schema:field_def(Name@1, Field_type@1),
                mochi@schema:resolver(_pipe@1, Resolver@1)
            end,
            With_args = gleam@list:fold(
                Args,
                Base@1,
                fun(Field, Arg) -> mochi@schema:argument(Field, Arg) end
            ),
            case Description@1 of
                {some, Desc@1} ->
                    mochi@schema:field_description(With_args, Desc@1);

                none ->
                    With_args
            end
    end.

-file("src/mochi/types.gleam", 613).
?DOC(" Build the TypeBuilder into an ObjectType with a decoder\n").
-spec build(
    type_builder(BRE),
    fun((gleam@dynamic:dynamic_()) -> {ok, BRE} | {error, binary()})
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

-file("src/mochi/types.gleam", 659).
?DOC(
    " Build the TypeBuilder into an ObjectType and auto-generated encoder\n"
    "\n"
    " This is the recommended way to build types - it generates both the\n"
    " schema type and an encoder function from the same field definitions,\n"
    " eliminating redundant code.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " pub type User {\n"
    "   User(id: String, name: String, age: Int)\n"
    " }\n"
    "\n"
    " let #(user_type, user_encoder) = types.object(\"User\")\n"
    "   |> types.id(\"id\", fn(u: User) { u.id })\n"
    "   |> types.string(\"name\", fn(u: User) { u.name })\n"
    "   |> types.int(\"age\", fn(u: User) { u.age })\n"
    "   |> types.build_with_encoder(decode_user)\n"
    "\n"
    " // Use user_type in schema, user_encoder in resolvers\n"
    " query.query(\n"
    "   name: \"user\",\n"
    "   returns: schema.named_type(\"User\"),\n"
    "   resolve: fn(_ctx) { Ok(User(\"1\", \"Alice\", 30)) },\n"
    "   encode: user_encoder,  // Auto-generated!\n"
    " )\n"
    " ```\n"
).
-spec build_with_encoder(
    type_builder(BRI),
    fun((gleam@dynamic:dynamic_()) -> {ok, BRI} | {error, binary()})
) -> {mochi@schema:object_type(), fun((BRI) -> gleam@dynamic:dynamic_())}.
build_with_encoder(Builder, Decoder) ->
    Object_type = build(Builder, Decoder),
    Encoder_fn = encoder(Builder),
    {Object_type, Encoder_fn}.

-file("src/mochi/types.gleam", 844).
?DOC(" Create a new enum builder\n").
-spec enum_type(binary()) -> enum_builder().
enum_type(Name) ->
    {enum_builder, Name, none, []}.

-file("src/mochi/types.gleam", 849).
?DOC(" Add description to enum\n").
-spec enum_description(enum_builder(), binary()) -> enum_builder().
enum_description(Builder, Desc) ->
    {enum_builder,
        erlang:element(2, Builder),
        {some, Desc},
        erlang:element(4, Builder)}.

-file("src/mochi/types.gleam", 854).
?DOC(" Add an enum value\n").
-spec value(enum_builder(), binary()) -> enum_builder().
value(Builder, Name) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, false, none} | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 862).
?DOC(" Add an enum value with description\n").
-spec value_with_desc(enum_builder(), binary(), binary()) -> enum_builder().
value_with_desc(Builder, Name, Desc) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, {some, Desc}, false, none} |
            erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 874).
?DOC(" Add a deprecated enum value\n").
-spec deprecated_value(enum_builder(), binary()) -> enum_builder().
deprecated_value(Builder, Name) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, true, none} | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 882).
?DOC(" Add a deprecated enum value with reason\n").
-spec deprecated_value_with_reason(enum_builder(), binary(), binary()) -> enum_builder().
deprecated_value_with_reason(Builder, Name, Reason) ->
    {enum_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [{enum_value, Name, none, true, {some, Reason}} |
            erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 894).
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

-file("src/mochi/types.gleam", 925).
?DOC(" Create an enum mapping\n").
-spec enum_mapping(BRW, binary()) -> enum_mapping(BRW).
enum_mapping(Gleam_value, Graphql_name) ->
    {enum_mapping, Gleam_value, Graphql_name, none}.

-file("src/mochi/types.gleam", 934).
?DOC(" Create an enum mapping with description\n").
-spec enum_mapping_with_desc(BRY, binary(), binary()) -> enum_mapping(BRY).
enum_mapping_with_desc(Gleam_value, Graphql_name, Desc) ->
    {enum_mapping, Gleam_value, Graphql_name, {some, Desc}}.

-file("src/mochi/types.gleam", 977).
?DOC(
    " Build an enum type from mappings with bidirectional coercion functions\n"
    "\n"
    " Returns a tuple of:\n"
    " - The EnumType for schema registration\n"
    " - A function to convert Gleam value to GraphQL string\n"
    " - A function to convert GraphQL string to Gleam value\n"
    "\n"
    " ```gleam\n"
    " pub type Status {\n"
    "   Active\n"
    "   Inactive\n"
    "   Pending\n"
    " }\n"
    "\n"
    " let #(enum_type, to_graphql, from_graphql) = types.enum_from_mappings(\n"
    "   \"Status\",\n"
    "   [\n"
    "     types.enum_mapping(Active, \"ACTIVE\"),\n"
    "     types.enum_mapping(Inactive, \"INACTIVE\"),\n"
    "     types.enum_mapping_with_desc(Pending, \"PENDING\", \"Awaiting approval\"),\n"
    "   ],\n"
    " )\n"
    "\n"
    " // Use in schema\n"
    " query.new()\n"
    "   |> query.add_enum(enum_type)\n"
    "\n"
    " // Convert values\n"
    " to_graphql(Active) // -> \"ACTIVE\"\n"
    " from_graphql(\"ACTIVE\") // -> Ok(Active)\n"
    " ```\n"
).
-spec enum_from_mappings(binary(), list(enum_mapping(BSA))) -> {mochi@schema:enum_type(),
    fun((BSA) -> binary()),
    fun((binary()) -> {ok, BSA} | {error, binary()})}.
enum_from_mappings(Name, Mappings) ->
    Enum_values = gleam@list:fold(
        Mappings,
        maps:new(),
        fun(Acc, M) ->
            Value_def = {enum_value_definition,
                erlang:element(3, M),
                erlang:element(4, M),
                gleam_stdlib:identity(erlang:element(3, M)),
                false,
                none},
            gleam@dict:insert(Acc, erlang:element(3, M), Value_def)
        end
    ),
    Enum_type = {enum_type, Name, none, Enum_values},
    To_graphql = fun(Value) ->
        case gleam@list:find(
            Mappings,
            fun(M@1) ->
                gleam_stdlib:identity(erlang:element(2, M@1)) =:= gleam_stdlib:identity(
                    Value
                )
            end
        ) of
            {ok, M@2} ->
                erlang:element(3, M@2);

            {error, _} ->
                <<""/utf8>>
        end
    end,
    From_graphql = fun(Gql_name) ->
        case gleam@list:find(
            Mappings,
            fun(M@3) -> erlang:element(3, M@3) =:= Gql_name end
        ) of
            {ok, M@4} ->
                {ok, erlang:element(2, M@4)};

            {error, _} ->
                {error, <<"Unknown enum value: "/utf8, Gql_name/binary>>}
        end
    end,
    {Enum_type, To_graphql, From_graphql}.

-file("src/mochi/types.gleam", 1020).
?DOC(" Build an enum type from mappings with description\n").
-spec enum_from_mappings_with_desc(binary(), binary(), list(enum_mapping(BSF))) -> {mochi@schema:enum_type(),
    fun((BSF) -> binary()),
    fun((binary()) -> {ok, BSF} | {error, binary()})}.
enum_from_mappings_with_desc(Name, Description, Mappings) ->
    {Enum_type, To_graphql, From_graphql} = enum_from_mappings(Name, Mappings),
    Enum_type_with_desc = {enum_type,
        erlang:element(2, Enum_type),
        {some, Description},
        erlang:element(4, Enum_type)},
    {Enum_type_with_desc, To_graphql, From_graphql}.

-file("src/mochi/types.gleam", 1065).
?DOC(
    " Create a new input type builder\n"
    "\n"
    " ```gleam\n"
    " let create_user_input = types.input(\"CreateUserInput\")\n"
    "   |> types.input_description(\"Input for creating a new user\")\n"
    "   |> types.input_string(\"name\", \"User's full name\")\n"
    "   |> types.input_string(\"email\", \"User's email address\")\n"
    "   |> types.input_optional_int(\"age\", \"User's age\")\n"
    "   |> types.build_input\n"
    " ```\n"
).
-spec input(binary()) -> input_builder().
input(Name) ->
    {input_builder, Name, none, []}.

-file("src/mochi/types.gleam", 1070).
?DOC(" Add description to input type\n").
-spec input_description(input_builder(), binary()) -> input_builder().
input_description(Builder, Desc) ->
    {input_builder,
        erlang:element(2, Builder),
        {some, Desc},
        erlang:element(4, Builder)}.

-file("src/mochi/types.gleam", 1156).
?DOC(" Add a field with custom type to input\n").
-spec input_field(
    input_builder(),
    binary(),
    mochi@schema:field_type(),
    binary()
) -> input_builder().
input_field(Builder, Name, Field_type, Desc) ->
    Field = {input_field, Name, {some, Desc}, Field_type, none},
    {input_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 1075).
?DOC(" Add a required string field to input\n").
-spec input_string(input_builder(), binary(), binary()) -> input_builder().
input_string(Builder, Name, Desc) ->
    input_field(
        Builder,
        Name,
        mochi@schema:non_null(mochi@schema:string_type()),
        Desc
    ).

-file("src/mochi/types.gleam", 1084).
?DOC(" Add a required int field to input\n").
-spec input_int(input_builder(), binary(), binary()) -> input_builder().
input_int(Builder, Name, Desc) ->
    input_field(
        Builder,
        Name,
        mochi@schema:non_null(mochi@schema:int_type()),
        Desc
    ).

-file("src/mochi/types.gleam", 1093).
?DOC(" Add a required float field to input\n").
-spec input_float(input_builder(), binary(), binary()) -> input_builder().
input_float(Builder, Name, Desc) ->
    input_field(
        Builder,
        Name,
        mochi@schema:non_null(mochi@schema:float_type()),
        Desc
    ).

-file("src/mochi/types.gleam", 1102).
?DOC(" Add a required bool field to input\n").
-spec input_bool(input_builder(), binary(), binary()) -> input_builder().
input_bool(Builder, Name, Desc) ->
    input_field(
        Builder,
        Name,
        mochi@schema:non_null(mochi@schema:boolean_type()),
        Desc
    ).

-file("src/mochi/types.gleam", 1111).
?DOC(" Add a required ID field to input\n").
-spec input_id(input_builder(), binary(), binary()) -> input_builder().
input_id(Builder, Name, Desc) ->
    input_field(
        Builder,
        Name,
        mochi@schema:non_null(mochi@schema:id_type()),
        Desc
    ).

-file("src/mochi/types.gleam", 1120).
?DOC(" Add an optional string field to input\n").
-spec input_optional_string(input_builder(), binary(), binary()) -> input_builder().
input_optional_string(Builder, Name, Desc) ->
    input_field(Builder, Name, mochi@schema:string_type(), Desc).

-file("src/mochi/types.gleam", 1129).
?DOC(" Add an optional int field to input\n").
-spec input_optional_int(input_builder(), binary(), binary()) -> input_builder().
input_optional_int(Builder, Name, Desc) ->
    input_field(Builder, Name, mochi@schema:int_type(), Desc).

-file("src/mochi/types.gleam", 1138).
?DOC(" Add an optional float field to input\n").
-spec input_optional_float(input_builder(), binary(), binary()) -> input_builder().
input_optional_float(Builder, Name, Desc) ->
    input_field(Builder, Name, mochi@schema:float_type(), Desc).

-file("src/mochi/types.gleam", 1147).
?DOC(" Add an optional bool field to input\n").
-spec input_optional_bool(input_builder(), binary(), binary()) -> input_builder().
input_optional_bool(Builder, Name, Desc) ->
    input_field(Builder, Name, mochi@schema:boolean_type(), Desc).

-file("src/mochi/types.gleam", 1173).
?DOC(" Add a field with custom type and default value to input\n").
-spec input_field_with_default(
    input_builder(),
    binary(),
    mochi@schema:field_type(),
    gleam@dynamic:dynamic_(),
    binary()
) -> input_builder().
input_field_with_default(Builder, Name, Field_type, Default, Desc) ->
    Field = {input_field, Name, {some, Desc}, Field_type, {some, Default}},
    {input_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [Field | erlang:element(4, Builder)]}.

-file("src/mochi/types.gleam", 1191).
?DOC(" Build the input type into a schema InputObjectType\n").
-spec build_input(input_builder()) -> mochi@schema:input_object_type().
build_input(Builder) ->
    Schema_fields = gleam@list:fold(
        lists:reverse(erlang:element(4, Builder)),
        maps:new(),
        fun(Acc, F) ->
            Field_def = {input_field_definition,
                erlang:element(2, F),
                erlang:element(3, F),
                erlang:element(4, F),
                erlang:element(5, F)},
            gleam@dict:insert(Acc, erlang:element(2, F), Field_def)
        end
    ),
    {input_object_type,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        Schema_fields}.
