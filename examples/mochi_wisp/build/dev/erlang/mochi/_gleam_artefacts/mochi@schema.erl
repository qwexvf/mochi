-module(mochi@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/schema.gleam").
-export([execution_context/1, add_data_loader/3, get_data_loader/2, update_data_loader/3, schema/0, 'query'/2, mutation/2, subscription/2, add_type/2, object/1, description/2, field/2, implements/2, field_def/2, deprecate/2, deprecate_field/1, field_description/2, argument/2, resolver/2, arg/2, arg_description/2, default_value/2, string_type/0, int_type/0, float_type/0, boolean_type/0, id_type/0, named_type/1, non_null/1, list_type/1, scalar/1, scalar_description/2, serialize/2, parse_value/2, parse_literal/2, string_scalar/0, int_scalar/0, float_scalar/0, boolean_scalar/0, id_scalar/0, interface/1, interface_description/2, interface_field/2, interface_resolve_type/2, union/1, union_description/2, union_member/2, union_resolve_type/2, add_directive/2, directive/2, directive_description/2, directive_argument/2, directive_repeatable/1, directive_handler/2, skip_directive/0, include_directive/0, deprecated_directive/0, builtin_directives/0, directive_location_to_string/1]).
-export_type([schema/0, directive_definition/0, directive_location/0, type_definition/0, object_type/0, field_definition/0, field_type/0, argument_definition/0, scalar_type/0, enum_type/0, enum_value_definition/0, interface_type/0, union_type/0, input_object_type/0, input_field_definition/0, execution_context/0, resolver_info/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type schema() :: {schema,
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@dict:dict(binary(), type_definition()),
        gleam@dict:dict(binary(), directive_definition())}.

-type directive_definition() :: {directive_definition,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), argument_definition()),
        list(directive_location()),
        boolean(),
        gleam@option:option(fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), gleam@dynamic:dynamic_()) -> {ok,
                gleam@dynamic:dynamic_()} |
            {error, binary()}))}.

-type directive_location() :: query_location |
    mutation_location |
    subscription_location |
    field_location |
    fragment_definition_location |
    fragment_spread_location |
    inline_fragment_location |
    variable_definition_location |
    schema_location |
    scalar_location |
    object_location |
    field_definition_location |
    argument_definition_location |
    interface_location |
    union_location |
    enum_location |
    enum_value_location |
    input_object_location |
    input_field_definition_location.

-type type_definition() :: {object_type_def, object_type()} |
    {scalar_type_def, scalar_type()} |
    {enum_type_def, enum_type()} |
    {interface_type_def, interface_type()} |
    {union_type_def, union_type()} |
    {input_object_type_def, input_object_type()}.

-type object_type() :: {object_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), field_definition()),
        list(interface_type())}.

-type field_definition() :: {field_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@dict:dict(binary(), argument_definition()),
        gleam@option:option(fun((resolver_info()) -> {ok,
                gleam@dynamic:dynamic_()} |
            {error, binary()})),
        boolean(),
        gleam@option:option(binary())}.

-type field_type() :: {non_null, field_type()} |
    {list, field_type()} |
    {named, binary()}.

-type argument_definition() :: {argument_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-type scalar_type() :: {scalar_type,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()}),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()}),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()})}.

-type enum_type() :: {enum_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), enum_value_definition())}.

-type enum_value_definition() :: {enum_value_definition,
        binary(),
        gleam@option:option(binary()),
        gleam@dynamic:dynamic_(),
        boolean(),
        gleam@option:option(binary())}.

-type interface_type() :: {interface_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), field_definition()),
        gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, binary()} |
            {error, binary()}))}.

-type union_type() :: {union_type,
        binary(),
        gleam@option:option(binary()),
        list(object_type()),
        gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, binary()} |
            {error, binary()}))}.

-type input_object_type() :: {input_object_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), input_field_definition())}.

-type input_field_definition() :: {input_field_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-type execution_context() :: {execution_context,
        gleam@dynamic:dynamic_(),
        gleam@dict:dict(binary(), mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()))}.

-type resolver_info() :: {resolver_info,
        gleam@option:option(gleam@dynamic:dynamic_()),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        execution_context(),
        gleam@dynamic:dynamic_()}.

-file("src/mochi/schema.gleam", 198).
?DOC(" Create a new execution context\n").
-spec execution_context(gleam@dynamic:dynamic_()) -> execution_context().
execution_context(User_context) ->
    {execution_context, User_context, maps:new()}.

-file("src/mochi/schema.gleam", 203).
?DOC(" Add a DataLoader to the execution context\n").
-spec add_data_loader(
    execution_context(),
    binary(),
    mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
add_data_loader(Context, Name, Loader) ->
    {execution_context,
        erlang:element(2, Context),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader)}.

-file("src/mochi/schema.gleam", 215).
?DOC(" Get a DataLoader from the execution context\n").
-spec get_data_loader(execution_context(), binary()) -> {ok,
        mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())} |
    {error, binary()}.
get_data_loader(Context, Name) ->
    case gleam_stdlib:map_get(erlang:element(3, Context), Name) of
        {ok, Loader} ->
            {ok, Loader};

        {error, _} ->
            {error,
                <<<<"DataLoader '"/utf8, Name/binary>>/binary,
                    "' not found in execution context"/utf8>>}
    end.

-file("src/mochi/schema.gleam", 227).
?DOC(" Update a DataLoader in the execution context\n").
-spec update_data_loader(
    execution_context(),
    binary(),
    mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
update_data_loader(Context, Name, Loader) ->
    {execution_context,
        erlang:element(2, Context),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader)}.

-file("src/mochi/schema.gleam", 239).
-spec schema() -> schema().
schema() ->
    {schema, none, none, none, maps:new(), maps:new()}.

-file("src/mochi/schema.gleam", 249).
-spec 'query'(schema(), object_type()) -> schema().
'query'(Schema, Query_type) ->
    {schema,
        {some, Query_type},
        erlang:element(3, Schema),
        erlang:element(4, Schema),
        erlang:element(5, Schema),
        erlang:element(6, Schema)}.

-file("src/mochi/schema.gleam", 253).
-spec mutation(schema(), object_type()) -> schema().
mutation(Schema, Mutation_type) ->
    {schema,
        erlang:element(2, Schema),
        {some, Mutation_type},
        erlang:element(4, Schema),
        erlang:element(5, Schema),
        erlang:element(6, Schema)}.

-file("src/mochi/schema.gleam", 257).
-spec subscription(schema(), object_type()) -> schema().
subscription(Schema, Subscription_type) ->
    {schema,
        erlang:element(2, Schema),
        erlang:element(3, Schema),
        {some, Subscription_type},
        erlang:element(5, Schema),
        erlang:element(6, Schema)}.

-file("src/mochi/schema.gleam", 261).
-spec add_type(schema(), type_definition()) -> schema().
add_type(Schema, Type_def) ->
    Type_name = case Type_def of
        {object_type_def, Obj} ->
            erlang:element(2, Obj);

        {scalar_type_def, Scalar} ->
            erlang:element(2, Scalar);

        {enum_type_def, Enum} ->
            erlang:element(2, Enum);

        {interface_type_def, Interface} ->
            erlang:element(2, Interface);

        {union_type_def, Union} ->
            erlang:element(2, Union);

        {input_object_type_def, Input} ->
            erlang:element(2, Input)
    end,
    {schema,
        erlang:element(2, Schema),
        erlang:element(3, Schema),
        erlang:element(4, Schema),
        gleam@dict:insert(erlang:element(5, Schema), Type_name, Type_def),
        erlang:element(6, Schema)}.

-file("src/mochi/schema.gleam", 275).
-spec object(binary()) -> object_type().
object(Name) ->
    {object_type, Name, none, maps:new(), []}.

-file("src/mochi/schema.gleam", 279).
-spec description(object_type(), binary()) -> object_type().
description(Obj, Desc) ->
    {object_type,
        erlang:element(2, Obj),
        {some, Desc},
        erlang:element(4, Obj),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 283).
-spec field(object_type(), field_definition()) -> object_type().
field(Obj, Field_def) ->
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        gleam@dict:insert(
            erlang:element(4, Obj),
            erlang:element(2, Field_def),
            Field_def
        ),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 287).
-spec implements(object_type(), interface_type()) -> object_type().
implements(Obj, Interface) ->
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        erlang:element(4, Obj),
        [Interface | erlang:element(5, Obj)]}.

-file("src/mochi/schema.gleam", 292).
-spec field_def(binary(), field_type()) -> field_definition().
field_def(Name, Field_type) ->
    {field_definition, Name, none, Field_type, maps:new(), none, false, none}.

-file("src/mochi/schema.gleam", 305).
?DOC(" Mark a field as deprecated\n").
-spec deprecate(field_definition(), binary()) -> field_definition().
deprecate(Field, Reason) ->
    {field_definition,
        erlang:element(2, Field),
        erlang:element(3, Field),
        erlang:element(4, Field),
        erlang:element(5, Field),
        erlang:element(6, Field),
        true,
        {some, Reason}}.

-file("src/mochi/schema.gleam", 314).
?DOC(" Mark a field as deprecated without a reason\n").
-spec deprecate_field(field_definition()) -> field_definition().
deprecate_field(Field) ->
    {field_definition,
        erlang:element(2, Field),
        erlang:element(3, Field),
        erlang:element(4, Field),
        erlang:element(5, Field),
        erlang:element(6, Field),
        true,
        none}.

-file("src/mochi/schema.gleam", 318).
-spec field_description(field_definition(), binary()) -> field_definition().
field_description(Field, Desc) ->
    {field_definition,
        erlang:element(2, Field),
        {some, Desc},
        erlang:element(4, Field),
        erlang:element(5, Field),
        erlang:element(6, Field),
        erlang:element(7, Field),
        erlang:element(8, Field)}.

-file("src/mochi/schema.gleam", 325).
-spec argument(field_definition(), argument_definition()) -> field_definition().
argument(Field, Arg_def) ->
    {field_definition,
        erlang:element(2, Field),
        erlang:element(3, Field),
        erlang:element(4, Field),
        gleam@dict:insert(
            erlang:element(5, Field),
            erlang:element(2, Arg_def),
            Arg_def
        ),
        erlang:element(6, Field),
        erlang:element(7, Field),
        erlang:element(8, Field)}.

-file("src/mochi/schema.gleam", 335).
-spec resolver(
    field_definition(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> field_definition().
resolver(Field, Resolve_fn) ->
    {field_definition,
        erlang:element(2, Field),
        erlang:element(3, Field),
        erlang:element(4, Field),
        erlang:element(5, Field),
        {some, Resolve_fn},
        erlang:element(7, Field),
        erlang:element(8, Field)}.

-file("src/mochi/schema.gleam", 340).
-spec arg(binary(), field_type()) -> argument_definition().
arg(Name, Arg_type) ->
    {argument_definition, Name, none, Arg_type, none}.

-file("src/mochi/schema.gleam", 349).
-spec arg_description(argument_definition(), binary()) -> argument_definition().
arg_description(Arg, Desc) ->
    {argument_definition,
        erlang:element(2, Arg),
        {some, Desc},
        erlang:element(4, Arg),
        erlang:element(5, Arg)}.

-file("src/mochi/schema.gleam", 356).
-spec default_value(argument_definition(), gleam@dynamic:dynamic_()) -> argument_definition().
default_value(Arg, Value) ->
    {argument_definition,
        erlang:element(2, Arg),
        erlang:element(3, Arg),
        erlang:element(4, Arg),
        {some, Value}}.

-file("src/mochi/schema.gleam", 364).
-spec string_type() -> field_type().
string_type() ->
    {named, <<"String"/utf8>>}.

-file("src/mochi/schema.gleam", 368).
-spec int_type() -> field_type().
int_type() ->
    {named, <<"Int"/utf8>>}.

-file("src/mochi/schema.gleam", 372).
-spec float_type() -> field_type().
float_type() ->
    {named, <<"Float"/utf8>>}.

-file("src/mochi/schema.gleam", 376).
-spec boolean_type() -> field_type().
boolean_type() ->
    {named, <<"Boolean"/utf8>>}.

-file("src/mochi/schema.gleam", 380).
-spec id_type() -> field_type().
id_type() ->
    {named, <<"ID"/utf8>>}.

-file("src/mochi/schema.gleam", 384).
-spec named_type(binary()) -> field_type().
named_type(Name) ->
    {named, Name}.

-file("src/mochi/schema.gleam", 388).
-spec non_null(field_type()) -> field_type().
non_null(Inner) ->
    {non_null, Inner}.

-file("src/mochi/schema.gleam", 392).
-spec list_type(field_type()) -> field_type().
list_type(Inner) ->
    {list, Inner}.

-file("src/mochi/schema.gleam", 397).
-spec scalar(binary()) -> scalar_type().
scalar(Name) ->
    {scalar_type,
        Name,
        none,
        fun(Value) -> {ok, Value} end,
        fun(Value@1) -> {ok, Value@1} end,
        fun(Value@2) -> {ok, Value@2} end}.

-file("src/mochi/schema.gleam", 407).
-spec scalar_description(scalar_type(), binary()) -> scalar_type().
scalar_description(Scalar, Desc) ->
    {scalar_type,
        erlang:element(2, Scalar),
        {some, Desc},
        erlang:element(4, Scalar),
        erlang:element(5, Scalar),
        erlang:element(6, Scalar)}.

-file("src/mochi/schema.gleam", 411).
-spec serialize(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
serialize(Scalar, Serialize_fn) ->
    {scalar_type,
        erlang:element(2, Scalar),
        erlang:element(3, Scalar),
        Serialize_fn,
        erlang:element(5, Scalar),
        erlang:element(6, Scalar)}.

-file("src/mochi/schema.gleam", 418).
-spec parse_value(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
parse_value(Scalar, Parse_fn) ->
    {scalar_type,
        erlang:element(2, Scalar),
        erlang:element(3, Scalar),
        erlang:element(4, Scalar),
        Parse_fn,
        erlang:element(6, Scalar)}.

-file("src/mochi/schema.gleam", 425).
-spec parse_literal(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
parse_literal(Scalar, Parse_fn) ->
    {scalar_type,
        erlang:element(2, Scalar),
        erlang:element(3, Scalar),
        erlang:element(4, Scalar),
        erlang:element(5, Scalar),
        Parse_fn}.

-file("src/mochi/schema.gleam", 433).
-spec string_scalar() -> scalar_type().
string_scalar() ->
    _pipe = scalar(<<"String"/utf8>>),
    scalar_description(
        _pipe,
        <<"The String scalar type represents textual data"/utf8>>
    ).

-file("src/mochi/schema.gleam", 438).
-spec int_scalar() -> scalar_type().
int_scalar() ->
    _pipe = scalar(<<"Int"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Int scalar type represents non-fractional signed whole numeric values"/utf8>>
    ).

-file("src/mochi/schema.gleam", 445).
-spec float_scalar() -> scalar_type().
float_scalar() ->
    _pipe = scalar(<<"Float"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Float scalar type represents signed double-precision fractional values"/utf8>>
    ).

-file("src/mochi/schema.gleam", 452).
-spec boolean_scalar() -> scalar_type().
boolean_scalar() ->
    _pipe = scalar(<<"Boolean"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Boolean scalar type represents true or false"/utf8>>
    ).

-file("src/mochi/schema.gleam", 457).
-spec id_scalar() -> scalar_type().
id_scalar() ->
    _pipe = scalar(<<"ID"/utf8>>),
    scalar_description(
        _pipe,
        <<"The ID scalar type represents a unique identifier"/utf8>>
    ).

-file("src/mochi/schema.gleam", 463).
-spec interface(binary()) -> interface_type().
interface(Name) ->
    {interface_type, Name, none, maps:new(), none}.

-file("src/mochi/schema.gleam", 472).
-spec interface_description(interface_type(), binary()) -> interface_type().
interface_description(Iface, Desc) ->
    {interface_type,
        erlang:element(2, Iface),
        {some, Desc},
        erlang:element(4, Iface),
        erlang:element(5, Iface)}.

-file("src/mochi/schema.gleam", 479).
-spec interface_field(interface_type(), field_definition()) -> interface_type().
interface_field(Iface, Field_def) ->
    {interface_type,
        erlang:element(2, Iface),
        erlang:element(3, Iface),
        gleam@dict:insert(
            erlang:element(4, Iface),
            erlang:element(2, Field_def),
            Field_def
        ),
        erlang:element(5, Iface)}.

-file("src/mochi/schema.gleam", 489).
-spec interface_resolve_type(
    interface_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, binary()} | {error, binary()})
) -> interface_type().
interface_resolve_type(Iface, Resolver) ->
    {interface_type,
        erlang:element(2, Iface),
        erlang:element(3, Iface),
        erlang:element(4, Iface),
        {some, Resolver}}.

-file("src/mochi/schema.gleam", 497).
-spec union(binary()) -> union_type().
union(Name) ->
    {union_type, Name, none, [], none}.

-file("src/mochi/schema.gleam", 501).
-spec union_description(union_type(), binary()) -> union_type().
union_description(Union_type, Desc) ->
    {union_type,
        erlang:element(2, Union_type),
        {some, Desc},
        erlang:element(4, Union_type),
        erlang:element(5, Union_type)}.

-file("src/mochi/schema.gleam", 505).
-spec union_member(union_type(), object_type()) -> union_type().
union_member(Union_type, Member) ->
    {union_type,
        erlang:element(2, Union_type),
        erlang:element(3, Union_type),
        [Member | erlang:element(4, Union_type)],
        erlang:element(5, Union_type)}.

-file("src/mochi/schema.gleam", 509).
-spec union_resolve_type(
    union_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, binary()} | {error, binary()})
) -> union_type().
union_resolve_type(Union_type, Resolver) ->
    {union_type,
        erlang:element(2, Union_type),
        erlang:element(3, Union_type),
        erlang:element(4, Union_type),
        {some, Resolver}}.

-file("src/mochi/schema.gleam", 521).
?DOC(" Add a directive definition to the schema\n").
-spec add_directive(schema(), directive_definition()) -> schema().
add_directive(Schema, Directive) ->
    {schema,
        erlang:element(2, Schema),
        erlang:element(3, Schema),
        erlang:element(4, Schema),
        erlang:element(5, Schema),
        gleam@dict:insert(
            erlang:element(6, Schema),
            erlang:element(2, Directive),
            Directive
        )}.

-file("src/mochi/schema.gleam", 529).
?DOC(" Create a new directive definition\n").
-spec directive(binary(), list(directive_location())) -> directive_definition().
directive(Name, Locations) ->
    {directive_definition, Name, none, maps:new(), Locations, false, none}.

-file("src/mochi/schema.gleam", 544).
?DOC(" Add description to a directive\n").
-spec directive_description(directive_definition(), binary()) -> directive_definition().
directive_description(Dir, Desc) ->
    {directive_definition,
        erlang:element(2, Dir),
        {some, Desc},
        erlang:element(4, Dir),
        erlang:element(5, Dir),
        erlang:element(6, Dir),
        erlang:element(7, Dir)}.

-file("src/mochi/schema.gleam", 552).
?DOC(" Add an argument to a directive\n").
-spec directive_argument(directive_definition(), argument_definition()) -> directive_definition().
directive_argument(Dir, Arg_def) ->
    {directive_definition,
        erlang:element(2, Dir),
        erlang:element(3, Dir),
        gleam@dict:insert(
            erlang:element(4, Dir),
            erlang:element(2, Arg_def),
            Arg_def
        ),
        erlang:element(5, Dir),
        erlang:element(6, Dir),
        erlang:element(7, Dir)}.

-file("src/mochi/schema.gleam", 563).
?DOC(" Make a directive repeatable\n").
-spec directive_repeatable(directive_definition()) -> directive_definition().
directive_repeatable(Dir) ->
    {directive_definition,
        erlang:element(2, Dir),
        erlang:element(3, Dir),
        erlang:element(4, Dir),
        erlang:element(5, Dir),
        true,
        erlang:element(7, Dir)}.

-file("src/mochi/schema.gleam", 568).
?DOC(" Set the handler function for a directive (for field-level directives)\n").
-spec directive_handler(
    directive_definition(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> directive_definition().
directive_handler(Dir, Handler) ->
    {directive_definition,
        erlang:element(2, Dir),
        erlang:element(3, Dir),
        erlang:element(4, Dir),
        erlang:element(5, Dir),
        erlang:element(6, Dir),
        {some, Handler}}.

-file("src/mochi/schema.gleam", 580).
?DOC(" @skip(if: Boolean!) directive\n").
-spec skip_directive() -> directive_definition().
skip_directive() ->
    _pipe = directive(
        <<"skip"/utf8>>,
        [field_location, fragment_spread_location, inline_fragment_location]
    ),
    _pipe@1 = directive_description(
        _pipe,
        <<"Directs the executor to skip this field or fragment when the `if` argument is true."/utf8>>
    ),
    directive_argument(
        _pipe@1,
        begin
            _pipe@2 = arg(<<"if"/utf8>>, non_null(boolean_type())),
            arg_description(_pipe@2, <<"Skipped when true."/utf8>>)
        end
    ).

-file("src/mochi/schema.gleam", 596).
?DOC(" @include(if: Boolean!) directive\n").
-spec include_directive() -> directive_definition().
include_directive() ->
    _pipe = directive(
        <<"include"/utf8>>,
        [field_location, fragment_spread_location, inline_fragment_location]
    ),
    _pipe@1 = directive_description(
        _pipe,
        <<"Directs the executor to include this field or fragment only when the `if` argument is true."/utf8>>
    ),
    directive_argument(
        _pipe@1,
        begin
            _pipe@2 = arg(<<"if"/utf8>>, non_null(boolean_type())),
            arg_description(_pipe@2, <<"Included when true."/utf8>>)
        end
    ).

-file("src/mochi/schema.gleam", 612).
?DOC(" @deprecated(reason: String) directive\n").
-spec deprecated_directive() -> directive_definition().
deprecated_directive() ->
    _pipe = directive(
        <<"deprecated"/utf8>>,
        [field_definition_location, enum_value_location]
    ),
    _pipe@1 = directive_description(
        _pipe,
        <<"Marks an element of a GraphQL schema as no longer supported."/utf8>>
    ),
    directive_argument(
        _pipe@1,
        begin
            _pipe@2 = arg(<<"reason"/utf8>>, string_type()),
            arg_description(
                _pipe@2,
                <<"Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data."/utf8>>
            )
        end
    ).

-file("src/mochi/schema.gleam", 626).
?DOC(" Get all built-in directives\n").
-spec builtin_directives() -> list(directive_definition()).
builtin_directives() ->
    [skip_directive(), include_directive(), deprecated_directive()].

-file("src/mochi/schema.gleam", 631).
?DOC(" Convert DirectiveLocation to string (for SDL generation)\n").
-spec directive_location_to_string(directive_location()) -> binary().
directive_location_to_string(Loc) ->
    case Loc of
        query_location ->
            <<"QUERY"/utf8>>;

        mutation_location ->
            <<"MUTATION"/utf8>>;

        subscription_location ->
            <<"SUBSCRIPTION"/utf8>>;

        field_location ->
            <<"FIELD"/utf8>>;

        fragment_definition_location ->
            <<"FRAGMENT_DEFINITION"/utf8>>;

        fragment_spread_location ->
            <<"FRAGMENT_SPREAD"/utf8>>;

        inline_fragment_location ->
            <<"INLINE_FRAGMENT"/utf8>>;

        variable_definition_location ->
            <<"VARIABLE_DEFINITION"/utf8>>;

        schema_location ->
            <<"SCHEMA"/utf8>>;

        scalar_location ->
            <<"SCALAR"/utf8>>;

        object_location ->
            <<"OBJECT"/utf8>>;

        field_definition_location ->
            <<"FIELD_DEFINITION"/utf8>>;

        argument_definition_location ->
            <<"ARGUMENT_DEFINITION"/utf8>>;

        interface_location ->
            <<"INTERFACE"/utf8>>;

        union_location ->
            <<"UNION"/utf8>>;

        enum_location ->
            <<"ENUM"/utf8>>;

        enum_value_location ->
            <<"ENUM_VALUE"/utf8>>;

        input_object_location ->
            <<"INPUT_OBJECT"/utf8>>;

        input_field_definition_location ->
            <<"INPUT_FIELD_DEFINITION"/utf8>>
    end.
