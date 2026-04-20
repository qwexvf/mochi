-module(mochi@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/schema.gleam").
-export([execution_context/1, execution_context_with_middleware/2, execution_context_with_telemetry/2, full_execution_context/3, with_middleware/2, with_telemetry/2, add_data_loader/3, get_middleware/1, get_telemetry/1, update_telemetry/2, with_telemetry_fn/2, get_telemetry_fn/1, get_data_loader/2, update_data_loader/3, with_loaders/2, load/3, load_many/3, load_by_id/3, load_many_by_id/3, schema/0, 'query'/2, mutation/2, subscription/2, add_type/2, object/1, description/2, field/2, implements/2, field_def/2, deprecate/2, deprecate_field/1, field_description/2, argument/2, resolver/2, guard/2, guards/2, all_guards/1, any_guard/1, auto_resolver/1, auto_field/3, id_field/2, string_field/2, required_string_field/2, int_field/2, required_int_field/2, bool_field/2, required_bool_field/2, float_field/2, required_float_field/2, list_field/3, required_list_field/3, ref_field/3, required_ref_field/3, resolver_field/5, list_query/5, ref_query/5, query_with_args/6, arg/2, arg_description/2, default_value/2, string_type/0, int_type/0, float_type/0, boolean_type/0, id_type/0, named_type/1, non_null/1, list_type/1, scalar/1, scalar_description/2, serialize/2, parse_value/2, parse_literal/2, string_scalar/0, int_scalar/0, float_scalar/0, boolean_scalar/0, id_scalar/0, interface/1, interface_description/2, interface_field/2, interface_resolve_type/2, union/1, union_description/2, union_member/2, union_resolve_type/2, add_directive/2, directive/2, directive_description/2, directive_argument/2, directive_repeatable/1, directive_handler/2, skip_directive/0, include_directive/0, deprecated_directive/0, builtin_directives/0, directive_location_to_string/1]).
-export_type([telemetry_context/0, schema_event/0, schema/0, directive_definition/0, directive_location/0, type_definition/0, object_type/0, field_definition/0, field_type/0, argument_definition/0, scalar_type/0, enum_type/0, enum_value_definition/0, interface_type/0, union_type/0, input_object_type/0, input_field_definition/0, execution_context/0, resolver_info/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Core schema types and execution context for GraphQL.\n"
    "\n"
    " This module provides:\n"
    " - Schema type definitions (ObjectType, FieldType, ScalarType, etc.)\n"
    " - ExecutionContext for managing DataLoaders and middleware\n"
    " - Builder functions for constructing schemas\n"
    "\n"
    " ## ExecutionContext with DataLoaders\n"
    "\n"
    " ```gleam\n"
    " // Create context with multiple loaders\n"
    " let ctx = schema.execution_context(types.to_dynamic(dict.new()))\n"
    "   |> schema.with_loaders([\n"
    "     #(\"pokemon\", pokemon_loader),\n"
    "     #(\"trainer\", trainer_loader),\n"
    "   ])\n"
    "\n"
    " // Load by ID (handles context threading automatically)\n"
    " let #(ctx, pokemon) = schema.load_by_id(ctx, \"pokemon\", 25)\n"
    " let #(ctx, trainers) = schema.load_many_by_id(ctx, \"trainer\", [1, 2, 3])\n"
    " ```\n"
    "\n"
    " ## Schema Building\n"
    "\n"
    " ```gleam\n"
    " let my_schema = schema.schema()\n"
    "   |> schema.query(query_type)\n"
    "   |> schema.mutation(mutation_type)\n"
    "   |> schema.add_type(schema.ObjectTypeDef(user_type))\n"
    "   |> schema.add_directive(auth_directive)\n"
    " ```\n"
).

-type telemetry_context() :: any().

-type schema_event() :: schema_parse_start |
    {schema_parse_end, boolean(), integer()} |
    schema_validation_start |
    {schema_validation_end, boolean(), integer(), integer()} |
    {schema_field_start, binary(), binary(), list(binary())} |
    {schema_field_end, binary(), binary(), list(binary()), boolean(), integer()} |
    {schema_operation_start, gleam@option:option(binary())} |
    {schema_operation_end, gleam@option:option(binary()), boolean(), integer()} |
    {schema_data_loader_batch, binary(), integer(), integer()}.

-type schema() :: {schema,
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@dict:dict(binary(), type_definition()),
        gleam@dict:dict(binary(), directive_definition()),
        gleam@option:option(mochi@document_cache:document_cache())}.

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
        gleam@option:option(binary()),
        gleam@option:option(fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), execution_context()) -> {ok,
                binary()} |
            {error, binary()}))}.

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
        gleam@dict:dict(binary(), mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())),
        gleam@option:option(fun((binary(), field_definition(), resolver_info(), fun((resolver_info()) -> {ok,
                gleam@dynamic:dynamic_()} |
            {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()})),
        gleam@option:option(telemetry_context()),
        gleam@option:option(fun((schema_event()) -> nil))}.

-type resolver_info() :: {resolver_info,
        gleam@option:option(gleam@dynamic:dynamic_()),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        execution_context(),
        gleam@dynamic:dynamic_()}.

-file("src/mochi/schema.gleam", 298).
?DOC(" Create a new execution context\n").
-spec execution_context(gleam@dynamic:dynamic_()) -> execution_context().
execution_context(User_context) ->
    {execution_context, User_context, maps:new(), none, none, none}.

-file("src/mochi/schema.gleam", 309).
?DOC(" Create an execution context with a middleware function\n").
-spec execution_context_with_middleware(
    gleam@dynamic:dynamic_(),
    fun((binary(), field_definition(), resolver_info(), fun((resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> execution_context().
execution_context_with_middleware(User_context, Middleware) ->
    {execution_context,
        User_context,
        maps:new(),
        {some, Middleware},
        none,
        none}.

-file("src/mochi/schema.gleam", 323).
?DOC(" Create an execution context with telemetry (legacy opaque context)\n").
-spec execution_context_with_telemetry(
    gleam@dynamic:dynamic_(),
    telemetry_context()
) -> execution_context().
execution_context_with_telemetry(User_context, Telemetry) ->
    {execution_context, User_context, maps:new(), none, {some, Telemetry}, none}.

-file("src/mochi/schema.gleam", 337).
?DOC(" Create a full execution context with all options\n").
-spec full_execution_context(
    gleam@dynamic:dynamic_(),
    gleam@option:option(fun((binary(), field_definition(), resolver_info(), fun((resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})),
    gleam@option:option(telemetry_context())
) -> execution_context().
full_execution_context(User_context, Middleware, Telemetry) ->
    {execution_context, User_context, maps:new(), Middleware, Telemetry, none}.

-file("src/mochi/schema.gleam", 352).
?DOC(" Set middleware function on an execution context\n").
-spec with_middleware(
    execution_context(),
    fun((binary(), field_definition(), resolver_info(), fun((resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> execution_context().
with_middleware(Context, Middleware) ->
    {execution_context,
        erlang:element(2, Context),
        erlang:element(3, Context),
        {some, Middleware},
        erlang:element(5, Context),
        erlang:element(6, Context)}.

-file("src/mochi/schema.gleam", 360).
?DOC(" Set telemetry on an execution context\n").
-spec with_telemetry(execution_context(), telemetry_context()) -> execution_context().
with_telemetry(Context, Telemetry) ->
    {execution_context,
        erlang:element(2, Context),
        erlang:element(3, Context),
        erlang:element(4, Context),
        {some, Telemetry},
        erlang:element(6, Context)}.

-file("src/mochi/schema.gleam", 368).
?DOC(" Add a DataLoader to the execution context\n").
-spec add_data_loader(
    execution_context(),
    binary(),
    mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
add_data_loader(Context, Name, Loader) ->
    {execution_context,
        erlang:element(2, Context),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader),
        erlang:element(4, Context),
        erlang:element(5, Context),
        erlang:element(6, Context)}.

-file("src/mochi/schema.gleam", 380).
?DOC(" Get the middleware function from context\n").
-spec get_middleware(execution_context()) -> gleam@option:option(fun((binary(), field_definition(), resolver_info(), fun((resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()})) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})).
get_middleware(Context) ->
    erlang:element(4, Context).

-file("src/mochi/schema.gleam", 385).
?DOC(" Get the telemetry context\n").
-spec get_telemetry(execution_context()) -> gleam@option:option(telemetry_context()).
get_telemetry(Context) ->
    erlang:element(5, Context).

-file("src/mochi/schema.gleam", 390).
?DOC(" Update the telemetry context\n").
-spec update_telemetry(execution_context(), telemetry_context()) -> execution_context().
update_telemetry(Context, Telemetry) ->
    {execution_context,
        erlang:element(2, Context),
        erlang:element(3, Context),
        erlang:element(4, Context),
        {some, Telemetry},
        erlang:element(6, Context)}.

-file("src/mochi/schema.gleam", 400).
?DOC(
    " Set a telemetry callback function on the execution context.\n"
    " This function is called for each SchemaEvent emitted during execution.\n"
    " Use `telemetry.to_schema_fn/1` to bridge a TelemetryConfig to this callback.\n"
).
-spec with_telemetry_fn(execution_context(), fun((schema_event()) -> nil)) -> execution_context().
with_telemetry_fn(Context, Telemetry_fn) ->
    {execution_context,
        erlang:element(2, Context),
        erlang:element(3, Context),
        erlang:element(4, Context),
        erlang:element(5, Context),
        {some, Telemetry_fn}}.

-file("src/mochi/schema.gleam", 408).
?DOC(" Get the telemetry callback function from the execution context\n").
-spec get_telemetry_fn(execution_context()) -> gleam@option:option(fun((schema_event()) -> nil)).
get_telemetry_fn(Context) ->
    erlang:element(6, Context).

-file("src/mochi/schema.gleam", 413).
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

-file("src/mochi/schema.gleam", 425).
?DOC(" Update a DataLoader in the execution context\n").
-spec update_data_loader(
    execution_context(),
    binary(),
    mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
update_data_loader(Context, Name, Loader) ->
    {execution_context,
        erlang:element(2, Context),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader),
        erlang:element(4, Context),
        erlang:element(5, Context),
        erlang:element(6, Context)}.

-file("src/mochi/schema.gleam", 448).
?DOC(
    " Add multiple DataLoaders to the execution context at once\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let ctx = schema.execution_context(user_ctx)\n"
    "   |> schema.with_loaders([\n"
    "     #(\"pokemon\", pokemon_loader),\n"
    "     #(\"move\", move_loader),\n"
    "     #(\"trainer\", trainer_loader),\n"
    "   ])\n"
    " ```\n"
).
-spec with_loaders(
    execution_context(),
    list({binary(),
        mochi@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())})
) -> execution_context().
with_loaders(Context, Loaders) ->
    gleam@list:fold(
        Loaders,
        Context,
        fun(Ctx, Loader_pair) ->
            {Name, Loader} = Loader_pair,
            add_data_loader(Ctx, Name, Loader)
        end
    ).

-file("src/mochi/schema.gleam", 467).
?DOC(
    " Load a value using a named DataLoader, returning updated context and result\n"
    "\n"
    " This handles the context threading automatically.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let #(ctx, result) = schema.load(ctx, \"pokemon\", dataloader.int_key(25))\n"
    " ```\n"
).
-spec load(execution_context(), binary(), gleam@dynamic:dynamic_()) -> {execution_context(),
    {ok, gleam@dynamic:dynamic_()} | {error, binary()}}.
load(Context, Loader_name, Key) ->
    case get_data_loader(Context, Loader_name) of
        {ok, Loader} ->
            Start_ns = mochi_time_ffi:monotonic_time_ns(),
            {New_loader, Result} = mochi@dataloader:load(Loader, Key),
            Duration_ns = mochi_time_ffi:monotonic_time_ns() - Start_ns,
            case erlang:element(6, Context) of
                {some, Fn_} ->
                    Fn_({schema_data_loader_batch, Loader_name, 1, Duration_ns});

                none ->
                    nil
            end,
            New_ctx = update_data_loader(Context, Loader_name, New_loader),
            {New_ctx, Result};

        {error, E} ->
            {Context, {error, E}}
    end.

-file("src/mochi/schema.gleam", 499).
?DOC(
    " Load multiple values using a named DataLoader\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let keys = list.map([1, 2, 3], dataloader.int_key)\n"
    " let #(ctx, results) = schema.load_many(ctx, \"pokemon\", keys)\n"
    " ```\n"
).
-spec load_many(execution_context(), binary(), list(gleam@dynamic:dynamic_())) -> {execution_context(),
    list({ok, gleam@dynamic:dynamic_()} | {error, binary()})}.
load_many(Context, Loader_name, Keys) ->
    case get_data_loader(Context, Loader_name) of
        {ok, Loader} ->
            Batch_size = erlang:length(Keys),
            Start_ns = mochi_time_ffi:monotonic_time_ns(),
            {New_loader, Results} = mochi@dataloader:load_many(Loader, Keys),
            Duration_ns = mochi_time_ffi:monotonic_time_ns() - Start_ns,
            case erlang:element(6, Context) of
                {some, Fn_} ->
                    Fn_(
                        {schema_data_loader_batch,
                            Loader_name,
                            Batch_size,
                            Duration_ns}
                    );

                none ->
                    nil
            end,
            New_ctx = update_data_loader(Context, Loader_name, New_loader),
            {New_ctx, Results};

        {error, E} ->
            {Context, gleam@list:map(Keys, fun(_) -> {error, E} end)}
    end.

-file("src/mochi/schema.gleam", 534).
?DOC(
    " Load an entity by Int ID using a named DataLoader\n"
    "\n"
    " Convenience wrapper for the common case of loading by integer ID.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let #(ctx, pokemon) = schema.load_by_id(ctx, \"pokemon\", 25)\n"
    " ```\n"
).
-spec load_by_id(execution_context(), binary(), integer()) -> {execution_context(),
    {ok, gleam@dynamic:dynamic_()} | {error, binary()}}.
load_by_id(Context, Loader_name, Id) ->
    load(Context, Loader_name, mochi@dataloader:int_key(Id)).

-file("src/mochi/schema.gleam", 549).
?DOC(
    " Load multiple entities by Int IDs using a named DataLoader\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let #(ctx, pokemon_list) = schema.load_many_by_id(ctx, \"pokemon\", [1, 4, 7, 25])\n"
    " ```\n"
).
-spec load_many_by_id(execution_context(), binary(), list(integer())) -> {execution_context(),
    list({ok, gleam@dynamic:dynamic_()} | {error, binary()})}.
load_many_by_id(Context, Loader_name, Ids) ->
    Keys = gleam@list:map(Ids, fun mochi@dataloader:int_key/1),
    load_many(Context, Loader_name, Keys).

-file("src/mochi/schema.gleam", 559).
-spec schema() -> schema().
schema() ->
    {schema, none, none, none, maps:new(), maps:new(), none}.

-file("src/mochi/schema.gleam", 570).
-spec 'query'(schema(), object_type()) -> schema().
'query'(Schema, Query_type) ->
    {schema,
        {some, Query_type},
        erlang:element(3, Schema),
        erlang:element(4, Schema),
        erlang:element(5, Schema),
        erlang:element(6, Schema),
        erlang:element(7, Schema)}.

-file("src/mochi/schema.gleam", 574).
-spec mutation(schema(), object_type()) -> schema().
mutation(Schema, Mutation_type) ->
    {schema,
        erlang:element(2, Schema),
        {some, Mutation_type},
        erlang:element(4, Schema),
        erlang:element(5, Schema),
        erlang:element(6, Schema),
        erlang:element(7, Schema)}.

-file("src/mochi/schema.gleam", 578).
-spec subscription(schema(), object_type()) -> schema().
subscription(Schema, Subscription_type) ->
    {schema,
        erlang:element(2, Schema),
        erlang:element(3, Schema),
        {some, Subscription_type},
        erlang:element(5, Schema),
        erlang:element(6, Schema),
        erlang:element(7, Schema)}.

-file("src/mochi/schema.gleam", 582).
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
        erlang:element(6, Schema),
        erlang:element(7, Schema)}.

-file("src/mochi/schema.gleam", 596).
-spec object(binary()) -> object_type().
object(Name) ->
    {object_type, Name, none, maps:new(), []}.

-file("src/mochi/schema.gleam", 600).
-spec description(object_type(), binary()) -> object_type().
description(Obj, Desc) ->
    {object_type,
        erlang:element(2, Obj),
        {some, Desc},
        erlang:element(4, Obj),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 604).
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

-file("src/mochi/schema.gleam", 608).
-spec implements(object_type(), interface_type()) -> object_type().
implements(Obj, Interface) ->
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        erlang:element(4, Obj),
        [Interface | erlang:element(5, Obj)]}.

-file("src/mochi/schema.gleam", 613).
-spec field_def(binary(), field_type()) -> field_definition().
field_def(Name, Field_type) ->
    {field_definition,
        Name,
        none,
        Field_type,
        maps:new(),
        none,
        false,
        none,
        none}.

-file("src/mochi/schema.gleam", 627).
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
        {some, Reason},
        erlang:element(9, Field)}.

-file("src/mochi/schema.gleam", 636).
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
        none,
        erlang:element(9, Field)}.

-file("src/mochi/schema.gleam", 640).
-spec field_description(field_definition(), binary()) -> field_definition().
field_description(Field, Desc) ->
    {field_definition,
        erlang:element(2, Field),
        {some, Desc},
        erlang:element(4, Field),
        erlang:element(5, Field),
        erlang:element(6, Field),
        erlang:element(7, Field),
        erlang:element(8, Field),
        erlang:element(9, Field)}.

-file("src/mochi/schema.gleam", 647).
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
        erlang:element(8, Field),
        erlang:element(9, Field)}.

-file("src/mochi/schema.gleam", 657).
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
        erlang:element(8, Field),
        erlang:element(9, Field)}.

-file("src/mochi/schema.gleam", 677).
?DOC(
    " Add a guard to a field definition.\n"
    " The guard runs before the resolver — if it returns Error, the resolver is skipped.\n"
    " Multiple guards can be stacked by calling this function multiple times;\n"
    " each new guard wraps the previous resolver+guards, so all must pass.\n"
).
-spec guard(
    field_definition(),
    fun((resolver_info()) -> {ok, nil} | {error, binary()})
) -> field_definition().
guard(Field, Guard_fn) ->
    case erlang:element(6, Field) of
        {some, Current_resolver} ->
            Guarded = fun(Info) ->
                gleam@result:'try'(
                    Guard_fn(Info),
                    fun(_) -> Current_resolver(Info) end
                )
            end,
            {field_definition,
                erlang:element(2, Field),
                erlang:element(3, Field),
                erlang:element(4, Field),
                erlang:element(5, Field),
                {some, Guarded},
                erlang:element(7, Field),
                erlang:element(8, Field),
                erlang:element(9, Field)};

        none ->
            Field
    end.

-file("src/mochi/schema.gleam", 699).
?DOC(
    " Add multiple guards to a field definition.\n"
    " Guards are checked in list order — the first guard in the list is checked first.\n"
    "\n"
    " ```gleam\n"
    " schema.field_def(\"secret\", schema.string_type())\n"
    "   |> schema.resolver(my_resolver)\n"
    "   |> schema.guards([require_auth, require_admin])\n"
    "   // require_auth is checked first, then require_admin\n"
    " ```\n"
).
-spec guards(
    field_definition(),
    list(fun((resolver_info()) -> {ok, nil} | {error, binary()}))
) -> field_definition().
guards(Field, Guard_fns) ->
    gleam@list:fold(
        lists:reverse(Guard_fns),
        Field,
        fun(F, G) -> guard(F, G) end
    ).

-file("src/mochi/schema.gleam", 707).
?DOC(
    " Combine guards with AND logic — all must pass (checked in list order).\n"
    " Returns a single guard that fails with the first error encountered.\n"
).
-spec all_guards(list(fun((resolver_info()) -> {ok, nil} | {error, binary()}))) -> fun((resolver_info()) -> {ok,
        nil} |
    {error, binary()}).
all_guards(Guard_fns) ->
    fun(Info) -> gleam@list:try_each(Guard_fns, fun(G) -> G(Info) end) end.

-file("src/mochi/schema.gleam", 723).
-spec try_any_guard(
    list(fun((resolver_info()) -> {ok, nil} | {error, binary()})),
    resolver_info(),
    binary()
) -> {ok, nil} | {error, binary()}.
try_any_guard(Guards, Info, Last_error) ->
    case Guards of
        [] ->
            {error, Last_error};

        [G | Rest] ->
            case G(Info) of
                {ok, nil} ->
                    {ok, nil};

                {error, E} ->
                    try_any_guard(Rest, Info, E)
            end
    end.

-file("src/mochi/schema.gleam", 714).
?DOC(
    " Combine guards with OR logic — at least one must pass.\n"
    " Returns a single guard that succeeds if any guard passes,\n"
    " or fails with the last error if all fail.\n"
).
-spec any_guard(list(fun((resolver_info()) -> {ok, nil} | {error, binary()}))) -> fun((resolver_info()) -> {ok,
        nil} |
    {error, binary()}).
any_guard(Guard_fns) ->
    fun(Info) -> case Guard_fns of
            [] ->
                {error, <<"No guards provided"/utf8>>};

            _ ->
                try_any_guard(Guard_fns, Info, <<"No guards passed"/utf8>>)
        end end.

-file("src/mochi/schema.gleam", 747).
?DOC(" Create a resolver that auto-extracts a field from parent by name\n").
-spec auto_resolver(binary()) -> fun((resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}).
auto_resolver(Field_name) ->
    fun(Info) -> case erlang:element(2, Info) of
            {some, Parent} ->
                case gleam@dynamic@decode:run(
                    Parent,
                    gleam@dynamic@decode:dict(
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    )
                ) of
                    {ok, D} ->
                        case gleam_stdlib:map_get(D, Field_name) of
                            {ok, Value} ->
                                {ok, Value};

                            {error, _} ->
                                {error,
                                    <<"Field not found: "/utf8,
                                        Field_name/binary>>}
                        end;

                    {error, _} ->
                        {error, <<"Invalid parent type"/utf8>>}
                end;

            none ->
                {error, <<"No parent"/utf8>>}
        end end.

-file("src/mochi/schema.gleam", 767).
?DOC(" Add a field with auto-resolver (extracts field by name from parent)\n").
-spec auto_field(object_type(), binary(), field_type()) -> object_type().
auto_field(Obj, Name, Field_type) ->
    F = {field_definition,
        Name,
        none,
        Field_type,
        maps:new(),
        {some, auto_resolver(Name)},
        false,
        none,
        none},
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        gleam@dict:insert(erlang:element(4, Obj), Name, F),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 787).
?DOC(" Add a non-null ID field with auto-resolver\n").
-spec id_field(object_type(), binary()) -> object_type().
id_field(Obj, Name) ->
    auto_field(Obj, Name, {non_null, {named, <<"ID"/utf8>>}}).

-file("src/mochi/schema.gleam", 792).
?DOC(" Add a nullable String field with auto-resolver\n").
-spec string_field(object_type(), binary()) -> object_type().
string_field(Obj, Name) ->
    auto_field(Obj, Name, {named, <<"String"/utf8>>}).

-file("src/mochi/schema.gleam", 797).
?DOC(" Add a non-null String field with auto-resolver\n").
-spec required_string_field(object_type(), binary()) -> object_type().
required_string_field(Obj, Name) ->
    auto_field(Obj, Name, {non_null, {named, <<"String"/utf8>>}}).

-file("src/mochi/schema.gleam", 802).
?DOC(" Add a nullable Int field with auto-resolver\n").
-spec int_field(object_type(), binary()) -> object_type().
int_field(Obj, Name) ->
    auto_field(Obj, Name, {named, <<"Int"/utf8>>}).

-file("src/mochi/schema.gleam", 807).
?DOC(" Add a non-null Int field with auto-resolver\n").
-spec required_int_field(object_type(), binary()) -> object_type().
required_int_field(Obj, Name) ->
    auto_field(Obj, Name, {non_null, {named, <<"Int"/utf8>>}}).

-file("src/mochi/schema.gleam", 812).
?DOC(" Add a nullable Boolean field with auto-resolver\n").
-spec bool_field(object_type(), binary()) -> object_type().
bool_field(Obj, Name) ->
    auto_field(Obj, Name, {named, <<"Boolean"/utf8>>}).

-file("src/mochi/schema.gleam", 817).
?DOC(" Add a non-null Boolean field with auto-resolver\n").
-spec required_bool_field(object_type(), binary()) -> object_type().
required_bool_field(Obj, Name) ->
    auto_field(Obj, Name, {non_null, {named, <<"Boolean"/utf8>>}}).

-file("src/mochi/schema.gleam", 822).
?DOC(" Add a nullable Float field with auto-resolver\n").
-spec float_field(object_type(), binary()) -> object_type().
float_field(Obj, Name) ->
    auto_field(Obj, Name, {named, <<"Float"/utf8>>}).

-file("src/mochi/schema.gleam", 827).
?DOC(" Add a non-null Float field with auto-resolver\n").
-spec required_float_field(object_type(), binary()) -> object_type().
required_float_field(Obj, Name) ->
    auto_field(Obj, Name, {non_null, {named, <<"Float"/utf8>>}}).

-file("src/mochi/schema.gleam", 832).
?DOC(" Add a list field with auto-resolver\n").
-spec list_field(object_type(), binary(), binary()) -> object_type().
list_field(Obj, Name, Item_type) ->
    auto_field(Obj, Name, {list, {named, Item_type}}).

-file("src/mochi/schema.gleam", 841).
?DOC(" Add a non-null list field with auto-resolver\n").
-spec required_list_field(object_type(), binary(), binary()) -> object_type().
required_list_field(Obj, Name, Item_type) ->
    auto_field(Obj, Name, {non_null, {list, {named, Item_type}}}).

-file("src/mochi/schema.gleam", 850).
?DOC(" Add a reference field to another type with auto-resolver\n").
-spec ref_field(object_type(), binary(), binary()) -> object_type().
ref_field(Obj, Name, Type_name) ->
    auto_field(Obj, Name, {named, Type_name}).

-file("src/mochi/schema.gleam", 855).
?DOC(" Add a non-null reference field with auto-resolver\n").
-spec required_ref_field(object_type(), binary(), binary()) -> object_type().
required_ref_field(Obj, Name, Type_name) ->
    auto_field(Obj, Name, {non_null, {named, Type_name}}).

-file("src/mochi/schema.gleam", 869).
?DOC(" Add a field with custom resolver and optional description\n").
-spec resolver_field(
    object_type(),
    binary(),
    field_type(),
    binary(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> object_type().
resolver_field(Obj, Name, Field_type, Desc, Resolve_fn) ->
    F = {field_definition,
        Name,
        {some, Desc},
        Field_type,
        maps:new(),
        {some, Resolve_fn},
        false,
        none,
        none},
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        gleam@dict:insert(erlang:element(4, Obj), Name, F),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 891).
?DOC(" Add a list query field: `name: [ItemType]`\n").
-spec list_query(
    object_type(),
    binary(),
    binary(),
    binary(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> object_type().
list_query(Obj, Name, Item_type, Desc, Resolve_fn) ->
    resolver_field(Obj, Name, {list, {named, Item_type}}, Desc, Resolve_fn).

-file("src/mochi/schema.gleam", 902).
?DOC(" Add a reference query field: `name: TypeName`\n").
-spec ref_query(
    object_type(),
    binary(),
    binary(),
    binary(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> object_type().
ref_query(Obj, Name, Type_name, Desc, Resolve_fn) ->
    resolver_field(Obj, Name, {named, Type_name}, Desc, Resolve_fn).

-file("src/mochi/schema.gleam", 913).
?DOC(" Add a query field with arguments\n").
-spec query_with_args(
    object_type(),
    binary(),
    field_type(),
    list(argument_definition()),
    binary(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> object_type().
query_with_args(Obj, Name, Field_type, Args, Desc, Resolve_fn) ->
    F = {field_definition,
        Name,
        {some, Desc},
        Field_type,
        gleam@list:fold(
            Args,
            maps:new(),
            fun(Acc, A) -> gleam@dict:insert(Acc, erlang:element(2, A), A) end
        ),
        {some, Resolve_fn},
        false,
        none,
        none},
    {object_type,
        erlang:element(2, Obj),
        erlang:element(3, Obj),
        gleam@dict:insert(erlang:element(4, Obj), Name, F),
        erlang:element(5, Obj)}.

-file("src/mochi/schema.gleam", 938).
-spec arg(binary(), field_type()) -> argument_definition().
arg(Name, Arg_type) ->
    {argument_definition, Name, none, Arg_type, none}.

-file("src/mochi/schema.gleam", 947).
-spec arg_description(argument_definition(), binary()) -> argument_definition().
arg_description(Arg, Desc) ->
    {argument_definition,
        erlang:element(2, Arg),
        {some, Desc},
        erlang:element(4, Arg),
        erlang:element(5, Arg)}.

-file("src/mochi/schema.gleam", 954).
-spec default_value(argument_definition(), gleam@dynamic:dynamic_()) -> argument_definition().
default_value(Arg, Value) ->
    {argument_definition,
        erlang:element(2, Arg),
        erlang:element(3, Arg),
        erlang:element(4, Arg),
        {some, Value}}.

-file("src/mochi/schema.gleam", 962).
-spec string_type() -> field_type().
string_type() ->
    {named, <<"String"/utf8>>}.

-file("src/mochi/schema.gleam", 966).
-spec int_type() -> field_type().
int_type() ->
    {named, <<"Int"/utf8>>}.

-file("src/mochi/schema.gleam", 970).
-spec float_type() -> field_type().
float_type() ->
    {named, <<"Float"/utf8>>}.

-file("src/mochi/schema.gleam", 974).
-spec boolean_type() -> field_type().
boolean_type() ->
    {named, <<"Boolean"/utf8>>}.

-file("src/mochi/schema.gleam", 978).
-spec id_type() -> field_type().
id_type() ->
    {named, <<"ID"/utf8>>}.

-file("src/mochi/schema.gleam", 982).
-spec named_type(binary()) -> field_type().
named_type(Name) ->
    {named, Name}.

-file("src/mochi/schema.gleam", 986).
-spec non_null(field_type()) -> field_type().
non_null(Inner) ->
    {non_null, Inner}.

-file("src/mochi/schema.gleam", 990).
-spec list_type(field_type()) -> field_type().
list_type(Inner) ->
    {list, Inner}.

-file("src/mochi/schema.gleam", 995).
-spec scalar(binary()) -> scalar_type().
scalar(Name) ->
    {scalar_type,
        Name,
        none,
        fun(Value) -> {ok, Value} end,
        fun(Value@1) -> {ok, Value@1} end,
        fun(Value@2) -> {ok, Value@2} end}.

-file("src/mochi/schema.gleam", 1005).
-spec scalar_description(scalar_type(), binary()) -> scalar_type().
scalar_description(Scalar, Desc) ->
    {scalar_type,
        erlang:element(2, Scalar),
        {some, Desc},
        erlang:element(4, Scalar),
        erlang:element(5, Scalar),
        erlang:element(6, Scalar)}.

-file("src/mochi/schema.gleam", 1009).
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

-file("src/mochi/schema.gleam", 1016).
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

-file("src/mochi/schema.gleam", 1023).
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

-file("src/mochi/schema.gleam", 1031).
-spec string_scalar() -> scalar_type().
string_scalar() ->
    _pipe = scalar(<<"String"/utf8>>),
    scalar_description(
        _pipe,
        <<"The String scalar type represents textual data"/utf8>>
    ).

-file("src/mochi/schema.gleam", 1036).
-spec int_scalar() -> scalar_type().
int_scalar() ->
    _pipe = scalar(<<"Int"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Int scalar type represents non-fractional signed whole numeric values"/utf8>>
    ).

-file("src/mochi/schema.gleam", 1043).
-spec float_scalar() -> scalar_type().
float_scalar() ->
    _pipe = scalar(<<"Float"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Float scalar type represents signed double-precision fractional values"/utf8>>
    ).

-file("src/mochi/schema.gleam", 1050).
-spec boolean_scalar() -> scalar_type().
boolean_scalar() ->
    _pipe = scalar(<<"Boolean"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Boolean scalar type represents true or false"/utf8>>
    ).

-file("src/mochi/schema.gleam", 1055).
-spec id_scalar() -> scalar_type().
id_scalar() ->
    _pipe = scalar(<<"ID"/utf8>>),
    scalar_description(
        _pipe,
        <<"The ID scalar type represents a unique identifier"/utf8>>
    ).

-file("src/mochi/schema.gleam", 1061).
-spec interface(binary()) -> interface_type().
interface(Name) ->
    {interface_type, Name, none, maps:new(), none}.

-file("src/mochi/schema.gleam", 1070).
-spec interface_description(interface_type(), binary()) -> interface_type().
interface_description(Iface, Desc) ->
    {interface_type,
        erlang:element(2, Iface),
        {some, Desc},
        erlang:element(4, Iface),
        erlang:element(5, Iface)}.

-file("src/mochi/schema.gleam", 1077).
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

-file("src/mochi/schema.gleam", 1087).
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

-file("src/mochi/schema.gleam", 1095).
-spec union(binary()) -> union_type().
union(Name) ->
    {union_type, Name, none, [], none}.

-file("src/mochi/schema.gleam", 1099).
-spec union_description(union_type(), binary()) -> union_type().
union_description(Union_type, Desc) ->
    {union_type,
        erlang:element(2, Union_type),
        {some, Desc},
        erlang:element(4, Union_type),
        erlang:element(5, Union_type)}.

-file("src/mochi/schema.gleam", 1103).
-spec union_member(union_type(), object_type()) -> union_type().
union_member(Union_type, Member) ->
    {union_type,
        erlang:element(2, Union_type),
        erlang:element(3, Union_type),
        [Member | erlang:element(4, Union_type)],
        erlang:element(5, Union_type)}.

-file("src/mochi/schema.gleam", 1107).
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

-file("src/mochi/schema.gleam", 1119).
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
        ),
        erlang:element(7, Schema)}.

-file("src/mochi/schema.gleam", 1127).
?DOC(" Create a new directive definition\n").
-spec directive(binary(), list(directive_location())) -> directive_definition().
directive(Name, Locations) ->
    {directive_definition, Name, none, maps:new(), Locations, false, none}.

-file("src/mochi/schema.gleam", 1142).
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

-file("src/mochi/schema.gleam", 1150).
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

-file("src/mochi/schema.gleam", 1161).
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

-file("src/mochi/schema.gleam", 1166).
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

-file("src/mochi/schema.gleam", 1178).
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

-file("src/mochi/schema.gleam", 1194).
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

-file("src/mochi/schema.gleam", 1210).
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

-file("src/mochi/schema.gleam", 1224).
?DOC(" Get all built-in directives\n").
-spec builtin_directives() -> list(directive_definition()).
builtin_directives() ->
    [skip_directive(), include_directive(), deprecated_directive()].

-file("src/mochi/schema.gleam", 1229).
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
