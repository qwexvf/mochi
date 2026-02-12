-module(mochi@query).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/query.gleam").
-export(['query'/4, query_with_args/6, query_description/2, mutation/6, mutation_description/2, subscription/4, subscription_with_args/6, subscription_description/2, field/5, field_with_args/7, field_description/2, arg/2, arg_with_desc/3, query_to_field_def/1, mutation_to_field_def/1, subscription_to_field_def/1, field_def_to_schema/1, new/0, add_query/2, add_mutation/2, add_subscription/2, add_type/2, add_enum/2, add_interface/2, add_union/2, build/1]).
-export_type([query_def/2, mutation_def/2, subscription_def/2, field_def/3, arg_def/0, no_args/0, schema_builder/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type query_def(ADQG, ADQH) :: {query_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADQG} |
            {error, binary()}),
        fun((ADQG, mochi@schema:execution_context()) -> {ok, ADQH} |
            {error, binary()}),
        fun((ADQH) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type mutation_def(ADQI, ADQJ) :: {mutation_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADQI} |
            {error, binary()}),
        fun((ADQI, mochi@schema:execution_context()) -> {ok, ADQJ} |
            {error, binary()}),
        fun((ADQJ) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type subscription_def(ADQK, ADQL) :: {subscription_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADQK} |
            {error, binary()}),
        fun((ADQK, mochi@schema:execution_context()) -> {ok, binary()} |
            {error, binary()}),
        fun((ADQL) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type field_def(ADQM, ADQN, ADQO) :: {field_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dynamic:dynamic_()) -> {ok, ADQM} | {error, binary()}),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADQN} |
            {error, binary()}),
        fun((ADQM, ADQN, mochi@schema:execution_context()) -> {ok, ADQO} |
            {error, binary()}),
        fun((ADQO) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type arg_def() :: {arg_def,
        binary(),
        mochi@schema:field_type(),
        gleam@option:option(binary())}.

-type no_args() :: no_args.

-type schema_builder() :: {schema_builder,
        list(mochi@schema:field_definition()),
        list(mochi@schema:field_definition()),
        list(mochi@schema:field_definition()),
        list(mochi@schema:object_type()),
        list(mochi@schema:enum_type()),
        list(mochi@schema:interface_type()),
        list(mochi@schema:union_type())}.

-file("src/mochi/query.gleam", 88).
?DOC(" Define a query with no arguments\n").
-spec 'query'(
    binary(),
    mochi@schema:field_type(),
    fun((mochi@schema:execution_context()) -> {ok, ADQP} | {error, binary()}),
    fun((ADQP) -> gleam@dynamic:dynamic_())
) -> query_def(no_args(), ADQP).
'query'(Name, Return_type, Resolver, Encoder) ->
    {query_def,
        Name,
        none,
        fun(_) -> {ok, no_args} end,
        fun(_, Ctx) -> Resolver(Ctx) end,
        Encoder,
        [],
        Return_type}.

-file("src/mochi/query.gleam", 106).
?DOC(" Define a query with arguments\n").
-spec query_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADQX} |
        {error, binary()}),
    fun((ADQX, mochi@schema:execution_context()) -> {ok, ADRA} |
        {error, binary()}),
    fun((ADRA) -> gleam@dynamic:dynamic_())
) -> query_def(ADQX, ADRA).
query_with_args(Name, Arg_defs, Return_type, Args_decoder, Resolver, Encoder) ->
    {query_def,
        Name,
        none,
        Args_decoder,
        Resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 126).
?DOC(" Add description to a query\n").
-spec query_description(query_def(ADRF, ADRG), binary()) -> query_def(ADRF, ADRG).
query_description(Q, Desc) ->
    {query_def,
        erlang:element(2, Q),
        {some, Desc},
        erlang:element(4, Q),
        erlang:element(5, Q),
        erlang:element(6, Q),
        erlang:element(7, Q),
        erlang:element(8, Q)}.

-file("src/mochi/query.gleam", 138).
?DOC(" Define a mutation with arguments\n").
-spec mutation(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADRO} |
        {error, binary()}),
    fun((ADRO, mochi@schema:execution_context()) -> {ok, ADRR} |
        {error, binary()}),
    fun((ADRR) -> gleam@dynamic:dynamic_())
) -> mutation_def(ADRO, ADRR).
mutation(Name, Arg_defs, Return_type, Args_decoder, Resolver, Encoder) ->
    {mutation_def,
        Name,
        none,
        Args_decoder,
        Resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 158).
?DOC(" Add description to a mutation\n").
-spec mutation_description(mutation_def(ADRW, ADRX), binary()) -> mutation_def(ADRW, ADRX).
mutation_description(M, Desc) ->
    {mutation_def,
        erlang:element(2, M),
        {some, Desc},
        erlang:element(4, M),
        erlang:element(5, M),
        erlang:element(6, M),
        erlang:element(7, M),
        erlang:element(8, M)}.

-file("src/mochi/query.gleam", 170).
?DOC(" Define a subscription with no arguments\n").
-spec subscription(
    binary(),
    mochi@schema:field_type(),
    binary(),
    fun((ADSC) -> gleam@dynamic:dynamic_())
) -> subscription_def(no_args(), ADSC).
subscription(Name, Return_type, Topic, Encoder) ->
    {subscription_def,
        Name,
        none,
        fun(_) -> {ok, no_args} end,
        fun(_, _) -> {ok, Topic} end,
        Encoder,
        [],
        Return_type}.

-file("src/mochi/query.gleam", 188).
?DOC(" Define a subscription with arguments\n").
-spec subscription_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADSI} |
        {error, binary()}),
    fun((ADSI, mochi@schema:execution_context()) -> {ok, binary()} |
        {error, binary()}),
    fun((ADSN) -> gleam@dynamic:dynamic_())
) -> subscription_def(ADSI, ADSN).
subscription_with_args(
    Name,
    Arg_defs,
    Return_type,
    Args_decoder,
    Topic_resolver,
    Encoder
) ->
    {subscription_def,
        Name,
        none,
        Args_decoder,
        Topic_resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 208).
?DOC(" Add description to a subscription\n").
-spec subscription_description(subscription_def(ADSQ, ADSR), binary()) -> subscription_def(ADSQ, ADSR).
subscription_description(S, Desc) ->
    {subscription_def,
        erlang:element(2, S),
        {some, Desc},
        erlang:element(4, S),
        erlang:element(5, S),
        erlang:element(6, S),
        erlang:element(7, S),
        erlang:element(8, S)}.

-file("src/mochi/query.gleam", 220).
?DOC(" Define a field on a type\n").
-spec field(
    binary(),
    mochi@schema:field_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, ADSW} | {error, binary()}),
    fun((ADSW, mochi@schema:execution_context()) -> {ok, ADSZ} |
        {error, binary()}),
    fun((ADSZ) -> gleam@dynamic:dynamic_())
) -> field_def(ADSW, no_args(), ADSZ).
field(Name, Return_type, Parent_decoder, Resolver, Encoder) ->
    {field_def,
        Name,
        none,
        Parent_decoder,
        fun(_) -> {ok, no_args} end,
        fun(Parent, _, Ctx) -> Resolver(Parent, Ctx) end,
        Encoder,
        [],
        Return_type}.

-file("src/mochi/query.gleam", 240).
?DOC(" Define a field with arguments\n").
-spec field_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, ADTG} | {error, binary()}),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, ADTL} |
        {error, binary()}),
    fun((ADTG, ADTL, mochi@schema:execution_context()) -> {ok, ADTO} |
        {error, binary()}),
    fun((ADTO) -> gleam@dynamic:dynamic_())
) -> field_def(ADTG, ADTL, ADTO).
field_with_args(
    Name,
    Arg_defs,
    Return_type,
    Parent_decoder,
    Args_decoder,
    Resolver,
    Encoder
) ->
    {field_def,
        Name,
        none,
        Parent_decoder,
        Args_decoder,
        Resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 262).
?DOC(" Add description to a field\n").
-spec field_description(field_def(ADTU, ADTV, ADTW), binary()) -> field_def(ADTU, ADTV, ADTW).
field_description(F, Desc) ->
    {field_def,
        erlang:element(2, F),
        {some, Desc},
        erlang:element(4, F),
        erlang:element(5, F),
        erlang:element(6, F),
        erlang:element(7, F),
        erlang:element(8, F),
        erlang:element(9, F)}.

-file("src/mochi/query.gleam", 274).
?DOC(" Create an argument definition\n").
-spec arg(binary(), mochi@schema:field_type()) -> arg_def().
arg(Name, Arg_type) ->
    {arg_def, Name, Arg_type, none}.

-file("src/mochi/query.gleam", 279).
?DOC(" Create an argument with description\n").
-spec arg_with_desc(binary(), mochi@schema:field_type(), binary()) -> arg_def().
arg_with_desc(Name, Arg_type, Description) ->
    {arg_def, Name, Arg_type, {some, Description}}.

-file("src/mochi/query.gleam", 398).
-spec add_args_to_field(mochi@schema:field_definition(), list(arg_def())) -> mochi@schema:field_definition().
add_args_to_field(Field, Args) ->
    gleam@list:fold(
        Args,
        Field,
        fun(F, A) ->
            Arg_def = case erlang:element(4, A) of
                {some, Desc} ->
                    _pipe = mochi@schema:arg(
                        erlang:element(2, A),
                        erlang:element(3, A)
                    ),
                    mochi@schema:arg_description(_pipe, Desc);

                none ->
                    mochi@schema:arg(erlang:element(2, A), erlang:element(3, A))
            end,
            mochi@schema:argument(F, Arg_def)
        end
    ).

-file("src/mochi/query.gleam", 292).
?DOC(" Convert a QueryDef to a schema FieldDefinition\n").
-spec query_to_field_def(query_def(any(), any())) -> mochi@schema:field_definition().
query_to_field_def(Q) ->
    Resolver = fun(Info) ->
        case (erlang:element(4, Q))(erlang:element(3, Info)) of
            {ok, Decoded_args} ->
                case (erlang:element(5, Q))(
                    Decoded_args,
                    erlang:element(4, Info)
                ) of
                    {ok, Result} ->
                        {ok, (erlang:element(6, Q))(Result)};

                    {error, E} ->
                        {error, E}
                end;

            {error, E@1} ->
                {error, <<"Failed to decode arguments: "/utf8, E@1/binary>>}
        end
    end,
    Base = begin
        _pipe = mochi@schema:field_def(
            erlang:element(2, Q),
            erlang:element(8, Q)
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    With_args = add_args_to_field(Base, erlang:element(7, Q)),
    case erlang:element(3, Q) of
        {some, Desc} ->
            mochi@schema:field_description(With_args, Desc);

        none ->
            With_args
    end.

-file("src/mochi/query.gleam", 317).
?DOC(" Convert a MutationDef to a schema FieldDefinition\n").
-spec mutation_to_field_def(mutation_def(any(), any())) -> mochi@schema:field_definition().
mutation_to_field_def(M) ->
    Resolver = fun(Info) ->
        case (erlang:element(4, M))(erlang:element(3, Info)) of
            {ok, Decoded_args} ->
                case (erlang:element(5, M))(
                    Decoded_args,
                    erlang:element(4, Info)
                ) of
                    {ok, Result} ->
                        {ok, (erlang:element(6, M))(Result)};

                    {error, E} ->
                        {error, E}
                end;

            {error, E@1} ->
                {error, <<"Failed to decode arguments: "/utf8, E@1/binary>>}
        end
    end,
    Base = begin
        _pipe = mochi@schema:field_def(
            erlang:element(2, M),
            erlang:element(8, M)
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    With_args = add_args_to_field(Base, erlang:element(7, M)),
    case erlang:element(3, M) of
        {some, Desc} ->
            mochi@schema:field_description(With_args, Desc);

        none ->
            With_args
    end.

-file("src/mochi/query.gleam", 344).
?DOC(
    " Convert a SubscriptionDef to a schema FieldDefinition\n"
    " Note: The actual subscription logic is handled by the subscription executor,\n"
    " this just creates the schema definition for introspection and SDL generation\n"
).
-spec subscription_to_field_def(subscription_def(any(), any())) -> mochi@schema:field_definition().
subscription_to_field_def(S) ->
    Resolver = fun(_) ->
        {error,
            <<"Subscriptions must be executed through the subscription executor"/utf8>>}
    end,
    Base = begin
        _pipe = mochi@schema:field_def(
            erlang:element(2, S),
            erlang:element(8, S)
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    With_args = add_args_to_field(Base, erlang:element(7, S)),
    case erlang:element(3, S) of
        {some, Desc} ->
            mochi@schema:field_description(With_args, Desc);

        none ->
            With_args
    end.

-file("src/mochi/query.gleam", 366).
?DOC(" Convert a FieldDef to a schema FieldDefinition\n").
-spec field_def_to_schema(field_def(any(), any(), any())) -> mochi@schema:field_definition().
field_def_to_schema(F) ->
    Resolver = fun(Info) -> case erlang:element(2, Info) of
            {some, Parent_dyn} ->
                case (erlang:element(4, F))(Parent_dyn) of
                    {ok, Parent} ->
                        case (erlang:element(5, F))(erlang:element(3, Info)) of
                            {ok, Decoded_args} ->
                                case (erlang:element(6, F))(
                                    Parent,
                                    Decoded_args,
                                    erlang:element(4, Info)
                                ) of
                                    {ok, Result} ->
                                        {ok, (erlang:element(7, F))(Result)};

                                    {error, E} ->
                                        {error, E}
                                end;

                            {error, E@1} ->
                                {error,
                                    <<"Failed to decode arguments: "/utf8,
                                        E@1/binary>>}
                        end;

                    {error, E@2} ->
                        {error,
                            <<"Failed to decode parent: "/utf8, E@2/binary>>}
                end;

            none ->
                {error, <<"No parent value provided"/utf8>>}
        end end,
    Base = begin
        _pipe = mochi@schema:field_def(
            erlang:element(2, F),
            erlang:element(9, F)
        ),
        mochi@schema:resolver(_pipe, Resolver)
    end,
    With_args = add_args_to_field(Base, erlang:element(8, F)),
    case erlang:element(3, F) of
        {some, Desc} ->
            mochi@schema:field_description(With_args, Desc);

        none ->
            With_args
    end.

-file("src/mochi/query.gleam", 431).
?DOC(" Create a new schema builder\n").
-spec new() -> schema_builder().
new() ->
    {schema_builder, [], [], [], [], [], [], []}.

-file("src/mochi/query.gleam", 444).
?DOC(" Add a query to the schema\n").
-spec add_query(schema_builder(), query_def(any(), any())) -> schema_builder().
add_query(Builder, Q) ->
    {schema_builder,
        [query_to_field_def(Q) | erlang:element(2, Builder)],
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 452).
?DOC(" Add a mutation to the schema\n").
-spec add_mutation(schema_builder(), mutation_def(any(), any())) -> schema_builder().
add_mutation(Builder, M) ->
    {schema_builder,
        erlang:element(2, Builder),
        [mutation_to_field_def(M) | erlang:element(3, Builder)],
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 463).
?DOC(" Add a subscription to the schema\n").
-spec add_subscription(schema_builder(), subscription_def(any(), any())) -> schema_builder().
add_subscription(Builder, S) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        [subscription_to_field_def(S) | erlang:element(4, Builder)],
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 474).
?DOC(" Add a type definition\n").
-spec add_type(schema_builder(), mochi@schema:object_type()) -> schema_builder().
add_type(Builder, T) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        [T | erlang:element(5, Builder)],
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 479).
?DOC(" Add an enum type\n").
-spec add_enum(schema_builder(), mochi@schema:enum_type()) -> schema_builder().
add_enum(Builder, E) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        [E | erlang:element(6, Builder)],
        erlang:element(7, Builder),
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 484).
?DOC(" Add an interface type\n").
-spec add_interface(schema_builder(), mochi@schema:interface_type()) -> schema_builder().
add_interface(Builder, I) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        [I | erlang:element(7, Builder)],
        erlang:element(8, Builder)}.

-file("src/mochi/query.gleam", 492).
?DOC(" Add a union type\n").
-spec add_union(schema_builder(), mochi@schema:union_type()) -> schema_builder().
add_union(Builder, U) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        [U | erlang:element(8, Builder)]}.

-file("src/mochi/query.gleam", 497).
?DOC(" Build the final schema\n").
-spec build(schema_builder()) -> mochi@schema:schema().
build(Builder) ->
    Query_type = gleam@list:fold(
        erlang:element(2, Builder),
        mochi@schema:object(<<"Query"/utf8>>),
        fun(Obj, Field) -> mochi@schema:field(Obj, Field) end
    ),
    Base_schema = begin
        _pipe = mochi@schema:schema(),
        mochi@schema:'query'(_pipe, Query_type)
    end,
    With_mutation = case erlang:element(3, Builder) of
        [] ->
            Base_schema;

        _ ->
            Mutation_type = gleam@list:fold(
                erlang:element(3, Builder),
                mochi@schema:object(<<"Mutation"/utf8>>),
                fun(Obj@1, Field@1) -> mochi@schema:field(Obj@1, Field@1) end
            ),
            mochi@schema:mutation(Base_schema, Mutation_type)
    end,
    With_subscription = case erlang:element(4, Builder) of
        [] ->
            With_mutation;

        _ ->
            Subscription_type = gleam@list:fold(
                erlang:element(4, Builder),
                mochi@schema:object(<<"Subscription"/utf8>>),
                fun(Obj@2, Field@2) -> mochi@schema:field(Obj@2, Field@2) end
            ),
            mochi@schema:subscription(With_mutation, Subscription_type)
    end,
    With_types = gleam@list:fold(
        erlang:element(5, Builder),
        With_subscription,
        fun(S, T) -> mochi@schema:add_type(S, {object_type_def, T}) end
    ),
    With_enums = gleam@list:fold(
        erlang:element(6, Builder),
        With_types,
        fun(S@1, E) -> mochi@schema:add_type(S@1, {enum_type_def, E}) end
    ),
    With_interfaces = gleam@list:fold(
        erlang:element(7, Builder),
        With_enums,
        fun(S@2, I) -> mochi@schema:add_type(S@2, {interface_type_def, I}) end
    ),
    gleam@list:fold(
        erlang:element(8, Builder),
        With_interfaces,
        fun(S@3, U) -> mochi@schema:add_type(S@3, {union_type_def, U}) end
    ).
