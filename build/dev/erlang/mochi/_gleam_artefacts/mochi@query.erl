-module(mochi@query).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/query.gleam").
-export(['query'/4, query_with_args/6, query_description/2, with_guard/2, mutation/6, mutation_description/2, mutation_with_guard/2, subscription/4, subscription_with_args/6, subscription_description/2, subscription_with_guard/2, field/5, field_with_args/7, field_description/2, field_with_guard/2, all_of/1, any_of/1, arg/2, arg_with_desc/3, arg_with_default/3, arg_with_default_desc/4, get_string/2, get_id/2, get_int/2, get_float/2, get_bool/2, get_optional_string/2, get_optional_id/2, get_optional_int/2, get_optional_float/2, get_optional_bool/2, get_string_list/2, get_int_list/2, decode_input/3, get_dynamic/2, get_optional_dynamic/2, get_string_or/3, get_id_or/3, get_int_or/3, get_float_or/3, get_bool_or/3, query_to_field_def/1, mutation_to_field_def/1, subscription_to_field_def/1, field_def_to_schema/1, new/0, add_query/2, add_mutation/2, add_subscription/2, add_type/2, add_enum/2, add_interface/2, add_union/2, add_scalar/2, add_input/2, add_queries/2, add_mutations/2, add_subscriptions/2, add_types/2, add_enums/2, add_interfaces/2, add_unions/2, add_scalars/2, add_inputs/2, merge/2, with_cache/1, build/1, build_without_cache/1]).
-export_type([query_def/2, mutation_def/2, subscription_def/2, field_def/3, arg_def/0, no_args/0, schema_builder/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type query_def(FUR, FUS) :: {query_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FUR} |
            {error, binary()}),
        fun((FUR, mochi@schema:execution_context()) -> {ok, FUS} |
            {error, binary()}),
        fun((FUS) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type mutation_def(FUT, FUU) :: {mutation_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FUT} |
            {error, binary()}),
        fun((FUT, mochi@schema:execution_context()) -> {ok, FUU} |
            {error, binary()}),
        fun((FUU) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type subscription_def(FUV, FUW) :: {subscription_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FUV} |
            {error, binary()}),
        fun((FUV, mochi@schema:execution_context()) -> {ok, binary()} |
            {error, binary()}),
        fun((FUW) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type field_def(FUX, FUY, FUZ) :: {field_def,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dynamic:dynamic_()) -> {ok, FUX} | {error, binary()}),
        fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FUY} |
            {error, binary()}),
        fun((FUX, FUY, mochi@schema:execution_context()) -> {ok, FUZ} |
            {error, binary()}),
        fun((FUZ) -> gleam@dynamic:dynamic_()),
        list(arg_def()),
        mochi@schema:field_type()}.

-type arg_def() :: {arg_def,
        binary(),
        mochi@schema:field_type(),
        gleam@option:option(binary()),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-type no_args() :: no_args.

-type schema_builder() :: {schema_builder,
        list(mochi@schema:field_definition()),
        list(mochi@schema:field_definition()),
        list(mochi@schema:field_definition()),
        list(mochi@schema:object_type()),
        list(mochi@schema:enum_type()),
        list(mochi@schema:interface_type()),
        list(mochi@schema:union_type()),
        list(mochi@schema:scalar_type()),
        list(mochi@schema:input_object_type()),
        boolean()}.

-file("src/mochi/query.gleam", 97).
?DOC(" Define a query with no arguments\n").
-spec 'query'(
    binary(),
    mochi@schema:field_type(),
    fun((mochi@schema:execution_context()) -> {ok, FVC} | {error, binary()}),
    fun((FVC) -> gleam@dynamic:dynamic_())
) -> query_def(no_args(), FVC).
'query'(Name, Return_type, Resolver, Encoder) ->
    {query_def,
        Name,
        none,
        fun(_) -> {ok, no_args} end,
        fun(_, Ctx) -> Resolver(Ctx) end,
        Encoder,
        [],
        Return_type}.

-file("src/mochi/query.gleam", 127).
?DOC(
    " Define a query with arguments\n"
    "\n"
    " Uses labeled arguments for clarity:\n"
    " ```gleam\n"
    " query.query_with_args(\n"
    "   name: \"user\",\n"
    "   args: [query.arg(\"id\", schema.non_null(schema.id_type()))],\n"
    "   returns: schema.named_type(\"User\"),\n"
    "   decode: fn(args) { get_id(args, \"id\") },\n"
    "   resolve: fn(id, ctx) { get_user_by_id(id) },\n"
    "   encode: fn(user) { types.to_dynamic(user) },\n"
    " )\n"
    " ```\n"
).
-spec query_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FVK} |
        {error, binary()}),
    fun((FVK, mochi@schema:execution_context()) -> {ok, FVN} | {error, binary()}),
    fun((FVN) -> gleam@dynamic:dynamic_())
) -> query_def(FVK, FVN).
query_with_args(Name, Arg_defs, Return_type, Args_decoder, Resolver, Encoder) ->
    {query_def,
        Name,
        none,
        Args_decoder,
        Resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 147).
?DOC(" Add description to a query\n").
-spec query_description(query_def(FVS, FVT), binary()) -> query_def(FVS, FVT).
query_description(Q, Desc) ->
    {query_def,
        erlang:element(2, Q),
        {some, Desc},
        erlang:element(4, Q),
        erlang:element(5, Q),
        erlang:element(6, Q),
        erlang:element(7, Q),
        erlang:element(8, Q)}.

-file("src/mochi/query.gleam", 166).
?DOC(
    " Add a guard to a query. The guard runs before the resolver — if it returns\n"
    " Error, the resolver is skipped and the error is returned.\n"
    " Multiple guards can be stacked by piping; the last guard added is checked\n"
    " first (outermost wrapper), so place the broadest checks last:\n"
    "\n"
    " ```gleam\n"
    " let my_posts = query.query_with_args(\n"
    "   name: \"myPosts\", ...\n"
    " )\n"
    " |> query.with_guard(require_admin)  // checked second\n"
    " |> query.with_guard(require_auth)   // checked first (outermost)\n"
    " ```\n"
).
-spec with_guard(
    query_def(FVY, FVZ),
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> query_def(FVY, FVZ).
with_guard(Q, Guard_fn) ->
    Original = erlang:element(5, Q),
    {query_def,
        erlang:element(2, Q),
        erlang:element(3, Q),
        erlang:element(4, Q),
        fun(Args, Ctx) -> case Guard_fn(Ctx) of
                {ok, nil} ->
                    Original(Args, Ctx);

                {error, Msg} ->
                    {error, Msg}
            end end,
        erlang:element(6, Q),
        erlang:element(7, Q),
        erlang:element(8, Q)}.

-file("src/mochi/query.gleam", 196).
?DOC(
    " Define a mutation with arguments\n"
    "\n"
    " Uses labeled arguments for clarity:\n"
    " ```gleam\n"
    " query.mutation(\n"
    "   name: \"createUser\",\n"
    "   args: [query.arg(\"input\", schema.non_null(schema.named_type(\"CreateUserInput\")))],\n"
    "   returns: schema.named_type(\"User\"),\n"
    "   decode: fn(args) { decode_create_user_input(args) },\n"
    "   resolve: fn(input, ctx) { create_user(input) },\n"
    "   encode: fn(user) { types.to_dynamic(user) },\n"
    " )\n"
    " ```\n"
).
-spec mutation(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FWJ} |
        {error, binary()}),
    fun((FWJ, mochi@schema:execution_context()) -> {ok, FWM} | {error, binary()}),
    fun((FWM) -> gleam@dynamic:dynamic_())
) -> mutation_def(FWJ, FWM).
mutation(Name, Arg_defs, Return_type, Args_decoder, Resolver, Encoder) ->
    {mutation_def,
        Name,
        none,
        Args_decoder,
        Resolver,
        Encoder,
        Arg_defs,
        Return_type}.

-file("src/mochi/query.gleam", 216).
?DOC(" Add description to a mutation\n").
-spec mutation_description(mutation_def(FWR, FWS), binary()) -> mutation_def(FWR, FWS).
mutation_description(M, Desc) ->
    {mutation_def,
        erlang:element(2, M),
        {some, Desc},
        erlang:element(4, M),
        erlang:element(5, M),
        erlang:element(6, M),
        erlang:element(7, M),
        erlang:element(8, M)}.

-file("src/mochi/query.gleam", 226).
?DOC(
    " Add a guard to a mutation. The guard runs before the resolver — if it returns\n"
    " Error, the resolver is skipped and the error is returned.\n"
    " Multiple guards can be stacked by piping; the last guard added is checked first.\n"
).
-spec mutation_with_guard(
    mutation_def(FWX, FWY),
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> mutation_def(FWX, FWY).
mutation_with_guard(M, Guard_fn) ->
    Original = erlang:element(5, M),
    {mutation_def,
        erlang:element(2, M),
        erlang:element(3, M),
        erlang:element(4, M),
        fun(Args, Ctx) -> case Guard_fn(Ctx) of
                {ok, nil} ->
                    Original(Args, Ctx);

                {error, Msg} ->
                    {error, Msg}
            end end,
        erlang:element(6, M),
        erlang:element(7, M),
        erlang:element(8, M)}.

-file("src/mochi/query.gleam", 244).
?DOC(" Define a subscription with no arguments\n").
-spec subscription(
    binary(),
    mochi@schema:field_type(),
    binary(),
    fun((FXF) -> gleam@dynamic:dynamic_())
) -> subscription_def(no_args(), FXF).
subscription(Name, Return_type, Topic, Encoder) ->
    {subscription_def,
        Name,
        none,
        fun(_) -> {ok, no_args} end,
        fun(_, _) -> {ok, Topic} end,
        Encoder,
        [],
        Return_type}.

-file("src/mochi/query.gleam", 274).
?DOC(
    " Define a subscription with arguments\n"
    "\n"
    " Uses labeled arguments for clarity:\n"
    " ```gleam\n"
    " query.subscription_with_args(\n"
    "   name: \"onMessage\",\n"
    "   args: [query.arg(\"channelId\", schema.non_null(schema.id_type()))],\n"
    "   returns: schema.named_type(\"Message\"),\n"
    "   decode: fn(args) { get_id(args, \"channelId\") },\n"
    "   topic: fn(channel_id, ctx) { Ok(\"channel:\" <> channel_id) },\n"
    "   encode: fn(msg) { types.to_dynamic(msg) },\n"
    " )\n"
    " ```\n"
).
-spec subscription_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FXL} |
        {error, binary()}),
    fun((FXL, mochi@schema:execution_context()) -> {ok, binary()} |
        {error, binary()}),
    fun((FXQ) -> gleam@dynamic:dynamic_())
) -> subscription_def(FXL, FXQ).
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

-file("src/mochi/query.gleam", 294).
?DOC(" Add description to a subscription\n").
-spec subscription_description(subscription_def(FXT, FXU), binary()) -> subscription_def(FXT, FXU).
subscription_description(S, Desc) ->
    {subscription_def,
        erlang:element(2, S),
        {some, Desc},
        erlang:element(4, S),
        erlang:element(5, S),
        erlang:element(6, S),
        erlang:element(7, S),
        erlang:element(8, S)}.

-file("src/mochi/query.gleam", 304).
?DOC(
    " Add a guard to a subscription. The guard runs before the topic resolver —\n"
    " if it returns Error, the subscription is rejected.\n"
    " Multiple guards can be stacked by piping; the last guard added is checked first.\n"
).
-spec subscription_with_guard(
    subscription_def(FXZ, FYA),
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> subscription_def(FXZ, FYA).
subscription_with_guard(S, Guard_fn) ->
    Original = erlang:element(5, S),
    {subscription_def,
        erlang:element(2, S),
        erlang:element(3, S),
        erlang:element(4, S),
        fun(Args, Ctx) -> case Guard_fn(Ctx) of
                {ok, nil} ->
                    Original(Args, Ctx);

                {error, Msg} ->
                    {error, Msg}
            end end,
        erlang:element(6, S),
        erlang:element(7, S),
        erlang:element(8, S)}.

-file("src/mochi/query.gleam", 322).
?DOC(" Define a field on a type\n").
-spec field(
    binary(),
    mochi@schema:field_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FYH} | {error, binary()}),
    fun((FYH, mochi@schema:execution_context()) -> {ok, FYK} | {error, binary()}),
    fun((FYK) -> gleam@dynamic:dynamic_())
) -> field_def(FYH, no_args(), FYK).
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

-file("src/mochi/query.gleam", 355).
?DOC(
    " Define a field with arguments\n"
    "\n"
    " Uses labeled arguments for clarity:\n"
    " ```gleam\n"
    " query.field_with_args(\n"
    "   name: \"posts\",\n"
    "   args: [query.arg(\"limit\", schema.int_type())],\n"
    "   returns: schema.list_type(schema.named_type(\"Post\")),\n"
    "   parent: decode_user,\n"
    "   decode: fn(args) { get_optional_int(args, \"limit\") },\n"
    "   resolve: fn(user, limit, ctx) { get_user_posts(user.id, limit) },\n"
    "   encode: fn(posts) { types.to_dynamic(posts) },\n"
    " )\n"
    " ```\n"
).
-spec field_with_args(
    binary(),
    list(arg_def()),
    mochi@schema:field_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FYR} | {error, binary()}),
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, FYW} |
        {error, binary()}),
    fun((FYR, FYW, mochi@schema:execution_context()) -> {ok, FYZ} |
        {error, binary()}),
    fun((FYZ) -> gleam@dynamic:dynamic_())
) -> field_def(FYR, FYW, FYZ).
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

-file("src/mochi/query.gleam", 377).
?DOC(" Add description to a field\n").
-spec field_description(field_def(FZF, FZG, FZH), binary()) -> field_def(FZF, FZG, FZH).
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

-file("src/mochi/query.gleam", 387).
?DOC(
    " Add a guard to a field definition. The guard runs before the resolver — if it\n"
    " returns Error, the resolver is skipped and the error is returned.\n"
    " Multiple guards can be stacked by piping; the last guard added is checked first.\n"
).
-spec field_with_guard(
    field_def(FZO, FZP, FZQ),
    fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})
) -> field_def(FZO, FZP, FZQ).
field_with_guard(F, Guard_fn) ->
    Original = erlang:element(6, F),
    {field_def,
        erlang:element(2, F),
        erlang:element(3, F),
        erlang:element(4, F),
        erlang:element(5, F),
        fun(Parent, Args, Ctx) -> case Guard_fn(Ctx) of
                {ok, nil} ->
                    Original(Parent, Args, Ctx);

                {error, Msg} ->
                    {error, Msg}
            end end,
        erlang:element(7, F),
        erlang:element(8, F),
        erlang:element(9, F)}.

-file("src/mochi/query.gleam", 409).
?DOC(" Combine guards with AND logic — all must pass (checked in list order).\n").
-spec all_of(
    list(fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()}))
) -> fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()}).
all_of(Guard_fns) ->
    fun(Ctx) -> gleam@list:try_each(Guard_fns, fun(G) -> G(Ctx) end) end.

-file("src/mochi/query.gleam", 424).
-spec try_any(
    list(fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()})),
    mochi@schema:execution_context(),
    binary()
) -> {ok, nil} | {error, binary()}.
try_any(Guards, Ctx, Last_error) ->
    case Guards of
        [] ->
            {error, Last_error};

        [G | Rest] ->
            case G(Ctx) of
                {ok, nil} ->
                    {ok, nil};

                {error, E} ->
                    try_any(Rest, Ctx, E)
            end
    end.

-file("src/mochi/query.gleam", 415).
?DOC(
    " Combine guards with OR logic — at least one must pass.\n"
    " Fails with the last error if all fail.\n"
).
-spec any_of(
    list(fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()}))
) -> fun((mochi@schema:execution_context()) -> {ok, nil} | {error, binary()}).
any_of(Guard_fns) ->
    fun(Ctx) -> case Guard_fns of
            [] ->
                {error, <<"No guards provided"/utf8>>};

            _ ->
                try_any(Guard_fns, Ctx, <<"No guards passed"/utf8>>)
        end end.

-file("src/mochi/query.gleam", 444).
?DOC(" Create an argument definition\n").
-spec arg(binary(), mochi@schema:field_type()) -> arg_def().
arg(Name, Arg_type) ->
    {arg_def, Name, Arg_type, none, none}.

-file("src/mochi/query.gleam", 449).
?DOC(" Create an argument with description\n").
-spec arg_with_desc(binary(), mochi@schema:field_type(), binary()) -> arg_def().
arg_with_desc(Name, Arg_type, Description) ->
    {arg_def, Name, Arg_type, {some, Description}, none}.

-file("src/mochi/query.gleam", 467).
?DOC(
    " Create an argument with a default value\n"
    "\n"
    " ```gleam\n"
    " query.arg_with_default(\"limit\", schema.int_type(), types.to_dynamic(10))\n"
    " ```\n"
).
-spec arg_with_default(
    binary(),
    mochi@schema:field_type(),
    gleam@dynamic:dynamic_()
) -> arg_def().
arg_with_default(Name, Arg_type, Default) ->
    {arg_def, Name, Arg_type, none, {some, Default}}.

-file("src/mochi/query.gleam", 485).
?DOC(
    " Create an argument with default value and description\n"
    "\n"
    " ```gleam\n"
    " query.arg_with_default_desc(\"limit\", schema.int_type(), types.to_dynamic(10), \"Maximum items to return\")\n"
    " ```\n"
).
-spec arg_with_default_desc(
    binary(),
    mochi@schema:field_type(),
    gleam@dynamic:dynamic_(),
    binary()
) -> arg_def().
arg_with_default_desc(Name, Arg_type, Default, Description) ->
    {arg_def, Name, Arg_type, {some, Description}, {some, Default}}.

-file("src/mochi/query.gleam", 503).
-spec fetch_required(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    gleam@dynamic@decode:decoder(GAG),
    binary()
) -> {ok, GAG} | {error, binary()}.
fetch_required(Args, Key, Decoder, Type_label) ->
    case gleam_stdlib:map_get(Args, Key) of
        {error, _} ->
            {error, <<"Missing required argument: "/utf8, Key/binary>>};

        {ok, Value} ->
            _pipe = gleam@dynamic@decode:run(Value, Decoder),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<<<<<"Invalid type for argument '"/utf8, Key/binary>>/binary,
                            "': expected "/utf8>>/binary,
                        Type_label/binary>>
                end
            )
    end.

-file("src/mochi/query.gleam", 519).
-spec fetch_optional(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    gleam@dynamic@decode:decoder(GAM)
) -> gleam@option:option(GAM).
fetch_optional(Args, Key, Decoder) ->
    case gleam_stdlib:map_get(Args, Key) of
        {error, _} ->
            none;

        {ok, Value} ->
            case gleam@dynamic@decode:run(Value, Decoder) of
                {ok, V} ->
                    {some, V};

                {error, _} ->
                    none
            end
    end.

-file("src/mochi/query.gleam", 534).
-spec fetch_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    gleam@dynamic@decode:decoder(GAR),
    GAR
) -> GAR.
fetch_or(Args, Key, Decoder, Default) ->
    case gleam_stdlib:map_get(Args, Key) of
        {error, _} ->
            Default;

        {ok, Value} ->
            gleam@result:unwrap(
                gleam@dynamic@decode:run(Value, Decoder),
                Default
            )
    end.

-file("src/mochi/query.gleam", 554).
?DOC(
    " Get a required string argument from the arguments dict\n"
    "\n"
    " ```gleam\n"
    " fn decode_args(args) {\n"
    "   use name <- result.try(query.get_string(args, \"name\"))\n"
    "   Ok(name)\n"
    " }\n"
    " ```\n"
).
-spec get_string(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        binary()} |
    {error, binary()}.
get_string(Args, Key) ->
    fetch_required(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        <<"String"/utf8>>
    ).

-file("src/mochi/query.gleam", 569).
?DOC(
    " Get a required ID argument (as String) from the arguments dict\n"
    "\n"
    " ```gleam\n"
    " fn decode_args(args) {\n"
    "   use id <- result.try(query.get_id(args, \"id\"))\n"
    "   Ok(id)\n"
    " }\n"
    " ```\n"
).
-spec get_id(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        binary()} |
    {error, binary()}.
get_id(Args, Key) ->
    fetch_required(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        <<"ID"/utf8>>
    ).

-file("src/mochi/query.gleam", 584).
?DOC(
    " Get a required int argument from the arguments dict\n"
    "\n"
    " ```gleam\n"
    " fn decode_args(args) {\n"
    "   use age <- result.try(query.get_int(args, \"age\"))\n"
    "   Ok(age)\n"
    " }\n"
    " ```\n"
).
-spec get_int(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        integer()} |
    {error, binary()}.
get_int(Args, Key) ->
    fetch_required(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        <<"Int"/utf8>>
    ).

-file("src/mochi/query.gleam", 589).
?DOC(" Get a required float argument from the arguments dict\n").
-spec get_float(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        float()} |
    {error, binary()}.
get_float(Args, Key) ->
    fetch_required(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        <<"Float"/utf8>>
    ).

-file("src/mochi/query.gleam", 597).
?DOC(" Get a required bool argument from the arguments dict\n").
-spec get_bool(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        boolean()} |
    {error, binary()}.
get_bool(Args, Key) ->
    fetch_required(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_bool/1},
        <<"Bool"/utf8>>
    ).

-file("src/mochi/query.gleam", 612).
?DOC(
    " Get an optional string argument from the arguments dict\n"
    "\n"
    " ```gleam\n"
    " fn decode_args(args) {\n"
    "   let name = query.get_optional_string(args, \"name\")\n"
    "   Ok(SearchArgs(name: name))\n"
    " }\n"
    " ```\n"
).
-spec get_optional_string(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(binary()).
get_optional_string(Args, Key) ->
    fetch_optional(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_string/1}
    ).

-file("src/mochi/query.gleam", 620).
?DOC(" Get an optional ID argument from the arguments dict\n").
-spec get_optional_id(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(binary()).
get_optional_id(Args, Key) ->
    get_optional_string(Args, Key).

-file("src/mochi/query.gleam", 628).
?DOC(" Get an optional int argument from the arguments dict\n").
-spec get_optional_int(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(integer()).
get_optional_int(Args, Key) ->
    fetch_optional(Args, Key, {decoder, fun gleam@dynamic@decode:decode_int/1}).

-file("src/mochi/query.gleam", 633).
?DOC(" Get an optional float argument from the arguments dict\n").
-spec get_optional_float(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(float()).
get_optional_float(Args, Key) ->
    fetch_optional(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_float/1}
    ).

-file("src/mochi/query.gleam", 641).
?DOC(" Get an optional bool argument from the arguments dict\n").
-spec get_optional_bool(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(boolean()).
get_optional_bool(Args, Key) ->
    fetch_optional(Args, Key, {decoder, fun gleam@dynamic@decode:decode_bool/1}).

-file("src/mochi/query.gleam", 649).
?DOC(" Get a required list of strings from the arguments dict\n").
-spec get_string_list(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> {ok, list(binary())} | {error, binary()}.
get_string_list(Args, Key) ->
    fetch_required(
        Args,
        Key,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        <<"[String]"/utf8>>
    ).

-file("src/mochi/query.gleam", 657).
?DOC(" Get a required list of ints from the arguments dict\n").
-spec get_int_list(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> {ok, list(integer())} | {error, binary()}.
get_int_list(Args, Key) ->
    fetch_required(
        Args,
        Key,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_int/1}
        ),
        <<"[Int]"/utf8>>
    ).

-file("src/mochi/query.gleam", 664).
-spec decode_input(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    gleam@dynamic@decode:decoder(GCO)
) -> {ok, GCO} | {error, binary()}.
decode_input(Args, Key, Decoder) ->
    case gleam_stdlib:map_get(Args, Key) of
        {error, _} ->
            {error, <<"Missing required argument: "/utf8, Key/binary>>};

        {ok, Value} ->
            _pipe = gleam@dynamic@decode:run(Value, Decoder),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<<<"Invalid input for '"/utf8, Key/binary>>/binary,
                        "'"/utf8>>
                end
            )
    end.

-file("src/mochi/query.gleam", 677).
-spec get_dynamic(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), binary()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
get_dynamic(Args, Key) ->
    _pipe = gleam_stdlib:map_get(Args, Key),
    gleam@result:map_error(
        _pipe,
        fun(_) -> <<"Missing required argument: "/utf8, Key/binary>> end
    ).

-file("src/mochi/query.gleam", 685).
-spec get_optional_dynamic(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary()
) -> gleam@option:option(gleam@dynamic:dynamic_()).
get_optional_dynamic(Args, Key) ->
    _pipe = gleam_stdlib:map_get(Args, Key),
    gleam@option:from_result(_pipe).

-file("src/mochi/query.gleam", 702).
?DOC(
    " Get a string argument or return default if not present\n"
    "\n"
    " ```gleam\n"
    " let name = query.get_string_or(args, \"name\", \"Anonymous\")\n"
    " ```\n"
).
-spec get_string_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    binary()
) -> binary().
get_string_or(Args, Key, Default) ->
    fetch_or(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        Default
    ).

-file("src/mochi/query.gleam", 711).
?DOC(" Get an ID argument or return default if not present\n").
-spec get_id_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    binary()
) -> binary().
get_id_or(Args, Key, Default) ->
    get_string_or(Args, Key, Default).

-file("src/mochi/query.gleam", 724).
?DOC(
    " Get an int argument or return default if not present\n"
    "\n"
    " ```gleam\n"
    " let limit = query.get_int_or(args, \"limit\", 10)\n"
    " ```\n"
).
-spec get_int_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    integer()
) -> integer().
get_int_or(Args, Key, Default) ->
    fetch_or(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        Default
    ).

-file("src/mochi/query.gleam", 733).
?DOC(
    " Get a float argument or return default if not present\n"
    "\n"
    " ```gleam\n"
    " let price = query.get_float_or(args, \"price\", 0.0)\n"
    " ```\n"
).
-spec get_float_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    float()
) -> float().
get_float_or(Args, Key, Default) ->
    fetch_or(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        Default
    ).

-file("src/mochi/query.gleam", 746).
?DOC(
    " Get a bool argument or return default if not present\n"
    "\n"
    " ```gleam\n"
    " let active = query.get_bool_or(args, \"active\", True)\n"
    " ```\n"
).
-spec get_bool_or(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    binary(),
    boolean()
) -> boolean().
get_bool_or(Args, Key, Default) ->
    fetch_or(
        Args,
        Key,
        {decoder, fun gleam@dynamic@decode:decode_bool/1},
        Default
    ).

-file("src/mochi/query.gleam", 758).
-spec build_op_resolver(
    fun((gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> {ok, GDL} |
        {error, binary()}),
    fun((GDL, mochi@schema:execution_context()) -> {ok, GDO} | {error, binary()}),
    fun((GDO) -> gleam@dynamic:dynamic_())
) -> fun((mochi@schema:resolver_info()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, binary()}).
build_op_resolver(Args_decoder, Resolver, Result_encoder) ->
    fun(Info) ->
        gleam@result:'try'(
            begin
                _pipe = Args_decoder(erlang:element(3, Info)),
                gleam@result:map_error(
                    _pipe,
                    fun(E) ->
                        <<"Failed to decode arguments: "/utf8, E/binary>>
                    end
                )
            end,
            fun(Decoded_args) ->
                gleam@result:map(
                    Resolver(Decoded_args, erlang:element(4, Info)),
                    fun(Res) -> Result_encoder(Res) end
                )
            end
        )
    end.

-file("src/mochi/query.gleam", 773).
-spec apply_desc(mochi@schema:field_definition(), gleam@option:option(binary())) -> mochi@schema:field_definition().
apply_desc(Field, Desc) ->
    case Desc of
        {some, D} ->
            mochi@schema:field_description(Field, D);

        none ->
            Field
    end.

-file("src/mochi/query.gleam", 855).
-spec add_args_to_field(mochi@schema:field_definition(), list(arg_def())) -> mochi@schema:field_definition().
add_args_to_field(Field, Args) ->
    gleam@list:fold(
        Args,
        Field,
        fun(F, A) ->
            Base_arg = mochi@schema:arg(
                erlang:element(2, A),
                erlang:element(3, A)
            ),
            With_desc = case erlang:element(4, A) of
                {some, Desc} ->
                    mochi@schema:arg_description(Base_arg, Desc);

                none ->
                    Base_arg
            end,
            Arg_def = case erlang:element(5, A) of
                {some, Default} ->
                    mochi@schema:default_value(With_desc, Default);

                none ->
                    With_desc
            end,
            mochi@schema:argument(F, Arg_def)
        end
    ).

-file("src/mochi/query.gleam", 781).
?DOC(" Convert a QueryDef to a schema FieldDefinition\n").
-spec query_to_field_def(query_def(any(), any())) -> mochi@schema:field_definition().
query_to_field_def(Q) ->
    _pipe = mochi@schema:field_def(erlang:element(2, Q), erlang:element(8, Q)),
    _pipe@1 = mochi@schema:resolver(
        _pipe,
        build_op_resolver(
            erlang:element(4, Q),
            erlang:element(5, Q),
            erlang:element(6, Q)
        )
    ),
    _pipe@2 = add_args_to_field(_pipe@1, erlang:element(7, Q)),
    apply_desc(_pipe@2, erlang:element(3, Q)).

-file("src/mochi/query.gleam", 793).
?DOC(" Convert a MutationDef to a schema FieldDefinition\n").
-spec mutation_to_field_def(mutation_def(any(), any())) -> mochi@schema:field_definition().
mutation_to_field_def(M) ->
    _pipe = mochi@schema:field_def(erlang:element(2, M), erlang:element(8, M)),
    _pipe@1 = mochi@schema:resolver(
        _pipe,
        build_op_resolver(
            erlang:element(4, M),
            erlang:element(5, M),
            erlang:element(6, M)
        )
    ),
    _pipe@2 = add_args_to_field(_pipe@1, erlang:element(7, M)),
    apply_desc(_pipe@2, erlang:element(3, M)).

-file("src/mochi/query.gleam", 807).
?DOC(
    " Convert a SubscriptionDef to a schema FieldDefinition.\n"
    " Actual subscription logic is handled by the subscription executor;\n"
    " this creates the schema definition for introspection and SDL generation.\n"
).
-spec subscription_to_field_def(subscription_def(any(), any())) -> mochi@schema:field_definition().
subscription_to_field_def(S) ->
    Resolver = fun(_) ->
        {error,
            <<"Subscriptions must be executed through the subscription executor"/utf8>>}
    end,
    Topic_fn = {some,
        fun(Raw_args, Ctx) -> case (erlang:element(4, S))(Raw_args) of
                {ok, Decoded_args} ->
                    (erlang:element(5, S))(Decoded_args, Ctx);

                {error, E} ->
                    {error,
                        <<"Failed to decode subscription args: "/utf8,
                            E/binary>>}
            end end},
    _pipe = mochi@schema:field_def(erlang:element(2, S), erlang:element(8, S)),
    _pipe@1 = mochi@schema:resolver(_pipe, Resolver),
    _pipe@2 = (fun(Fd) ->
        {field_definition,
            erlang:element(2, Fd),
            erlang:element(3, Fd),
            erlang:element(4, Fd),
            erlang:element(5, Fd),
            erlang:element(6, Fd),
            erlang:element(7, Fd),
            erlang:element(8, Fd),
            Topic_fn}
    end)(_pipe@1),
    _pipe@3 = add_args_to_field(_pipe@2, erlang:element(7, S)),
    apply_desc(_pipe@3, erlang:element(3, S)).

-file("src/mochi/query.gleam", 831).
?DOC(" Convert a FieldDef to a schema FieldDefinition\n").
-spec field_def_to_schema(field_def(any(), any(), any())) -> mochi@schema:field_definition().
field_def_to_schema(F) ->
    Resolver = fun(Info) ->
        gleam@result:'try'(
            gleam@option:to_result(
                erlang:element(2, Info),
                <<"No parent value provided"/utf8>>
            ),
            fun(Parent_dyn) ->
                gleam@result:'try'(
                    begin
                        _pipe = (erlang:element(4, F))(Parent_dyn),
                        gleam@result:map_error(
                            _pipe,
                            fun(E) ->
                                <<"Failed to decode parent: "/utf8, E/binary>>
                            end
                        )
                    end,
                    fun(Parent) ->
                        gleam@result:'try'(
                            begin
                                _pipe@1 = (erlang:element(5, F))(
                                    erlang:element(3, Info)
                                ),
                                gleam@result:map_error(
                                    _pipe@1,
                                    fun(E@1) ->
                                        <<"Failed to decode arguments: "/utf8,
                                            E@1/binary>>
                                    end
                                )
                            end,
                            fun(Decoded_args) ->
                                gleam@result:map(
                                    (erlang:element(6, F))(
                                        Parent,
                                        Decoded_args,
                                        erlang:element(4, Info)
                                    ),
                                    fun(Res) -> (erlang:element(7, F))(Res) end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    _pipe@2 = mochi@schema:field_def(erlang:element(2, F), erlang:element(9, F)),
    _pipe@3 = mochi@schema:resolver(_pipe@2, Resolver),
    _pipe@4 = add_args_to_field(_pipe@3, erlang:element(8, F)),
    apply_desc(_pipe@4, erlang:element(3, F)).

-file("src/mochi/query.gleam", 897).
?DOC(" Create a new schema builder\n").
-spec new() -> schema_builder().
new() ->
    {schema_builder, [], [], [], [], [], [], [], [], [], false}.

-file("src/mochi/query.gleam", 913).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 921).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 932).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 943).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 948).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 953).
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
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 961).
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
        [U | erlang:element(8, Builder)],
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 966).
?DOC(" Add a custom scalar type (e.g., Upload, DateTime, JSON)\n").
-spec add_scalar(schema_builder(), mochi@schema:scalar_type()) -> schema_builder().
add_scalar(Builder, S) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        [S | erlang:element(9, Builder)],
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 971).
?DOC(" Add an input object type\n").
-spec add_input(schema_builder(), mochi@schema:input_object_type()) -> schema_builder().
add_input(Builder, I) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        [I | erlang:element(10, Builder)],
        erlang:element(11, Builder)}.

-file("src/mochi/query.gleam", 979).
?DOC(" Add multiple queries to the schema\n").
-spec add_queries(schema_builder(), list(query_def(any(), any()))) -> schema_builder().
add_queries(Builder, Queries) ->
    gleam@list:fold(Queries, Builder, fun(B, Q) -> add_query(B, Q) end).

-file("src/mochi/query.gleam", 987).
?DOC(" Add multiple mutations to the schema\n").
-spec add_mutations(schema_builder(), list(mutation_def(any(), any()))) -> schema_builder().
add_mutations(Builder, Mutations) ->
    gleam@list:fold(Mutations, Builder, fun(B, M) -> add_mutation(B, M) end).

-file("src/mochi/query.gleam", 995).
?DOC(" Add multiple subscriptions to the schema\n").
-spec add_subscriptions(schema_builder(), list(subscription_def(any(), any()))) -> schema_builder().
add_subscriptions(Builder, Subscriptions) ->
    gleam@list:fold(
        Subscriptions,
        Builder,
        fun(B, S) -> add_subscription(B, S) end
    ).

-file("src/mochi/query.gleam", 1003).
?DOC(" Add multiple type definitions\n").
-spec add_types(schema_builder(), list(mochi@schema:object_type())) -> schema_builder().
add_types(Builder, Types) ->
    gleam@list:fold(Types, Builder, fun(B, T) -> add_type(B, T) end).

-file("src/mochi/query.gleam", 1011).
?DOC(" Add multiple enum types\n").
-spec add_enums(schema_builder(), list(mochi@schema:enum_type())) -> schema_builder().
add_enums(Builder, Enums) ->
    gleam@list:fold(Enums, Builder, fun(B, E) -> add_enum(B, E) end).

-file("src/mochi/query.gleam", 1019).
?DOC(" Add multiple interface types\n").
-spec add_interfaces(schema_builder(), list(mochi@schema:interface_type())) -> schema_builder().
add_interfaces(Builder, Interfaces) ->
    gleam@list:fold(Interfaces, Builder, fun(B, I) -> add_interface(B, I) end).

-file("src/mochi/query.gleam", 1027).
?DOC(" Add multiple union types\n").
-spec add_unions(schema_builder(), list(mochi@schema:union_type())) -> schema_builder().
add_unions(Builder, Unions) ->
    gleam@list:fold(Unions, Builder, fun(B, U) -> add_union(B, U) end).

-file("src/mochi/query.gleam", 1035).
?DOC(" Add multiple custom scalar types\n").
-spec add_scalars(schema_builder(), list(mochi@schema:scalar_type())) -> schema_builder().
add_scalars(Builder, Scalars) ->
    gleam@list:fold(Scalars, Builder, fun(B, S) -> add_scalar(B, S) end).

-file("src/mochi/query.gleam", 1043).
?DOC(" Add multiple input object types\n").
-spec add_inputs(schema_builder(), list(mochi@schema:input_object_type())) -> schema_builder().
add_inputs(Builder, Inputs) ->
    gleam@list:fold(Inputs, Builder, fun(B, I) -> add_input(B, I) end).

-file("src/mochi/query.gleam", 1050).
-spec check_duplicates(binary(), list(GFS), list(GFS), fun((GFS) -> binary())) -> nil.
check_duplicates(Label, A_items, B_items, Name_fn) ->
    A_names = gleam@list:map(A_items, Name_fn),
    Conflicts = gleam@list:filter(
        B_items,
        fun(Item) -> gleam@list:contains(A_names, Name_fn(Item)) end
    ),
    case Conflicts of
        [] ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => (<<<<<<"Schema merge conflict: duplicate "/utf8,
                                Label/binary>>/binary,
                            " name(s): "/utf8>>/binary,
                        (gleam@string:join(
                            gleam@list:map(Conflicts, Name_fn),
                            <<", "/utf8>>
                        ))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mochi/query"/utf8>>,
                    function => <<"check_duplicates"/utf8>>,
                    line => 1062})
    end.

-file("src/mochi/query.gleam", 1071).
-spec merge(schema_builder(), schema_builder()) -> schema_builder().
merge(A, B) ->
    check_duplicates(
        <<"query"/utf8>>,
        erlang:element(2, A),
        erlang:element(2, B),
        fun(F) -> erlang:element(2, F) end
    ),
    check_duplicates(
        <<"mutation"/utf8>>,
        erlang:element(3, A),
        erlang:element(3, B),
        fun(F@1) -> erlang:element(2, F@1) end
    ),
    check_duplicates(
        <<"subscription"/utf8>>,
        erlang:element(4, A),
        erlang:element(4, B),
        fun(F@2) -> erlang:element(2, F@2) end
    ),
    check_duplicates(
        <<"type"/utf8>>,
        erlang:element(5, A),
        erlang:element(5, B),
        fun(T) -> erlang:element(2, T) end
    ),
    check_duplicates(
        <<"enum"/utf8>>,
        erlang:element(6, A),
        erlang:element(6, B),
        fun(E) -> erlang:element(2, E) end
    ),
    check_duplicates(
        <<"interface"/utf8>>,
        erlang:element(7, A),
        erlang:element(7, B),
        fun(I) -> erlang:element(2, I) end
    ),
    check_duplicates(
        <<"union"/utf8>>,
        erlang:element(8, A),
        erlang:element(8, B),
        fun(U) -> erlang:element(2, U) end
    ),
    check_duplicates(
        <<"scalar"/utf8>>,
        erlang:element(9, A),
        erlang:element(9, B),
        fun(S) -> erlang:element(2, S) end
    ),
    check_duplicates(
        <<"input"/utf8>>,
        erlang:element(10, A),
        erlang:element(10, B),
        fun(I@1) -> erlang:element(2, I@1) end
    ),
    {schema_builder,
        lists:append(erlang:element(2, A), erlang:element(2, B)),
        lists:append(erlang:element(3, A), erlang:element(3, B)),
        lists:append(erlang:element(4, A), erlang:element(4, B)),
        lists:append(erlang:element(5, A), erlang:element(5, B)),
        lists:append(erlang:element(6, A), erlang:element(6, B)),
        lists:append(erlang:element(7, A), erlang:element(7, B)),
        lists:append(erlang:element(8, A), erlang:element(8, B)),
        lists:append(erlang:element(9, A), erlang:element(9, B)),
        lists:append(erlang:element(10, A), erlang:element(10, B)),
        erlang:element(11, A) orelse erlang:element(11, B)}.

-file("src/mochi/query.gleam", 1098).
?DOC(" Build the final schema\n").
-spec with_cache(schema_builder()) -> schema_builder().
with_cache(Builder) ->
    {schema_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        true}.

-file("src/mochi/query.gleam", 1106).
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
    With_unions = gleam@list:fold(
        erlang:element(8, Builder),
        With_interfaces,
        fun(S@3, U) -> mochi@schema:add_type(S@3, {union_type_def, U}) end
    ),
    With_scalars = gleam@list:fold(
        erlang:element(9, Builder),
        With_unions,
        fun(S@4, Scalar) ->
            mochi@schema:add_type(S@4, {scalar_type_def, Scalar})
        end
    ),
    With_inputs = gleam@list:fold(
        erlang:element(10, Builder),
        With_scalars,
        fun(S@5, Input) ->
            mochi@schema:add_type(S@5, {input_object_type_def, Input})
        end
    ),
    With_directives = gleam@list:fold(
        mochi@schema:builtin_directives(),
        With_inputs,
        fun(S@6, Directive) -> mochi@schema:add_directive(S@6, Directive) end
    ),
    {schema,
        erlang:element(2, With_directives),
        erlang:element(3, With_directives),
        erlang:element(4, With_directives),
        erlang:element(5, With_directives),
        erlang:element(6, With_directives),
        {some, mochi@document_cache:new()}}.

-file("src/mochi/query.gleam", 1102).
-spec build_without_cache(schema_builder()) -> mochi@schema:schema().
build_without_cache(Builder) ->
    build(
        {schema_builder,
            erlang:element(2, Builder),
            erlang:element(3, Builder),
            erlang:element(4, Builder),
            erlang:element(5, Builder),
            erlang:element(6, Builder),
            erlang:element(7, Builder),
            erlang:element(8, Builder),
            erlang:element(9, Builder),
            erlang:element(10, Builder),
            false}
    ).
