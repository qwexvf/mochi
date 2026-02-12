-module(mochi@subscription_executor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/subscription_executor.gleam").
-export([unsubscribe/2, publish_event/3, subscribe_document/3, subscribe/3]).
-export_type([subscription_result/0, subscription_context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type subscription_result() :: {subscription_result,
        binary(),
        binary(),
        mochi@subscription:pub_sub()} |
    {subscription_error, binary()}.

-type subscription_context() :: {subscription_context,
        mochi@schema:schema(),
        mochi@subscription:pub_sub(),
        mochi@schema:execution_context(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())}.

-file("src/mochi/subscription_executor.gleam", 84).
?DOC(" Unsubscribe from a subscription\n").
-spec unsubscribe(mochi@subscription:pub_sub(), binary()) -> mochi@subscription:pub_sub().
unsubscribe(Pubsub, Subscription_id) ->
    mochi@subscription:unsubscribe(Pubsub, Subscription_id).

-file("src/mochi/subscription_executor.gleam", 90).
?DOC(
    " Publish an event to subscribers\n"
    " This will execute the selection set for each subscriber and call their callbacks\n"
).
-spec publish_event(subscription_context(), binary(), gleam@dynamic:dynamic_()) -> nil.
publish_event(Context, Topic, Event_data) ->
    mochi@subscription:publish(erlang:element(3, Context), Topic, Event_data).

-file("src/mochi/subscription_executor.gleam", 102).
-spec find_subscription_operation(mochi@ast:document()) -> gleam@option:option(mochi@ast:operation()).
find_subscription_operation(Document) ->
    _pipe = erlang:element(2, Document),
    _pipe@1 = gleam@list:find_map(_pipe, fun(Def) -> case Def of
                {operation_definition,
                    {operation, subscription, _, _, _, _} = Op} ->
                    {ok, Op};

                _ ->
                    {error, nil}
            end end),
    _pipe@2 = gleam@result:map(_pipe@1, fun(Field@0) -> {some, Field@0} end),
    gleam@result:unwrap(_pipe@2, none).

-file("src/mochi/subscription_executor.gleam", 155).
?DOC(" Require the schema defines a subscription type\n").
-spec require_subscription_type(
    mochi@schema:schema(),
    fun((mochi@schema:object_type()) -> subscription_result())
) -> subscription_result().
require_subscription_type(Schema_def, Next) ->
    case erlang:element(4, Schema_def) of
        {some, Subscription_type} ->
            Next(Subscription_type);

        none ->
            {subscription_error,
                <<"Schema does not define a Subscription type"/utf8>>}
    end.

-file("src/mochi/subscription_executor.gleam", 177).
?DOC(" Require the field exists on the subscription type\n").
-spec require_subscription_field(
    mochi@schema:object_type(),
    binary(),
    fun((mochi@schema:field_definition()) -> subscription_result())
) -> subscription_result().
require_subscription_field(Subscription_type, Field_name, Next) ->
    case gleam_stdlib:map_get(erlang:element(4, Subscription_type), Field_name) of
        {ok, Field_def} ->
            Next(Field_def);

        {error, _} ->
            {subscription_error,
                <<<<"Field '"/utf8, Field_name/binary>>/binary,
                    "' not found on Subscription type"/utf8>>}
    end.

-file("src/mochi/subscription_executor.gleam", 191).
-spec get_selection_set(mochi@ast:operation()) -> mochi@ast:selection_set().
get_selection_set(Operation) ->
    case Operation of
        {operation, _, _, _, _, Ss} ->
            Ss;

        {shorthand_query, Ss@1} ->
            Ss@1
    end.

-file("src/mochi/subscription_executor.gleam", 198).
-spec get_single_root_field(mochi@ast:selection_set()) -> {ok,
        mochi@ast:field()} |
    {error, binary()}.
get_single_root_field(Selection_set) ->
    Fields = gleam@list:filter_map(
        erlang:element(2, Selection_set),
        fun(Selection) -> case Selection of
                {field_selection, Field} ->
                    {ok, Field};

                _ ->
                    {error, nil}
            end end
    ),
    case Fields of
        [] ->
            {error, <<"Subscription must have at least one field"/utf8>>};

        [Field@1] ->
            {ok, Field@1};

        _ ->
            {error, <<"Subscription must have exactly one root field"/utf8>>}
    end.

-file("src/mochi/subscription_executor.gleam", 166).
?DOC(" Require exactly one root field in selection set\n").
-spec require_single_root_field_result(
    mochi@ast:selection_set(),
    fun((mochi@ast:field()) -> subscription_result())
) -> subscription_result().
require_single_root_field_result(Selection_set, Next) ->
    case get_single_root_field(Selection_set) of
        {ok, Field} ->
            Next(Field);

        {error, Msg} ->
            {subscription_error, Msg}
    end.

-file("src/mochi/subscription_executor.gleam", 225).
-spec coerce_value(
    mochi@ast:value(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dynamic:dynamic_().
coerce_value(Value, Variables) ->
    case Value of
        {int_value, I} ->
            gleam_stdlib:identity(I);

        {float_value, F} ->
            gleam_stdlib:identity(F);

        {string_value, S} ->
            gleam_stdlib:identity(S);

        {boolean_value, B} ->
            gleam_stdlib:identity(B);

        null_value ->
            gleam_stdlib:identity(nil);

        {enum_value, E} ->
            gleam_stdlib:identity(E);

        {variable_value, Name} ->
            _pipe = gleam_stdlib:map_get(Variables, Name),
            gleam@result:unwrap(_pipe, gleam_stdlib:identity(nil));

        {list_value, Values} ->
            gleam_stdlib:identity(
                gleam@list:map(
                    Values,
                    fun(_capture) -> coerce_value(_capture, Variables) end
                )
            );

        {object_value, Fields} ->
            gleam_stdlib:identity(
                gleam@list:fold(
                    Fields,
                    maps:new(),
                    fun(Acc, F@1) ->
                        gleam@dict:insert(
                            Acc,
                            erlang:element(2, F@1),
                            coerce_value(erlang:element(3, F@1), Variables)
                        )
                    end
                )
            )
    end.

-file("src/mochi/subscription_executor.gleam", 216).
-spec coerce_arguments(
    list(mochi@ast:argument()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
coerce_arguments(Ast_args, Variables) ->
    gleam@list:fold(
        Ast_args,
        maps:new(),
        fun(Acc, Arg) ->
            gleam@dict:insert(
                Acc,
                erlang:element(2, Arg),
                coerce_value(erlang:element(3, Arg), Variables)
            )
        end
    ).

-file("src/mochi/subscription_executor.gleam", 296).
-spec get_field_type_name(mochi@schema:field_type()) -> gleam@option:option(binary()).
get_field_type_name(Field_type) ->
    case Field_type of
        {named, Name} ->
            {some, Name};

        {non_null, Inner} ->
            get_field_type_name(Inner);

        {list, Inner@1} ->
            get_field_type_name(Inner@1)
    end.

-file("src/mochi/subscription_executor.gleam", 406).
-spec make_field(binary(), gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_().
make_field(Name, Value) ->
    gleam_stdlib:identity(maps:from_list([{Name, Value}])).

-file("src/mochi/subscription_executor.gleam", 410).
-spec merge_field_results(list(gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
merge_field_results(Results) ->
    gleam_stdlib:identity(Results).

-file("src/mochi/subscription_executor.gleam", 342).
-spec execute_event_field(
    subscription_context(),
    mochi@ast:field(),
    mochi@schema:object_type(),
    gleam@dynamic:dynamic_()
) -> mochi@executor:execution_result().
execute_event_field(Context, Field, Object_type, Event_data) ->
    Response_name = gleam@option:unwrap(
        erlang:element(2, Field),
        erlang:element(3, Field)
    ),
    case erlang:element(3, Field) of
        <<"__typename"/utf8>> ->
            {execution_result,
                {some,
                    make_field(
                        Response_name,
                        gleam_stdlib:identity(erlang:element(2, Object_type))
                    )},
                []};

        _ ->
            case gleam_stdlib:map_get(
                erlang:element(4, Object_type),
                erlang:element(3, Field)
            ) of
                {error, _} ->
                    {execution_result,
                        none,
                        [{validation_error,
                                <<<<"Field '"/utf8,
                                        (erlang:element(3, Field))/binary>>/binary,
                                    "' not found"/utf8>>,
                                []}]};

                {ok, Field_def} ->
                    case erlang:element(6, Field_def) of
                        {some, Resolver} ->
                            Resolver_info = {resolver_info,
                                {some, Event_data},
                                coerce_arguments(
                                    erlang:element(4, Field),
                                    erlang:element(5, Context)
                                ),
                                erlang:element(4, Context),
                                gleam_stdlib:identity(maps:new())},
                            case Resolver(Resolver_info) of
                                {ok, Value} ->
                                    {execution_result,
                                        {some, make_field(Response_name, Value)},
                                        []};

                                {error, Msg} ->
                                    {execution_result,
                                        none,
                                        [{resolver_error, Msg, [Response_name]}]}
                            end;

                        none ->
                            Value@1 = mochi_ffi:extract_field(
                                Event_data,
                                erlang:element(3, Field)
                            ),
                            {execution_result,
                                {some, make_field(Response_name, Value@1)},
                                []}
                    end
            end
    end.

-file("src/mochi/subscription_executor.gleam", 304).
-spec execute_selection_on_event(
    subscription_context(),
    mochi@ast:selection_set(),
    mochi@schema:object_type(),
    gleam@dynamic:dynamic_(),
    binary()
) -> mochi@executor:execution_result().
execute_selection_on_event(
    Context,
    Selection_set,
    Object_type,
    Event_data,
    Response_name
) ->
    Field_results = gleam@list:filter_map(
        erlang:element(2, Selection_set),
        fun(Selection) -> case Selection of
                {field_selection, Field} ->
                    {ok,
                        execute_event_field(
                            Context,
                            Field,
                            Object_type,
                            Event_data
                        )};

                _ ->
                    {error, nil}
            end end
    ),
    Errors = gleam@list:flat_map(
        Field_results,
        fun(R) -> erlang:element(3, R) end
    ),
    Data_parts = gleam@list:filter_map(
        Field_results,
        fun(R@1) -> gleam@option:to_result(erlang:element(2, R@1), nil) end
    ),
    case Data_parts of
        [] ->
            {execution_result,
                {some,
                    make_field(Response_name, gleam_stdlib:identity(maps:new()))},
                Errors};

        _ ->
            Merged = merge_field_results(Data_parts),
            {execution_result,
                {some, make_field(Response_name, Merged)},
                Errors}
    end.

-file("src/mochi/subscription_executor.gleam", 246).
-spec execute_subscription_event(
    subscription_context(),
    mochi@ast:document(),
    mochi@ast:field(),
    mochi@schema:field_definition(),
    gleam@dynamic:dynamic_()
) -> mochi@executor:execution_result().
execute_subscription_event(Context, _, Field, Field_def, Event_data) ->
    Response_name = gleam@option:unwrap(
        erlang:element(2, Field),
        erlang:element(3, Field)
    ),
    case erlang:element(6, Field) of
        none ->
            {execution_result,
                {some, make_field(Response_name, Event_data)},
                []};

        {some, Sub_ss} ->
            case get_field_type_name(erlang:element(4, Field_def)) of
                none ->
                    {execution_result,
                        {some, make_field(Response_name, Event_data)},
                        []};

                {some, Type_name} ->
                    case gleam_stdlib:map_get(
                        erlang:element(5, erlang:element(2, Context)),
                        Type_name
                    ) of
                        {ok, {object_type_def, Obj_type}} ->
                            execute_selection_on_event(
                                Context,
                                Sub_ss,
                                Obj_type,
                                Event_data,
                                Response_name
                            );

                        _ ->
                            {execution_result,
                                {some, make_field(Response_name, Event_data)},
                                []}
                    end
            end
    end.

-file("src/mochi/subscription_executor.gleam", 116).
-spec execute_subscription(
    subscription_context(),
    mochi@ast:document(),
    mochi@ast:operation(),
    fun((mochi@executor:execution_result()) -> nil)
) -> subscription_result().
execute_subscription(Context, Document, Operation, Callback) ->
    require_subscription_type(
        erlang:element(2, Context),
        fun(Subscription_type) ->
            Selection_set = get_selection_set(Operation),
            require_single_root_field_result(
                Selection_set,
                fun(Field) ->
                    require_subscription_field(
                        Subscription_type,
                        erlang:element(3, Field),
                        fun(Field_def) ->
                            Args = coerce_arguments(
                                erlang:element(4, Field),
                                erlang:element(5, Context)
                            ),
                            Topic = erlang:element(3, Field),
                            Event_callback = fun(Event_data) ->
                                Result = execute_subscription_event(
                                    Context,
                                    Document,
                                    Field,
                                    Field_def,
                                    Event_data
                                ),
                                Callback(Result)
                            end,
                            Result@1 = mochi@subscription:subscribe(
                                erlang:element(3, Context),
                                Topic,
                                erlang:element(3, Field),
                                Args,
                                Event_callback
                            ),
                            {subscription_result,
                                erlang:element(2, Result@1),
                                Topic,
                                erlang:element(3, Result@1)}
                        end
                    )
                end
            )
        end
    ).

-file("src/mochi/subscription_executor.gleam", 69).
?DOC(" Subscribe with a pre-parsed document\n").
-spec subscribe_document(
    subscription_context(),
    mochi@ast:document(),
    fun((mochi@executor:execution_result()) -> nil)
) -> subscription_result().
subscribe_document(Context, Document, Callback) ->
    case find_subscription_operation(Document) of
        {some, Operation} ->
            execute_subscription(Context, Document, Operation, Callback);

        none ->
            {subscription_error,
                <<"Document does not contain a subscription operation"/utf8>>}
    end.

-file("src/mochi/subscription_executor.gleam", 57).
?DOC(
    " Subscribe to a GraphQL subscription operation\n"
    " Returns the subscription ID and updated PubSub, or an error\n"
).
-spec subscribe(
    subscription_context(),
    binary(),
    fun((mochi@executor:execution_result()) -> nil)
) -> subscription_result().
subscribe(Context, Query, Callback) ->
    case mochi@parser:parse(Query) of
        {ok, Document} ->
            subscribe_document(Context, Document, Callback);

        {error, _} ->
            {subscription_error, <<"Failed to parse subscription query"/utf8>>}
    end.
