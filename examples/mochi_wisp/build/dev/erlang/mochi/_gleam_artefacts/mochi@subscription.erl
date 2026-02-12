-module(mochi@subscription).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/subscription.gleam").
-export([new_pubsub/0, unsubscribe/2, publish/3, get_topic_subscribers/2, get_subscription/2, subscription_count/1, subscription/4, description/2, argument/2, filter/2, to_field_definition/1, topic/1, topic_with_id/2, subscribe/5, topic_from_parts/1]).
-export_type([subscription_definition/1, subscription/0, pub_sub/0, subscribe_result/0, subscription_event/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type subscription_definition(AHDK) :: {subscription_definition,
        binary(),
        gleam@option:option(binary()),
        mochi@schema:field_type(),
        gleam@dict:dict(binary(), mochi@schema:argument_definition()),
        fun((mochi@schema:resolver_info()) -> {ok, binary()} | {error, binary()}),
        fun((AHDK) -> gleam@dynamic:dynamic_()),
        gleam@option:option(fun((AHDK, gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> boolean()))}.

-type subscription() :: {subscription,
        binary(),
        binary(),
        binary(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        fun((gleam@dynamic:dynamic_()) -> nil)}.

-opaque pub_sub() :: {pub_sub,
        gleam@dict:dict(binary(), subscription()),
        gleam@dict:dict(binary(), list(binary())),
        integer()}.

-type subscribe_result() :: {subscribe_result, binary(), pub_sub()}.

-type subscription_event() :: {subscription_event,
        binary(),
        gleam@dynamic:dynamic_()}.

-file("src/mochi/subscription.gleam", 86).
?DOC(" Create a new PubSub instance\n").
-spec new_pubsub() -> pub_sub().
new_pubsub() ->
    {pub_sub, maps:new(), maps:new(), 1}.

-file("src/mochi/subscription.gleam", 128).
?DOC(" Unsubscribe from a topic\n").
-spec unsubscribe(pub_sub(), binary()) -> pub_sub().
unsubscribe(Pubsub, Subscription_id) ->
    case gleam_stdlib:map_get(erlang:element(2, Pubsub), Subscription_id) of
        {ok, Subscription} ->
            Subscriptions = gleam@dict:delete(
                erlang:element(2, Pubsub),
                Subscription_id
            ),
            Topics = case gleam_stdlib:map_get(
                erlang:element(3, Pubsub),
                erlang:element(3, Subscription)
            ) of
                {ok, Sub_ids} ->
                    Filtered = gleam@list:filter(
                        Sub_ids,
                        fun(Id) -> Id /= Subscription_id end
                    ),
                    case Filtered of
                        [] ->
                            gleam@dict:delete(
                                erlang:element(3, Pubsub),
                                erlang:element(3, Subscription)
                            );

                        _ ->
                            gleam@dict:insert(
                                erlang:element(3, Pubsub),
                                erlang:element(3, Subscription),
                                Filtered
                            )
                    end;

                {error, _} ->
                    erlang:element(3, Pubsub)
            end,
            {pub_sub, Subscriptions, Topics, erlang:element(4, Pubsub)};

        {error, _} ->
            Pubsub
    end.

-file("src/mochi/subscription.gleam", 151).
?DOC(" Publish an event to all subscribers of a topic\n").
-spec publish(pub_sub(), binary(), gleam@dynamic:dynamic_()) -> nil.
publish(Pubsub, Topic, Payload) ->
    case gleam_stdlib:map_get(erlang:element(3, Pubsub), Topic) of
        {ok, Sub_ids} ->
            gleam@list:each(
                Sub_ids,
                fun(Sub_id) ->
                    case gleam_stdlib:map_get(erlang:element(2, Pubsub), Sub_id) of
                        {ok, Subscription} ->
                            (erlang:element(6, Subscription))(Payload);

                        {error, _} ->
                            nil
                    end
                end
            );

        {error, _} ->
            nil
    end.

-file("src/mochi/subscription.gleam", 166).
?DOC(" Get all active subscription IDs for a topic\n").
-spec get_topic_subscribers(pub_sub(), binary()) -> list(binary()).
get_topic_subscribers(Pubsub, Topic) ->
    case gleam_stdlib:map_get(erlang:element(3, Pubsub), Topic) of
        {ok, Sub_ids} ->
            Sub_ids;

        {error, _} ->
            []
    end.

-file("src/mochi/subscription.gleam", 177).
?DOC(" Get subscription by ID\n").
-spec get_subscription(pub_sub(), binary()) -> gleam@option:option(subscription()).
get_subscription(Pubsub, Subscription_id) ->
    case gleam_stdlib:map_get(erlang:element(2, Pubsub), Subscription_id) of
        {ok, Sub} ->
            {some, Sub};

        {error, _} ->
            none
    end.

-file("src/mochi/subscription.gleam", 188).
?DOC(" Count active subscriptions\n").
-spec subscription_count(pub_sub()) -> integer().
subscription_count(Pubsub) ->
    maps:size(erlang:element(2, Pubsub)).

-file("src/mochi/subscription.gleam", 197).
?DOC(" Create a new subscription definition\n").
-spec subscription(
    binary(),
    mochi@schema:field_type(),
    fun((mochi@schema:resolver_info()) -> {ok, binary()} | {error, binary()}),
    fun((AHDR) -> gleam@dynamic:dynamic_())
) -> subscription_definition(AHDR).
subscription(Name, Field_type, Topic_resolver, Event_transformer) ->
    {subscription_definition,
        Name,
        none,
        Field_type,
        maps:new(),
        Topic_resolver,
        Event_transformer,
        none}.

-file("src/mochi/subscription.gleam", 215).
?DOC(" Add description to subscription\n").
-spec description(subscription_definition(AHDT), binary()) -> subscription_definition(AHDT).
description(Sub, Desc) ->
    {subscription_definition,
        erlang:element(2, Sub),
        {some, Desc},
        erlang:element(4, Sub),
        erlang:element(5, Sub),
        erlang:element(6, Sub),
        erlang:element(7, Sub),
        erlang:element(8, Sub)}.

-file("src/mochi/subscription.gleam", 223).
?DOC(" Add argument to subscription\n").
-spec argument(
    subscription_definition(AHDW),
    mochi@schema:argument_definition()
) -> subscription_definition(AHDW).
argument(Sub, Arg) ->
    {subscription_definition,
        erlang:element(2, Sub),
        erlang:element(3, Sub),
        erlang:element(4, Sub),
        gleam@dict:insert(erlang:element(5, Sub), erlang:element(2, Arg), Arg),
        erlang:element(6, Sub),
        erlang:element(7, Sub),
        erlang:element(8, Sub)}.

-file("src/mochi/subscription.gleam", 234).
?DOC(" Add filter function to subscription\n").
-spec filter(
    subscription_definition(AHDZ),
    fun((AHDZ, gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> boolean())
) -> subscription_definition(AHDZ).
filter(Sub, Filter_fn) ->
    {subscription_definition,
        erlang:element(2, Sub),
        erlang:element(3, Sub),
        erlang:element(4, Sub),
        erlang:element(5, Sub),
        erlang:element(6, Sub),
        erlang:element(7, Sub),
        {some, Filter_fn}}.

-file("src/mochi/subscription.gleam", 242).
?DOC(" Convert subscription definition to a field definition for schema\n").
-spec to_field_definition(subscription_definition(any())) -> mochi@schema:field_definition().
to_field_definition(Sub) ->
    {field_definition,
        erlang:element(2, Sub),
        erlang:element(3, Sub),
        erlang:element(4, Sub),
        erlang:element(5, Sub),
        none,
        false,
        none}.

-file("src/mochi/subscription.gleam", 261).
?DOC(" Create a simple topic string\n").
-spec topic(binary()) -> binary().
topic(Name) ->
    Name.

-file("src/mochi/subscription.gleam", 266).
?DOC(" Create a topic with an ID suffix (e.g., \"user:123:updated\")\n").
-spec topic_with_id(binary(), binary()) -> binary().
topic_with_id(Base, Id) ->
    <<<<Base/binary, ":"/utf8>>/binary, Id/binary>>.

-file("src/mochi/subscription.gleam", 293).
-spec digit_to_string(integer()) -> binary().
digit_to_string(D) ->
    case D of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        2 ->
            <<"2"/utf8>>;

        3 ->
            <<"3"/utf8>>;

        4 ->
            <<"4"/utf8>>;

        5 ->
            <<"5"/utf8>>;

        6 ->
            <<"6"/utf8>>;

        7 ->
            <<"7"/utf8>>;

        8 ->
            <<"8"/utf8>>;

        _ ->
            <<"9"/utf8>>
    end.

-file("src/mochi/subscription.gleam", 286).
-spec positive_int_to_string(integer()) -> binary().
positive_int_to_string(N) ->
    case N < 10 of
        true ->
            digit_to_string(N);

        false ->
            <<(positive_int_to_string(N div 10))/binary,
                (digit_to_string(N rem 10))/binary>>
    end.

-file("src/mochi/subscription.gleam", 279).
-spec int_to_string(integer()) -> binary().
int_to_string(N) ->
    case N < 0 of
        true ->
            <<"-"/utf8, (positive_int_to_string(- N))/binary>>;

        false ->
            positive_int_to_string(N)
    end.

-file("src/mochi/subscription.gleam", 91).
?DOC(" Subscribe to a topic\n").
-spec subscribe(
    pub_sub(),
    binary(),
    binary(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    fun((gleam@dynamic:dynamic_()) -> nil)
) -> subscribe_result().
subscribe(Pubsub, Topic, Field_name, Arguments, Callback) ->
    Id = <<"sub_"/utf8, (int_to_string(erlang:element(4, Pubsub)))/binary>>,
    Subscription = {subscription, Id, Topic, Field_name, Arguments, Callback},
    Subscriptions = gleam@dict:insert(
        erlang:element(2, Pubsub),
        Id,
        Subscription
    ),
    Topic_subs = case gleam_stdlib:map_get(erlang:element(3, Pubsub), Topic) of
        {ok, Existing} ->
            [Id | Existing];

        {error, _} ->
            [Id]
    end,
    Topics = gleam@dict:insert(erlang:element(3, Pubsub), Topic, Topic_subs),
    New_pubsub = {pub_sub, Subscriptions, Topics, erlang:element(4, Pubsub) + 1},
    {subscribe_result, Id, New_pubsub}.

-file("src/mochi/subscription.gleam", 308).
-spec join_with_colon(list(binary())) -> binary().
join_with_colon(Parts) ->
    case Parts of
        [] ->
            <<""/utf8>>;

        [First] ->
            First;

        [First@1 | Rest] ->
            <<<<First@1/binary, ":"/utf8>>/binary,
                (join_with_colon(Rest))/binary>>
    end.

-file("src/mochi/subscription.gleam", 271).
?DOC(" Create a topic from multiple parts\n").
-spec topic_from_parts(list(binary())) -> binary().
topic_from_parts(Parts) ->
    join_with_colon(Parts).
