-module(mochi@transport@websocket).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/transport/websocket.gleam").
-export([new_connection/3, new_connection_with_params/4, handle_message/2, send_next/2, send_error/2, send_complete/1, get_active_subscriptions/1, has_subscription/2, cleanup/1, server_message_type/1, client_message_type/1]).
-export_type([client_message/0, subscription_payload/0, server_message/0, connection_state/0, handle_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type client_message() :: {connection_init,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))} |
    {subscribe, binary(), subscription_payload()} |
    {complete, binary()} |
    {ping,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))} |
    {pong,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-type subscription_payload() :: {subscription_payload,
        binary(),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())),
        gleam@option:option(binary()),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-type server_message() :: {connection_ack,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))} |
    {next, binary(), mochi@executor:execution_result()} |
    {subscription_error, binary(), list(mochi@error:graph_q_l_error())} |
    {server_complete, binary()} |
    {server_ping,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))} |
    {server_pong,
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-type connection_state() :: {connection_state,
        mochi@schema:schema(),
        mochi@subscription:pub_sub(),
        mochi@schema:execution_context(),
        gleam@dict:dict(binary(), binary()),
        boolean(),
        gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))}.

-type handle_result() :: {handle_ok,
        connection_state(),
        gleam@option:option(server_message())} |
    {handle_multiple, connection_state(), list(server_message())} |
    {handle_close, binary()}.

-file("src/mochi/transport/websocket.gleam", 120).
?DOC(" Create a new connection state\n").
-spec new_connection(
    mochi@schema:schema(),
    mochi@subscription:pub_sub(),
    mochi@schema:execution_context()
) -> connection_state().
new_connection(Schema, Pubsub, Execution_context) ->
    {connection_state,
        Schema,
        Pubsub,
        Execution_context,
        maps:new(),
        false,
        none}.

-file("src/mochi/transport/websocket.gleam", 136).
?DOC(" Create connection state with custom parameters\n").
-spec new_connection_with_params(
    mochi@schema:schema(),
    mochi@subscription:pub_sub(),
    mochi@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> connection_state().
new_connection_with_params(Schema, Pubsub, Execution_context, Params) ->
    {connection_state,
        Schema,
        Pubsub,
        Execution_context,
        maps:new(),
        false,
        {some, Params}}.

-file("src/mochi/transport/websocket.gleam", 171).
?DOC(" Handle ConnectionInit message\n").
-spec handle_connection_init(
    connection_state(),
    gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
) -> handle_result().
handle_connection_init(State, Payload) ->
    case erlang:element(6, State) of
        true ->
            {handle_close, <<"Too many initialization requests"/utf8>>};

        false ->
            New_state = {connection_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                erlang:element(5, State),
                true,
                Payload},
            {handle_ok, New_state, {some, {connection_ack, none}}}
    end.

-file("src/mochi/transport/websocket.gleam", 217).
?DOC(" Handle Complete message (client unsubscribing)\n").
-spec handle_complete(connection_state(), binary()) -> handle_result().
handle_complete(State, Id) ->
    case gleam_stdlib:map_get(erlang:element(5, State), Id) of
        {ok, Subscription_id} ->
            New_pubsub = mochi@subscription:unsubscribe(
                erlang:element(3, State),
                Subscription_id
            ),
            New_state = {connection_state,
                erlang:element(2, State),
                New_pubsub,
                erlang:element(4, State),
                gleam@dict:delete(erlang:element(5, State), Id),
                erlang:element(6, State),
                erlang:element(7, State)},
            {handle_ok, New_state, {some, {server_complete, Id}}};

        {error, _} ->
            {handle_ok, State, {some, {server_complete, Id}}}
    end.

-file("src/mochi/transport/websocket.gleam", 237).
?DOC(" Handle Ping message\n").
-spec handle_ping(
    connection_state(),
    gleam@option:option(gleam@dict:dict(binary(), gleam@dynamic:dynamic_()))
) -> handle_result().
handle_ping(State, Payload) ->
    {handle_ok, State, {some, {server_pong, Payload}}}.

-file("src/mochi/transport/websocket.gleam", 249).
?DOC(" Require connection to be acknowledged before processing\n").
-spec require_acknowledged(connection_state(), fun(() -> handle_result())) -> handle_result().
require_acknowledged(State, Next) ->
    case erlang:element(6, State) of
        true ->
            Next();

        false ->
            {handle_close, <<"Unauthorized"/utf8>>}
    end.

-file("src/mochi/transport/websocket.gleam", 260).
?DOC(" Check for duplicate subscription ID\n").
-spec check_duplicate_subscription(
    connection_state(),
    binary(),
    fun(() -> handle_result())
) -> handle_result().
check_duplicate_subscription(State, Id, Next) ->
    case gleam@dict:has_key(erlang:element(5, State), Id) of
        true ->
            {handle_close,
                <<<<"Subscriber for "/utf8, Id/binary>>/binary,
                    " already exists"/utf8>>};

        false ->
            Next()
    end.

-file("src/mochi/transport/websocket.gleam", 192).
?DOC(" Handle Subscribe message\n").
-spec handle_subscribe(connection_state(), binary(), subscription_payload()) -> handle_result().
handle_subscribe(State, Id, _) ->
    require_acknowledged(
        State,
        fun() ->
            check_duplicate_subscription(
                State,
                Id,
                fun() ->
                    New_state = {connection_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        gleam@dict:insert(
                            erlang:element(5, State),
                            Id,
                            <<"pending_"/utf8, Id/binary>>
                        ),
                        erlang:element(6, State),
                        erlang:element(7, State)},
                    {handle_ok, New_state, none}
                end
            )
        end
    ).

-file("src/mochi/transport/websocket.gleam", 157).
?DOC(" Handle an incoming client message\n").
-spec handle_message(connection_state(), client_message()) -> handle_result().
handle_message(State, Message) ->
    case Message of
        {connection_init, Payload} ->
            handle_connection_init(State, Payload);

        {subscribe, Id, Payload@1} ->
            handle_subscribe(State, Id, Payload@1);

        {complete, Id@1} ->
            handle_complete(State, Id@1);

        {ping, Payload@2} ->
            handle_ping(State, Payload@2);

        {pong, _} ->
            {handle_ok, State, none}
    end.

-file("src/mochi/transport/websocket.gleam", 272).
?DOC(" Send a Next message to the client\n").
-spec send_next(binary(), mochi@executor:execution_result()) -> server_message().
send_next(Id, Result) ->
    {next, Id, Result}.

-file("src/mochi/transport/websocket.gleam", 277).
?DOC(" Send an Error message to the client\n").
-spec send_error(binary(), list(mochi@error:graph_q_l_error())) -> server_message().
send_error(Id, Errors) ->
    {subscription_error, Id, Errors}.

-file("src/mochi/transport/websocket.gleam", 282).
?DOC(" Send a Complete message to the client\n").
-spec send_complete(binary()) -> server_message().
send_complete(Id) ->
    {server_complete, Id}.

-file("src/mochi/transport/websocket.gleam", 287).
?DOC(" Get all active subscription IDs\n").
-spec get_active_subscriptions(connection_state()) -> list(binary()).
get_active_subscriptions(State) ->
    maps:keys(erlang:element(5, State)).

-file("src/mochi/transport/websocket.gleam", 292).
?DOC(" Check if a subscription is active\n").
-spec has_subscription(connection_state(), binary()) -> boolean().
has_subscription(State, Id) ->
    gleam@dict:has_key(erlang:element(5, State), Id).

-file("src/mochi/transport/websocket.gleam", 297).
?DOC(" Clean up all subscriptions (e.g., on disconnect)\n").
-spec cleanup(connection_state()) -> connection_state().
cleanup(State) ->
    New_pubsub = gleam@dict:fold(
        erlang:element(5, State),
        erlang:element(3, State),
        fun(Pubsub, _, Sub_id) ->
            mochi@subscription:unsubscribe(Pubsub, Sub_id)
        end
    ),
    {connection_state,
        erlang:element(2, State),
        New_pubsub,
        erlang:element(4, State),
        maps:new(),
        erlang:element(6, State),
        erlang:element(7, State)}.

-file("src/mochi/transport/websocket.gleam", 332).
?DOC(" Get the message type string for a server message\n").
-spec server_message_type(server_message()) -> binary().
server_message_type(Message) ->
    case Message of
        {connection_ack, _} ->
            <<"connection_ack"/utf8>>;

        {next, _, _} ->
            <<"next"/utf8>>;

        {subscription_error, _, _} ->
            <<"error"/utf8>>;

        {server_complete, _} ->
            <<"complete"/utf8>>;

        {server_ping, _} ->
            <<"ping"/utf8>>;

        {server_pong, _} ->
            <<"pong"/utf8>>
    end.

-file("src/mochi/transport/websocket.gleam", 344).
?DOC(" Get the message type string for a client message\n").
-spec client_message_type(client_message()) -> binary().
client_message_type(Message) ->
    case Message of
        {connection_init, _} ->
            <<"connection_init"/utf8>>;

        {subscribe, _, _} ->
            <<"subscribe"/utf8>>;

        {complete, _} ->
            <<"complete"/utf8>>;

        {ping, _} ->
            <<"ping"/utf8>>;

        {pong, _} ->
            <<"pong"/utf8>>
    end.
