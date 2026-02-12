-module(mochi_pubsub_ffi).
-export([get_pubsub/0, set_pubsub/1]).

%% Store PubSub in persistent_term for fast access across processes
-define(PUBSUB_KEY, mochi_global_pubsub).

set_pubsub(PubSub) ->
    persistent_term:put(?PUBSUB_KEY, PubSub),
    nil.

get_pubsub() ->
    case persistent_term:get(?PUBSUB_KEY, undefined) of
        undefined ->
            %% Return empty pubsub if not initialized
            {pubsub, #{}, 0};
        PubSub ->
            PubSub
    end.
