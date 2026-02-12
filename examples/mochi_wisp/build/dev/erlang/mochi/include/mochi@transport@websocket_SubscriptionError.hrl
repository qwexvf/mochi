-record(subscription_error, {
    id :: binary(),
    payload :: list(mochi@error:graph_q_l_error())
}).
