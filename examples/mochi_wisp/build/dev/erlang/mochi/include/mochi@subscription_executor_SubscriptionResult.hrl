-record(subscription_result, {
    subscription_id :: binary(),
    topic :: binary(),
    pubsub :: mochi@subscription:pub_sub()
}).
