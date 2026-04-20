-record(user, {
    id :: binary(),
    name :: binary(),
    email :: binary(),
    score :: integer(),
    profile :: context_isolation_test:profile()
}).
