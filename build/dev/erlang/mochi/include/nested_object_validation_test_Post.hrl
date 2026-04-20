-record(post, {
    id :: binary(),
    title :: binary(),
    metadata :: nested_object_validation_test:metadata(),
    view_count :: integer()
}).
