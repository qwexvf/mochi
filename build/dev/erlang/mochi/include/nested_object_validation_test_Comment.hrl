-record(comment, {
    id :: binary(),
    body :: binary(),
    author :: nested_object_validation_test:user()
}).
