-record(department, {
    id :: binary(),
    name :: binary(),
    employees :: list(nested_object_validation_test:employee()),
    head_count :: integer()
}).
