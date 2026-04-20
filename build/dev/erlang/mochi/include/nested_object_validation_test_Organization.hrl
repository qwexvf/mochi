-record(organization, {
    id :: binary(),
    name :: binary(),
    departments :: list(nested_object_validation_test:department()),
    ceo :: nested_object_validation_test:employee(),
    founded :: integer()
}).
