-record(user, {
    id :: binary(),
    name :: binary(),
    address :: nested_object_validation_test:address(),
    contact :: nested_object_validation_test:contact_info(),
    score :: integer(),
    active :: boolean()
}).
