-record(order, {
    id :: binary(),
    items :: list(nested_object_validation_test:line_item()),
    total :: float(),
    status :: binary()
}).
