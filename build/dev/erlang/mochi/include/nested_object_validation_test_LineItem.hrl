-record(line_item, {
    product :: nested_object_validation_test:product(),
    quantity :: integer(),
    unit_price :: float()
}).
