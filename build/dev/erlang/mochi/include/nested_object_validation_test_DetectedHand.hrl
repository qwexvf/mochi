-record(detected_hand, {
    id :: binary(),
    hole_cards :: binary(),
    board :: binary(),
    equity :: nested_object_validation_test:equity_by_street(),
    gto_deviation :: float(),
    accepted :: boolean()
}).
