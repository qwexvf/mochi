-record(product, {
    id :: binary(),
    name :: binary(),
    category :: nested_object_validation_test:category(),
    price :: float(),
    in_stock :: boolean(),
    sku :: binary()
}).
