-record(schema_builder, {
    queries :: list(mochi@schema:field_definition()),
    mutations :: list(mochi@schema:field_definition()),
    subscriptions :: list(mochi@schema:field_definition()),
    types :: list(mochi@schema:object_type()),
    enums :: list(mochi@schema:enum_type()),
    interfaces :: list(mochi@schema:interface_type()),
    unions :: list(mochi@schema:union_type())
}).
