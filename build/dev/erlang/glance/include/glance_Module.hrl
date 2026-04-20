-record(module, {
    imports :: list(glance:definition(glance:import())),
    custom_types :: list(glance:definition(glance:custom_type())),
    type_aliases :: list(glance:definition(glance:type_alias())),
    constants :: list(glance:definition(glance:constant())),
    functions :: list(glance:definition(glance:function_()))
}).
