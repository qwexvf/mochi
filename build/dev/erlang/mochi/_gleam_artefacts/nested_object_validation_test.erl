-module(nested_object_validation_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/nested_object_validation_test.gleam").
-export([invalid_field_on_parent_type_is_rejected_test/0, invalid_field_on_nested_type_is_rejected_test/0, field_from_nested_type_on_parent_is_rejected_test/0, field_from_parent_type_inside_nested_object_is_rejected_test/0, sibling_field_after_nested_object_is_valid_test/0, sibling_field_before_nested_object_is_valid_test/0, multiple_sibling_fields_after_nested_object_are_valid_test/0, two_nested_objects_then_sibling_is_valid_test/0, nested_object_only_no_siblings_is_valid_test/0, deeply_nested_object_with_sibling_is_valid_test/0, sibling_after_nested_at_depth_two_is_valid_test/0, equity_and_gto_deviation_both_resolve_test/0, user_siblings_after_nested_address_resolve_test/0, product_nested_category_then_scalar_siblings_test/0, scalar_then_nested_category_then_more_scalars_test/0, alias_on_nested_object_and_sibling_test/0, multiple_aliases_on_different_fields_test/0, typename_inside_nested_object_then_parent_sibling_test/0, typename_on_parent_and_in_nested_object_test/0, named_fragment_with_nested_and_scalar_fields_test/0, named_fragment_on_nested_type_test/0, named_fragment_sibling_after_spread_test/0, inline_fragment_on_nested_type_then_sibling_test/0, inline_fragment_without_type_condition_in_nested_test/0, mutation_returning_nested_type_with_siblings_test/0, same_nested_type_reused_with_siblings_between_test/0, interleaved_nested_and_scalar_fields_test/0, order_list_object_then_scalar_siblings_validation_test/0, order_scalars_before_and_after_list_object_test/0, invalid_field_after_list_object_is_rejected_test/0, field_from_list_item_type_on_parent_is_rejected_test/0, three_level_deep_nesting_with_siblings_test/0, siblings_after_nested_object_at_depth_two_org_test/0, field_from_nested_level_on_org_root_is_rejected_test/0]).
-export_type([address/0, contact_info/0, user/0, equity_by_street/0, detected_hand/0, comment/0, category/0, product/0, line_item/0, order/0, employee/0, department/0, organization/0]).

-type address() :: {address, binary(), binary(), binary()}.

-type contact_info() :: {contact_info, binary(), binary()}.

-type user() :: {user,
        binary(),
        binary(),
        address(),
        contact_info(),
        integer(),
        boolean()}.

-type equity_by_street() :: {equity_by_street,
        float(),
        float(),
        float(),
        float()}.

-type detected_hand() :: {detected_hand,
        binary(),
        binary(),
        binary(),
        equity_by_street(),
        float(),
        boolean()}.

-type comment() :: {comment, binary(), binary(), user()}.

-type category() :: {category, binary(), binary(), binary()}.

-type product() :: {product,
        binary(),
        binary(),
        category(),
        float(),
        boolean(),
        binary()}.

-type line_item() :: {line_item, product(), integer(), float()}.

-type order() :: {order, binary(), list(line_item()), float(), binary()}.

-type employee() :: {employee, binary(), binary(), binary()}.

-type department() :: {department,
        binary(),
        binary(),
        list(employee()),
        integer()}.

-type organization() :: {organization,
        binary(),
        binary(),
        list(department()),
        employee(),
        integer()}.

-file("test/nested_object_validation_test.gleam", 58).
-spec address_type() -> mochi@schema:object_type().
address_type() ->
    _pipe = mochi@types:object(<<"Address"/utf8>>),
    _pipe@1 = mochi@types:string(
        _pipe,
        <<"street"/utf8>>,
        fun(A) -> erlang:element(2, A) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"city"/utf8>>,
        fun(A@1) -> erlang:element(3, A@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"zip"/utf8>>,
        fun(A@2) -> erlang:element(4, A@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) -> {ok, {address, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/nested_object_validation_test.gleam", 66).
-spec contact_type() -> mochi@schema:object_type().
contact_type() ->
    _pipe = mochi@types:object(<<"ContactInfo"/utf8>>),
    _pipe@1 = mochi@types:string(
        _pipe,
        <<"email"/utf8>>,
        fun(C) -> erlang:element(2, C) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"phone"/utf8>>,
        fun(C@1) -> erlang:element(3, C@1) end
    ),
    mochi@types:build(
        _pipe@2,
        fun(_) -> {ok, {contact_info, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/nested_object_validation_test.gleam", 73).
-spec user_type() -> mochi@schema:object_type().
user_type() ->
    _pipe = mochi@types:object(<<"User"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(U) -> erlang:element(2, U) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(U@1) -> erlang:element(3, U@1) end
    ),
    _pipe@3 = mochi@types:object_field(
        _pipe@2,
        <<"address"/utf8>>,
        <<"Address"/utf8>>,
        fun(U@2) -> gleam_stdlib:identity(erlang:element(4, U@2)) end
    ),
    _pipe@4 = mochi@types:object_field(
        _pipe@3,
        <<"contact"/utf8>>,
        <<"ContactInfo"/utf8>>,
        fun(U@3) -> gleam_stdlib:identity(erlang:element(5, U@3)) end
    ),
    _pipe@5 = mochi@types:int(
        _pipe@4,
        <<"score"/utf8>>,
        fun(U@4) -> erlang:element(6, U@4) end
    ),
    _pipe@6 = mochi@types:bool(
        _pipe@5,
        <<"active"/utf8>>,
        fun(U@5) -> erlang:element(7, U@5) end
    ),
    mochi@types:build(
        _pipe@6,
        fun(_) ->
            {ok,
                {user,
                    <<"1"/utf8>>,
                    <<"Alice"/utf8>>,
                    {address,
                        <<"123 Main St"/utf8>>,
                        <<"Springfield"/utf8>>,
                        <<"12345"/utf8>>},
                    {contact_info,
                        <<"alice@example.com"/utf8>>,
                        <<"555-0100"/utf8>>},
                    42,
                    true}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 97).
-spec equity_type() -> mochi@schema:object_type().
equity_type() ->
    _pipe = mochi@types:object(<<"EquityByStreet"/utf8>>),
    _pipe@1 = mochi@types:float(
        _pipe,
        <<"preflop"/utf8>>,
        fun(E) -> erlang:element(2, E) end
    ),
    _pipe@2 = mochi@types:float(
        _pipe@1,
        <<"flop"/utf8>>,
        fun(E@1) -> erlang:element(3, E@1) end
    ),
    _pipe@3 = mochi@types:float(
        _pipe@2,
        <<"turn"/utf8>>,
        fun(E@2) -> erlang:element(4, E@2) end
    ),
    _pipe@4 = mochi@types:float(
        _pipe@3,
        <<"river"/utf8>>,
        fun(E@3) -> erlang:element(5, E@3) end
    ),
    mochi@types:build(
        _pipe@4,
        fun(_) -> {ok, {equity_by_street, +0.0, +0.0, +0.0, +0.0}} end
    ).

-file("test/nested_object_validation_test.gleam", 106).
-spec hand_type() -> mochi@schema:object_type().
hand_type() ->
    _pipe = mochi@types:object(<<"DetectedHand"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(H) -> erlang:element(2, H) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"holeCards"/utf8>>,
        fun(H@1) -> erlang:element(3, H@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"board"/utf8>>,
        fun(H@2) -> erlang:element(4, H@2) end
    ),
    _pipe@4 = mochi@types:object_field(
        _pipe@3,
        <<"equity"/utf8>>,
        <<"EquityByStreet"/utf8>>,
        fun(H@3) -> gleam_stdlib:identity(erlang:element(5, H@3)) end
    ),
    _pipe@5 = mochi@types:float(
        _pipe@4,
        <<"gtoDeviation"/utf8>>,
        fun(H@4) -> erlang:element(6, H@4) end
    ),
    _pipe@6 = mochi@types:bool(
        _pipe@5,
        <<"accepted"/utf8>>,
        fun(H@5) -> erlang:element(7, H@5) end
    ),
    mochi@types:build(
        _pipe@6,
        fun(_) ->
            {ok,
                {detected_hand,
                    <<"1"/utf8>>,
                    <<"As Kh"/utf8>>,
                    <<"Qd Jc Ts"/utf8>>,
                    {equity_by_street, 0.6, 0.55, 0.7, 0.9},
                    0.05,
                    false}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 128).
-spec comment_type() -> mochi@schema:object_type().
comment_type() ->
    _pipe = mochi@types:object(<<"Comment"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(C) -> erlang:element(2, C) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"body"/utf8>>,
        fun(C@1) -> erlang:element(3, C@1) end
    ),
    _pipe@3 = mochi@types:object_field(
        _pipe@2,
        <<"author"/utf8>>,
        <<"User"/utf8>>,
        fun(C@2) -> gleam_stdlib:identity(erlang:element(4, C@2)) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) ->
            {ok,
                {comment,
                    <<"1"/utf8>>,
                    <<"Nice post"/utf8>>,
                    {user,
                        <<"2"/utf8>>,
                        <<"Bob"/utf8>>,
                        {address, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
                        {contact_info, <<""/utf8>>, <<""/utf8>>},
                        0,
                        true}}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 144).
-spec user_schema() -> mochi@schema:schema().
user_schema() ->
    The_user = {user,
        <<"1"/utf8>>,
        <<"Alice"/utf8>>,
        {address,
            <<"123 Main St"/utf8>>,
            <<"Springfield"/utf8>>,
            <<"12345"/utf8>>},
        {contact_info, <<"alice@example.com"/utf8>>, <<"555-0100"/utf8>>},
        42,
        true},
    User_query = mochi@query:'query'(
        <<"user"/utf8>>,
        mochi@schema:named_type(<<"User"/utf8>>),
        fun(_) -> {ok, The_user} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, User_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, user_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, address_type()),
    _pipe@4 = mochi@query:add_type(_pipe@3, contact_type()),
    mochi@query:build(_pipe@4).

-file("test/nested_object_validation_test.gleam", 171).
-spec hand_schema() -> mochi@schema:schema().
hand_schema() ->
    The_hand = {detected_hand,
        <<"hand-1"/utf8>>,
        <<"As Kh"/utf8>>,
        <<"Qd Jc Ts"/utf8>>,
        {equity_by_street, 0.62, 0.55, 0.71, 0.9},
        0.05,
        false},
    Hand_query = mochi@query:'query'(
        <<"hand"/utf8>>,
        mochi@schema:named_type(<<"DetectedHand"/utf8>>),
        fun(_) -> {ok, The_hand} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Hand_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, hand_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, equity_type()),
    mochi@query:build(_pipe@3).

-file("test/nested_object_validation_test.gleam", 197).
-spec comment_schema() -> mochi@schema:schema().
comment_schema() ->
    The_comment = {comment,
        <<"c1"/utf8>>,
        <<"Nice post"/utf8>>,
        {user,
            <<"2"/utf8>>,
            <<"Bob"/utf8>>,
            {address,
                <<"456 Elm St"/utf8>>,
                <<"Shelbyville"/utf8>>,
                <<"67890"/utf8>>},
            {contact_info, <<"bob@example.com"/utf8>>, <<"555-0200"/utf8>>},
            10,
            false}},
    Comment_query = mochi@query:'query'(
        <<"comment"/utf8>>,
        mochi@schema:named_type(<<"Comment"/utf8>>),
        fun(_) -> {ok, The_comment} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Comment_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, comment_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, user_type()),
    _pipe@4 = mochi@query:add_type(_pipe@3, address_type()),
    _pipe@5 = mochi@query:add_type(_pipe@4, contact_type()),
    mochi@query:build(_pipe@5).

-file("test/nested_object_validation_test.gleam", 302).
-spec invalid_field_on_parent_type_is_rejected_test() -> nil.
invalid_field_on_parent_type_is_rejected_test() ->
    S = hand_schema(),
    Q = <<"{ hand { equity { preflop } nonExistentField } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for nonExistentField"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"invalid_field_on_parent_type_is_rejected_test"/utf8>>,
                    line => 307});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 312).
-spec invalid_field_on_nested_type_is_rejected_test() -> nil.
invalid_field_on_nested_type_is_rejected_test() ->
    S = hand_schema(),
    Q = <<"{ hand { equity { preflop bogus } } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for bogus field on EquityByStreet"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"invalid_field_on_nested_type_is_rejected_test"/utf8>>,
                    line => 317});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 322).
-spec field_from_nested_type_on_parent_is_rejected_test() -> nil.
field_from_nested_type_on_parent_is_rejected_test() ->
    S = hand_schema(),
    Q = <<"{ hand { preflop } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error: preflop is not on DetectedHand"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"field_from_nested_type_on_parent_is_rejected_test"/utf8>>,
                    line => 328});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 333).
-spec field_from_parent_type_inside_nested_object_is_rejected_test() -> nil.
field_from_parent_type_inside_nested_object_is_rejected_test() ->
    S = hand_schema(),
    Q = <<"{ hand { equity { preflop gtoDeviation } } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error: gtoDeviation is not on EquityByStreet"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"field_from_parent_type_inside_nested_object_is_rejected_test"/utf8>>,
                    line => 340});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 437).
-spec category_type() -> mochi@schema:object_type().
category_type() ->
    _pipe = mochi@types:object(<<"Category"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(C) -> erlang:element(2, C) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(C@1) -> erlang:element(3, C@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"slug"/utf8>>,
        fun(C@2) -> erlang:element(4, C@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) -> {ok, {category, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/nested_object_validation_test.gleam", 445).
-spec product_type() -> mochi@schema:object_type().
product_type() ->
    _pipe = mochi@types:object(<<"Product"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(P) -> erlang:element(2, P) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(P@1) -> erlang:element(3, P@1) end
    ),
    _pipe@3 = mochi@types:object_field(
        _pipe@2,
        <<"category"/utf8>>,
        <<"Category"/utf8>>,
        fun(P@2) -> gleam_stdlib:identity(erlang:element(4, P@2)) end
    ),
    _pipe@4 = mochi@types:float(
        _pipe@3,
        <<"price"/utf8>>,
        fun(P@3) -> erlang:element(5, P@3) end
    ),
    _pipe@5 = mochi@types:bool(
        _pipe@4,
        <<"inStock"/utf8>>,
        fun(P@4) -> erlang:element(6, P@4) end
    ),
    _pipe@6 = mochi@types:string(
        _pipe@5,
        <<"sku"/utf8>>,
        fun(P@5) -> erlang:element(7, P@5) end
    ),
    mochi@types:build(
        _pipe@6,
        fun(_) ->
            {ok,
                {product,
                    <<""/utf8>>,
                    <<""/utf8>>,
                    {category, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
                    +0.0,
                    false,
                    <<""/utf8>>}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 460).
-spec line_item_type() -> mochi@schema:object_type().
line_item_type() ->
    _pipe = mochi@types:object(<<"LineItem"/utf8>>),
    _pipe@1 = mochi@types:object_field(
        _pipe,
        <<"product"/utf8>>,
        <<"Product"/utf8>>,
        fun(Li) -> gleam_stdlib:identity(erlang:element(2, Li)) end
    ),
    _pipe@2 = mochi@types:int(
        _pipe@1,
        <<"quantity"/utf8>>,
        fun(Li@1) -> erlang:element(3, Li@1) end
    ),
    _pipe@3 = mochi@types:float(
        _pipe@2,
        <<"unitPrice"/utf8>>,
        fun(Li@2) -> erlang:element(4, Li@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) ->
            {ok,
                {line_item,
                    {product,
                        <<""/utf8>>,
                        <<""/utf8>>,
                        {category, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
                        +0.0,
                        false,
                        <<""/utf8>>},
                    0,
                    +0.0}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 511).
-spec the_order() -> order().
the_order() ->
    Cat = {category,
        <<"cat-1"/utf8>>,
        <<"Electronics"/utf8>>,
        <<"electronics"/utf8>>},
    Prod = {product,
        <<"prod-1"/utf8>>,
        <<"Mechanical Keyboard"/utf8>>,
        Cat,
        149.99,
        true,
        <<"KB-MX-001"/utf8>>},
    {order,
        <<"ord-1"/utf8>>,
        [{line_item, Prod, 2, 149.99}],
        299.98,
        <<"PENDING"/utf8>>}.

-file("test/nested_object_validation_test.gleam", 536).
-spec product_schema() -> mochi@schema:schema().
product_schema() ->
    Cat = {category,
        <<"cat-1"/utf8>>,
        <<"Electronics"/utf8>>,
        <<"electronics"/utf8>>},
    Prod = {product,
        <<"prod-1"/utf8>>,
        <<"Mechanical Keyboard"/utf8>>,
        Cat,
        149.99,
        true,
        <<"KB-MX-001"/utf8>>},
    Product_query = mochi@query:'query'(
        <<"product"/utf8>>,
        mochi@schema:named_type(<<"Product"/utf8>>),
        fun(_) -> {ok, Prod} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Product_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, product_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, category_type()),
    mochi@query:build(_pipe@3).

-file("test/nested_object_validation_test.gleam", 659).
-spec employee_type() -> mochi@schema:object_type().
employee_type() ->
    _pipe = mochi@types:object(<<"Employee"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(E) -> erlang:element(2, E) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(E@1) -> erlang:element(3, E@1) end
    ),
    _pipe@3 = mochi@types:string(
        _pipe@2,
        <<"role"/utf8>>,
        fun(E@2) -> erlang:element(4, E@2) end
    ),
    mochi@types:build(
        _pipe@3,
        fun(_) -> {ok, {employee, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>}} end
    ).

-file("test/nested_object_validation_test.gleam", 1037).
-spec string_join(list(QTZ), fun((QTZ) -> binary())) -> binary().
string_join(Items, F) ->
    case Items of
        [] ->
            <<""/utf8>>;

        [X] ->
            F(X);

        [X@1 | Rest] ->
            <<<<(F(X@1))/binary, ", "/utf8>>/binary,
                (string_join(Rest, F))/binary>>
    end.

-file("test/nested_object_validation_test.gleam", 233).
-spec sibling_field_after_nested_object_is_valid_test() -> nil.
sibling_field_after_nested_object_is_valid_test() ->
    S = hand_schema(),
    Q = <<"{ hand { id equity { preflop flop turn river } gtoDeviation accepted } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"sibling_field_after_nested_object_is_valid_test"/utf8>>,
                    line => 241})
    end.

-file("test/nested_object_validation_test.gleam", 247).
-spec sibling_field_before_nested_object_is_valid_test() -> nil.
sibling_field_before_nested_object_is_valid_test() ->
    S = hand_schema(),
    Q = <<"{ hand { id gtoDeviation equity { preflop flop } accepted } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"sibling_field_before_nested_object_is_valid_test"/utf8>>,
                    line => 254})
    end.

-file("test/nested_object_validation_test.gleam", 260).
-spec multiple_sibling_fields_after_nested_object_are_valid_test() -> nil.
multiple_sibling_fields_after_nested_object_are_valid_test() ->
    S = user_schema(),
    Q = <<"{ user { id address { street city zip } score active name } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"multiple_sibling_fields_after_nested_object_are_valid_test"/utf8>>,
                    line => 267})
    end.

-file("test/nested_object_validation_test.gleam", 273).
-spec two_nested_objects_then_sibling_is_valid_test() -> nil.
two_nested_objects_then_sibling_is_valid_test() ->
    S = user_schema(),
    Q = <<"{ user { id address { street city } contact { email phone } score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"two_nested_objects_then_sibling_is_valid_test"/utf8>>,
                    line => 281})
    end.

-file("test/nested_object_validation_test.gleam", 287).
-spec nested_object_only_no_siblings_is_valid_test() -> nil.
nested_object_only_no_siblings_is_valid_test() ->
    S = hand_schema(),
    Q = <<"{ hand { equity { preflop flop turn river } } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"nested_object_only_no_siblings_is_valid_test"/utf8>>,
                    line => 294})
    end.

-file("test/nested_object_validation_test.gleam", 347).
-spec deeply_nested_object_with_sibling_is_valid_test() -> nil.
deeply_nested_object_with_sibling_is_valid_test() ->
    S = comment_schema(),
    Q = <<"{ comment { id body author { id name address { street city } score } } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"deeply_nested_object_with_sibling_is_valid_test"/utf8>>,
                    line => 355})
    end.

-file("test/nested_object_validation_test.gleam", 361).
-spec sibling_after_nested_at_depth_two_is_valid_test() -> nil.
sibling_after_nested_at_depth_two_is_valid_test() ->
    S = comment_schema(),
    Q = <<"{ comment { id author { name address { street } active score } body } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"sibling_after_nested_at_depth_two_is_valid_test"/utf8>>,
                    line => 370})
    end.

-file("test/nested_object_validation_test.gleam", 378).
-spec equity_and_gto_deviation_both_resolve_test() -> nil.
equity_and_gto_deviation_both_resolve_test() ->
    S = hand_schema(),
    Q = <<"{ hand { equity { preflop river } gtoDeviation accepted } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"equity_and_gto_deviation_both_resolve_test"/utf8>>,
                    line => 385})
    end,
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"equity_and_gto_deviation_both_resolve_test"/utf8>>,
                    line => 390});

        {some, _} ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 395).
-spec user_siblings_after_nested_address_resolve_test() -> nil.
user_siblings_after_nested_address_resolve_test() ->
    S = user_schema(),
    Q = <<"{ user { address { city } name score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"user_siblings_after_nested_address_resolve_test"/utf8>>,
                    line => 402})
    end,
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"user_siblings_after_nested_address_resolve_test"/utf8>>,
                    line => 407});

        {some, _} ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 556).
-spec product_nested_category_then_scalar_siblings_test() -> nil.
product_nested_category_then_scalar_siblings_test() ->
    S = product_schema(),
    Q = <<"{ product { id name category { id name slug } price inStock sku } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"product_nested_category_then_scalar_siblings_test"/utf8>>,
                    line => 563})
    end,
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"product_nested_category_then_scalar_siblings_test"/utf8>>,
                    line => 568});

        {some, _} ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 573).
-spec scalar_then_nested_category_then_more_scalars_test() -> nil.
scalar_then_nested_category_then_more_scalars_test() ->
    S = product_schema(),
    Q = <<"{ product { sku name category { slug name } price inStock id } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"scalar_then_nested_category_then_more_scalars_test"/utf8>>,
                    line => 580})
    end.

-file("test/nested_object_validation_test.gleam", 790).
-spec alias_on_nested_object_and_sibling_test() -> nil.
alias_on_nested_object_and_sibling_test() ->
    S = user_schema(),
    Q = <<"{ user { homeAddress: address { city zip } fullName: name score active } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"alias_on_nested_object_and_sibling_test"/utf8>>,
                    line => 798})
    end.

-file("test/nested_object_validation_test.gleam", 804).
-spec multiple_aliases_on_different_fields_test() -> nil.
multiple_aliases_on_different_fields_test() ->
    S = user_schema(),
    Q = <<"{ user { addr: address { street city } info: contact { email phone } points: score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"multiple_aliases_on_different_fields_test"/utf8>>,
                    line => 812})
    end.

-file("test/nested_object_validation_test.gleam", 820).
-spec typename_inside_nested_object_then_parent_sibling_test() -> nil.
typename_inside_nested_object_then_parent_sibling_test() ->
    S = user_schema(),
    Q = <<"{ user { address { __typename city zip } name score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"typename_inside_nested_object_then_parent_sibling_test"/utf8>>,
                    line => 827})
    end.

-file("test/nested_object_validation_test.gleam", 833).
-spec typename_on_parent_and_in_nested_object_test() -> nil.
typename_on_parent_and_in_nested_object_test() ->
    S = user_schema(),
    Q = <<"{ user { __typename id address { __typename street } active } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"typename_on_parent_and_in_nested_object_test"/utf8>>,
                    line => 840})
    end.

-file("test/nested_object_validation_test.gleam", 848).
-spec named_fragment_with_nested_and_scalar_fields_test() -> nil.
named_fragment_with_nested_and_scalar_fields_test() ->
    S = user_schema(),
    Q = <<"
    {
      user {
        ...UserProfile
      }
    }
    fragment UserProfile on User {
      id
      address { street city zip }
      name
      score
      active
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"named_fragment_with_nested_and_scalar_fields_test"/utf8>>,
                    line => 869})
    end.

-file("test/nested_object_validation_test.gleam", 875).
-spec named_fragment_on_nested_type_test() -> nil.
named_fragment_on_nested_type_test() ->
    S = comment_schema(),
    Q = <<"
    {
      comment {
        id
        author {
          ...AuthorDetails
          score
          active
        }
        body
      }
    }
    fragment AuthorDetails on User {
      id
      name
      address { street city }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"named_fragment_on_nested_type_test"/utf8>>,
                    line => 900})
    end.

-file("test/nested_object_validation_test.gleam", 906).
-spec named_fragment_sibling_after_spread_test() -> nil.
named_fragment_sibling_after_spread_test() ->
    S = comment_schema(),
    Q = <<"
    {
      comment {
        ...CommentCore
        body
      }
    }
    fragment CommentCore on Comment {
      id
      author { name address { city } score }
    }
    "/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"named_fragment_sibling_after_spread_test"/utf8>>,
                    line => 925})
    end.

-file("test/nested_object_validation_test.gleam", 933).
-spec inline_fragment_on_nested_type_then_sibling_test() -> nil.
inline_fragment_on_nested_type_then_sibling_test() ->
    S = user_schema(),
    Q = <<"{ user { address { ... on Address { city zip } street } name score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"inline_fragment_on_nested_type_then_sibling_test"/utf8>>,
                    line => 941})
    end.

-file("test/nested_object_validation_test.gleam", 947).
-spec inline_fragment_without_type_condition_in_nested_test() -> nil.
inline_fragment_without_type_condition_in_nested_test() ->
    S = user_schema(),
    Q = <<"{ user { address { ... { city } street } name score } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"inline_fragment_without_type_condition_in_nested_test"/utf8>>,
                    line => 954})
    end.

-file("test/nested_object_validation_test.gleam", 962).
-spec mutation_returning_nested_type_with_siblings_test() -> nil.
mutation_returning_nested_type_with_siblings_test() ->
    Prod = {product,
        <<"new-1"/utf8>>,
        <<"Wireless Mouse"/utf8>>,
        {category,
            <<"cat-2"/utf8>>,
            <<"Peripherals"/utf8>>,
            <<"peripherals"/utf8>>},
        39.99,
        true,
        <<"MS-WL-002"/utf8>>},
    Create_product = mochi@query:mutation(
        <<"createProduct"/utf8>>,
        [mochi@query:arg(
                <<"name"/utf8>>,
                mochi@schema:non_null(mochi@schema:string_type())
            )],
        mochi@schema:named_type(<<"Product"/utf8>>),
        fun(Args) -> mochi@query:get_string(Args, <<"name"/utf8>>) end,
        fun(_, _) -> {ok, Prod} end,
        fun gleam_stdlib:identity/1
    ),
    S = begin
        _pipe = mochi@query:new(),
        _pipe@1 = mochi@query:add_mutation(_pipe, Create_product),
        _pipe@2 = mochi@query:add_type(_pipe@1, product_type()),
        _pipe@3 = mochi@query:add_type(_pipe@2, category_type()),
        mochi@query:build(_pipe@3)
    end,
    Q = <<"mutation { createProduct(name: \"Wireless Mouse\") { id name category { id name slug } price inStock sku } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"mutation_returning_nested_type_with_siblings_test"/utf8>>,
                    line => 996})
    end,
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"mutation_returning_nested_type_with_siblings_test"/utf8>>,
                    line => 1001});

        {some, _} ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 1008).
-spec same_nested_type_reused_with_siblings_between_test() -> nil.
same_nested_type_reused_with_siblings_between_test() ->
    S = user_schema(),
    Q = <<"{ user { id address { street city } score contact { email phone } active name } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"same_nested_type_reused_with_siblings_between_test"/utf8>>,
                    line => 1016})
    end.

-file("test/nested_object_validation_test.gleam", 1022).
-spec interleaved_nested_and_scalar_fields_test() -> nil.
interleaved_nested_and_scalar_fields_test() ->
    S = user_schema(),
    Q = <<"{ user { id address { city } name contact { email } score active } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"interleaved_nested_and_scalar_fields_test"/utf8>>,
                    line => 1029})
    end.

-file("test/nested_object_validation_test.gleam", 1045).
-spec list_to_dynamic(list(QUB), fun((QUB) -> gleam@dynamic:dynamic_())) -> list(gleam@dynamic:dynamic_()).
list_to_dynamic(Items, F) ->
    case Items of
        [] ->
            [];

        [X | Rest] ->
            [F(X) | list_to_dynamic(Rest, F)]
    end.

-file("test/nested_object_validation_test.gleam", 472).
-spec order_type() -> mochi@schema:object_type().
order_type() ->
    _pipe = mochi@types:object(<<"Order"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(O) -> erlang:element(2, O) end
    ),
    _pipe@5 = mochi@types:list_object(
        _pipe@1,
        <<"items"/utf8>>,
        <<"LineItem"/utf8>>,
        fun(O@1) ->
            gleam_stdlib:identity(
                list_to_dynamic(
                    erlang:element(3, O@1),
                    fun(Li) ->
                        _pipe@4 = maps:from_list(
                            [{<<"product"/utf8>>,
                                    begin
                                        _pipe@3 = maps:from_list(
                                            [{<<"id"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(
                                                            2,
                                                            erlang:element(
                                                                2,
                                                                Li
                                                            )
                                                        )
                                                    )},
                                                {<<"name"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(
                                                            3,
                                                            erlang:element(
                                                                2,
                                                                Li
                                                            )
                                                        )
                                                    )},
                                                {<<"price"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(
                                                            5,
                                                            erlang:element(
                                                                2,
                                                                Li
                                                            )
                                                        )
                                                    )},
                                                {<<"inStock"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(
                                                            6,
                                                            erlang:element(
                                                                2,
                                                                Li
                                                            )
                                                        )
                                                    )},
                                                {<<"sku"/utf8>>,
                                                    gleam_stdlib:identity(
                                                        erlang:element(
                                                            7,
                                                            erlang:element(
                                                                2,
                                                                Li
                                                            )
                                                        )
                                                    )},
                                                {<<"category"/utf8>>,
                                                    begin
                                                        _pipe@2 = maps:from_list(
                                                            [{<<"id"/utf8>>,
                                                                    gleam_stdlib:identity(
                                                                        erlang:element(
                                                                            2,
                                                                            erlang:element(
                                                                                4,
                                                                                erlang:element(
                                                                                    2,
                                                                                    Li
                                                                                )
                                                                            )
                                                                        )
                                                                    )},
                                                                {<<"name"/utf8>>,
                                                                    gleam_stdlib:identity(
                                                                        erlang:element(
                                                                            3,
                                                                            erlang:element(
                                                                                4,
                                                                                erlang:element(
                                                                                    2,
                                                                                    Li
                                                                                )
                                                                            )
                                                                        )
                                                                    )},
                                                                {<<"slug"/utf8>>,
                                                                    gleam_stdlib:identity(
                                                                        erlang:element(
                                                                            4,
                                                                            erlang:element(
                                                                                4,
                                                                                erlang:element(
                                                                                    2,
                                                                                    Li
                                                                                )
                                                                            )
                                                                        )
                                                                    )}]
                                                        ),
                                                        gleam_stdlib:identity(
                                                            _pipe@2
                                                        )
                                                    end}]
                                        ),
                                        gleam_stdlib:identity(_pipe@3)
                                    end},
                                {<<"quantity"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(3, Li))},
                                {<<"unitPrice"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(4, Li))}]
                        ),
                        gleam_stdlib:identity(_pipe@4)
                    end
                )
            )
        end
    ),
    _pipe@6 = mochi@types:float(
        _pipe@5,
        <<"total"/utf8>>,
        fun(O@2) -> erlang:element(4, O@2) end
    ),
    _pipe@7 = mochi@types:string(
        _pipe@6,
        <<"status"/utf8>>,
        fun(O@3) -> erlang:element(5, O@3) end
    ),
    mochi@types:build(
        _pipe@7,
        fun(_) -> {ok, {order, <<""/utf8>>, [], +0.0, <<""/utf8>>}} end
    ).

-file("test/nested_object_validation_test.gleam", 518).
-spec order_schema() -> mochi@schema:schema().
order_schema() ->
    Order_query = mochi@query:'query'(
        <<"order"/utf8>>,
        mochi@schema:named_type(<<"Order"/utf8>>),
        fun(_) -> {ok, the_order()} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Order_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, order_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, line_item_type()),
    _pipe@4 = mochi@query:add_type(_pipe@3, product_type()),
    _pipe@5 = mochi@query:add_type(_pipe@4, category_type()),
    mochi@query:build(_pipe@5).

-file("test/nested_object_validation_test.gleam", 586).
-spec order_list_object_then_scalar_siblings_validation_test() -> nil.
order_list_object_then_scalar_siblings_validation_test() ->
    S = order_schema(),
    Q = <<"{ order { id items { quantity unitPrice product { name price } } total status } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"order_list_object_then_scalar_siblings_validation_test"/utf8>>,
                    line => 594})
    end.

-file("test/nested_object_validation_test.gleam", 600).
-spec order_scalars_before_and_after_list_object_test() -> nil.
order_scalars_before_and_after_list_object_test() ->
    S = order_schema(),
    Q = <<"{ order { status items { quantity product { name category { name } } } total id } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"order_scalars_before_and_after_list_object_test"/utf8>>,
                    line => 608})
    end.

-file("test/nested_object_validation_test.gleam", 614).
-spec invalid_field_after_list_object_is_rejected_test() -> nil.
invalid_field_after_list_object_is_rejected_test() ->
    S = order_schema(),
    Q = <<"{ order { items { quantity } bogusField } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected validation error for bogusField on Order"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"invalid_field_after_list_object_is_rejected_test"/utf8>>,
                    line => 619});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 624).
-spec field_from_list_item_type_on_parent_is_rejected_test() -> nil.
field_from_list_item_type_on_parent_is_rejected_test() ->
    S = order_schema(),
    Q = <<"{ order { quantity } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected error: quantity is on LineItem, not Order"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"field_from_list_item_type_on_parent_is_rejected_test"/utf8>>,
                    line => 629});

        _ ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 667).
-spec department_type() -> mochi@schema:object_type().
department_type() ->
    _pipe = mochi@types:object(<<"Department"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(D) -> erlang:element(2, D) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(D@1) -> erlang:element(3, D@1) end
    ),
    _pipe@4 = mochi@types:list_object(
        _pipe@2,
        <<"employees"/utf8>>,
        <<"Employee"/utf8>>,
        fun(D@2) ->
            gleam_stdlib:identity(
                list_to_dynamic(
                    erlang:element(4, D@2),
                    fun(E) ->
                        _pipe@3 = maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(2, E))},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(3, E))},
                                {<<"role"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(4, E))}]
                        ),
                        gleam_stdlib:identity(_pipe@3)
                    end
                )
            )
        end
    ),
    _pipe@5 = mochi@types:int(
        _pipe@4,
        <<"headCount"/utf8>>,
        fun(D@3) -> erlang:element(5, D@3) end
    ),
    mochi@types:build(
        _pipe@5,
        fun(_) -> {ok, {department, <<""/utf8>>, <<""/utf8>>, [], 0}} end
    ).

-file("test/nested_object_validation_test.gleam", 687).
-spec organization_type() -> mochi@schema:object_type().
organization_type() ->
    _pipe = mochi@types:object(<<"Organization"/utf8>>),
    _pipe@1 = mochi@types:id(
        _pipe,
        <<"id"/utf8>>,
        fun(O) -> erlang:element(2, O) end
    ),
    _pipe@2 = mochi@types:string(
        _pipe@1,
        <<"name"/utf8>>,
        fun(O@1) -> erlang:element(3, O@1) end
    ),
    _pipe@6 = mochi@types:list_object(
        _pipe@2,
        <<"departments"/utf8>>,
        <<"Department"/utf8>>,
        fun(O@2) ->
            gleam_stdlib:identity(
                list_to_dynamic(
                    erlang:element(4, O@2),
                    fun(D) ->
                        _pipe@5 = maps:from_list(
                            [{<<"id"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(2, D))},
                                {<<"name"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(3, D))},
                                {<<"headCount"/utf8>>,
                                    gleam_stdlib:identity(erlang:element(5, D))},
                                {<<"employees"/utf8>>,
                                    begin
                                        _pipe@4 = list_to_dynamic(
                                            erlang:element(4, D),
                                            fun(E) ->
                                                _pipe@3 = maps:from_list(
                                                    [{<<"id"/utf8>>,
                                                            gleam_stdlib:identity(
                                                                erlang:element(
                                                                    2,
                                                                    E
                                                                )
                                                            )},
                                                        {<<"name"/utf8>>,
                                                            gleam_stdlib:identity(
                                                                erlang:element(
                                                                    3,
                                                                    E
                                                                )
                                                            )},
                                                        {<<"role"/utf8>>,
                                                            gleam_stdlib:identity(
                                                                erlang:element(
                                                                    4,
                                                                    E
                                                                )
                                                            )}]
                                                ),
                                                gleam_stdlib:identity(_pipe@3)
                                            end
                                        ),
                                        gleam_stdlib:identity(_pipe@4)
                                    end}]
                        ),
                        gleam_stdlib:identity(_pipe@5)
                    end
                )
            )
        end
    ),
    _pipe@7 = mochi@types:object_field(
        _pipe@6,
        <<"ceo"/utf8>>,
        <<"Employee"/utf8>>,
        fun(O@3) -> gleam_stdlib:identity(erlang:element(5, O@3)) end
    ),
    _pipe@8 = mochi@types:int(
        _pipe@7,
        <<"founded"/utf8>>,
        fun(O@4) -> erlang:element(6, O@4) end
    ),
    mochi@types:build(
        _pipe@8,
        fun(_) ->
            {ok,
                {organization,
                    <<""/utf8>>,
                    <<""/utf8>>,
                    [],
                    {employee, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
                    0}}
        end
    ).

-file("test/nested_object_validation_test.gleam", 722).
-spec org_schema() -> mochi@schema:schema().
org_schema() ->
    Eng = {employee, <<"e1"/utf8>>, <<"Alice"/utf8>>, <<"ENGINEER"/utf8>>},
    Des = {employee, <<"e2"/utf8>>, <<"Bob"/utf8>>, <<"DESIGNER"/utf8>>},
    Ceo = {employee, <<"e0"/utf8>>, <<"Carol"/utf8>>, <<"CEO"/utf8>>},
    Dept = {department, <<"d1"/utf8>>, <<"Product"/utf8>>, [Eng, Des], 2},
    Org = {organization,
        <<"org-1"/utf8>>,
        <<"Acme Corp"/utf8>>,
        [Dept],
        Ceo,
        2010},
    Org_query = mochi@query:'query'(
        <<"org"/utf8>>,
        mochi@schema:named_type(<<"Organization"/utf8>>),
        fun(_) -> {ok, Org} end,
        fun gleam_stdlib:identity/1
    ),
    _pipe = mochi@query:new(),
    _pipe@1 = mochi@query:add_query(_pipe, Org_query),
    _pipe@2 = mochi@query:add_type(_pipe@1, organization_type()),
    _pipe@3 = mochi@query:add_type(_pipe@2, department_type()),
    _pipe@4 = mochi@query:add_type(_pipe@3, employee_type()),
    mochi@query:build(_pipe@4).

-file("test/nested_object_validation_test.gleam", 745).
-spec three_level_deep_nesting_with_siblings_test() -> nil.
three_level_deep_nesting_with_siblings_test() ->
    S = org_schema(),
    Q = <<"{ org { id departments { name employees { id name role } headCount } ceo { name role } founded } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"three_level_deep_nesting_with_siblings_test"/utf8>>,
                    line => 753})
    end,
    case erlang:element(2, Result) of
        none ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected data"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"three_level_deep_nesting_with_siblings_test"/utf8>>,
                    line => 758});

        {some, _} ->
            nil
    end.

-file("test/nested_object_validation_test.gleam", 763).
-spec siblings_after_nested_object_at_depth_two_org_test() -> nil.
siblings_after_nested_object_at_depth_two_org_test() ->
    S = org_schema(),
    Q = <<"{ org { departments { employees { name } headCount id name } name founded } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            nil;

        Errs ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"Expected no errors but got: "/utf8,
                        (string_join(Errs, fun(E) -> erlang:element(2, E) end))/binary>>),
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"siblings_after_nested_object_at_depth_two_org_test"/utf8>>,
                    line => 771})
    end.

-file("test/nested_object_validation_test.gleam", 777).
-spec field_from_nested_level_on_org_root_is_rejected_test() -> nil.
field_from_nested_level_on_org_root_is_rejected_test() ->
    S = org_schema(),
    Q = <<"{ org { headCount } }"/utf8>>,
    Result = mochi@executor:execute_query(S, Q),
    case erlang:element(3, Result) of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Expected error: headCount is on Department, not Organization"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"nested_object_validation_test"/utf8>>,
                    function => <<"field_from_nested_level_on_org_root_is_rejected_test"/utf8>>,
                    line => 783});

        _ ->
            nil
    end.
