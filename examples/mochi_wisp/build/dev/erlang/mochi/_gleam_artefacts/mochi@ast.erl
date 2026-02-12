-module(mochi@ast).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/ast.gleam").
-export_type([document/0, definition/0, operation/0, operation_type/0, selection_set/0, selection/0, field/0, fragment/0, fragment_spread_value/0, inline_fragment_value/0, variable_definition/0, argument/0, directive/0, type/0, value/0, object_field/0]).

-type document() :: {document, list(definition())}.

-type definition() :: {operation_definition, operation()} |
    {fragment_definition, fragment()}.

-type operation() :: {operation,
        operation_type(),
        gleam@option:option(binary()),
        list(variable_definition()),
        list(directive()),
        selection_set()} |
    {shorthand_query, selection_set()}.

-type operation_type() :: 'query' | mutation | subscription.

-type selection_set() :: {selection_set, list(selection())}.

-type selection() :: {field_selection, field()} |
    {fragment_spread, fragment_spread_value()} |
    {inline_fragment, inline_fragment_value()}.

-type field() :: {field,
        gleam@option:option(binary()),
        binary(),
        list(argument()),
        list(directive()),
        gleam@option:option(selection_set())}.

-type fragment() :: {fragment,
        binary(),
        binary(),
        list(directive()),
        selection_set()}.

-type fragment_spread_value() :: {fragment_spread_value,
        binary(),
        list(directive())}.

-type inline_fragment_value() :: {inline_fragment_value,
        gleam@option:option(binary()),
        list(directive()),
        selection_set()}.

-type variable_definition() :: {variable_definition,
        binary(),
        type(),
        gleam@option:option(value()),
        list(directive())}.

-type argument() :: {argument, binary(), value()}.

-type directive() :: {directive, binary(), list(argument())}.

-type type() :: {named_type, binary()} |
    {list_type, type()} |
    {non_null_type, type()}.

-type value() :: {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    {boolean_value, boolean()} |
    null_value |
    {enum_value, binary()} |
    {list_value, list(value())} |
    {object_value, list(object_field())} |
    {variable_value, binary()}.

-type object_field() :: {object_field, binary(), value()}.


