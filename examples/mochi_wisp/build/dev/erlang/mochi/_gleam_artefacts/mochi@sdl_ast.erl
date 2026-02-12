-module(mochi@sdl_ast).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/sdl_ast.gleam").
-export([get_type_name/1, sdl_type_to_string/1, is_non_null_type/1, is_list_type/1, get_base_type/1]).
-export_type([s_d_l_document/0, type_system_definition/0, type_def/0, object_type_def/0, field_def/0, argument_def/0, s_d_l_type/0, s_d_l_value/0, object_field_value/0, interface_type_def/0, union_type_def/0, scalar_type_def/0, enum_type_def/0, enum_value_def/0, input_object_type_def/0, input_field_def/0, directive_def/0, directive_usage/0, directive_argument/0, directive_location/0, schema_def/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type s_d_l_document() :: {s_d_l_document, list(type_system_definition())}.

-type type_system_definition() :: {type_definition, type_def()} |
    {directive_definition, directive_def()} |
    {schema_definition, schema_def()}.

-type type_def() :: {object_type_definition, object_type_def()} |
    {interface_type_definition, interface_type_def()} |
    {union_type_definition, union_type_def()} |
    {scalar_type_definition, scalar_type_def()} |
    {enum_type_definition, enum_type_def()} |
    {input_object_type_definition, input_object_type_def()}.

-type object_type_def() :: {object_type_def,
        binary(),
        gleam@option:option(binary()),
        list(binary()),
        list(directive_usage()),
        list(field_def())}.

-type field_def() :: {field_def,
        binary(),
        gleam@option:option(binary()),
        list(argument_def()),
        s_d_l_type(),
        list(directive_usage())}.

-type argument_def() :: {argument_def,
        binary(),
        gleam@option:option(binary()),
        s_d_l_type(),
        gleam@option:option(s_d_l_value()),
        list(directive_usage())}.

-type s_d_l_type() :: {named_type, binary()} |
    {list_type, s_d_l_type()} |
    {non_null_type, s_d_l_type()}.

-type s_d_l_value() :: {int_value, integer()} |
    {float_value, float()} |
    {string_value, binary()} |
    {boolean_value, boolean()} |
    null_value |
    {enum_value, binary()} |
    {list_value, list(s_d_l_value())} |
    {object_value, list(object_field_value())}.

-type object_field_value() :: {object_field_value, binary(), s_d_l_value()}.

-type interface_type_def() :: {interface_type_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage()),
        list(field_def())}.

-type union_type_def() :: {union_type_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage()),
        list(binary())}.

-type scalar_type_def() :: {scalar_type_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage())}.

-type enum_type_def() :: {enum_type_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage()),
        list(enum_value_def())}.

-type enum_value_def() :: {enum_value_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage())}.

-type input_object_type_def() :: {input_object_type_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_usage()),
        list(input_field_def())}.

-type input_field_def() :: {input_field_def,
        binary(),
        gleam@option:option(binary()),
        s_d_l_type(),
        gleam@option:option(s_d_l_value()),
        list(directive_usage())}.

-type directive_def() :: {directive_def,
        binary(),
        gleam@option:option(binary()),
        list(directive_location()),
        list(argument_def())}.

-type directive_usage() :: {directive_usage,
        binary(),
        list(directive_argument())}.

-type directive_argument() :: {directive_argument, binary(), s_d_l_value()}.

-type directive_location() :: q_u_e_r_y |
    m_u_t_a_t_i_o_n |
    s_u_b_s_c_r_i_p_t_i_o_n |
    f_i_e_l_d |
    f_r_a_g_m_e_n_t_d_e_f_i_n_i_t_i_o_n |
    f_r_a_g_m_e_n_t_s_p_r_e_a_d |
    i_n_l_i_n_e_f_r_a_g_m_e_n_t |
    s_c_h_e_m_a |
    s_c_a_l_a_r |
    o_b_j_e_c_t |
    f_i_e_l_d_d_e_f_i_n_i_t_i_o_n |
    a_r_g_u_m_e_n_t_d_e_f_i_n_i_t_i_o_n |
    i_n_t_e_r_f_a_c_e |
    u_n_i_o_n |
    e_n_u_m |
    e_n_u_m_v_a_l_u_e |
    i_n_p_u_t_o_b_j_e_c_t |
    i_n_p_u_t_f_i_e_l_d_d_e_f_i_n_i_t_i_o_n.

-type schema_def() :: {schema_def,
        gleam@option:option(binary()),
        list(directive_usage()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-file("src/mochi/sdl_ast.gleam", 207).
?DOC(
    " Helper functions for working with SDL AST\n"
    " Get the name of a type definition\n"
).
-spec get_type_name(type_def()) -> binary().
get_type_name(Type_def) ->
    case Type_def of
        {object_type_definition, Obj} ->
            erlang:element(2, Obj);

        {interface_type_definition, Iface} ->
            erlang:element(2, Iface);

        {union_type_definition, Union} ->
            erlang:element(2, Union);

        {scalar_type_definition, Scalar} ->
            erlang:element(2, Scalar);

        {enum_type_definition, Enum} ->
            erlang:element(2, Enum);

        {input_object_type_definition, Input} ->
            erlang:element(2, Input)
    end.

-file("src/mochi/sdl_ast.gleam", 219).
?DOC(" Convert SDL type to string representation\n").
-spec sdl_type_to_string(s_d_l_type()) -> binary().
sdl_type_to_string(Sdl_type) ->
    case Sdl_type of
        {named_type, Name} ->
            Name;

        {list_type, Inner} ->
            <<<<"["/utf8, (sdl_type_to_string(Inner))/binary>>/binary,
                "]"/utf8>>;

        {non_null_type, Inner@1} ->
            <<(sdl_type_to_string(Inner@1))/binary, "!"/utf8>>
    end.

-file("src/mochi/sdl_ast.gleam", 228).
?DOC(" Check if a type is non-null\n").
-spec is_non_null_type(s_d_l_type()) -> boolean().
is_non_null_type(Sdl_type) ->
    case Sdl_type of
        {non_null_type, _} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/sdl_ast.gleam", 236).
?DOC(" Check if a type is a list\n").
-spec is_list_type(s_d_l_type()) -> boolean().
is_list_type(Sdl_type) ->
    case Sdl_type of
        {list_type, _} ->
            true;

        {non_null_type, {list_type, _}} ->
            true;

        _ ->
            false
    end.

-file("src/mochi/sdl_ast.gleam", 245).
?DOC(" Get the base type (unwrap NonNull and List wrappers)\n").
-spec get_base_type(s_d_l_type()) -> binary().
get_base_type(Sdl_type) ->
    case Sdl_type of
        {named_type, Name} ->
            Name;

        {list_type, Inner} ->
            get_base_type(Inner);

        {non_null_type, Inner@1} ->
            get_base_type(Inner@1)
    end.
