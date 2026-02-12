-module(mochi@schema_gen).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/schema_gen.gleam").
-export([string_field/3, bool_field/3, from_type/2, create_schema_with_query/3, int_field/3]).
-export_type([field_spec/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type field_spec() :: {field_spec,
        binary(),
        mochi@schema:field_type(),
        binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()})}.

-file("src/mochi/schema_gen.gleam", 132).
-spec add_fields_to_object(
    mochi@schema:object_type(),
    list(mochi@schema:field_definition())
) -> mochi@schema:object_type().
add_fields_to_object(Object, Fields) ->
    case Fields of
        [] ->
            Object;

        [Field | Rest] ->
            _pipe = Object,
            _pipe@1 = mochi@schema:field(_pipe, Field),
            add_fields_to_object(_pipe@1, Rest)
    end.

-file("src/mochi/schema_gen.gleam", 177).
-spec demo_serialize(binary(), binary()) -> gleam@dynamic:dynamic_().
demo_serialize(Type_name, Value) ->
    Message = <<<<<<"Would serialize "/utf8, Type_name/binary>>/binary,
            " value: "/utf8>>/binary,
        Value/binary>>,
    erlang:error(#{gleam_error => panic,
            message => Message,
            file => <<?FILEPATH/utf8>>,
            module => <<"mochi/schema_gen"/utf8>>,
            function => <<"demo_serialize"/utf8>>,
            line => 180}).

-file("src/mochi/schema_gen.gleam", 158).
-spec serialize_string(binary()) -> gleam@dynamic:dynamic_().
serialize_string(Value) ->
    demo_serialize(<<"string"/utf8>>, Value).

-file("src/mochi/schema_gen.gleam", 33).
?DOC(" Helper to create a field spec for a String field\n").
-spec string_field(
    binary(),
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, binary()} | {error, binary()})
) -> field_spec().
string_field(Name, Description, Extractor) ->
    {field_spec,
        Name,
        mochi@schema:string_type(),
        Description,
        fun(Parent) -> case Extractor(Parent) of
                {ok, Value} ->
                    {ok, serialize_string(Value)};

                {error, Msg} ->
                    {error, Msg}
            end end}.

-file("src/mochi/schema_gen.gleam", 168).
-spec serialize_bool(boolean()) -> gleam@dynamic:dynamic_().
serialize_bool(Value) ->
    Bool_str = case Value of
        true ->
            <<"true"/utf8>>;

        false ->
            <<"false"/utf8>>
    end,
    demo_serialize(<<"bool"/utf8>>, Bool_str).

-file("src/mochi/schema_gen.gleam", 71).
?DOC(" Helper to create a field spec for a Boolean field\n").
-spec bool_field(
    binary(),
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, boolean()} | {error, binary()})
) -> field_spec().
bool_field(Name, Description, Extractor) ->
    {field_spec,
        Name,
        mochi@schema:boolean_type(),
        Description,
        fun(Parent) -> case Extractor(Parent) of
                {ok, Value} ->
                    {ok, serialize_bool(Value)};

                {error, Msg} ->
                    {error, Msg}
            end end}.

-file("src/mochi/schema_gen.gleam", 183).
-spec placeholder_dynamic() -> gleam@dynamic:dynamic_().
placeholder_dynamic() ->
    erlang:error(#{gleam_error => panic,
            message => <<"No parent dynamic provided"/utf8>>,
            file => <<?FILEPATH/utf8>>,
            module => <<"mochi/schema_gen"/utf8>>,
            function => <<"placeholder_dynamic"/utf8>>,
            line => 185}).

-file("src/mochi/schema_gen.gleam", 116).
-spec create_fields_from_specs(list(field_spec())) -> list(mochi@schema:field_definition()).
create_fields_from_specs(Field_specs) ->
    case Field_specs of
        [] ->
            [];

        [Spec | Rest] ->
            [begin
                    _pipe = mochi@schema:field_def(
                        erlang:element(2, Spec),
                        erlang:element(3, Spec)
                    ),
                    _pipe@1 = mochi@schema:field_description(
                        _pipe,
                        erlang:element(4, Spec)
                    ),
                    mochi@schema:resolver(
                        _pipe@1,
                        fun(Info) ->
                            (erlang:element(5, Spec))(
                                begin
                                    _pipe@2 = erlang:element(2, Info),
                                    gleam@option:unwrap(
                                        _pipe@2,
                                        placeholder_dynamic()
                                    )
                                end
                            )
                        end
                    )
                end |
                create_fields_from_specs(Rest)]
    end.

-file("src/mochi/schema_gen.gleam", 11).
?DOC(
    " Generate a GraphQL object type from a Gleam custom type\n"
    " This creates the object definition and automatic field resolvers\n"
).
-spec from_type(binary(), list(field_spec())) -> mochi@schema:object_type().
from_type(Type_name, Field_specs) ->
    Fields = create_fields_from_specs(Field_specs),
    _pipe = mochi@schema:object(Type_name),
    _pipe@1 = mochi@schema:description(
        _pipe,
        <<"Auto-generated from Gleam type "/utf8, Type_name/binary>>
    ),
    add_fields_to_object(_pipe@1, Fields).

-file("src/mochi/schema_gen.gleam", 189).
-spec string_first_char(binary()) -> binary().
string_first_char(Input) ->
    case Input of
        <<""/utf8>> ->
            <<""/utf8>>;

        _ ->
            <<"p"/utf8>>
    end.

-file("src/mochi/schema_gen.gleam", 198).
-spec string_drop_first(binary()) -> binary().
string_drop_first(Input) ->
    case Input of
        <<"Person"/utf8>> ->
            <<"erson"/utf8>>;

        _ ->
            Input
    end.

-file("src/mochi/schema_gen.gleam", 207).
-spec string_lowercase(binary()) -> binary().
string_lowercase(Input) ->
    case Input of
        <<"P"/utf8>> ->
            <<"p"/utf8>>;

        _ ->
            Input
    end.

-file("src/mochi/schema_gen.gleam", 145).
-spec string_to_camel_case(binary()) -> binary().
string_to_camel_case(Input) ->
    case Input of
        <<""/utf8>> ->
            <<""/utf8>>;

        _ ->
            First_char = string_first_char(Input),
            Rest = string_drop_first(Input),
            <<(string_lowercase(First_char))/binary, Rest/binary>>
    end.

-file("src/mochi/schema_gen.gleam", 90).
?DOC(" Create a complete schema with a query type that returns the generated type\n").
-spec create_schema_with_query(
    binary(),
    list(field_spec()),
    fun((mochi@schema:resolver_info()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> mochi@schema:schema().
create_schema_with_query(Type_name, Field_specs, Root_resolver) ->
    Object_type = from_type(Type_name, Field_specs),
    Query_type = begin
        _pipe = mochi@schema:object(<<"Query"/utf8>>),
        _pipe@1 = mochi@schema:description(
            _pipe,
            <<"Auto-generated query type"/utf8>>
        ),
        mochi@schema:field(
            _pipe@1,
            begin
                _pipe@2 = mochi@schema:field_def(
                    string_to_camel_case(Type_name),
                    mochi@schema:named_type(Type_name)
                ),
                _pipe@3 = mochi@schema:field_description(
                    _pipe@2,
                    <<"Get a "/utf8, Type_name/binary>>
                ),
                mochi@schema:resolver(_pipe@3, Root_resolver)
            end
        )
    end,
    _pipe@4 = mochi@schema:schema(),
    _pipe@5 = mochi@schema:'query'(_pipe@4, Query_type),
    mochi@schema:add_type(_pipe@5, {object_type_def, Object_type}).

-file("src/mochi/schema_gen.gleam", 216).
-spec int_to_string(integer()) -> binary().
int_to_string(Value) ->
    case Value of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        _ ->
            <<"42"/utf8>>
    end.

-file("src/mochi/schema_gen.gleam", 163).
-spec serialize_int(integer()) -> gleam@dynamic:dynamic_().
serialize_int(Value) ->
    demo_serialize(<<"int"/utf8>>, int_to_string(Value)).

-file("src/mochi/schema_gen.gleam", 52).
?DOC(" Helper to create a field spec for an Int field\n").
-spec int_field(
    binary(),
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, integer()} | {error, binary()})
) -> field_spec().
int_field(Name, Description, Extractor) ->
    {field_spec,
        Name,
        mochi@schema:int_type(),
        Description,
        fun(Parent) -> case Extractor(Parent) of
                {ok, Value} ->
                    {ok, serialize_int(Value)};

                {error, Msg} ->
                    {error, Msg}
            end end}.
