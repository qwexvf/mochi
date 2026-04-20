-module(mochi@payload).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/payload.gleam").
-export([ok/1, error/1, message/1, message_for/2, with_code/2, validation_message_type/0, payload_type/2, payload_types/2, validation_message_to_dynamic/1, to_dynamic/2]).
-export_type([validation_message/0, mutation_payload/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A reference implementation of the structured mutation payload pattern.\n"
    "\n"
    " Inspired by [absinthe_error_payload](https://github.com/mirego/absinthe_error_payload)\n"
    " by Mirego.\n"
    "\n"
    " GraphQL has two ways to signal failure from a mutation:\n"
    "\n"
    " 1. **Top-level errors** — raised as `Error(msg)` from a resolver. These are\n"
    "    for unexpected failures (bugs, auth errors, infrastructure problems).\n"
    "\n"
    " 2. **Payload errors** — returned as DATA inside the response. These are for\n"
    "    predictable validation failures the client should handle (e.g. \"email\n"
    "    already taken\"). The client can pattern-match on `successful` and inspect\n"
    "    `messages` without treating them as exceptions.\n"
    "\n"
    " This module ships a ready-to-use `MutationPayload(result)` type. It is a\n"
    " **reference implementation** — copy or adapt it freely. Your own payload\n"
    " type can have a completely different shape (e.g. a union, an `errors`-only\n"
    " field, or domain-specific codes). The only requirement mochi has is that\n"
    " your resolver returns something your encoder can turn into a `Dynamic`.\n"
    "\n"
    " ## Quick start\n"
    "\n"
    " ```gleam\n"
    " import mochi/payload\n"
    " import mochi/query\n"
    " import mochi/schema\n"
    "\n"
    " fn create_user_resolver(name: String, _ctx) {\n"
    "   case validate_name(name) {\n"
    "     Ok(user) -> payload.ok(user)\n"
    "     Error(_) ->\n"
    "       payload.error([\n"
    "         payload.message_for(\"name\", \"has already been taken\")\n"
    "         |> payload.with_code(\"already_taken\"),\n"
    "       ])\n"
    "   }\n"
    " }\n"
    "\n"
    " let #(create_user_payload, vm_type) = payload.payload_types(\"CreateUser\", \"User\")\n"
    "\n"
    " query.new()\n"
    "   |> query.add_type(vm_type)              // shared — register once per schema\n"
    "   |> query.add_type(create_user_payload)  // one per mutation\n"
    "   |> query.add_mutation(\n"
    "     query.mutation(\n"
    "       name: \"createUser\",\n"
    "       args: [query.arg(\"name\", schema.non_null(schema.string_type()))],\n"
    "       returns: schema.named_type(\"CreateUserPayload\"),\n"
    "       decode: fn(args) { query.get_string(args, \"name\") },\n"
    "       resolve: fn(name, ctx) { Ok(create_user_resolver(name, ctx)) },\n"
    "       encode: fn(p) { payload.to_dynamic(p, user_to_dynamic) },\n"
    "     )\n"
    "   )\n"
    " ```\n"
    "\n"
    " ## Building a custom payload\n"
    "\n"
    " You are not required to use this module. Any type that can be encoded to\n"
    " `Dynamic` works as a mutation result. A minimal custom example:\n"
    "\n"
    " ```gleam\n"
    " pub type MyPayload(a) {\n"
    "   Success(data: a)\n"
    "   Failure(errors: List(String))\n"
    " }\n"
    "\n"
    " fn my_payload_to_dynamic(p: MyPayload(a), encode: fn(a) -> Dynamic) -> Dynamic {\n"
    "   case p {\n"
    "     Success(data) ->\n"
    "       types.record([\n"
    "         types.field(\"ok\", True),\n"
    "         types.field(\"data\", encode(data)),\n"
    "       ])\n"
    "     Failure(errors) ->\n"
    "       types.record([\n"
    "         types.field(\"ok\", False),\n"
    "         types.field(\"errors\", types.to_dynamic(errors)),\n"
    "       ])\n"
    "   }\n"
    " }\n"
    " ```\n"
    "\n"
    " Register the matching GraphQL type with `schema.object(...)` and\n"
    " `query.add_type(...)` the same way you would any other type.\n"
).

-type validation_message() :: {validation_message,
        gleam@option:option(binary()),
        binary(),
        gleam@option:option(binary())}.

-type mutation_payload(PXS) :: {mutation_payload,
        boolean(),
        gleam@option:option(PXS),
        list(validation_message())}.

-file("src/mochi/payload.gleam", 120).
?DOC(" Build a successful payload wrapping the given result.\n").
-spec ok(PXT) -> mutation_payload(PXT).
ok(Result) ->
    {mutation_payload, true, {some, Result}, []}.

-file("src/mochi/payload.gleam", 125).
?DOC(" Build a failed payload with validation messages and no result.\n").
-spec error(list(validation_message())) -> mutation_payload(any()).
error(Messages) ->
    {mutation_payload, false, none, Messages}.

-file("src/mochi/payload.gleam", 130).
?DOC(" Create a top-level (non-field-specific) validation message.\n").
-spec message(binary()) -> validation_message().
message(Msg) ->
    {validation_message, none, Msg, none}.

-file("src/mochi/payload.gleam", 135).
?DOC(" Create a field-specific validation message.\n").
-spec message_for(binary(), binary()) -> validation_message().
message_for(Field, Msg) ->
    {validation_message, {some, Field}, Msg, none}.

-file("src/mochi/payload.gleam", 140).
?DOC(" Attach a machine-readable error code to a validation message.\n").
-spec with_code(validation_message(), binary()) -> validation_message().
with_code(Vm, Code) ->
    {validation_message,
        erlang:element(2, Vm),
        erlang:element(3, Vm),
        {some, Code}}.

-file("src/mochi/payload.gleam", 158).
?DOC(
    " The shared `ValidationMessage` GraphQL object type.\n"
    "\n"
    " Register this once per schema:\n"
    " ```gleam\n"
    " query.new()\n"
    "   |> query.add_type(payload.validation_message_type())\n"
    " ```\n"
).
-spec validation_message_type() -> mochi@schema:object_type().
validation_message_type() ->
    _pipe = mochi@schema:object(<<"ValidationMessage"/utf8>>),
    _pipe@1 = mochi@schema:description(
        _pipe,
        <<"A validation error associated with a mutation field"/utf8>>
    ),
    _pipe@3 = mochi@schema:field(
        _pipe@1,
        begin
            _pipe@2 = mochi@schema:field_def(
                <<"field"/utf8>>,
                {named, <<"String"/utf8>>}
            ),
            mochi@schema:field_description(
                _pipe@2,
                <<"The input field that triggered the error"/utf8>>
            )
        end
    ),
    _pipe@5 = mochi@schema:field(
        _pipe@3,
        begin
            _pipe@4 = mochi@schema:field_def(
                <<"message"/utf8>>,
                {non_null, {named, <<"String"/utf8>>}}
            ),
            mochi@schema:field_description(
                _pipe@4,
                <<"A human-readable description of the error"/utf8>>
            )
        end
    ),
    mochi@schema:field(
        _pipe@5,
        begin
            _pipe@6 = mochi@schema:field_def(
                <<"code"/utf8>>,
                {named, <<"String"/utf8>>}
            ),
            mochi@schema:field_description(
                _pipe@6,
                <<"A machine-readable error code"/utf8>>
            )
        end
    ).

-file("src/mochi/payload.gleam", 184).
?DOC(
    " Create a payload type for a specific mutation result type.\n"
    "\n"
    " `name` is used to build `<name>Payload` (e.g. `\"CreateUser\"` → `\"CreateUserPayload\"`).\n"
    " `result_type_name` is the GraphQL type name of the success result.\n"
    "\n"
    " ```gleam\n"
    " let user_payload = payload.payload_type(\"CreateUser\", \"User\")\n"
    " // Produces CreateUserPayload { successful, messages, result }\n"
    " ```\n"
).
-spec payload_type(binary(), binary()) -> mochi@schema:object_type().
payload_type(Name, Result_type_name) ->
    _pipe = mochi@schema:object(<<Name/binary, "Payload"/utf8>>),
    _pipe@1 = mochi@schema:description(
        _pipe,
        <<<<"The result of the "/utf8, Name/binary>>/binary, " mutation"/utf8>>
    ),
    _pipe@3 = mochi@schema:field(
        _pipe@1,
        begin
            _pipe@2 = mochi@schema:field_def(
                <<"successful"/utf8>>,
                {non_null, {named, <<"Boolean"/utf8>>}}
            ),
            mochi@schema:field_description(
                _pipe@2,
                <<"Whether the mutation succeeded"/utf8>>
            )
        end
    ),
    _pipe@5 = mochi@schema:field(
        _pipe@3,
        begin
            _pipe@4 = mochi@schema:field_def(
                <<"messages"/utf8>>,
                {non_null, {list, {named, <<"ValidationMessage"/utf8>>}}}
            ),
            mochi@schema:field_description(
                _pipe@4,
                <<"Validation errors, empty on success"/utf8>>
            )
        end
    ),
    mochi@schema:field(
        _pipe@5,
        begin
            _pipe@6 = mochi@schema:field_def(
                <<"result"/utf8>>,
                {named, Result_type_name}
            ),
            mochi@schema:field_description(
                _pipe@6,
                <<"The mutation result, present only on success"/utf8>>
            )
        end
    ).

-file("src/mochi/payload.gleam", 217).
?DOC(
    " Create both the payload type and the shared ValidationMessage type at once.\n"
    "\n"
    " Returns `#(payload_type, validation_message_type)`.\n"
    " Register `validation_message_type` once per schema even if you have\n"
    " multiple mutations.\n"
    "\n"
    " ```gleam\n"
    " let #(create_user_payload, vm_type) = payload.payload_types(\"CreateUser\", \"User\")\n"
    "\n"
    " query.new()\n"
    "   |> query.add_type(vm_type)\n"
    "   |> query.add_type(create_user_payload)\n"
    " ```\n"
).
-spec payload_types(binary(), binary()) -> {mochi@schema:object_type(),
    mochi@schema:object_type()}.
payload_types(Name, Result_type_name) ->
    {payload_type(Name, Result_type_name), validation_message_type()}.

-file("src/mochi/payload.gleam", 229).
?DOC(" Encode a `ValidationMessage` to Dynamic for the GraphQL response.\n").
-spec validation_message_to_dynamic(validation_message()) -> gleam@dynamic:dynamic_().
validation_message_to_dynamic(Vm) ->
    gleam_stdlib:identity(
        maps:from_list([{<<"field"/utf8>>, case erlang:element(2, Vm) of
                        {some, F} ->
                            gleam_stdlib:identity(F);

                        none ->
                            gleam_stdlib:identity(nil)
                    end}, {<<"message"/utf8>>,
                    gleam_stdlib:identity(erlang:element(3, Vm))}, {<<"code"/utf8>>,
                    case erlang:element(4, Vm) of
                        {some, C} ->
                            gleam_stdlib:identity(C);

                        none ->
                            gleam_stdlib:identity(nil)
                    end}])
    ).

-file("src/mochi/payload.gleam", 251).
?DOC(
    " Encode a `MutationPayload` to Dynamic for the GraphQL response.\n"
    "\n"
    " Pass the encoder for the inner result type:\n"
    " ```gleam\n"
    " payload.to_dynamic(p, user_to_dynamic)\n"
    " ```\n"
).
-spec to_dynamic(mutation_payload(PXZ), fun((PXZ) -> gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
to_dynamic(Payload, Encode_result) ->
    gleam_stdlib:identity(
        maps:from_list(
            [{<<"successful"/utf8>>,
                    gleam_stdlib:identity(erlang:element(2, Payload))},
                {<<"messages"/utf8>>,
                    gleam_stdlib:identity(
                        gleam@list:map(
                            erlang:element(4, Payload),
                            fun validation_message_to_dynamic/1
                        )
                    )},
                {<<"result"/utf8>>, case erlang:element(3, Payload) of
                        {some, R} ->
                            Encode_result(R);

                        none ->
                            gleam_stdlib:identity(nil)
                    end}]
        )
    ).
