-module(mochi@json).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/json.gleam").
-export([encode_string_value/1, encode_int/1, encode_float_value/1, encode_bool/1, encode_null/0, encode_pretty/2, encode/1, encode_dict/1, encode_list/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi/json.gleam", 163).
-spec escape_char(binary()) -> binary().
escape_char(C) ->
    case C of
        <<"\""/utf8>> ->
            <<"\\\""/utf8>>;

        <<"\\"/utf8>> ->
            <<"\\\\"/utf8>>;

        <<"\n"/utf8>> ->
            <<"\\n"/utf8>>;

        <<"\r"/utf8>> ->
            <<"\\r"/utf8>>;

        <<"\t"/utf8>> ->
            <<"\\t"/utf8>>;

        _ ->
            C
    end.

-file("src/mochi/json.gleam", 156).
-spec escape_string(binary()) -> binary().
escape_string(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun escape_char/1),
    erlang:list_to_binary(_pipe@2).

-file("src/mochi/json.gleam", 152).
-spec encode_string(binary()) -> binary().
encode_string(S) ->
    <<<<"\""/utf8, (escape_string(S))/binary>>/binary, "\""/utf8>>.

-file("src/mochi/json.gleam", 288).
-spec make_indent(integer(), integer()) -> binary().
make_indent(Depth, Indent) ->
    gleam@string:repeat(<<" "/utf8>>, Depth * Indent).

-file("src/mochi/json.gleam", 307).
?DOC(" Encode a string value\n").
-spec encode_string_value(binary()) -> binary().
encode_string_value(S) ->
    encode_string(S).

-file("src/mochi/json.gleam", 312).
?DOC(" Encode an int value\n").
-spec encode_int(integer()) -> binary().
encode_int(N) ->
    erlang:integer_to_binary(N).

-file("src/mochi/json.gleam", 317).
?DOC(" Encode a float value\n").
-spec encode_float_value(float()) -> binary().
encode_float_value(F) ->
    gleam_stdlib:float_to_string(F).

-file("src/mochi/json.gleam", 322).
?DOC(" Encode a bool value\n").
-spec encode_bool(boolean()) -> binary().
encode_bool(B) ->
    case B of
        true ->
            <<"true"/utf8>>;

        false ->
            <<"false"/utf8>>
    end.

-file("src/mochi/json.gleam", 330).
?DOC(" Encode null\n").
-spec encode_null() -> binary().
encode_null() ->
    <<"null"/utf8>>.

-file("src/mochi/json.gleam", 246).
-spec encode_array_pretty(list(gleam@dynamic:dynamic_()), integer(), integer()) -> binary().
encode_array_pretty(Items, Depth, Indent) ->
    case Items of
        [] ->
            <<"[]"/utf8>>;

        _ ->
            Inner_indent = make_indent(Depth + 1, Indent),
            Outer_indent = make_indent(Depth, Indent),
            Encoded = begin
                _pipe = Items,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(Item) ->
                        <<Inner_indent/binary,
                            (encode_value_pretty(Item, Depth + 1, Indent))/binary>>
                    end
                ),
                gleam@string:join(_pipe@1, <<",\n"/utf8>>)
            end,
            <<<<<<<<"[\n"/utf8, Encoded/binary>>/binary, "\n"/utf8>>/binary,
                    Outer_indent/binary>>/binary,
                "]"/utf8>>
    end.

-file("src/mochi/json.gleam", 197).
-spec encode_value_pretty(gleam@dynamic:dynamic_(), integer(), integer()) -> binary().
encode_value_pretty(Value, Depth, Indent) ->
    case mochi_json_ffi:is_null(Value) of
        true ->
            <<"null"/utf8>>;

        false ->
            case mochi_json_ffi:is_option(Value) of
                true ->
                    encode_value_pretty(
                        mochi_json_ffi:unwrap_option(Value),
                        Depth,
                        Indent
                    );

                false ->
                    case mochi_json_ffi:is_bool(Value) of
                        true ->
                            case mochi_json_ffi:get_bool(Value) of
                                true ->
                                    <<"true"/utf8>>;

                                false ->
                                    <<"false"/utf8>>
                            end;

                        false ->
                            case mochi_json_ffi:is_int(Value) of
                                true ->
                                    erlang:integer_to_binary(
                                        mochi_json_ffi:get_int(Value)
                                    );

                                false ->
                                    case mochi_json_ffi:is_float(Value) of
                                        true ->
                                            gleam_stdlib:float_to_string(
                                                mochi_json_ffi:get_float(Value)
                                            );

                                        false ->
                                            case mochi_json_ffi:is_string(Value) of
                                                true ->
                                                    encode_string(
                                                        mochi_json_ffi:get_string(
                                                            Value
                                                        )
                                                    );

                                                false ->
                                                    case mochi_json_ffi:is_list(
                                                        Value
                                                    ) of
                                                        true ->
                                                            encode_array_pretty(
                                                                mochi_json_ffi:get_list(
                                                                    Value
                                                                ),
                                                                Depth,
                                                                Indent
                                                            );

                                                        false ->
                                                            case mochi_json_ffi:is_dict(
                                                                Value
                                                            ) of
                                                                true ->
                                                                    encode_object_pretty(
                                                                        mochi_json_ffi:get_dict_entries(
                                                                            Value
                                                                        ),
                                                                        Depth,
                                                                        Indent
                                                                    );

                                                                false ->
                                                                    <<"null"/utf8>>
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/mochi/json.gleam", 26).
?DOC(" Encode a Dynamic value to a pretty-printed JSON string\n").
-spec encode_pretty(gleam@dynamic:dynamic_(), integer()) -> binary().
encode_pretty(Value, Indent) ->
    encode_value_pretty(Value, 0, Indent).

-file("src/mochi/json.gleam", 263).
-spec encode_object_pretty(
    list({binary(), gleam@dynamic:dynamic_()}),
    integer(),
    integer()
) -> binary().
encode_object_pretty(Entries, Depth, Indent) ->
    case Entries of
        [] ->
            <<"{}"/utf8>>;

        _ ->
            Inner_indent = make_indent(Depth + 1, Indent),
            Outer_indent = make_indent(Depth, Indent),
            Encoded = begin
                _pipe = Entries,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(Entry) ->
                        {Key, Val} = Entry,
                        <<<<<<Inner_indent/binary, (encode_string(Key))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (encode_value_pretty(Val, Depth + 1, Indent))/binary>>
                    end
                ),
                gleam@string:join(_pipe@1, <<",\n"/utf8>>)
            end,
            <<<<<<<<"{\n"/utf8, Encoded/binary>>/binary, "\n"/utf8>>/binary,
                    Outer_indent/binary>>/binary,
                "}"/utf8>>
    end.

-file("src/mochi/json.gleam", 174).
-spec encode_array(list(gleam@dynamic:dynamic_())) -> binary().
encode_array(Items) ->
    Encoded = begin
        _pipe = Items,
        _pipe@1 = gleam@list:map(_pipe, fun encode_value/1),
        gleam@string:join(_pipe@1, <<","/utf8>>)
    end,
    <<<<"["/utf8, Encoded/binary>>/binary, "]"/utf8>>.

-file("src/mochi/json.gleam", 113).
-spec encode_value(gleam@dynamic:dynamic_()) -> binary().
encode_value(Value) ->
    case mochi_json_ffi:is_null(Value) of
        true ->
            <<"null"/utf8>>;

        false ->
            case mochi_json_ffi:is_option(Value) of
                true ->
                    encode_value(mochi_json_ffi:unwrap_option(Value));

                false ->
                    case mochi_json_ffi:is_bool(Value) of
                        true ->
                            case mochi_json_ffi:get_bool(Value) of
                                true ->
                                    <<"true"/utf8>>;

                                false ->
                                    <<"false"/utf8>>
                            end;

                        false ->
                            case mochi_json_ffi:is_int(Value) of
                                true ->
                                    erlang:integer_to_binary(
                                        mochi_json_ffi:get_int(Value)
                                    );

                                false ->
                                    case mochi_json_ffi:is_float(Value) of
                                        true ->
                                            gleam_stdlib:float_to_string(
                                                mochi_json_ffi:get_float(Value)
                                            );

                                        false ->
                                            case mochi_json_ffi:is_string(Value) of
                                                true ->
                                                    encode_string(
                                                        mochi_json_ffi:get_string(
                                                            Value
                                                        )
                                                    );

                                                false ->
                                                    case mochi_json_ffi:is_list(
                                                        Value
                                                    ) of
                                                        true ->
                                                            encode_array(
                                                                mochi_json_ffi:get_list(
                                                                    Value
                                                                )
                                                            );

                                                        false ->
                                                            case mochi_json_ffi:is_dict(
                                                                Value
                                                            ) of
                                                                true ->
                                                                    encode_object(
                                                                        mochi_json_ffi:get_dict_entries(
                                                                            Value
                                                                        )
                                                                    );

                                                                false ->
                                                                    <<"null"/utf8>>
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/mochi/json.gleam", 21).
?DOC(" Encode a Dynamic value to a JSON string\n").
-spec encode(gleam@dynamic:dynamic_()) -> binary().
encode(Value) ->
    encode_value(Value).

-file("src/mochi/json.gleam", 182).
-spec encode_object(list({binary(), gleam@dynamic:dynamic_()})) -> binary().
encode_object(Entries) ->
    Encoded = begin
        _pipe = Entries,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Entry) ->
                {Key, Val} = Entry,
                <<<<(encode_string(Key))/binary, ":"/utf8>>/binary,
                    (encode_value(Val))/binary>>
            end
        ),
        gleam@string:join(_pipe@1, <<","/utf8>>)
    end,
    <<<<"{"/utf8, Encoded/binary>>/binary, "}"/utf8>>.

-file("src/mochi/json.gleam", 297).
?DOC(" Encode a Dict directly to JSON\n").
-spec encode_dict(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> binary().
encode_dict(D) ->
    encode_object(maps:to_list(D)).

-file("src/mochi/json.gleam", 302).
?DOC(" Encode a List directly to JSON\n").
-spec encode_list(list(gleam@dynamic:dynamic_())) -> binary().
encode_list(Items) ->
    encode_array(Items).
