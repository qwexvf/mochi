-module(mochi@json).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mochi/json.gleam").
-export([encode/1, encode_pretty/2, encode_dict/1, encode_list/1, encode_string_value/1, encode_int/1, encode_float_value/1, encode_bool/1, encode_null/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/mochi/json.gleam", 35).
-spec dynamic_to_json(gleam@dynamic:dynamic_()) -> gleam@json:json().
dynamic_to_json(Value) ->
    case {gleam@dynamic@decode:run(
            Value,
            {decoder, fun gleam@dynamic@decode:decode_bool/1}
        ),
        gleam@dynamic@decode:run(
            Value,
            {decoder, fun gleam@dynamic@decode:decode_int/1}
        ),
        gleam@dynamic@decode:run(
            Value,
            {decoder, fun gleam@dynamic@decode:decode_float/1}
        ),
        gleam@dynamic@decode:run(
            Value,
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        gleam@dynamic@decode:run(
            Value,
            gleam@dynamic@decode:list(
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
            )
        ),
        gleam@dynamic@decode:run(
            Value,
            gleam@dynamic@decode:dict(
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
            )
        )} of
        {{ok, B}, _, _, _, _, _} ->
            gleam@json:bool(B);

        {_, {ok, I}, _, _, _, _} ->
            gleam@json:int(I);

        {_, _, {ok, F}, _, _, _} ->
            gleam@json:float(F);

        {_, _, _, {ok, S}, _, _} ->
            gleam@json:string(S);

        {_, _, _, _, {ok, Items}, _} ->
            gleam@json:array(Items, fun dynamic_to_json/1);

        {_, _, _, _, _, {ok, D}} ->
            _pipe = maps:to_list(D),
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(Kv) ->
                    {erlang:element(1, Kv),
                        dynamic_to_json(erlang:element(2, Kv))}
                end
            ),
            gleam@json:object(_pipe@1);

        {_, _, _, _, _, _} ->
            gleam@json:null()
    end.

-file("src/mochi/json.gleam", 18).
?DOC(" Encode a Dynamic value to a JSON string\n").
-spec encode(gleam@dynamic:dynamic_()) -> binary().
encode(Value) ->
    _pipe = dynamic_to_json(Value),
    gleam@json:to_string(_pipe).

-file("src/mochi/json.gleam", 155).
-spec spaces(integer(), integer()) -> binary().
spaces(Depth, Indent) ->
    gleam@string:repeat(<<" "/utf8>>, Depth * Indent).

-file("src/mochi/json.gleam", 65).
-spec do_format(binary(), integer(), integer(), binary(), boolean()) -> binary().
do_format(S, Depth, Indent, Acc, In_string) ->
    case gleam_stdlib:string_pop_grapheme(S) of
        {error, _} ->
            Acc;

        {ok, {C, Rest}} ->
            case In_string of
                true ->
                    case C of
                        <<"\""/utf8>> ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<Acc/binary, C/binary>>,
                                false
                            );

                        <<"\\"/utf8>> ->
                            case gleam_stdlib:string_pop_grapheme(Rest) of
                                {ok, {Next, Rest2}} ->
                                    do_format(
                                        Rest2,
                                        Depth,
                                        Indent,
                                        <<<<Acc/binary, C/binary>>/binary,
                                            Next/binary>>,
                                        true
                                    );

                                {error, _} ->
                                    <<Acc/binary, C/binary>>
                            end;

                        _ ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<Acc/binary, C/binary>>,
                                true
                            )
                    end;

                false ->
                    case C of
                        <<"\""/utf8>> ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<Acc/binary, C/binary>>,
                                true
                            );

                        <<"{"/utf8>> ->
                            case gleam@string:first(Rest) of
                                {ok, <<"}"/utf8>>} ->
                                    do_format(
                                        Rest,
                                        Depth,
                                        Indent,
                                        <<Acc/binary, "{"/utf8>>,
                                        false
                                    );

                                _ ->
                                    D = Depth + 1,
                                    do_format(
                                        Rest,
                                        D,
                                        Indent,
                                        <<<<Acc/binary, "{\n"/utf8>>/binary,
                                            (spaces(D, Indent))/binary>>,
                                        false
                                    )
                            end;

                        <<"}"/utf8>> ->
                            D@1 = Depth - 1,
                            do_format(
                                Rest,
                                D@1,
                                Indent,
                                <<<<<<Acc/binary, "\n"/utf8>>/binary,
                                        (spaces(D@1, Indent))/binary>>/binary,
                                    "}"/utf8>>,
                                false
                            );

                        <<"["/utf8>> ->
                            case gleam@string:first(Rest) of
                                {ok, <<"]"/utf8>>} ->
                                    do_format(
                                        Rest,
                                        Depth,
                                        Indent,
                                        <<Acc/binary, "["/utf8>>,
                                        false
                                    );

                                _ ->
                                    D@2 = Depth + 1,
                                    do_format(
                                        Rest,
                                        D@2,
                                        Indent,
                                        <<<<Acc/binary, "[\n"/utf8>>/binary,
                                            (spaces(D@2, Indent))/binary>>,
                                        false
                                    )
                            end;

                        <<"]"/utf8>> ->
                            D@3 = Depth - 1,
                            do_format(
                                Rest,
                                D@3,
                                Indent,
                                <<<<<<Acc/binary, "\n"/utf8>>/binary,
                                        (spaces(D@3, Indent))/binary>>/binary,
                                    "]"/utf8>>,
                                false
                            );

                        <<","/utf8>> ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<<<Acc/binary, ",\n"/utf8>>/binary,
                                    (spaces(Depth, Indent))/binary>>,
                                false
                            );

                        <<":"/utf8>> ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<Acc/binary, ": "/utf8>>,
                                false
                            );

                        <<" "/utf8>> ->
                            do_format(Rest, Depth, Indent, Acc, false);

                        <<"\t"/utf8>> ->
                            do_format(Rest, Depth, Indent, Acc, false);

                        <<"\n"/utf8>> ->
                            do_format(Rest, Depth, Indent, Acc, false);

                        <<"\r"/utf8>> ->
                            do_format(Rest, Depth, Indent, Acc, false);

                        _ ->
                            do_format(
                                Rest,
                                Depth,
                                Indent,
                                <<Acc/binary, C/binary>>,
                                false
                            )
                    end
            end
    end.

-file("src/mochi/json.gleam", 61).
-spec format_json(binary(), integer()) -> binary().
format_json(S, Indent) ->
    do_format(S, 0, Indent, <<""/utf8>>, false).

-file("src/mochi/json.gleam", 24).
?DOC(" Encode a Dynamic value to a pretty-printed JSON string\n").
-spec encode_pretty(gleam@dynamic:dynamic_(), integer()) -> binary().
encode_pretty(Value, Indent) ->
    Json_str = begin
        _pipe = dynamic_to_json(Value),
        gleam@json:to_string(_pipe)
    end,
    format_json(Json_str, Indent).

-file("src/mochi/json.gleam", 163).
-spec encode_dict(gleam@dict:dict(binary(), gleam@dynamic:dynamic_())) -> binary().
encode_dict(D) ->
    Entries = begin
        _pipe = maps:to_list(D),
        gleam@list:map(
            _pipe,
            fun(Kv) ->
                {erlang:element(1, Kv), dynamic_to_json(erlang:element(2, Kv))}
            end
        )
    end,
    _pipe@1 = gleam@json:object(Entries),
    gleam@json:to_string(_pipe@1).

-file("src/mochi/json.gleam", 170).
-spec encode_list(list(gleam@dynamic:dynamic_())) -> binary().
encode_list(Items) ->
    _pipe = gleam@json:array(Items, fun dynamic_to_json/1),
    gleam@json:to_string(_pipe).

-file("src/mochi/json.gleam", 174).
-spec encode_string_value(binary()) -> binary().
encode_string_value(S) ->
    _pipe = gleam@json:string(S),
    gleam@json:to_string(_pipe).

-file("src/mochi/json.gleam", 178).
-spec encode_int(integer()) -> binary().
encode_int(N) ->
    erlang:integer_to_binary(N).

-file("src/mochi/json.gleam", 182).
-spec encode_float_value(float()) -> binary().
encode_float_value(F) ->
    gleam_stdlib:float_to_string(F).

-file("src/mochi/json.gleam", 186).
-spec encode_bool(boolean()) -> binary().
encode_bool(B) ->
    case B of
        true ->
            <<"true"/utf8>>;

        false ->
            <<"false"/utf8>>
    end.

-file("src/mochi/json.gleam", 193).
-spec encode_null() -> binary().
encode_null() ->
    <<"null"/utf8>>.
