-module(rank).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([suffix/1, ordinalise/1]).

-spec suffix(integer()) -> binary().
suffix(Number) ->
    Number@1 = gleam@int:absolute_value(Number),
    Hundred = Number@1 rem 100,
    case Number@1 rem 10 of
        _ when ((Hundred =:= 11) orelse (Hundred =:= 12)) orelse (Hundred =:= 13) ->
            <<"th"/utf8>>;

        1 ->
            <<"st"/utf8>>;

        2 ->
            <<"nd"/utf8>>;

        3 ->
            <<"rd"/utf8>>;

        _ ->
            <<"th"/utf8>>
    end.

-spec ordinalise(integer()) -> binary().
ordinalise(Number) ->
    <<(gleam@int:to_string(Number))/binary, (suffix(Number))/binary>>.
