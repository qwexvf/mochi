-module(rank).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/rank.gleam").
-export([suffix/1, ordinalise/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/rank.gleam", 14).
?DOC(
    " Returns the ordinal suffix for a number.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " suffix(1) // -> \"st\"\n"
    " suffix(2) // -> \"nd\"\n"
    " suffix(3) // -> \"rd\"\n"
    " suffix(4) // -> \"th\"\n"
    " ```\n"
).
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

-file("src/rank.gleam", 37).
?DOC(
    " Convert a number to its ordinal form.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " ordinalise(1) // -> \"1st\"\n"
    " ordinalise(2) // -> \"2nd\"\n"
    " ordinalise(3) // -> \"3rd\"\n"
    " ordinalise(4) // -> \"4th\"\n"
    " ```\n"
).
-spec ordinalise(integer()) -> binary().
ordinalise(Number) ->
    <<(erlang:integer_to_binary(Number))/binary, (suffix(Number))/binary>>.
