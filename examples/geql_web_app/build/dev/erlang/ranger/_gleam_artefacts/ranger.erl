-module(ranger).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/ranger.gleam").
-export([create/4, create_infinite/3]).
-export_type([direction/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type direction() :: forward | backward.

-file("src/ranger.gleam", 68).
?DOC(
    " returns a function that can be used to create a range\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > let range =\n"
    " >  create(\n"
    " >    validate: fn(a) { string.length(a) == 1 },\n"
    " >    negate_step: fn(s) { -1 * s },\n"
    " >    add: fn(a: String, b: Int) {\n"
    " >      let assert [code] = string.to_utf_codepoints(a)\n"
    " >      let int_code = string.utf_codepoint_to_int(code)\n"
    " >      let new_int_code = int_code + b\n"
    " >      let assert Ok(new_code) = string.utf_codepoint(new_int_code)\n"
    " >      string.from_utf_codepoints([new_code])\n"
    " >    },\n"
    " >    compare: string.compare,\n"
    " >  )\n"
    "\n"
    " > range(\"ab\", \"e\", 1)\n"
    " Error(Nil)\n"
    "\n"
    " > let assert Ok(a_to_e) = range(\"a\", \"e\", 1)\n"
    " > a_to_e |> yielder.to_list\n"
    " [\"a\", \"b\", \"c\", \"d\", \"e\"]\n"
    " \n"
    " > let assert Ok(z_to_p) = range(\"z\", \"p\", 1)\n"
    " > z_to_p |> yielder.to_list\n"
    " [\"z\", \"y\", \"x\", \"w\", \"v\", \"u\", \"t\", \"s\", \"r\", \"q\", \"p\"]\n"
    "\n"
    " > let assert Ok(z_to_p) = range(\"z\", \"p\", -2)\n"
    " > z_to_p |> yielder.to_list\n"
    " [\"z\", \"x\", \"v\", \"t\", \"r\", \"p\"]\n"
    "\n"
    " > let assert Ok(z_to_p) = range(\"z\", \"p\", 3)\n"
    " > z_to_p |> yielder.to_list\n"
    " [\"z\", \"w\", \"t\", \"q\"]\n"
    " ```\n"
    "\n"
    "\n"
    " ```gleam\n"
    " > let range =\n"
    " >    create(\n"
    " >      validate: fn(_) { True },\n"
    " >      negate_step: fn(s) { -1.0 *. s },\n"
    " >      add: fn(a, b) { a +. b },\n"
    " >      compare: float.compare,\n"
    " >    )\n"
    "\n"
    " > let assert Ok(weird_step_case) = range(1.25, 4.5, -0.5)\n"
    " > weird_step_case |> yielder.to_list\n"
    " [1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25]\n"
    " \n"
    " > let assert Ok(single_item_case) = range(1.25, 1.25, -0.25)\n"
    " > single_item_case |> yielder.to_list\n"
    " [1.25]\n"
    "\n"
    " > let assert Ok(zero_step_case) = range(2.5, 5.0, 0)\n"
    " > zero_step_case |> yielder.to_list\n"
    " [2.5]\n"
    " ```\n"
).
-spec create(
    fun((KWM) -> boolean()),
    fun((KWN) -> KWN),
    fun((KWM, KWN) -> KWM),
    fun((KWM, KWM) -> gleam@order:order())
) -> fun((KWM, KWM, KWN) -> {ok, gleam@yielder:yielder(KWM)} | {error, nil}).
create(Validate, Negate_step, Add, Compare) ->
    Adjust_step = fun(A, B, Step) ->
        Negated_step = Negate_step(Step),
        case {Compare(A, B),
            Compare(A, Add(A, Step)),
            Compare(A, Add(A, Negated_step))} of
            {eq, _, _} ->
                {ok, none};

            {_, eq, eq} ->
                {ok, none};

            {lt, lt, _} ->
                {ok, {some, {forward, Step}}};

            {lt, _, lt} ->
                {ok, {some, {forward, Negated_step}}};

            {lt, _, _} ->
                {error, nil};

            {gt, gt, _} ->
                {ok, {some, {backward, Step}}};

            {gt, _, gt} ->
                {ok, {some, {backward, Negated_step}}};

            {gt, _, _} ->
                {error, nil}
        end
    end,
    fun(A@1, B@1, S) ->
        gleam@bool:guard(
            not Validate(A@1) orelse not Validate(B@1),
            {error, nil},
            fun() -> case Adjust_step(A@1, B@1, S) of
                    {ok, {some, {Direction, Step@1}}} ->
                        {ok,
                            gleam@yielder:unfold(
                                A@1,
                                fun(Current) ->
                                    case {Compare(Current, B@1), Direction} of
                                        {gt, forward} ->
                                            done;

                                        {lt, backward} ->
                                            done;

                                        {_, _} ->
                                            {next,
                                                Current,
                                                Add(Current, Step@1)}
                                    end
                                end
                            )};

                    {ok, none} ->
                        {ok, gleam@yielder:once(fun() -> A@1 end)};

                    {error, nil} ->
                        {error, nil}
                end end
        )
    end.

-file("src/ranger.gleam", 144).
?DOC(
    " returns a function that can be used to create an infinite range\n"
    "\n"
    " should be used carefully because careless use of infinite yielders could crash your app\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > let range =\n"
    " >  create_infinite(\n"
    " >    validate: fn(a) { string.length(a) == 1 },\n"
    " >    add: fn(a: String, b: Int) {\n"
    " >      let assert [code] = string.to_utf_codepoints(a)\n"
    " >      let int_code = string.utf_codepoint_to_int(code)\n"
    " >      let new_int_code = int_code + b\n"
    " >      let assert Ok(new_code) = string.utf_codepoint(new_int_code)\n"
    " >      string.from_utf_codepoints([new_code])\n"
    " >    },\n"
    " >    compare: string.compare,\n"
    " >   )\n"
    "\n"
    " > let assert Ok(from_a) = range(\"a\", 1)\n"
    " > from_a |> yielder.take(26) |> yielder.to_list\n"
    " [\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\", \"k\", \"l\", \"m\", \"n\",\n"
    "   \"o\", \"p\", \"q\", \"r\", \"s\", \"t\", \"u\", \"v\", \"w\", \"x\", \"y\", \"z\"]\n"
    " ```\n"
).
-spec create_infinite(
    fun((KWR) -> boolean()),
    fun((KWR, KWS) -> KWR),
    fun((KWR, KWR) -> gleam@order:order())
) -> fun((KWR, KWS) -> {ok, gleam@yielder:yielder(KWR)} | {error, nil}).
create_infinite(Validate, Add, Compare) ->
    Is_step_zero = fun(A, S) -> case Compare(A, Add(A, S)) of
            eq ->
                true;

            _ ->
                false
        end end,
    fun(A@1, S@1) ->
        gleam@bool:guard(
            not Validate(A@1),
            {error, nil},
            fun() ->
                gleam@bool:guard(
                    Is_step_zero(A@1, S@1),
                    begin
                        _pipe = gleam@yielder:once(fun() -> A@1 end),
                        {ok, _pipe}
                    end,
                    fun() ->
                        _pipe@1 = gleam@yielder:unfold(
                            A@1,
                            fun(Current) ->
                                {next, Current, Add(Current, S@1)}
                            end
                        ),
                        {ok, _pipe@1}
                    end
                )
            end
        )
    end.
