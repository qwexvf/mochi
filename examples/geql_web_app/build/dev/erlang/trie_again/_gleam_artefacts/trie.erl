-module(trie).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/trie.gleam").
-export([fold/3, get/2, has_path/2, map/2, new/0, delete/2, insert/3, from_list/1, paths/1, singleton/2, size/1, is_empty/1, subtrie/2, to_list/1, update/3, values/1]).
-export_type([trie/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque trie(HGB, HGC) :: {trie,
        gleam@option:option(HGC),
        gleam@dict:dict(HGB, trie(HGB, HGC))}.

-file("src/trie.gleam", 43).
?DOC(
    " Exactly same behaviour as delete but returns `None` if the tree is empty as a\n"
    " result of the deletion.\n"
).
-spec do_delete(trie(HGK, HGL), list(HGK)) -> gleam@option:option(trie(HGK, HGL)).
do_delete(Trie, Path) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            case maps:size(Children_map) of
                0 ->
                    none;

                _ ->
                    {some, {trie, none, Children_map}}
            end;

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            New_children = case gleam_stdlib:map_get(Children_map@1, First) of
                {error, _} ->
                    Children_map@1;

                {ok, Child} ->
                    case do_delete(Child, Rest) of
                        none ->
                            gleam@dict:delete(Children_map@1, First);

                        {some, Trie@1} ->
                            gleam@dict:insert(Children_map@1, First, Trie@1)
                    end
            end,
            case {Entry, maps:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry, New_children}}
            end
    end.

-file("src/trie.gleam", 80).
?DOC(
    " Combines all the trie's values into a single one by calling a given function on each one.\n"
    "\n"
    " The function takes as input the accumulator, the path of a value and the corresponding value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], 10), #([1], 1)]\n"
    " > |> from_list\n"
    " > |> fold(from: 0, with: fn(sum, _, value) { sum + value })\n"
    " 11\n"
    " ```\n"
).
-spec fold(trie(HGS, HGT), HGW, fun((HGW, list(HGS), HGT) -> HGW)) -> HGW.
fold(Trie, Initial, Fun) ->
    gleam@dict:fold(
        erlang:element(3, Trie),
        begin
            _pipe = erlang:element(2, Trie),
            _pipe@1 = gleam@option:map(
                _pipe,
                fun(_capture) -> Fun(Initial, [], _capture) end
            ),
            gleam@option:unwrap(_pipe@1, Initial)
        end,
        fun(Acc, First, Trie@1) ->
            fold(
                Trie@1,
                Acc,
                fun(Acc@1, Rest, Value) -> Fun(Acc@1, [First | Rest], Value) end
            )
        end
    ).

-file("src/trie.gleam", 133).
?DOC(
    " Fetches a value from a trie for a given path.\n"
    " If a value is present at the given path it returns it wrapped in an `Ok`,\n"
    " otherwise it returns `Error(Nil)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> get(at: [1, 2])\n"
    " Result(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> get(at: [1, 2])\n"
    " Ok(\"a\")\n"
    " ```\n"
).
-spec get(trie(HHE, HHF), list(HHE)) -> {ok, HHF} | {error, nil}.
get(From, Path) ->
    case {Path, From} of
        {[], {trie, none, _}} ->
            {error, nil};

        {[], {trie, {some, Value}, _}} ->
            {ok, Value};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam_stdlib:map_get(_pipe, First),
            gleam@result:'try'(
                _pipe@1,
                fun(_capture) -> get(_capture, Rest) end
            )
    end.

-file("src/trie.gleam", 160).
?DOC(
    " Determines wether a trie contains a value associated with the given path.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> has_path([1, 2])\n"
    " True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> has_path([1])\n"
    " False\n"
    " ```\n"
).
-spec has_path(trie(HHL, any()), list(HHL)) -> boolean().
has_path(Trie, Path) ->
    case get(Trie, Path) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-file("src/trie.gleam", 237).
?DOC(
    " Updates all the values in a given trie by calling a function on each value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> map(fn(s) { s <> \"!\" })\n"
    " > |> to_list\n"
    " [#([1, 2], \"a!\"), #([1], \"b!\")]\n"
    " ```\n"
).
-spec map(trie(HIB, HIC), fun((HIC) -> HIF)) -> trie(HIB, HIF).
map(Trie, Fun) ->
    {trie,
        gleam@option:map(erlang:element(2, Trie), Fun),
        gleam@dict:map_values(
            erlang:element(3, Trie),
            fun(_, T) -> map(T, Fun) end
        )}.

-file("src/trie.gleam", 254).
?DOC(
    " Creates a new empty trie.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> to_list\n"
    " []\n"
    " ```\n"
).
-spec new() -> trie(any(), any()).
new() ->
    {trie, none, maps:new()}.

-file("src/trie.gleam", 35).
?DOC(
    " Deletes from a trie the value associated with a given path.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> delete(at: [1, 2])\n"
    " > |> to_list\n"
    " [#([1], \"b\")]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> delete(at: [1, 2])\n"
    " > |> to_list\n"
    " []\n"
    " ```\n"
).
-spec delete(trie(HGD, HGE), list(HGD)) -> trie(HGD, HGE).
delete(Trie, Path) ->
    _pipe = do_delete(Trie, Path),
    gleam@option:unwrap(_pipe, new()).

-file("src/trie.gleam", 188).
?DOC(
    " Inserts a value in a trie at a given path. If there already is a value\n"
    " at the given path it is replaced by the new one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> insert(at: [1, 2], value: \"a\")\n"
    " > |> insert(at: [1], value: \"b\")\n"
    " > |> to_list\n"
    " [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> insert(at: [1, 2], value: \"a\")\n"
    " > |> insert(at: [1, 2], value: \"b\")\n"
    " > |> to_list\n"
    " [#([1, 2], \"b\")]\n"
    " ```\n"
).
-spec insert(trie(HHQ, HHR), list(HHQ), HHR) -> trie(HHQ, HHR).
insert(Trie, Path, Value) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            {trie, {some, Value}, Children_map};

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            _pipe = gleam_stdlib:map_get(Children_map@1, First),
            _pipe@1 = gleam@result:unwrap(_pipe, new()),
            _pipe@2 = insert(_pipe@1, Rest, Value),
            _pipe@3 = gleam@dict:insert(Children_map@1, First, _pipe@2),
            {trie, Entry, _pipe@3}
    end.

-file("src/trie.gleam", 109).
?DOC(
    " Creates a new trie from a list of path-value pairs.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> to_list\n"
    " [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " ```\n"
).
-spec from_list(list({list(HGY), HHA})) -> trie(HGY, HHA).
from_list(List) ->
    gleam@list:fold(
        List,
        new(),
        fun(Trie, Pair) ->
            insert(Trie, erlang:element(1, Pair), erlang:element(2, Pair))
        end
    ).

-file("src/trie.gleam", 278).
?DOC(
    " Gets a list of all the valid paths in the trie. That is all the paths associated with a value.\n"
    "\n"
    " Tries are not ordered so the paths are not returned in any specific order.\n"
    " Do not write code that relies on the order paths are returned by this function\n"
    " as it may change in later versions of the library.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> paths\n"
    " [[1, 2], [1]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> paths\n"
    " []\n"
    " ```\n"
).
-spec paths(trie(HIM, any())) -> list(list(HIM)).
paths(Trie) ->
    fold(Trie, [], fun(Rest, Path, _) -> [Path | Rest] end).

-file("src/trie.gleam", 292).
?DOC(
    " Creates a new trie with a single value associated to the given path.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> to_list\n"
    " [#([1, 2], \"a\")]\n"
    " ```\n"
).
-spec singleton(list(HIS), HIU) -> trie(HIS, HIU).
singleton(Path, Value) ->
    insert(new(), Path, Value).

-file("src/trie.gleam", 307).
?DOC(
    " Gets the number of elements in the trie.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> size\n"
    " 2\n"
    " ```\n"
).
-spec size(trie(any(), any())) -> integer().
size(Trie) ->
    fold(Trie, 0, fun(Acc, _, _) -> Acc + 1 end).

-file("src/trie.gleam", 221).
?DOC(
    " Determines wether or not the trie is empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> is_empty\n"
    " True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> is_empty\n"
    " False\n"
    " ```\n"
).
-spec is_empty(trie(any(), any())) -> boolean().
is_empty(Trie) ->
    size(Trie) =:= 0.

-file("src/trie.gleam", 323).
?DOC(
    " Gets the subtrie whose elements all share a common given prefix.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2, 3], \"a\"), #([1, 2, 4, 5], \"b\"), #([3, 4], \"c\")]\n"
    " > |> from_list\n"
    " > |> subtrie(at: [1, 2])\n"
    " > |> to_list\n"
    " [#([1, 2, 3], \"a\"), #([1, 2, 4, 5], \"b\")]\n"
    " ```\n"
).
-spec subtrie(trie(HJB, HJC), list(HJB)) -> {ok, trie(HJB, HJC)} | {error, nil}.
subtrie(Trie, Prefix) ->
    case {Prefix, Trie} of
        {[], _} ->
            {ok, Trie};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam_stdlib:map_get(_pipe, First),
            _pipe@2 = gleam@result:'try'(
                _pipe@1,
                fun(_capture) -> subtrie(_capture, Rest) end
            ),
            gleam@result:map(_pipe@2, fun(Subtrie) -> _pipe@3 = maps:new(),
                    _pipe@4 = gleam@dict:insert(_pipe@3, First, Subtrie),
                    {trie, none, _pipe@4} end)
    end.

-file("src/trie.gleam", 354).
?DOC(
    " Turns a trie into a list of path-value pairs.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> to_list\n"
    " [#([1, 2], \"a\")]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> to_list\n"
    " []\n"
    " ```\n"
).
-spec to_list(trie(HJK, HJL)) -> list({list(HJK), HJL}).
to_list(Trie) ->
    fold(Trie, [], fun(Rest, Path, Value) -> [{Path, Value} | Rest] end).

-file("src/trie.gleam", 401).
?DOC(
    " Exactly same behaviour as update but returns `None` if the tree is empty as a\n"
    " result of the (possible) deletion.\n"
).
-spec do_update(
    trie(HJZ, HKA),
    list(HJZ),
    fun((gleam@option:option(HKA)) -> gleam@option:option(HKA))
) -> gleam@option:option(trie(HJZ, HKA)).
do_update(Trie, Path, Fun) ->
    case {Path, Trie} of
        {[], {trie, Entry, Children_map}} ->
            case {Fun(Entry), maps:size(Children_map)} of
                {none, 0} ->
                    none;

                {_ = New_entry, _} ->
                    {some, {trie, New_entry, Children_map}}
            end;

        {[First | Rest], {trie, Entry@1, Children_map@1}} ->
            New_children = case gleam_stdlib:map_get(Children_map@1, First) of
                {ok, Child} ->
                    case do_update(Child, Rest, Fun) of
                        none ->
                            gleam@dict:delete(Children_map@1, First);

                        {some, New_child} ->
                            gleam@dict:insert(Children_map@1, First, New_child)
                    end;

                {error, _} ->
                    case Fun(none) of
                        none ->
                            Children_map@1;

                        {some, Value} ->
                            gleam@dict:insert(
                                Children_map@1,
                                First,
                                singleton(Rest, Value)
                            )
                    end
            end,
            case {Entry@1, maps:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry@1, New_children}}
            end
    end.

-file("src/trie.gleam", 389).
?DOC(
    " Updates the value associated with a path applying it the given function.\n"
    " If there is no value associated with the given path the function is passed `None`.\n"
    "\n"
    " If the function returns `None` any value associated with the path is deleted from the trie.\n"
    " If the function returns `Some(value)` then the new value is associated to the given path.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> update(at: [1, 2], with: fn(n) { n |> option.map(fn(_) { \"b\" }) })\n"
    " > |> to_list\n"
    " [#([1, 2], \"b\")]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> update(at: [1, 2], with: fn(_) { None })\n"
    " > |> to_list\n"
    " []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > singleton([1, 2], \"a\")\n"
    " > |> update(at: [1], with: fn(_) { Some(\"b\") })\n"
    " > |> to_list\n"
    " [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " ```\n"
).
-spec update(
    trie(HJQ, HJR),
    list(HJQ),
    fun((gleam@option:option(HJR)) -> gleam@option:option(HJR))
) -> trie(HJQ, HJR).
update(Trie, Path, Fun) ->
    _pipe = do_update(Trie, Path, Fun),
    gleam@option:unwrap(_pipe, new()).

-file("src/trie.gleam", 458).
?DOC(
    " Gets a list of all the values in a given trie.\n"
    "\n"
    " Tries are not ordered so the values are not returned in any specific order.\n"
    " Do not write code that relies on the order values are returned by this function\n"
    " as it may change in later versions of the library.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " > [#([1, 2], \"a\"), #([1], \"b\")]\n"
    " > |> from_list\n"
    " > |> values\n"
    " [\"a\", \"b\"]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> values\n"
    " []\n"
    " ```\n"
).
-spec values(trie(any(), HKK)) -> list(HKK).
values(Trie) ->
    fold(Trie, [], fun(Values, _, Value) -> [Value | Values] end).
