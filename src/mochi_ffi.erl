-module(mochi_ffi).
-export([get_args/0]).

get_args() ->
    case init:get_plain_arguments() of
        Args when is_list(Args) ->
            [unicode:characters_to_binary(A) || A <- Args];
        _ ->
            []
    end.
