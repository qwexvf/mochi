-module(json_ffi).
-export([type_of/1]).

type_of(V) when is_boolean(V) -> <<"bool">>;
type_of(V) when is_integer(V) -> <<"int">>;
type_of(V) when is_float(V)   -> <<"float">>;
type_of(V) when is_binary(V)  -> <<"string">>;
type_of(V) when is_list(V)    -> <<"list">>;
type_of(V) when is_map(V)     -> <<"dict">>;
type_of(nil)                  -> <<"null">>;
type_of(_)                    -> <<"null">>.
