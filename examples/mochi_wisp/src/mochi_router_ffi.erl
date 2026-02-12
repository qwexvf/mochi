-module(mochi_router_ffi).
-export([get_schema/0, set_schema/1]).

-define(SCHEMA_KEY, mochi_graphql_schema).

%% Store schema in persistent_term for optimal read performance
%% persistent_term is ideal for data that is read often but written rarely
set_schema(Schema) ->
    persistent_term:put(?SCHEMA_KEY, Schema),
    nil.

%% Get the cached schema
%% This is extremely fast - no copying, direct reference
get_schema() ->
    persistent_term:get(?SCHEMA_KEY).
