-module(mochi_app_state_ffi).
-export([set_app_state/1, get_app_state/0]).

-define(APP_STATE_KEY, mochi_app_state).

%% Store app state in persistent_term
%% persistent_term is ideal for data that is read often but written rarely
set_app_state(State) ->
    persistent_term:put(?APP_STATE_KEY, State),
    nil.

%% Get the app state
%% This is extremely fast - no copying, direct reference
get_app_state() ->
    persistent_term:get(?APP_STATE_KEY).
