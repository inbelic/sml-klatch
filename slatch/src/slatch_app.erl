-module(slatch_app).

-behaviour(application).

%% Behaviour callbacks
-export([start/0, start/2, stop/1]).

start() ->
    start(undefined, undefined).

start(_Type, _Args) ->
    slatch_sup:start_link().

stop(_State) ->
    ok.
