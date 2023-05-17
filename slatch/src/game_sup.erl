-module(game_sup).

-behaviour(supervisor).

%% start up and behaviour exports
-export([start_link/0]).
-export([init/1]).

%% Supervisor to allow our port program that contains the haskell game server
%% and our tcp_harness to the underlying game to reconnect in the case either
%% crashes.
%%
%% If either worker terminates for any reason then the tcp connection between
%% the two programs will disconnect. We use the supervisor to restart the
%% workers and re-establish the tcp connection. The harness server is
%% responsible to close the existing sockets and the haskell code is
%% responsible to save its game state to disk before restarting.

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags
        = #{ strategy => one_for_all
           },
    HarnessSpec
        = #{ id => harness
           , start => {harness, start_link, []}
           },
    PortSpec
        = #{ id => game_port
           , start => {game_port, start_link, []}
           },
    {ok, {SupFlags, [HarnessSpec, PortSpec]}}.
