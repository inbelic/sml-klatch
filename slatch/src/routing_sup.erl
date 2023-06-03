-module(routing_sup).

-behaviour(supervisor).

%% Behaviour callbacks
-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags
        = #{ strategy => one_for_all
           },
    ClientRouterSpec
        = #{ id => client_router
           , start => {client_router, start_link, []}
           , modules => [client_router]
           },
    GameRelaySpec
        = #{ id => game_relay 
           , start => {game_relay, start_link, []}
           , modules => [game_relay]
           },
    GameQueueSpec
        = #{ id => game_queue
           , start => {game_queue, start_link, []}
           , modules => [game_queue]
           },
    {ok, {SupFlags, [ClientRouterSpec, GameRelaySpec, GameQueueSpec]}}.
