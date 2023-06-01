-module(slatch_sup).

-behaviour(supervisor).

%% Behaviour callbacks
-export([start_link/0, init/1]).

%% TODO: Add tree structure dipiction here

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags
        = #{ strategy => rest_for_one
           },
    ClientMgrSpec
        = #{ id => client_mgr
           , start => {client_mgr, start_link, []}
           , modules => [client_mgr]
           },
    ClientSupSpec
        = #{ id => client_sup
           , start => {client_sup, start_link, []}
           , modules => [client_sup]
           , type => supervisor
           },
    GameQueueSpec
        = #{ id => game_queue
           , start => {game_queue, start_link, []}
           , modules => [game_queue]
           },
    GameSupSpec
        = #{ id => game_sup
           , start => {game_sup, start_link, []}
           , modules => [game_sup]
           , type => supervisor
           },
    {ok, {SupFlags, [ClientMgrSpec, ClientSupSpec,
                     GameQueueSpec, GameSupSpec]}}.
