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
    RoutingSpec
        = #{ id => routing_sup
           , start => {routing_sup,start_link, []}
           , modules => [routing_sup]
           },
    ClientSupSpec
        = #{ id => client_sup
           , start => {client_sup, start_link, []}
           , modules => [client_sup]
           , type => supervisor
           },
    GameSupSpec
        = #{ id => game_sup
           , start => {game_sup, start_link, []}
           , modules => [game_sup]
           , type => supervisor
           },
    {ok, {SupFlags, [RoutingSpec, ClientSupSpec, GameSupSpec]}}.
