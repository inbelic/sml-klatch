%% Module adapted from learn you erlang
-module(client_sup).
-behaviour(supervisor).

%% Behaviour callbacks
-export([start_link/0, init/1]).

%% Start a dynamic child
-export([start_socket/0]).

-define(CONN_PORT, 3638).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% See client_srvr
    {ok, ListenSock} = gen_tcp:listen(?CONN_PORT,
                                        [ binary
                                        , {active, once}
                                        , {packet, line}
                                        , {reuseaddr, true}
                                        ]),
    spawn_link(fun empty_listeners/0),
    SupFlags
        = #{ strategy => simple_one_for_one
           , intensity => 60
           , period => 3600
           },
    ClientSpec
        = #{ id => client_srvr
           , start => {client_srvr, start_link, [[ListenSock]]}
           , restart => temporary   %% Connection dropped no need to restart
           , modules => [client_srvr]
           },
    {ok, {SupFlags, [ClientSpec]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
