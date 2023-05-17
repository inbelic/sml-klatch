-module(game_port).

%% Port to the klatch program to allow a strangling process to monitor it.
%% Allows us to restart the game server using erlang supervisors.

-behaviour(gen_server).

%% gen_server exports and port startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(KLATCHLINGS, "../../klatch/build/klatch-game").

-record(state,
        { port = undefined
        }).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    {ok, Port} = start_port(),
    {ok, #state{port = Port}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

%% Cast catch-all
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

%% Info catch-all
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    {stop, port_disconnect, State};
handle_info(_Info, State) ->
    {stop, unknown_info, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
    ok.

start_port() ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ?KLATCHLINGS}, []),
    {ok, Port}.
