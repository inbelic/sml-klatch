-module(game_resource).

%% This server implements the logic to respond to the output of the messages
%% in the third {} of the game

-behaviour(gen_server).

%% API for serverside interactions
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%% API
forward(Pid, Msg, Type) ->
    gen_server:cast(Pid, {forward, Msg, Type}).

%% Startup
start(GameID) ->
    gen_server:start(?MODULE, [GameID], []).

start_link(GameID) ->
    gen_server:start_link(?MODULE, [GameID], []).

init([GameID]) ->
    {ok, GameID}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, _Msg, _Type}, State) ->
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.
