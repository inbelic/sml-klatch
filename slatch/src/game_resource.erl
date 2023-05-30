-module(game_resource).

%% This server implements the logic to respond to the output of the messages
%% in the third {} of the game

-behaviour(gen_server).

%% API for serverside interactions
-export([forward/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

%% API
forward(Pid, Msg) ->
    gen_server:cast(Pid, {forward, Msg}).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, undefined}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, _Msg}, State) ->
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.
