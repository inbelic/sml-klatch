-module(game_port).

%% Port to the klatch program to allow a strangling process to monitor it.
%% Allows us to restart the game server using erlang supervisors.

-behaviour(gen_server).

%% gen_server exports and port startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(KLATCHLINGS, "../../klatch/build/klatch-game").

-record(state,
        { port
        , handle
        }).

%% Startup
-spec start() -> ok.
start() ->
    gen_server:start(?MODULE, [], []).

-spec start_link() -> ok.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    {ok, Handle} = file:open("port.logs", [write]),
    {ok, Port} = start_port(),
    {ok, #state{port = Port, handle = Handle}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Info catch-all
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    Handle = State#state.handle,
    io:format(Handle, "~p~n", [Data]),
    {noreply, State};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{port = Port, handle = Handle} = _State) ->
    port_close(Port),
    file:close(Handle),
    ok.

start_port() ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ?KLATCHLINGS},
                     [use_stdio, {packet, 1}, in]),
    {ok, Port}.
