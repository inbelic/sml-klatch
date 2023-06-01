-module(client_srvr).

-behaviour(gen_server).

-include("../include/client_srvr.hrl").

%% API for responding to the Haskell game management
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { c_sock    = undefined
        , username  = undefined
        , game_id   = undefined
        , fsm       = login :: fsm_state()
        }).

-type fsm_state() :: login | lobby | queued | joining
                   | {playing, client_mgr:response_type() | waiting}.

-type user_info() :: #user_info{}.

-export_type([user_info/0]).

%% Helpful macros
-define(WAIT, {playing, waiting}).

%% API
forward(Pid, Bin, Type) ->
    gen_server:cast(Pid, {forward, Bin, Type}).

%% Startup
start([ListenSock]) ->
    gen_server:start(?MODULE, [ListenSock], []).

start_link([ListenSock]) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn_link(fun client_sup:start_socket/0), %% Replace with listening socket
    {ok, #state{c_sock = Sock}}.

%% TCP Socket handling
handle_info({tcp, Sock, Bin}, #state{c_sock = Sock} = State) ->
    do_tcp(Bin, State);
handle_info({tcp_closed, Sock}, #state{c_sock = Sock} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Sock}, #state{c_sock = Sock} = State) ->
    {stop, normal, State}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({started, GameID}, #state{fsm = queued} = State) ->
    {noreply, State#state{fsm = joining, game_id = GameID}};
handle_cast({forward, Bin, Type}, #state{fsm = ?WAIT} = State) ->
    do_forward(Bin, Type, State),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{c_sock = Sock} = _State) ->
    try
        gen_tcp:close(Sock),
        ok
    catch
        _:_ ->
            ok
    end.

do_forward(Bin, Type, #state{c_sock = Sock} = State) ->
    gen_tcp:send(Sock, Bin),
    {noreply, State#state{fsm = {playing, Type}}}.

%% Handle client input logic
do_tcp(<<Cmd,Bin/binary>>, #state{fsm = login} = State) ->
    case Cmd of
        ?USERNAME ->
            Username = binary_to_list(Bin),
            State1 = State#state{fsm = lobby, username = Username},
            respond_to_client(<<?OK>>, State1);
        _ ->
            respond_to_client(<<?NOTOKAY>>, State)
    end;
do_tcp(<<Cmd, _Bin/binary>>, #state{fsm = lobby} = State) ->
    case Cmd of
        ?QUEUE ->
            %% TODO: implement a database to store and fetch this info from
            Info = #user_info{pid = self(), rating = 100, location = sweden},
            game_queue:queue(State#state.username, Info),
            respond_to_client(<<?OK>>, State#state{fsm = queued});
        _ ->
            respond_to_client(<<?NOTOKAY>>, State)
    end;
do_tcp(<<Cmd, Bin/binary>>, #state{fsm = {playing, Type}} = State) ->
    %% TODO: add validation that info is appropriate
    GameID = State#state.game_id,
    case {Cmd, Type} of
        {?OK, display} ->
            respond_to_game(GameID, <<?OK>>, State);
        {?CONFIG, start} ->
            respond_to_game(GameID, Bin, State);
        {?TARGET, target} ->
            respond_to_game(GameID, Bin, State);
        {?ORDER, order} ->
            respond_to_game(GameID, Bin, State);
        _ ->
            respond_to_client(<<?NOTOKAY>>, State)
    end.

respond_to_game(GameID, Bin, State) ->
    client_mgr:respond(GameID, self(), Bin),
    respond_to_client(<<?OK>>, State#state{fsm = ?WAIT}).

respond_to_client(Bin, #state{c_sock = Sock} = State) ->
    ok = gen_tcp:send(Sock, Bin),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State}.
