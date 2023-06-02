-module(client_srvr).

-behaviour(gen_server).

-include("../include/cmds.hrl").

%% API for client_router
-export([forward/3]).

%% API for game_queue
-export([notify_start/2]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { c_sock    :: gen_tcp:socket()
        , username  :: string()
        , game_id   :: misc:game_id()
        , fsm       :: pid()
        }).

%% API
-spec forward(pid(), byte(), binary()) -> ok.
forward(Pid, Cmd, Req) ->
    gen_server:cast(Pid, {forward, Cmd, Req}).

-spec notify_start(pid(), misc:game_id()) -> ok.
notify_start(Pid, GameID) ->
    gen_server:cast(Pid, {notify_start, GameID}).

%% Startup
start([ListenSock]) ->
    gen_server:start(?MODULE, [ListenSock], []).

start_link([ListenSock]) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn_link(fun client_sup:start_socket/0), %% Replace with listening socket
    Fsm = client_sm:start_link(),
    {ok, #state{c_sock = Sock, fsm = Fsm}}.

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

handle_cast({forward, _, <<>>}, State) ->
    %% No need to bother the client
    ok = game_relay:respond(State#state.game_id, ready),
    {noreply, State};
handle_cast({forward, Cmd, Req}, State) ->
    ok = do_forward(Cmd, Req, State),
    {noreply, State};
handle_cast({notify_state, GameID}, State) ->
    ok = client_sm:request(State#state.fsm, <<?STARTED>>),
    {noreply, State#state{game_id = GameID}};
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

%% Client input
do_tcp(<<Cmd, Bin>>, #state{fsm = Fsm} = State) ->
    State2 = case client_sm:validate(Fsm, Bin) of
                 valid ->
                     {ok, State1} = handle_cmd(Cmd, Bin, State),
                     State1;
                 invalid ->
                     ok = do_forward(<<?INVALID>>, <<>>, State),
                     State
             end,
    {noreply, State2}.

handle_cmd(<<?LOGIN>>, Username, State) ->
    ok = do_forward(<<?OK>>, <<>>, State),
    State#state{username = Username};
handle_cmd(<<?QUEUE>>, Config, State) ->
    Username = State#state.username,
    ok = game_queue:queue(Username, Config),
    State;
handle_cmd(PlayCmd, Response, State) when PlayCmd == <<?ORDER>> orelse
                                          PlayCmd == <<?TARGET>> orelse
                                          PlayCmd == <<?CONCEDE>> ->
    GameID = State#state.game_id,
    ok = game_relay:respond(GameID, Response),
    State;
handle_cmd(_, _, _) ->
    bad_state.

do_forward(Cmd, Request, #state{c_sock = Sock} = State) ->
    ok = client_sm:request(State#state.fsm, Cmd),
    gen_tcp:send(Sock, [Cmd | Request]),
    inet:setopts(Sock, [{active, once}]),
    ok.
