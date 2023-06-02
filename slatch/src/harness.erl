-module(harness).

%% Harness around the Haskell Klatchling implementation to iteract with the
%% broader server

-behaviour(gen_server).

%% API for responding to the Haskell game management
-export([send_response/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


%% Internal definitions
-define(GAME_PORT, 3637). %%TODO: could be moved to public info

-record(state,
        { l_sock :: gen_tcp:socket()
        , c_sock :: gen_tcp:socket()
        }).


%% Respond API
-spec send_response(integer(), iolist()) -> ok.
send_response(GameID, Bin) when is_integer(GameID) andalso is_binary(Bin) ->
    gen_server:cast(?MODULE, {send, GameID, Bin}).

%% Startup
-spec start() -> ok.
start() ->
    gen_server:start(?MODULE, [], []).

-spec start_link() -> ok.
start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% gen_server exports
%% Join and listen until you connect with corresponding game 'server'
init([]) ->
    register(?MODULE, self()),
    gen_server:cast(?MODULE, init),
    %% We can just let our things crash in the unlikely event that someone
    %% casts before we have processed our own init message with the undefined
    %% state
    {ok, undefined}.

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

handle_cast(init, undefined) ->
    {ok, State} = do_init(),
    {noreply, State};
handle_cast({send, GameID, Bin}, State) ->
    ok = do_send(GameID, Bin, State#state.c_sock),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{c_sock = Sock, l_sock = ListenSock} = _State) ->
    try
        gen_tcp:close(Sock),
        gen_tcp:close(ListenSock),
        ok
    catch
        _:_ ->
            ok
    end.

%% Helper functions
do_init() ->
    {ok, ListenSock}
        = gen_tcp:listen(?GAME_PORT,
                         [ binary
                         , {packet, 0}
                         , {reuseaddr, true}
                         , {active, true}
                         ]),
    {ok, Sock} = gen_tcp:accept(ListenSock),
    {ok, #state{l_sock = ListenSock, c_sock = Sock}}.


do_tcp(<<Cmd, Bin/binary>>, State) ->
    {GameID, Request} = misc:strip_gameid(Bin),
    ok = client_router:forward(Cmd, GameID, Request),
    {noreply, State}.

do_send(GameID, Response, Sock) ->
    Bin = iolist_to_binary([[integer_to_binary(GameID) | ":" ] | Response]),
    gen_tcp:send(Sock, Bin),
    ok.
