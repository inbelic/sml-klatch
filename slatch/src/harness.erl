-module(harness).

%% Harness around the Haskell Klatchling implementation to iteract with the
%% broader server

-behaviour(gen_server).

-define(GAME_PORT, 3637).
-define(OK, "0").
-define(NEW_GAME, 0).

-record(state,
        { l_sock = undefined
        , c_sock = undefined
        }).

%% API for responding to the Haskell game management
-export([send_ok/1, send_start/1, send_target/2, send_order/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
send_ok(GameID) when is_integer(GameID) ->
    package_and_send(GameID, ?OK).

send_start(Config) when is_list(Config) ->
    package_and_send(?NEW_GAME, Config).

send_target(GameID, Int) when is_integer(GameID) andalso is_integer(Int) ->
    IntStr = integer_to_list(Int),
    package_and_send(GameID, IntStr).

send_order(GameID, Order) when is_integer(GameID) andalso is_list(Order) ->
    OrderStr = str_conv:int_list_to_string(Order),
    package_and_send(GameID, OrderStr).

package_and_send(GameID, Str) when is_integer(GameID) andalso is_list(Str) ->
    Header = integer_to_list(GameID) ++ ":",
    send_bin(list_to_binary(Header ++ Str)).

send_bin(Bin) ->
    gen_server:cast(harness, {send, Bin}).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% gen_server exports
%% Join and listen until you connect with corresponding game 'server'
init([]) ->
    register(harness, self()),
    gen_server:cast(harness, init),
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
handle_cast({send, Bin}, State) ->
    do_send(State, Bin);
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


do_tcp(Bin, State) ->
    client_mgr:forward(Bin),
    {noreply, State}.

do_send(#state{c_sock = Sock} = State, Bin) ->
    gen_tcp:send(Sock, Bin),
    {noreply, State}.
