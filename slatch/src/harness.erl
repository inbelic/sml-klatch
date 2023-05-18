-module(harness).

%% Harness around the Haskell Klatchling implementation to iteract with the
%% broader server

-behaviour(gen_server).

-define(GAME_PORT, 3637).

-define(OK, <<"0">>).

-record(state,
        { l_sock = undefined
        , c_sock = undefined
        }).

%% API for responding to the Haskell game management
-export([send_ok/0, send_target/1, send_order/1]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
send_ok() ->
    send_bin(?OK).

send_target(Int) when is_integer(Int) ->
    IntStr = integer_to_list(Int),
    send_bin(list_to_binary(IntStr)).

send_order(Order) when is_list(Order) ->
    OrderStr = int_list_to_string(Order),
    send_bin(list_to_binary(OrderStr)).

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
    {stop, haskell_closed, State};
handle_info({tcp_error, Sock}, #state{c_sock = Sock} = State) ->
    {stop, haskell_closed, State}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

handle_cast(init, undefined) ->
    {ok, State} = do_init(),
    {noreply, State};
handle_cast({send, Bin}, State) ->
    do_send(State, Bin);
%% Cast catch-all
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

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



int_list_to_string([]) ->
    "[]";
int_list_to_string([Int | Rest]) when is_integer(Int) ->
    int_list_to_string(Rest, "[" ++ integer_to_list(Int)).

int_list_to_string([], Acc) ->
    Acc ++ "]";
int_list_to_string([Int | Rest], Acc) when is_integer(Int) ->
    int_list_to_string(Rest, Acc ++ "," ++ integer_to_list(Int)).
