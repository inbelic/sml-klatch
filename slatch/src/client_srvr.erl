-module(client_srvr).

-behaviour(gen_server).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { c_sock = undefined
        , username = undefined
        }).

%% Startup
start([ListenSock]) ->
    gen_server:start(?MODULE, [ListenSock], []).

start_link([ListenSock]) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
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
    {stop, unknown_call, State}.

%% Cast catch-all
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

%% Terminate catch-all
terminate(_Reason, #state{c_sock = Sock} = _State) ->
    try
        gen_tcp:close(Sock),
        ok
    catch
        _:_ ->
            ok
    end.

%% Helper Function
do_tcp(Bin, #state{c_sock = Sock} = State) ->
    io:format("~p~n", [Bin]),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State}.
