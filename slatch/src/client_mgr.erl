-module(client_mgr).

%% API for responding to the Haskell game management
-export([forward/1]).

%% API for other serverside servers to interact with the games
-export([start_game/3]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
        { client_map = maps:new()
        }).

%% API
forward(Bin) ->
    gen_server:cast(client_mgr, {forward, Bin}).

start_game(P1Pid, P2Pid, InitConfig) ->
    Req = {start_game, P1Pid, P2Pid, InitConfig},
    gen_server:cast(client_mgr, Req).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    register(client_mgr, self()),
    {ok, #state{}}.

handle_call({start_game, P1Pid, P2Pid, InitConfig}, _From,
            #state{client_map = ClientMap} = State) ->
    GameID = allocate_gid(ClientMap),
    GameResourcePid = game_resource:start_link(),
    ok = harness:send_start(GameID, InitConfig),
    ClientMap1 = maps:put(GameID, {P1Pid, P2Pid, GameResourcePid}, ClientMap),
    {reply, ok, State#state{client_map = ClientMap1}};
%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, Bin}, #state{client_map = ClientMap} = State) ->
    {GameID, Msg} = str_conv:strip_game_id(Bin),
    case maps:get(GameID, ClientMap) of
        {badmap, _ClientMap} ->
            ok; %% TODO
        {badkey, _Key} ->
            ok; %% TODO
        {P1Pid, P2Pid, GameResourcePid} ->
            {P1Msg, P2Msg, GameMsg} = split_message(Msg),
            client_srvr:forward(P1Pid, list_to_binary(P1Msg)),
            client_srvr:forward(P2Pid, list_to_binary(P2Msg)),
            game_resource:forward(GameResourcePid, GameMsg)
    end,
    {noreply, State};

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
    ok.


%% Helper functions
max(X, _Val, Y) when Y < X ->
    X;
max(_X, _Val, Y) ->
    Y.

allocate_gid(ClientMap) ->
    maps:fold(fun max/3, 0, ClientMap) + 1.

split_message(Msg) ->
    split_message(Msg, []).

split_message([], Acc) ->
    list_to_tuple(lists:reverse(Acc));
split_message([${ | Msg], Acc) ->
    {CurMsg, [$} | Rest]}
        = lists:splitwith(fun(Char) ->
                                  Char /= $}
                          end, Msg),
    split_message(Rest, [CurMsg | Acc]).
