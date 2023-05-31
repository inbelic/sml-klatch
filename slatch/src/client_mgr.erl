-module(client_mgr).

%% This server implements the logic to route the messages from the harness
%% to the correct clients and their respective game_resource. It also manages
%% the game ids that will map between these instances. Furthermore, it handles
%% collecting all the responses of clients before sending a unified response
%% to the game harness.

%% API for responding to the Haskell game management
-export([forward/1]).

%% API for clients to respond to the Haskell game
-export([respond/3]).

%% API for other serverside servers to interact with the games
-export([start_game/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export_type([response_type/0]).

-define(REQ_CONFIG, ":{config}{config}{}").

-record(state,
        { client_map = maps:new() %% maps the routing of message to clients
        }).

-record(client_state,
        { p1            :: response_state()
        , p2            :: response_state()
        , game_resource :: response_state()
        , type          :: response_type()
        }).

-type response_state()  :: {pid(), response_status()}.
-type response_status() :: responding | waiting | ready | {ready, binary()}.

%% corresponding to send_* in harness
-type response_type()   :: display | start | target | random | order.

%% API
forward(Bin) ->
    gen_server:cast(?MODULE, {forward, Bin}).

respond(GameID, From, Bin) ->
    gen_server:cast(?MODULE, {respond, GameID, From, Bin}).

start_game(P1Pid, P2Pid) ->
    Req = {start_game, P1Pid, P2Pid},
    gen_server:call(?MODULE, Req).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    register(?MODULE, self()),
    {ok, #state{}}.

%% Start the game
handle_call({start_game, P1Pid, P2Pid}, _From,
            #state{client_map = ClientMap} = State) ->
    ClientMap1 = do_start_game(P1Pid, P2Pid, ClientMap),
    {reply, ok, State#state{client_map = ClientMap1}};
%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

%% Forward the message from the game to the corresponding players/resource
handle_cast({forward, Bin}, #state{client_map = ClientMap} = State) ->
    ClientMap1 = do_forward(Bin, ClientMap),
    {noreply, State#state{client_map = ClientMap1}};
%% Handle the response from one of the clients
%% WARNING: all responses are discarded if the response_status is not
%% responding for the sender (From)
handle_cast({respond, GameID, From, Bin},
            #state{client_map = ClientMap} = State) ->
    ClientMap1 = do_response(GameID, From, Bin, ClientMap),
    {noreply, State#state{client_map = ClientMap1}};

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
    ok.

%% Start the game by allocating an available game id and a game resource
%% then add these to the client map to be retreive when routing
do_start_game(P1Pid, P2Pid, ClientMap) ->
    GameID = allocate_gid(ClientMap),
    {ok, GameResourcePid} = game_resource:start_link(GameID),
    ClientState = #client_state{ p1 = {P1Pid, waiting}
                               , p2 = {P2Pid, waiting}
                               , game_resource = {GameResourcePid, waiting}
                               , type = start
                               },
    RequestConfig = list_to_binary(integer_to_list(GameID) ++ ?REQ_CONFIG),
    forward(RequestConfig),
    maps:put(GameID, ClientState, ClientMap).

%% Split the message into the required sections and route the messages to their
%% respective client or game_resource
do_forward(Bin, ClientMap) ->
    {GameID, Msg} = str_conv:strip_game_id(Bin),
    case maps:get(GameID, ClientMap) of
        {badmap, _ClientMap} ->
            ClientMap;
        {badkey, _Key} ->
            ClientMap;
        #client_state{} = ClientState ->
            {P1Msg, P2Msg, GameResourceMsg} = str_conv:split_message(Msg),
            Type = str_conv:determine_msg_type(P1Msg, P2Msg, GameResourceMsg),
            P1Status = forward_msg(client, ClientState#client_state.p1,
                                   P1Msg, Type),
            P2Status = forward_msg(client, ClientState#client_state.p2,
                                   P2Msg, Type),
            GameResourceStatus
                = forward_msg(game, ClientState#client_state.game_resource,
                              GameResourceMsg, Type),
            ClientState1 = #client_state{ p1 = P1Status
                                        , p2 = P2Status
                                        , game_resource = GameResourceStatus
                                        , type = Type
                                        },
            %% Check in case all messages were empty, otherwise we will get
            %% stuck in this state
            send_if_ready(GameID, ClientState1, ClientMap)
    end.


%% Record a response from a client if we are expecting a response (are in
%% responding state). If all clients are ready after processing the response
%% then we can send the complete response to the harness
do_response(GameID, From, Bin, ClientMap) ->
    case maps:get(GameID, ClientMap) of
        {badmap, _ClientMap} ->
            ClientMap;
        {badkey, _Key} ->
            ClientMap;
        #client_state{} = ClientState ->
            {From, Status} = with_from(fun(X) ->
                                              X
                                       end, ClientState, From),
            case Status of
                responding ->
                    ClientState1 = map_from(fun({Pid, _Status}) ->
                                                     {Pid, {ready, Bin}}
                                             end, ClientState, From),
                    send_if_ready(GameID, ClientState1, ClientMap);
                _ ->
                    ClientMap
            end
    end.

send_if_ready(GameID, ClientState, ClientMap) ->
    case are_all(fun is_ready/1, ClientState) of
        true ->
            ok = respond_to_harness(GameID, ClientState),
            ClientState1 = map_responses(fun({Pid, _}) ->
                                                 {Pid, waiting}
                                         end, ClientState),
            maps:put(GameID, ClientState1, ClientMap);
        false ->
            maps:put(GameID, ClientState, ClientMap)
    end.

%% Helper functions
max(X, _Val, Y) when Y < X ->
    X;
max(_X, _Val, Y) ->
    Y.

allocate_gid(ClientMap) ->
    maps:fold(fun max/3, 0, ClientMap) + 1.

%% No message to respond to so we don't need to pester the client and we will
%% just be ready in collection
forward_msg(_, {Pid, _Status}, "", _) ->
    {Pid, ready};
%% A msg to be forwarded to a client so we turn to binary to let the client_srvr
%% just put the binary into the port directly
forward_msg(client, {Pid, _Status}, Msg, Type) ->
    client_srvr:forward(Pid, Type, list_to_binary(Msg)),
    {Pid, responding};
%% A msg to be forwarded to a game_resource so we keep as a list for easier
%% operation of data in that logic
forward_msg(game, {Pid, _Status}, Msg, Type) ->
    game_resource:forward(Pid, Type, Msg),
    {Pid, responding}.

with_from(Fun, #client_state{p1 = {From, _} = State}, From) ->
    Fun(State);
with_from(Fun, #client_state{p2 = {From, _} = State}, From) ->
    Fun(State);
with_from(Fun, #client_state{game_resource = {From, _} = State}, From) ->
    Fun(State).

%% map_from will apply Fun to the state of the client that has the Pid matching
%% From
map_from(Fun, #client_state{p1 = {From, _} = P1} = State, From) ->
    State#client_state{p1 = Fun(P1)};
map_from(Fun, #client_state{p2 = {From, _} = P2} = State, From) ->
    State#client_state{p2 = Fun(P2)};
map_from(Fun, #client_state{game_resource = {From, _} = GameResource} = State,
         From) ->
    State#client_state{game_resource = Fun(GameResource)}.

are_all(Fun, #client_state{p1 = P1, p2 = P2, game_resource = GameResource}) ->
    Fun(P1) andalso Fun(P2) andalso Fun(GameResource).

map_responses(Fun, #client_state{p1 = P1, p2 = P2,
                           game_resource = GameResource} = ClientState) ->
    ClientState#client_state{p1 = Fun(P1), p2 = Fun(P2),
                             game_resource = Fun(GameResource)}.

fold_responses(Fun, Acc, #client_state{p1 = P1, p2 = P2,
                                       game_resource = GameResource}) ->
    lists:foldl(Fun, Acc, [P1, P2, GameResource]).

is_ready({_, ready}) ->
    true;
is_ready({_, {ready, _}}) ->
    true;
is_ready(_) ->
    false.

respond_to_harness(GameID, ClientState) ->
    case ClientState#client_state.type of
        display ->
            harness:send_ok(GameID);
        start ->
            Fun = fun({_, {ready, Config}}, Acc) ->
                          [Config, Acc]; %% IO list
                     (_, Acc) ->
                          Acc
                  end,
            InitConfig = fold_responses(Fun, <<"">>, ClientState),
            harness:send_start(GameID, InitConfig);
        target ->
            Fun = fun({_, {ready, TargetBin}}, _Acc) ->
                          TargetStr = binary_to_list(TargetBin),
                          Target = str_conv:string_to_int(TargetStr),
                          Target;
                     (_, Acc) ->
                          Acc
                  end,
            Target = fold_responses(Fun, bad_value, ClientState),
            harness:send_target(GameID, Target);
        order ->
            %% TODO: Sorting order is just Game -> P2 -> P1 but should be
            %% dep on the current turn maybe or something?
            Fun = fun({_, {ready, OrderBin}}, Acc) ->
                          OrderStr = binary_to_list(OrderBin),
                          Order = str_conv:string_to_int_list(OrderStr),
                          Order ++ Acc;
                     (_, Acc) ->
                          Acc
                  end,
            Order = lists:reverse(fold_responses(Fun, [], ClientState)),
            harness:send_order(GameID, Order)
    end.
