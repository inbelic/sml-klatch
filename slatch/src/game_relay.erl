-module(game_relay).

%% server to handle the the startup of a game and relaying messages from
%% the clients to the game harness

-behaviour(gen_server).


%% client_srvr API
-export([respond/2]).

%% game_queue API
-export([start_game/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { relays = maps:new() :: maps:maps(game_id(), responses())
        , orders = maps:new() :: maps:maps(game_id(), orders())
        }).

-type game_id()    :: integer().
-type responses()  :: [{ready, pid(), binary()}].
-type orders()     :: maps:maps(p1 | p2 | resource, pid()).


%% APIs
-spec start_game(pid(), pid()) -> ok.
start_game(P1Pid, P2Pid) ->
    gen_server:cast(?MODULE, {start_game, P1Pid, P2Pid}).

-spec respond(game_id(), ready | binary()) -> ok.
respond(GameID, Response) ->
    gen_server:cast(?MODULE, {response, GameID, {self(), Response}}).

%% Startup
-spec start() -> ok.
start() ->
    gen_server:start(?MODULE, [], []).

-spec start_link() -> ok.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    register(?MODULE, self()),
    {ok, #state{}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({start_game, P1Pid, P2Pid}, State) ->
    State1 = do_start_game(P1Pid, P2Pid, State),
    {noreply, State1};
handle_cast({response, GameID, Relay}, State) ->
    State1 = do_response(GameID, Relay, State),
    {noreply, State1};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.


do_start_game(P1Pid, P2Pid, State) ->
    GameID = allocate_game_id(State#state.orders),
    {ok, ResourcePid} = game_resource:start_link(GameID),
    Order = #{p1 => P1Pid, p2 => P2Pid, resource => ResourcePid},
    Orders = maps:put(GameID, Order, State#state.orders),
    Relays = maps:put(GameID, [], State#state.relays),

    ok = client_srvr:notify_start(P1Pid, GameID),
    ok = client_srvr:notify_start(P2Pid, GameID),

    ok = client_router:notify_start(GameID, P1Pid, P2Pid, ResourcePid),
    State#state{relays = Relays, orders = Orders}.

%% We could have a more sofisticated way to do so here if we desire (and to
%% also prevent the eventual overflow)
allocate_game_id(Orders) ->
    maps:fold(fun max/3, 0, Orders) + 1.

max(X, _Val, Y) when Y < X ->
    X;
max(_X, _Val, Y) ->
    Y.

do_response(GameID, {Pid, ready}, State) ->
    do_response(GameID, {Pid, <<>>}, State);
do_response(GameID, {Pid, Response}, State) ->
    {ok, Responses0} = maps:find(GameID, State#state.relays),
    Responses = [{ready, Pid, Response} | Responses0],
    %% If length is 3 then we have received a ready from all clients
    case length(Responses) == 3 of
        true ->
            do_relay(GameID, Responses, State);
        false ->
            Relays = maps:put(GameID, Responses, State#state.relays),
            State#state{relays = Relays}
    end.

do_relay(GameID, Responses, State) ->
    {ok, Order} = maps:find(GameID, State#state.orders),

    {ok, P1Pid} = map:find(p1, Order),
    {ok, P1Response} = lists:keyfind(P1Pid, 2, Responses),

    {ok, P2Pid} = map:find(p1, Order),
    {ok, P2Response} = lists:keyfind(P2Pid, 2, Responses),

    {ok, ResourcePid} = map:find(p1, Order),
    {ok, ResourceResponse} = lists:keyfind(ResourcePid, 2, Responses),

    Response = ["{", P1Response, "}{", P2Response, "}{", ResourceResponse, "}"],
    harness:send_response(GameID, Response),

    Relays = maps:put(GameID, [], State#state.relays),
    State#state{relays = Relays}.
