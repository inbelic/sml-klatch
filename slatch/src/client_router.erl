-module(client_router).

%% This server implements the logic to split the incoming request from the
%% Haskell game server into the three components and route the individual
%% components to their respective client connections

%% API for harness
-export([forward/3]).

%% API for game_queue
-export([notify_start/4]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { client_map = maps:new() :: maps:maps(game_id(), clients())
        }).

-record(clients,
        { p1        :: pid()
        , p2        :: pid()
        , resource  :: pid()
        }).

-type game_id() :: integer().
-type clients() :: #clients{}.

-export_type([game_id/0]).

-spec forward(byte(), game_id(), binary()) -> ok.
forward(Cmd, GameID, Request) ->
    gen_server:cast(?MODULE, {forward, Cmd, GameID, Request}).

-spec notify_start(game_id(), pid(), pid(), pid()) -> ok.
notify_start(GameID, P1Pid, P2Pid, ResourcePid) ->
    Clients = #clients{p1 = P1Pid, p2 = P2Pid, resource = ResourcePid},
    gen_server:cast(?MODULE, {notify_start, GameID, Clients}).

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

handle_cast({forward, Cmd, GameID, Request}, State) ->
    ok = do_forward(Cmd, GameID, Request, State#state.client_map),
    {noreply, State};
handle_cast({notify_start, GameID, Clients}, State) ->
    {ok, ClientMap} = do_notify_start(GameID, Clients, State#state.client_map),
    {noreply, State#state{client_map = ClientMap}};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

do_forward(Cmd, GameID, Request, ClientMap) ->
    {P1Req, P2Req, ResourceReq} = str_conv:split_request(Request),
    case maps:get(GameID, ClientMap) of
            %% TODO: The game server and this are out of sync...
            %% shouldn't just ignore (but we will for now :))
            {badmap, _ClientMap} -> ok;
            {badkey, _Key} -> ok;
            Clients ->
                P1Pid = Clients#clients.p1,
                pid_forward(client_srvr, P1Pid, Cmd, P1Req),
                P2Pid = Clients#clients.p2,
                pid_forward(client_srvr, P2Pid, Cmd, P2Req),
                ResourcePid = Clients#clients.resource,
                pid_forward(game_resource, ResourcePid, Cmd, ResourceReq),
                ok
    end.

pid_forward(Module, Pid, Cmd, Req) ->
    Module:forward(Pid, Cmd, Req).

do_notify_start(GameID, Clients, ClientMap) ->
    {ok, maps:put(GameID, Clients, ClientMap)}.
