-module(game_resource).

%% This server implements the logic to respond to the output of the messages
%% in the third {} of the game

-behaviour(gen_server).

-include("../include/cmds.hrl").

%% API for serverside interactions
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { game_id  :: misc:game_id()
        , rand     :: rand:state()
        }).

%% API
-spec forward(pid(), byte(), binary()) -> ok.
forward(Pid, Cmd, Req) ->
    gen_server:cast(Pid, {forward, Cmd, Req}).

%% Startup
-spec start(misc:game_id()) -> ok.
start(GameID) ->
    gen_server:start(?MODULE, [GameID], []).

-spec start_link(misc:game_id()) -> ok.
start_link(GameID) ->
    gen_server:start_link(?MODULE, [GameID], []).

init([GameID]) ->
    Seed = rand:seed(exsss),
    {ok, #state{game_id = GameID, rand = Seed}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, <<Cmd>>, Req}, State) ->
    State1 = do_handle_forward(Cmd, Req, State),
    {noreply, State1};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_forward(?DISPLAY, _, #state{game_id = GameID} = State) ->
    ok = do_response(GameID, ready),
    State;
do_handle_forward(?ORDER, OrderBin, #state{game_id = GameID} = State) ->
    OrderStr = binary_to_list(OrderBin),
    %% Just do the order that they arrived in
    Num = lists:foldl(fun(Char, Count) when Char == $, -> Count + 1;
                         (_, Count) -> Count
                      end, 1, OrderStr),
    Response = misc:int_list_to_string(lists:seq(1, Num)),
    ok = do_response(GameID, Response),
    State;
do_handle_forward(?TARGET, TargetBin, #state{game_id = GameID} = State) ->
    Targets = misc:string_to_int_list(binary_to_list(TargetBin)),
    {TargetIdx, RState} = rand:uniform_s(length(Targets), State#state.rand),
    Target = lists:nth(TargetIdx, Targets),
    Response = integer_to_list(Target),
    ok = do_response(GameID, Response),
    State#state{rand = RState}.

do_response(GameID, ready) ->
    game_relay:respond(GameID, ready);
do_response(GameID, Response) ->
    game_relay:respond(GameID, list_to_binary(Response)).
