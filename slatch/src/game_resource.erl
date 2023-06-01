-module(game_resource).

%% This server implements the logic to respond to the output of the messages
%% in the third {} of the game

-behaviour(gen_server).

%% API for serverside interactions
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { game_id
        , rand_state
        }).

%% API
forward(Pid, Msg, Type) ->
    gen_server:cast(Pid, {forward, Msg, Type}).

%% Startup
start(GameID) ->
    gen_server:start(?MODULE, [GameID], []).

start_link(GameID) ->
    gen_server:start_link(?MODULE, [GameID], []).

init([GameID]) ->
    Seed = rand:seed(exsss),
    {ok, #state{game_id = GameID, rand_state = Seed}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, Msg, Type}, State) ->
    State1 = do_handle_forward(Msg, Type, State),
    {noreply, State1};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_forward(_Msg, display, #state{game_id = GameID} = State) ->
    do_response(GameID, "ok"),
    State;
do_handle_forward(OrderStr, order, #state{game_id = GameID} = State) ->
    %% Just do the order that they arrived in
    Num = lists:foldl(fun(Char, Count) when Char == $, -> Count + 1;
                         (_, Count) -> Count
                      end, 1, OrderStr),
    Response = str_conv:int_list_to_string(lists:seq(1, Num)),
    do_response(GameID, Response),
    State;
do_handle_forward(Msg, random,
                  #state{game_id = GameID, rand_state = RState} = State) ->
    {_, TargetStr} = str_conv:strip_to_colon(Msg),
    Targets = str_conv:string_to_int_list(TargetStr),
    {TargetIdx, RState1} = rand:uniform_s(length(Targets), RState),
    Target = lists:nth(TargetIdx, Targets),
    Response = integer_to_list(Target),
    do_response(GameID, Response),
    State#state{rand_state = RState1}.

do_response(GameID, Response) ->
    client_mgr:respond(GameID, self(), list_to_binary(Response)).
