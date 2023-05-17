-module(client_mgr).

%% API for responding to the Haskell game management
-export([forward/1]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
        { client_map = maps:new()
        }).

%% API
forward(Bin) ->
    gen_server:cast(client_mgr, {forward, Bin}).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    register(client_mgr, self()),
    {ok, #state{}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

handle_cast({forward, Bin}, State) ->
    io:format("~p~n", [binary_to_list(Bin)]),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {stop, unknown_cast, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
        ok.
