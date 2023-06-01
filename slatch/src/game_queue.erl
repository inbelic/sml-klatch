-module(game_queue).

%% server to handle match-making of queued clients and notify the start of a
%% new game to the client_mgr

-behaviour(gen_server).

-include("../include/client_srvr.hrl").

%% client_srvr API
-export([queue/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
        { queue = maps:new()
        }).

%% APIs
queue(Username, Info) ->
    gen_server:cast(?MODULE, {queue, Username, Info}).

%% Startup
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server exports
init([]) ->
    register(?MODULE, self()),
    {ok, #state{}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({queue, Username, Info}, State) ->
    Queue = do_queue(Username, Info, State#state.queue),
    {noreply, State#state{queue = Queue}};

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
    ok.

%% Logic components
do_queue(Username, Info, Queue) ->
    case find_match(Info, Queue) of
        {match, {OppUsername, OppPid}} ->
            case rand:uniform(2) of
                1 ->
                    client_mgr:start_game(Info#user_info.pid, OppPid);
                2 ->
                    client_mgr:start_game(OppPid, Info#user_info.pid)
            end,
            maps:remove(OppUsername, Queue);
        nomatch ->
            maps:put(Username, Info, Queue)
    end.

%% Can do whatever heuristic here to find opponents when the game has millions
%% of players to search upon ;) for now we just play against the only other
%% player on the server
find_match(Info, Queue) ->
    Fun = fun(Username, OpInfo, nomatch) ->
                  Val = match_heuristic(Info, OpInfo),
                  {match, {Username, OpInfo#user_info.pid}, Val};
             (Username, OpInfo, {match, _, BestVal} = Match) ->
                  Val = match_heuristic(Info, OpInfo),
                  case BestVal < Val of
                      false ->
                          Match;
                      true ->
                        {match, {Username, OpInfo#user_info.pid}, Val}
                  end
          end,
    case maps:fold(Fun, nomatch, Queue) of
        nomatch ->
            nomatch;
        {match, Match, _Val} ->
            {match, Match}
    end.

match_heuristic(_Info, _PotentialOp) ->
    1.
