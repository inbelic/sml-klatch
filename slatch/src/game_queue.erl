-module(game_queue).

%% server to handle match-making of queued clients and notify the start of a
%% new game to the game_relay

-behaviour(gen_server).

%% client_srvr API
-export([queue/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
        { queue = maps:new()  :: maps:maps(string(), {pid(), binary()})
        }).

%% APIs
-spec queue(string(), binary()) -> ok.
queue(Username, Config) ->
    gen_server:cast(?MODULE, {queue, Username, self(), Config}).

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

handle_cast({queue, Username, Pid, Config}, State) ->
    Queue = do_queue(Username, Pid, Config, State#state.queue),
    {noreply, State#state{queue = Queue}};

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, _State) ->
    ok.

%% Logic components
do_queue(Username, Pid, Config, Queue) ->
    case find_match(Config, Queue) of
        {match, {OppUsername, OppPid}} ->
            case rand:uniform(2) of
                1 ->
                    client_mgr:start_game(Pid, OppPid);
                2 ->
                    client_mgr:start_game(OppPid, Pid)
            end,
            maps:remove(OppUsername, Queue);
        nomatch ->
            maps:put(Username, {Pid, Config}, Queue)
    end.

find_match(Config, Queue) ->
    Fun = fun(Username, {OpPid, OpConfig}, nomatch) ->
                  Val = match_heuristic(Config, OpConfig),
                  {match, {Username, OpPid}, Val};
             (Username, {OpPid, OpConfig}, {match, _, BestVal} = Match) ->
                  Val = match_heuristic(Config, OpConfig),
                  case BestVal < Val of
                      false ->
                          Match;
                      true ->
                        {match, {Username, OpPid}, Val}
                  end
          end,
    case maps:fold(Fun, nomatch, Queue) of
        nomatch ->
            nomatch;
        {match, Match, _Val} ->
            {match, Match}
    end.

%% Can do whatever heuristic here to find opponents when the game has millions
%% of players to search upon ;) for now we just play against the only other
%% player on the server (most likely myself)
match_heuristic(_Config, _OpConfig) ->
    1.
