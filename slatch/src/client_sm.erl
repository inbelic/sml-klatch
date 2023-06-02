-module(client_sm).

%% This finite state machine is used to model the state of the
%% interaction between the client and the client_srvr and is
%% used to deem if the client input is valid.

-behaviour(gen_statem).

-include("../include/cmds.hrl").

%% External APIs
-export([validate/2, request/2]).


%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([callback_mode/0, init/1]).

%% custom state_function gen_fsm callbacks
-export([login/3, lobby/3, queued/3, waiting/3, requesting/3]).

-define(is_byte(X), is_binary(X) andalso size(X) == 1).

%% API for client inputs
-spec validate(pid(), binary()) -> valid | invalid.
validate(Pid, Response) ->
    gen_statem:call(Pid, {client, Response}).

%% API for server inputs
-spec request(pid(), byte()) -> ok.
request(Pid, Cmd) when ?is_byte(Cmd) ->
     gen_statem:call(Pid, {server, Cmd}).

%% Startup
start() ->
    gen_statem:start(?MODULE, [], []).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

callback_mode() ->
    state_functions.

init([]) ->
    State = login,
    Data = {},
    {ok, State, Data}.

%% Waiting means that we are playing the game and waiting for the server to
%% give us our next request
waiting({call, From}, {server, Cmd}, _Data) when ?is_byte(Cmd) ->
    %% Store the Cmd so that we can use it to validate in requesting
    {next_state, requesting, Cmd, [{reply, From, ok}]};
waiting({call, From}, _, _) ->
    {keep_state_and_data, [{reply, From, invalid}]}.

%% Requesting means that we are waiting for our client to provide a valid
%% response the games request
requesting({call, From}, {client, Response}, Cmd) ->
    case is_valid_response(Cmd, Response) of
        true ->
            {next_state, waiting, ok, [{reply, From, valid}]};
        false ->
            {keep_state_and_data, [{reply, From, invalid}]}
    end;
requesting({call, From}, _, _) ->
    {keep_state_and_data, [{reply, From, invalid}]}.

%% TODO: implement it such that it will validate the formatting of the data
%% according to the haskell requirements, once those have been implemnted.
is_valid_response(_Cmd, _Response) ->
    true.

login({call, From}, {client, Username}, _Data) ->
    case is_valid_username(Username) of
        true ->
            {next_state, lobby, ok, [{reply, From, valid}]};
        false ->
            {keep_state_and_data, [{reply, From, invalid}]}
    end;
login({call, From}, _, _) ->
    {keep_state_and_data, [{reply, From, invalid}]}.

%% Can protect against offensive usernames, excessive length etc.
is_valid_username(_Username) ->
    true.

lobby({call, From}, {client, Config}, _Data) ->
    case is_valid_config(Config) of
        true ->
            {next_state, queued, ok, [{reply, From, valid}]};
        false ->
            {keep_state_and_data, [{reply, From, invalid}]}
    end;
lobby({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, invalid}]}.

queued({call, From}, {server, <<?STARTED>>}, _Data) ->
    {next_state, waiting, ok, [{reply, From, valid}]};
queued({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, invalid}]}.

%% Can check to ensure the config is in the correct format when that is decided
is_valid_config(_Config) ->
    true.
