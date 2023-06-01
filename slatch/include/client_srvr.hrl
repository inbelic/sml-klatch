%% user_info record for ease of use
-record(user_info,
        { pid
        , rating
        , location
        }).

%% Encodings of the various commands from client
%% to server

%% OKAY and NOTOKAY
-define(OK, 0).
-define(NOTOKAY, 1).

%% In game commands (when client_srvr is in playing state)
-define(CONFIG, 2).
-define(TARGET, 3).
-define(ORDER, 4).

%% Lobby commands (when client_srvr is in lobby state)
-define(USERNAME, 5).
-define(QUEUE, 6).
-define(DISCONNECT, 7).
