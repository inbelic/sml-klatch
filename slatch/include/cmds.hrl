%% Here we define all the commands we expect to receive throughout the system

%% General server cmds
-define(OK, 0).
-define(INVALID, 1).
-define(STARTED, 2).

%% Lobby (client cmds)
-define(LOGIN, 3).
-define(QUEUE, 4).

%% Playing (client cmds)
-define(ORDER, 5).
-define(TARGET, 6).
-define(CONCEDE, 7).
