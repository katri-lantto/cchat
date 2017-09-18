-module(cchat).
-export([server/0,client/0, stop/0]).
-define(SERVERNAME,shire).

% Start a server
server() ->
    server:start(?SERVERNAME).

% Start a client GUI
client() ->
    gui:start(?SERVERNAME).

% stop a server
stop() ->
	server:stop(?SERVERNAME).
