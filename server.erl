-module(server).
-export([start/1,stop/1,loop/2]).
-include_lib("./records.hrl"). % Load library with all the records.

%% Start a new server process with the given name.
start(ServerAtom) ->
    genserver:start(ServerAtom,#serv_st{name = ServerAtom,clients = [],channels = []},fun server:loop/2).

%% Connects client to server.
loop(S = #serv_st{clients = Clients},{Client = #cl_st{},connected}) -> 
    {reply,Client,S#serv_st{clients = Clients++[Client]}};

%% Disconnects client from server.
loop(S = #serv_st{clients = Clients, channels = Channels},{Client = #cl_st{},disconnected}) ->
	[spawn(fun() -> genserver:request(list_to_atom(Channel),{leave,Client}) end) || Channel <- Channels], % Notifies all channels that client has disconnected.
	{reply,Client,S#serv_st{clients = Clients--[Client]}};

%% Change clients nickname.
loop(S = #serv_st{clients = Clients, channels = Channels},{Client = #cl_st{},{nick,Nick}}) ->
    case lists:any(fun(X) -> X#cl_st.nick == Nick end, Clients) of % Checks if nickname is already taken.
        false ->
            NewClient = Client#cl_st{nick = Nick}, % Creates a new client record with the new nickname.
            [spawn(fun() -> genserver:request(list_to_atom(Channel),{Client,{nick,Nick}}) end) || Channel <- Channels], % Notifies all channels that the client has changed nickname.
            {reply,ok,S#serv_st{clients = (Clients--[Client])++[NewClient]}}; % Replace the old client with the new client.
        true ->
            {reply,nick_taken,S}
    end;

%% Joins a channel.
loop(S = #serv_st{channels = Channels},{Client = #cl_st{},{join, Channel}}) ->
    case lists:member(Channel,Channels) of % Checks if channel exists.
        true ->
            Result = genserver:request(list_to_atom(Channel),{join,Client}), % Tell the channel that the client wants to join.
            {reply,Result,S};
        false ->
            channel:init(Channel,[Client]), % If channel does not exist, create it with this client joined.
            {reply,ok,S#serv_st{channels = Channels++[Channel]}}
    end;

%% Leaves a channel.
loop(S = #serv_st{channels = Channels},{Client = #cl_st{},{leave,Channel}}) -> 
    case lists:member(Channel, Channels) of % Checks if channel exists.
        true ->
            Result = genserver:request(list_to_atom(Channel),{leave,Client}), % Tell the channel that the client wants to leave.
            {reply,Result,S};
        false ->
            {reply,channel_doesnt_exit,S} % Replies that channel does not exist.
    end;

%% Returns the channels in this server
loop(S = #serv_st{channels = Channels},{get_channels}) ->
	{reply,Channels,S}.

%% Stop the server process registered to the given name, together with all the channels.
%% This implementation will make our program the last test, but after discussion with 
%% the TAs we came to the conclusion that it is ok to do this.
stop(ServerAtom) ->
	Channels = genserver:request(ServerAtom,{get_channels}),
	[spawn(fun() -> genserver:stop(list_to_atom(Channel)) end) || Channel <- Channels],
	genserver:stop(ServerAtom).
