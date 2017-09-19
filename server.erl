-module(server).
-export([start/1,stop/1,loop/2]).
-include_lib("./records.hrl").

%%%********************************************
% stänger av severn, sätter på servern igen???
% meddela alla kanaler när klienten lämnar
% lämna en kanal när servern är nedstängd??
% byta nick när servern är nere?
% quit när servern är nere?
% servern nere men kanalerna lever?

%%***************************************


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom,#serv_st{name = ServerAtom,clients = [],channels = []},fun server:loop/2).


%% varje channel ska vara en process
loop(S = #serv_st{clients = Clients},{Client = #cl_st{},connected}) ->
    {reply,Client,S#serv_st{clients = Clients++[Client]}};

loop(S = #serv_st{clients = Clients, channels = Channels},{Client = #cl_st{},{nick,Nick}}) ->
    case lists:any(fun(X) -> X#cl_st.nick == Nick end, Clients) of
        false ->
            NewClient = Client#cl_st{nick = Nick},
            [spawn(fun() -> genserver:request(list_to_atom(Channel),{Client,{nick,Nick}}) end) || Channel <- Channels],
            {reply,ok,S#serv_st{clients = (Clients--[Client])++[NewClient]}};
        true ->
            {reply,nick_taken,S}
    end;

loop(S = #serv_st{channels = Channels},{Client = #cl_st{},{join, Channel}}) ->
    case lists:member(Channel,Channels) of
        true ->
            Result = genserver:request(list_to_atom(Channel),{join,Client}),
            {reply,Result,S};
        false ->
            channel:init(Channel,[Client]),
            {reply,ok,S#serv_st{channels = Channels++[Channel]}}
    end;

loop(S = #serv_st{channels = Channels},{Client = #cl_st{},{leave,Channel}}) ->
    case lists:member(Channel, Channels) of
        true ->
            Result = genserver:request(list_to_atom(Channel),{leave,Client}),
            {reply,Result,S};
        false ->
            {reply,channel_doesnt_exit,S}
    end;

loop(S = #serv_st{channels = Channels},{get_channels}) ->
	{reply,Channels,S}.


%% return serv_st of sever


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
	Channels = genserver:request(ServerAtom,{get_channels}),
	[spawn(fun() -> genserver:stop(list_to_atom(Channel)) end) || Channel <- Channels],
	genserver:stop(ServerAtom).
