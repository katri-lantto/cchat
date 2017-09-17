-module(server).
-export([start/1,stop/1,loop/2]).
-record(state,{clients,channels}). %%channels är lista med namn på kanalerna
-record(client_st, {          %% clients lista  alla möjliga klienter (records)
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom,#state{clients = [],channels = []},fun server:loop/2).


%% varje channel ska vara en process
loop(S = #state{clients = Clients},{Client = #client_st{},connected}) ->
    {reply,Client,S#state{clients = Clients++[Client]}};

loop(S = #state{clients = Clients, channels = Channels},{Client = #client_st{},{nick,Nick}}) ->
    L = lists:filter(fun(X) -> X#client_st.nick == Nick end, Clients),
    case length(L) of
        0 ->
            NewClient = Client#client_st{nick = Nick},
            [spawn(fun() -> genserver:request(list_to_atom(Channel),{Client,{nick,Nick}}) end) || Channel <- Channels],
            {reply,ok,S#state{clients = (Clients--[Client])++[NewClient]}};
        _ ->
            {reply,nick_taken,S}
    end;





loop(S = #state{channels = Channels},{Client = #client_st{},{join, Channel}}) ->
    case lists:member(Channel,Channels) of
        true ->
            Result = genserver:request(list_to_atom(Channel),{join,Client}),
            {reply,Result,S};
        false ->
            channel:init(Channel,[Client]),
            {reply,ok,S#state{channels = Channels++[Channel]}}
    end;

loop(S = #state{channels = Channels},{Client = #client_st{},{leave,Channel}}) ->
    case lists:member(Channel, Channels) of
        true ->
            Result = genserver:request(list_to_atom(Channel),{leave,Client}),
            {reply,Result,S};
        false ->
            {reply,channel_doesnt_exit,S}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
