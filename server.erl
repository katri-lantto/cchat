-module(server).
-export([start/1,stop/1,loop/2]).
-record(state,{channels}). %% borde väl ha mer än channels??
-record(client_st, {
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
    genserver:start(ServerAtom,#state{channels = dict:new()},fun server:loop/2).


%% varje channel ska vara en process

loop(S = #state{channels = Channels},{Client = #client_st{},{join, Channel}}) ->
    case dict:find(Channel,Channels) of
        {ok, List} ->
            case lists:member(Client,List) of
                true ->
                    {reply,user_already_joined,S};
                false ->
                    NewChannelDict = dict:store(Channel,List++[Client],Channels),
                    {reply,ok,S#state{channels = NewChannelDict}}
                end;
        _ ->
            NewChannelDict = dict:store(Channel,[Client], Channels),
            {reply,ok,S#state{channels = NewChannelDict}}
    end;

loop(S = #state{channels = Channels},{Client = #client_st{},{leave,Channel}}) ->
    case dict:find(Channel, Channels) of
        {ok,List} ->
            case lists:member(Client,List) of
                true ->
                    NewList = lists:delete(Client,List),
                    NewChannelDict = dict:store(Channel,NewList,Channels),
                    {reply,ok,S#state{channels = NewChannelDict}};
                false ->
                    {reply,user_not_joined,S}
            end;
        _ ->
            {reply,channel_doesnt_exit,S}
    end;

loop(S = #state{channels = Channels},{Client = #client_st{},{message_send,Channel,Msg}}) ->
    case dict:find(Channel, Channels) of
        {ok,List} ->
            case lists:member(Client,List) of
                true ->
                    [spawn(fun() -> client:handle(X, {message_receive, Channel, Client#client_st.nick, Msg}) end)|| X <- List, X /= Client],
                    {reply,ok,S};
                false ->
                    {reply,user_not_joined,S}
            end;
        _ ->
            {reply,channel_doesnt_exit,S}
    end.










% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
