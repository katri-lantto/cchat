-module(channel).
-export([init/1,init/2,loop/2]).
-include_lib("./records.hrl"). % Load library of all the records.

%% Start a new channel process with no clients. 
init(Channel) ->
    init(Channel,[]).

%% Start a new channel process with clients given in the list Clients. 
init(Channel,Clients) ->
    genserver:start(list_to_atom(Channel),#ch_st{name = Channel,clients = Clients},fun channel:loop/2).

%% If this client is a member of the channel, change its nickname.
loop(S = #ch_st{clients = Clients},{Client = #cl_st{},{nick,Nick}}) ->
    case lists:member(Client,Clients) of
        true ->
            NewClient = Client#cl_st{nick = Nick},
            {reply,ok,S#ch_st{clients = (Clients--[Client])++[NewClient]}};
        false ->
            {reply,no_such_client_in_channel,S}
    end;

%% If this client is not a member of this channel, 
%% add it to the list of channel members.
loop(S = #ch_st{clients = Clients},{join,Client}) ->
    case lists:member(Client,Clients) of
        true ->
            {reply,user_already_joined,S};
        false ->
            {reply,ok,S#ch_st{clients = Clients++[Client]}}
    end;

%% If this client is a member of the channel, 
%% remove it from the list of channel members.
loop(S = #ch_st{clients = Clients},{leave,Client}) ->
    case lists:member(Client,Clients) of
        true ->
            {reply,ok,S#ch_st{clients = Clients--[Client]}};
        false ->
            {reply,user_not_joined,S}
    end;

%% The client wants to send a message to the channel. 
loop(S = #ch_st{name = Channel,clients = Clients},{Client = #cl_st{},{message_send,Msg}}) -> 
    case lists:member(Client,Clients) of % Check if client is a member of the channel.
        true ->
			% Send a message to all other member clients of the channel.
            [spawn(fun() -> client:handle(X, {message_receive, Channel, Client#cl_st.nick, Msg}) end)|| X <- Clients, X /= Client], 
            {reply,ok,S};
        false ->
            {reply,user_not_joined,S}
    end.
