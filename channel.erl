-module(channel).
-export([init/1,init/2,loop/2]).
-record(state,{clients}). %% clients är en lista med client-records
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

%% KOLLA SÅ ATT REQUEST KMR FRÅN SERVERN SOM SKAPADE DEN!!

init(Channel) ->
    init(Channel,[]).
init(Channel,Clients) ->
    genserver:start(list_to_atom(Channel),#state{clients = Clients},fun channel:loop/2).

loop(S = #state{clients = Clients},{Client = #client_st{},{nick,Nick}}) ->
    case lists:member(Client,Clients) of
        true ->
            NewClient = Client#client_st{nick = Nick},
            {reply,ok,S#state{clients = (Clients--[Client])++[NewClient]}};
        false ->
            {reply,no_such_client_in_channel,S}
    end;



loop(S = #state{clients = Clients},{join,Client}) ->
    case lists:member(Client,Clients) of
        true ->
            {reply,user_already_joined,S};
        false ->
            {reply,ok,S#state{clients = Clients++[Client]}}
    end;

loop(S = #state{clients = Clients},{leave,Client}) -> %% här blir det ngt galet emd records
    case lists:member(Client,Clients) of
        true ->
            {reply,ok,S#state{clients = Clients--[Client]}};
        false ->
            {reply,user_not_in_channel,S}
    end;

loop(S = #state{clients = Clients},{Client = #client_st{},{message_send,Channel,Msg}}) ->
    case lists:member(Client,Clients) of
        true ->
            [spawn(fun() -> client:handle(X, {message_receive, Channel, Client#client_st.nick, Msg}) end)|| X <- Clients, X /= Client],
            {reply,ok,S};
        false ->
            {reply,user_not_in_channel,S}
    end.
