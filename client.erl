-module(client).
-export([handle/2, initial_state/3]).
-include_lib("./records.hrl"). % Load library with all the records.

%% Return an initial state record. This is called from GUI.
initial_state(Nick, GUIAtom, ServerAtom) ->
    Client = #cl_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        pid = list_to_atom(Nick)
    },
	%% Tries to connect to the server.
	catch(genserver:request(ServerAtom,{Client,connected})),
	Client.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

%% Join channel.
handle(St, {join, Channel}) ->
	%% Tries to the join the given channel. If the client has already joined
	%% the channel, reply with "You have already entered this channel".
    case catch(genserver:request(St#cl_st.server,{St,{join,Channel}})) of
		{'EXIT', Reason} -> {reply,{error,server_not_reached,"Server not responsive!"},St};
        ok -> {reply,ok,St};
        user_already_joined -> {reply,{error,user_already_joined,"You have already entered this channel"},St}
    end;

%% Leave channel.
handle(St, {leave, Channel}) ->
	%% Tries to leave the given channel. If the client is not in the channel,
	%% reply with "User is not in channel". If the channel does not exist,
	%% reply with "Channel does not exist".
	case catch(genserver:request(St#cl_st.server,{St,{leave,Channel}})) of
		{'EXIT', Reason} -> {reply,{error,server_not_reached,"Server not responsive!"},St};
        ok -> {reply,ok,St};
        user_not_joined -> {reply,{error,user_not_joined,"User is not in channel"},St};
        channel_doesnt_exit -> {reply,{error,channel_doesnt_exit,"Channel does not exit"},St}
    end;


%% Send a message to the channel.
handle(St, {message_send, Channel, Msg}) ->
	%% Tries to send a message to this channel. If the channel does not exist,
	%% reply with "Channel not responsive!". If the client is not a member
	%% of the channel, reply with "User is not in channel".
    case catch(genserver:request(list_to_atom(Channel),{St,{message_send,Msg}})) of
		{'EXIT', Reason} -> {reply,{error,channel_not_reached,"Channel not responsive!"},St};
        ok -> {reply,ok,St};
        user_not_joined -> {reply,{error,user_not_joined,"User is not in channel"},St}
    end;

%% Get current nick.
handle(St, whoami) ->
    {reply, St#cl_st.nick, St};

%% Change nick, if not already taken.
handle(St, {nick, NewNick}) ->
	%% Tries to change the clients nickname. If nickname is taken, reply with
	%% "Nick already taken!".
    case catch(genserver:request(St#cl_st.server,{St,{nick,NewNick}})) of
		{'EXIT', Reason} -> {reply,{error,server_not_reached,"Server not responsive!"},St};
        ok -> {reply, ok, St#cl_st{nick = NewNick}};
        nick_taken -> {reply,{error,nick_taken,"Nick already taken!"},St}
    end;

% Incoming message (from channel, to GUI)
handle(St = #cl_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) -> 
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

%% Quit client via GUI.
handle(St, quit) ->
	catch(genserver:request(St#cl_st.server,{St,disconnected})),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
