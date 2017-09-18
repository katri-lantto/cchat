-module(client).
-export([handle/2, initial_state/3]).
-include_lib("./records.hrl").



% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    Client = #cl_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    },
catch(genserver:request(ServerAtom,{Client,connected})).  %% skriv ngt mer här!!

    %% säg till servern att ny klient tillkommit

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    case catch(genserver:request(St#cl_st.server,{St,{join,Channel}})) of
	{'EXIT', Reason} -> {reply,{error,server_not_reached,"Server not responsive!"},St};
        ok -> {reply,ok,St};
        user_already_joined -> {reply,{error,user_already_joined,"You have already entered this channel"},St};
        _ -> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;
    %{reply, {error, not_implemented, "join not implemented"}, St} ;

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    Result = genserver:request(St#cl_st.server,{St,{leave,Channel}}),
    case Result of
        ok -> {reply,Result,St};
        user_not_joined -> {reply,{error,user_not_joined,"user is not in channel"},St};
        channel_doesnt_exit -> {reply,{error,channel_doesnt_exit,"channel does not exit"},St};
        _-> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) -> %% ska man kunna skicka meddelande trots server nere?
    % TODO: Implement this function
    % {reply, ok, St} ;
    case catch(genserver:request(list_to_atom(Channel),{St,{message_send,Msg}})) of
	    {'EXIT', Reason} -> {reply,{error,channel_not_reached,"Channel not responsive!"},St};
        ok -> {reply,ok,St};
        user_not_joined -> {reply,{error,user_not_joined,"user is not in channel"},St};
		_-> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;

% --------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#cl_st.nick, St};

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Result = genserver:request(St#cl_st.server,{St,{nick,NewNick}}),
    case Result of
        ok -> {reply, Result, St#cl_st{nick = NewNick}};
        nick_taken -> {reply,{error,Result,"Nick already taken!"},St};
		_-> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;

% Incoming message (from channel, to GUI)
handle(St = #cl_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) -> %%
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->

	% TODO säg till servern att nicket ledigt
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
