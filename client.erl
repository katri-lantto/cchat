-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server %% ha kanske ett fält för aktuell kanal?
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

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
    Result = genserver:request(shire,{St,{join,Channel}}),
    case Result of
        ok -> {reply,Result,St};
        user_already_joined -> {reply,{error,Result,"You have already entered this channel"},St};
        _ -> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;
    %{reply, {error, not_implemented, "join not implemented"}, St} ;

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    Result = genserver:request(shire,{St,{leave,Channel}}),
    case Result of
        ok -> {reply,Result,St};
        user_not_joined -> {reply,{error,Result,"user is not in channel"},St};
        channel_doesnt_exit -> {reply,{error,Result,"channel does not exit"},St};
        _-> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;

    Result = genserver:request(shire,{St,{message_send,Channel,Msg}}),
    case Result of
        ok -> {reply,Result,st};
        user_not_joined -> {reply,{error,Result,"user is not in channel"},St};
        channel_doesnt_exit -> {reply,{error,Result,"channel does not exit"},St};
        _-> {reply,{error,something_went_wrong,"something went wrong"},St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:format("kmr vi hit"),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
