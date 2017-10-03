%% This defines a server record. name is the atom that the server is registered to,
%% clients is a list of client records, channels is a list of channel names (strings)
-record(serv_st,{name,clients,channels}).

%% This defines a client record.
-record(cl_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    pid
}).

%% This defines a channel record. name is the name of the channel,
%% clients is a list of client records.
-record(ch_st,{name,clients}).
