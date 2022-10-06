-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    chatroom
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        chatroom = []
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

    %case lists:member(Channel, St#client_st.chatroom) of
    %   true ->

    try genserver:request(St#client_st.server, {join, Channel, self()}) of
        Result ->
            case Result of
                ok ->
                    ChatRoomList = [Channel | St#client_st.chatroom],
                    {reply, ok, St#client_st{chatroom = ChatRoomList}};
                user_already_joined ->
                    {reply, {error, user_already_joined, "User already joined channel"}, St}
            end
    catch
        _:_ ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
% TODO: Implement this function
% {reply, ok, St} ;
    Result = genserver:request(list_to_atom(Channel), {leave, self()}),
    case Result of
        ok ->
            ChatRoomList = lists:delete(Channel, St#client_st.chatroom),
            {reply, ok, St#client_st{chatroom = ChatRoomList}};
        Error ->
            {reply, {error, Error, "User has not joined server or other error"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
% TODO: Implement this function
% {reply, ok, St} ;
    try genserver:request(list_to_atom(Channel), {message_send, St#client_st.nick, Msg, self()}) of
        Res ->
            case Res of
                ok ->
                    {reply, ok, St};
                Error ->
                    io:fwrite("Error: ~p~n", [Error]),
                    {reply, {error, Error, "Error sending message"}, St}
            end
    catch
        _:_ ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
% Any cleanup should happen here, but this is optional
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
