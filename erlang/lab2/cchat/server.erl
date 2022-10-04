-module(server).
-export([start/1, stop/1]).

-record(state, {
    nicknames,
    channels
}).

% initial state of server
initialState() ->
    #state{
        nicknames = [],
        channels = []
    }.

-record(channelSt, {
    users,
    name
}).

% initial state of channel
initialChannelState(Name) ->
    #channelSt{
        users = [],
        name = Name
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    io:fwrite("Server started ~n"),
    io:fwrite("ServerAtom: ~p~n", [ServerAtom]),
    Pid = genserver:start(ServerAtom, initialState(), fun handle/2),
    io:fwrite("Pid: ~p~n", [Pid]).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).

handle(St, {join, Channel, Client}) ->
    % check if channel exists
    case lists:member(Channel, St#state.channels) of
        true ->
            Result = genserver:request(list_to_atom(Channel), {join, Client}),
            {reply, Result, St};
        false ->
            Pid = genserver:start(list_to_atom(Channel), initialChannelState(Channel), fun channelHandle/2),
            io:fwrite("Server: Created channel ~p with Pid ~p~n", [Channel, Pid]),
            Result = genserver:request(list_to_atom(Channel), {join, Client}),
            {reply, Result, St#state{channels = [Channel | St#state.channels]}}
    end;

% Catch all function for handling messages
handle(St, Data) ->
    io:fwrite("Server: Unknown command with data, ~p ~n", [Data]),
    {reply, unknown_command, St}.

channelHandle(St, {join, Client}) ->
    % check if user is already in channel
    case lists:member(Client, St#channelSt.users) of
        true ->
            {reply, user_already_joined, St};
        false ->
            io:fwrite("Channel ~p: ~p joined ~n", [St#channelSt.name, Client]),
            {reply, ok, St#channelSt{users = [Client | St#channelSt.users]}}
    end;

channelHandle(St, {leave, Client}) ->
    case lists:member(Client, St#channelSt.users) of
        true ->
            NewUsers = lists:delete(Client, St#channelSt.users),
            io:fwrite("Channel ~p: ~p left ~n", [St#channelSt.name, Client]),
            {reply, ok, St#channelSt{users = NewUsers}};
        false ->
            {reply, user_not_joined, St}

    end;

channelHandle(St, {message_send, Nick, Msg, Client}) ->
    case lists:member(Client, St#channelSt.users) of
        true ->
            io:fwrite("Channel ~p: ~p sent '~p: ~p' ~n", [St#channelSt.name, Client, Nick, Msg]),
            lists:foreach(fun(User) ->
                genserver:request(User, {message_receive, St#channelSt.name, Nick, Msg}), io:fwrite("sending to ~p", [User]) end, lists:delete(Client, St#channelSt.users)),
            {reply, ok, St};
        %genserver:request(Pid, {message_receive, Channel, Nick, Msg})
        false ->
            {reply, user_not_joined, St}

    end;


channelHandle(St, Data) ->
    io:fwrite("Channel ~p: Unknown command with data, ~p ~n", [St#channelSt.name, Data]),
    {reply, unknown_command, St}.


% cd erlang/lab2/cchat && erl -compile *.erl lib/*.erl && erl -noshell -eval "cchat:server()." -eval "cchat:client()."
