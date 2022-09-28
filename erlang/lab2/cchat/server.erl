-module(server).
-export([start/1,stop/1]).

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

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    io:fwrite("Server started ~n"),
    io:fwrite("ServerAtom: ~p~n", [ServerAtom]),
    Pid = genserver:start(ServerAtom, initialState(), handle),
    io:fwrite("Pid: ~p~n", [Pid]),
    io:fwrite(whereis(ServerAtom)).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).

handle(_, _) ->
    % TODO Implement function
    % Return ok
    io:fwrite("Recieved from client ~n"),
    ok.

