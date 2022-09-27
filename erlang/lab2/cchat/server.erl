-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, 0, handle),
    io:fwrite("Server started~n").

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).

handle(S, _) ->
    % TODO Implement function
    % Return ok
    io:fwrite("Recieved from client: ~p~n", [S]),
    ok.

