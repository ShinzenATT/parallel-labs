-module(helloworld).
-author("mukaca").

%% API
-compile(export_all).


f() ->
  io:format("hello world~n").

