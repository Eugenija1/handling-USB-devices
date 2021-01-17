-module(utils).
-compile([export_all]).

print_devices(_, []) ->
    io:format("");

print_devices(Num, [H|T]) ->
    Params = [DeviceName || {_, DeviceName, _} <- [H]],
    io:format("~p. ~p~n", [Num] ++ Params),
    print_devices(Num+1, T).

print_files(_, []) ->
    io:format("");

print_files(Num, [H|T]) ->
    Params = [[FileName, FileExtension] || {_, FileName, FileExtension} <- [H]],
    io:format("~p. ~p.~p~n", [Num] ++ lists:flatten(Params)),
    print_files(Num+1, T).

print({gotoxy,X,Y}) ->
   io:format("\e[~p;~pH",[Y,X]).