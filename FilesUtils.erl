-module(filesUtils).
-compile([export_all]).
-import(utils, [print_files/2]).
-import(usb, [manage_menu/1]).

add_file(FilesMap) ->
    io:format(">>> adding file: ~n"),
    io:format("Enter file name: "),
    {ok, FileName} = io:read(""),
    io:format("Enter file extension: "),
    {ok, FileExtension} = io:read(""),
    timer:sleep(250),
    io:format("'~p' has been added~n", [FileName]),
    addFiles(FilesMap, FileName, FileExtension).

print_files(FilesMap) ->
    io:format(">>>>>  files on device: ~n"),
    if length(FilesMap) == 0 ->
        io:format("Device is empty!~n");
    true ->
        print_files(1, FilesMap),
        io:format("~n")
    end.

remove_file(DevicePID, FilesMap) ->
    io:format(">>> removing file: ~n"),
    if length(FilesMap) == 0 ->
        io:format("Device is empty!~n"),
        self() ! {DevicePID, menu},
        manage_menu(FilesMap);
    true ->
        print_files(1, FilesMap),
        io:format("0. Back to previous menu~n"),
        {ok, Choice} = io:read(""),
        if Choice == 0 ->
            DevicePID ! {DevicePID, menu},
            self() ! {DevicePID, menu},
            manage_menu(FilesMap);
        true ->
            FileID = [Idx || {Idx, _, _} <- FilesMap, Idx =:= Choice],
            if length(FileID) == 0 ->
                io:format("There's no such option, please, try again~n"),
                remove_file(DevicePID, FilesMap);
            true ->
                io:format("File removed succesfully!~n"),
                NewFilesMap = removeFile(FilesMap, Choice),
                DevicePID ! {DevicePID, menu},
                self() ! {DevicePID, menu},
                manage_menu(NewFilesMap)
            end
        end
    end.

addFiles(FilesMap, FileName, FileExtension) ->
    Size = length(FilesMap),
    FilesMap ++ [{Size+1, FileName, FileExtension}].

removeFile(FilesMap, FileIndex) ->
    NewFilesMap = [{K1, K2, K3} || {K1, K2, K3} <- FilesMap, K1 =/= FileIndex],
    MapFirstPart = [{K1, K2, K3} || {K1, K2, K3} <- FilesMap, K1 < FileIndex],
    MapSecondPart = [{K1-1, K2, K3} || {K1, K2, K3} <- NewFilesMap, K1 > FileIndex],
    MapFirstPart ++ MapSecondPart. 