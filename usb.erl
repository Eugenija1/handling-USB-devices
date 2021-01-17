-module(usb).
-compile([export_all]).
-import(utils, [print_devices/2, print_files/2, print/1]).

start() ->
    spawn(usb, menu, [self(), 1, []]),
    receive
        {ok, exit} ->
            io:format("Closing the simulator...~n"),
            timer:sleep(1000),
            {ok,start}
    end.
        
menu(MainPID, DevicesCnt, DevicesMap) ->
    io:format("
>>>>>>>>>> DEVICE MENU <<<<<<<<<<
_________________________________
| 1. Connected USB devices      |
| 2. Add new USB device         |
| 3. Manage devices             |
| 4. Remove device              |
| 5. Exit                       |
_________________________________

Choose action: ~n"),

    {ok, Choice} = io:read(""),
    if Choice == 1 ->
        show_connected_devices(MainPID, DevicesCnt, DevicesMap);
    Choice == 2 ->
        add_device(MainPID, DevicesCnt, DevicesMap);
    Choice == 3 ->
        manage_devices_menu(MainPID, DevicesCnt, DevicesMap);
    Choice == 4 ->
        remove_devices_menu(MainPID, DevicesCnt, DevicesMap);
    Choice == 5 ->
        MainPID ! {ok, exit};
    true ->
        io:format("There's no such action, please, try again.~n"),
        menu(MainPID, DevicesCnt, DevicesMap)
    end.

show_connected_devices(MainPID, DevicesCnt, DevicesMap) ->
    io:format(">>> CONNECTED DEVICES <<< ~n"),
    DevicesNum = length(DevicesMap),
    if DevicesNum == 0 ->
        io:format("There is no connected devices~n~n"),
        menu(MainPID, DevicesCnt, DevicesMap);
    true ->
        print_devices(1, DevicesMap),
        io:format("~n"),
        menu(MainPID, DevicesCnt, DevicesMap)
    end.

add_device(MainPID, DevicesCnt, DevicesMap) ->
    NewPID = spawn(usb, add_device_back, [DevicesCnt, DevicesMap]),
    NewPID ! {self(), adding_device},
    timer:sleep(250),
    io:format("~n'USB ~p' device was added succesfully! ~n", [DevicesCnt]),
    receive
        {ok, added, NewDevicesMap} ->
            menu(MainPID, DevicesCnt+1, NewDevicesMap)
    end.

add_device_back(DevicesCnt, DevicesMap) ->
    receive
        {Pid, adding_device} -> 
            timer:sleep(250),
            DeviceName = "USB " ++ integer_to_list(DevicesCnt), 
            timer:sleep(250),
            NewDeviceMap = updateDevicesMap(DevicesMap, DeviceName),
            Pid ! {ok, added, NewDeviceMap}
    end.

manage_devices_menu(MainPID, DevicesCnt, DevicesMap) ->
    io:format("~n>>>>>  DEVICE OPTIONS  <<<<<~n"),
    DevicesNum = length(DevicesMap),
    if DevicesNum == 0 ->
        io:format("No connected devices found~n"),
        menu(MainPID, DevicesCnt, DevicesMap);
    true ->
        print_devices(1, DevicesMap),
        io:format("0. Go back to start menu~n"),
        {ok, Choice} = io:read(""),
        if Choice == 0 ->
            menu(MainPID, DevicesCnt, DevicesMap);
        true ->
            PID = [ManagePID || {Idx, _, ManagePID} <- DevicesMap, Idx =:= Choice],
            FoundPID = length(PID),
            if FoundPID == 0 ->
                io:format("There's no such option, please, try again~n"),
                manage_devices_menu(MainPID, DevicesCnt, DevicesMap);
            true ->
                lists:nth(1, PID) ! {self(), menu}
            end
        end
    end,
    receive
        {ok, koniec} ->
            manage_devices_menu(MainPID, DevicesCnt, DevicesMap)
    end.

manage_devices(FilesMap) ->
    receive
        {DevicePID, menu} ->
            io:format("
>>>>>>> DEVICE OPTIONS <<<<<<<
_____________________________
| 1. Add file               |
| 2. Remove file            |
| 3. Print files            |
| 5. Go back to start menu  |
_____________________________

Choose action: ~n"),
            {ok, Choice} = io:read(""),
            if Choice == 1 ->
                NewFilesMap = add_file_menu(FilesMap),
                self() ! {DevicePID, menu},
                manage_devices(NewFilesMap);
            Choice == 2 ->
                remove_files_menu(DevicePID, FilesMap);
            Choice == 3 ->
                print_files_menu(FilesMap),
                self() ! {DevicePID, menu},
                manage_devices(FilesMap);
            Choice == 5 ->
                DevicePID ! {ok, koniec},
                manage_devices(FilesMap)
            end;
        {kill} ->
            timer:sleep(1)
    end.

add_file_menu(FilesMap) ->
    io:format(">>> adding file: ~n"),
    io:format("Enter file name: "),
    {ok, FileName} = io:read(""),
    io:format("Enter file extension: "),
    {ok, FileExtension} = io:read(""),
    timer:sleep(250),
    io:format("'~p' has been added~n", [FileName]),
    updateFilesMap(FilesMap, FileName, FileExtension).

print_files_menu(FilesMap) ->
    io:format(">>>>>  FILES ON THE DEVICE  <<<<<~n"),
    FilesNum = length(FilesMap),
    if FilesNum == 0 ->
        io:format("There is no files on the selected device~n~n");
    true ->
        print_files(1, FilesMap),
        io:format("~n")
    end.

remove_files_menu(DevicePID, FilesMap) ->
    io:format(">>> removing file: ~n"),
    NumOfFiles = length(FilesMap),
    if NumOfFiles == 0 ->
        io:format("There is no files to remove~n~n"),
        self() ! {DevicePID, menu},
        manage_devices(FilesMap);
    true ->
        print_files(1, FilesMap),
        io:format("0. Back to previous menu~n"),
        {ok, Choice} = io:read(""),
        if Choice == 0 ->
            DevicePID ! {DevicePID, menu},
            self() ! {DevicePID, menu},
            manage_devices(FilesMap);
        true ->
            FileID = [Idx || {Idx, _, _} <- FilesMap, Idx =:= Choice],
            FoundFileFlag = length(FileID),
            if FoundFileFlag == 0 ->
                io:format("Wrong option! Try again.~n"),
                remove_files_menu(DevicePID, FilesMap);
            true ->
                NewFilesMap = updateFilesMap(FilesMap, "REMOVING", "FILE", Choice),
                remove_files_menu(DevicePID, NewFilesMap)
            end
        end
    end.

remove_devices_menu(MainPID, DevicesCnt, DevicesMap) ->
    io:format(">>> removing device~n"),
    DevicesNum = length(DevicesMap),
    if DevicesNum == 0 ->
        io:format("There is no devices to remove~n~n"),
        menu(MainPID, DevicesCnt, DevicesMap);
    true ->
        print_devices(1, DevicesMap),
        io:format("0. Back to previous menu~n"),
        {ok, Choice} = io:read(""),
        if Choice == 0 ->
            menu(MainPID, DevicesCnt, DevicesMap);
        true ->    
            PID = [ManagePID || {Idx, _, ManagePID} <- DevicesMap, Idx =:= Choice],
            FoundPID = length(PID),
            if FoundPID == 0 ->
                io:format("Wrong option! Try again.~n"),
                remove_devices_menu(MainPID, DevicesCnt, DevicesMap);
            true ->
                lists:nth(1, PID) ! {kill},
                NewDevicesMap = updateDevicesMap(DevicesMap, "REMOVING", Choice),
                remove_devices_menu(MainPID, DevicesCnt, NewDevicesMap)
            end
        end
    end.

updateDevicesMap(DevicesMap, DeviceName) ->
    ManagePID = spawn(usb, manage_devices, [[]]),
    Size = length(DevicesMap),
    DevicesMap ++ [{Size+1, DeviceName, ManagePID}].

updateDevicesMap(DevicesMap, _, DeviceIndex) ->
    NewDevicesMap = [{K1, K2, K3} || {K1, K2, K3} <- DevicesMap, K1 =/= DeviceIndex],
    MapFirstPart = [{K1, K2, K3} || {K1, K2, K3} <- NewDevicesMap, K1 < DeviceIndex],
    MapSecondPart = [{K1-1, K2, K3} || {K1, K2, K3} <- NewDevicesMap, K1 > DeviceIndex],
    MapFirstPart ++ MapSecondPart.

updateFilesMap(FilesMap, FileName, FileExtension) ->
    Size = length(FilesMap),
    FilesMap ++ [{Size+1, FileName, FileExtension}].

updateFilesMap(FilesMap, _, _, FileIndex) ->
    NewFilesMap = [{K1, K2, K3} || {K1, K2, K3} <- FilesMap, K1 =/= FileIndex],
    MapFirstPart = [{K1, K2, K3} || {K1, K2, K3} <- FilesMap, K1 < FileIndex],
    MapSecondPart = [{K1-1, K2, K3} || {K1, K2, K3} <- NewFilesMap, K1 > FileIndex],
    MapFirstPart ++ MapSecondPart. 