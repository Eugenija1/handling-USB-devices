-module(usb).
-compile([export_all]).
-import(utils, [print_devices/2, print_files/2, print/1]).
-import(filesUtils, [add_file/1, print_files/1, remove_file/2, addFiles/3, removeFile/2]).

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
| 4. Disconnect device          |
| 5. Exit                       |
_________________________________

Choose action: ~n"),

    {ok, Choice} = io:read(""),
    if Choice == 1 ->
        show_connected_devices(MainPID, DevicesCnt, DevicesMap);
    Choice == 2 ->
        add_device(MainPID, DevicesCnt, DevicesMap);
    Choice == 3 ->
        manage_devices(MainPID, DevicesCnt, DevicesMap);
    Choice == 4 ->
        disconnect(MainPID, DevicesCnt, DevicesMap);
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
    NewPID = spawn(usb, add_device_function, [DevicesCnt, DevicesMap]),
    NewPID ! {self(), adding_device}, % message {} is sent to the process NewPID
    timer:sleep(250),
    io:format("~n'USB ~p' device was added succesfully! ~n", [DevicesCnt]),
    receive
        {ok, added, NewDevicesMap} ->
            menu(MainPID, DevicesCnt+1, NewDevicesMap)
    end.

add_device_function(DevicesCnt, DevicesMap) ->
    receive
        {Pid, adding_device} -> 
            timer:sleep(250),
            DeviceName = "USB " ++ integer_to_list(DevicesCnt), 
            timer:sleep(250),
            NewDeviceMap = addDevice(DevicesMap, DeviceName),
            Pid ! {ok, added, NewDeviceMap}
    end.

manage_devices(MainPID, DevicesCnt, DevicesMap) ->
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
            IsFound = length(PID),
            if IsFound == 0 ->
                io:format("There's no such option, please, try again~n"),
                manage_devices(MainPID, DevicesCnt, DevicesMap);
            true ->
                lists:nth(1, PID) ! {self(), menu}
            end
        end
    end,
    receive
        {ok, koniec} ->
            manage_devices(MainPID, DevicesCnt, DevicesMap)
    end.

manage_menu(FilesMap) ->
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
                NewFilesMap = add_file(FilesMap),
                self() ! {DevicePID, menu},
                manage_menu(NewFilesMap);
            Choice == 2 ->
                remove_file(DevicePID, FilesMap);
            Choice == 3 ->
                print_files(FilesMap),
                self() ! {DevicePID, menu},
                manage_menu(FilesMap);
            Choice == 5 ->
                DevicePID ! {ok, koniec},
                manage_menu(FilesMap)
            end;
        {kill} ->
            timer:sleep(1)
    end.

disconnect(MainPID, DevicesCnt, DevicesMap) ->
    io:format(">>> disconnecting device~n"),
    DevicesNum = length(DevicesMap),
    if DevicesNum == 0 ->
        io:format("There are no connected devices~n"),
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
                disconnect(MainPID, DevicesCnt, DevicesMap);
            true ->
                io:format("Device disconected succesfully!~n"),
                lists:nth(1, PID) ! {kill},
                NewDevicesMap = removeDevice(DevicesMap, Choice),
                disconnect(MainPID, DevicesCnt, NewDevicesMap)
            end
        end
    end.

addDevice(DevicesMap, DeviceName) ->
    ManagePID = spawn(usb, manage_menu, [[]]),
    Size = length(DevicesMap),
    DevicesMap ++ [{Size+1, DeviceName, ManagePID}].

removeDevice(DevicesMap, DeviceIndex) ->
    NewDevicesMap = [{K1, K2, K3} || {K1, K2, K3} <- DevicesMap, K1 =/= DeviceIndex],
    MapFirstPart = [{K1, K2, K3} || {K1, K2, K3} <- NewDevicesMap, K1 < DeviceIndex],
    MapSecondPart = [{K1-1, K2, K3} || {K1, K2, K3} <- NewDevicesMap, K1 > DeviceIndex],
    MapFirstPart ++ MapSecondPart.