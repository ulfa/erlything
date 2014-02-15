%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(fungi_driver).

%% --------------------------------------------------------------------
%% defines
%% --------------------------------------------------------------------
-define(WINDOW_CLOSE, "closed").
-define(WINDOW_OPEN, "open").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

%% 
%% Here we handle the messages from the dht22 sensor
%%
handle_msg([Node ,Sensor, Id, Time, [{temp, 0.0}, {hum, 0.0}]] = Msg, Config, Module_config) ->
    Config;
handle_msg([Node ,Sensor, Id, Time, [{temp, Temp}, {hum, Hum}]] = Msg, Config, Module_config) ->
    lager:info("~p got a message with values: ~p", [?MODULE, Msg]),
    {Room, Room_id} = get_room(binary_to_list(Node), Module_config),
    Hum_max = get_config(hum_max, Module_config),
    Window_state = get_data(window, Module_config),
    Temp_max = get_config(temp_max, Module_config),
    Temp_min = get_config(temp_min, Module_config),
    handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config),
    handle_temp(Room, Temp, Temp_min, Temp_max, Window_state, Config, Module_config),
    Config;
%%
%% Here we handle the window state from the cuberl. 
%%
handle_msg([Node ,<<"cube_driver">>, Id, Time, {window_state, Body}] = Msg, Config, Module_config) ->
    lager:info("~p got a message with values: ~p", [?MODULE, Msg]),
    Window_state = get_data(window, Module_config),
    Window_state_new = get_value(window_state, Body),
    Room_id = get_value(room_id, Body),
    Room_name = get_value(room_name, Body),
    handle_window(Window_state, Window_state_new, Room_id, Room_name, Config, Module_config);
%%
%% This function handles unknwon messages.
%%
handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got an unkown message with values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%
%% If the act temp in the room < Temp_min then increase the heater, but
%% only if the window state is closed. If the window is open, we don't
%% need tp heat the room.
%%
handle_temp(Room, Actual_temp, Temp_min, Temp_max, ?WINDOW_CLOSE, Config, Module_config) when Actual_temp =< Temp_min ->
    lager:info("we have to heat room : ~p, because the temp is too cold", [Room]),
    Config;
handle_temp(Room, Actual_temp, Temp_min, Temp_max, Window_state, Config, Module_config) ->
    lager:info("we have an actual temp : ~p in the room : ~p.",[Actual_temp, Room]),
    Config.
    
%%
%% When the hum > max and window is closed then open the window
%%
handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config) when Hum >= Hum_max ->
    lager:info("~p sends a message 'open window in room : ~p", [?MODULE, Room]),
    send_msg_to_human(Room, Window_state, ?WINDOW_OPEN, Config, Module_config),
    Config;
%%
%% When the hum < max and the window is open then close the window
%%
handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config) when Window_state == ?WINDOW_OPEN->
    lager:info("~p sends a message 'close window in room : ~p", [?MODULE, Room]),
    send_msg_to_human(Room, Window_state, ?WINDOW_CLOSE, Config, Module_config),    
    Config;
handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config) ->
    lager:info("~p send no message  room : ~p, ~p", [?MODULE, Room, Window_state]),
    Config.
%%
%% Here we handle the message from the cuberl. (window state)
%%
handle_window(Window_state_new, Window_state_new, Room_id, Room_name,Config, Module_config) ->
    Config;

handle_window(Window_state, ?WINDOW_CLOSE, Room_id, Room_name,Config, Module_config) ->
    set_data(window, ?WINDOW_CLOSE, Config, Module_config);

handle_window(Window_state, ?WINDOW_OPEN, Room_id, Room_name,Config, Module_config) ->
    set_data(window, ?WINDOW_OPEN, Config, Module_config).
%%
%% Here we handle the message from the cuberl. (temp state)
%%
handle_msg_intern([{}],Config, Module_config) ->
    Config.
%%
%% Here we send a message to the boxcar
%%
send_msg_to_human(Room, Window_state, Window_state, Config, Module_config) ->
    lager:info("~p sends nothing because the window is already in state : ~p", [?MODULE, Window_state]),
    Config;
send_msg_to_human(Room, Window_state, Window_state_new, Config, Module_config) ->
    Msg = sensor:create_message(node(), ?MODULE, [{account, "NmEAW2euRjuUGesV58n"},{title, Window_state_new ++ " the window in room : " ++ Room }, {message, "see title"}, {sound, "digital-alarm"}]),
    sensor:send_message(Msg),  
    Config.
%%
%% If the temp in the observed room is < 16 degrees then is increase the temp
%%
send_msg_to_cuberl(Room, Temp, Config, Module_config) ->
    Config.

get_config(Key, Module_config) -> 
    Config = get_value(config, Module_config), 
    get_value(Key, Config). 

get_data(Key, Module_config) ->
    Data = get_value(data, Module_config), 
    get_value(Key, Data).     

get_room(Key, Module_config) ->
    Config = get_value(config, Module_config), 
    Rooms = get_value(rooms, Config),
    {Room_name, Room_id} = get_value(Key,Rooms).

set_data(Key, Value, Config, Module_config) ->
    Data = get_value(data, Module_config),
    New_data = lists:keyreplace(Key, 1, Data, {Key, Value}), 
    Module_config_1 = lists:keyreplace(data, 1, Module_config, {data, New_data}),
    lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1}).

get_value(Key, List) ->
    proplists:get_value(Key, List). 
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_value_test() ->
    ?assertEqual("Büro", get_config(room, [{config, [{room, "Büro"}, {test, "Test"}]}])),
    ?assertEqual(undefined, get_config(unknwon, [{config, [{room, "Büro"}, {test, "Test"}]}])).

set_data_test() ->
    Config = 
     [{driver, {fungi_driver,handle_msg},
        [{data,[{window,close}, {lastaction, {time, action}}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}]}],

    Config_result = 
     [{driver, {fungi_driver,handle_msg},
        [{data,[{window,open}, {lastaction, {time, action}}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}]}],

    {driver, {_Module, _Func}, Module_config} = lists:keyfind(driver, 1, Config),
    ?assertEqual(Config_result, set_data(window, open, Config, Module_config)).

get_room_test() ->
    Config = [{config,
               [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                         {"horst@erwin", {"Wohnzimmer", 2}}]},
                {"horst@raspberrypi","Büro"},
                {hum_max,"60.0"},
                {temp_min,"16.0"},
                {temp_max,"21.0"}]}],
    ?assertEqual({"Wohnzimmer", 2}, get_room("horst@erwin", Config)).


-endif.
