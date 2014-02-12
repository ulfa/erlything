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
-define(WINDOW_CLOSE, "close").
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
handle_msg([Node ,Sensor, Id, Time, [{temp, Temp}, {hum, Hum}]], Config, Module_config) ->
    Room = get_config(room, Module_config),
    Hum_max = get_config(hum_max, Module_config),
    Config;

handle_msg([Node ,'cube_driver', Id, Time, Body], Config, Module_config) ->
    handle_msg(Body, Config, Module_config);

handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got a message with incorrect values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%
%% When the hum > max and window is closed then open the window
%%
handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config) when Hum >= Hum_max ->
    send_msg_to_human(Room, ?WINDOW_OPEN, Config, Module_config),
    Config;
%%
%% When the hum < max and window is open then close the window
%%
handle_hum(Room, Hum, Hum_max, Window_state, Config, Module_config) ->
    send_msg_to_human(Room, ?WINDOW_CLOSE, Config, Module_config),
    Config.
%%
%% Here we handle the message from the cuberl. (window state)
%%
handle_msg_intern([{}],Config, Module_config) ->
    Config;
%%
%% Here we handle the message from the cuberl. (temp state)
%%
handle_msg_intern([{}],Config, Module_config) ->
    Config.
%%
%% Here we send a message to the boxcar
%%
send_msg_to_human(Room, Window_state, Config, Module_config) ->
    Config.
%%
%% If the temp in the observed room is < 16 degrees then is increase the temp
%%
send_msg_to_cuberl(Room, Temp, Config, Module_config) ->
    Config.

get_config(Key, Module_config) -> 
    Config = proplists:get_value(config, Module_config), 
    proplists:get_value(Key, Config). 

get_data(Key, Module_config) ->
    Data = proplists:get_value(data, Module_config), 
    proplists:get_value(Key, Data).     

set_data(Key, Value, Module_config) ->
    Data = proplists:get_value(data, Module_config), 
    New_data = lists:keyreplace(Key, 1, Data, {Key, Value}), 
    lists:keyreplace(data, 1, Module_config, {data, New_data}). 
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_value_test() ->
    ?assertEqual("Büro", get_config(room, [{config, [{room, "Büro"}, {test, "Test"}]}])),
    ?assertEqual(undefined, get_config(unknwon, [{config, [{room, "Büro"}, {test, "Test"}]}])).

set_data_test() ->
    Module_config = [{config, [{room, "Büro"}]}, {data, [{window, closed}]}],
    ?assertEqual([{config, [{room, "Büro"}]}, {data, [{window, open}]}], set_data(window, open, Module_config)).
-endif.
