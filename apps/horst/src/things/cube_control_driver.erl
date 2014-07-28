%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(cube_control_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).


handle_msg([Node ,Sensor, Id, Time, [{temp, 0.0},{hum, 0.0}]=Body], Config, Module_config) ->
    lager:warning("~p got a message with incorrect values. message:~p", [?MODULE, [Node ,Sensor, Id, Time, Body]]),
    Config;

handle_msg([Node ,Sensor, Id, Time, [{temp, Temp},{hum, Hum}] = Body], Config, Module_config) ->
    lager:info("~p got a message with body : ~p : ", [?MODULE, Body]),
    Table_Id = proplists:get_value(?TABLE, Config),
    [{data, Data}] = ets:lookup(Table_Id, data),    
    ets:insert(Table_Id, [{data, add(Data, {Time, [{temp, Temp},{hum, Hum}]})}]),
    Config;

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
    lager:warning("~p got a message with i can't understand: message:~p", [?MODULE, [Node ,Sensor, Id, Time, Body]]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
control_heater(undefined, Threshold_hum, []) ->
    ok;

control_heater(Room_id, Threshold_hum, []) ->
    ok;

control_heater(Room_id, Threshold_hum, [{temp, Temp},{hum, Hum}]) ->
    ok.

get_room_id(Node, Mapping) ->
    proplists:get_value(Node, Mapping, undefined).

get_threshold(threshold, Mapping) ->
    proplists:get_value(threshold, Mapping, undefined).

add([], Value) ->
    [Value];
add(List, Value) ->
    case length(List) =< ?MAX_QUEUE_LENGTH of
        true -> [Value|List];
        false -> [Value|lists:sublist(List, ?MAX_QUEUE_LENGTH)]
    end.    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
