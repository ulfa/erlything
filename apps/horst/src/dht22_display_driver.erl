%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(dht22_display_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
-define(MAX_QUEUE_LENGTH, 19).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, [{temp, 0.0},{hum, 0.0}]], Config, Module_config) ->
	lager:warning("dht22_display_driver got a message with incorrect values: ~p", [[Node ,Sensor, Id, Time, [{temp, 0.0},{hum, 0.0}]]]),
	Config;

handle_msg([Node ,Sensor, Id, Time, [{temp, Temp},{hum, Hum}]], Config, Module_config) ->
	Table_Id = proplists:get_value(?TABLE, Config),
	[{data, Data}] = ets:lookup(Table_Id, data),	
	ets:insert(Table_Id, [{data, add(Data, {Time, [{temp, Temp},{hum, Hum}]})}]),
	Config;

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	lager:warning("dht22_display_driver got the wrong message : ~p", [[Node ,Sensor, Id, Time, Body]]),
	Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
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
handle_msg_test() ->
	Config = [{driver, {dht22_display_driver, handle_msg}, [{data, []}]}],
	Config_1 = [{driver, {dht22_display_driver, handle_msg}, [{data, [{"1", "2"}]}]}],
	?assertEqual(Config_1, handle_msg(["Node", "Sensor", "Id", "1", "2"], Config, [{data, []}])).
-endif.
