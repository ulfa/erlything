%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(switch_measurement_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

%%{"Licht","11111 2","1"}

handle_msg([Node ,Sensor, Id, Time, Optional, {Name, Device, Status}], Config, Module_config) ->
	lager:info("Device : ~p ,Name : ~p, Status : ~p, Time : ~p ", [Device, Name, Status, Time]),
	Table_Id = proplists:get_value(?TABLE, Config),
	[{data, Data}] = ets:lookup(Table_Id, data),	
	ets:insert(Table_Id, [{data, add(Data, {Device, Name, Status, Time})}]),
	Config;

%%
%% This function handles unknwon messages.
%%
handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got an unkown message with values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% i only add a new tuple to a list, when the status is different
%%[
%%{device_1, name_1, [{time, status}, {time, status}]},
%%{device_2, name_2, [{time, status}, {time, status}]}
%%]
add([], {Device, Name, "0", Time}) ->
	[];
add(Data, {Device, Name, Status, Time}) when is_binary(Time) ->
	add(Data, {Device, Name, Status, list_to_integer(binary_to_list(Time))}); 
add(Data, {Device, Name, Status, Time}) ->
	case lists:keysearch(Device, 1, Data) of 
		false -> add_new_device(Data, {Device, Name, Status, Time});
		{value, {Device_1, Name_1, Data_1}} ->  case is_status_equal(Status, Data_1) of 
													true -> Data;
													false -> lists:keyreplace(Device, 1, Data, {Device, Name, [{Time, Status}|Data_1]}) 
												end
	end.

add_new_device(Data, {Device, Name, "0", Time}) ->
	Data;
add_new_device(Data, {Device, Name, "1", Time}) ->
	[{Device, Name, [{Time, "1"}]}|Data].

is_status_equal(Status, []) ->
	false;
is_status_equal(Status, [{Time, Status}|Data]) ->
	true;
is_status_equal(Status, [{Time, Status_1}|Data]) ->
	false.
%%[
%%{device_1, name_1, [{time, status}, {time, status}]},
%%{device_2, name_2, [{time, status}, {time, status}]}
%%]
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
is_status_equal_test() ->
	?assertEqual(true, is_status_equal("0", [{"63549069666", "0"}])),
	?assertEqual(false, is_status_equal("0", [{"63549069666", "1"}])),
	?assertEqual(false, is_status_equal("0", [])).



add_test() ->
	?assertEqual([{"11111 2", "Licht", [{"63549069666", "1"}]}], add([{"11111 2", "Licht", [{"63549069666", "1"}]}], {"11111 1", "Wasser", "0", "63549069666"})),
	?assertEqual([], add([], {"11111 2", "Licht", "0", "63549069666"})),
	?assertEqual([{"11111 2", "Licht", [{"63549069666", "1"}]}], add([], {"11111 2", "Licht", "1", "63549069666"})),	
	?assertEqual([{"11111 2", "Licht", [{"63549069666", "1"}, {"63549069666", "0"}]}], add([{"11111 2", "Licht", [{"63549069666", "0"}]}], {"11111 2", "Licht", "1", "63549069666"})).
-endif.
