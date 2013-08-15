%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(usb_cam_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, "RISING"], Config, Module_config) ->
	Path = proplists:get_value(path, Module_config, []),
	Last_shot = proplists:get_value(last_shot, Module_config, 0),
	Actual_time = date:get_date_seconds(),
	case check_time(Last_shot, Actual_time) of 
		true -> call_driver(Path, Actual_time),
				Module_config_1 = lists:keyreplace(last_shot, 1, Module_config, {last_shot, Actual_time}),
				lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1});
		false -> lager:info("time diff is too short"),
				 Config
	end;

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver([], Actual_time) ->
 	lager:error("you have to specify a path in your things.config");
call_driver(Path, Actual_time) ->
    os:cmd("streamer -o " ++ filename:join([Path, integer_to_list(Actual_time) ++ ".jpeg"])).

check_time(Last_shot, Actual_time) ->
	(Actual_time - Last_shot) > 60.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
handle_msg_test() ->
	true.
-endif.
