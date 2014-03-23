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
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, handle_msg/3]).

init(Config) ->
	os:cmd("mount -a -o nolock"), 
	{ok, Config}.

handle_msg([Node ,Sensor, Id, Time, ?ON], Config, Module_config) ->
	Path = config:get_value(path, Module_config, []),
	Last_shot = config:get_value(last_shot, Module_config, 0),
	Actual_time = date:get_date_seconds(),
	case check_time(Last_shot, Actual_time) of 
		true -> call_driver(Path, Actual_time),
				Module_config_1 = lists:keyreplace(last_shot, 1, Module_config, {last_shot, Actual_time}),
				lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1});
		false -> lager:info("time diff is too short"),
				 Config
	end;

%%
%% This function handles unknwon messages.
%%
handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got an unkown message with values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver([], Actual_time) ->
 	lager:error("you have to specify a path in your things.config");
call_driver(Path, Actual_time) ->
    os:cmd("streamer -o " ++ filename:join([Path, integer_to_list(Actual_time) ++ ".jpeg"])).

check_time(Last_shot, Actual_time) ->
	(Actual_time - Last_shot) > 5.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
