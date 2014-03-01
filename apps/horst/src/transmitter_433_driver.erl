%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(transmitter_433_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, _Id, Time, {Switch, Number, Status}] = Msg, Config, Module_config) ->
	lager:info("~p got a message with values: ~p. ", [?MODULE, Msg]),
	case is_valid_device(Number, Module_config) of 
		false -> lager:info("Device : ~p is not a valid device for this node", [Number]),
				 Config;
		_ -> call_driver(Number, Status),
			 Table_Id = proplists:get_value(?TABLE, Config),
			 Data = ets:tab2list(Table_Id), 	
			 Data1 = lists:keyreplace(Switch, 1 , Data, {Switch, Number, Status}),
			 ets:insert(Table_Id, Data1),
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
is_valid_device(Number, Module_config) ->
	lists:keysearch(Number, 2, Module_config).

call_driver(Switch, Status) ->
	lager:debug("switch state from : ~p to : ~p", [Switch, Status]),
    Driver = filename:join([code:priv_dir(horst), "driver", "remote", "send "]),
    Command=Driver ++ Switch ++ " " ++ Status,
    lager:info("Command : ~p", [Command]),
    os:cmd(Command),
    os:cmd(Command).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
