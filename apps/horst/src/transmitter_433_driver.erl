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
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, _Id, Time, {Switch, Number, Status}], Config, Module_config) ->
	call_driver(Number, Status),	
	Module_config_1 = lists:keyreplace(Switch, 1 , Module_config, {Switch, Number, Status}),
	lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1});

handle_msg([Node ,Sensor, _Id, Time, Body], Config, Module_config) ->
	lager:warning("transmitter_433_driver got the wrong message : ~p", [[Node ,Sensor, _Id, Time, Body]]),
	Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver(Switch, Status) ->
	lager:debug("switch state from : ~p to : ~p", [Switch, Status]),
    Driver = filename:join([code:priv_dir(horst), "driver", "remote", "send "]),
    Command=Driver ++ Switch ++ " " ++ Status,
    lager:debug("Command : ~p", [Command]),
    os:cmd(Command),
    os:cmd(Command).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
