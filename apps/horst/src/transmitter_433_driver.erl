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

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	{Switch, Number, Status} = Body,
	call_driver(Number, Status),	
	Module_config_1 = lists:keyreplace(Switch, 1 , Module_config, {Switch, Number, Status}),
	lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1}).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver(Switch, Status) ->
    Driver = filename:join([code:priv_dir(horst), "driver", "remote", "send"]),
    os:cmd(Driver ++ " 11111 " ++ Switch ++ " " ++ Status).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
