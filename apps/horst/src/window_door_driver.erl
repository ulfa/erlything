%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(window_door_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1]).
-export([handle_msg/3]).
%% --------------------------------------------------------------------
%% if you want to initialize during startup, you have to do it here
%% --------------------------------------------------------------------
init(Config) ->
	lager:info("window_door_driver:init('~p')", [Config]),	
	Pin = proplists:get_value(pin, Config),
	gpio:init(Pin), 
	gpio:set_interrupt(Pin, proplists:get_value(int_type, Config)).

handle_msg({gpio_interrupt, Reg, Pin, Status}, Config, Module_config) ->
	Time_1 = os:timestamp(),
%%	Value = proplists:get_value(value, Module_config, []),
	Time = proplists:get_value(time, Module_config, []),
	lager:info("Pin : ~p with Status : ~p and time : ~p ", [Pin, Status, timer:now_diff(Time_1, Time)]),
%%	Msg = create_message(Status, sensor:get_id(Config)),
%%	sensor:send_message(Msg),
%%	lager:info("send message : ~p", [Msg]),
%%	Module_config_1 = lists:keyreplace(value, 1, Module_config, {value,add(Value, Status)}),
%%	lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1}).

	Module_config_1 = lists:keyreplace(value, 1, Module_config, {time, Time_1}),
	lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
add(Value, Status)  ->
	[Status|Value].

create_message(Status, Id) ->
	sensor:create_message(node(), ?MODULE, Id, date:get_date_seconds(), Status).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
add_value_test() ->
	?assertEqual([1,2], add([2], 1)).
-endif.
