%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(photocell_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1]).
-export([call_sensor/2]).
%% --------------------------------------------------------------------
%% if you want to initialize during startup, you have to do it here
%% --------------------------------------------------------------------
init(Config) ->
	lager:info("~p:init('~p')", [?MODULE, Config]),
    {driver, {Module, _Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Pin = config:get_value(pin, Module_config),
	{ok, Config}.

call_sensor(Config, Module_config) ->
    Pin = config:get_value(pin, Module_config),
    gpio:set_direction(Pin, out),
    gpio:set_direction(Pin, in),
    Value = loop(gpio:get(Pin), Pin, 0),
    thing:set_value(self(), Value),
    ?SEND(Value),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
loop({ok, 1}, Pin, Acc) ->
	Acc;
loop({ok, 0}, Pin, Acc) ->
	loop(gpio:get(Pin), Pin, Acc + 1).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
