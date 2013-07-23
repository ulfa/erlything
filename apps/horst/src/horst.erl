%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%% @doc API to start and stop the application and to interact with

-module(horst).

%% Application callbacks
-export([start/0, stop/0]).
-export([set_debug/0, set_info/0]).
-export([get_sensors/0]).

start() ->
 	application:start(gpio), 
    application:start(horst).

stop() ->
    application:start(gpio), 
    application:stop(horst).

get_sensors() ->
    get_sensors(sensor_sup:get_sensors(), []).

get_sensors([], Acc) ->
	Acc;
get_sensors([{Modul, Pid, _X, _Y}|Actors], Acc) ->
	get_sensors(Actors, [{Pid, Modul, Modul:get_description()}|Acc]).

set_debug() ->
	lager:set_loglevel(lager_console_backend, debug).

set_info() ->
	lager:set_loglevel(lager_console_backend, info).
