%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENS,E for licensing information.

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
-export([get_things/1]).

start() ->
 	application:start(gpio), 
    application:start(horst).

stop() ->
    application:start(gpio), 
    application:stop(horst).

get_things(sensor) ->
    {node(), get_things(things_sup:get_sensors(), [])};
get_things(actor) ->
    {node(), get_things(things_sup:get_actors(), [])}.
get_things([], Acc) ->
	Acc;
get_things([{Name, Pid, _X, _Y}|Things], Acc) ->
	{{Driver, _Func}, _Config} = thing:get_driver(Name),
	get_things(Things, [{Pid, Name, thing:get_description(Name), Driver}|Acc]).



set_debug() ->
	lager:set_loglevel(lager_console_backend, debug).

set_info() ->
	lager:set_loglevel(lager_console_backend, info).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.