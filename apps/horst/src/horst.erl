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
-export([get_sensors/0, get_actors/0]).

start() ->
 	application:start(gpio), 
    application:start(horst).

stop() ->
    application:start(gpio), 
    application:stop(horst).

get_sensors() ->
    {node(), get_sensors(sensor_sup:get_sensors(), [])}.

get_sensors([], Acc) ->
	Acc;
get_sensors([{Modul, Pid, _X, _Y}|Actors], Acc) ->
	get_sensors(Actors, [{Pid, Modul, Modul:get_description()}|Acc]).

get_actors() ->
	{node(), get_actors(actor_sup:get_actors(),[])}.

get_actors([], Acc) ->
	Acc;
get_actors([{Modul, Pid, _X, _Y}|Actors], Acc) ->
	get_actors(Actors, [{Pid, Modul, Modul:get_description()}|Acc]).

set_debug() ->
	lager:set_loglevel(lager_console_backend, debug).

set_info() ->
	lager:set_loglevel(lager_console_backend, info).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_actors_test() ->
	dht22_actor:start() ,
	seven_eleven_actor:start(),
	A = [{dht22_actor,"<0.97.0>",worker,[dht22_actor]}, 
		{seven_eleven_actor,"<0.96.0>",worker,[seven_eleven_actor]}],
	B = get_actors(A, []),
	?assertEqual([{"<0.96.0>", seven_eleven_actor, "Actor, which rings the 7-11 bell"},
				   {"<0.97.0>", dht22_actor, "Actor, which is responspable for dht22 messages"}], B).
-endif.