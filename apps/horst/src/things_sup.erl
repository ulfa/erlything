%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(things_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([get_sensors/0, get_actors/0, get_sensors_pids/0, get_actors_pids/0]).
-export([get_things/0, get_things_pids/0, update_list_of_things/1]).
-export([is_thing_running/2]).

-include("../include/horst.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	{_Node, Config} = node_config:get_things_config(),
    {ok, {{one_for_one, 5, 10}, config_handler:create_thing_spec(Config)}}.

get_sensors() ->
	[{Name, Pid, Type, Modules} || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_sensor(thing:get_type(Name))].

get_actors() ->
	[{Name, Pid, Type, Modules}|| {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup),  is_actor(thing:get_type(Name))].

get_sensors_pids() ->
	[Pid || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_sensor(thing:get_type(Name))].

get_actors_pids() ->
	[Pid || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup),  is_valid_pid(Pid), is_actor(thing:get_type(Name))].

get_things() ->
	supervisor:which_children(things_sup).

get_things_pids() ->
	[Pid || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup)].

is_actor(actor) ->
	true;
is_actor(sensor) ->
	false;
is_actor(unknown) ->
	false.

is_sensor(sensor) ->
	true;
is_sensor(actor) ->
	false;
is_sensor(unknown) ->
	false.
is_valid_pid(undefined) ->
	false;
is_valid_pid(Pid) when is_pid(Pid) ->
	true.

update_list_of_things(Config) ->
	lager:info("update the list of sensors and actors, because the things.config changed"),
	[start_if_not_running([{thing, Name, List}], supervisor:which_children(things_sup)) || {thing, Name, List} <- Config, config_handler:is_activ(List)],
	[kill_if_running(Name, supervisor:which_children(things_sup)) || {thing, Name, List} <- Config, false =:= config_handler:is_activ(List)].

start_if_not_running([], Things) ->
	ok;
start_if_not_running(Config, Things) ->
	[{thing, Thing, List}] = Config, 
	case is_thing_running(Thing, Things) of 
		true -> ok;
		false -> [Child_spec] = config_handler:create_thing_spec(Config),
				 Result = supervisor:start_child(?MODULE, Child_spec),
				 lager:info("started thing with result : ~p", [Result]), 
				 ok
	end. 

kill_if_running(Thing, Things) ->
	case is_thing_running(Thing, Things) of 
		false -> ok;
		true -> %%thing:stop(Thing),
				Result = supervisor:terminate_child(?MODULE, list_to_atom(Thing)),
				supervisor:delete_child(?MODULE, list_to_atom(Thing)),
				lager:info("terminated thing with result : ~p", [Thing]),
				ok
	end. 

is_thing_running(Thing, Things) when is_list(Thing) ->
	is_thing_running(list_to_atom(Thing), Things);
is_thing_running(Thing, Things) when is_atom(Thing) ->
	case [Name ||{Name, Pid, Type, Modules} <- Things, Pid =/= undefined,  Name =:= Thing] of 
		[] -> false;
		_ -> true 
	end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

is_thing_running_test() ->
	Things = [{'Temperatur_Anzeige',"<0.104.0>",worker,[thing]},{'Temperatur_Sensor',"<0.105.0>",worker,[thing]}, {'Message_Logger',undefined ,worker,[thing]}],
	?assertEqual(true, is_thing_running('Temperatur_Sensor', Things)),
	?assertEqual(false, is_thing_running('unknown_Sensor', Things)),
	?assertEqual(false, is_thing_running('Message_Logger', Things)).


-endif.